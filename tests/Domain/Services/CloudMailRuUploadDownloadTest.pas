unit CloudMailRuUploadDownloadTest;

{Tests for TCloudMailRu upload and download related methods.
 Focuses on API interactions and hash-based deduplication.}

interface

uses
	CloudMailRu,
	CloudSettings,
	CloudFileIdentity,
	CloudDirItem,
	CloudConstants,
	Cipher,
	WFXTypes,
	Logger,
	Progress,
	Request,
	TCHandler,
	AuthStrategy,
	FileSystem,
	CloudHTTP,
	HTTPManager,
	MockCloudHTTP,
	MockHTTPManager,
	TestableCloudMailRu,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework,
	OpenSSLProvider,
	AccountCredentialsProvider;

type
	[TestFixture]
	TCloudMailRuUploadDownloadTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TTestableCloudMailRu;
		FSettings: TCloudSettings;

		function CreateCloud(PublicAccount: Boolean = False): TTestableCloudMailRu;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{AddFileByIdentity - hash-based deduplication}
		[Test]
		procedure TestAddFileByIdentity_Success_ReturnsOK;
		[Test]
		procedure TestAddFileByIdentity_Failure_ReturnsError;
		[Test]
		procedure TestAddFileByIdentity_PublicAccount_ReturnsNotSupported;
		[Test]
		procedure TestAddFileByIdentity_EmptyHash_StillCallsAPI;
		[Test]
		procedure TestAddFileByIdentity_ConstructsCorrectPostData;

		{AddFileByIdentity with TCloudDirItem}
		[Test]
		procedure TestAddFileByIdentity_DirItem_Success;
		[Test]
		procedure TestAddFileByIdentity_DirItem_UsesHashAndSize;

		{StatusFile - checking file exists by path}
		[Test]
		procedure TestStatusFile_Success_ReturnsTrue;
		[Test]
		procedure TestStatusFile_NotFound_ReturnsFalse;
		[Test]
		procedure TestStatusFile_PopulatesFileInfo;
		[Test]
		procedure TestStatusFile_PublicAccount_Works;

		{PublishFile tests}
		[Test]
		procedure TestPublishFile_Success_ReturnsTrue;
		[Test]
		procedure TestPublishFile_Failure_ReturnsFalse;
		[Test]
		procedure TestPublishFile_ExtractsWeblink;
		[Test]
		procedure TestUnpublishFile_Success_ReturnsTrue;
	end;

implementation

const
	JSON_FILE_ADD_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';
	JSON_FILE_ADD_FAILURE = '{"email":"test@mail.ru","body":{"home":{"error":"invalid"}},"status":400}';

	JSON_STATUS_FILE_SUCCESS =
		'{"email":"test@mail.ru","body":{' +
		'"name":"test.txt","size":1024,"hash":"ABCD1234567890","mtime":1700000000,' +
		'"kind":"file","type":"file","home":"/test.txt","virus_scan":"pass"' +
		'},"status":200}';

	JSON_STATUS_FILE_NOT_FOUND =
		'{"email":"test@mail.ru","body":{"home":{"error":"not_exists"}},"status":404}';

	JSON_PUBLISH_SUCCESS =
		'{"email":"test@mail.ru","body":"weblink123abc","status":200}';

	JSON_PUBLISH_FAILURE =
		'{"email":"test@mail.ru","body":{"home":{"error":"invalid"}},"status":400}';

{TCloudMailRuUploadDownloadTest}

procedure TCloudMailRuUploadDownloadTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
end;

procedure TCloudMailRuUploadDownloadTest.TearDown;
begin
	FCloud.Free;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCloudMailRuUploadDownloadTest.CreateCloud(PublicAccount: Boolean): TTestableCloudMailRu;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := PublicAccount;

	Result := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TestThreadID(),
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		TNullCipher.Create, TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);

	Result.SetUnitedParams('api=2&access_token=test_token');
end;

{AddFileByIdentity tests}

procedure TCloudMailRuUploadDownloadTest.TestAddFileByIdentity_Success_ReturnsOK;
var
	FileIdentity: TCloudFileIdentity;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_FILE_ADD_SUCCESS);

	FileIdentity.Hash := 'ABCD1234567890ABCD1234567890ABCD12345678';
	FileIdentity.size := 1024;

	var Result := FCloud.Uploader.AddFileByIdentity(FileIdentity, '/dest/file.txt');

	Assert.AreEqual(FS_FILE_OK, Result, 'AddFileByIdentity should return FS_FILE_OK on success');
end;

procedure TCloudMailRuUploadDownloadTest.TestAddFileByIdentity_Failure_ReturnsError;
var
	FileIdentity: TCloudFileIdentity;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_FILE_ADD_FAILURE);

	FileIdentity.Hash := 'ABCD1234567890ABCD1234567890ABCD12345678';
	FileIdentity.size := 1024;

	var Result := FCloud.Uploader.AddFileByIdentity(FileIdentity, '/dest/file.txt');

	Assert.AreNotEqual(FS_FILE_OK, Result, 'AddFileByIdentity should return error on failure');
end;

procedure TCloudMailRuUploadDownloadTest.TestAddFileByIdentity_PublicAccount_ReturnsNotSupported;
var
	FileIdentity: TCloudFileIdentity;
begin
	FCloud := CreateCloud(True);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_FILE_ADD_SUCCESS);

	FileIdentity.Hash := 'ABCD1234567890ABCD1234567890ABCD12345678';
	FileIdentity.size := 1024;

	var Result := FCloud.Uploader.AddFileByIdentity(FileIdentity, '/dest/file.txt');

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result,
		'AddFileByIdentity should return FS_FILE_NOTSUPPORTED for public accounts');
end;

procedure TCloudMailRuUploadDownloadTest.TestAddFileByIdentity_EmptyHash_StillCallsAPI;
var
	FileIdentity: TCloudFileIdentity;
begin
	{Note: The API is called even with empty hash - this may be a potential improvement
	 to skip the API call when hash is empty since it will likely fail anyway.
	 Current behavior: API is called regardless of hash validity.}
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_FILE_ADD_SUCCESS);

	FileIdentity.Hash := '';
	FileIdentity.size := 1024;

	FCloud.Uploader.AddFileByIdentity(FileIdentity, '/dest/file.txt');

	{Verifies current behavior: API is called even with empty hash}
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_ADD),
		'API is called even with empty hash (current behavior)');
end;

procedure TCloudMailRuUploadDownloadTest.TestAddFileByIdentity_ConstructsCorrectPostData;
var
	FileIdentity: TCloudFileIdentity;
	PostedData: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_FILE_ADD_SUCCESS);

	FileIdentity.Hash := 'TESTHAS1234567890ABCD1234567890ABCD1234';
	FileIdentity.size := 2048;

	FCloud.Uploader.AddFileByIdentity(FileIdentity, '/dest/file.txt');

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.IsTrue(Pos(String('hash='), String(PostedData)) > 0, 'Post data should contain hash');
	Assert.IsTrue(Pos(String('size='), String(PostedData)) > 0, 'Post data should contain size');
end;

{AddFileByIdentity with TCloudFileIdentity record fields}

procedure TCloudMailRuUploadDownloadTest.TestAddFileByIdentity_DirItem_Success;
var
	FileIdentity: TCloudFileIdentity;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_FILE_ADD_SUCCESS);

	FileIdentity := Default(TCloudFileIdentity);
	FileIdentity.Hash := 'ABCD1234567890ABCD1234567890ABCD12345678';
	FileIdentity.Size := 1024;

	var Result := FCloud.Uploader.AddFileByIdentity(FileIdentity, '/dest/file.txt');

	Assert.AreEqual(FS_FILE_OK, Result, 'AddFileByIdentity with FileIdentity should return FS_FILE_OK');
end;

procedure TCloudMailRuUploadDownloadTest.TestAddFileByIdentity_DirItem_UsesHashAndSize;
var
	FileIdentity: TCloudFileIdentity;
	PostedData: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_FILE_ADD_SUCCESS);

	FileIdentity := Default(TCloudFileIdentity);
	FileIdentity.Hash := 'MYHASH1234567890ABCD1234567890ABCD123456';
	FileIdentity.Size := 4096;

	FCloud.Uploader.AddFileByIdentity(FileIdentity, '/dest/file.txt');

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.IsTrue(Pos(String('MYHASH'), String(PostedData)) > 0, 'Should use hash from FileIdentity');
	Assert.IsTrue(Pos(String('4096'), String(PostedData)) > 0, 'Should use size from FileIdentity');
end;

{StatusFile tests}

procedure TCloudMailRuUploadDownloadTest.TestStatusFile_Success_ReturnsTrue;
var
	FileInfo: TCloudDirItem;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_SUCCESS);

	var Success := FCloud.ListingService.StatusFile('/test.txt', FileInfo);

	Assert.IsTrue(Success, 'StatusFile should return True when file exists');
end;

procedure TCloudMailRuUploadDownloadTest.TestStatusFile_NotFound_ReturnsFalse;
var
	FileInfo: TCloudDirItem;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_NOT_FOUND);

	var Success := FCloud.ListingService.StatusFile('/nonexistent.txt', FileInfo);

	Assert.IsFalse(Success, 'StatusFile should return False when file not found');
end;

procedure TCloudMailRuUploadDownloadTest.TestStatusFile_PopulatesFileInfo;
var
	FileInfo: TCloudDirItem;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_SUCCESS);

	FCloud.ListingService.StatusFile('/test.txt', FileInfo);

	Assert.AreEqual(String('test.txt'), String(FileInfo.name), 'Should populate file name');
	Assert.AreEqual(Int64(1024), FileInfo.size, 'Should populate file size');
	Assert.AreEqual(String('ABCD1234567890'), String(FileInfo.hash), 'Should populate file hash');
end;

procedure TCloudMailRuUploadDownloadTest.TestStatusFile_PublicAccount_Works;
var
	FileInfo: TCloudDirItem;
begin
	FCloud := CreateCloud(True);
	FCloud.SetPublicLink('publiclink123');
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_SUCCESS);

	var Success := FCloud.ListingService.StatusFile('/test.txt', FileInfo);

	{Public accounts should be able to check file status}
	Assert.IsTrue(Success, 'StatusFile should work for public accounts');
end;

{PublishFile tests}

procedure TCloudMailRuUploadDownloadTest.TestPublishFile_Success_ReturnsTrue;
var
	PublicLink: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_PUBLISH, True, JSON_PUBLISH_SUCCESS);
	PublicLink := '';

	var Success := FCloud.PublishFile('/file.txt', PublicLink);

	Assert.IsTrue(Success, 'PublishFile should return True on success');
end;

procedure TCloudMailRuUploadDownloadTest.TestPublishFile_Failure_ReturnsFalse;
var
	PublicLink: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_PUBLISH, True, JSON_PUBLISH_FAILURE);
	PublicLink := '';

	var Success := FCloud.PublishFile('/file.txt', PublicLink);

	Assert.IsFalse(Success, 'PublishFile should return False on failure');
end;

procedure TCloudMailRuUploadDownloadTest.TestPublishFile_ExtractsWeblink;
var
	PublicLink: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_PUBLISH, True, JSON_PUBLISH_SUCCESS);
	PublicLink := '';

	FCloud.PublishFile('/file.txt', PublicLink);

	Assert.AreEqual(String('weblink123abc'), String(PublicLink), 'Should extract weblink from response');
end;

procedure TCloudMailRuUploadDownloadTest.TestUnpublishFile_Success_ReturnsTrue;
var
	PublicLink: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_UNPUBLISH, True, '{"email":"test@mail.ru","body":{},"status":200}');
	PublicLink := 'weblink123abc';

	var Success := FCloud.PublishFile('/file.txt', PublicLink, False); {Publish=False means unpublish}

	Assert.IsTrue(Success, 'UnpublishFile should return True on success');
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuUploadDownloadTest);

end.
