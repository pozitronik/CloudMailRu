unit CloudMailRuPublicAccountTest;

{Tests for TCloudMailRu public account behavior.
 Public accounts access shared content via weblink and cannot modify files.}

interface

uses
	CloudMailRu,
	CloudSettings,
	CMRDirItemList,
	CMRDirItem,
	CMRConstants,
	PLUGIN_TYPES,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	IAuthStrategyInterface,
	WindowsFileSystem,
	CloudHTTP,
	HTTPManager,
	MockCloudHTTP,
	MockHTTPManager,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework;

type
	{Testable subclass that exposes protected members}
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
		procedure SetPublicLink(const Value: WideString);
		function TestGetPublicLink: WideString;
	end;

	[TestFixture]
	TCloudMailRuPublicAccountTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TTestableCloudMailRu;
		FSettings: TCloudSettings;

		function CreatePublicCloud(PublicUrl: WideString = ''): TTestableCloudMailRu;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{IsPublicAccount property}
		[Test]
		procedure TestIsPublicAccount_WhenPublicTrue_ReturnsTrue;
		[Test]
		procedure TestIsPublicAccount_WhenPublicFalse_ReturnsFalse;

		{GetPublicLink behavior}
		[Test]
		procedure TestGetPublicLink_ReturnsLinkFromSettings;
		[Test]
		procedure TestGetPublicLink_StripsPublicAccessUrl;
		[Test]
		procedure TestGetPublicLink_CachesResult;

		{GetDirListing for public accounts}
		[Test]
		procedure TestGetDirListing_Public_UsesWeblinkParameter;
		[Test]
		procedure TestGetDirListing_Public_Success;
		[Test]
		procedure TestGetDirListing_Public_IncludesPublicLinkInUrl;

		{Operations blocked for public accounts}
		[Test]
		procedure TestCreateDir_Public_ReturnsFalse;
		[Test]
		procedure TestDeleteFile_Public_ReturnsFalse;
		[Test]
		procedure TestCopyFile_Public_ReturnsNotSupported;
		[Test]
		procedure TestMoveFile_Public_ReturnsNotSupported;
		[Test]
		procedure TestRenameFile_Public_ReturnsWriteError;
		[Test]
		procedure TestPublishFile_Public_ReturnsFalse;
		[Test]
		procedure TestGetTrashbinListing_Public_ReturnsFalse;
		[Test]
		procedure TestGetSharedLinksListing_Public_ReturnsFalse;
		[Test]
		procedure TestCloneWeblink_Public_ReturnsNotSupported;
	end;

implementation

const
	TEST_PUBLIC_URL = 'https://cloud.mail.ru/public/abcd/TestShare';
	TEST_PUBLIC_LINK = 'abcd/TestShare';

	JSON_DIR_LISTING_SUCCESS =
		'{"email":"test@mail.ru","body":{"count":{"folders":1,"files":1},' +
		'"name":"TestDir","kind":"folder","type":"folder","home":"/TestDir",' +
		'"list":[' +
		'{"name":"file.txt","size":1024,"hash":"ABC123","mtime":1700000000,"kind":"file","type":"file","home":"/TestDir/file.txt"}' +
		']},"status":200}';

{TTestableCloudMailRu}

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

procedure TTestableCloudMailRu.SetPublicLink(const Value: WideString);
begin
	FPublicLink := Value;
end;

function TTestableCloudMailRu.TestGetPublicLink: WideString;
begin
	Result := GetPublicLink;
end;

{TCloudMailRuPublicAccountTest}

procedure TCloudMailRuPublicAccountTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
end;

procedure TCloudMailRuPublicAccountTest.TearDown;
begin
	FCloud.Free;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCloudMailRuPublicAccountTest.CreatePublicCloud(PublicUrl: WideString): TTestableCloudMailRu;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := True;
	FSettings.AccountSettings.PublicUrl := PublicUrl;

	Result := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create);

	Result.SetUnitedParams('api=2');
end;

{IsPublicAccount property tests}

procedure TCloudMailRuPublicAccountTest.TestIsPublicAccount_WhenPublicTrue_ReturnsTrue;
begin
	FCloud := CreatePublicCloud;
	Assert.IsTrue(FCloud.IsPublicAccount, 'IsPublicAccount should be True for public accounts');
end;

procedure TCloudMailRuPublicAccountTest.TestIsPublicAccount_WhenPublicFalse_ReturnsFalse;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := False;

	FCloud := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create);

	Assert.IsFalse(FCloud.IsPublicAccount, 'IsPublicAccount should be False for private accounts');
end;

{GetPublicLink tests}

procedure TCloudMailRuPublicAccountTest.TestGetPublicLink_ReturnsLinkFromSettings;
begin
	FCloud := CreatePublicCloud(TEST_PUBLIC_URL);
	FCloud.SetPublicLink(''); {Clear any cached value}

	var Link := FCloud.TestGetPublicLink;

	Assert.IsTrue(Link <> '', 'GetPublicLink should return non-empty link');
end;

procedure TCloudMailRuPublicAccountTest.TestGetPublicLink_StripsPublicAccessUrl;
begin
	FCloud := CreatePublicCloud(TEST_PUBLIC_URL);
	FCloud.SetPublicLink(''); {Clear any cached value}

	var Link := FCloud.TestGetPublicLink;

	{Link should not start with the full PUBLIC_ACCESS_URL}
	Assert.IsFalse(Pos(String('https://'), String(Link)) > 0,
		'GetPublicLink should strip the protocol from URL');
end;

procedure TCloudMailRuPublicAccountTest.TestGetPublicLink_CachesResult;
begin
	FCloud := CreatePublicCloud(TEST_PUBLIC_URL);

	{Set a cached value}
	FCloud.SetPublicLink('cached_link');

	var Link := FCloud.TestGetPublicLink;

	Assert.AreEqual(String('cached_link'), String(Link),
		'GetPublicLink should return cached value if available');
end;

{GetDirListing for public accounts}

procedure TCloudMailRuPublicAccountTest.TestGetDirListing_Public_UsesWeblinkParameter;
var
	DirListing: TCMRDirItemList;
	LastCall: WideString;
begin
	FCloud := CreatePublicCloud(TEST_PUBLIC_URL);
	FCloud.SetPublicLink(TEST_PUBLIC_LINK);
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING_SUCCESS);

	FCloud.GetDirListing('/TestDir', DirListing);

	LastCall := FMockHTTP.GetLastCall;
	Assert.IsTrue(Pos(String('weblink='), String(LastCall)) > 0,
		'Public account should use weblink parameter in URL');
end;

procedure TCloudMailRuPublicAccountTest.TestGetDirListing_Public_Success;
var
	DirListing: TCMRDirItemList;
	Success: Boolean;
begin
	FCloud := CreatePublicCloud(TEST_PUBLIC_URL);
	FCloud.SetPublicLink(TEST_PUBLIC_LINK);
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING_SUCCESS);

	Success := FCloud.GetDirListing('/TestDir', DirListing);

	Assert.IsTrue(Success, 'Public account GetDirListing should succeed');
	Assert.IsTrue(Length(DirListing) > 0, 'Should return items');
end;

procedure TCloudMailRuPublicAccountTest.TestGetDirListing_Public_IncludesPublicLinkInUrl;
var
	DirListing: TCMRDirItemList;
	LastCall: WideString;
begin
	FCloud := CreatePublicCloud(TEST_PUBLIC_URL);
	FCloud.SetPublicLink('myPublicLink');
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING_SUCCESS);

	FCloud.GetDirListing('/TestDir', DirListing);

	LastCall := FMockHTTP.GetLastCall;
	Assert.IsTrue(Pos(String('myPublicLink'), String(LastCall)) > 0,
		'URL should contain the public link identifier');
end;

{Operations blocked for public accounts}

procedure TCloudMailRuPublicAccountTest.TestCreateDir_Public_ReturnsFalse;
begin
	FCloud := CreatePublicCloud;
	FMockHTTP.SetResponse(API_FOLDER_ADD, True, '{"status":200}');

	var Success := FCloud.CreateDir('/NewFolder');

	Assert.IsFalse(Success, 'CreateDir should return False for public accounts');
	Assert.IsFalse(FMockHTTP.WasURLCalled(API_FOLDER_ADD),
		'Should not make API call for blocked operation');
end;

procedure TCloudMailRuPublicAccountTest.TestDeleteFile_Public_ReturnsFalse;
begin
	FCloud := CreatePublicCloud;
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, '{"status":200}');

	var Success := FCloud.DeleteFile('/file.txt');

	Assert.IsFalse(Success, 'DeleteFile should return False for public accounts');
end;

procedure TCloudMailRuPublicAccountTest.TestCopyFile_Public_ReturnsNotSupported;
begin
	FCloud := CreatePublicCloud;
	FMockHTTP.SetResponse(API_FILE_COPY, True, '{"status":200}');

	var Result := FCloud.CopyFile('/source.txt', '/dest');

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result,
		'CopyFile should return FS_FILE_NOTSUPPORTED for public accounts');
end;

procedure TCloudMailRuPublicAccountTest.TestMoveFile_Public_ReturnsNotSupported;
begin
	FCloud := CreatePublicCloud;
	FMockHTTP.SetResponse(API_FILE_MOVE, True, '{"status":200}');

	var Result := FCloud.MoveFile('/source.txt', '/dest');

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result,
		'MoveFile should return FS_FILE_NOTSUPPORTED for public accounts');
end;

procedure TCloudMailRuPublicAccountTest.TestRenameFile_Public_ReturnsWriteError;
begin
	FCloud := CreatePublicCloud;
	FMockHTTP.SetResponse(API_FILE_RENAME, True, '{"status":200}');

	var Result := FCloud.RenameFile('/old.txt', 'new.txt');

	Assert.AreEqual(FS_FILE_WRITEERROR, Result,
		'RenameFile should return FS_FILE_WRITEERROR for public accounts');
end;

procedure TCloudMailRuPublicAccountTest.TestPublishFile_Public_ReturnsFalse;
var
	PublicLink: WideString;
begin
	FCloud := CreatePublicCloud;
	FMockHTTP.SetResponse(API_FILE_PUBLISH, True, '{"status":200}');
	PublicLink := '';

	var Success := FCloud.PublishFile('/file.txt', PublicLink);

	Assert.IsFalse(Success, 'PublishFile should return False for public accounts');
end;

procedure TCloudMailRuPublicAccountTest.TestGetTrashbinListing_Public_ReturnsFalse;
var
	DirListing: TCMRDirItemList;
begin
	FCloud := CreatePublicCloud;
	FMockHTTP.SetResponse(API_TRASHBIN, True, '{"status":200,"body":{"list":[]}}');

	var Success := FCloud.GetTrashbinListing(DirListing);

	Assert.IsFalse(Success, 'GetTrashbinListing should return False for public accounts');
end;

procedure TCloudMailRuPublicAccountTest.TestGetSharedLinksListing_Public_ReturnsFalse;
var
	DirListing: TCMRDirItemList;
begin
	FCloud := CreatePublicCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARED_LINKS, True, '{"status":200,"body":{"list":[]}}');

	var Success := FCloud.GetSharedLinksListing(DirListing);

	Assert.IsFalse(Success, 'GetSharedLinksListing should return False for public accounts');
end;

procedure TCloudMailRuPublicAccountTest.TestCloneWeblink_Public_ReturnsNotSupported;
begin
	FCloud := CreatePublicCloud;
	FMockHTTP.SetResponse(API_CLONE, True, '{"status":200}');

	var Result := FCloud.CloneWeblink('/dest', 'https://cloud.mail.ru/public/test');

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result,
		'CloneWeblink should return FS_FILE_NOTSUPPORTED for public accounts');
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuPublicAccountTest);

end.
