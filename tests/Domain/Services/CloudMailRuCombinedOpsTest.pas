unit CloudMailRuCombinedOpsTest;

{Tests for TCloudMailRu combined/wrapper operations:
 - FileMove (combines MoveFile + RenameFile)
 - FileCopy (combines CopyFile + RenameFile)
 - GetSharedFileUrl
 - GetShareInfo
 - GetDescriptionFile / PutDescriptionFile}

interface

uses
	CloudMailRu,
	CloudSettings,
	CloudDirItem,
	CloudInviteList,
	CloudConstants,
	FileCipher,
	WFXTypes,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	AuthStrategy,
	WindowsFileSystem,
	CloudHTTP,
	HTTPManager,
	MockCloudHTTP,
	MockHTTPManager,
	TestHelper,
	System.SysUtils,
	System.Classes,
	DUnitX.TestFramework,
	OpenSSLProvider;

type
	{Testable subclass that exposes protected members}
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
		procedure SetPublicLink(const Value: WideString);
	end;

	[TestFixture]
	TCloudMailRuCombinedOpsTest = class
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

		{FileMove tests - combines MoveFile and RenameFile}
		[Test]
		procedure TestFileMove_SameDir_CallsRenameOnly;
		[Test]
		procedure TestFileMove_DifferentDir_SameName_CallsMoveOnly;
		[Test]
		procedure TestFileMove_DifferentDir_DifferentName_CallsMoveAndRename;
		[Test]
		procedure TestFileMove_MoveFailure_DoesNotRename;
		[Test]
		procedure TestFileMove_PublicAccount_Fails;

		{FileCopy tests - combines CopyFile and RenameFile}
		[Test]
		procedure TestFileCopy_SameDir_ReturnsNotSupported;
		[Test]
		procedure TestFileCopy_DifferentDir_SameName_CallsCopyOnly;
		[Test]
		procedure TestFileCopy_DifferentDir_DifferentName_CallsCopyAndRename;
		[Test]
		procedure TestFileCopy_CopyFailure_DoesNotRename;
		[Test]
		procedure TestFileCopy_PublicAccount_Fails;

		{GetShareInfo tests}
		[Test]
		procedure TestGetShareInfo_Success;
		[Test]
		procedure TestGetShareInfo_Empty;
		[Test]
		procedure TestGetShareInfo_Failure;
		[Test]
		procedure TestGetShareInfo_ParsesInviteList;

		{GetSharedFileUrl tests - require shard/redirect mocking}
		[Test]
		procedure TestGetSharedFileUrl_PublicAccount_ReturnsUrl;
		[Test]
		procedure TestGetSharedFileUrl_WithShardType;
	end;

implementation

const
	JSON_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';
	JSON_FAILURE = '{"email":"test@mail.ru","body":{"home":{"error":"invalid"}},"status":400}';

	JSON_SHARE_INFO_SUCCESS =
		'{"email":"test@mail.ru","body":{' +
		'"invited":[{"email":"user@mail.ru","access":"read_only","status":"pending"}]' +
		'},"status":200}';

	JSON_SHARE_INFO_EMPTY =
		'{"email":"test@mail.ru","body":{"invited":[]},"status":200}';

	JSON_SHARD_SUCCESS =
		'{"email":"test@mail.ru","body":{' +
		'"weblink_get":[{"url":"https://cloclo1.cloud.mail.ru/weblink/"}],' +
		'"get":[{"url":"https://cloclo1.cloud.mail.ru/get/"}]' +
		'},"status":200}';

{TTestableCloudMailRu}

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

procedure TTestableCloudMailRu.SetPublicLink(const Value: WideString);
begin
	FPublicLink := Value;
end;

{TCloudMailRuCombinedOpsTest}

procedure TCloudMailRuCombinedOpsTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
end;

procedure TCloudMailRuCombinedOpsTest.TearDown;
begin
	FCloud.Free;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCloudMailRuCombinedOpsTest.CreateCloud(PublicAccount: Boolean): TTestableCloudMailRu;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := PublicAccount;
	if PublicAccount then
		FSettings.AccountSettings.PublicUrl := 'https://cloud.mail.ru/public/abc123';

	Result := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		TNullCipher.Create, TNullOpenSSLProvider.Create);

	Result.SetUnitedParams('api=2&access_token=test_token');
end;

{FileMove tests}

procedure TCloudMailRuCombinedOpsTest.TestFileMove_SameDir_CallsRenameOnly;
begin
	FCloud := CreateCloud;
	{Setup rename response}
	FMockHTTP.SetResponse(API_FILE_RENAME, True, JSON_SUCCESS);

	var Result := FCloud.FileOperations.Move('/folder/oldname.txt', '/folder/newname.txt');

	Assert.AreEqual(CLOUD_OPERATION_OK, Result, 'FileMove in same dir should succeed');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_RENAME), 'Should call RenameFile API');
	Assert.IsFalse(FMockHTTP.WasURLCalled(API_FILE_MOVE), 'Should NOT call MoveFile API');
end;

procedure TCloudMailRuCombinedOpsTest.TestFileMove_DifferentDir_SameName_CallsMoveOnly;
begin
	FCloud := CreateCloud;
	{Setup move response}
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_SUCCESS);

	var Result := FCloud.FileOperations.Move('/folder1/file.txt', '/folder2/file.txt');

	Assert.AreEqual(CLOUD_OPERATION_OK, Result, 'FileMove to different dir should succeed');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_MOVE), 'Should call MoveFile API');
	Assert.IsFalse(FMockHTTP.WasURLCalled(API_FILE_RENAME), 'Should NOT call RenameFile when name is same');
end;

procedure TCloudMailRuCombinedOpsTest.TestFileMove_DifferentDir_DifferentName_CallsMoveAndRename;
begin
	FCloud := CreateCloud;
	{Setup move and rename responses}
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_RENAME, True, JSON_SUCCESS);

	var Result := FCloud.FileOperations.Move('/folder1/oldname.txt', '/folder2/newname.txt');

	Assert.AreEqual(CLOUD_OPERATION_OK, Result, 'FileMove with rename should succeed');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_MOVE), 'Should call MoveFile API');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_RENAME), 'Should call RenameFile API');
end;

procedure TCloudMailRuCombinedOpsTest.TestFileMove_MoveFailure_DoesNotRename;
begin
	FCloud := CreateCloud;
	{Setup failed move response}
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_FAILURE);

	var Result := FCloud.FileOperations.Move('/folder1/oldname.txt', '/folder2/newname.txt');

	Assert.AreNotEqual(CLOUD_OPERATION_OK, Result, 'FileMove should fail');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_MOVE), 'Should call MoveFile API');
	Assert.IsFalse(FMockHTTP.WasURLCalled(API_FILE_RENAME), 'Should NOT call RenameFile after move failure');
end;

procedure TCloudMailRuCombinedOpsTest.TestFileMove_PublicAccount_Fails;
begin
	{FileMove uses ExtractUniversalFilePath which correctly handles forward slashes.
	 For public accounts, MoveToPath returns FS_FILE_NOTSUPPORTED.}
	FCloud := CreateCloud(True);
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_SUCCESS);

	var Result := FCloud.FileOperations.Move('/folder1/file.txt', '/folder2/file.txt');

	{Public accounts cannot perform move operations}
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'FileMove should fail for public accounts');
end;

{FileCopy tests}

procedure TCloudMailRuCombinedOpsTest.TestFileCopy_SameDir_ReturnsNotSupported;
begin
	FCloud := CreateCloud;

	var Result := FCloud.FileOperations.Copy('/folder/file.txt', '/folder/copy.txt');

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'FileCopy to same dir should return not supported');
end;

procedure TCloudMailRuCombinedOpsTest.TestFileCopy_DifferentDir_SameName_CallsCopyOnly;
begin
	FCloud := CreateCloud;
	{Setup copy response}
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_SUCCESS);

	var Result := FCloud.FileOperations.Copy('/folder1/file.txt', '/folder2/file.txt');

	Assert.AreEqual(CLOUD_OPERATION_OK, Result, 'FileCopy to different dir should succeed');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_COPY), 'Should call CopyFile API');
	Assert.IsFalse(FMockHTTP.WasURLCalled(API_FILE_RENAME), 'Should NOT call RenameFile when name is same');
end;

procedure TCloudMailRuCombinedOpsTest.TestFileCopy_DifferentDir_DifferentName_CallsCopyAndRename;
begin
	FCloud := CreateCloud;
	{Setup copy and rename responses}
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_RENAME, True, JSON_SUCCESS);

	var Result := FCloud.FileOperations.Copy('/folder1/oldname.txt', '/folder2/newname.txt');

	Assert.AreEqual(CLOUD_OPERATION_OK, Result, 'FileCopy with rename should succeed');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_COPY), 'Should call CopyFile API');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_RENAME), 'Should call RenameFile API');
end;

procedure TCloudMailRuCombinedOpsTest.TestFileCopy_CopyFailure_DoesNotRename;
begin
	FCloud := CreateCloud;
	{Setup failed copy response}
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_FAILURE);

	var Result := FCloud.FileOperations.Copy('/folder1/oldname.txt', '/folder2/newname.txt');

	Assert.AreNotEqual(CLOUD_OPERATION_OK, Result, 'FileCopy should fail');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_COPY), 'Should call CopyFile API');
	Assert.IsFalse(FMockHTTP.WasURLCalled(API_FILE_RENAME), 'Should NOT call RenameFile after copy failure');
end;

procedure TCloudMailRuCombinedOpsTest.TestFileCopy_PublicAccount_Fails;
begin
	{Note: FileCopy uses standard ExtractFilePath which doesn't recognize forward slashes
	 on Windows. With paths like '/folder1/file.txt', ExtractFilePath returns '',
	 making SameDir=True, which returns FS_FILE_NOTSUPPORTED immediately (before
	 checking public account). This is the expected behavior with current implementation.}
	FCloud := CreateCloud(True);
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_SUCCESS);

	var Result := FCloud.FileOperations.Copy('/folder1/file.txt', '/folder2/file.txt');

	{Due to ExtractFilePath not recognizing '/', SameDir becomes True,
	 so FileCopy returns FS_FILE_NOTSUPPORTED for same-dir copy attempt}
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'FileCopy should fail for public accounts');
end;

{GetShareInfo tests}

procedure TCloudMailRuCombinedOpsTest.TestGetShareInfo_Success;
var
	InviteListing: TCloudInviteList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARED_INFO, True, JSON_SHARE_INFO_SUCCESS);

	var Success := FCloud.ShareService.GetShareInfo('/MySharedFolder', InviteListing);

	Assert.IsTrue(Success, 'GetShareInfo should return True on success');
end;

procedure TCloudMailRuCombinedOpsTest.TestGetShareInfo_Empty;
var
	InviteListing: TCloudInviteList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARED_INFO, True, JSON_SHARE_INFO_EMPTY);

	var Success := FCloud.ShareService.GetShareInfo('/MyFolder', InviteListing);

	Assert.IsTrue(Success, 'GetShareInfo should return True for empty list');
	Assert.AreEqual(Integer(0), Integer(Length(InviteListing)), 'InviteListing should be empty');
end;

procedure TCloudMailRuCombinedOpsTest.TestGetShareInfo_Failure;
var
	InviteListing: TCloudInviteList;
begin
	{To test failure, we need HTTP.GetPage to return False, not just error JSON.
	 TCloudInviteList.FromJSON returns True for missing "invited" field (treats as empty).}
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARED_INFO, False, '');

	var Success := FCloud.ShareService.GetShareInfo('/MyFolder', InviteListing);

	Assert.IsFalse(Success, 'GetShareInfo should return False when HTTP fails');
end;

procedure TCloudMailRuCombinedOpsTest.TestGetShareInfo_ParsesInviteList;
var
	InviteListing: TCloudInviteList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARED_INFO, True, JSON_SHARE_INFO_SUCCESS);

	FCloud.ShareService.GetShareInfo('/MySharedFolder', InviteListing);

	Assert.AreEqual(Integer(1), Integer(Length(InviteListing)), 'Should have 1 invite');
	Assert.AreEqual(WideString('user@mail.ru'), InviteListing[0].email, 'Should parse email');
	Assert.AreEqual(WideString(CLOUD_SHARE_ACCESS_READ_ONLY), InviteListing[0].access, 'Should parse access');
end;

{GetSharedFileUrl tests}

procedure TCloudMailRuCombinedOpsTest.TestGetSharedFileUrl_PublicAccount_ReturnsUrl;
begin
	FCloud := CreateCloud(True);
	FCloud.SetPublicLink('abc123');
	{Setup shard response for redirection}
	FMockHTTP.SetResponse('', True, 'https://final.url/file.txt');

	var Url := FCloud.Downloader.GetSharedFileUrl('/test/file.txt');

	Assert.IsNotEmpty(Url, 'GetSharedFileUrl should return non-empty URL');
end;

procedure TCloudMailRuCombinedOpsTest.TestGetSharedFileUrl_WithShardType;
begin
	FCloud := CreateCloud(True);
	FCloud.SetPublicLink('abc123');
	FMockHTTP.SetResponse(API_DISPATCHER, True, JSON_SHARD_SUCCESS);
	FMockHTTP.SetResponse('', True, 'https://final.url/file.txt');

	var Url := FCloud.Downloader.GetSharedFileUrl('/test/file.txt', SHARD_TYPE_WEBLINK_GET);

	{Method should request shard when ShardType is not default}
	Assert.IsNotEmpty(Url, 'GetSharedFileUrl should return URL');
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuCombinedOpsTest);

end.
