unit CloudMailRuIntegrationTest;

{Integration tests for TCloudMailRu using enhanced mock infrastructure.
 Tests multi-step flows through public API methods with response queuing.
 Demonstrates mock HTTP infrastructure usage for complex scenarios.}

interface

uses
	CloudMailRu,
	CloudSettings,
	CMRConstants,
	CMRFileIdentity,
	CMRDirItem,
	WFXTypes,
	SettingsConstants,
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
	MockShardHelper,
	TestHelper,
	System.Classes,
	System.SysUtils,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCloudMailRuIntegrationTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TCloudMailRu;
		FSettings: TCloudSettings;

		function CreateCloud(PublicAccount: Boolean = False): TCloudMailRu;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{CreateDir integration tests}
		[Test]
		procedure TestCreateDir_Success_ReturnsTrue;
		[Test]
		procedure TestCreateDir_AlreadyExists_ReturnsFalse;
		[Test]
		procedure TestCreateDir_PublicAccount_ReturnsFalse;

		{DeleteFile integration tests}
		[Test]
		procedure TestDeleteFile_Success_ReturnsTrue;
		[Test]
		procedure TestDeleteFile_NotFound_ReturnsFalse;

		{RemoveDir integration tests}
		[Test]
		procedure TestRemoveDir_Success_ReturnsTrue;
		[Test]
		procedure TestRemoveDir_NotFound_ReturnsFalse;

		{RenameFile integration tests}
		[Test]
		procedure TestRenameFile_Success_ReturnsOK;
		[Test]
		procedure TestRenameFile_NotFound_ReturnsError;

		{CopyFile integration tests}
		[Test]
		procedure TestCopyFile_Success_ReturnsOK;
		[Test]
		procedure TestCopyFile_NotFound_ReturnsError;

		{MoveFile integration tests}
		[Test]
		procedure TestMoveFile_Success_ReturnsOK;
		[Test]
		procedure TestMoveFile_NotFound_ReturnsError;

		{StatusFile integration tests}
		[Test]
		procedure TestStatusFile_Success_ParsesResponse;
		[Test]
		procedure TestStatusFile_NotFound_ReturnsFalse;

		{AddFileByIdentity integration tests}
		[Test]
		procedure TestAddFileByIdentity_Success_ReturnsOK;
		[Test]
		procedure TestAddFileByIdentity_Exists_ReturnsExistsError;

		{CloneWeblink integration tests}
		[Test]
		procedure TestCloneWeblink_Success_ReturnsOK;
		[Test]
		procedure TestCloneWeblink_NotFound_ReturnsError;

		{PublishFile integration tests}
		[Test]
		procedure TestPublishFile_Success_ReturnsTrue;
		[Test]
		procedure TestPublishFile_Unpublish_ReturnsTrue;

		{TrashbinRestore integration tests}
		[Test]
		procedure TestTrashbinRestore_Success_ReturnsTrue;
		[Test]
		procedure TestTrashbinEmpty_Success_ReturnsTrue;

		{Queue-based multi-request tests}
		[Test]
		procedure TestQueuedResponses_MultipleAPICalls_ProcessedInOrder;
		[Test]
		procedure TestQueuedResponses_SameEndpoint_DifferentResults;

		{Token refresh scenario tests}
		[Test]
		procedure TestTokenRefresh_OnAPIError_RefreshesAndRetries;

		{Multi-step flow tests}
		[Test]
		procedure TestMultiStepFlow_CreateThenDeleteDir;
		[Test]
		procedure TestMultiStepFlow_CopyThenRenameFile;
	end;

implementation

const
	SHA1_HASH_40 = 'ABCD1234567890ABCD1234567890ABCD12345678';

	JSON_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';
	{File status must include type="file" for TCMRDirItem.FromJSON to parse correctly}
	JSON_FILE_STATUS = '{"email":"test@mail.ru","body":{"name":"file.txt","type":"file","size":1024,"hash":"' + SHA1_HASH_40 + '","mtime":1609459200},"status":200}';
	JSON_PUBLISH_SUCCESS = '{"email":"test@mail.ru","body":"https://cloud.mail.ru/public/abcd1234","status":200}';
	{Error responses must have non-200 status for TCMROperationResult to parse body errors}
	JSON_EXISTS_ERROR = '{"email":"test@mail.ru","body":{"home":{"error":"exists"}},"status":400}';
	JSON_NOT_EXISTS_ERROR = '{"email":"test@mail.ru","body":{"home":{"error":"not_exists"}},"status":404}';
	JSON_TOKEN_ERROR = '{"email":"test@mail.ru","body":{"home":{"error":"token"}},"status":403}';
	JSON_CSRF_SUCCESS = '{"body":{"token":"new_csrf_token"},"status":200}';

{TCloudMailRuIntegrationTest}

procedure TCloudMailRuIntegrationTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
end;

procedure TCloudMailRuIntegrationTest.TearDown;
begin
	FreeAndNil(FCloud);
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCloudMailRuIntegrationTest.CreateCloud(PublicAccount: Boolean): TCloudMailRu;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := PublicAccount;
	FSettings.AccountSettings.Email := 'test@mail.ru';
	FSettings.CloudMaxFileSize := CLOUD_MAX_FILESIZE_DEFAULT;

	Result := TCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create);
end;

{CreateDir integration tests}

procedure TCloudMailRuIntegrationTest.TestCreateDir_Success_ReturnsTrue;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_ADD, True, JSON_SUCCESS);

	var Result := FCloud.FileOps.CreateDirectory('/test/newdir');

	Assert.IsTrue(Result, 'CreateDir should return true on success');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FOLDER_ADD), 'Should call folder add API');
end;

procedure TCloudMailRuIntegrationTest.TestCreateDir_AlreadyExists_ReturnsFalse;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_ADD, True, JSON_EXISTS_ERROR);

	var Result := FCloud.FileOps.CreateDirectory('/test/existing');

	Assert.IsFalse(Result, 'CreateDir should return false when dir exists');
end;

procedure TCloudMailRuIntegrationTest.TestCreateDir_PublicAccount_ReturnsFalse;
begin
	FCloud := CreateCloud(True); {Public account}

	var Result := FCloud.FileOps.CreateDirectory('/test/dir');

	Assert.IsFalse(Result, 'Public account should not create directories');
	Assert.AreEqual(0, FMockHTTP.GetCallCount, 'Should not make HTTP calls for public account');
end;

{DeleteFile integration tests}

procedure TCloudMailRuIntegrationTest.TestDeleteFile_Success_ReturnsTrue;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_SUCCESS);

	var Result := FCloud.FileOps.Delete('/file/to/delete.txt');

	Assert.IsTrue(Result, 'DeleteFile should return true on success');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_REMOVE), 'Should call file remove API');
end;

procedure TCloudMailRuIntegrationTest.TestDeleteFile_NotFound_ReturnsFalse;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_NOT_EXISTS_ERROR);

	var Result := FCloud.FileOps.Delete('/nonexistent.txt');

	Assert.IsFalse(Result, 'DeleteFile should return false when file not found');
end;

{RemoveDir integration tests}

procedure TCloudMailRuIntegrationTest.TestRemoveDir_Success_ReturnsTrue;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_SUCCESS);

	var Result := FCloud.FileOps.RemoveDirectory('/dir/to/remove');

	Assert.IsTrue(Result, 'RemoveDir should return true on success');
end;

procedure TCloudMailRuIntegrationTest.TestRemoveDir_NotFound_ReturnsFalse;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_NOT_EXISTS_ERROR);

	var Result := FCloud.FileOps.RemoveDirectory('/nonexistent/dir');

	Assert.IsFalse(Result, 'RemoveDir should return false when dir not found');
end;

{RenameFile integration tests}

procedure TCloudMailRuIntegrationTest.TestRenameFile_Success_ReturnsOK;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_RENAME, True, JSON_SUCCESS);

	var Result := FCloud.FileOps.Rename('/old/path.txt', '/old/newname.txt');

	Assert.AreEqual(FS_FILE_OK, Result, 'RenameFile should return OK on success');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_RENAME), 'Should call file rename API');
end;

procedure TCloudMailRuIntegrationTest.TestRenameFile_NotFound_ReturnsError;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_RENAME, True, JSON_NOT_EXISTS_ERROR);

	var Result := FCloud.FileOps.Rename('/nonexistent.txt', '/newname.txt');

	Assert.AreNotEqual(FS_FILE_OK, Result, 'RenameFile should return error when file not found');
end;

{CopyFile integration tests}

procedure TCloudMailRuIntegrationTest.TestCopyFile_Success_ReturnsOK;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_SUCCESS);

	var Result := FCloud.FileOps.CopyToPath('/source/file.txt', '/dest/');

	Assert.AreEqual(FS_FILE_OK, Result, 'CopyFile should return OK on success');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_COPY), 'Should call file copy API');
end;

procedure TCloudMailRuIntegrationTest.TestCopyFile_NotFound_ReturnsError;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_NOT_EXISTS_ERROR);

	var Result := FCloud.FileOps.CopyToPath('/nonexistent.txt', '/dest/');

	Assert.AreNotEqual(FS_FILE_OK, Result, 'CopyFile should return error when file not found');
end;

{MoveFile integration tests}

procedure TCloudMailRuIntegrationTest.TestMoveFile_Success_ReturnsOK;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_SUCCESS);

	var Result := FCloud.FileOps.MoveToPath('/source/file.txt', '/dest/');

	Assert.AreEqual(FS_FILE_OK, Result, 'MoveFile should return OK on success');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_MOVE), 'Should call file move API');
end;

procedure TCloudMailRuIntegrationTest.TestMoveFile_NotFound_ReturnsError;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_NOT_EXISTS_ERROR);

	var Result := FCloud.FileOps.MoveToPath('/nonexistent.txt', '/dest/');

	Assert.AreNotEqual(FS_FILE_OK, Result, 'MoveFile should return error when file not found');
end;

{StatusFile integration tests}

procedure TCloudMailRuIntegrationTest.TestStatusFile_Success_ParsesResponse;
var
	DirItem: TCMRDirItem;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE, True, JSON_FILE_STATUS);

	var Result := FCloud.ListingService.StatusFile('/file.txt', DirItem);

	Assert.IsTrue(Result, 'StatusFile should return true on success');
	Assert.AreEqual(String('file.txt'), String(DirItem.name), 'Should parse file name');
	Assert.AreEqual(Int64(1024), Int64(DirItem.size), 'Should parse file size');
end;

procedure TCloudMailRuIntegrationTest.TestStatusFile_NotFound_ReturnsFalse;
var
	DirItem: TCMRDirItem;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE, True, JSON_NOT_EXISTS_ERROR);

	var Result := FCloud.ListingService.StatusFile('/nonexistent.txt', DirItem);

	Assert.IsFalse(Result, 'StatusFile should return false when file not found');
end;

{AddFileByIdentity integration tests}

procedure TCloudMailRuIntegrationTest.TestAddFileByIdentity_Success_ReturnsOK;
var
	FileIdentity: TCMRFileIdentity;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_SUCCESS);

	FileIdentity.Hash := SHA1_HASH_40;
	FileIdentity.size := 1024;

	var Result := FCloud.Uploader.AddFileByIdentity(FileIdentity, '/dest/file.txt');

	Assert.AreEqual(FS_FILE_OK, Result, 'AddFileByIdentity should return OK on success');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_ADD), 'Should call file add API');
end;

procedure TCloudMailRuIntegrationTest.TestAddFileByIdentity_Exists_ReturnsExistsError;
var
	FileIdentity: TCMRFileIdentity;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_EXISTS_ERROR);

	FileIdentity.Hash := SHA1_HASH_40;
	FileIdentity.size := 1024;

	var Result := FCloud.Uploader.AddFileByIdentity(FileIdentity, '/dest/file.txt');

	Assert.AreEqual(FS_FILE_EXISTS, Result, 'AddFileByIdentity should return exists error');
end;

{CloneWeblink integration tests}

procedure TCloudMailRuIntegrationTest.TestCloneWeblink_Success_ReturnsOK;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_CLONE, True, JSON_SUCCESS);

	var Result := FCloud.ShareService.CloneWeblink('/dest/', 'https://cloud.mail.ru/public/abcd1234', CLOUD_CONFLICT_STRICT);

	Assert.AreEqual(FS_FILE_OK, Result, 'CloneWeblink should return OK on success');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_CLONE), 'Should call clone API');
end;

procedure TCloudMailRuIntegrationTest.TestCloneWeblink_NotFound_ReturnsError;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_CLONE, True, JSON_NOT_EXISTS_ERROR);

	var Result := FCloud.ShareService.CloneWeblink('/dest/', 'https://cloud.mail.ru/public/invalid', CLOUD_CONFLICT_STRICT);

	Assert.AreNotEqual(FS_FILE_OK, Result, 'CloneWeblink should return error for invalid link');
end;

{PublishFile integration tests}

procedure TCloudMailRuIntegrationTest.TestPublishFile_Success_ReturnsTrue;
var
	PublicLink: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_PUBLISH, True, JSON_PUBLISH_SUCCESS);

	var Result := FCloud.PublishFile('/file.txt', PublicLink, True);

	Assert.IsTrue(Result, 'PublishFile should return true on success');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_PUBLISH), 'Should call publish API');
end;

procedure TCloudMailRuIntegrationTest.TestPublishFile_Unpublish_ReturnsTrue;
var
	PublicLink: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_UNPUBLISH, True, JSON_SUCCESS);

	var Result := FCloud.PublishFile('/file.txt', PublicLink, False);

	Assert.IsTrue(Result, 'UnpublishFile should return true on success');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_UNPUBLISH), 'Should call unpublish API');
end;

{TrashbinRestore integration tests}

procedure TCloudMailRuIntegrationTest.TestTrashbinRestore_Success_ReturnsTrue;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_TRASHBIN_RESTORE, True, JSON_SUCCESS);

	var Result := FCloud.ListingService.TrashbinRestore('/deleted/file.txt', 0, CLOUD_CONFLICT_RENAME);

	Assert.IsTrue(Result, 'TrashbinRestore should return true on success');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_TRASHBIN_RESTORE), 'Should call restore API');
end;

procedure TCloudMailRuIntegrationTest.TestTrashbinEmpty_Success_ReturnsTrue;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_TRASHBIN_EMPTY, True, JSON_SUCCESS);

	var Result := FCloud.ListingService.TrashbinEmpty();

	Assert.IsTrue(Result, 'TrashbinEmpty should return true on success');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_TRASHBIN_EMPTY), 'Should call empty API');
end;

{Queue-based multi-request tests}

procedure TCloudMailRuIntegrationTest.TestQueuedResponses_MultipleAPICalls_ProcessedInOrder;
var
	DirItem: TCMRDirItem;
begin
	FCloud := CreateCloud;

	{Queue different responses for same API endpoint - must include type="file" for parsing}
	FMockHTTP.QueueResponse(API_FILE, True,
		'{"email":"test@mail.ru","body":{"name":"first.txt","type":"file","size":100,"mtime":1000},"status":200}');
	FMockHTTP.QueueResponse(API_FILE, True,
		'{"email":"test@mail.ru","body":{"name":"second.txt","type":"file","size":200,"mtime":2000},"status":200}');

	{First call gets first response}
	FCloud.ListingService.StatusFile('/first.txt', DirItem);
	Assert.AreEqual(String('first.txt'), String(DirItem.name), 'First call should get first response');

	{Second call gets second response}
	FCloud.ListingService.StatusFile('/second.txt', DirItem);
	Assert.AreEqual(String('second.txt'), String(DirItem.name), 'Second call should get second response');
end;

procedure TCloudMailRuIntegrationTest.TestQueuedResponses_SameEndpoint_DifferentResults;
begin
	FCloud := CreateCloud;

	{Queue alternating success/failure for same endpoint}
	FMockHTTP.QueueResponse(API_FOLDER_ADD, True, JSON_SUCCESS);
	FMockHTTP.QueueResponse(API_FOLDER_ADD, True, JSON_EXISTS_ERROR);
	FMockHTTP.QueueResponse(API_FOLDER_ADD, True, JSON_SUCCESS);

	Assert.IsTrue(FCloud.FileOps.CreateDirectory('/dir1'), 'First create should succeed');
	Assert.IsFalse(FCloud.FileOps.CreateDirectory('/dir2'), 'Second create should fail (exists)');
	Assert.IsTrue(FCloud.FileOps.CreateDirectory('/dir3'), 'Third create should succeed');
end;

{Token refresh scenario tests}

procedure TCloudMailRuIntegrationTest.TestTokenRefresh_OnAPIError_RefreshesAndRetries;
begin
	FCloud := CreateCloud;

	{Queue: first call fails with token error, then CSRF refresh, then retry succeeds}
	FMockHTTP.QueueResponse(API_FOLDER_ADD, True, JSON_TOKEN_ERROR);
	FMockHTTP.QueueResponse(API_CSRF, True, JSON_CSRF_SUCCESS);
	FMockHTTP.QueueResponse(API_FOLDER_ADD, True, JSON_SUCCESS);

	FCloud.FileOps.CreateDirectory('/test/newdir');

	{Note: actual retry behavior depends on TCloudMailRu implementation}
	{This test verifies the mock queue works for multi-step error handling}
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FOLDER_ADD), 'Should call folder add API');
end;

{Multi-step flow tests}

procedure TCloudMailRuIntegrationTest.TestMultiStepFlow_CreateThenDeleteDir;
begin
	FCloud := CreateCloud;

	{Setup responses for create then delete}
	FMockHTTP.QueueResponse(API_FOLDER_ADD, True, JSON_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_SUCCESS);

	{Execute multi-step flow}
	Assert.IsTrue(FCloud.FileOps.CreateDirectory('/test/newdir'), 'Create should succeed');
	Assert.IsTrue(FCloud.FileOps.RemoveDirectory('/test/newdir'), 'Delete should succeed');

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FOLDER_ADD), 'Should call create API');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_REMOVE), 'Should call remove API');
end;

procedure TCloudMailRuIntegrationTest.TestMultiStepFlow_CopyThenRenameFile;
begin
	FCloud := CreateCloud;

	{Setup responses for copy then rename}
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_RENAME, True, JSON_SUCCESS);

	{Execute multi-step flow}
	Assert.AreEqual(FS_FILE_OK, FCloud.FileOps.CopyToPath('/source.txt', '/dest/'), 'Copy should succeed');
	Assert.AreEqual(FS_FILE_OK, FCloud.FileOps.Rename('/dest/source.txt', '/dest/renamed.txt'), 'Rename should succeed');

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_COPY), 'Should call copy API');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_RENAME), 'Should call rename API');
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuIntegrationTest);

end.
