unit CloudMailRuFileOperationsTest;

{Tests for TCloudMailRu file operations: CreateDir, DeleteFile, CopyFile, MoveFile, RenameFile.
 Uses mock HTTP to verify request formation and response handling.}

interface

uses
	CloudMailRu,
	CloudSettings,
	CMRConstants,
	PLUGIN_TYPES,
	ILoggerInterface,
	IProgressInterface,
	IRequestInterface,
	IAuthStrategyInterface,
	IFileSystemInterface,
	ICloudHTTPInterface,
	IHTTPManagerInterface,
	MockCloudHTTP,
	MockHTTPManager,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework;

type
	{Testable subclass that exposes protected members for testing}
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
	end;

	[TestFixture]
	TCloudMailRuFileOperationsTest = class
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

		{CreateDir tests}
		[Test]
		procedure TestCreateDir_Success_ReturnsTrue;
		[Test]
		procedure TestCreateDir_Failure_ReturnsFalse;
		[Test]
		procedure TestCreateDir_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestCreateDir_ConstructsCorrectURL;
		[Test]
		procedure TestCreateDir_EncodesPathInPostData;

		{DeleteFile tests}
		[Test]
		procedure TestDeleteFile_Success_ReturnsTrue;
		[Test]
		procedure TestDeleteFile_Failure_ReturnsFalse;
		[Test]
		procedure TestDeleteFile_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestDeleteFile_ConstructsCorrectPostData;

		{CopyFile tests}
		[Test]
		procedure TestCopyFile_Success_ReturnsOK;
		[Test]
		procedure TestCopyFile_Failure_ReturnsError;
		[Test]
		procedure TestCopyFile_PublicAccount_ReturnsNotSupported;
		[Test]
		procedure TestCopyFile_ConstructsCorrectPostData;

		{MoveFile tests}
		[Test]
		procedure TestMoveFile_Success_ReturnsOK;
		[Test]
		procedure TestMoveFile_Failure_ReturnsError;
		[Test]
		procedure TestMoveFile_PublicAccount_ReturnsNotSupported;

		{RenameFile tests}
		[Test]
		procedure TestRenameFile_Success_ReturnsOK;
		[Test]
		procedure TestRenameFile_Failure_ReturnsError;
		[Test]
		procedure TestRenameFile_PublicAccount_ReturnsError;
		[Test]
		procedure TestRenameFile_ConstructsCorrectPostData;

		{Error scenarios}
		[Test]
		procedure TestCreateDir_AlreadyExists_ReturnsFalse;
		[Test]
		procedure TestDeleteFile_NotExists_ReturnsFalse;
	end;

implementation

const
	{Sample API responses}
	JSON_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';
	JSON_FAILURE = '{"email":"test@mail.ru","body":{},"status":400,"error":"invalid"}';
	JSON_EXISTS = '{"email":"test@mail.ru","body":{},"status":400,"error":"exists"}';
	JSON_NOT_EXISTS = '{"email":"test@mail.ru","body":{},"status":404,"error":"not_exists"}';

{TTestableCloudMailRu}

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

{TCloudMailRuFileOperationsTest}

procedure TCloudMailRuFileOperationsTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
end;

procedure TCloudMailRuFileOperationsTest.TearDown;
begin
	FCloud.Free;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCloudMailRuFileOperationsTest.CreateCloud(PublicAccount: Boolean): TTestableCloudMailRu;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := PublicAccount;

	Result := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create);

	Result.SetUnitedParams('api=2&access_token=test_token');
end;

{CreateDir tests}

procedure TCloudMailRuFileOperationsTest.TestCreateDir_Success_ReturnsTrue;
var
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_ADD, True, JSON_SUCCESS);

	Success := FCloud.CreateDir('/NewFolder');

	Assert.IsTrue(Success, 'CreateDir should return True on success');
end;

procedure TCloudMailRuFileOperationsTest.TestCreateDir_Failure_ReturnsFalse;
var
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_ADD, True, JSON_FAILURE);

	Success := FCloud.CreateDir('/NewFolder');

	Assert.IsFalse(Success, 'CreateDir should return False on API error');
end;

procedure TCloudMailRuFileOperationsTest.TestCreateDir_PublicAccount_ReturnsFalse;
var
	Success: Boolean;
begin
	FCloud := CreateCloud(True); {Public account}
	FMockHTTP.SetResponse(API_FOLDER_ADD, True, JSON_SUCCESS);

	Success := FCloud.CreateDir('/NewFolder');

	Assert.IsFalse(Success, 'CreateDir should return False for public accounts');
	Assert.IsFalse(FMockHTTP.WasURLCalled(API_FOLDER_ADD), 'Should not call API for public accounts');
end;

procedure TCloudMailRuFileOperationsTest.TestCreateDir_ConstructsCorrectURL;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_ADD, True, JSON_SUCCESS);

	FCloud.CreateDir('/TestDir');

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FOLDER_ADD), 'Should call folder add API');
end;

procedure TCloudMailRuFileOperationsTest.TestCreateDir_EncodesPathInPostData;
var
	PostedData: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_ADD, True, JSON_SUCCESS);

	FCloud.CreateDir('/Test Folder');

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.IsTrue(Pos(String('home=/'), String(PostedData)) > 0, 'Post data should contain home parameter');
end;

{DeleteFile tests}

procedure TCloudMailRuFileOperationsTest.TestDeleteFile_Success_ReturnsTrue;
var
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_SUCCESS);

	Success := FCloud.DeleteFile('/file.txt');

	Assert.IsTrue(Success, 'DeleteFile should return True on success');
end;

procedure TCloudMailRuFileOperationsTest.TestDeleteFile_Failure_ReturnsFalse;
var
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_FAILURE);

	Success := FCloud.DeleteFile('/file.txt');

	Assert.IsFalse(Success, 'DeleteFile should return False on API error');
end;

procedure TCloudMailRuFileOperationsTest.TestDeleteFile_PublicAccount_ReturnsFalse;
var
	Success: Boolean;
begin
	FCloud := CreateCloud(True);
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_SUCCESS);

	Success := FCloud.DeleteFile('/file.txt');

	Assert.IsFalse(Success, 'DeleteFile should return False for public accounts');
end;

procedure TCloudMailRuFileOperationsTest.TestDeleteFile_ConstructsCorrectPostData;
var
	PostedData: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_SUCCESS);

	FCloud.DeleteFile('/path/to/file.txt');

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.IsTrue(Pos(String('home=/'), String(PostedData)) > 0, 'Post data should contain home parameter');
end;

{CopyFile tests}

procedure TCloudMailRuFileOperationsTest.TestCopyFile_Success_ReturnsOK;
var
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_SUCCESS);

	ResultCode := FCloud.CopyFile('/source/file.txt', '/destination');

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'CopyFile should return FS_FILE_OK on success');
end;

procedure TCloudMailRuFileOperationsTest.TestCopyFile_Failure_ReturnsError;
var
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_FAILURE);

	ResultCode := FCloud.CopyFile('/source/file.txt', '/destination');

	Assert.AreNotEqual(FS_FILE_OK, ResultCode, 'CopyFile should return error on API failure');
end;

procedure TCloudMailRuFileOperationsTest.TestCopyFile_PublicAccount_ReturnsNotSupported;
var
	ResultCode: Integer;
begin
	FCloud := CreateCloud(True);
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_SUCCESS);

	ResultCode := FCloud.CopyFile('/source/file.txt', '/destination');

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, ResultCode, 'CopyFile should return FS_FILE_NOTSUPPORTED for public accounts');
end;

procedure TCloudMailRuFileOperationsTest.TestCopyFile_ConstructsCorrectPostData;
var
	PostedData: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_SUCCESS);

	FCloud.CopyFile('/source/file.txt', '/destination');

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.IsTrue(Pos(String('home=/'), String(PostedData)) > 0, 'Post data should contain home parameter');
	Assert.IsTrue(Pos(String('folder=/'), String(PostedData)) > 0, 'Post data should contain folder parameter');
end;

{MoveFile tests}

procedure TCloudMailRuFileOperationsTest.TestMoveFile_Success_ReturnsOK;
var
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_SUCCESS);

	ResultCode := FCloud.MoveFile('/source/file.txt', '/destination');

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'MoveFile should return FS_FILE_OK on success');
end;

procedure TCloudMailRuFileOperationsTest.TestMoveFile_Failure_ReturnsError;
var
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_FAILURE);

	ResultCode := FCloud.MoveFile('/source/file.txt', '/destination');

	Assert.AreNotEqual(FS_FILE_OK, ResultCode, 'MoveFile should return error on API failure');
end;

procedure TCloudMailRuFileOperationsTest.TestMoveFile_PublicAccount_ReturnsNotSupported;
var
	ResultCode: Integer;
begin
	FCloud := CreateCloud(True);
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_SUCCESS);

	ResultCode := FCloud.MoveFile('/source/file.txt', '/destination');

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, ResultCode, 'MoveFile should return FS_FILE_NOTSUPPORTED for public accounts');
end;

{RenameFile tests}

procedure TCloudMailRuFileOperationsTest.TestRenameFile_Success_ReturnsOK;
var
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_RENAME, True, JSON_SUCCESS);

	ResultCode := FCloud.RenameFile('/path/oldname.txt', 'newname.txt');

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'RenameFile should return FS_FILE_OK on success');
end;

procedure TCloudMailRuFileOperationsTest.TestRenameFile_Failure_ReturnsError;
var
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_RENAME, True, JSON_FAILURE);

	ResultCode := FCloud.RenameFile('/path/oldname.txt', 'newname.txt');

	Assert.AreNotEqual(FS_FILE_OK, ResultCode, 'RenameFile should return error on API failure');
end;

procedure TCloudMailRuFileOperationsTest.TestRenameFile_PublicAccount_ReturnsError;
var
	ResultCode: Integer;
begin
	FCloud := CreateCloud(True);
	FMockHTTP.SetResponse(API_FILE_RENAME, True, JSON_SUCCESS);

	ResultCode := FCloud.RenameFile('/path/oldname.txt', 'newname.txt');

	Assert.AreEqual(FS_FILE_WRITEERROR, ResultCode, 'RenameFile should return FS_FILE_WRITEERROR for public accounts');
end;

procedure TCloudMailRuFileOperationsTest.TestRenameFile_ConstructsCorrectPostData;
var
	PostedData: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_RENAME, True, JSON_SUCCESS);

	FCloud.RenameFile('/path/oldname.txt', 'newname.txt');

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.IsTrue(Pos(String('home='), String(PostedData)) > 0, 'Post data should contain home parameter');
	Assert.IsTrue(Pos(String('name='), String(PostedData)) > 0, 'Post data should contain name parameter');
end;

{Error scenarios}

procedure TCloudMailRuFileOperationsTest.TestCreateDir_AlreadyExists_ReturnsFalse;
var
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_ADD, True, JSON_EXISTS);

	Success := FCloud.CreateDir('/ExistingFolder');

	Assert.IsFalse(Success, 'CreateDir should return False when folder already exists');
end;

procedure TCloudMailRuFileOperationsTest.TestDeleteFile_NotExists_ReturnsFalse;
var
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_NOT_EXISTS);

	Success := FCloud.DeleteFile('/nonexistent.txt');

	Assert.IsFalse(Success, 'DeleteFile should return False when file does not exist');
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuFileOperationsTest);

end.
