unit CloudFileOperationsTest;

interface

uses
	CloudFileOperations,
	CMRConstants,
	WFXTypes,
	TCLogger,
	CloudHTTP,
	MockCloudHTTP,
	TokenRetryHelper,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework;

type
	{Tests for TCloudFileOperations}
	[TestFixture]
	TCloudFileOperationsTest = class
	private
		FService: ICloudFileOperations;
		FMockHTTP: TMockCloudHTTP;
		FIsPublicAccount: Boolean;
		FUnitedParams: WideString;
		FRetryOperation: TRetryOperation;

		function GetHTTP: ICloudHTTP;
		function IsPublicAccount: Boolean;
		function GetUnitedParams: WideString;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Construction tests}
		[Test]
		procedure TestCreate_InitializesCorrectly;

		{CreateDirectory tests}
		[Test]
		procedure TestCreateDirectory_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestCreateDirectory_Success_ReturnsTrue;

		{RemoveDirectory tests}
		[Test]
		procedure TestRemoveDirectory_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestRemoveDirectory_Success_ReturnsTrue;

		{Delete tests}
		[Test]
		procedure TestDelete_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestDelete_Success_ReturnsTrue;

		{Rename tests}
		[Test]
		procedure TestRename_PublicAccount_ReturnsWriteError;
		[Test]
		procedure TestRename_Success_ReturnsOK;

		{MoveToPath tests}
		[Test]
		procedure TestMoveToPath_PublicAccount_ReturnsNotSupported;
		[Test]
		procedure TestMoveToPath_Success_ReturnsOK;

		{CopyToPath tests}
		[Test]
		procedure TestCopyToPath_PublicAccount_ReturnsNotSupported;
		[Test]
		procedure TestCopyToPath_Success_ReturnsOK;

		{Move tests}
		[Test]
		procedure TestMove_SameDirectory_CallsRename;
		[Test]
		procedure TestMove_DifferentDirectory_CallsMoveAndRename;
		[Test]
		procedure TestMove_DifferentDirectorySameName_OnlyCallsMove;

		{Copy tests}
		[Test]
		procedure TestCopy_SameDirectory_ReturnsNotSupported;
		[Test]
		procedure TestCopy_DifferentDirectory_CallsCopyAndRename;
		[Test]
		procedure TestCopy_DifferentDirectorySameName_OnlyCallsCopy;
	end;

implementation

{ TCloudFileOperationsTest }

procedure TCloudFileOperationsTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');

	FIsPublicAccount := False;
	FUnitedParams := 'token=test&x-email=test@mail.ru';

	{Create retry operation for tests}
	FRetryOperation := TRetryOperation.Create(
		function: Boolean begin Result := True; end, {RefreshToken}
		function(const URL, Data: WideString; var Answer: WideString): Boolean begin Result := FMockHTTP.PostForm(URL, Data, Answer); end, {PostForm}
		function(const URL: WideString; var JSON: WideString; var ShowProgress: Boolean): Boolean begin Result := FMockHTTP.GetPage(URL, JSON, ShowProgress); end, {GetPage}
		function(const JSON, ErrorPrefix: WideString): Boolean begin Result := Pos(WideString('"status":200'), JSON) > 0; end, {ToBoolean}
		function(const JSON, ErrorPrefix: WideString): Integer begin if Pos(WideString('"status":200'), JSON) > 0 then Result := FS_FILE_OK else Result := FS_FILE_WRITEERROR; end, {ToInteger}
		3 {MaxRetries}
	);

	FService := TCloudFileOperations.Create(
		GetHTTP,
		TNullLogger.Create,
		FRetryOperation,
		IsPublicAccount,
		GetUnitedParams
	);
end;

procedure TCloudFileOperationsTest.TearDown;
begin
	FService := nil;
	if Assigned(FRetryOperation) then
		FRetryOperation.Free;
end;

function TCloudFileOperationsTest.GetHTTP: ICloudHTTP;
begin
	Result := FMockHTTP;
end;

function TCloudFileOperationsTest.IsPublicAccount: Boolean;
begin
	Result := FIsPublicAccount;
end;

function TCloudFileOperationsTest.GetUnitedParams: WideString;
begin
	Result := FUnitedParams;
end;

{Construction tests}

procedure TCloudFileOperationsTest.TestCreate_InitializesCorrectly;
begin
	Assert.IsNotNull(FService, 'Service should be created');
end;

{CreateDirectory tests}

procedure TCloudFileOperationsTest.TestCreateDirectory_PublicAccount_ReturnsFalse;
var
	Success: Boolean;
begin
	FIsPublicAccount := True;

	Success := FService.CreateDirectory('/test/newdir');

	Assert.IsFalse(Success, 'CreateDirectory should return false for public account');
end;

procedure TCloudFileOperationsTest.TestCreateDirectory_Success_ReturnsTrue;
var
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');

	Success := FService.CreateDirectory('/test/newdir');

	Assert.IsTrue(Success, 'CreateDirectory should return true on success');
end;

{RemoveDirectory tests}

procedure TCloudFileOperationsTest.TestRemoveDirectory_PublicAccount_ReturnsFalse;
var
	Success: Boolean;
begin
	FIsPublicAccount := True;

	Success := FService.RemoveDirectory('/test/dir');

	Assert.IsFalse(Success, 'RemoveDirectory should return false for public account');
end;

procedure TCloudFileOperationsTest.TestRemoveDirectory_Success_ReturnsTrue;
var
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');

	Success := FService.RemoveDirectory('/test/dir');

	Assert.IsTrue(Success, 'RemoveDirectory should return true on success');
end;

{Delete tests}

procedure TCloudFileOperationsTest.TestDelete_PublicAccount_ReturnsFalse;
var
	Success: Boolean;
begin
	FIsPublicAccount := True;

	Success := FService.Delete('/test/file.txt');

	Assert.IsFalse(Success, 'Delete should return false for public account');
end;

procedure TCloudFileOperationsTest.TestDelete_Success_ReturnsTrue;
var
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');

	Success := FService.Delete('/test/file.txt');

	Assert.IsTrue(Success, 'Delete should return true on success');
end;

{Rename tests}

procedure TCloudFileOperationsTest.TestRename_PublicAccount_ReturnsWriteError;
var
	ResultCode: Integer;
begin
	FIsPublicAccount := True;

	ResultCode := FService.Rename('/test/old.txt', 'new.txt');

	Assert.AreEqual(FS_FILE_WRITEERROR, ResultCode, 'Rename should return FS_FILE_WRITEERROR for public account');
end;

procedure TCloudFileOperationsTest.TestRename_Success_ReturnsOK;
var
	ResultCode: Integer;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');

	ResultCode := FService.Rename('/test/old.txt', 'new.txt');

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Rename should return FS_FILE_OK on success');
end;

{MoveToPath tests}

procedure TCloudFileOperationsTest.TestMoveToPath_PublicAccount_ReturnsNotSupported;
var
	ResultCode: Integer;
begin
	FIsPublicAccount := True;

	ResultCode := FService.MoveToPath('/test/file.txt', '/other/');

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, ResultCode, 'MoveToPath should return FS_FILE_NOTSUPPORTED for public account');
end;

procedure TCloudFileOperationsTest.TestMoveToPath_Success_ReturnsOK;
var
	ResultCode: Integer;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');

	ResultCode := FService.MoveToPath('/test/file.txt', '/other/');

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'MoveToPath should return FS_FILE_OK on success');
end;

{CopyToPath tests}

procedure TCloudFileOperationsTest.TestCopyToPath_PublicAccount_ReturnsNotSupported;
var
	ResultCode: Integer;
begin
	FIsPublicAccount := True;

	ResultCode := FService.CopyToPath('/test/file.txt', '/other/');

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, ResultCode, 'CopyToPath should return FS_FILE_NOTSUPPORTED for public account');
end;

procedure TCloudFileOperationsTest.TestCopyToPath_Success_ReturnsOK;
var
	ResultCode: Integer;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');

	ResultCode := FService.CopyToPath('/test/file.txt', '/other/');

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'CopyToPath should return FS_FILE_OK on success');
end;

{Move tests}

procedure TCloudFileOperationsTest.TestMove_SameDirectory_CallsRename;
var
	ResultCode: Integer;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');

	ResultCode := FService.Move('/test/old.txt', '/test/new.txt');

	{Same directory move is a rename operation}
	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Move in same directory should succeed');
	Assert.IsTrue(FMockHTTP.WasURLCalled('rename'), 'Move in same directory should call rename API');
end;

procedure TCloudFileOperationsTest.TestMove_DifferentDirectory_CallsMoveAndRename;
var
	ResultCode: Integer;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');

	ResultCode := FService.Move('/test/old.txt', '/other/new.txt');

	{Different directory with different name requires move + rename}
	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Move to different directory with rename should succeed');
	Assert.IsTrue(FMockHTTP.WasURLCalled('move'), 'Should call move API');
	Assert.IsTrue(FMockHTTP.WasURLCalled('rename'), 'Should call rename API');
end;

procedure TCloudFileOperationsTest.TestMove_DifferentDirectorySameName_OnlyCallsMove;
var
	ResultCode: Integer;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');

	ResultCode := FService.Move('/test/file.txt', '/other/file.txt');

	{Different directory with same name only needs move}
	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Move to different directory with same name should succeed');
	Assert.IsTrue(FMockHTTP.WasURLCalled('move'), 'Should call move API');
end;

{Copy tests}

procedure TCloudFileOperationsTest.TestCopy_SameDirectory_ReturnsNotSupported;
var
	ResultCode: Integer;
begin
	ResultCode := FService.Copy('/test/file.txt', '/test/copy.txt');

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, ResultCode, 'Copy in same directory should return FS_FILE_NOTSUPPORTED');
end;

procedure TCloudFileOperationsTest.TestCopy_DifferentDirectory_CallsCopyAndRename;
var
	ResultCode: Integer;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');

	ResultCode := FService.Copy('/test/old.txt', '/other/new.txt');

	{Different directory with different name requires copy + rename}
	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Copy to different directory with rename should succeed');
	Assert.IsTrue(FMockHTTP.WasURLCalled('copy'), 'Should call copy API');
	Assert.IsTrue(FMockHTTP.WasURLCalled('rename'), 'Should call rename API');
end;

procedure TCloudFileOperationsTest.TestCopy_DifferentDirectorySameName_OnlyCallsCopy;
var
	ResultCode: Integer;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{}}');

	ResultCode := FService.Copy('/test/file.txt', '/other/file.txt');

	{Different directory with same name only needs copy}
	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Copy to different directory with same name should succeed');
	Assert.IsTrue(FMockHTTP.WasURLCalled('copy'), 'Should call copy API');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileOperationsTest);

end.
