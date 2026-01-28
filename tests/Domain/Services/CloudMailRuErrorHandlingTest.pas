unit CloudMailRuErrorHandlingTest;

{Tests for TCloudMailRu error handling and result mapping.
 Verifies correct handling of API errors, HTTP failures, and error code translation.}

interface

uses
	CloudMailRu,
	CloudSettings,
	CloudDirItemList,
	CloudOperationResult,
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
	DUnitX.TestFramework,
	OpenSSLProvider;

type
	{Testable subclass that exposes protected members}
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
		{Expose CloudResultToFsResult for testing}
		function TestCloudResultToFsResult(JSON: WideString): Integer;
		function TestCloudResultToBoolean(JSON: WideString): Boolean;
	end;

	[TestFixture]
	TCloudMailRuErrorHandlingTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TTestableCloudMailRu;
		FSettings: TCloudSettings;

		function CreateCloud: TTestableCloudMailRu;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{CloudResultToFsResult mapping tests}
		[Test]
		procedure TestCloudResultToFsResult_OK_ReturnsFileOK;
		[Test]
		procedure TestCloudResultToFsResult_EXISTS_ReturnsFileExists;
		[Test]
		procedure TestCloudResultToFsResult_REQUIRED_ReturnsWriteError;
		[Test]
		procedure TestCloudResultToFsResult_INVALID_ReturnsWriteError;
		[Test]
		procedure TestCloudResultToFsResult_READONLY_ReturnsWriteError;
		[Test]
		procedure TestCloudResultToFsResult_OVERQUOTA_ReturnsWriteError;
		[Test]
		procedure TestCloudResultToFsResult_NAME_TOO_LONG_ReturnsWriteError;
		[Test]
		procedure TestCloudResultToFsResult_UNKNOWN_ReturnsNotSupported;
		[Test]
		procedure TestCloudResultToFsResult_UnknownError_ReturnsNotSupported;

		{CloudResultToBoolean mapping tests}
		[Test]
		procedure TestCloudResultToBoolean_OK_ReturnsTrue;
		[Test]
		procedure TestCloudResultToBoolean_Error_ReturnsFalse;
		[Test]
		procedure TestCloudResultToBoolean_InvalidJSON_ReturnsFalse;

		{HTTP failure scenarios}
		[Test]
		procedure TestCreateDir_HTTPFailure_ReturnsFalse;
		[Test]
		procedure TestDeleteFile_HTTPFailure_ReturnsFalse;
		[Test]
		procedure TestCopyFile_HTTPFailure_ReturnsError;
		[Test]
		procedure TestGetDirListing_HTTPFailure_ReturnsFalse;

		{Invalid JSON responses}
		[Test]
		procedure TestCreateDir_InvalidJSON_ReturnsFalse;
		[Test]
		procedure TestGetDirListing_InvalidJSON_ReturnsFalse;

		{Specific error scenarios}
		[Test]
		procedure TestCopyFile_Overquota_ReturnsWriteError;
		[Test]
		procedure TestCopyFile_NameTooLong_ReturnsWriteError;
		[Test]
		procedure TestCreateDir_AlreadyExists_ReturnsFalse;
		[Test]
		procedure TestDeleteFile_NotExists_ReturnsFalse;
		[Test]
		procedure TestRenameFile_Readonly_ReturnsWriteError;
	end;

implementation

const
	{JSON responses for different error scenarios.
	 Note: Errors must be inside body.home.error structure for proper parsing.}
	JSON_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';
	JSON_ERROR_EXISTS = '{"email":"test@mail.ru","body":{"home":{"error":"exists"}},"status":400}';
	JSON_ERROR_NOT_EXISTS = '{"email":"test@mail.ru","body":{"home":{"error":"not_exists"}},"status":404}';
	JSON_ERROR_REQUIRED = '{"email":"test@mail.ru","body":{"home":{"error":"required"}},"status":400}';
	JSON_ERROR_INVALID = '{"email":"test@mail.ru","body":{"home":{"error":"invalid"}},"status":400}';
	JSON_ERROR_READONLY = '{"email":"test@mail.ru","body":{"home":{"error":"readonly"}},"status":403}';
	JSON_ERROR_OVERQUOTA = '{"email":"test@mail.ru","body":{},"status":507}'; {507 has special handling}
	JSON_ERROR_NAME_TOO_LONG = '{"email":"test@mail.ru","body":{"home":{"error":"name_too_long"}},"status":400}';
	JSON_ERROR_UNKNOWN = '{"email":"test@mail.ru","body":{"home":{"error":"unknown"}},"status":500}';
	JSON_ERROR_GENERIC = '{"email":"test@mail.ru","body":{"home":{"error":"some_other_error"}},"status":500}';
	JSON_INVALID = 'not valid json {{{';

{TTestableCloudMailRu}

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

function TTestableCloudMailRu.TestCloudResultToFsResult(JSON: WideString): Integer;
begin
	Result := CloudResultToFsResult(JSON, '');
end;

function TTestableCloudMailRu.TestCloudResultToBoolean(JSON: WideString): Boolean;
begin
	Result := CloudResultToBoolean(JSON, '');
end;

{TCloudMailRuErrorHandlingTest}

procedure TCloudMailRuErrorHandlingTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
end;

procedure TCloudMailRuErrorHandlingTest.TearDown;
begin
	FCloud.Free;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCloudMailRuErrorHandlingTest.CreateCloud: TTestableCloudMailRu;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := False;

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
		TNullCipher.Create, TNullOpenSSLProvider.Create);

	Result.SetUnitedParams('api=2&access_token=test_token');
end;

{CloudResultToFsResult mapping tests}

procedure TCloudMailRuErrorHandlingTest.TestCloudResultToFsResult_OK_ReturnsFileOK;
begin
	FCloud := CreateCloud;
	var Result := FCloud.TestCloudResultToFsResult(JSON_SUCCESS);
	Assert.AreEqual(FS_FILE_OK, Result, 'OK status should map to FS_FILE_OK');
end;

procedure TCloudMailRuErrorHandlingTest.TestCloudResultToFsResult_EXISTS_ReturnsFileExists;
begin
	FCloud := CreateCloud;
	var Result := FCloud.TestCloudResultToFsResult(JSON_ERROR_EXISTS);
	Assert.AreEqual(FS_FILE_EXISTS, Result, 'EXISTS error should map to FS_FILE_EXISTS');
end;

procedure TCloudMailRuErrorHandlingTest.TestCloudResultToFsResult_REQUIRED_ReturnsWriteError;
begin
	FCloud := CreateCloud;
	var Result := FCloud.TestCloudResultToFsResult(JSON_ERROR_REQUIRED);
	Assert.AreEqual(FS_FILE_WRITEERROR, Result, 'REQUIRED error should map to FS_FILE_WRITEERROR');
end;

procedure TCloudMailRuErrorHandlingTest.TestCloudResultToFsResult_INVALID_ReturnsWriteError;
begin
	FCloud := CreateCloud;
	var Result := FCloud.TestCloudResultToFsResult(JSON_ERROR_INVALID);
	Assert.AreEqual(FS_FILE_WRITEERROR, Result, 'INVALID error should map to FS_FILE_WRITEERROR');
end;

procedure TCloudMailRuErrorHandlingTest.TestCloudResultToFsResult_READONLY_ReturnsWriteError;
begin
	FCloud := CreateCloud;
	var Result := FCloud.TestCloudResultToFsResult(JSON_ERROR_READONLY);
	Assert.AreEqual(FS_FILE_WRITEERROR, Result, 'READONLY error should map to FS_FILE_WRITEERROR');
end;

procedure TCloudMailRuErrorHandlingTest.TestCloudResultToFsResult_OVERQUOTA_ReturnsWriteError;
begin
	FCloud := CreateCloud;
	var Result := FCloud.TestCloudResultToFsResult(JSON_ERROR_OVERQUOTA);
	Assert.AreEqual(FS_FILE_WRITEERROR, Result, 'OVERQUOTA error should map to FS_FILE_WRITEERROR');
end;

procedure TCloudMailRuErrorHandlingTest.TestCloudResultToFsResult_NAME_TOO_LONG_ReturnsWriteError;
begin
	FCloud := CreateCloud;
	var Result := FCloud.TestCloudResultToFsResult(JSON_ERROR_NAME_TOO_LONG);
	Assert.AreEqual(FS_FILE_WRITEERROR, Result, 'NAME_TOO_LONG error should map to FS_FILE_WRITEERROR');
end;

procedure TCloudMailRuErrorHandlingTest.TestCloudResultToFsResult_UNKNOWN_ReturnsNotSupported;
begin
	FCloud := CreateCloud;
	var Result := FCloud.TestCloudResultToFsResult(JSON_ERROR_UNKNOWN);
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'UNKNOWN error should map to FS_FILE_NOTSUPPORTED');
end;

procedure TCloudMailRuErrorHandlingTest.TestCloudResultToFsResult_UnknownError_ReturnsNotSupported;
begin
	FCloud := CreateCloud;
	{Unrecognized error strings stay as CLOUD_ERROR_UNKNOWN, which maps to FS_FILE_NOTSUPPORTED}
	var Result := FCloud.TestCloudResultToFsResult(JSON_ERROR_GENERIC);
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'Unrecognized errors should map to FS_FILE_NOTSUPPORTED');
end;

{CloudResultToBoolean mapping tests}

procedure TCloudMailRuErrorHandlingTest.TestCloudResultToBoolean_OK_ReturnsTrue;
begin
	FCloud := CreateCloud;
	var Result := FCloud.TestCloudResultToBoolean(JSON_SUCCESS);
	Assert.IsTrue(Result, 'OK status should return True');
end;

procedure TCloudMailRuErrorHandlingTest.TestCloudResultToBoolean_Error_ReturnsFalse;
begin
	FCloud := CreateCloud;
	var Result := FCloud.TestCloudResultToBoolean(JSON_ERROR_EXISTS);
	Assert.IsFalse(Result, 'Error status should return False');
end;

procedure TCloudMailRuErrorHandlingTest.TestCloudResultToBoolean_InvalidJSON_ReturnsFalse;
begin
	FCloud := CreateCloud;
	var Result := FCloud.TestCloudResultToBoolean(JSON_INVALID);
	Assert.IsFalse(Result, 'Invalid JSON should return False');
end;

{HTTP failure scenarios}

procedure TCloudMailRuErrorHandlingTest.TestCreateDir_HTTPFailure_ReturnsFalse;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetDefaultResponse(False, '', FS_FILE_READERROR);

	var Success := FCloud.FileOperations.CreateDirectory('/NewFolder');

	Assert.IsFalse(Success, 'CreateDir should return False on HTTP failure');
end;

procedure TCloudMailRuErrorHandlingTest.TestDeleteFile_HTTPFailure_ReturnsFalse;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetDefaultResponse(False, '', FS_FILE_READERROR);

	var Success := FCloud.FileOperations.Delete('/file.txt');

	Assert.IsFalse(Success, 'DeleteFile should return False on HTTP failure');
end;

procedure TCloudMailRuErrorHandlingTest.TestCopyFile_HTTPFailure_ReturnsError;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetDefaultResponse(False, '', FS_FILE_READERROR);

	var Result := FCloud.FileOperations.CopyToPath('/source.txt', '/dest');

	Assert.AreNotEqual(FS_FILE_OK, Result, 'CopyFile should return error on HTTP failure');
end;

procedure TCloudMailRuErrorHandlingTest.TestGetDirListing_HTTPFailure_ReturnsFalse;
var
	DirListing: TCloudDirItemList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetDefaultResponse(False, '', FS_FILE_READERROR);

	var Success := FCloud.ListingService.GetDirectory('/test', DirListing);

	Assert.IsFalse(Success, 'GetDirListing should return False on HTTP failure');
end;

{Invalid JSON responses}

procedure TCloudMailRuErrorHandlingTest.TestCreateDir_InvalidJSON_ReturnsFalse;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_ADD, True, JSON_INVALID);

	var Success := FCloud.FileOperations.CreateDirectory('/NewFolder');

	Assert.IsFalse(Success, 'CreateDir should return False on invalid JSON');
end;

procedure TCloudMailRuErrorHandlingTest.TestGetDirListing_InvalidJSON_ReturnsFalse;
var
	DirListing: TCloudDirItemList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_INVALID);

	var Success := FCloud.ListingService.GetDirectory('/test', DirListing);

	Assert.IsFalse(Success, 'GetDirListing should return False on invalid JSON');
end;

{Specific error scenarios}

procedure TCloudMailRuErrorHandlingTest.TestCopyFile_Overquota_ReturnsWriteError;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_ERROR_OVERQUOTA);

	var Result := FCloud.FileOperations.CopyToPath('/source.txt', '/dest');

	Assert.AreEqual(FS_FILE_WRITEERROR, Result, 'CopyFile should return FS_FILE_WRITEERROR on overquota');
end;

procedure TCloudMailRuErrorHandlingTest.TestCopyFile_NameTooLong_ReturnsWriteError;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_ERROR_NAME_TOO_LONG);

	var Result := FCloud.FileOperations.CopyToPath('/source.txt', '/dest');

	Assert.AreEqual(FS_FILE_WRITEERROR, Result, 'CopyFile should return FS_FILE_WRITEERROR on name too long');
end;

procedure TCloudMailRuErrorHandlingTest.TestCreateDir_AlreadyExists_ReturnsFalse;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_ADD, True, JSON_ERROR_EXISTS);

	var Success := FCloud.FileOperations.CreateDirectory('/ExistingFolder');

	Assert.IsFalse(Success, 'CreateDir should return False when folder already exists');
end;

procedure TCloudMailRuErrorHandlingTest.TestDeleteFile_NotExists_ReturnsFalse;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_ERROR_NOT_EXISTS);

	var Success := FCloud.FileOperations.Delete('/nonexistent.txt');

	Assert.IsFalse(Success, 'DeleteFile should return False when file does not exist');
end;

procedure TCloudMailRuErrorHandlingTest.TestRenameFile_Readonly_ReturnsWriteError;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FILE_RENAME, True, JSON_ERROR_READONLY);

	var Result := FCloud.FileOperations.Rename('/readonly.txt', 'newname.txt');

	Assert.AreEqual(FS_FILE_WRITEERROR, Result, 'RenameFile should return FS_FILE_WRITEERROR on readonly');
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuErrorHandlingTest);

end.
