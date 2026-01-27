unit CloudFileUploaderTest;

interface

uses
	CloudFileUploader,
	CloudShardManager,
	CloudHashCalculator,
	CloudOAuth,
	CloudSpace,
	CloudFileIdentity,
	CloudConstants,
	WFXTypes,
	SettingsConstants,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	FileCipher,
	CloudHTTP,
	WindowsFileSystem,
	MockCloudHTTP,
	TokenRetryHelper,
	TestHelper,
	System.Classes,
	System.SysUtils,
	DUnitX.TestFramework;

type
	{Tests for TCloudFileUploader service}
	[TestFixture]
	TCloudFileUploaderTest = class
	private
		FUploader: ICloudFileUploader;
		FShardManager: ICloudShardManager;
		FHashCalculator: ICloudHashCalculator;
		FMockHTTP: TMockCloudHTTP;
		FIsPublicAccount: Boolean;
		FOAuthToken: TCloudOAuth;
		FSettings: TUploadSettings;
		FDeleteFileCalled: Boolean;
		FRetryOperation: TRetryOperation;

		function GetHTTP: ICloudHTTP;
		function GetOAuthToken: TCloudOAuth;
		function IsPublicAccount: Boolean;
		function GetRetryOperation: TRetryOperation;
		function GetUnitedParams: WideString;
		function CloudResultToFsResult(JSON: WideString; ErrorPrefix: WideString): Integer;
		function DeleteFile(Path: WideString): Boolean;
		function GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ Construction tests }
		[Test]
		procedure TestCreate_InitializesCorrectly;

		{ Upload tests }
		[Test]
		procedure TestUpload_PublicAccount_ReturnsNotSupported;
		[Test]
		procedure TestUpload_LargeFileWithoutSplit_ReturnsNotSupported;
		[Test]
		procedure TestUpload_LargeFileWithSplit_CallsPutFileSplit;

		{ AddFileByIdentity tests }
		[Test]
		procedure TestAddFileByIdentity_PublicAccount_ReturnsNotSupported;
		[Test]
		procedure TestAddFileByIdentity_Success_ReturnsOK;
		[Test]
		procedure TestAddFileByIdentity_PostFormFails_ReturnsWriteError;
	end;

implementation

{ TCloudFileUploaderTest }

procedure TCloudFileUploaderTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTP.SetDefaultResponse(True, 'https://upload.shard/path 127.0.0.1 1');
	{Set up successful upload response - returns 40-char hash}
	FMockHTTP.SetPutFileResponse('', 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', FS_FILE_OK);

	FShardManager := TCloudShardManager.Create(TNullLogger.Create,
		function(const URL, Data: WideString; var Answer: WideString): Boolean begin Result := True; end,
		function(const JSON, ErrorPrefix: WideString): Boolean begin Result := True; end,
		function: WideString begin Result := ''; end, '', '');
	FShardManager.SetUploadShard('https://upload.shard/');

	FHashCalculator := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	FIsPublicAccount := False;
	FOAuthToken.access_token := 'test_token';
	FOAuthToken.refresh_token := 'test_refresh';

	FDeleteFileCalled := False;

	{Default settings}
	FSettings.PrecalculateHash := False;
	FSettings.ForcePrecalculateSize := 0;
	FSettings.CheckCRC := False;
	FSettings.OperationErrorMode := OperationErrorModeAbort;
	FSettings.RetryAttempts := 3;
	FSettings.AttemptWait := 100;
	FSettings.UnlimitedFileSize := False;
	FSettings.SplitLargeFiles := False;
	FSettings.CloudMaxFileSize := 1024 * 1024; {1 MB for testing}

	{Create retry operation for tests - matching TRetryOperation.Create signature}
	FRetryOperation := TRetryOperation.Create(
		function: Boolean begin Result := True; end, {RefreshToken}
		function(const URL, Data: WideString; var Answer: WideString): Boolean begin Result := True; end, {PostForm}
		function(const URL: WideString; var JSON: WideString; var ShowProgress: Boolean): Boolean begin Result := True; end, {GetPage}
		function(const JSON, ErrorPrefix: WideString): Boolean begin Result := True; end, {ToBoolean}
		function(const JSON, ErrorPrefix: WideString): Integer begin Result := FS_FILE_OK; end, {ToInteger}
		3 {MaxRetries}
	);

	FUploader := TCloudFileUploader.Create(
		GetHTTP,
		FShardManager,
		FHashCalculator,
		TNullCipher.Create,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		GetOAuthToken,
		IsPublicAccount,
		GetRetryOperation,
		GetUnitedParams,
		CloudResultToFsResult,
		DeleteFile,
		GetUserSpace,
		False, {DoCryptFiles}
		False, {DoCryptFilenames}
		FSettings
	);
end;

procedure TCloudFileUploaderTest.TearDown;
begin
	FUploader := nil;
	FShardManager := nil;
	FHashCalculator := nil;
	if Assigned(FRetryOperation) then
		FRetryOperation.Free;
end;

function TCloudFileUploaderTest.GetHTTP: ICloudHTTP;
begin
	Result := FMockHTTP;
end;

function TCloudFileUploaderTest.GetOAuthToken: TCloudOAuth;
begin
	Result := FOAuthToken;
end;

function TCloudFileUploaderTest.IsPublicAccount: Boolean;
begin
	Result := FIsPublicAccount;
end;

function TCloudFileUploaderTest.GetRetryOperation: TRetryOperation;
begin
	Result := FRetryOperation;
end;

function TCloudFileUploaderTest.GetUnitedParams: WideString;
begin
	Result := 'token=test&x-email=test@mail.ru';
end;

function TCloudFileUploaderTest.CloudResultToFsResult(JSON: WideString; ErrorPrefix: WideString): Integer;
begin
	if Pos(WideString('"status":200'), JSON) > 0 then
		Result := FS_FILE_OK
	else
		Result := FS_FILE_WRITEERROR;
end;

function TCloudFileUploaderTest.DeleteFile(Path: WideString): Boolean;
begin
	FDeleteFileCalled := True;
	Result := True;
end;

function TCloudFileUploaderTest.GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
begin
	{Return False to skip quota check in tests}
	Result := False;
end;

{ Construction tests }

procedure TCloudFileUploaderTest.TestCreate_InitializesCorrectly;
begin
	Assert.IsNotNull(FUploader, 'Uploader should be created');
end;

{ Upload tests }

procedure TCloudFileUploaderTest.TestUpload_PublicAccount_ReturnsNotSupported;
var
	Result: Integer;
begin
	FIsPublicAccount := True;
	Result := FUploader.Upload('test.txt', '/test.txt');
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'Public account should return FS_FILE_NOTSUPPORTED');
end;

procedure TCloudFileUploaderTest.TestUpload_LargeFileWithoutSplit_ReturnsNotSupported;
var
	UploadResult: Integer;
	LargeFilePath: WideString;
begin
	{Create a test file that's larger than CloudMaxFileSize (1MB)}
	FSettings.CloudMaxFileSize := 1; {Very small to trigger split logic}
	FSettings.SplitLargeFiles := False;

	{Recreate uploader with new settings}
	FUploader := TCloudFileUploader.Create(
		GetHTTP,
		FShardManager,
		FHashCalculator,
		nil,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		GetOAuthToken,
		IsPublicAccount,
		GetRetryOperation,
		GetUnitedParams,
		CloudResultToFsResult,
		DeleteFile,
		GetUserSpace,
		False,
		False,
		FSettings
	);

	{Use existing test file - it will be larger than 1 byte}
	LargeFilePath := DataPath('SIMPLE.JSON');
	UploadResult := FUploader.Upload(LargeFilePath, '/test/large.txt');
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, UploadResult, 'Large file without split enabled should return FS_FILE_NOTSUPPORTED');
end;

procedure TCloudFileUploaderTest.TestUpload_LargeFileWithSplit_CallsPutFileSplit;
var
	UploadResult: Integer;
	LargeFilePath: WideString;
begin
	{Configure for split upload}
	FSettings.CloudMaxFileSize := 1; {Very small to trigger split logic}
	FSettings.SplitLargeFiles := True;
	FSettings.UnlimitedFileSize := False;

	{Configure mock to return success for uploads}
	FMockHTTP.SetPutFileResponse('', 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', FS_FILE_OK);
	FMockHTTP.SetResponse('/file/add', True, '{"status":200,"body":"ok"}');

	{Recreate uploader with split enabled}
	FUploader := TCloudFileUploader.Create(
		GetHTTP,
		FShardManager,
		FHashCalculator,
		TNullCipher.Create,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		GetOAuthToken,
		IsPublicAccount,
		GetRetryOperation,
		GetUnitedParams,
		CloudResultToFsResult,
		DeleteFile,
		GetUserSpace,
		False,
		False,
		FSettings
	);

	LargeFilePath := DataPath('SIMPLE.JSON');
	UploadResult := FUploader.Upload(LargeFilePath, '/test/large.txt');
	{Split upload path should be exercised}
	Assert.AreEqual(FS_FILE_OK, UploadResult, 'Large file with split enabled should succeed');
end;

{ AddFileByIdentity tests }

procedure TCloudFileUploaderTest.TestAddFileByIdentity_PublicAccount_ReturnsNotSupported;
var
	Identity: TCloudFileIdentity;
	Result: Integer;
begin
	FIsPublicAccount := True;
	Identity.Hash := 'ABCD1234567890ABCD1234567890ABCD12345678';
	Identity.size := 1000;

	Result := FUploader.AddFileByIdentity(Identity, '/test/file.txt');

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'Public account should return FS_FILE_NOTSUPPORTED');
end;

procedure TCloudFileUploaderTest.TestAddFileByIdentity_Success_ReturnsOK;
var
	Identity: TCloudFileIdentity;
	Result: Integer;
begin
	Identity.Hash := 'ABCD1234567890ABCD1234567890ABCD12345678';
	Identity.size := 1000;

	{Configure mock to return success}
	FMockHTTP.SetResponse('/file/add', True, '{"status":200,"body":{"home":"/test/file.txt"}}');

	Result := FUploader.AddFileByIdentity(Identity, '/test/file.txt');

	Assert.AreEqual(FS_FILE_OK, Result, 'Successful AddFileByIdentity should return FS_FILE_OK');
end;

procedure TCloudFileUploaderTest.TestAddFileByIdentity_PostFormFails_ReturnsWriteError;
var
	Identity: TCloudFileIdentity;
	Result: Integer;
begin
	Identity.Hash := 'ABCD1234567890ABCD1234567890ABCD12345678';
	Identity.size := 1000;

	{Configure mock to fail POST}
	FMockHTTP.SetResponse('/file/add', False, '');

	Result := FUploader.AddFileByIdentity(Identity, '/test/file.txt');

	Assert.AreEqual(FS_FILE_WRITEERROR, Result, 'Failed POST should return FS_FILE_WRITEERROR');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileUploaderTest);

end.
