unit CloudFileUploaderTest;

interface

uses
	CloudFileUploader,
	CloudShardManager,
	CloudHashCalculator,
	CMROAuth,
	CMRFileIdentity,
	CMRConstants,
	PLUGIN_TYPES,
	SETTINGS_CONSTANTS,
	TCLogger,
	TCProgress,
	TCRequest,
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
		FOAuthToken: TCMROAuth;
		FSettings: TUploadSettings;
		FAddByIdentityCalled: Boolean;
		FDeleteFileCalled: Boolean;
		FRetryOperation: TRetryOperation;

		function GetHTTP: ICloudHTTP;
		function GetOAuthToken: TCMROAuth;
		function IsPublicAccount: Boolean;
		function GetRetryOperation: TRetryOperation;
		function AddFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath, ConflictMode: WideString; LogErrors, LogSuccess: Boolean): Integer;
		function DeleteFile(Path: WideString): Boolean;
		function FileIdentity(LocalPath: WideString): TCMRFileIdentity;
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

	FAddByIdentityCalled := False;
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
		nil, {No cipher for basic tests}
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		GetOAuthToken,
		IsPublicAccount,
		GetRetryOperation,
		AddFileByIdentity,
		DeleteFile,
		FileIdentity,
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

function TCloudFileUploaderTest.GetOAuthToken: TCMROAuth;
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

function TCloudFileUploaderTest.AddFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath, ConflictMode: WideString; LogErrors, LogSuccess: Boolean): Integer;
begin
	FAddByIdentityCalled := True;
	Result := FS_FILE_OK;
end;

function TCloudFileUploaderTest.DeleteFile(Path: WideString): Boolean;
begin
	FDeleteFileCalled := True;
	Result := True;
end;

function TCloudFileUploaderTest.FileIdentity(LocalPath: WideString): TCMRFileIdentity;
begin
	Result.Hash := '';
	Result.size := 0;
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
		GetOAuthToken,
		IsPublicAccount,
		GetRetryOperation,
		AddFileByIdentity,
		DeleteFile,
		FileIdentity,
		False,
		False,
		FSettings
	);

	{Use existing test file - it will be larger than 1 byte}
	LargeFilePath := DataPath('SIMPLE.JSON');
	UploadResult := FUploader.Upload(LargeFilePath, '/test/large.txt');
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, UploadResult, 'Large file without split enabled should return FS_FILE_NOTSUPPORTED');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileUploaderTest);

end.
