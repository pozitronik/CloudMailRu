unit CloudFileUploaderTest;

interface

uses
	CloudFileUploader,
	CloudShardManager,
	CloudEndpoints,
	CloudHashCalculator,
	CloudContext,
	CloudOAuth,
	CloudSpace,
	CloudFileIdentity,
	CloudConstants,
	WFXTypes,
	SettingsConstants,
	Logger,
	Progress,
	Request,
	TCHandler,
	Cipher,
	CloudHTTP,
	FileSystem,
	MockCloudHTTP,
	MockCloudContext,
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
		FMockContext: TMockCloudContext;
		FMockContextRef: ICloudContext;
		FSettings: TUploadSettings;
		FRetryOperation: IRetryOperation;
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

		{ PutFileWhole tests }
		[Test]
		procedure TestUpload_NormalFile_ReturnsOK;
		[Test]
		procedure TestUpload_FileOpenError_ReturnsReadError;

		{ CRC check tests }
		[Test]
		procedure TestUpload_CRCMismatch_ReturnsFailed;

		{ Hash precalculation dedup tests }
		[Test]
		procedure TestUpload_PrecalculateHash_DedupeSuccess_SkipsUpload;
		[Test]
		procedure TestUpload_PrecalculateHash_DedupeExists_ReturnsExists;

		{ UploadStream tests }
		[Test]
		procedure TestUploadStream_Success_ReturnsOK;
		[Test]
		procedure TestUploadStream_WithKnownHash_SkipsDedupAttempt;
	end;

implementation

{ TCloudFileUploaderTest }

procedure TCloudFileUploaderTest.Setup;
var
	OAuthToken: TCloudOAuth;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTP.SetDefaultResponse(True, 'https://upload.shard/path 127.0.0.1 1');
	{Set up successful upload response - returns 40-char hash}
	FMockHTTP.SetPutFileResponse('', 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', FS_FILE_OK);

	{Setup mock context}
	FMockContext := TMockCloudContext.Create;
	FMockContextRef := FMockContext;
	FMockContext.SetHTTP(FMockHTTP);
	FMockContext.SetIsPublicAccount(False);
	FMockContext.SetUnitedParams('token=test&x-email=test@mail.ru');

	OAuthToken.access_token := 'test_token';
	OAuthToken.refresh_token := 'test_refresh';
	FMockContext.SetOAuthToken(OAuthToken);

	{Return False for GetUserSpace to skip quota check in tests}
	FMockContext.SetGetUserSpaceResult(False, Default(TCloudSpace));

	FShardManager := TCloudShardManager.Create(TNullLogger.Create, FMockContext, TCloudEndpoints.CreateDefaults);
	FShardManager.SetUploadShard('https://upload.shard/');

	FHashCalculator := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);

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

	{Create retry operation with mock context for token refresh and result mapping}
	FRetryOperation := TRetryOperation.Create(FMockContext, 3);

	FUploader := TCloudFileUploader.Create(
		FMockContext,
		FShardManager,
		FHashCalculator,
		TNullCipher.Create,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		FRetryOperation,
		False, {DoCryptFiles}
		FSettings
	);
end;

procedure TCloudFileUploaderTest.TearDown;
begin
	FUploader := nil;
	FShardManager := nil;
	FHashCalculator := nil;
	FRetryOperation := nil;
	FMockContextRef := nil;
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
	FMockContext.SetIsPublicAccount(True);
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
		FMockContext,
		FShardManager,
		FHashCalculator,
		nil,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		FRetryOperation,
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
		FMockContext,
		FShardManager,
		FHashCalculator,
		TNullCipher.Create,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		FRetryOperation,
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
	FMockContext.SetIsPublicAccount(True);
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

{ PutFileWhole tests }

procedure TCloudFileUploaderTest.TestUpload_NormalFile_ReturnsOK;
var
	UploadResult: Integer;
begin
	{Normal upload: file size <= CloudMaxFileSize -> PutFileWhole -> PutFileStream -> PutFileToCloud}
	FSettings.CloudMaxFileSize := 100 * 1024 * 1024;
	FMockHTTP.SetResponse('/file/add', True, '{"status":200,"body":{"home":"/test/file.txt"}}');

	FUploader := TCloudFileUploader.Create(
		FMockContext, FShardManager, FHashCalculator,
		TNullCipher.Create, TWindowsFileSystem.Create,
		TNullLogger.Create, TNullProgress.Create, TNullRequest.Create,
		TNullTCHandler.Create, FRetryOperation, False, FSettings);

	UploadResult := FUploader.Upload(DataPath('SIMPLE.JSON'), '/test/file.txt');
	Assert.AreEqual(FS_FILE_OK, UploadResult, 'Normal file upload should succeed');
end;

procedure TCloudFileUploaderTest.TestUpload_FileOpenError_ReturnsReadError;
var
	UploadResult: Integer;
begin
	{Non-existent file -> file open exception in PutFileWhole}
	FSettings.CloudMaxFileSize := 100 * 1024 * 1024;

	UploadResult := FUploader.Upload('C:\nonexistent_dir\missing_file.txt', '/test/missing.txt');
	Assert.AreEqual(FS_FILE_READERROR, UploadResult, 'File open error should return READERROR');
end;

{ CRC check tests }

procedure TCloudFileUploaderTest.TestUpload_CRCMismatch_ReturnsFailed;
var
	UploadResult: Integer;
begin
	{CRC check enabled: upload succeeds but returned hash differs from local hash}
	FSettings.CloudMaxFileSize := 100 * 1024 * 1024;
	FSettings.CheckCRC := True;
	FSettings.PrecalculateHash := False;

	{Mock returns a hash that differs from the real file SHA1}
	FMockHTTP.SetPutFileResponse('', 'BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB', FS_FILE_OK);
	FMockHTTP.SetResponse('/file/add', True, '{"status":200,"body":{"home":"/test/file.txt"}}');

	FUploader := TCloudFileUploader.Create(
		FMockContext, FShardManager, FHashCalculator,
		TNullCipher.Create, TWindowsFileSystem.Create,
		TNullLogger.Create, TNullProgress.Create, TNullRequest.Create,
		TNullTCHandler.Create, FRetryOperation, False, FSettings);

	UploadResult := FUploader.Upload(DataPath('SIMPLE.JSON'), '/test/file.txt');
	Assert.AreEqual(CLOUD_OPERATION_FAILED, UploadResult, 'CRC mismatch should return CLOUD_OPERATION_FAILED');
end;

procedure TCloudFileUploaderTest.TestUpload_PrecalculateHash_DedupeSuccess_SkipsUpload;
var
	UploadResult: Integer;
begin
	{PrecalculateHash=True causes hash to be computed, then AddFileByIdentity tries dedup.
	 If server recognizes hash -> FS_FILE_OK -> early exit without actual upload.}
	FSettings.CloudMaxFileSize := 100 * 1024 * 1024;
	FSettings.PrecalculateHash := True;

	{Mock: /file/add succeeds (hash recognized); CloudResultToFsResult returns OK}
	FMockHTTP.SetResponse('/file/add', True, '{"status":200,"body":{"home":"/test/file.txt"}}');
	FMockContext.SetCloudResultToFsResultResult(FS_FILE_OK);

	FUploader := TCloudFileUploader.Create(
		FMockContext, FShardManager, FHashCalculator,
		TNullCipher.Create, TWindowsFileSystem.Create,
		TNullLogger.Create, TNullProgress.Create, TNullRequest.Create,
		TNullTCHandler.Create, FRetryOperation, False, FSettings);

	UploadResult := FUploader.Upload(DataPath('SIMPLE.JSON'), '/test/dedup.txt');
	Assert.AreEqual(FS_FILE_OK, UploadResult, 'Dedup success should skip upload and return OK');
end;

procedure TCloudFileUploaderTest.TestUpload_PrecalculateHash_DedupeExists_ReturnsExists;
var
	UploadResult: Integer;
begin
	{PrecalculateHash dedup where server returns EXISTS (path conflict).}
	FSettings.CloudMaxFileSize := 100 * 1024 * 1024;
	FSettings.PrecalculateHash := True;

	{Mock: /file/add succeeds at HTTP level but CloudResultToFsResult returns EXISTS}
	FMockHTTP.SetResponse('/file/add', True, '{"status":200,"body":"exists"}');
	FMockContext.SetCloudResultToFsResultResult(FS_FILE_EXISTS);

	FUploader := TCloudFileUploader.Create(
		FMockContext, FShardManager, FHashCalculator,
		TNullCipher.Create, TWindowsFileSystem.Create,
		TNullLogger.Create, TNullProgress.Create, TNullRequest.Create,
		TNullTCHandler.Create, FRetryOperation, False, FSettings);

	UploadResult := FUploader.Upload(DataPath('SIMPLE.JSON'), '/test/existing.txt');
	Assert.AreEqual(FS_FILE_EXISTS, UploadResult, 'Dedup with EXISTS conflict should return EXISTS');
end;

{ UploadStream tests }

procedure TCloudFileUploaderTest.TestUploadStream_Success_ReturnsOK;
var
	FileStream: TMemoryStream;
	UploadResult: Integer;
	Content: TBytes;
begin
	FMockHTTP.SetPutFileResponse('', 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', FS_FILE_OK);
	FMockHTTP.SetResponse('/file/add', True, '{"status":200,"body":{"home":"/test/file.txt"}}');

	Content := TEncoding.UTF8.GetBytes('Stream upload content');
	FileStream := TMemoryStream.Create;
	try
		FileStream.Write(Content[0], Length(Content));
		FileStream.Position := 0;
		UploadResult := FUploader.UploadStream('file.txt', '/test/file.txt', FileStream,
			CLOUD_CONFLICT_STRICT, '', '\source\file.txt', '\target\file.txt');
		Assert.AreEqual(FS_FILE_OK, UploadResult, 'Stream upload should succeed');
	finally
		FileStream.Free;
	end;
end;

procedure TCloudFileUploaderTest.TestUploadStream_WithKnownHash_SkipsDedupAttempt;
var
	FileStream: TMemoryStream;
	UploadResult: Integer;
	Content: TBytes;
begin
	{When KnownHash is provided, PutFileStream skips hash calculation and dedup attempt,
		going straight to PutFileToCloud + AddFileByIdentity}
	FMockHTTP.SetPutFileResponse('', 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', FS_FILE_OK);
	FMockHTTP.SetResponse('/file/add', True, '{"status":200,"body":{"home":"/test/file.txt"}}');

	Content := TEncoding.UTF8.GetBytes('Known hash content');
	FileStream := TMemoryStream.Create;
	try
		FileStream.Write(Content[0], Length(Content));
		FileStream.Position := 0;
		UploadResult := FUploader.UploadStream('file.txt', '/test/file.txt', FileStream,
			CLOUD_CONFLICT_STRICT, 'ABCD1234567890ABCD1234567890ABCD12345678',
			'\source\file.txt', '\target\file.txt');
		Assert.AreEqual(FS_FILE_OK, UploadResult, 'Stream upload with known hash should succeed');
	finally
		FileStream.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileUploaderTest);

end.
