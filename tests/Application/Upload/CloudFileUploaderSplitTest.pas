unit CloudFileUploaderSplitTest;

{Tests for TCloudFileUploader chunked upload behavior (PutFileSplit method).
 Tests split upload including retry logic, error handling modes,
 and chunk overwrite scenarios. Uses real temporary files because FileSplitInfo
 and ChunkedFileStream require actual file access.}

interface

uses
	CloudFileUploader,
	CloudShardManager,
	CloudHashCalculator,
	CloudContext,
	CloudOAuth,
	CloudSpace,
	CloudConstants,
	CloudFileIdentity,
	WFXTypes,
	SettingsConstants,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	WindowsFileSystem,
	CloudHTTP,
	MockCloudHTTP,
	MockCloudContext,
	TokenRetryHelper,
	TestHelper,
	System.Classes,
	System.SysUtils,
	System.IOUtils,
	DUnitX.TestFramework;

type
	{File system wrapper that uses real file operations for GetFileSize.
	 Needed because PutFileSplit checks file size via IFileSystem but then
	 uses real file operations via TFileSplitInfo and TChunkedFileStream.}
	TRealSizeFileSystem = class(TInterfacedObject, IFileSystem)
	public
		function FileExists(const Path: WideString): Boolean;
		function GetFileSize(const Path: WideString): Int64;
		procedure CreateEmptyFile(const Path: WideString);
		procedure DeleteFile(const Path: WideString);
		function ReadFileHeader(const Path: WideString; ByteCount: Integer): TBytes;
		function ReadAllText(const Path: WideString; Encoding: TEncoding): WideString;
		function ReadAllLines(const Path: WideString; Encoding: TEncoding): TStringList;
		procedure WriteAllText(const Path: WideString; const Content: WideString; Encoding: TEncoding);
		procedure WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);
		function OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;
		function GetTmpFileName(const Prefix: WideString = ''): WideString;
	end;

	{Testable subclass that exposes protected PutFileSplit method}
	TTestableCloudFileUploader = class(TCloudFileUploader)
	public
		function TestPutFileSplit(LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
	end;

	[TestFixture]
	TCloudFileUploaderSplitTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPRef: ICloudHTTP; {Prevents premature destruction via interface refcounting}
		FMockContext: TMockCloudContext;
		FMockContextRef: ICloudContext;
		FUploader: TTestableCloudFileUploader;
		FShardManager: ICloudShardManager;
		FHashCalculator: ICloudHashCalculator;
		FSettings: TUploadSettings;
		FTempDir: string;
		FRetryOperation: IRetryOperation;

		{Creates TTestableCloudFileUploader instance with test settings}
		function CreateUploader: TTestableCloudFileUploader;

		{Creates temporary test file with specified size}
		function CreateTestFile(const FileName: string; SizeInBytes: Int64): string;

		{Deletes all temporary test files}
		procedure CleanupTempFiles;

		public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Hash deduplication tests - file already exists on cloud by hash}
		[Test]
		procedure TestPutFileSplit_HashDedup_SkipsUploadIfHashMatches;

		{User abort tests}
		[Test]
		procedure TestPutFileSplit_UserAbort_ReturnsAborted;

		{ChunkOverwriteMode tests - AddFileByIdentity returns EXISTS}
		[Test]
		procedure TestPutFileSplit_ChunkExists_IgnoreMode_SkipsChunk;
		[Test]
		procedure TestPutFileSplit_ChunkExists_AbortMode_ReturnsError;
		[Test]
		procedure TestPutFileSplit_ChunkExists_OverwriteMode_DeletesAndRetries;
		[Test]
		procedure TestPutFileSplit_ChunkExists_DeleteFails_ReturnsWriteError;

		{OperationErrorMode tests - upload returns error}
		[Test]
		procedure TestPutFileSplit_UploadError_IgnoreMode_Continues;
		[Test]
		procedure TestPutFileSplit_UploadError_AbortMode_Aborts;
		[Test]
		procedure TestPutFileSplit_UploadError_RetryMode_RetriesAndSucceeds;
		[Test]
		procedure TestPutFileSplit_UploadError_RetryMode_ExhaustsRetries;
	end;

implementation

uses
	FileSplitInfo,
	FileHelper,
	FileCipher;

{TRealSizeFileSystem - only implements GetFileSize with real file access}

function TRealSizeFileSystem.FileExists(const Path: WideString): Boolean;
begin
	Result := System.SysUtils.FileExists(Path);
end;

function TRealSizeFileSystem.GetFileSize(const Path: WideString): Int64;
begin
	{Use real file size for accurate test behavior}
	Result := SizeOfFile(Path);
end;

procedure TRealSizeFileSystem.CreateEmptyFile(const Path: WideString);
begin
	{No-op for tests}
end;

procedure TRealSizeFileSystem.DeleteFile(const Path: WideString);
begin
	{No-op for tests}
end;

function TRealSizeFileSystem.ReadFileHeader(const Path: WideString; ByteCount: Integer): TBytes;
begin
	SetLength(Result, 0);
end;

function TRealSizeFileSystem.ReadAllText(const Path: WideString; Encoding: TEncoding): WideString;
begin
	Result := '';
end;

function TRealSizeFileSystem.ReadAllLines(const Path: WideString; Encoding: TEncoding): TStringList;
begin
	Result := TStringList.Create;
end;

procedure TRealSizeFileSystem.WriteAllText(const Path: WideString; const Content: WideString; Encoding: TEncoding);
begin
	{No-op for tests}
end;

procedure TRealSizeFileSystem.WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);
begin
	{No-op for tests}
end;

function TRealSizeFileSystem.OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;
var
	Stream: TStringStream;
begin
	{Return reader over empty stream}
	Stream := TStringStream.Create('', Encoding);
	Result := TStreamReader.Create(Stream, Encoding, False);
end;

function TRealSizeFileSystem.GetTmpFileName(const Prefix: WideString): WideString;
begin
	Result := '';
end;

{TTestableCloudFileUploader}

function TTestableCloudFileUploader.TestPutFileSplit(LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
begin
	Result := inherited PutFileSplit(LocalPath, RemotePath, ConflictMode, ChunkOverwriteMode);
end;

const
	SHA1_HASH_40 = 'ABCD1234567890ABCD1234567890ABCD12345678';

{TCloudFileUploaderSplitTest}

procedure TCloudFileUploaderSplitTest.Setup;
var
	OAuthToken: TCloudOAuth;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPRef := FMockHTTP; {Hold interface reference to prevent premature destruction}
	FMockHTTP.SetDefaultResponse(True, 'https://upload.shard/path 127.0.0.1 1');
	{Set up response for /file/add endpoint used by AddFileByIdentity - needs valid JSON}
	FMockHTTP.SetResponse('/file/add', True, '{"status":200,"body":"ok"}');

	{Setup mock context}
	FMockContext := TMockCloudContext.Create;
	FMockContextRef := FMockContext;
	FMockContext.SetHTTP(FMockHTTP);
	FMockContext.SetIsPublicAccount(False);
	FMockContext.SetUnitedParams('token=test&x-email=test@mail.ru');
	FMockContext.SetCloudResultToFsResultResult(FS_FILE_OK);
	FMockContext.SetDeleteFileResult(True);

	OAuthToken.access_token := 'test_token';
	OAuthToken.refresh_token := 'test_refresh';
	FMockContext.SetOAuthToken(OAuthToken);

	{Return False for GetUserSpace to skip quota check in tests}
	FMockContext.SetGetUserSpaceResult(False, Default(TCloudSpace));

	FTempDir := TPath.Combine(TPath.GetTempPath, 'CloudFileUploaderTest_' + TGUID.NewGuid.ToString);
	TDirectory.CreateDirectory(FTempDir);

	FShardManager := TCloudShardManager.Create(TNullLogger.Create, FMockContext, '', '');
	FShardManager.SetUploadShard('https://upload.shard/');
	FHashCalculator := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	{Create retry operation with mock context for token refresh and result mapping}
	FRetryOperation := TRetryOperation.Create(FMockContext, 3);

	{Default settings for split upload}
	FSettings.CloudMaxFileSize := 1024; {1KB chunks}
	FSettings.SplitLargeFiles := True;
	FSettings.PrecalculateHash := False;
	FSettings.ForcePrecalculateSize := 0;
	FSettings.CheckCRC := False;
	FSettings.OperationErrorMode := OperationErrorModeAbort;
	FSettings.RetryAttempts := 3;
	FSettings.AttemptWait := 1;
	FSettings.UnlimitedFileSize := False;
end;

procedure TCloudFileUploaderSplitTest.TearDown;
begin
	FreeAndNil(FUploader);
	FShardManager := nil;
	FHashCalculator := nil;
	FRetryOperation := nil;
	FMockHTTP := nil;
	FMockHTTPRef := nil; {Release interface reference - triggers destruction}
	FMockContextRef := nil;
	CleanupTempFiles;
end;

function TCloudFileUploaderSplitTest.CreateUploader: TTestableCloudFileUploader;
begin
	Result := TTestableCloudFileUploader.Create(
		FMockContext,
		FShardManager,
		FHashCalculator,
		TNullCipher.Create,
		TRealSizeFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		FRetryOperation,
		False, {DoCryptFiles}
		FSettings);
end;

function TCloudFileUploaderSplitTest.CreateTestFile(const FileName: string; SizeInBytes: Int64): string;
var
	FileStream: TFileStream;
	Buffer: TBytes;
	BytesRemaining, BytesToWrite: Int64;
begin
	Result := TPath.Combine(FTempDir, FileName);
	SetLength(Buffer, 4096);
	FillChar(Buffer[0], Length(Buffer), $AA); {Fill with recognizable pattern}

	FileStream := TFileStream.Create(Result, fmCreate);
	try
		BytesRemaining := SizeInBytes;
		while BytesRemaining > 0 do
		begin
			BytesToWrite := BytesRemaining;
			if BytesToWrite > Length(Buffer) then
				BytesToWrite := Length(Buffer);
			FileStream.WriteBuffer(Buffer[0], BytesToWrite);
			Dec(BytesRemaining, BytesToWrite);
		end;
	finally
		FileStream.Free;
	end;
end;

procedure TCloudFileUploaderSplitTest.CleanupTempFiles;
begin
	if TDirectory.Exists(FTempDir) then
		TDirectory.Delete(FTempDir, True);
end;

{Hash deduplication tests - file already exists on cloud by hash}

procedure TCloudFileUploaderSplitTest.TestPutFileSplit_HashDedup_SkipsUploadIfHashMatches;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	{Enable hash precalculation so dedup is checked first}
	FSettings.PrecalculateHash := True;
	FMockContext.SetCloudResultToFsResultResult(FS_FILE_OK); {Hash match found - file already exists}

	FUploader := CreateUploader;
	LocalPath := CreateTestFile('dedup.txt', 512);

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/dedup.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{When hash dedup succeeds, no upload should occur}
	Assert.AreEqual(CLOUD_OPERATION_OK, ResultCode, 'Hash dedup should return OK');
	Assert.IsTrue(FMockContext.GetCloudResultToFsResultCallCount > 0, 'AddFileByIdentity should have been called for dedup check');
	Assert.AreEqual(0, FMockHTTP.GetUploadCount, 'No actual upload should occur when dedup succeeds');
end;

{User abort tests}

procedure TCloudFileUploaderSplitTest.TestPutFileSplit_UserAbort_ReturnsAborted;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FUploader := CreateUploader;
	LocalPath := CreateTestFile('aborttest.txt', 512);

	{Configure mock: upload returns user abort}
	FMockHTTP.SetPutFileResponse('', '', FS_FILE_USERABORT);

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/aborttest.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	Assert.AreEqual(FS_FILE_USERABORT, ResultCode, 'Should return USERABORT when user cancels');
end;

{ChunkOverwriteMode tests - AddFileByIdentity returns EXISTS}

procedure TCloudFileUploaderSplitTest.TestPutFileSplit_ChunkExists_IgnoreMode_SkipsChunk;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FUploader := CreateUploader;
	LocalPath := CreateTestFile('ignorechunk.txt', 512);

	{Upload succeeds, but AddFileByIdentity returns EXISTS for chunk, OK for CRC}
	FMockHTTP.SetPutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockContext.QueueCloudResultToFsResult(FS_FILE_EXISTS); {First call returns EXISTS}
	FMockContext.SetCloudResultToFsResultResult(FS_FILE_OK); {Subsequent calls return OK}

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/ignorechunk.txt', CLOUD_CONFLICT_STRICT, ChunkOverwriteIgnore);

	{In ignore mode, EXISTS is treated as success and continues}
	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Should succeed with ignored existing chunk');
end;

procedure TCloudFileUploaderSplitTest.TestPutFileSplit_ChunkExists_AbortMode_ReturnsError;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FUploader := CreateUploader;
	LocalPath := CreateTestFile('abortchunk.txt', 512);

	{Upload succeeds, but AddFileByIdentity returns EXISTS}
	FMockHTTP.SetPutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockContext.SetCloudResultToFsResultResult(FS_FILE_EXISTS);

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/abortchunk.txt', CLOUD_CONFLICT_STRICT, ChunkOverwriteAbort);

	{In abort mode, EXISTS should cause immediate abort}
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, ResultCode, 'Should return NOTSUPPORTED when chunk exists in abort mode');
end;

procedure TCloudFileUploaderSplitTest.TestPutFileSplit_ChunkExists_OverwriteMode_DeletesAndRetries;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FUploader := CreateUploader;
	LocalPath := CreateTestFile('overwrite.txt', 512);

	{Upload succeeds, AddFileByIdentity returns EXISTS first, then OK after delete}
	FMockHTTP.SetPutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	{First call returns EXISTS triggering delete, subsequent calls return OK after successful delete}
	FMockContext.QueueCloudResultToFsResult(FS_FILE_EXISTS);
	FMockContext.SetCloudResultToFsResultResult(FS_FILE_OK);
	FMockContext.SetDeleteFileResult(True);

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/overwrite.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{In overwrite mode, should delete existing and retry successfully}
	Assert.IsTrue(FMockContext.WasDeleteFileCalled, 'Should call delete when EXISTS returned');
	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Should succeed after delete and retry');
end;

procedure TCloudFileUploaderSplitTest.TestPutFileSplit_ChunkExists_DeleteFails_ReturnsWriteError;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FUploader := CreateUploader;
	LocalPath := CreateTestFile('deletefail.txt', 512);

	{Upload succeeds, but AddFileByIdentity returns EXISTS and delete fails}
	FMockHTTP.SetPutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockContext.SetCloudResultToFsResultResult(FS_FILE_EXISTS);
	FMockContext.SetDeleteFileResult(False); {Delete will fail}

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/deletefail.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{When delete fails, should return WRITEERROR}
	Assert.AreEqual(FS_FILE_WRITEERROR, ResultCode, 'Should return WRITEERROR when delete fails');
end;

{OperationErrorMode tests - upload returns error}

procedure TCloudFileUploaderSplitTest.TestPutFileSplit_UploadError_IgnoreMode_Continues;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	{Configure Ignore mode - continue despite errors}
	FSettings.OperationErrorMode := OperationErrorModeIgnore;
	FUploader := CreateUploader;
	LocalPath := CreateTestFile('ignoreerror.txt', 512);

	{First upload fails, but Ignore mode continues}
	FMockHTTP.SetPutFileResponse('', '', FS_FILE_WRITEERROR);
	FMockContext.SetCloudResultToFsResultResult(FS_FILE_OK); {Registration succeeds after ignored error}

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/ignoreerror.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{In ignore mode, errors should be logged but upload continues}
	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Ignore mode should continue despite errors');
end;

procedure TCloudFileUploaderSplitTest.TestPutFileSplit_UploadError_AbortMode_Aborts;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	{Configure Abort mode}
	FSettings.OperationErrorMode := OperationErrorModeAbort;
	FUploader := CreateUploader;
	LocalPath := CreateTestFile('aborterror.txt', 512);

	{Upload fails - Abort mode should stop immediately}
	FMockHTTP.SetPutFileResponse('', '', FS_FILE_WRITEERROR);

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/aborterror.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{In abort mode, errors should cause immediate abort}
	Assert.AreEqual(FS_FILE_USERABORT, ResultCode, 'Abort mode should return USERABORT on error');
end;

procedure TCloudFileUploaderSplitTest.TestPutFileSplit_UploadError_RetryMode_RetriesAndSucceeds;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	{Configure Retry mode with 3 attempts}
	FSettings.OperationErrorMode := OperationErrorModeRetry;
	FSettings.RetryAttempts := 3;
	FSettings.AttemptWait := 1; {Minimal wait for tests}
	FUploader := CreateUploader;
	LocalPath := CreateTestFile('retryok.txt', 512);

	{Queue responses: first 2 fail, then succeed}
	FMockHTTP.QueuePutFileResponse('', '', FS_FILE_WRITEERROR);
	FMockHTTP.QueuePutFileResponse('', '', FS_FILE_WRITEERROR);
	FMockHTTP.QueuePutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockHTTP.QueuePutFileResponse('', SHA1_HASH_40, FS_FILE_OK); {CRC file upload}
	FMockContext.SetCloudResultToFsResultResult(FS_FILE_OK);

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/retryok.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{Retry mode should eventually succeed after retries}
	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Retry mode should succeed after retries');
end;

procedure TCloudFileUploaderSplitTest.TestPutFileSplit_UploadError_RetryMode_ExhaustsRetries;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	{Configure Retry mode with limited attempts}
	FSettings.OperationErrorMode := OperationErrorModeRetry;
	FSettings.RetryAttempts := 2;
	FSettings.AttemptWait := 1;
	FUploader := CreateUploader;
	LocalPath := CreateTestFile('retryexhaust.txt', 512);

	{All uploads fail - should exhaust retries}
	FMockHTTP.SetPutFileResponse('', '', FS_FILE_WRITEERROR);

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/retryexhaust.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{After exhausting retries, should return failure}
	Assert.AreEqual(CLOUD_OPERATION_FAILED, ResultCode, 'Should fail after exhausting retries');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileUploaderSplitTest);

end.
