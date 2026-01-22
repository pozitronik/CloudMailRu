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
	CMROAuth,
	CMRConstants,
	CMRFileIdentity,
	PLUGIN_TYPES,
	SETTINGS_CONSTANTS,
	TCLogger,
	TCProgress,
	TCRequest,
	WindowsFileSystem,
	CloudHTTP,
	MockCloudHTTP,
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
		FUploader: TTestableCloudFileUploader;
		FShardManager: ICloudShardManager;
		FHashCalculator: ICloudHashCalculator;
		FSettings: TUploadSettings;
		FTempDir: string;
		FOAuthToken: TCMROAuth;
		FRetryOperation: TRetryOperation;
		FIsPublicAccount: Boolean;
		FAddByIdentityFirstResult: Integer;  {Result for first call - used to control CloudResultToFsResult}
		FAddByIdentityNextResult: Integer;   {Result for subsequent calls}
		FAddByIdentityCallCount: Integer;
		FDeleteFileResult: Boolean;
		FDeleteFileCalled: Boolean;

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
	end;

implementation

uses
	FileSplitInfo,
	FileHelper;

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

{TTestableCloudFileUploader}

function TTestableCloudFileUploader.TestPutFileSplit(LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
begin
	Result := inherited PutFileSplit(LocalPath, RemotePath, ConflictMode, ChunkOverwriteMode);
end;

const
	SHA1_HASH_40 = 'ABCD1234567890ABCD1234567890ABCD12345678';

{TCloudFileUploaderSplitTest}

procedure TCloudFileUploaderSplitTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPRef := FMockHTTP; {Hold interface reference to prevent premature destruction}
	FMockHTTP.SetDefaultResponse(True, 'https://upload.shard/path 127.0.0.1 1');
	{Set up response for /file/add endpoint used by AddFileByIdentity - needs valid JSON}
	FMockHTTP.SetResponse('/file/add', True, '{"status":200,"body":"ok"}');
	FTempDir := TPath.Combine(TPath.GetTempPath, 'CloudFileUploaderTest_' + TGUID.NewGuid.ToString);
	TDirectory.CreateDirectory(FTempDir);

	FShardManager := TCloudShardManager.Create(TNullLogger.Create,
		function(const URL, Data: WideString; var Answer: WideString): Boolean begin Result := True; end,
		function(const JSON, ErrorPrefix: WideString): Boolean begin Result := True; end,
		function: WideString begin Result := ''; end, '', '');
	FShardManager.SetUploadShard('https://upload.shard/');
	FHashCalculator := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	FOAuthToken.access_token := 'test_token';
	FOAuthToken.refresh_token := 'test_refresh';

	FIsPublicAccount := False;
	FAddByIdentityFirstResult := FS_FILE_OK;
	FAddByIdentityNextResult := FS_FILE_OK;
	FAddByIdentityCallCount := 0;
	FDeleteFileResult := True;
	FDeleteFileCalled := False;

	{Create retry operation - callbacks for token refresh and result mapping}
	FRetryOperation := TRetryOperation.Create(
		function: Boolean begin Result := True; end,
		function(const URL, Data: WideString; var Answer: WideString): Boolean begin Result := True; end,
		function(const URL: WideString; var JSON: WideString; var ShowProgress: Boolean): Boolean begin Result := True; end,
		function(const JSON, ErrorPrefix: WideString): Boolean begin Result := True; end,
		function(const JSON, ErrorPrefix: WideString): Integer begin Result := FS_FILE_OK; end,
		3
	);

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
	if Assigned(FRetryOperation) then
		FRetryOperation.Free;
	FMockHTTP := nil;
	FMockHTTPRef := nil; {Release interface reference - triggers destruction}
	CleanupTempFiles;
end;

function TCloudFileUploaderSplitTest.CreateUploader: TTestableCloudFileUploader;
var
	TestSelf: TCloudFileUploaderSplitTest;
begin
	TestSelf := Self;
	Result := TTestableCloudFileUploader.Create(
		function: ICloudHTTP begin Result := TestSelf.FMockHTTP; end,
		FShardManager,
		FHashCalculator,
		nil, {No cipher}
		TRealSizeFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		function: TCMROAuth begin Result := TestSelf.FOAuthToken; end,
		function: Boolean begin Result := TestSelf.FIsPublicAccount; end,
		function: TRetryOperation begin Result := TestSelf.FRetryOperation; end,
		function: WideString begin Result := 'token=test&x-email=test@mail.ru'; end,
		function(JSON: WideString; ErrorPrefix: WideString): Integer
		begin
			Inc(TestSelf.FAddByIdentityCallCount);
			if TestSelf.FAddByIdentityCallCount = 1 then
				Result := TestSelf.FAddByIdentityFirstResult
			else
				Result := TestSelf.FAddByIdentityNextResult;
		end,
		function(Path: WideString): Boolean begin TestSelf.FDeleteFileCalled := True; Result := TestSelf.FDeleteFileResult; end,
		False, {DoCryptFiles}
		False, {DoCryptFilenames}
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
	FAddByIdentityFirstResult := FS_FILE_OK; {Hash match found - file already exists}

	FUploader := CreateUploader;
	LocalPath := CreateTestFile('dedup.txt', 512);

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/dedup.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{When hash dedup succeeds, no upload should occur}
	Assert.AreEqual(CLOUD_OPERATION_OK, ResultCode, 'Hash dedup should return OK');
	Assert.IsTrue(FAddByIdentityCallCount > 0, 'AddFileByIdentity should have been called for dedup check');
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
	FAddByIdentityFirstResult := FS_FILE_EXISTS;
	FAddByIdentityNextResult := FS_FILE_OK; {CRC file registration succeeds}

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
	FAddByIdentityFirstResult := FS_FILE_EXISTS;

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
	FAddByIdentityFirstResult := FS_FILE_EXISTS;
	FAddByIdentityNextResult := FS_FILE_OK;
	FDeleteFileResult := True;

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/overwrite.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{In overwrite mode, should delete existing and retry successfully}
	Assert.IsTrue(FDeleteFileCalled, 'Should call delete when EXISTS returned');
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
	FAddByIdentityFirstResult := FS_FILE_EXISTS;
	FAddByIdentityNextResult := FS_FILE_EXISTS; {Won't be reached - aborts on delete failure}
	FDeleteFileResult := False; {Delete will fail}

	ResultCode := FUploader.TestPutFileSplit(LocalPath, '/remote/deletefail.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{When delete fails, should return WRITEERROR}
	Assert.AreEqual(FS_FILE_WRITEERROR, ResultCode, 'Should return WRITEERROR when delete fails');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileUploaderSplitTest);

end.
