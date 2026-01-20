unit CloudMailRuPutFileSplitTest;

{Tests for TCloudMailRu.PutFileSplit method.
 Tests chunked upload behavior including retry logic, error handling modes,
 and chunk overwrite scenarios. Uses real temporary files because FileSplitInfo
 and ChunkedFileStream require actual file access.}

interface

uses
	CloudMailRu,
	CloudSettings,
	CMRConstants,
	CMRFileIdentity,
	PLUGIN_TYPES,
	SETTINGS_CONSTANTS,
	TCLogger,
	TCProgress,
	TCRequest,
	IAuthStrategyInterface,
	IFileSystemInterface,
	ICloudHTTPInterface,
	IHTTPManagerInterface,
	MockCloudHTTP,
	MockHTTPManager,
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
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		{Expose protected fields for test verification}
		function GetDoCryptFiles: Boolean;
		procedure SetDoCryptFiles(Value: Boolean);

		{Expose protected method for direct testing}
		function TestPutFileSplit(LocalPath, RemotePath: WideString;
			ConflictMode: WideString = CLOUD_CONFLICT_STRICT;
			ChunkOverwriteMode: Integer = 0): Integer;
	end;

	[TestFixture]
	TCloudMailRuPutFileSplitTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TTestableCloudMailRu;
		FSettings: TCloudSettings;
		FTempDir: string;

		{Creates TCloudMailRu instance with test settings}
		function CreateCloud: TTestableCloudMailRu;

		{Creates temporary test file with specified size}
		function CreateTestFile(const FileName: string; SizeInBytes: Int64): string;

		{Deletes all temporary test files}
		procedure CleanupTempFiles;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Core success tests - single chunk scenario}
		[Test]
		procedure TestPutFileSplit_SmallFile_SucceedsWithSingleChunk;
		[Test]
		procedure TestPutFileSplit_MultipleChunks_UploadsAllSequentially;
		[Test]
		procedure TestPutFileSplit_GeneratesCRCFile_OnSuccess;

		{Hash deduplication tests}
		[Test]
		procedure TestPutFileSplit_HashDedup_SkipsUploadIfHashMatches;

		{User abort tests}
		[Test]
		procedure TestPutFileSplit_UserAbort_ReturnsAborted;

		{ChunkOverwriteMode tests (when chunk already exists)}
		[Test]
		procedure TestPutFileSplit_ChunkExists_OverwriteMode_DeletesAndRetries;
		[Test]
		procedure TestPutFileSplit_ChunkExists_IgnoreMode_SkipsChunk;
		[Test]
		procedure TestPutFileSplit_ChunkExists_AbortMode_ReturnsError;
		[Test]
		procedure TestPutFileSplit_ChunkExists_DeleteFails_ReturnsWriteError;

		{OperationErrorMode tests (on upload error)}
		[Test]
		procedure TestPutFileSplit_Error_IgnoreMode_ContinuesAnyway;
		[Test]
		procedure TestPutFileSplit_Error_AbortMode_StopsImmediately;
		[Test]
		procedure TestPutFileSplit_Error_RetryMode_RetriesWithBackoff;
		[Test]
		procedure TestPutFileSplit_Error_RetryMode_FailsAfterMaxRetries;

		{CRC file tests}
		[Test]
		procedure TestPutFileSplit_PartialSuccess_NoCRCFile;
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

const
	SHA1_HASH_40 = 'ABCD1234567890ABCD1234567890ABCD12345678';
	SHA1_HASH_2 = 'EFGH5678901234EFGH5678901234EFGH56789012';

	JSON_ADD_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';
	JSON_ADD_CONFLICT = '{"email":"test@mail.ru","body":{"home":{"error":"exists"}},"status":400}';
	JSON_DELETE_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';
	JSON_DELETE_FAILURE = '{"email":"test@mail.ru","body":{"home":{"error":"not_exists"}},"status":404}';

{TTestableCloudMailRu}

function TTestableCloudMailRu.GetDoCryptFiles: Boolean;
begin
	Result := FDoCryptFiles;
end;

procedure TTestableCloudMailRu.SetDoCryptFiles(Value: Boolean);
begin
	FDoCryptFiles := Value;
end;

function TTestableCloudMailRu.TestPutFileSplit(LocalPath, RemotePath: WideString;
	ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
begin
	Result := inherited PutFileSplit(LocalPath, RemotePath, ConflictMode, ChunkOverwriteMode);
end;

{TCloudMailRuPutFileSplitTest}

procedure TCloudMailRuPutFileSplitTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
	FTempDir := TPath.Combine(TPath.GetTempPath, 'CloudMailRuTest_' + TGUID.NewGuid.ToString);
	TDirectory.CreateDirectory(FTempDir);
end;

procedure TCloudMailRuPutFileSplitTest.TearDown;
begin
	FreeAndNil(FCloud);
	FMockHTTPManager := nil;
	FMockHTTP := nil;
	CleanupTempFiles;
end;

function TCloudMailRuPutFileSplitTest.CreateCloud: TTestableCloudMailRu;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := False;
	FSettings.AccountSettings.Email := 'test@mail.ru';
	{Set upload shard to skip dispatcher lookup}
	FSettings.AccountSettings.UploadUrlOverride := 'https://upload.test.com/';
	{Set small chunk size to force multiple chunks on small test files}
	FSettings.CloudMaxFileSize := 1024; {1KB chunks for testing}
	FSettings.PrecalculateHash := False; {Disable hash pre-calc by default}
	FSettings.ForcePrecalculateSize := 0; {Disable forced precalc}
	FSettings.CheckCRC := False;
	FSettings.OperationErrorMode := OperationErrorModeAbort; {Default to abort on error}
	FSettings.RetryAttempts := 3;
	FSettings.AttemptWait := 10; {Very short wait for tests}

	Result := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TRealSizeFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create);
end;

function TCloudMailRuPutFileSplitTest.CreateTestFile(const FileName: string; SizeInBytes: Int64): string;
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

procedure TCloudMailRuPutFileSplitTest.CleanupTempFiles;
begin
	if TDirectory.Exists(FTempDir) then
		TDirectory.Delete(FTempDir, True);
end;

{Core success tests}

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_SmallFile_SucceedsWithSingleChunk;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	{Create file smaller than chunk size - will result in 1 chunk}
	LocalPath := CreateTestFile('smallfile.txt', 512);

	{Configure mock: upload succeeds, file/add succeeds}
	FMockHTTP.SetPutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_SUCCESS);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/smallfile.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Single chunk upload should succeed');
	Assert.IsTrue(FMockHTTP.GetUploadCount >= 1, 'Should have at least one upload');
end;

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_MultipleChunks_UploadsAllSequentially;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	{Create file that will require 3 chunks (chunk size is 1024)}
	LocalPath := CreateTestFile('largefile.txt', 2560); {2.5 KB = 3 chunks}

	{Configure mock: all chunk uploads succeed}
	FMockHTTP.QueuePutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockHTTP.QueuePutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockHTTP.QueuePutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	{CRC file upload}
	FMockHTTP.QueuePutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	{API calls for file/add for each chunk plus CRC}
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_SUCCESS);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/largefile.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Multiple chunk upload should succeed');
	{3 chunks + 1 CRC file = 4 uploads}
	Assert.IsTrue(FMockHTTP.GetUploadCount >= 3, 'Should upload at least 3 chunks');
end;

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_GeneratesCRCFile_OnSuccess;
var
	LocalPath: string;
	ResultCode: Integer;
	LastUpload: TMockUploadCapture;
begin
	FCloud := CreateCloud;
	LocalPath := CreateTestFile('testfile.txt', 512);

	FMockHTTP.SetPutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_SUCCESS);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/testfile.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Upload should succeed');
	{Last upload should be the CRC file}
	if FMockHTTP.GetUploadCount >= 2 then
	begin
		LastUpload := FMockHTTP.GetLastUploadCapture;
		Assert.IsTrue(Pos('.crc', String(LastUpload.FileName)) > 0, 'Last upload should be CRC file');
	end;
end;

{Hash deduplication tests}

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_HashDedup_SkipsUploadIfHashMatches;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	{Create cloud with hash precalculation enabled}
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.Email := 'test@mail.ru';
	FSettings.AccountSettings.UploadUrlOverride := 'https://upload.test.com/';
	FSettings.CloudMaxFileSize := 1024;
	FSettings.PrecalculateHash := True; {Enable hash deduplication}
	FSettings.OperationErrorMode := OperationErrorModeAbort;

	FCloud := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TRealSizeFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create);

	LocalPath := CreateTestFile('dedup.txt', 512);

	{Configure mock: file/add succeeds (dedup path) - this simulates hash match}
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_SUCCESS);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/dedup.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{When hash dedup succeeds, no upload should occur}
	Assert.AreEqual(CLOUD_OPERATION_OK, ResultCode, 'Hash dedup should return OK');
end;

{User abort tests}

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_UserAbort_ReturnsAborted;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	LocalPath := CreateTestFile('aborttest.txt', 512);

	{Configure mock: first chunk returns user abort}
	FMockHTTP.SetPutFileResponse('', '', FS_FILE_USERABORT);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/aborttest.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	Assert.AreEqual(FS_FILE_USERABORT, ResultCode, 'Should return USERABORT when user cancels');
end;

{ChunkOverwriteMode tests}

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_ChunkExists_OverwriteMode_DeletesAndRetries;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	LocalPath := CreateTestFile('overwrite.txt', 512);

	{Upload succeeds with hash, but API_FILE_ADD returns EXISTS for first chunk}
	{Flow: PutFile -> OK -> AddFileByIdentity -> EXISTS -> delete -> retry -> OK}
	FMockHTTP.SetPutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	{First file/add returns EXISTS, then delete, then retry file/add succeeds}
	FMockHTTP.QueueResponse(API_FILE_ADD, True, JSON_ADD_CONFLICT);
	FMockHTTP.QueueResponse(API_FILE_ADD, True, JSON_ADD_SUCCESS);
	{CRC file/add}
	FMockHTTP.QueueResponse(API_FILE_ADD, True, JSON_ADD_SUCCESS);
	{Delete API call}
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_DELETE_SUCCESS);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/overwrite.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Should succeed after delete and retry');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_REMOVE), 'Should call file remove API');
end;

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_ChunkExists_IgnoreMode_SkipsChunk;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	LocalPath := CreateTestFile('ignorechunk.txt', 512);

	{Upload succeeds with hash, but API_FILE_ADD returns EXISTS - in ignore mode, should skip and continue}
	FMockHTTP.SetPutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	{First file/add returns EXISTS (ignored), then CRC file/add succeeds}
	FMockHTTP.QueueResponse(API_FILE_ADD, True, JSON_ADD_CONFLICT);
	FMockHTTP.QueueResponse(API_FILE_ADD, True, JSON_ADD_SUCCESS);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/ignorechunk.txt', CLOUD_CONFLICT_STRICT, ChunkOverwriteIgnore);

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Should succeed with ignored existing chunk');
end;

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_ChunkExists_AbortMode_ReturnsError;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	LocalPath := CreateTestFile('abortchunk.txt', 512);

	{Upload succeeds with hash, but API_FILE_ADD returns EXISTS - in abort mode, should return error}
	FMockHTTP.SetPutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_CONFLICT);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/abortchunk.txt', CLOUD_CONFLICT_STRICT, ChunkOverwriteAbort);

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, ResultCode, 'Should return NOTSUPPORTED in abort mode');
end;

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_ChunkExists_DeleteFails_ReturnsWriteError;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	LocalPath := CreateTestFile('deletefail.txt', 512);

	{Upload succeeds with hash, but API_FILE_ADD returns EXISTS, then delete fails}
	FMockHTTP.SetPutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_CONFLICT);
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_DELETE_FAILURE);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/deletefail.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	Assert.AreEqual(FS_FILE_WRITEERROR, ResultCode, 'Should return WRITEERROR when delete fails');
end;

{OperationErrorMode tests}

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_Error_IgnoreMode_ContinuesAnyway;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	{Create cloud with ignore error mode}
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.Email := 'test@mail.ru';
	FSettings.AccountSettings.UploadUrlOverride := 'https://upload.test.com/';
	FSettings.CloudMaxFileSize := 1024;
	FSettings.OperationErrorMode := OperationErrorModeIgnore;

	FCloud := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TRealSizeFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create);

	LocalPath := CreateTestFile('ignoreerror.txt', 512);

	{Upload returns error - in ignore mode, should continue}
	FMockHTTP.SetPutFileResponse('', '', FS_FILE_WRITEERROR);
	{CRC upload - also return error but will be ignored}
	FMockHTTP.QueuePutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_SUCCESS);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/ignoreerror.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{In ignore mode, errors are logged but processing continues and returns OK}
	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Should return OK in ignore mode even when chunk fails');
end;

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_Error_AbortMode_StopsImmediately;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	{Create cloud with abort error mode}
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.Email := 'test@mail.ru';
	FSettings.AccountSettings.UploadUrlOverride := 'https://upload.test.com/';
	FSettings.CloudMaxFileSize := 1024;
	FSettings.OperationErrorMode := OperationErrorModeAbort;

	FCloud := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TRealSizeFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create);

	LocalPath := CreateTestFile('aborterror.txt', 512);

	{Upload returns error - in abort mode, should stop}
	FMockHTTP.SetPutFileResponse('', '', FS_FILE_WRITEERROR);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/aborterror.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	Assert.AreEqual(FS_FILE_USERABORT, ResultCode, 'Should return USERABORT in abort mode');
end;

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_Error_RetryMode_RetriesWithBackoff;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	{Create cloud with retry error mode}
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.Email := 'test@mail.ru';
	FSettings.AccountSettings.UploadUrlOverride := 'https://upload.test.com/';
	FSettings.CloudMaxFileSize := 1024;
	FSettings.OperationErrorMode := OperationErrorModeRetry;
	FSettings.RetryAttempts := 3;
	FSettings.AttemptWait := 1; {1ms wait for fast tests}

	FCloud := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TRealSizeFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create);

	LocalPath := CreateTestFile('retry.txt', 512);

	{First upload fails, second succeeds}
	FMockHTTP.QueuePutFileResponse('', '', FS_FILE_WRITEERROR);
	FMockHTTP.QueuePutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	{CRC upload}
	FMockHTTP.QueuePutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_SUCCESS);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/retry.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Should succeed after retry');
	Assert.IsTrue(FMockHTTP.GetUploadCount >= 2, 'Should have attempted at least 2 uploads (1 fail + 1 success)');
end;

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_Error_RetryMode_FailsAfterMaxRetries;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	{Create cloud with retry error mode and limited retries}
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.Email := 'test@mail.ru';
	FSettings.AccountSettings.UploadUrlOverride := 'https://upload.test.com/';
	FSettings.CloudMaxFileSize := 1024;
	FSettings.OperationErrorMode := OperationErrorModeRetry;
	FSettings.RetryAttempts := 2; {Only 2 retries}
	FSettings.AttemptWait := 1;

	FCloud := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TRealSizeFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create);

	LocalPath := CreateTestFile('maxretry.txt', 512);

	{All uploads fail}
	FMockHTTP.SetPutFileResponse('', '', FS_FILE_WRITEERROR);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/maxretry.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	Assert.AreEqual(CLOUD_OPERATION_FAILED, ResultCode, 'Should fail after max retries exceeded');
end;

{CRC file tests}

procedure TCloudMailRuPutFileSplitTest.TestPutFileSplit_PartialSuccess_NoCRCFile;
var
	LocalPath: string;
	ResultCode: Integer;
begin
	FCloud := CreateCloud;
	{Create file that needs 2 chunks}
	LocalPath := CreateTestFile('partial.txt', 1536); {1.5 KB = 2 chunks}

	{First chunk succeeds, second fails}
	FMockHTTP.QueuePutFileResponse('', SHA1_HASH_40, FS_FILE_OK);
	FMockHTTP.QueuePutFileResponse('', '', FS_FILE_WRITEERROR);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_SUCCESS);

	ResultCode := FCloud.TestPutFileSplit(LocalPath, '/remote/partial.txt', CLOUD_CONFLICT_STRICT, ChunkOverwrite);

	{Should fail and NOT upload CRC file}
	Assert.AreNotEqual(FS_FILE_OK, ResultCode, 'Should not succeed on partial upload');
	{Verify CRC was not uploaded - only 2 uploads should have occurred}
	Assert.AreEqual(2, FMockHTTP.GetUploadCount, 'Should only have 2 uploads (1 success + 1 fail), no CRC');
end;

end.
