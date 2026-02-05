unit ChunkedUploadHandlerTest;

{Tests for TChunkedUploadHandler covering all code paths including
	error handling modes, quota checks, hash-match skips, and exception handling.
	Uses real temporary files because TFileSplitInfo and TChunkedFileStream
	require actual file access.}

interface

uses
	ChunkedUploadHandler,
	CloudContext,
	CloudSpace,
	CloudFileIdentity,
	CloudHashCalculator,
	CloudHTTP,
	WFXTypes,
	SettingsConstants,
	CloudConstants,
	Logger,
	Progress,
	Request,
	TCHandler,
	FileSystem,
	MockCloudContext,
	MockCloudHTTP,
	System.Classes,
	System.SysUtils,
	System.IOUtils,
	Winapi.Windows,
	DUnitX.TestFramework;

type
	{Real file system for GetFileSize - needed because TFileSplitInfo
		and TChunkedFileStream operate on real files}
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
		procedure SetFileTime(const Path: WideString; const FileTime: TFileTime);
		function FindFiles(const Pattern: WideString): TStringList;
	end;

	{Configurable IRequest mock - returns configured result for dialog prompts}
	TConfigurableRequest = class(TInterfacedObject, IRequest)
	private
		FResult: Boolean;
	public
		constructor Create(AResult: Boolean);
		function Request(RequestType: Integer; CustomTitle, CustomText: WideString; var ReturnedText: WideString; maxlen: Integer): Boolean;
	end;

	{Testable subclass: overrides ShowChunkErrorDialog to avoid UI in tests}
	TTestableChunkedUploadHandler = class(TChunkedUploadHandler)
	private
		FDialogResult: Integer;
	protected
		function ShowChunkErrorDialog(UploadResult: Integer; const ChunkRemotePath: WideString): Integer; override;
	public
		property DialogResult: Integer read FDialogResult write FDialogResult;
	end;

	[TestFixture]
	TChunkedUploadHandlerTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPRef: ICloudHTTP;
		FMockContext: TMockCloudContext;
		FMockContextRef: ICloudContext;
		FHandler: TTestableChunkedUploadHandler;
		FTempDir: string;
		FTempFile: string;

		function CreateDefaultSettings: TChunkedUploadSettings;
		function CreateHandler(const Settings: TChunkedUploadSettings; ARequest: IRequest = nil; DoCryptFiles: Boolean = False): TTestableChunkedUploadHandler;
		procedure CreateTempFile(SizeInBytes: Int64);
		function MakeUploadFunc(ReturnCode: Integer): TStreamUploadFunc;
		function MakeUploadFuncQueue(const Codes: TArray<Integer>): TStreamUploadFunc;
		function MakeUploadFuncRaising: TStreamUploadFunc;
		function MakeIdentityFunc(ReturnCode: Integer): TAddByIdentityFunc;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{HandleChunkExists - unknown ChunkOverwriteMode (else branch)}
		[Test]
		procedure TestUpload_ChunkExists_UnknownMode_Aborts;

		{HandleChunkError - OperationErrorModeAsk dialog sub-branches}
		[Test]
		procedure TestUpload_Error_AskMode_AbortClicked;
		[Test]
		procedure TestUpload_Error_AskMode_RetryClicked;
		[Test]
		procedure TestUpload_Error_AskMode_IgnoreClicked;
		[Test]
		procedure TestUpload_Error_AskMode_UnknownDialogResult;

		{HandleChunkError - unknown OperationErrorMode (else branch)}
		[Test]
		procedure TestUpload_Error_UnknownErrorMode_Aborts;

		{CheckQuotaForSplitUpload - no space at all (AvailableSpace <= 0)}
		[Test]
		procedure TestUpload_Quota_NoSpace_Aborts;

		{CheckQuotaForSplitUpload - partial upload, user accepts}
		[Test]
		procedure TestUpload_Quota_PartialSpace_Accepted;

		{CheckQuotaForSplitUpload - partial upload, user declines}
		[Test]
		procedure TestUpload_Quota_PartialSpace_Declined;

		{Upload - exception during chunk upload}
		[Test]
		procedure TestUpload_UploadFuncRaises_HandlesException;

		{Upload - UseHash + FS_FILE_EXISTS skips chunk without overwrite logic}
		[Test]
		procedure TestUpload_UseHash_ExistsResponse_SkipsChunk;

		{Upload - HandleChunkExists returns caAbort, breaks loop}
		[Test]
		procedure TestUpload_ChunkExists_AbortBreaksLoop;
	end;

implementation

uses
	FileSplitInfo,
	PathHelper;

{TRealSizeFileSystem}

function TRealSizeFileSystem.FileExists(const Path: WideString): Boolean;
begin
	Result := System.SysUtils.FileExists(Path);
end;

function TRealSizeFileSystem.GetFileSize(const Path: WideString): Int64;
var
	Handle: THandle;
begin
	Handle := CreateFile(PChar(string(Path)), 0, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if Handle = INVALID_HANDLE_VALUE then
		Result := -1
	else
		try
			Int64Rec(Result).Lo := Winapi.Windows.GetFileSize(Handle, @Int64Rec(Result).Hi);
		finally
			CloseHandle(Handle);
		end;
end;

procedure TRealSizeFileSystem.CreateEmptyFile(const Path: WideString);
begin
end;

procedure TRealSizeFileSystem.DeleteFile(const Path: WideString);
begin
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
end;

procedure TRealSizeFileSystem.WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);
begin
end;

function TRealSizeFileSystem.OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;
var
	Stream: TStringStream;
begin
	Stream := TStringStream.Create('', Encoding);
	Result := TStreamReader.Create(Stream, Encoding, False);
end;

function TRealSizeFileSystem.GetTmpFileName(const Prefix: WideString): WideString;
begin
	Result := '';
end;

procedure TRealSizeFileSystem.SetFileTime(const Path: WideString; const FileTime: TFileTime);
begin
end;

function TRealSizeFileSystem.FindFiles(const Pattern: WideString): TStringList;
begin
	Result := TStringList.Create;
end;

{TConfigurableRequest}

constructor TConfigurableRequest.Create(AResult: Boolean);
begin
	inherited Create;
	FResult := AResult;
end;

function TConfigurableRequest.Request(RequestType: Integer; CustomTitle, CustomText: WideString; var ReturnedText: WideString; maxlen: Integer): Boolean;
begin
	Result := FResult;
end;

{TTestableChunkedUploadHandler}

function TTestableChunkedUploadHandler.ShowChunkErrorDialog(UploadResult: Integer; const ChunkRemotePath: WideString): Integer;
begin
	Result := FDialogResult;
end;

{TChunkedUploadHandlerTest}

procedure TChunkedUploadHandlerTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPRef := FMockHTTP;
	FMockHTTP.SetDefaultResponse(True, '');

	FMockContext := TMockCloudContext.Create;
	FMockContextRef := FMockContext;
	FMockContext.SetHTTP(FMockHTTP);
	FMockContext.SetIsPublicAccount(False);
	{Skip quota check by default - individual quota tests enable it}
	FMockContext.SetGetUserSpaceResult(False, Default(TCloudSpace));
	FMockContext.SetDeleteFileResult(True);

	FTempDir := TPath.Combine(TPath.GetTempPath, 'ChunkedUploadHandlerTest_' + TGUID.NewGuid.ToString);
	TDirectory.CreateDirectory(FTempDir);
	FTempFile := '';
	FHandler := nil;
end;

procedure TChunkedUploadHandlerTest.TearDown;
begin
	FreeAndNil(FHandler);
	FMockHTTPRef := nil;
	FMockContextRef := nil;
	if (FTempDir <> '') and TDirectory.Exists(FTempDir) then
		TDirectory.Delete(FTempDir, True);
end;

function TChunkedUploadHandlerTest.CreateDefaultSettings: TChunkedUploadSettings;
begin
	Result.CloudMaxFileSize := 10; {Small chunks for fast tests}
	Result.PrecalculateHash := False;
	Result.ForcePrecalculateSize := 0;
	Result.OperationErrorMode := OperationErrorModeAbort;
	Result.RetryAttempts := 3;
	Result.AttemptWait := 1; {Minimal wait}
end;

function TChunkedUploadHandlerTest.CreateHandler(const Settings: TChunkedUploadSettings; ARequest: IRequest; DoCryptFiles: Boolean): TTestableChunkedUploadHandler;
begin
	if ARequest = nil then
		ARequest := TNullRequest.Create;

	Result := TTestableChunkedUploadHandler.Create(
		FMockContext,
		TNullHashCalculator.Create,
		TRealSizeFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		ARequest,
		TNullTCHandler.Create,
		DoCryptFiles,
		Settings);
end;

procedure TChunkedUploadHandlerTest.CreateTempFile(SizeInBytes: Int64);
var
	FileStream: TFileStream;
	Buffer: TBytes;
	BytesRemaining, BytesToWrite: Int64;
begin
	FTempFile := TPath.Combine(FTempDir, 'testfile.dat');
	SetLength(Buffer, 256);
	FillChar(Buffer[0], Length(Buffer), $AA);

	FileStream := TFileStream.Create(FTempFile, fmCreate);
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

function TChunkedUploadHandlerTest.MakeUploadFunc(ReturnCode: Integer): TStreamUploadFunc;
begin
	Result := function(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString): Integer
	begin
		Result := ReturnCode;
	end;
end;

function TChunkedUploadHandlerTest.MakeUploadFuncQueue(const Codes: TArray<Integer>): TStreamUploadFunc;
var
	CallIndex: Integer;
begin
	CallIndex := 0;
	Result := function(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString): Integer
	begin
		if CallIndex < Length(Codes) then
		begin
			Result := Codes[CallIndex];
			Inc(CallIndex);
		end
		else
			Result := FS_FILE_OK;
	end;
end;

function TChunkedUploadHandlerTest.MakeUploadFuncRaising: TStreamUploadFunc;
begin
	Result := function(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString): Integer
	begin
		raise Exception.Create('Simulated upload failure');
	end;
end;

function TChunkedUploadHandlerTest.MakeIdentityFunc(ReturnCode: Integer): TAddByIdentityFunc;
begin
	Result := function(FileIdentity: TCloudFileIdentity; RemotePath: WideString; ConflictMode: WideString; LogErrors: Boolean; LogSuccess: Boolean): Integer
	begin
		Result := ReturnCode;
	end;
end;

{HandleChunkExists - unknown ChunkOverwriteMode}

procedure TChunkedUploadHandlerTest.TestUpload_ChunkExists_UnknownMode_Aborts;
var
	Settings: TChunkedUploadSettings;
	UploadResult: Integer;
begin
	CreateTempFile(25); {3 chunks of 10, 10, 5}
	Settings := CreateDefaultSettings;
	FHandler := CreateHandler(Settings);

	UploadResult := FHandler.Upload(
		MakeUploadFunc(FS_FILE_EXISTS),
		MakeIdentityFunc(CLOUD_OPERATION_FAILED),
		FTempFile, '/remote/testfile.dat', 'rename', 99 {unknown mode});

	Assert.AreEqual(Integer(CLOUD_OPERATION_FAILED), UploadResult,
		'Unknown ChunkOverwriteMode should abort with CLOUD_OPERATION_FAILED');
end;

{HandleChunkError - OperationErrorModeAsk}

procedure TChunkedUploadHandlerTest.TestUpload_Error_AskMode_AbortClicked;
var
	Settings: TChunkedUploadSettings;
	UploadResult: Integer;
begin
	CreateTempFile(25);
	Settings := CreateDefaultSettings;
	Settings.OperationErrorMode := OperationErrorModeAsk;
	FHandler := CreateHandler(Settings);
	FHandler.DialogResult := ID_ABORT;

	UploadResult := FHandler.Upload(
		MakeUploadFunc(FS_FILE_WRITEERROR),
		MakeIdentityFunc(CLOUD_OPERATION_FAILED),
		FTempFile, '/remote/testfile.dat', 'rename', ChunkOverwrite);

	Assert.AreEqual(Integer(FS_FILE_USERABORT), UploadResult,
		'User clicking Abort in error dialog should return FS_FILE_USERABORT');
end;

procedure TChunkedUploadHandlerTest.TestUpload_Error_AskMode_RetryClicked;
var
	Settings: TChunkedUploadSettings;
	UploadResult: Integer;
begin
	CreateTempFile(25);
	Settings := CreateDefaultSettings;
	Settings.OperationErrorMode := OperationErrorModeAsk;
	FHandler := CreateHandler(Settings);
	FHandler.DialogResult := ID_RETRY;

	{First call fails, retry succeeds, then remaining chunks and CRC succeed}
	UploadResult := FHandler.Upload(
		MakeUploadFuncQueue([FS_FILE_WRITEERROR, FS_FILE_OK, FS_FILE_OK, FS_FILE_OK, FS_FILE_OK]),
		MakeIdentityFunc(CLOUD_OPERATION_FAILED),
		FTempFile, '/remote/testfile.dat', 'rename', ChunkOverwrite);

	Assert.AreEqual(Integer(FS_FILE_OK), UploadResult,
		'After retry and success, upload should complete OK');
end;

procedure TChunkedUploadHandlerTest.TestUpload_Error_AskMode_IgnoreClicked;
var
	Settings: TChunkedUploadSettings;
	UploadResult: Integer;
begin
	CreateTempFile(25);
	Settings := CreateDefaultSettings;
	Settings.OperationErrorMode := OperationErrorModeAsk;
	FHandler := CreateHandler(Settings);
	FHandler.DialogResult := ID_IGNORE;

	{First chunk fails, ignore skips it, remaining chunks and CRC succeed}
	UploadResult := FHandler.Upload(
		MakeUploadFuncQueue([FS_FILE_WRITEERROR, FS_FILE_OK, FS_FILE_OK, FS_FILE_OK]),
		MakeIdentityFunc(CLOUD_OPERATION_FAILED),
		FTempFile, '/remote/testfile.dat', 'rename', ChunkOverwrite);

	Assert.AreEqual(Integer(FS_FILE_OK), UploadResult,
		'Ignoring error should continue upload');
end;

procedure TChunkedUploadHandlerTest.TestUpload_Error_AskMode_UnknownDialogResult;
var
	Settings: TChunkedUploadSettings;
	UploadResult: Integer;
begin
	CreateTempFile(25);
	Settings := CreateDefaultSettings;
	Settings.OperationErrorMode := OperationErrorModeAsk;
	FHandler := CreateHandler(Settings);
	FHandler.DialogResult := 0; {Unknown dialog result - defaults to caContinue}

	{First chunk fails, unknown dialog result defaults to continue}
	UploadResult := FHandler.Upload(
		MakeUploadFuncQueue([FS_FILE_WRITEERROR, FS_FILE_OK, FS_FILE_OK, FS_FILE_OK]),
		MakeIdentityFunc(CLOUD_OPERATION_FAILED),
		FTempFile, '/remote/testfile.dat', 'rename', ChunkOverwrite);

	Assert.AreEqual(Integer(FS_FILE_OK), UploadResult,
		'Unknown dialog result should default to continue');
end;

{HandleChunkError - unknown OperationErrorMode}

procedure TChunkedUploadHandlerTest.TestUpload_Error_UnknownErrorMode_Aborts;
var
	Settings: TChunkedUploadSettings;
	UploadResult: Integer;
begin
	CreateTempFile(25);
	Settings := CreateDefaultSettings;
	Settings.OperationErrorMode := 99; {Unknown error mode}
	FHandler := CreateHandler(Settings);

	UploadResult := FHandler.Upload(
		MakeUploadFunc(FS_FILE_WRITEERROR),
		MakeIdentityFunc(CLOUD_OPERATION_FAILED),
		FTempFile, '/remote/testfile.dat', 'rename', ChunkOverwrite);

	Assert.AreEqual(Integer(CLOUD_OPERATION_FAILED), UploadResult,
		'Unknown OperationErrorMode should abort with CLOUD_OPERATION_FAILED');
end;

{CheckQuotaForSplitUpload - no space}

procedure TChunkedUploadHandlerTest.TestUpload_Quota_NoSpace_Aborts;
var
	Settings: TChunkedUploadSettings;
	Space: TCloudSpace;
	UploadResult: Integer;
begin
	CreateTempFile(25);
	Settings := CreateDefaultSettings;

	{Set up quota: used > total -> AvailableSpace clamped to 0 -> ChunksThatFit = 0}
	Space := Default(TCloudSpace);
	Space.total := 10;
	Space.used := 20;
	FMockContext.SetGetUserSpaceResult(True, Space);

	FHandler := CreateHandler(Settings);

	UploadResult := FHandler.Upload(
		MakeUploadFunc(FS_FILE_OK),
		MakeIdentityFunc(CLOUD_OPERATION_FAILED),
		FTempFile, '/remote/testfile.dat', 'rename', ChunkOverwrite);

	Assert.AreEqual(Integer(FS_FILE_USERABORT), UploadResult,
		'No available space should abort with FS_FILE_USERABORT');
end;

{CheckQuotaForSplitUpload - partial upload accepted}

procedure TChunkedUploadHandlerTest.TestUpload_Quota_PartialSpace_Accepted;
var
	Settings: TChunkedUploadSettings;
	Space: TCloudSpace;
	UploadResult: Integer;
	UploadCallCount: Integer;
begin
	CreateTempFile(25); {3 chunks: 10, 10, 5}
	Settings := CreateDefaultSettings;

	{AvailableSpace = 100 - 85 = 15; ChunksThatFit = 15 div 10 = 1 (out of 3)}
	Space := Default(TCloudSpace);
	Space.total := 100;
	Space.used := 85;
	FMockContext.SetGetUserSpaceResult(True, Space);

	{User accepts partial upload}
	FHandler := CreateHandler(Settings, TConfigurableRequest.Create(True));

	UploadCallCount := 0;
	UploadResult := FHandler.Upload(
		function(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString): Integer
		begin
			Inc(UploadCallCount);
			Result := FS_FILE_OK;
		end,
		MakeIdentityFunc(CLOUD_OPERATION_FAILED),
		FTempFile, '/remote/testfile.dat', 'rename', ChunkOverwrite);

	Assert.AreEqual(Integer(FS_FILE_OK), UploadResult,
		'Partial upload should succeed');
	Assert.AreEqual(1, UploadCallCount,
		'Should upload exactly 1 chunk when only 1 fits');
end;

{CheckQuotaForSplitUpload - partial upload declined}

procedure TChunkedUploadHandlerTest.TestUpload_Quota_PartialSpace_Declined;
var
	Settings: TChunkedUploadSettings;
	Space: TCloudSpace;
	UploadResult: Integer;
begin
	CreateTempFile(25);
	Settings := CreateDefaultSettings;

	Space := Default(TCloudSpace);
	Space.total := 100;
	Space.used := 85;
	FMockContext.SetGetUserSpaceResult(True, Space);

	{User declines partial upload}
	FHandler := CreateHandler(Settings, TConfigurableRequest.Create(False));

	UploadResult := FHandler.Upload(
		MakeUploadFunc(FS_FILE_OK),
		MakeIdentityFunc(CLOUD_OPERATION_FAILED),
		FTempFile, '/remote/testfile.dat', 'rename', ChunkOverwrite);

	Assert.AreEqual(Integer(FS_FILE_USERABORT), UploadResult,
		'Declining partial upload should return FS_FILE_USERABORT');
end;

{Upload - exception handler}

procedure TChunkedUploadHandlerTest.TestUpload_UploadFuncRaises_HandlesException;
var
	Settings: TChunkedUploadSettings;
	UploadResult: Integer;
begin
	CreateTempFile(25);
	Settings := CreateDefaultSettings;
	Settings.OperationErrorMode := OperationErrorModeAbort;
	FHandler := CreateHandler(Settings);

	{UploadFunc raises -> caught by except block -> FS_FILE_READERROR ->
		HandleChunkError with Abort mode -> FS_FILE_USERABORT}
	UploadResult := FHandler.Upload(
		MakeUploadFuncRaising(),
		MakeIdentityFunc(CLOUD_OPERATION_FAILED),
		FTempFile, '/remote/testfile.dat', 'rename', ChunkOverwrite);

	Assert.AreEqual(Integer(FS_FILE_USERABORT), UploadResult,
		'Exception in upload func should be caught and result in abort');
end;

{Upload - hash match skip}

procedure TChunkedUploadHandlerTest.TestUpload_UseHash_ExistsResponse_SkipsChunk;
var
	Settings: TChunkedUploadSettings;
	UploadResult: Integer;
	UploadCallCount: Integer;
begin
	CreateTempFile(25); {3 chunks}
	Settings := CreateDefaultSettings;
	Settings.PrecalculateHash := True; {Enable UseHash path}
	FHandler := CreateHandler(Settings);

	UploadCallCount := 0;
	{First chunk returns EXISTS (hash-matched, skip), remaining succeed}
	UploadResult := FHandler.Upload(
		function(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString): Integer
		begin
			Inc(UploadCallCount);
			if UploadCallCount = 1 then
				Result := FS_FILE_EXISTS
			else
				Result := FS_FILE_OK;
		end,
		MakeIdentityFunc(CLOUD_OPERATION_FAILED),
		FTempFile, '/remote/testfile.dat', 'rename', ChunkOverwrite);

	Assert.AreEqual(Integer(FS_FILE_OK), UploadResult,
		'Upload should succeed with hash-matched skip');
	Assert.AreEqual(4, UploadCallCount,
		'Should have 4 upload calls: 1 exists + 2 OK chunks + 1 CRC');
end;

{Upload - HandleChunkExists caAbort break}

procedure TChunkedUploadHandlerTest.TestUpload_ChunkExists_AbortBreaksLoop;
var
	Settings: TChunkedUploadSettings;
	UploadResult: Integer;
begin
	CreateTempFile(25);
	Settings := CreateDefaultSettings;
	Settings.PrecalculateHash := False; {Disable UseHash to reach HandleChunkExists}
	FHandler := CreateHandler(Settings);

	UploadResult := FHandler.Upload(
		MakeUploadFunc(FS_FILE_EXISTS),
		MakeIdentityFunc(CLOUD_OPERATION_FAILED),
		FTempFile, '/remote/testfile.dat', 'rename', ChunkOverwriteAbort);

	Assert.AreEqual(Integer(FS_FILE_NOTSUPPORTED), UploadResult,
		'ChunkOverwriteAbort should break loop with FS_FILE_NOTSUPPORTED');
end;

initialization

TDUnitX.RegisterTestFixture(TChunkedUploadHandlerTest);

end.
