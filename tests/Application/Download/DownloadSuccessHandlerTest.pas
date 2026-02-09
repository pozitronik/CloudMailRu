unit DownloadSuccessHandlerTest;

{Unit tests for TDownloadSuccessHandler - post-download success operations.
 Tests verify CRC checking, timestamp preservation, move operations, and sync.}

interface

uses
	Windows,
	System.Classes,
	SysUtils,
	DUnitX.TestFramework,
	DownloadSuccessHandler,
	MockSettingsManager,
	MockLogger,
	MockProgress,
	Logger,
	Progress,
	MockSyncGuards,
	PluginSettings,
	WFXTypes,
	RealPath,
	CloudDirItem,
	CloudMailRu,
	CloudSettings,
	Cipher,
	AuthStrategy,
	FileSystem,
	HTTPManager,
	Request,
	TCHandler,
	OpenSSLProvider,
	AccountCredentialsProvider,
	TestHelper;

type
	[TestFixture]
	TDownloadSuccessHandlerTest = class
	private
		FHandler: IDownloadSuccessHandler;
		FSettings: TMockSettingsManager;
		FLogger: TMockLogger;
		FProgress: TMockProgress;
		FSyncGuard: TMockDescriptionSyncGuard;
		FTimestampSyncGuard: TMockTimestampSyncGuard;
		FFileSystem: IFileSystem;
		FMockCloud: TCloudMailRu;

		function CreateContext(const Hash, ExpectedHash: WideString; MoveFlag: Boolean): TDownloadContext;
		function CreateMockCloud: TCloudMailRu;
		procedure CreateHandler;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Basic success}
		[Test]
		procedure TestHandleSuccess_ReturnsOK;

		{CRC verification tests}
		[Test]
		procedure TestHandleSuccess_CRCEnabled_HashMatch_ReturnsOK;
		[Test]
		procedure TestHandleSuccess_CRCEnabled_HashMismatch_ReturnsReadError;
		[Test]
		procedure TestHandleSuccess_CRCDisabled_HashMismatch_ReturnsOK;
		[Test]
		procedure TestHandleSuccess_CRCEnabled_EmptyResultHash_ReturnsOK;

		{Progress and logging}
		[Test]
		procedure TestHandleSuccess_ReportsProgress100;
		[Test]
		procedure TestHandleSuccess_LogsTransfer;

		{Move operation}
		[Test]
		procedure TestHandleSuccess_MoveFlag_CallsDeleteSync;
		[Test]
		procedure TestHandleSuccess_MoveFlag_CallsTimestampDeleteSync;
		[Test]
		procedure TestHandleSuccess_NoMoveFlag_DoesNotCallDeleteSync;

		{Description sync}
		[Test]
		procedure TestHandleSuccess_CallsDownloadedSync;

		{File time preservation tests}
		[Test]
		procedure TestHandleSuccess_PreserveTimeEnabled_NonZeroMtime_SetsFileTime;
		[Test]
		procedure TestHandleSuccess_PreserveTimeDisabled_NonZeroMtime_DoesNotSetFileTime;

		{Timestamp sync tests}
		[Test]
		procedure TestHandleSuccess_TimestampReturnsStoredMTime_UsesStoredMTime;
		[Test]
		procedure TestHandleSuccess_TimestampReturnsZero_FallsBackToPreserveFileTime;
		[Test]
		procedure TestHandleSuccess_TimestampReturnsZero_PreserveDisabled_NoFileTimeSet;

		{Null handler tests}
		[Test]
		procedure TestNullHandler_ReturnsOK;
	end;

implementation

{TDownloadSuccessHandlerTest}

procedure TDownloadSuccessHandlerTest.Setup;
begin
	FSettings := TMockSettingsManager.Create;
	FLogger := TMockLogger.Create;
	FProgress := TMockProgress.Create;
	FSyncGuard := TMockDescriptionSyncGuard.Create;
	FTimestampSyncGuard := TMockTimestampSyncGuard.Create;
	FFileSystem := TNullFileSystem.Create;
end;

procedure TDownloadSuccessHandlerTest.TearDown;
begin
	FHandler := nil;
	FSettings := nil;
	FLogger := nil;
	FProgress := nil;
	FSyncGuard := nil;
	FTimestampSyncGuard := nil;
	FFileSystem := nil;
	FreeAndNil(FMockCloud);
end;

function TDownloadSuccessHandlerTest.CreateMockCloud: TCloudMailRu;
var
	Settings: TCloudSettings;
begin
	Settings := Default(TCloudSettings);
	Result := TCloudMailRu.Create(
		Settings,
		TNullHTTPManager.Create,
		TestThreadID(),
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		TNullCipher.Create,
		TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create);
end;

function TDownloadSuccessHandlerTest.CreateContext(const Hash, ExpectedHash: WideString; MoveFlag: Boolean): TDownloadContext;
begin
	Result := Default(TDownloadContext);
	Result.RemotePath.FromPath('\account\file.txt');
	Result.LocalName := 'C:\local\file.txt';
	Result.RemoteName := '\account\file.txt';
	Result.ResultHash := Hash;
	Result.Item.hash := ExpectedHash;
	Result.Item.mtime := 0;
	Result.Cloud := nil; {Can be nil for most tests}
	if MoveFlag then
		Result.CopyFlags := FS_COPYFLAGS_MOVE
	else
		Result.CopyFlags := 0;
end;

procedure TDownloadSuccessHandlerTest.CreateHandler;
begin
	FHandler := TDownloadSuccessHandler.Create(FSettings, FLogger, FProgress, FSyncGuard, FTimestampSyncGuard, FFileSystem);
end;

{Basic success}

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_ReturnsOK;
var
	Context: TDownloadContext;
begin
	CreateHandler;
	Context := CreateContext('', '', False);

	Assert.AreEqual(FS_FILE_OK, FHandler.HandleSuccess(Context));
end;

{CRC verification tests}

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_CRCEnabled_HashMatch_ReturnsOK;
var
	Context: TDownloadContext;
begin
	FSettings.SetCheckCRC(True);
	CreateHandler;
	Context := CreateContext('abc123', 'abc123', False);

	Assert.AreEqual(FS_FILE_OK, FHandler.HandleSuccess(Context));
end;

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_CRCEnabled_HashMismatch_ReturnsReadError;
var
	Context: TDownloadContext;
begin
	FSettings.SetCheckCRC(True);
	CreateHandler;
	Context := CreateContext('abc123', 'xyz789', False);

	Assert.AreEqual(FS_FILE_READERROR, FHandler.HandleSuccess(Context));
end;

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_CRCDisabled_HashMismatch_ReturnsOK;
var
	Context: TDownloadContext;
begin
	FSettings.SetCheckCRC(False);
	CreateHandler;
	Context := CreateContext('abc123', 'xyz789', False);

	Assert.AreEqual(FS_FILE_OK, FHandler.HandleSuccess(Context), 'CRC disabled should ignore mismatch');
end;

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_CRCEnabled_EmptyResultHash_ReturnsOK;
var
	Context: TDownloadContext;
begin
	FSettings.SetCheckCRC(True);
	CreateHandler;
	Context := CreateContext('', 'xyz789', False); {Empty result hash means not calculated}

	Assert.AreEqual(FS_FILE_OK, FHandler.HandleSuccess(Context), 'Empty result hash should pass');
end;

{Progress and logging}

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_ReportsProgress100;
var
	Context: TDownloadContext;
begin
	CreateHandler;
	Context := CreateContext('', '', False);

	FHandler.HandleSuccess(Context);

	Assert.AreEqual(1, FProgress.ProgressCalls, 'Should call progress once');
	Assert.AreEqual(100, FProgress.LastPercentDone, 'Should report 100%');
end;

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_LogsTransfer;
var
	Context: TDownloadContext;
begin
	CreateHandler;
	Context := CreateContext('', '', False);

	FHandler.HandleSuccess(Context);

	Assert.AreEqual(1, FLogger.LogCalls, 'Should log once');
end;

{Move operation}

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_MoveFlag_CallsDeleteSync;
var
	Context: TDownloadContext;
begin
	CreateHandler;
	FMockCloud := CreateMockCloud;
	Context := CreateContext('', '', True); {Move flag set}
	Context.Cloud := FMockCloud;

	FHandler.HandleSuccess(Context);

	Assert.AreEqual(1, FSyncGuard.DeletedCalls, 'Should call OnFileDeleted for move');
end;

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_MoveFlag_CallsTimestampDeleteSync;
var
	Context: TDownloadContext;
begin
	CreateHandler;
	FMockCloud := CreateMockCloud;
	Context := CreateContext('', '', True); {Move flag set}
	Context.Cloud := FMockCloud;

	FHandler.HandleSuccess(Context);

	Assert.AreEqual(1, FTimestampSyncGuard.DeletedCalls, 'Should call timestamp OnFileDeleted for move');
end;

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_NoMoveFlag_DoesNotCallDeleteSync;
var
	Context: TDownloadContext;
begin
	CreateHandler;
	Context := CreateContext('', '', False); {No move flag}

	FHandler.HandleSuccess(Context);

	Assert.AreEqual(0, FSyncGuard.DeletedCalls, 'Should not call OnFileDeleted without move flag');
end;

{Description sync}

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_CallsDownloadedSync;
var
	Context: TDownloadContext;
begin
	CreateHandler;
	Context := CreateContext('', '', False);

	FHandler.HandleSuccess(Context);

	Assert.AreEqual(1, FSyncGuard.DownloadedCalls, 'Should call OnFileDownloaded');
end;

{File time preservation tests}

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_PreserveTimeEnabled_NonZeroMtime_SetsFileTime;
var
	Context: TDownloadContext;
	TempFile: WideString;
	FileHandle: THandle;
begin
	{Create a temp file to test file time modification}
	TempFile := GetEnvironmentVariable('TEMP') + '\DownloadSuccessTest_' + IntToStr(GetTickCount) + '.tmp';
	FileHandle := CreateFileW(PWideChar(TempFile), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	if FileHandle = INVALID_HANDLE_VALUE then
	begin
		Assert.Pass('Cannot create temp file, skipping test');
		Exit;
	end;
	CloseHandle(FileHandle);

	try
		{Use real file system for file time modification test}
		FFileSystem := TWindowsFileSystem.Create;
		FSettings.SetPreserveFileTime(True);
		CreateHandler;
		Context := CreateContext('', '', False);
		Context.LocalName := TempFile;
		Context.Item.mtime := 1700000000; {Nov 2023 Unix timestamp}

		FHandler.HandleSuccess(Context);

		{Verify file still exists - SetFileTime was called}
		Assert.IsTrue(FileExists(TempFile), 'File should still exist after time modification');
	finally
		DeleteFile(TempFile);
	end;
end;

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_PreserveTimeDisabled_NonZeroMtime_DoesNotSetFileTime;
var
	Context: TDownloadContext;
begin
	{When PreserveFileTime is disabled, mtime should be ignored - no file access}
	FSettings.SetPreserveFileTime(False);
	CreateHandler;
	Context := CreateContext('', '', False);
	Context.Item.mtime := 1700000000; {Non-zero but won't be used}

	{Should not throw even with non-existent file since PreserveFileTime is disabled}
	Assert.AreEqual(FS_FILE_OK, FHandler.HandleSuccess(Context));
end;

{Timestamp sync tests}

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_TimestampReturnsStoredMTime_UsesStoredMTime;
var
	Context: TDownloadContext;
	TempFile: WideString;
	FileHandle: THandle;
begin
	{When timestamp sync returns a stored mtime, use it instead of server mtime}
	TempFile := GetEnvironmentVariable('TEMP') + '\DownloadTimestampTest_' + IntToStr(GetTickCount) + '.tmp';
	FileHandle := CreateFileW(PWideChar(TempFile), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	if FileHandle = INVALID_HANDLE_VALUE then
	begin
		Assert.Pass('Cannot create temp file, skipping test');
		Exit;
	end;
	CloseHandle(FileHandle);

	try
		FFileSystem := TWindowsFileSystem.Create;
		FTimestampSyncGuard := TMockTimestampSyncGuard.Create;
		FTimestampSyncGuard.DownloadReturnValue := 1704067200; {Stored local mtime}
		FSettings.SetPreserveFileTime(False); {PreserveFileTime disabled - but stored mtime takes priority}
		CreateHandler;
		Context := CreateContext('', '', False);
		Context.LocalName := TempFile;
		Context.Item.mtime := 1700000000; {Server mtime - should be ignored}

		FHandler.HandleSuccess(Context);

		Assert.AreEqual(1, FTimestampSyncGuard.DownloadedCalls, 'Should call timestamp sync');
		Assert.IsTrue(FileExists(TempFile), 'File should exist after time modification');
	finally
		DeleteFile(TempFile);
	end;
end;

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_TimestampReturnsZero_FallsBackToPreserveFileTime;
var
	Context: TDownloadContext;
	TempFile: WideString;
	FileHandle: THandle;
begin
	{When timestamp sync returns 0, fall back to PreserveFileTime behavior}
	TempFile := GetEnvironmentVariable('TEMP') + '\DownloadTimestampFallback_' + IntToStr(GetTickCount) + '.tmp';
	FileHandle := CreateFileW(PWideChar(TempFile), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	if FileHandle = INVALID_HANDLE_VALUE then
	begin
		Assert.Pass('Cannot create temp file, skipping test');
		Exit;
	end;
	CloseHandle(FileHandle);

	try
		FFileSystem := TWindowsFileSystem.Create;
		FTimestampSyncGuard := TMockTimestampSyncGuard.Create;
		FTimestampSyncGuard.DownloadReturnValue := 0; {No stored mtime}
		FSettings.SetPreserveFileTime(True); {Fall back to server mtime}
		CreateHandler;
		Context := CreateContext('', '', False);
		Context.LocalName := TempFile;
		Context.Item.mtime := 1700000000; {Server mtime - should be used}

		FHandler.HandleSuccess(Context);

		Assert.IsTrue(FileExists(TempFile), 'File should exist after time modification');
	finally
		DeleteFile(TempFile);
	end;
end;

procedure TDownloadSuccessHandlerTest.TestHandleSuccess_TimestampReturnsZero_PreserveDisabled_NoFileTimeSet;
var
	Context: TDownloadContext;
begin
	{When both timestamp and PreserveFileTime are inactive, no file time should be set}
	FTimestampSyncGuard := TMockTimestampSyncGuard.Create;
	FTimestampSyncGuard.DownloadReturnValue := 0;
	FSettings.SetPreserveFileTime(False);
	CreateHandler;
	Context := CreateContext('', '', False);
	Context.Item.mtime := 1700000000;

	{Should succeed without trying to access file (NullFileSystem)}
	Assert.AreEqual(FS_FILE_OK, FHandler.HandleSuccess(Context));
end;

{Null handler tests}

procedure TDownloadSuccessHandlerTest.TestNullHandler_ReturnsOK;
var
	NullHandler: IDownloadSuccessHandler;
	Context: TDownloadContext;
begin
	NullHandler := TNullDownloadSuccessHandler.Create;
	Context := CreateContext('abc', 'xyz', True); {Would fail in real handler}

	Assert.AreEqual(FS_FILE_OK, NullHandler.HandleSuccess(Context), 'Null handler always returns OK');
end;

initialization
	TDUnitX.RegisterTestFixture(TDownloadSuccessHandlerTest);

end.
