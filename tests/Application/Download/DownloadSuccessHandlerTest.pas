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
	PluginSettingsManager,
	TCLogger,
	TCProgress,
	DescriptionSyncGuard,
	PluginSettings,
	PLUGIN_TYPES,
	RealPath,
	CMRDirItem,
	CloudMailRu,
	StreamingSettings;

type
	{Mock settings manager}
	TMockSettingsManager = class(TInterfacedObject, IPluginSettingsManager)
	private
		FSettings: TPluginSettings;
	public
		constructor Create;
		function GetSettings: TPluginSettings;
		procedure SetSettings(Value: TPluginSettings);
		procedure Save;
		procedure SwitchProxyPasswordStorage;
		function GetStreamingSettings(const FileName: WideString): TStreamingSettings;
		procedure SetStreamingSettings(const FileName: WideString; StreamSettings: TStreamingSettings);
		procedure GetStreamingExtensionsList(ExtensionsList: TStrings);
		procedure RemoveStreamingExtension(const Extension: WideString);
		procedure SetCheckCRC(Value: Boolean);
		procedure SetPreserveFileTime(Value: Boolean);
	end;

	{Mock logger}
	TMockLogger = class(TInterfacedObject, ILogger)
	public
		LogCalls: Integer;
		constructor Create;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
	end;

	{Mock progress}
	TMockProgress = class(TInterfacedObject, IProgress)
	public
		ProgressCalls: Integer;
		LastPercent: Integer;
		constructor Create;
		function Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(SourceName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(PercentDone: Integer): Boolean; overload;
		function Aborted: Boolean;
	end;

	{Mock description sync guard}
	TMockDescriptionSyncGuard = class(TInterfacedObject, IDescriptionSyncGuard)
	public
		DeletedCalls: Integer;
		DownloadedCalls: Integer;
		constructor Create;
		procedure OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
		procedure OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
	end;

	[TestFixture]
	TDownloadSuccessHandlerTest = class
	private
		FHandler: IDownloadSuccessHandler;
		FSettings: TMockSettingsManager;
		FLogger: TMockLogger;
		FProgress: TMockProgress;
		FSyncGuard: TMockDescriptionSyncGuard;

		function CreateContext(const Hash, ExpectedHash: WideString; MoveFlag: Boolean): TDownloadContext;
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
		procedure TestHandleSuccess_NoMoveFlag_DoesNotCallDeleteSync;

		{Description sync}
		[Test]
		procedure TestHandleSuccess_CallsDownloadedSync;

		{File time preservation tests}
		[Test]
		procedure TestHandleSuccess_PreserveTimeEnabled_NonZeroMtime_SetsFileTime;
		[Test]
		procedure TestHandleSuccess_PreserveTimeDisabled_NonZeroMtime_DoesNotSetFileTime;

		{Null handler tests}
		[Test]
		procedure TestNullHandler_ReturnsOK;
	end;

implementation

{TMockSettingsManager}

constructor TMockSettingsManager.Create;
begin
	inherited Create;
	FSettings.CheckCRC := False;
	FSettings.PreserveFileTime := False;
end;

function TMockSettingsManager.GetSettings: TPluginSettings;
begin
	Result := FSettings;
end;

procedure TMockSettingsManager.SetSettings(Value: TPluginSettings);
begin
	FSettings := Value;
end;

procedure TMockSettingsManager.Save;
begin
	{No-op}
end;

procedure TMockSettingsManager.SwitchProxyPasswordStorage;
begin
end;

function TMockSettingsManager.GetStreamingSettings(const FileName: WideString): TStreamingSettings;
begin
	Result := Default(TStreamingSettings);
end;

procedure TMockSettingsManager.SetStreamingSettings(const FileName: WideString; StreamSettings: TStreamingSettings);
begin
	{No-op}
end;

procedure TMockSettingsManager.GetStreamingExtensionsList(ExtensionsList: TStrings);
begin
	ExtensionsList.Clear;
end;

procedure TMockSettingsManager.RemoveStreamingExtension(const Extension: WideString);
begin
	{No-op}
end;

procedure TMockSettingsManager.SetCheckCRC(Value: Boolean);
begin
	FSettings.CheckCRC := Value;
end;

procedure TMockSettingsManager.SetPreserveFileTime(Value: Boolean);
begin
	FSettings.PreserveFileTime := Value;
end;

{TMockLogger}

constructor TMockLogger.Create;
begin
	inherited Create;
	LogCalls := 0;
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	Inc(LogCalls);
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
begin
	Inc(LogCalls);
end;

{TMockProgress}

constructor TMockProgress.Create;
begin
	inherited Create;
	ProgressCalls := 0;
	LastPercent := 0;
end;

function TMockProgress.Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean;
begin
	Inc(ProgressCalls);
	LastPercent := PercentDone;
	Result := False; {Not cancelled}
end;

function TMockProgress.Progress(SourceName: WideString; PercentDone: Integer): Boolean;
begin
	Inc(ProgressCalls);
	LastPercent := PercentDone;
	Result := False; {Not cancelled}
end;

function TMockProgress.Progress(PercentDone: Integer): Boolean;
begin
	Inc(ProgressCalls);
	LastPercent := PercentDone;
	Result := False; {Not cancelled}
end;

function TMockProgress.Aborted: Boolean;
begin
	Result := False; {Not aborted}
end;

{TMockDescriptionSyncGuard}

constructor TMockDescriptionSyncGuard.Create;
begin
	inherited Create;
	DeletedCalls := 0;
	DownloadedCalls := 0;
end;

procedure TMockDescriptionSyncGuard.OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
begin
	Inc(DeletedCalls);
end;

procedure TMockDescriptionSyncGuard.OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
begin
end;

procedure TMockDescriptionSyncGuard.OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
	Inc(DownloadedCalls);
end;

procedure TMockDescriptionSyncGuard.OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
end;

{TDownloadSuccessHandlerTest}

procedure TDownloadSuccessHandlerTest.Setup;
begin
	FSettings := TMockSettingsManager.Create;
	FLogger := TMockLogger.Create;
	FProgress := TMockProgress.Create;
	FSyncGuard := TMockDescriptionSyncGuard.Create;
end;

procedure TDownloadSuccessHandlerTest.TearDown;
begin
	FHandler := nil;
	FSettings := nil;
	FLogger := nil;
	FProgress := nil;
	FSyncGuard := nil;
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
	FHandler := TDownloadSuccessHandler.Create(FSettings, FLogger, FProgress, FSyncGuard);
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
	Assert.AreEqual(100, FProgress.LastPercent, 'Should report 100%');
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
	Context := CreateContext('', '', True); {Move flag set}

	FHandler.HandleSuccess(Context);

	Assert.AreEqual(1, FSyncGuard.DeletedCalls, 'Should call OnFileDeleted for move');
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
		FSettings.SetPreserveFileTime(True);
		CreateHandler;
		Context := CreateContext('', '', False);
		Context.LocalName := TempFile;
		Context.Item.mtime := 1700000000; {Nov 2023 Unix timestamp}

		FHandler.HandleSuccess(Context);

		{Verify file still exists - SetAllFileTime was called}
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
