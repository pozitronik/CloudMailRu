unit LocalFileConflictResolverTest;

{Unit tests for TLocalFileConflictResolver - local file conflict handling.
 Tests OverwriteLocalMode settings (Ask/Ignore/Overwrite) and flag checking.}

interface

uses
	DUnitX.TestFramework,
	PLUGIN_TYPES,
	SETTINGS_CONSTANTS,
	ILoggerInterface,
	LocalFileConflictResolver;

type
	{Mock logger to track log calls}
	TMockConflictLogger = class(TInterfacedObject, ILogger)
	public
		LogCalls: Integer;
		LastMessage: WideString;

		constructor Create;

		procedure Log(LogLevel, MsgType: Integer; Msg: WideString; const Params: array of const); overload;
		procedure Log(LogLevel, MsgType: Integer; Msg: WideString); overload;
		procedure LogError(Msg: WideString);
	end;

	[TestFixture]
	TLocalFileConflictResolverTest = class
	private
		FResolver: ILocalFileConflictResolver;
		FLogger: TMockConflictLogger;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Overwrite flag tests}
		[Test]
		procedure TestResolve_OverwriteFlagSet_AlwaysProceeds;

		{File does not exist tests}
		[Test]
		procedure TestResolve_FileNotExists_Proceeds;

		{Mode: Ask tests}
		[Test]
		procedure TestResolve_ModeAsk_FileExists_ReturnsExists;

		{Mode: Ignore tests}
		[Test]
		procedure TestResolve_ModeIgnore_FileExists_ReturnsOK;
		[Test]
		procedure TestResolve_ModeIgnore_FileExists_LogsMessage;

		{Mode: Overwrite tests}
		[Test]
		procedure TestResolve_ModeOverwrite_FileExists_Proceeds;
		[Test]
		procedure TestResolve_ModeOverwrite_FileExists_LogsMessage;
	end;

implementation

uses
	SysUtils,
	IOUtils;

var
	TestTempDir: string;
	TestExistingFile: string;

{TMockConflictLogger}

constructor TMockConflictLogger.Create;
begin
	inherited Create;
	LogCalls := 0;
	LastMessage := '';
end;

procedure TMockConflictLogger.Log(LogLevel, MsgType: Integer; Msg: WideString; const Params: array of const);
begin
	Inc(LogCalls);
	LastMessage := Msg;
end;

procedure TMockConflictLogger.Log(LogLevel, MsgType: Integer; Msg: WideString);
begin
	Inc(LogCalls);
	LastMessage := Msg;
end;

procedure TMockConflictLogger.LogError(Msg: WideString);
begin
	Inc(LogCalls);
	LastMessage := Msg;
end;

{TLocalFileConflictResolverTest}

procedure TLocalFileConflictResolverTest.Setup;
begin
	FLogger := TMockConflictLogger.Create;
	FResolver := TLocalFileConflictResolver.Create(FLogger);

	{Create a temp file for testing "file exists" scenarios}
	TestTempDir := TPath.GetTempPath + 'ConflictResolverTest\';
	ForceDirectories(TestTempDir);
	TestExistingFile := TestTempDir + 'existing.txt';
	TFile.WriteAllText(TestExistingFile, 'test content');
end;

procedure TLocalFileConflictResolverTest.TearDown;
begin
	FResolver := nil;
	FLogger := nil;

	{Clean up temp files}
	if DirectoryExists(TestTempDir) then
		TDirectory.Delete(TestTempDir, True);
end;

{Overwrite flag tests}

procedure TLocalFileConflictResolverTest.TestResolve_OverwriteFlagSet_AlwaysProceeds;
var
	Resolution: TConflictResolution;
begin
	{When overwrite flag is set, should always proceed regardless of file existence}
	Resolution := FResolver.Resolve(TestExistingFile, FS_COPYFLAGS_OVERWRITE, OverwriteLocalModeAsk);

	Assert.IsTrue(Resolution.ShouldProceed, 'Should proceed when overwrite flag set');
end;

{File does not exist tests}

procedure TLocalFileConflictResolverTest.TestResolve_FileNotExists_Proceeds;
var
	Resolution: TConflictResolution;
begin
	{When file doesn't exist, should proceed}
	Resolution := FResolver.Resolve(TestTempDir + 'nonexistent.txt', 0, OverwriteLocalModeAsk);

	Assert.IsTrue(Resolution.ShouldProceed, 'Should proceed when file does not exist');
end;

{Mode: Ask tests}

procedure TLocalFileConflictResolverTest.TestResolve_ModeAsk_FileExists_ReturnsExists;
var
	Resolution: TConflictResolution;
begin
	{Mode Ask + file exists = return FS_FILE_EXISTS to let TC ask}
	Resolution := FResolver.Resolve(TestExistingFile, 0, OverwriteLocalModeAsk);

	Assert.IsFalse(Resolution.ShouldProceed, 'Should not proceed in Ask mode');
	Assert.AreEqual(FS_FILE_EXISTS, Resolution.ResultCode, 'Should return FS_FILE_EXISTS');
end;

{Mode: Ignore tests}

procedure TLocalFileConflictResolverTest.TestResolve_ModeIgnore_FileExists_ReturnsOK;
var
	Resolution: TConflictResolution;
begin
	{Mode Ignore + file exists = skip silently with OK}
	Resolution := FResolver.Resolve(TestExistingFile, 0, OverwriteLocalModeIgnore);

	Assert.IsFalse(Resolution.ShouldProceed, 'Should not proceed in Ignore mode');
	Assert.AreEqual(FS_FILE_OK, Resolution.ResultCode, 'Should return FS_FILE_OK');
end;

procedure TLocalFileConflictResolverTest.TestResolve_ModeIgnore_FileExists_LogsMessage;
var
	Resolution: TConflictResolution;
begin
	{Mode Ignore should log the skip}
	FLogger.LogCalls := 0;

	Resolution := FResolver.Resolve(TestExistingFile, 0, OverwriteLocalModeIgnore);

	Assert.AreEqual(1, FLogger.LogCalls, 'Should log once');
end;

{Mode: Overwrite tests}

procedure TLocalFileConflictResolverTest.TestResolve_ModeOverwrite_FileExists_Proceeds;
var
	Resolution: TConflictResolution;
begin
	{Mode Overwrite + file exists = proceed with overwrite}
	Resolution := FResolver.Resolve(TestExistingFile, 0, OverwriteLocalModeOverwrite);

	Assert.IsTrue(Resolution.ShouldProceed, 'Should proceed in Overwrite mode');
end;

procedure TLocalFileConflictResolverTest.TestResolve_ModeOverwrite_FileExists_LogsMessage;
var
	Resolution: TConflictResolution;
begin
	{Mode Overwrite should log the overwrite}
	FLogger.LogCalls := 0;

	Resolution := FResolver.Resolve(TestExistingFile, 0, OverwriteLocalModeOverwrite);

	Assert.AreEqual(1, FLogger.LogCalls, 'Should log once');
end;

initialization
	TDUnitX.RegisterTestFixture(TLocalFileConflictResolverTest);

end.
