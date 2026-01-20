unit LocalFileDeletionHandlerTest;

{Unit tests for TLocalFileDeletionHandler - local file deletion with retry and error modes.
 Tests verify correct behavior for each configured delete mode and user interaction scenarios.}

interface

uses
	Windows,
	SysUtils,
	DUnitX.TestFramework,
	IPluginSettingsManagerInterface,
	TCLogger,
	LocalFileDeletionHandler,
	PluginSettings,
	PLUGIN_TYPES,
	SETTINGS_CONSTANTS;

type
	{Mock settings manager for controlling delete mode}
	TMockSettingsManager = class(TInterfacedObject, IPluginSettingsManager)
	private
		FSettings: TPluginSettings;
	public
		constructor Create;
		function GetSettings: TPluginSettings;
		procedure SwitchProxyPasswordStorage;
		procedure SetDeleteMode(Mode: Integer);
	end;

	{Mock logger that tracks calls}
	TMockLogger = class(TInterfacedObject, ILogger)
	public
		LogCalls: Integer;
		LastLogLevel: Integer;
		LastMsgType: Integer;
		LastMessage: WideString;

		constructor Create;
		procedure Reset;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
	end;

	[TestFixture]
	TLocalFileDeletionHandlerTest = class
	private
		FHandler: ILocalFileDeletionHandler;
		FSettings: TMockSettingsManager;
		FLogger: TMockLogger;

		{Mock state for file operations}
		FDeleteSucceeds: Boolean;
		FDeleteCallCount: Integer;
		FFileAttr: Integer;
		FSetAttrSucceeds: Boolean;
		FUserChoice: Integer;
		FAskUserCalled: Boolean;

		{Mock callbacks}
		function MockDeleteFile(const Path: WideString): Boolean;
		function MockGetFileAttr(const Path: WideString): Integer;
		function MockSetFileAttr(const Path: WideString; Attr: Integer): Boolean;
		function MockAskUser(const FileName: WideString): Integer;

		procedure CreateHandler;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Success cases}
		[Test]
		procedure TestDeleteSucceeds_ReturnsOK;

		{Ignore mode tests}
		[Test]
		procedure TestDeleteFails_IgnoreMode_ReturnsOK;
		[Test]
		procedure TestDeleteFails_IgnoreMode_Logs;

		{Abort mode tests}
		[Test]
		procedure TestDeleteFails_AbortMode_ReturnsNotSupported;
		[Test]
		procedure TestDeleteFails_AbortMode_Logs;

		{Ask mode tests}
		[Test]
		procedure TestDeleteFails_AskMode_UserRetry_RetriesOnce;
		[Test]
		procedure TestDeleteFails_AskMode_UserAbort_ReturnsNotSupported;
		[Test]
		procedure TestDeleteFails_AskMode_UserIgnore_ReturnsOK;

		{DeleteIgnore mode tests (tries to clear readonly)}
		[Test]
		procedure TestDeleteFails_DeleteIgnoreMode_ClearsReadonly_Succeeds;
		[Test]
		procedure TestDeleteFails_DeleteIgnoreMode_CantClearReadonly_ReturnsOK;

		{DeleteAbort mode tests}
		[Test]
		procedure TestDeleteFails_DeleteAbortMode_ClearsReadonly_Succeeds;
		[Test]
		procedure TestDeleteFails_DeleteAbortMode_CantClearReadonly_ReturnsNotSupported;

		{Retry behavior}
		[Test]
		procedure TestDeleteFails_AskMode_UserRetry_SucceedsOnSecondTry;
	end;

implementation

{TMockSettingsManager}

constructor TMockSettingsManager.Create;
begin
	inherited Create;
	FSettings.DeleteFailOnUploadMode := DeleteFailOnUploadIgnore;
end;

function TMockSettingsManager.GetSettings: TPluginSettings;
begin
	Result := FSettings;
end;

procedure TMockSettingsManager.SwitchProxyPasswordStorage;
begin
end;

procedure TMockSettingsManager.SetDeleteMode(Mode: Integer);
begin
	FSettings.DeleteFailOnUploadMode := Mode;
end;

{TMockLogger}

constructor TMockLogger.Create;
begin
	inherited Create;
	Reset;
end;

procedure TMockLogger.Reset;
begin
	LogCalls := 0;
	LastLogLevel := 0;
	LastMsgType := 0;
	LastMessage := '';
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	Inc(LogCalls);
	LastLogLevel := LogLevel;
	LastMsgType := MsgType;
	LastMessage := LogString;
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
begin
	Inc(LogCalls);
	LastLogLevel := LogLevel;
	LastMsgType := MsgType;
	LastMessage := LogString;
end;

{TLocalFileDeletionHandlerTest}

procedure TLocalFileDeletionHandlerTest.Setup;
begin
	FSettings := TMockSettingsManager.Create;
	FLogger := TMockLogger.Create;
	FDeleteSucceeds := True;
	FDeleteCallCount := 0;
	FFileAttr := 0;
	FSetAttrSucceeds := True;
	FUserChoice := IDIGNORE;
	FAskUserCalled := False;
end;

procedure TLocalFileDeletionHandlerTest.TearDown;
begin
	FHandler := nil;
	FSettings := nil;
	FLogger := nil;
end;

function TLocalFileDeletionHandlerTest.MockDeleteFile(const Path: WideString): Boolean;
begin
	Inc(FDeleteCallCount);
	Result := FDeleteSucceeds;
end;

function TLocalFileDeletionHandlerTest.MockGetFileAttr(const Path: WideString): Integer;
begin
	Result := FFileAttr;
end;

function TLocalFileDeletionHandlerTest.MockSetFileAttr(const Path: WideString; Attr: Integer): Boolean;
begin
	Result := FSetAttrSucceeds;
end;

function TLocalFileDeletionHandlerTest.MockAskUser(const FileName: WideString): Integer;
begin
	FAskUserCalled := True;
	Result := FUserChoice;
end;

procedure TLocalFileDeletionHandlerTest.CreateHandler;
begin
	FHandler := TLocalFileDeletionHandler.Create(
		FSettings,
		FLogger,
		MockDeleteFile,
		MockGetFileAttr,
		MockSetFileAttr,
		MockAskUser
	);
end;

{Success cases}

procedure TLocalFileDeletionHandlerTest.TestDeleteSucceeds_ReturnsOK;
begin
	FDeleteSucceeds := True;
	CreateHandler;

	Assert.AreEqual(FS_FILE_OK, FHandler.DeleteLocalFile('C:\test.txt'));
	Assert.AreEqual(1, FDeleteCallCount, 'Should call delete once');
end;

{Ignore mode tests}

procedure TLocalFileDeletionHandlerTest.TestDeleteFails_IgnoreMode_ReturnsOK;
begin
	FDeleteSucceeds := False;
	FSettings.SetDeleteMode(DeleteFailOnUploadIgnore);
	CreateHandler;

	Assert.AreEqual(FS_FILE_OK, FHandler.DeleteLocalFile('C:\test.txt'));
end;

procedure TLocalFileDeletionHandlerTest.TestDeleteFails_IgnoreMode_Logs;
begin
	FDeleteSucceeds := False;
	FSettings.SetDeleteMode(DeleteFailOnUploadIgnore);
	CreateHandler;

	FHandler.DeleteLocalFile('C:\test.txt');

	Assert.AreEqual(1, FLogger.LogCalls, 'Should log once');
end;

{Abort mode tests}

procedure TLocalFileDeletionHandlerTest.TestDeleteFails_AbortMode_ReturnsNotSupported;
begin
	FDeleteSucceeds := False;
	FSettings.SetDeleteMode(DeleteFailOnUploadAbort);
	CreateHandler;

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, FHandler.DeleteLocalFile('C:\test.txt'));
end;

procedure TLocalFileDeletionHandlerTest.TestDeleteFails_AbortMode_Logs;
begin
	FDeleteSucceeds := False;
	FSettings.SetDeleteMode(DeleteFailOnUploadAbort);
	CreateHandler;

	FHandler.DeleteLocalFile('C:\test.txt');

	Assert.AreEqual(1, FLogger.LogCalls, 'Should log once');
end;

{Ask mode tests}

procedure TLocalFileDeletionHandlerTest.TestDeleteFails_AskMode_UserRetry_RetriesOnce;
var
	RetryCount: Integer;
begin
	FDeleteSucceeds := False;
	FSettings.SetDeleteMode(DeleteFailOnUploadAsk);
	RetryCount := 0;

	FHandler := TLocalFileDeletionHandler.Create(
		FSettings,
		FLogger,
		function(const Path: WideString): Boolean
		begin
			Inc(RetryCount);
			{Fail first time, succeed second time}
			Result := RetryCount > 1;
		end,
		MockGetFileAttr,
		MockSetFileAttr,
		function(const FileName: WideString): Integer
		begin
			{Return IDRETRY only on first ask, then IDIGNORE}
			if RetryCount = 1 then
				Result := IDRETRY
			else
				Result := IDIGNORE;
		end
	);

	FHandler.DeleteLocalFile('C:\test.txt');

	Assert.AreEqual(2, RetryCount, 'Should retry deletion once after user chooses retry');
end;

procedure TLocalFileDeletionHandlerTest.TestDeleteFails_AskMode_UserAbort_ReturnsNotSupported;
begin
	FDeleteSucceeds := False;
	FSettings.SetDeleteMode(DeleteFailOnUploadAsk);
	FUserChoice := IDABORT;
	CreateHandler;

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, FHandler.DeleteLocalFile('C:\test.txt'));
	Assert.IsTrue(FAskUserCalled, 'Should ask user');
end;

procedure TLocalFileDeletionHandlerTest.TestDeleteFails_AskMode_UserIgnore_ReturnsOK;
begin
	FDeleteSucceeds := False;
	FSettings.SetDeleteMode(DeleteFailOnUploadAsk);
	FUserChoice := IDIGNORE;
	CreateHandler;

	Assert.AreEqual(FS_FILE_OK, FHandler.DeleteLocalFile('C:\test.txt'));
	Assert.IsTrue(FAskUserCalled, 'Should ask user');
end;

{DeleteIgnore mode tests}

procedure TLocalFileDeletionHandlerTest.TestDeleteFails_DeleteIgnoreMode_ClearsReadonly_Succeeds;
var
	DeleteAttempts: Integer;
begin
	FSettings.SetDeleteMode(DeleteFailOnUploadDeleteIgnore);
	FFileAttr := faReadOnly;
	FSetAttrSucceeds := True;
	DeleteAttempts := 0;

	FHandler := TLocalFileDeletionHandler.Create(
		FSettings,
		FLogger,
		function(const Path: WideString): Boolean
		begin
			Inc(DeleteAttempts);
			{Fail first attempt, succeed after attribute cleared}
			Result := DeleteAttempts > 1;
		end,
		MockGetFileAttr,
		MockSetFileAttr,
		MockAskUser
	);

	Assert.AreEqual(FS_FILE_OK, FHandler.DeleteLocalFile('C:\test.txt'));
end;

procedure TLocalFileDeletionHandlerTest.TestDeleteFails_DeleteIgnoreMode_CantClearReadonly_ReturnsOK;
begin
	FDeleteSucceeds := False;
	FSettings.SetDeleteMode(DeleteFailOnUploadDeleteIgnore);
	FFileAttr := faReadOnly;
	FSetAttrSucceeds := False; {Can't clear readonly}
	CreateHandler;

	Assert.AreEqual(FS_FILE_OK, FHandler.DeleteLocalFile('C:\test.txt'), 'Should ignore when cant clear readonly');
end;

{DeleteAbort mode tests}

procedure TLocalFileDeletionHandlerTest.TestDeleteFails_DeleteAbortMode_ClearsReadonly_Succeeds;
var
	DeleteAttempts: Integer;
begin
	FSettings.SetDeleteMode(DeleteFailOnUploadDeleteAbort);
	FFileAttr := faReadOnly;
	FSetAttrSucceeds := True;
	DeleteAttempts := 0;

	FHandler := TLocalFileDeletionHandler.Create(
		FSettings,
		FLogger,
		function(const Path: WideString): Boolean
		begin
			Inc(DeleteAttempts);
			Result := DeleteAttempts > 1;
		end,
		MockGetFileAttr,
		MockSetFileAttr,
		MockAskUser
	);

	Assert.AreEqual(FS_FILE_OK, FHandler.DeleteLocalFile('C:\test.txt'));
end;

procedure TLocalFileDeletionHandlerTest.TestDeleteFails_DeleteAbortMode_CantClearReadonly_ReturnsNotSupported;
begin
	FDeleteSucceeds := False;
	FSettings.SetDeleteMode(DeleteFailOnUploadDeleteAbort);
	FFileAttr := faReadOnly;
	FSetAttrSucceeds := False;
	CreateHandler;

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, FHandler.DeleteLocalFile('C:\test.txt'), 'Should abort when cant clear readonly');
end;

{Retry behavior}

procedure TLocalFileDeletionHandlerTest.TestDeleteFails_AskMode_UserRetry_SucceedsOnSecondTry;
var
	Attempts: Integer;
begin
	FSettings.SetDeleteMode(DeleteFailOnUploadAsk);
	Attempts := 0;

	FHandler := TLocalFileDeletionHandler.Create(
		FSettings,
		FLogger,
		function(const Path: WideString): Boolean
		begin
			Inc(Attempts);
			Result := Attempts >= 2; {Succeed on second attempt}
		end,
		MockGetFileAttr,
		MockSetFileAttr,
		function(const FileName: WideString): Integer
		begin
			Result := IDRETRY;
		end
	);

	Assert.AreEqual(FS_FILE_OK, FHandler.DeleteLocalFile('C:\test.txt'));
	Assert.AreEqual(2, Attempts, 'Should succeed on second attempt');
end;

initialization
	TDUnitX.RegisterTestFixture(TLocalFileDeletionHandlerTest);

end.
