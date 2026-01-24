unit OperationActionExecutorTest;

{Unit tests for TOperationActionExecutor - operation lifecycle action execution.
 Tests verify thread state actions (retry, skip lists, abort, paths, background).
 Cloud-related actions (user space logging, description loading) require
 ConnectionManager and are covered in integration tests.}

interface

uses
	System.Classes,
	DUnitX.TestFramework,
	OperationLifecycleHandler,
	OperationActionExecutor,
	ThreadStateManager,
	PluginSettingsManager,
	TCLogger,
	PluginSettings,
	RealPath,
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
		function GetAccountsIniFilePath: WideString;
		procedure Refresh;
	end;

	{Mock logger}
	TMockLogger = class(TInterfacedObject, ILogger)
	public
		LogCalls: Integer;
		LastLogLevel: Integer;
		LastMessage: WideString;
		constructor Create;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
	end;

	[TestFixture]
	TOperationActionExecutorTest = class
	private
		FExecutor: IOperationActionExecutor;
		FThreadState: IThreadStateManager;
		FSettings: TMockSettingsManager;
		FLogger: TMockLogger;

		function CreatePath(const Account, Path: WideString): TRealPath;
		procedure CreateExecutor;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Retry counter tests}
		[Test]
		procedure TestExecute_ResetRetryDownload_ResetsCounter;
		[Test]
		procedure TestExecute_ResetRetryUpload_ResetsCounter;
		[Test]
		procedure TestExecute_ResetRetryRenMov_ResetsCounter;
		[Test]
		procedure TestExecute_MultipleRetryResets_ResetsAll;

		{Skip list flag tests}
		[Test]
		procedure TestExecute_SetSkipListDelete_SetsFlag;
		[Test]
		procedure TestExecute_ClearSkipListDelete_ClearsFlag;
		[Test]
		procedure TestExecute_SetSkipListRenMov_SetsFlag;
		[Test]
		procedure TestExecute_ClearSkipListRenMov_ClearsFlag;

		{Abort control tests}
		[Test]
		procedure TestExecute_SetCanAbortRenMov_SetsFlag;
		[Test]
		procedure TestExecute_ClearCanAbortRenMov_ClearsFlag;

		{Skipped path tests}
		[Test]
		procedure TestExecute_CreateSkippedPath_CreatesPath;
		[Test]
		procedure TestExecute_ClearSkippedPath_ClearsPath;

		{Background job tests}
		[Test]
		procedure TestExecute_IncrementBackgroundJobs_IncrementsForAccount;
		[Test]
		procedure TestExecute_DecrementBackgroundJobs_DecrementsForAccount;

		{Background thread tests}
		[Test]
		procedure TestExecute_SetBackgroundThreadStatus_SetsOperation;
		[Test]
		procedure TestExecute_RemoveBackgroundThread_RemovesThread;

		{Warning action tests}
		[Test]
		procedure TestExecute_WarnPublicAccountCopy_LogsAndSetsSkip;

		{Empty actions test}
		[Test]
		procedure TestExecute_EmptyActions_DoesNothing;

		{Combined actions test}
		[Test]
		procedure TestExecute_MultipleActions_ExecutesAll;
	end;

implementation

uses
	SysUtils,
	CMRConstants,
	LANGUAGE_STRINGS;

{TMockSettingsManager}

constructor TMockSettingsManager.Create;
begin
	inherited Create;
	FSettings.DescriptionFileName := 'descript.ion';
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

function TMockSettingsManager.GetAccountsIniFilePath: WideString;
begin
	Result := EmptyWideStr;
end;

procedure TMockSettingsManager.Refresh;
begin
	{No-op}
end;

{TMockLogger}

constructor TMockLogger.Create;
begin
	inherited Create;
	LogCalls := 0;
	LastLogLevel := 0;
	LastMessage := '';
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	Inc(LogCalls);
	LastLogLevel := LogLevel;
	LastMessage := LogString;
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
begin
	Inc(LogCalls);
	LastLogLevel := LogLevel;
	LastMessage := LogString;
end;

{TOperationActionExecutorTest}

procedure TOperationActionExecutorTest.Setup;
begin
	FThreadState := TThreadStateManager.Create;
	FSettings := TMockSettingsManager.Create;
	FLogger := TMockLogger.Create;
end;

procedure TOperationActionExecutorTest.TearDown;
begin
	FExecutor := nil;
	FThreadState := nil;
	FSettings := nil;
	FLogger := nil;
end;

function TOperationActionExecutorTest.CreatePath(const Account, Path: WideString): TRealPath;
begin
	Result := Default(TRealPath);
	Result.account := Account;
	Result.Path := Path;
end;

procedure TOperationActionExecutorTest.CreateExecutor;
begin
	{Pass nil for ConnectionManager and Descriptions - not needed for thread state tests}
	FExecutor := TOperationActionExecutor.Create(FThreadState, nil, FSettings, nil, FLogger);
end;

{Retry counter tests}

procedure TOperationActionExecutorTest.TestExecute_ResetRetryDownload_ResetsCounter;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	FThreadState.IncrementRetryCountDownload;
	FThreadState.IncrementRetryCountDownload;
	Assert.AreEqual(2, FThreadState.GetRetryCountDownload, 'Precondition: counter should be 2');

	Actions := [oaResetRetryDownload];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.AreEqual(0, FThreadState.GetRetryCountDownload, 'Counter should be reset');
end;

procedure TOperationActionExecutorTest.TestExecute_ResetRetryUpload_ResetsCounter;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	FThreadState.IncrementRetryCountUpload;
	Assert.AreEqual(1, FThreadState.GetRetryCountUpload, 'Precondition: counter should be 1');

	Actions := [oaResetRetryUpload];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.AreEqual(0, FThreadState.GetRetryCountUpload, 'Counter should be reset');
end;

procedure TOperationActionExecutorTest.TestExecute_ResetRetryRenMov_ResetsCounter;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	FThreadState.IncrementRetryCountRenMov;
	Assert.AreEqual(1, FThreadState.GetRetryCountRenMov, 'Precondition: counter should be 1');

	Actions := [oaResetRetryRenMov];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.AreEqual(0, FThreadState.GetRetryCountRenMov, 'Counter should be reset');
end;

procedure TOperationActionExecutorTest.TestExecute_MultipleRetryResets_ResetsAll;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	FThreadState.IncrementRetryCountDownload;
	FThreadState.IncrementRetryCountUpload;
	FThreadState.IncrementRetryCountRenMov;

	Actions := [oaResetRetryDownload, oaResetRetryUpload, oaResetRetryRenMov];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.AreEqual(0, FThreadState.GetRetryCountDownload, 'Download counter should be reset');
	Assert.AreEqual(0, FThreadState.GetRetryCountUpload, 'Upload counter should be reset');
	Assert.AreEqual(0, FThreadState.GetRetryCountRenMov, 'RenMov counter should be reset');
end;

{Skip list flag tests}

procedure TOperationActionExecutorTest.TestExecute_SetSkipListDelete_SetsFlag;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	Assert.IsFalse(FThreadState.GetSkipListDelete, 'Precondition: flag should be false');

	Actions := [oaSetSkipListDelete];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.IsTrue(FThreadState.GetSkipListDelete, 'Flag should be set');
end;

procedure TOperationActionExecutorTest.TestExecute_ClearSkipListDelete_ClearsFlag;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	FThreadState.SetSkipListDelete(True);
	Assert.IsTrue(FThreadState.GetSkipListDelete, 'Precondition: flag should be true');

	Actions := [oaClearSkipListDelete];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.IsFalse(FThreadState.GetSkipListDelete, 'Flag should be cleared');
end;

procedure TOperationActionExecutorTest.TestExecute_SetSkipListRenMov_SetsFlag;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	Assert.IsFalse(FThreadState.GetSkipListRenMov, 'Precondition: flag should be false');

	Actions := [oaSetSkipListRenMov];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.IsTrue(FThreadState.GetSkipListRenMov, 'Flag should be set');
end;

procedure TOperationActionExecutorTest.TestExecute_ClearSkipListRenMov_ClearsFlag;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	FThreadState.SetSkipListRenMov(True);
	Assert.IsTrue(FThreadState.GetSkipListRenMov, 'Precondition: flag should be true');

	Actions := [oaClearSkipListRenMov];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.IsFalse(FThreadState.GetSkipListRenMov, 'Flag should be cleared');
end;

{Abort control tests}

procedure TOperationActionExecutorTest.TestExecute_SetCanAbortRenMov_SetsFlag;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	Assert.IsFalse(FThreadState.GetCanAbortRenMov, 'Precondition: flag should be false');

	Actions := [oaSetCanAbortRenMov];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.IsTrue(FThreadState.GetCanAbortRenMov, 'Flag should be set');
end;

procedure TOperationActionExecutorTest.TestExecute_ClearCanAbortRenMov_ClearsFlag;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	FThreadState.SetCanAbortRenMov(True);
	Assert.IsTrue(FThreadState.GetCanAbortRenMov, 'Precondition: flag should be true');

	Actions := [oaClearCanAbortRenMov];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.IsFalse(FThreadState.GetCanAbortRenMov, 'Flag should be cleared');
end;

{Skipped path tests}

procedure TOperationActionExecutorTest.TestExecute_CreateSkippedPath_CreatesPath;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	Assert.IsFalse(FThreadState.HasRemoveDirSkippedPath, 'Precondition: path should not exist');

	Actions := [oaCreateSkippedPath];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.IsTrue(FThreadState.HasRemoveDirSkippedPath, 'Path list should be created');
end;

procedure TOperationActionExecutorTest.TestExecute_ClearSkippedPath_ClearsPath;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	FThreadState.CreateRemoveDirSkippedPath;
	FThreadState.AddSkippedPath('/some/path');
	Assert.IsTrue(FThreadState.HasRemoveDirSkippedPath, 'Precondition: path should exist');

	Actions := [oaClearSkippedPath];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.IsFalse(FThreadState.HasRemoveDirSkippedPath, 'Path list should be cleared');
end;

{Background job tests}

procedure TOperationActionExecutorTest.TestExecute_IncrementBackgroundJobs_IncrementsForAccount;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	Assert.AreEqual(0, FThreadState.GetBackgroundJobsCount('testaccount'), 'Precondition: count should be 0');

	Actions := [oaIncrementBackgroundJobs];
	FExecutor.Execute(Actions, CreatePath('testaccount', '/'), 0);

	Assert.AreEqual(1, FThreadState.GetBackgroundJobsCount('testaccount'), 'Count should be incremented');
end;

procedure TOperationActionExecutorTest.TestExecute_DecrementBackgroundJobs_DecrementsForAccount;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	FThreadState.IncrementBackgroundJobs('testaccount');
	FThreadState.IncrementBackgroundJobs('testaccount');
	Assert.AreEqual(2, FThreadState.GetBackgroundJobsCount('testaccount'), 'Precondition: count should be 2');

	Actions := [oaDecrementBackgroundJobs];
	FExecutor.Execute(Actions, CreatePath('testaccount', '/'), 0);

	Assert.AreEqual(1, FThreadState.GetBackgroundJobsCount('testaccount'), 'Count should be decremented');
end;

{Background thread tests}

procedure TOperationActionExecutorTest.TestExecute_SetBackgroundThreadStatus_SetsOperation;
var
	Actions: TOperationActions;
begin
	CreateExecutor;

	Actions := [oaSetBackgroundThreadStatus];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 42);

	Assert.AreEqual(42, FThreadState.GetBackgroundThreadStatus, 'Operation should be stored');
end;

procedure TOperationActionExecutorTest.TestExecute_RemoveBackgroundThread_RemovesThread;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	FThreadState.SetBackgroundThreadStatus(99);
	Assert.AreEqual(99, FThreadState.GetBackgroundThreadStatus, 'Precondition: status should be set');

	Actions := [oaRemoveBackgroundThread];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.AreEqual(0, FThreadState.GetBackgroundThreadStatus, 'Status should be removed (returns 0)');
end;

{Warning action tests}

procedure TOperationActionExecutorTest.TestExecute_WarnPublicAccountCopy_LogsAndSetsSkip;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	Assert.IsFalse(FThreadState.GetSkipListRenMov, 'Precondition: skip flag should be false');
	Assert.AreEqual(0, FLogger.LogCalls, 'Precondition: no logs yet');

	Actions := [oaWarnPublicAccountCopy];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	Assert.AreEqual(1, FLogger.LogCalls, 'Should log warning');
	Assert.AreEqual(LOG_LEVEL_WARNING, FLogger.LastLogLevel, 'Should be warning level');
	Assert.IsTrue(FThreadState.GetSkipListRenMov, 'Skip flag should be set');
end;

{Empty actions test}

procedure TOperationActionExecutorTest.TestExecute_EmptyActions_DoesNothing;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	FThreadState.IncrementRetryCountDownload;
	FThreadState.SetSkipListDelete(True);

	Actions := [];
	FExecutor.Execute(Actions, CreatePath('test', '/'), 0);

	{Verify state unchanged}
	Assert.AreEqual(1, FThreadState.GetRetryCountDownload, 'Counter should be unchanged');
	Assert.IsTrue(FThreadState.GetSkipListDelete, 'Flag should be unchanged');
	Assert.AreEqual(0, FLogger.LogCalls, 'No logs should occur');
end;

{Combined actions test}

procedure TOperationActionExecutorTest.TestExecute_MultipleActions_ExecutesAll;
var
	Actions: TOperationActions;
begin
	CreateExecutor;
	FThreadState.IncrementRetryCountDownload;
	FThreadState.IncrementRetryCountUpload;

	Actions := [oaResetRetryDownload, oaSetSkipListDelete, oaSetCanAbortRenMov, oaIncrementBackgroundJobs];
	FExecutor.Execute(Actions, CreatePath('account1', '/folder'), 0);

	Assert.AreEqual(0, FThreadState.GetRetryCountDownload, 'Download counter should be reset');
	Assert.AreEqual(1, FThreadState.GetRetryCountUpload, 'Upload counter should be unchanged');
	Assert.IsTrue(FThreadState.GetSkipListDelete, 'Skip delete flag should be set');
	Assert.IsTrue(FThreadState.GetCanAbortRenMov, 'Abort flag should be set');
	Assert.AreEqual(1, FThreadState.GetBackgroundJobsCount('account1'), 'Background jobs should be incremented');
end;

initialization
	TDUnitX.RegisterTestFixture(TOperationActionExecutorTest);

end.
