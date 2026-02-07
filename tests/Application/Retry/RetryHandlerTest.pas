unit RetryHandlerTest;

{Unit tests for TRetryHandler - operation retry logic.
 Tests all error modes: Ask, Ignore, Abort, Retry.}

interface

uses
	System.SysUtils,
	System.Classes,
	System.Generics.Collections,
	DUnitX.TestFramework,
	ThreadStateManager,
	MockSettingsManager,
	PluginSettingsManager,
	RetryHandler,
	PluginSettings,
	WFXTypes,
	SettingsConstants,
	TCHandler;

type
	{Shared test state for callbacks - avoids anonymous method capture issues}
	TTestState = class
	public
		MsgBoxCalls: Integer;
		MsgBoxLastMessage: WideString;
		MsgBoxResponses: TList<Integer>;
		MsgBoxResponseIndex: Integer;

		LogCalls: Integer;
		LogLastMessage: WideString;

		OperationCalls: Integer;
		OperationResults: TList<Integer>;
		OperationResultIndex: Integer;

		AbortCheckCalls: Integer;
		AbortCheckResult: Boolean;

		constructor Create;
		destructor Destroy; override;
		procedure Reset;
	end;

	[TestFixture]
	TRetryHandlerTest = class
	private
		FHandler: IRetryHandler;
		FThreadState: IThreadStateManager;
		FSettingsManager: TMockSettingsManager;
		FSettingsIntf: IPluginSettingsManager;
		FState: TTestState;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Ignore mode tests}
		[Test]
		procedure TestIgnoreMode_ReturnsCurrentResult;

		{Abort mode tests}
		[Test]
		procedure TestAbortMode_ReturnsUserAbort;

		{Ask mode tests}
		[Test]
		procedure TestAskMode_UserSelectsAbort_ReturnsUserAbort;
		[Test]
		procedure TestAskMode_UserSelectsIgnore_ReturnsCurrentResult;
		[Test]
		procedure TestAskMode_UserSelectsRetry_CallsOperation;
		[Test]
		procedure TestAskMode_RetrySucceeds_ReturnsOK;
		[Test]
		procedure TestAskMode_MultipleRetries_StopsOnSuccess;

		{Retry mode tests}
		[Test]
		procedure TestRetryMode_IncrementsCounter;
		[Test]
		procedure TestRetryMode_RespectsMaxAttempts;
		[Test]
		procedure TestRetryMode_ResetsCounterOnSuccess;
		[Test]
		procedure TestRetryMode_AbortCheckStopsRetry;
		[Test]
		procedure TestRetryMode_LogsEachAttempt;

		{Operation type tests}
		[Test]
		procedure TestRetryMode_Download_UsesDownloadCounter;
		[Test]
		procedure TestRetryMode_Upload_UsesUploadCounter;
		[Test]
		procedure TestRetryMode_RenMov_UsesRenMovCounter;
		[Test]
		procedure TestRetryMode_Upload_ResetsCounterOnSuccess;
		[Test]
		procedure TestRetryMode_RenMov_ResetsCounterOnSuccess;
		[Test]
		procedure TestGetRetryCount_UnknownType_ReturnsZero;
	end;

implementation

uses
	Windows;

var
	{Global state pointer for callbacks - set before each test}
	GState: TTestState;

{Callback functions that use global state to avoid anonymous method capture issues}

function TestMsgBox(const Text: WideString; const Args: array of const;
	const Caption: WideString; Flags: Integer): Integer;
begin
	Inc(GState.MsgBoxCalls);
	GState.MsgBoxLastMessage := Text;

	if GState.MsgBoxResponseIndex < GState.MsgBoxResponses.Count then
	begin
		Result := GState.MsgBoxResponses[GState.MsgBoxResponseIndex];
		Inc(GState.MsgBoxResponseIndex);
	end
	else
		Result := ID_IGNORE;
end;

procedure TestLog(LogLevel, MsgType: Integer; const Msg: WideString; const Args: array of const);
begin
	Inc(GState.LogCalls);
	GState.LogLastMessage := Msg;
end;

function TestOperation: Integer;
begin
	Inc(GState.OperationCalls);
	if GState.OperationResultIndex < GState.OperationResults.Count then
	begin
		Result := GState.OperationResults[GState.OperationResultIndex];
		Inc(GState.OperationResultIndex);
	end
	else
		Result := FS_FILE_NOTFOUND;
end;

function TestAbortCheck: Boolean;
begin
	Inc(GState.AbortCheckCalls);
	Result := GState.AbortCheckResult;
end;

{TTestState}

constructor TTestState.Create;
begin
	inherited Create;
	MsgBoxResponses := TList<Integer>.Create;
	OperationResults := TList<Integer>.Create;
	Reset;
end;

destructor TTestState.Destroy;
begin
	MsgBoxResponses.Free;
	OperationResults.Free;
	inherited;
end;

procedure TTestState.Reset;
begin
	MsgBoxCalls := 0;
	MsgBoxLastMessage := '';
	MsgBoxResponses.Clear;
	MsgBoxResponseIndex := 0;

	LogCalls := 0;
	LogLastMessage := '';

	OperationCalls := 0;
	OperationResults.Clear;
	OperationResultIndex := 0;

	AbortCheckCalls := 0;
	AbortCheckResult := False;
end;

{TRetryHandlerTest}

procedure TRetryHandlerTest.Setup;
begin
	FState := TTestState.Create;
	GState := FState; {Set global state pointer}

	FThreadState := TThreadStateManager.Create;
	FSettingsManager := TMockSettingsManager.Create;
	FSettingsManager.SetOperationErrorMode(OperationErrorModeIgnore);
	FSettingsManager.SetRetryAttempts(3);
	FSettingsManager.SetAttemptWait(0); {No wait in tests}
	FSettingsIntf := FSettingsManager;

	FHandler := TRetryHandler.Create(FThreadState, FSettingsIntf, TNullTCHandler.Create, TestMsgBox, TestLog);
end;

procedure TRetryHandlerTest.TearDown;
begin
	FHandler := nil;
	FSettingsIntf := nil;
	FSettingsManager := nil;
	FThreadState := nil;
	GState := nil;
	FreeAndNil(FState);
end;

{Ignore mode tests}

procedure TRetryHandlerTest.TestIgnoreMode_ReturnsCurrentResult;
var
	Result: Integer;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeIgnore);

	Result := FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(FS_FILE_NOTFOUND, Result, 'Should return original result');
	Assert.AreEqual(0, FState.OperationCalls, 'Should not call operation');
	Assert.AreEqual(0, FState.MsgBoxCalls, 'Should not show message box');
end;

{Abort mode tests}

procedure TRetryHandlerTest.TestAbortMode_ReturnsUserAbort;
var
	Result: Integer;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeAbort);

	Result := FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(FS_FILE_USERABORT, Result, 'Should return user abort');
	Assert.AreEqual(0, FState.OperationCalls, 'Should not call operation');
end;

{Ask mode tests}

procedure TRetryHandlerTest.TestAskMode_UserSelectsAbort_ReturnsUserAbort;
var
	Result: Integer;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeAsk);
	FState.MsgBoxResponses.Add(ID_ABORT);

	Result := FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(FS_FILE_USERABORT, Result, 'Should return user abort');
	Assert.AreEqual(1, FState.MsgBoxCalls, 'Should show message box once');
end;

procedure TRetryHandlerTest.TestAskMode_UserSelectsIgnore_ReturnsCurrentResult;
var
	Result: Integer;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeAsk);
	FState.MsgBoxResponses.Add(ID_IGNORE);

	Result := FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(FS_FILE_NOTFOUND, Result, 'Should return original result');
	Assert.AreEqual(1, FState.MsgBoxCalls, 'Should show message box once');
	Assert.AreEqual(0, FState.OperationCalls, 'Should not retry');
end;

procedure TRetryHandlerTest.TestAskMode_UserSelectsRetry_CallsOperation;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeAsk);
	FState.MsgBoxResponses.Add(ID_RETRY);
	FState.MsgBoxResponses.Add(ID_IGNORE); {Stop after one retry}
	FState.OperationResults.Add(FS_FILE_NOTFOUND); {Retry still fails}

	FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(1, FState.OperationCalls, 'Should call operation once');
	Assert.AreEqual(2, FState.MsgBoxCalls, 'Should show dialog twice');
end;

procedure TRetryHandlerTest.TestAskMode_RetrySucceeds_ReturnsOK;
var
	Result: Integer;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeAsk);
	FState.MsgBoxResponses.Add(ID_RETRY);
	FState.OperationResults.Add(FS_FILE_OK);

	Result := FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(FS_FILE_OK, Result, 'Should return OK after successful retry');
	Assert.AreEqual(1, FState.MsgBoxCalls, 'Should show dialog once before retry');
end;

procedure TRetryHandlerTest.TestAskMode_MultipleRetries_StopsOnSuccess;
var
	Result: Integer;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeAsk);
	FState.MsgBoxResponses.Add(ID_RETRY);
	FState.MsgBoxResponses.Add(ID_RETRY);
	FState.MsgBoxResponses.Add(ID_RETRY);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_OK);

	Result := FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(FS_FILE_OK, Result, 'Should return OK');
	Assert.AreEqual(3, FState.OperationCalls, 'Should retry 3 times');
	Assert.AreEqual(3, FState.MsgBoxCalls, 'Should show 3 dialogs');
end;

{Retry mode tests}

procedure TRetryHandlerTest.TestRetryMode_IncrementsCounter;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeRetry);
	FSettingsManager.SetRetryAttempts(2);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);

	FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(2, FState.OperationCalls, 'Should retry twice');
	Assert.AreEqual(2, FState.LogCalls, 'Should log each attempt');
end;

procedure TRetryHandlerTest.TestRetryMode_RespectsMaxAttempts;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeRetry);
	FSettingsManager.SetRetryAttempts(3);

	{All operations fail}
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);

	FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(3, FState.OperationCalls, 'Should stop at max attempts');
end;

procedure TRetryHandlerTest.TestRetryMode_ResetsCounterOnSuccess;
var
	Result: Integer;
	CountBefore, CountAfter: Integer;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeRetry);
	FSettingsManager.SetRetryAttempts(5);

	{Fail twice, then succeed}
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_OK);

	CountBefore := FThreadState.GetRetryCountDownload;

	Result := FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	CountAfter := FThreadState.GetRetryCountDownload;

	Assert.AreEqual(FS_FILE_OK, Result, 'Should return OK');
	Assert.AreEqual(CountBefore, CountAfter, 'Counter should be reset to original value');
end;

procedure TRetryHandlerTest.TestRetryMode_AbortCheckStopsRetry;
var
	Result: Integer;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeRetry);
	FSettingsManager.SetRetryAttempts(10);
	FState.AbortCheckResult := True; {Simulate user abort}
	FState.OperationResults.Add(FS_FILE_NOTFOUND);

	Result := FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(FS_FILE_USERABORT, Result, 'Should return user abort');
	Assert.AreEqual(1, FState.OperationCalls, 'Should stop after first abort check');
end;

procedure TRetryHandlerTest.TestRetryMode_LogsEachAttempt;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeRetry);
	FSettingsManager.SetRetryAttempts(3);

	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_OK);

	FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Custom retry message', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(3, FState.LogCalls, 'Should log each retry attempt');
	Assert.AreEqual('Custom retry message', FState.LogLastMessage, 'Should use provided log message');
end;

{Operation type tests}

procedure TRetryHandlerTest.TestRetryMode_Download_UsesDownloadCounter;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeRetry);
	FSettingsManager.SetRetryAttempts(2);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);

	{Verify download counter is used, not upload or renmov}
	Assert.AreEqual(0, FThreadState.GetRetryCountDownload, 'Download counter should start at 0');

	FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotDownload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	{Counter should be incremented to max (2), not reset because operation failed}
	Assert.AreEqual(2, FThreadState.GetRetryCountDownload, 'Download counter should be at max');
	Assert.AreEqual(0, FThreadState.GetRetryCountUpload, 'Upload counter should be unchanged');
	Assert.AreEqual(0, FThreadState.GetRetryCountRenMov, 'RenMov counter should be unchanged');
end;

procedure TRetryHandlerTest.TestRetryMode_Upload_UsesUploadCounter;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeRetry);
	FSettingsManager.SetRetryAttempts(2);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);

	FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotUpload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(0, FThreadState.GetRetryCountDownload, 'Download counter should be unchanged');
	Assert.AreEqual(2, FThreadState.GetRetryCountUpload, 'Upload counter should be at max');
	Assert.AreEqual(0, FThreadState.GetRetryCountRenMov, 'RenMov counter should be unchanged');
end;

procedure TRetryHandlerTest.TestRetryMode_RenMov_UsesRenMovCounter;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeRetry);
	FSettingsManager.SetRetryAttempts(2);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_NOTFOUND);

	FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotRenMov,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(0, FThreadState.GetRetryCountDownload, 'Download counter should be unchanged');
	Assert.AreEqual(0, FThreadState.GetRetryCountUpload, 'Upload counter should be unchanged');
	Assert.AreEqual(2, FThreadState.GetRetryCountRenMov, 'RenMov counter should be at max');
end;

procedure TRetryHandlerTest.TestRetryMode_Upload_ResetsCounterOnSuccess;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeRetry);
	FSettingsManager.SetRetryAttempts(5);

	{ Fail once, then succeed }
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_OK);

	FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotUpload,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(0, FThreadState.GetRetryCountUpload, 'Upload counter should be reset after success');
end;

procedure TRetryHandlerTest.TestRetryMode_RenMov_ResetsCounterOnSuccess;
begin
	FSettingsManager.SetOperationErrorMode(OperationErrorModeRetry);
	FSettingsManager.SetRetryAttempts(5);

	{ Fail once, then succeed }
	FState.OperationResults.Add(FS_FILE_NOTFOUND);
	FState.OperationResults.Add(FS_FILE_OK);

	FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		rotRenMov,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(0, FThreadState.GetRetryCountRenMov, 'RenMov counter should be reset after success');
end;

procedure TRetryHandlerTest.TestGetRetryCount_UnknownType_ReturnsZero;
var
	UnknownType: TRetryOperationType;
	Count: Integer;
begin
	{Unknown operation type falls through to default case returning 0.
	 RetryAttempts=0 so the while loop condition (GetRetryCount <> RetryAttempts)
	 evaluates to (0 <> 0) = False and exits immediately.}
	FSettingsManager.SetOperationErrorMode(OperationErrorModeRetry);
	FSettingsManager.SetRetryAttempts(0);

	{Range check must be disabled to cast out-of-range integer to enum}
	{$RANGECHECKS OFF}
	UnknownType := TRetryOperationType(99);
	{$RANGECHECKS ON}

	Count := FHandler.HandleOperationError(
		FS_FILE_NOTFOUND,
		UnknownType,
		'Ask message', 'Title', 'Retry log', 'param',
		TestOperation,
		TestAbortCheck
	);

	Assert.AreEqual(FS_FILE_NOTFOUND, Count, 'Unknown operation type should return original error');
	Assert.AreEqual(0, FState.OperationCalls, 'Should not retry when GetRetryCount returns 0');
end;

initialization
	TDUnitX.RegisterTestFixture(TRetryHandlerTest);

end.
