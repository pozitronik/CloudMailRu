unit RetryHandler;

{Handles operation retry logic based on configured error mode.
	Extracts common retry pattern from FsGetFile, FsPutFile, and RenMovFile operations.}

interface

uses
	SysUtils,
	ThreadStateManager,
	PluginSettingsManager,
	TCHandler;

type
	{Identifies which retry counter to use for tracking attempts}
	TRetryOperationType = (rotDownload, rotUpload, rotRenMov);

	{Callback type for the operation to retry}
	TRetryOperation = reference to function: Integer;

	{Callback type for checking if user aborted}
	TAbortCheck = reference to function: Boolean;

	IRetryHandler = interface
		['{1360F478-B642-4EBB-AD57-20ECDFF67C8F}']

		{Handle operation error based on configured error mode.
			Implements the common retry pattern: Ask/Ignore/Abort/Retry modes.
			@param CurrentResult The failed operation result code
			@param OperationType Which retry counter to use (Download/Upload/RenMov)
			@param AskMessage Message template for Ask mode dialog
			@param AskTitle Title for Ask mode dialog
			@param RetryLogMessage Log message template for retry attempts
			@param FormatParam Single format parameter for messages (filename or error text)
			@param RetryOperation Callback that performs the actual operation
			@param AbortCheck Callback that checks if user requested abort
			@return Final operation result after handling}
		function HandleOperationError(CurrentResult: Integer; OperationType: TRetryOperationType; const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString; RetryOperation: TRetryOperation; AbortCheck: TAbortCheck): Integer;
	end;

	{Callback for showing message box - injectable for testing}
	TMsgBoxCallback = reference to function(const Text: WideString; const Args: array of const; const Caption: WideString; Flags: Integer): Integer;

	{Callback for logging - injectable for testing}
	TLogCallback = reference to procedure(LogLevel, MsgType: Integer; const Msg: WideString; const Args: array of const);

	TRetryHandler = class(TInterfacedObject, IRetryHandler)
	private
		FThreadState: IThreadStateManager;
		FSettingsManager: IPluginSettingsManager;
		FTCHandler: ITCHandler;
		FMsgBox: TMsgBoxCallback;
		FLog: TLogCallback;

		function GetRetryCount(OperationType: TRetryOperationType): Integer;
		procedure IncrementRetryCount(OperationType: TRetryOperationType);
		procedure ResetRetryCount(OperationType: TRetryOperationType);

		function HandleAskMode(CurrentResult: Integer; const AskMessage, AskTitle, FormatParam: WideString; RetryOperation: TRetryOperation): Integer;

		function HandleRetryMode(CurrentResult: Integer; OperationType: TRetryOperationType; const RetryLogMessage, FormatParam: WideString; RetryOperation: TRetryOperation; AbortCheck: TAbortCheck): Integer;
	public
		{Create with required dependencies.
			@param ThreadState Thread state manager for retry counters
			@param SettingsManager Settings manager for error mode configuration
			@param TCHandler TC handler for window handle in message boxes
			@param MsgBoxCallback Optional message box callback (uses WindowsHelper.MsgBox if nil)
			@param LogCallback Optional log callback (uses TCLogger.Log if nil)}
		constructor Create(ThreadState: IThreadStateManager; SettingsManager: IPluginSettingsManager; TCHandler: ITCHandler; MsgBoxCallback: TMsgBoxCallback = nil; LogCallback: TLogCallback = nil);

		function HandleOperationError(CurrentResult: Integer; OperationType: TRetryOperationType; const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString; RetryOperation: TRetryOperation; AbortCheck: TAbortCheck): Integer;
	end;

implementation

uses
	Windows,
	PLUGIN_TYPES,
	SETTINGS_CONSTANTS,
	CMRConstants,
	TCLogger,
	WindowsHelper,
	SystemHelper;

const
	MB_ABORTRETRYIGNORE_ICONERROR = MB_ABORTRETRYIGNORE + MB_ICONERROR;

{Default Log implementation - no-op. Caller should provide real callback.}
procedure DefaultLog(LogLevel, MsgType: Integer; const Msg: WideString; const Args: array of const);
begin
	{No-op - caller provides logging callback if logging is needed}
end;

constructor TRetryHandler.Create(ThreadState: IThreadStateManager; SettingsManager: IPluginSettingsManager; TCHandler: ITCHandler; MsgBoxCallback: TMsgBoxCallback; LogCallback: TLogCallback);
begin
	inherited Create;
	FThreadState := ThreadState;
	FSettingsManager := SettingsManager;
	FTCHandler := TCHandler;

	if Assigned(MsgBoxCallback) then
		FMsgBox := MsgBoxCallback
	else
		FMsgBox := function(const Text: WideString; const Args: array of const; const Caption: WideString; Flags: Integer): Integer
			begin
				Result := MsgBox(FTCHandler.FindTCWindow, Text, Args, Caption, Flags);
			end;

	if Assigned(LogCallback) then
		FLog := LogCallback
	else
		FLog := DefaultLog;
end;

function TRetryHandler.GetRetryCount(OperationType: TRetryOperationType): Integer;
begin
	case OperationType of
		rotDownload:
			Result := FThreadState.GetRetryCountDownload;
		rotUpload:
			Result := FThreadState.GetRetryCountUpload;
		rotRenMov:
			Result := FThreadState.GetRetryCountRenMov;
		else
			Result := 0;
	end;
end;

procedure TRetryHandler.IncrementRetryCount(OperationType: TRetryOperationType);
begin
	case OperationType of
		rotDownload:
			FThreadState.IncrementRetryCountDownload;
		rotUpload:
			FThreadState.IncrementRetryCountUpload;
		rotRenMov:
			FThreadState.IncrementRetryCountRenMov;
	end;
end;

procedure TRetryHandler.ResetRetryCount(OperationType: TRetryOperationType);
begin
	case OperationType of
		rotDownload:
			FThreadState.ResetRetryCountDownload;
		rotUpload:
			FThreadState.ResetRetryCountUpload;
		rotRenMov:
			FThreadState.ResetRetryCountRenMov;
	end;
end;

function TRetryHandler.HandleAskMode(CurrentResult: Integer; const AskMessage, AskTitle, FormatParam: WideString; RetryOperation: TRetryOperation): Integer;
begin
	Result := CurrentResult;
	while not(Result in [FS_FILE_OK, FS_FILE_USERABORT]) do
	begin
		case FMsgBox(AskMessage, [FormatParam], AskTitle, MB_ABORTRETRYIGNORE_ICONERROR) of
			ID_ABORT:
				Result := FS_FILE_USERABORT;
			ID_RETRY:
				Result := RetryOperation();
			ID_IGNORE:
				break;
		end;
	end;
end;

function TRetryHandler.HandleRetryMode(CurrentResult: Integer; OperationType: TRetryOperationType; const RetryLogMessage, FormatParam: WideString; RetryOperation: TRetryOperation; AbortCheck: TAbortCheck): Integer;
var
	RetryAttempts, CurrentCount: Integer;
begin
	Result := CurrentResult;
	RetryAttempts := FSettingsManager.GetSettings.RetryAttempts;

	while (GetRetryCount(OperationType) <> RetryAttempts) and not(Result in [FS_FILE_OK, FS_FILE_USERABORT]) do
	begin
		IncrementRetryCount(OperationType);
		CurrentCount := GetRetryCount(OperationType);
		FLog(LOG_LEVEL_DETAIL, msgtype_details, RetryLogMessage, [FormatParam, CurrentCount, RetryAttempts]);

		Result := RetryOperation();

		if AbortCheck() then
			Result := FS_FILE_USERABORT;

		if Result in [FS_FILE_OK, FS_FILE_USERABORT] then
			ResetRetryCount(OperationType);

		ProcessMessages;
		Sleep(FSettingsManager.GetSettings.AttemptWait);
	end;
end;

function TRetryHandler.HandleOperationError(CurrentResult: Integer; OperationType: TRetryOperationType; const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString; RetryOperation: TRetryOperation; AbortCheck: TAbortCheck): Integer;
begin
	Result := CurrentResult;

	case FSettingsManager.GetSettings.OperationErrorMode of
		OperationErrorModeAsk:
			Result := HandleAskMode(Result, AskMessage, AskTitle, FormatParam, RetryOperation);
		OperationErrorModeIgnore:
			; {Just return current result - caller will exit}
		OperationErrorModeAbort:
			Result := FS_FILE_USERABORT;
		OperationErrorModeRetry:
			Result := HandleRetryMode(Result, OperationType, RetryLogMessage, FormatParam, RetryOperation, AbortCheck);
	end;
end;

end.
