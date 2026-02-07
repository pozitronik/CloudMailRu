unit DownloadOrchestratorTest;

{Tests for TDownloadOrchestrator.
 Verifies download flow orchestration: validation, conflict resolution, retry handling.}

interface

uses
	System.Classes,
	DUnitX.TestFramework,
	CloudCallbackTypes,
	DownloadOrchestrator,
	DownloadPreparationValidator,
	LocalFileConflictResolver,
	RetryHandler,
	MockSettingsManager,
	PluginSettingsManager,
	RealPath,
	WFXTypes;

type
	[TestFixture]
	TDownloadOrchestratorTest = class
	private
		FOrchestrator: IDownloadOrchestrator;
		FMockValidator: IDownloadPreparationValidator;
		FMockConflictResolver: ILocalFileConflictResolver;
		FMockRetryHandler: IRetryHandler;
		FMockSettingsManager: IPluginSettingsManager;

		FDownloadCallCount: Integer;
		FProgressCallCount: Integer;
		FLastDownloadPath: TRealPath;
	public
		[Setup]
		procedure Setup;

		[TearDown]
		procedure TearDown;

		[Test]
		procedure Execute_WhenValidationFails_ReturnsValidationResultCode;

		[Test]
		procedure Execute_WhenValidationFails_DoesNotCallDownload;

		[Test]
		procedure Execute_WhenConflictResolutionFails_ReturnsConflictResultCode;

		[Test]
		procedure Execute_WhenConflictResolutionFails_DoesNotCallDownload;

		[Test]
		procedure Execute_WhenDownloadSucceeds_ReturnsFS_FILE_OK;

		[Test]
		procedure Execute_CallsProgressBeforeConflictResolution;

		[Test]
		procedure Execute_PassesCorrectPathToDownloadCallback;

		{Retry handler tests}
		[Test]
		procedure Execute_WhenDownloadReturnsReadError_CallsRetryHandler;

		[Test]
		procedure Execute_WhenDownloadReturnsWriteError_DoesNotCallRetryHandler;

		[Test]
		procedure Execute_WhenRetryHandlerReturnsOK_ReturnsOK;

		[Test]
		procedure Execute_WhenRetryHandlerReturnsError_ReturnsError;

		{Retry handler callback coverage - verify closures passed to retry handler}
		[Test]
		procedure Execute_RetryCallback_InvokesDownloadOp;
		[Test]
		procedure Execute_AbortCheckCallback_InvokesProgressOp;
	end;

implementation

uses
	SysUtils,
	PluginSettings,
	LanguageStrings;

type
	{Mock validator that can be configured to pass or fail}
	TMockDownloadValidator = class(TInterfacedObject, IDownloadPreparationValidator)
	private
		FShouldProceed: Boolean;
		FResultCode: Integer;
	public
		constructor Create(ShouldProceed: Boolean; ResultCode: Integer);
		function Validate(const RemotePath: TRealPath; CopyFlags: Integer): TDownloadValidationResult;
	end;

	{Mock conflict resolver}
	TMockConflictResolver = class(TInterfacedObject, ILocalFileConflictResolver)
	private
		FShouldProceed: Boolean;
		FResultCode: Integer;
	public
		constructor Create(ShouldProceed: Boolean; ResultCode: Integer);
		function Resolve(const LocalPath: WideString; CopyFlags: Integer;
			OverwriteMode: Integer): TConflictResolution;
	end;

	{Mock retry handler with configurable behavior}
	TMockRetryHandler = class(TInterfacedObject, IRetryHandler)
	private
		FWasCalled: Boolean;
		FReturnValue: Integer;
	public
		constructor Create; overload;
		constructor Create(ReturnValue: Integer); overload;
		function HandleOperationError(
			CurrentResult: Integer;
			OperationType: TRetryOperationType;
			const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
			RetryOperation: TRetryOperation;
			AbortCheck: TAbortCheckFunc
		): Integer;
		property WasCalled: Boolean read FWasCalled;
	end;

	{Mock retry handler that invokes the captured callbacks to exercise closure code}
	TCallbackInvokingRetryHandler = class(TInterfacedObject, IRetryHandler)
	private
		FReturnValue: Integer;
		FRetryOpResult: Integer;
		FAbortCheckResult: Boolean;
	public
		constructor Create(ReturnValue: Integer);
		function HandleOperationError(
			CurrentResult: Integer;
			OperationType: TRetryOperationType;
			const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
			RetryOperation: TRetryOperation;
			AbortCheck: TAbortCheckFunc
		): Integer;
		property RetryOpResult: Integer read FRetryOpResult;
		property AbortCheckResult: Boolean read FAbortCheckResult;
	end;

{TMockDownloadValidator}

constructor TMockDownloadValidator.Create(ShouldProceed: Boolean; ResultCode: Integer);
begin
	inherited Create;
	FShouldProceed := ShouldProceed;
	FResultCode := ResultCode;
end;

function TMockDownloadValidator.Validate(const RemotePath: TRealPath;
	CopyFlags: Integer): TDownloadValidationResult;
begin
	Result.ShouldProceed := FShouldProceed;
	Result.ResultCode := FResultCode;
end;

{TMockConflictResolver}

constructor TMockConflictResolver.Create(ShouldProceed: Boolean; ResultCode: Integer);
begin
	inherited Create;
	FShouldProceed := ShouldProceed;
	FResultCode := ResultCode;
end;

function TMockConflictResolver.Resolve(const LocalPath: WideString;
	CopyFlags: Integer; OverwriteMode: Integer): TConflictResolution;
begin
	Result.ShouldProceed := FShouldProceed;
	Result.ResultCode := FResultCode;
end;

{TMockRetryHandler}

constructor TMockRetryHandler.Create;
begin
	inherited Create;
	FWasCalled := False;
	FReturnValue := FS_FILE_READERROR; {Default to pass-through behavior}
end;

constructor TMockRetryHandler.Create(ReturnValue: Integer);
begin
	inherited Create;
	FWasCalled := False;
	FReturnValue := ReturnValue;
end;

function TMockRetryHandler.HandleOperationError(
	CurrentResult: Integer;
	OperationType: TRetryOperationType;
	const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
	RetryOperation: TRetryOperation;
	AbortCheck: TAbortCheckFunc
): Integer;
begin
	FWasCalled := True;
	Result := FReturnValue;
end;

{TCallbackInvokingRetryHandler}

constructor TCallbackInvokingRetryHandler.Create(ReturnValue: Integer);
begin
	inherited Create;
	FReturnValue := ReturnValue;
end;

function TCallbackInvokingRetryHandler.HandleOperationError(
	CurrentResult: Integer;
	OperationType: TRetryOperationType;
	const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
	RetryOperation: TRetryOperation;
	AbortCheck: TAbortCheckFunc
): Integer;
begin
	FRetryOpResult := RetryOperation();
	FAbortCheckResult := AbortCheck();
	Result := FReturnValue;
end;

{TDownloadOrchestratorTest}

procedure TDownloadOrchestratorTest.Setup;
begin
	FDownloadCallCount := 0;
	FProgressCallCount := 0;
	FMockValidator := TMockDownloadValidator.Create(True, FS_FILE_OK);
	FMockConflictResolver := TMockConflictResolver.Create(True, FS_FILE_OK);
	FMockRetryHandler := TMockRetryHandler.Create;
	FMockSettingsManager := TMockSettingsManager.Create;
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);
end;

procedure TDownloadOrchestratorTest.TearDown;
begin
	FOrchestrator := nil;
	FMockValidator := nil;
	FMockConflictResolver := nil;
	FMockRetryHandler := nil;
	FMockSettingsManager := nil;
end;

procedure TDownloadOrchestratorTest.Execute_WhenValidationFails_ReturnsValidationResultCode;
var
	Result: Integer;
begin
	FMockValidator := TMockDownloadValidator.Create(False, FS_FILE_NOTSUPPORTED);
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	Result := FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Inc(FDownloadCallCount);
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Inc(FProgressCallCount);
			Result := False;
		end);

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'Should return validation result code');
end;

procedure TDownloadOrchestratorTest.Execute_WhenValidationFails_DoesNotCallDownload;
begin
	FMockValidator := TMockDownloadValidator.Create(False, FS_FILE_NOTSUPPORTED);
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Inc(FDownloadCallCount);
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual(0, FDownloadCallCount, 'Should not call download when validation fails');
end;

procedure TDownloadOrchestratorTest.Execute_WhenConflictResolutionFails_ReturnsConflictResultCode;
var
	Result: Integer;
begin
	FMockConflictResolver := TMockConflictResolver.Create(False, FS_FILE_EXISTS);
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	Result := FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Inc(FDownloadCallCount);
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual(FS_FILE_EXISTS, Result, 'Should return conflict result code');
end;

procedure TDownloadOrchestratorTest.Execute_WhenConflictResolutionFails_DoesNotCallDownload;
begin
	FMockConflictResolver := TMockConflictResolver.Create(False, FS_FILE_EXISTS);
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Inc(FDownloadCallCount);
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual(0, FDownloadCallCount, 'Should not call download when conflict resolution fails');
end;

procedure TDownloadOrchestratorTest.Execute_WhenDownloadSucceeds_ReturnsFS_FILE_OK;
var
	Result: Integer;
begin
	Result := FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual(FS_FILE_OK, Result, 'Should return FS_FILE_OK on success');
end;

procedure TDownloadOrchestratorTest.Execute_CallsProgressBeforeConflictResolution;
begin
	FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Inc(FProgressCallCount);
			Result := False;
		end);

	Assert.IsTrue(FProgressCallCount >= 1, 'Should call progress at least once');
end;

procedure TDownloadOrchestratorTest.Execute_PassesCorrectPathToDownloadCallback;
begin
	FOrchestrator.Execute('\account\folder\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			FLastDownloadPath := Path;
			Result := FS_FILE_OK;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual('account', FLastDownloadPath.account, 'Account should be parsed correctly');
	Assert.AreEqual('folder\test.txt', FLastDownloadPath.Path, 'Path should be parsed correctly');
end;

procedure TDownloadOrchestratorTest.Execute_WhenDownloadReturnsReadError_CallsRetryHandler;
var
	MockRetry: TMockRetryHandler;
begin
	MockRetry := TMockRetryHandler.Create(FS_FILE_READERROR);
	FMockRetryHandler := MockRetry;
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Result := FS_FILE_READERROR;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.IsTrue(MockRetry.WasCalled, 'Should call retry handler when download returns read error');
end;

procedure TDownloadOrchestratorTest.Execute_WhenDownloadReturnsWriteError_DoesNotCallRetryHandler;
var
	MockRetry: TMockRetryHandler;
begin
	MockRetry := TMockRetryHandler.Create(FS_FILE_WRITEERROR);
	FMockRetryHandler := MockRetry;
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Result := FS_FILE_WRITEERROR;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.IsFalse(MockRetry.WasCalled, 'Should not call retry handler for write errors');
end;

procedure TDownloadOrchestratorTest.Execute_WhenRetryHandlerReturnsOK_ReturnsOK;
var
	Result: Integer;
begin
	FMockRetryHandler := TMockRetryHandler.Create(FS_FILE_OK);
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	Result := FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Result := FS_FILE_READERROR;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual(FS_FILE_OK, Result, 'Should return OK when retry handler succeeds');
end;

procedure TDownloadOrchestratorTest.Execute_WhenRetryHandlerReturnsError_ReturnsError;
var
	Result: Integer;
begin
	FMockRetryHandler := TMockRetryHandler.Create(FS_FILE_USERABORT);
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	Result := FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Result := FS_FILE_READERROR;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual(FS_FILE_USERABORT, Result, 'Should return error code from retry handler');
end;

procedure TDownloadOrchestratorTest.Execute_RetryCallback_InvokesDownloadOp;
var
	InvokingRetry: TCallbackInvokingRetryHandler;
begin
	InvokingRetry := TCallbackInvokingRetryHandler.Create(FS_FILE_OK);
	FMockRetryHandler := InvokingRetry;
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Inc(FDownloadCallCount);
			Result := FS_FILE_READERROR;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := False;
		end);

	Assert.AreEqual(2, FDownloadCallCount,
		'Download should be called twice: once normally, once via retry callback');
	Assert.AreEqual(Integer(FS_FILE_READERROR), InvokingRetry.RetryOpResult,
		'Retry callback should return same result as DownloadOp');
end;

procedure TDownloadOrchestratorTest.Execute_AbortCheckCallback_InvokesProgressOp;
var
	InvokingRetry: TCallbackInvokingRetryHandler;
begin
	InvokingRetry := TCallbackInvokingRetryHandler.Create(FS_FILE_OK);
	FMockRetryHandler := InvokingRetry;
	FOrchestrator := TDownloadOrchestrator.Create(
		FMockValidator, FMockConflictResolver, FMockRetryHandler, FMockSettingsManager);

	FOrchestrator.Execute('\account\test.txt', 'C:\local.txt', 0,
		function(const Path: TRealPath; const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer
		begin
			Result := FS_FILE_READERROR;
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Inc(FProgressCallCount);
			Result := True; {Simulates user abort}
		end);

	Assert.AreEqual(2, FProgressCallCount,
		'Progress should be called twice: once before download, once via abort check');
	Assert.IsTrue(InvokingRetry.AbortCheckResult,
		'Abort check callback should return True when ProgressOp signals abort');
end;

initialization
	TDUnitX.RegisterTestFixture(TDownloadOrchestratorTest);

end.
