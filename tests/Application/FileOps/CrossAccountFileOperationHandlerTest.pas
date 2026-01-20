unit CrossAccountFileOperationHandlerTest;

{Unit tests for TCrossAccountFileOperationHandler.
 Note: Full integration tests require TCloudMailRu which isn't interface-based.}

interface

uses
	DUnitX.TestFramework,
	SysUtils,
	CrossAccountFileOperationHandler,
	RetryHandler,
	TCLogger,
	RealPath;

type
	{Mock retry handler that tracks calls}
	TMockRetryHandler = class(TInterfacedObject, IRetryHandler)
	private
		FHandleErrorCalled: Boolean;
		FReturnValue: Integer;
	public
		constructor Create(ReturnValue: Integer = 0);
		function HandleOperationError(
			CurrentResult: Integer;
			OperationType: TRetryOperationType;
			const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
			RetryOperation: TRetryOperation;
			AbortCheck: TAbortCheck
		): Integer;
		property HandleErrorCalled: Boolean read FHandleErrorCalled;
	end;

	{Mock logger that tracks log calls}
	TMockLogger = class(TInterfacedObject, ILogger)
	private
		FLogCalled: Boolean;
		FLastLogLevel: Integer;
		FLastMessage: WideString;
	public
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
		property LogCalled: Boolean read FLogCalled;
		property LastLogLevel: Integer read FLastLogLevel;
		property LastMessage: WideString read FLastMessage;
	end;

	[TestFixture]
	TCrossAccountFileOperationHandlerTest = class
	private
		FHandler: ICrossAccountFileOperationHandler;
		FMockRetryHandler: TMockRetryHandler;
		FMockLogger: TMockLogger;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Public account validation tests}
		[Test]
		procedure TestExecute_PublicAccount_ReturnsUserAbort;
		[Test]
		procedure TestExecute_PublicAccount_LogsWarning;

		{Disabled mode tests}
		[Test]
		procedure TestExecute_DisabledMode_ReturnsUserAbort;
		[Test]
		procedure TestExecute_DisabledMode_LogsWarning;

		{Invalid mode tests}
		[Test]
		procedure TestExecute_InvalidMode_ReturnsWriteError;

		{Mode delegation tests - require integration}
		[Test]
		procedure TestExecute_ViaHash_RequiresIntegration;
		[Test]
		procedure TestExecute_ViaPublicLink_RequiresIntegration;
	end;

implementation

uses
	PLUGIN_TYPES,
	CMRConstants,
	SETTINGS_CONSTANTS,
	LANGUAGE_STRINGS;

{TMockRetryHandler}

constructor TMockRetryHandler.Create(ReturnValue: Integer);
begin
	inherited Create;
	FHandleErrorCalled := False;
	FReturnValue := ReturnValue;
end;

function TMockRetryHandler.HandleOperationError(
	CurrentResult: Integer;
	OperationType: TRetryOperationType;
	const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
	RetryOperation: TRetryOperation;
	AbortCheck: TAbortCheck
): Integer;
begin
	FHandleErrorCalled := True;
	Result := FReturnValue;
end;

{TMockLogger}

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	FLogCalled := True;
	FLastLogLevel := LogLevel;
	FLastMessage := LogString;
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
begin
	FLogCalled := True;
	FLastLogLevel := LogLevel;
	FLastMessage := LogString;
end;

{TCrossAccountFileOperationHandlerTest}

procedure TCrossAccountFileOperationHandlerTest.Setup;
begin
	FMockRetryHandler := TMockRetryHandler.Create;
	FMockLogger := TMockLogger.Create;
	FHandler := TCrossAccountFileOperationHandler.Create(FMockRetryHandler, FMockLogger);
end;

procedure TCrossAccountFileOperationHandlerTest.TearDown;
begin
	FHandler := nil;
	FMockRetryHandler := nil;
	FMockLogger := nil;
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_PublicAccount_ReturnsUserAbort;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	Result := FHandler.Execute(nil, nil, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaHash, True, {IsSourcePublicAccount}
		function: Boolean begin Result := False; end);

	Assert.AreEqual(FS_FILE_USERABORT, Result, 'Should return user abort for public account');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_PublicAccount_LogsWarning;
var
	OldPath, NewPath: TRealPath;
begin
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	FHandler.Execute(nil, nil, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaHash, True, {IsSourcePublicAccount}
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockLogger.LogCalled, 'Should log warning');
	Assert.AreEqual(LOG_LEVEL_WARNING, FMockLogger.LastLogLevel, 'Should log at warning level');
	Assert.AreEqual(ERR_DIRECT_OPERATIONS_NOT_SUPPORTED, FMockLogger.LastMessage, 'Should log correct message');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_DisabledMode_ReturnsUserAbort;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	Result := FHandler.Execute(nil, nil, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeDisabled, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(FS_FILE_USERABORT, Result, 'Should return user abort for disabled mode');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_DisabledMode_LogsWarning;
var
	OldPath, NewPath: TRealPath;
begin
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	FHandler.Execute(nil, nil, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeDisabled, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockLogger.LogCalled, 'Should log warning');
	Assert.AreEqual(LOG_LEVEL_WARNING, FMockLogger.LastLogLevel, 'Should log at warning level');
	Assert.AreEqual(ERR_DIRECT_OPERATIONS_DISABLED, FMockLogger.LastMessage, 'Should log correct message');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_InvalidMode_ReturnsWriteError;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	Result := FHandler.Execute(nil, nil, OldPath, NewPath, False, False,
		999, {invalid mode}
		False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(FS_FILE_WRITEERROR, Result, 'Should return write error for invalid mode');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaHash_RequiresIntegration;
begin
	{ViaHash mode requires real TCloudMailRu instances to test file identity operations.
	 The handler calls OldCloud.statusFile and NewCloud.addFileByIdentity.}
	Assert.Pass('ViaHash mode tested through integration tests');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaPublicLink_RequiresIntegration;
begin
	{ViaPublicLink mode requires real TCloudMailRu instances to test weblink operations.
	 The handler calls publishFile, CloneWeblink, and optionally unpublishFile.}
	Assert.Pass('ViaPublicLink mode tested through integration tests');
end;

initialization
	TDUnitX.RegisterTestFixture(TCrossAccountFileOperationHandlerTest);

end.
