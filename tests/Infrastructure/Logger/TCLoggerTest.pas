unit TCLoggerTest;

interface

uses
	TCLogger,
	WFXTypes,
	CloudConstants,
	TestHelper,
	SysUtils,
	DUnitX.TestFramework;

type

	[TestFixture]
	TTCLoggerTest = class
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;
		[Test]
		procedure TestLog;
		[Test]
		procedure TestCreateDummy;
		[Test]
		procedure TestFilterLevel;
		[Test]
		{Verifies TTCLogger implements ILogger interface}
		procedure TestImplementsILogger;
	end;

	[TestFixture]
	TNullLoggerTest = class
	public
		[Test]
		{Verifies TNullLogger can be assigned to ILogger variable}
		procedure TestImplementsILogger;

		[Test]
		{Verifies Log without args executes without errors}
		procedure TestLogDoesNotCrash;

		[Test]
		{Verifies Log with format args executes without errors}
		procedure TestLogWithArgsDoesNotCrash;

		[Test]
		{Verifies multiple sequential Log calls work correctly}
		procedure TestMultipleLogCalls;
	end;

var
	PluginNr: integer;
	MsgType: integer;
	LogString: WideString;

procedure TestLoggerProc(Plugin_Num, Msg_Type: integer; Log_String: PWideChar); stdcall;

implementation

procedure TestLoggerProc(Plugin_Num, Msg_Type: integer; Log_String: PWideChar);
begin
	PluginNr := Plugin_Num;
	MsgType := Msg_Type;
	LogString := Log_String;
end;

procedure TTCLoggerTest.TestCreateDummy;
var
	TestLogger: TTCLogger;
begin
	Assert.AreEqual(0, PluginNr);
	Assert.AreEqual(0, MsgType);
	Assert.AreEqual('', LogString);

	TestLogger := TTCLogger.Create();
	TestLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, 'detailed message');

	Assert.AreEqual(0, PluginNr);
	Assert.AreEqual(0, MsgType);
	Assert.AreEqual('', LogString);
end;

procedure TTCLoggerTest.TestFilterLevel;
var
	TestLogger: TTCLogger;
	randomPN: integer;
	randomStr: WideString;
begin
	randomPN := Random(100);
	randomStr := RandomString(32);

	Assert.AreEqual(0, PluginNr);
	Assert.AreEqual(0, MsgType);
	Assert.AreEqual('', LogString);

	TestLogger := TTCLogger.Create(@TestLoggerProc, randomPN, LOG_LEVEL_CONNECT + LOG_LEVEL_FILE_OPERATION + LOG_LEVEL_DETAIL + LOG_LEVEL_WARNING + LOG_LEVEL_ERROR);

	TestLogger.Log(LOG_LEVEL_DEBUG, msgtype_details, 'debug message');

	Assert.AreEqual(0, PluginNr);
	Assert.AreEqual(0, MsgType);
	Assert.AreEqual('', LogString);

end;

procedure TTCLoggerTest.TestLog;
var
	TestLogger: TTCLogger;
	randomPN: integer;
	randomStr: WideString;
begin
	randomPN := Random(100);
	randomStr := RandomString(32);

	Assert.AreEqual(0, PluginNr);
	Assert.AreEqual(0, MsgType);
	Assert.AreEqual('', LogString);

	TestLogger := TTCLogger.Create(@TestLoggerProc, randomPN, LOG_LEVEL_CONNECT + LOG_LEVEL_FILE_OPERATION + LOG_LEVEL_DETAIL + LOG_LEVEL_WARNING + LOG_LEVEL_ERROR + LOG_LEVEL_DEBUG);

	TestLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, 'detailed message');

	Assert.AreEqual(randomPN, PluginNr);
	Assert.AreEqual(msgtype_details, MsgType);
	Assert.AreEqual('detailed message', LogString);

	TestLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, 'formatted message %s, %d', [randomStr, randomPN]);

	Assert.AreEqual(randomPN, PluginNr);
	Assert.AreEqual(msgtype_details, MsgType);
	Assert.AreEqual(Format('formatted message %s, %d', [randomStr, randomPN]), LogString);

end;

procedure TTCLoggerTest.TestImplementsILogger;
var
	Logger: ILogger;
	TestLogger: TTCLogger;
	randomPN: integer;
begin
	randomPN := Random(100);

	TestLogger := TTCLogger.Create(@TestLoggerProc, randomPN, LOG_LEVEL_DEBUG);
	Logger := TestLogger;

	Assert.IsNotNull(Logger, 'TTCLogger should be assignable to ILogger');

	Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, 'interface test');
	Assert.AreEqual('interface test', LogString, 'Log via ILogger should work');
end;

procedure TTCLoggerTest.Setup;
begin
	PluginNr := 0;
	MsgType := 0;
	LogString := '';
	Randomize;
end;

procedure TTCLoggerTest.TearDown;
begin
end;

{TNullLoggerTest}

procedure TNullLoggerTest.TestImplementsILogger;
var
	Logger: ILogger;
begin
	Logger := TNullLogger.Create;
	Assert.IsNotNull(Logger);
end;

procedure TNullLoggerTest.TestLogDoesNotCrash;
var
	Logger: ILogger;
begin
	Logger := TNullLogger.Create;
	Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, 'test message');
	Assert.Pass('Log completed without exception');
end;

procedure TNullLoggerTest.TestLogWithArgsDoesNotCrash;
var
	Logger: ILogger;
begin
	Logger := TNullLogger.Create;
	Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, 'test message %s %d', ['arg', 42]);
	Assert.Pass('Log with args completed without exception');
end;

procedure TNullLoggerTest.TestMultipleLogCalls;
var
	Logger: ILogger;
	i: Integer;
begin
	Logger := TNullLogger.Create;
	for i := 1 to 100 do
	begin
		Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, 'message %d', [i]);
	end;
	Assert.Pass('Multiple log calls completed without exception');
end;

initialization

TDUnitX.RegisterTestFixture(TTCLoggerTest);
TDUnitX.RegisterTestFixture(TNullLoggerTest);

end.
