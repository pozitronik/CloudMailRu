unit TCLoggerTest;

interface

uses
	TCLogger,
	Plugin_TYPES,
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

initialization

TDUnitX.RegisterTestFixture(TTCLoggerTest);

end.
