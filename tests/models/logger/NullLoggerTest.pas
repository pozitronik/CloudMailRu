unit NullLoggerTest;

interface

uses
	ILoggerInterface,
	CMRConstants,
	PLUGIN_TYPES,
	DUnitX.TestFramework;

type
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

implementation

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

TDUnitX.RegisterTestFixture(TNullLoggerTest);

end.
