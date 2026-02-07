unit MockLogger;

{Shared mock ILogger for testing. Tracks call count, last parameters,
	and provides a Reset method for multi-step tests.}

interface

uses
	System.SysUtils,
	Logger;

type
	TMockLogger = class(TInterfacedObject, ILogger)
	public
		LogCalls: Integer;
		LogCalled: Boolean;
		LastLogLevel: Integer;
		LastMsgType: Integer;
		LastMessage: WideString;

		constructor Create;
		procedure Reset;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
	end;

implementation

constructor TMockLogger.Create;
begin
	inherited Create;
	Reset;
end;

procedure TMockLogger.Reset;
begin
	LogCalls := 0;
	LogCalled := False;
	LastLogLevel := 0;
	LastMsgType := 0;
	LastMessage := '';
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	Inc(LogCalls);
	LogCalled := True;
	LastLogLevel := LogLevel;
	LastMsgType := MsgType;
	LastMessage := LogString;
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
begin
	Inc(LogCalls);
	LogCalled := True;
	LastLogLevel := LogLevel;
	LastMsgType := MsgType;
	LastMessage := Format(LogString, Args);
end;

end.
