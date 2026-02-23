unit MockLogger;

{Shared mock ILogger for testing. Tracks call count, last parameters,
	full log history, and provides a Reset method for multi-step tests.}

interface

uses
	System.SysUtils,
	Logger;

type
	TLogEntry = record
		LogLevel: Integer;
		MsgType: Integer;
		Message: WideString;
	end;

	TMockLogger = class(TInterfacedObject, ILogger)
	public
		LogCalls: Integer;
		LogCalled: Boolean;
		LastLogLevel: Integer;
		LastMsgType: Integer;
		LastMessage: WideString;
		Entries: TArray<TLogEntry>;

		constructor Create;
		procedure Reset;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
		function HasLogWithLevel(Level: Integer): Boolean;
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
	Entries := nil;
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
var
	Entry: TLogEntry;
begin
	Inc(LogCalls);
	LogCalled := True;
	LastLogLevel := LogLevel;
	LastMsgType := MsgType;
	LastMessage := LogString;
	Entry.LogLevel := LogLevel;
	Entry.MsgType := MsgType;
	Entry.Message := LogString;
	SetLength(Entries, Length(Entries) + 1);
	Entries[High(Entries)] := Entry;
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
var
	Entry: TLogEntry;
begin
	Inc(LogCalls);
	LogCalled := True;
	LastLogLevel := LogLevel;
	LastMsgType := MsgType;
	LastMessage := Format(LogString, Args);
	Entry.LogLevel := LogLevel;
	Entry.MsgType := MsgType;
	Entry.Message := LastMessage;
	SetLength(Entries, Length(Entries) + 1);
	Entries[High(Entries)] := Entry;
end;

function TMockLogger.HasLogWithLevel(Level: Integer): Boolean;
var
	I: Integer;
begin
	Result := False;
	for I := 0 to High(Entries) do
		if Entries[I].LogLevel = Level then
			Exit(True);
end;

end.
