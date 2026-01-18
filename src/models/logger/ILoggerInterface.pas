unit ILoggerInterface;

interface

type
	{Generic logging interface for dependency injection.
	 Implementations decide where and how to output log messages.}
	ILogger = interface
		['{A0C88D15-B097-48F4-823B-804F65C774B4}']
		{Logs a message at the specified level.
		 @param LogLevel Bitmask indicating message severity (LOG_LEVEL_* constants)
		 @param MsgType Message type for display formatting (MSGTYPE_* constants)
		 @param LogString The message text}
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;

		{Logs a formatted message at the specified level.
		 @param LogLevel Bitmask indicating message severity (LOG_LEVEL_* constants)
		 @param MsgType Message type for display formatting (MSGTYPE_* constants)
		 @param LogString Format string with placeholders
		 @param Args Arguments to substitute into format string}
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
	end;

	{Null implementation that discards all log messages.
	 Use when logging is not needed (e.g., in unit tests or silent operations).}
	TNullLogger = class(TInterfacedObject, ILogger)
	public
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
	end;

implementation

{TNullLogger}

procedure TNullLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	{No-op: intentionally discards all log messages}
end;

procedure TNullLogger.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
begin
	{No-op: intentionally discards all log messages}
end;

end.
