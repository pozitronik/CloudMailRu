unit Logger;

interface

uses
	SysUtils,
	WFXTypes;

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

	{Logger implementation that outputs to Total Commander's connection log window.
		Filters messages based on configured log level bitmask.}
	TTCLogger = class(TInterfacedObject, ILogger)
	private
		LogProc: TLogProcW;
		PluginNum: Integer;
		LogLevel: Integer;
	public
		{Creates a dummy logger that discards all messages.
			Use when TC callbacks are not available.}
		constructor Create(); overload;

		{Creates a logger connected to Total Commander.
			@param LogProc TC callback for logging
			@param PluginNum Plugin instance number
			@param LogLevel Bitmask of enabled log levels}
		constructor Create(LogProc: TLogProcW; PluginNum: Integer; LogLevel: Integer); overload;

		{ILogger}
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

{TTCLogger}

constructor TTCLogger.Create(LogProc: TLogProcW; PluginNum: Integer; LogLevel: Integer);
begin
	self.LogProc := LogProc;
	self.LogLevel := LogLevel;
	self.PluginNum := PluginNum;
end;

constructor TTCLogger.Create;
begin
	self.LogProc := nil;
	self.LogLevel := 0;
	self.PluginNum := -1;
end;

procedure TTCLogger.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
begin
	if (self.LogLevel and LogLevel) <> 0 then
	begin
		LogString := Format(LogString, Args);
		Log(LogLevel, MsgType, LogString);
	end;

end;

procedure TTCLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	if (self.LogLevel and LogLevel) <> 0 then
		LogProc(PluginNum, MsgType, PWideChar(LogString));
end;

end.
