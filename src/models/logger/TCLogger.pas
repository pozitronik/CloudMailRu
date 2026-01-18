unit TCLogger;

interface

uses
	SysUtils,
	PLUGIN_TYPES,
	SystemHelper,
	ILoggerInterface;

type
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
	if CheckFlag(LogLevel, self.LogLevel) then
	begin
		LogString := Format(LogString, Args);
		Log(LogLevel, MsgType, LogString);
	end;

end;

procedure TTCLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	if CheckFlag(LogLevel, self.LogLevel) then
		LogProc(PluginNum, MsgType, PWideChar(LogString));
end;

end.
