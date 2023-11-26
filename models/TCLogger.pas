unit TCLogger;

interface

uses
	SysUtils,
	PLUGIN_TYPES,
	SystemHelper;

type
	TTCLogger = class
	private
		LogProc: TLogProcW;
		PluginNum: Integer;
		LogLevel: Integer;
	public
		constructor Create(); overload; //creates a dummy logger
		constructor Create(LogProc: TLogProcW; PluginNum: Integer; LogLevel: Integer); overload;
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
