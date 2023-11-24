unit TCLogger;

interface

uses
	SysUtils,
	PLUGIN_TYPES,
	SystemHelper;

const
	//Уровни логирования (по степеням двойки)
	LOG_LEVEL_CONNECT = 1; //connection
	LOG_LEVEL_FILE_OPERATION = 2; //file operations && free space
	LOG_LEVEL_DETAIL = 4; //some detailed info (i.e. retry data or smth)
	LOG_LEVEL_WARNING = 8; //non-critical warnings
	LOG_LEVEL_ERROR = 16; //error details
	LOG_LEVEL_DEBUG = 32; //also same internal debugging info

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
