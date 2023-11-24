unit TCProgress;

interface

uses
	SysUtils,
	PLUGIN_TYPES,
	SystemHelper;

type
	TTCProgress = class
	private
		PluginNum: Integer;
		ProgressProc: TProgressProcW;
	public
		constructor Create(); overload; //creates a dummy progress
		constructor Create(ProgressProc: TProgressProcW; PluginNum: Integer); overload;
		function Progress(SourceName, TargetName: WideString; PercentDone: Integer): Integer; //todo: check if result can be boolean
	end;

implementation

{TTCProgress}

constructor TTCProgress.Create;
begin
	self.ProgressProc := nil;
	self.PluginNum := -1;
end;

constructor TTCProgress.Create(ProgressProc: TProgressProcW; PluginNum: Integer);
begin
	self.ProgressProc := ProgressProc;
	self.PluginNum := PluginNum;
end;

function TTCProgress.Progress(SourceName, TargetName: WideString; PercentDone: Integer): Integer;
begin
	Result := -1;
	if Assigned(ProgressProc) then
		Result := ProgressProc(PluginNum, PWideChar(SourceName), PWideChar(TargetName), PercentDone);
end;

end.
