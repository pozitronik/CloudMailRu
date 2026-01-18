unit TCProgress;

interface

uses
	SysUtils,
	IProgressInterface,
	PLUGIN_TYPES;

type
	TTCProgress = class(TInterfacedObject, IProgress)
	private
		PluginNum: Integer;
		ProgressProc: TProgressProcW;
	public
		constructor Create(ProgressProc: TProgressProcW; PluginNum: Integer);
		{Progress() methods return True when current operation is cancelled}
		function Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(SourceName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(PercentDone: Integer): Boolean; overload;
		function Aborted(): Boolean; {call without any parameters is used to check if the operation cancelled or not}
	end;

implementation

{TTCProgress}

constructor TTCProgress.Create(ProgressProc: TProgressProcW; PluginNum: Integer);
begin
	self.ProgressProc := ProgressProc;
	self.PluginNum := PluginNum;
end;

function TTCProgress.Aborted: Boolean;
begin
	Result := False;
	if Assigned(ProgressProc) then
		{When nil passed as a parameter, TC won't change it}
		Result := ProgressProc(PluginNum, nil, nil, 0) = 1;
end;

function TTCProgress.Progress(PercentDone: Integer): Boolean;
begin
	Result := False;
	if Assigned(ProgressProc) then
		Result := ProgressProc(PluginNum, nil, nil, PercentDone) = 1;
end;

function TTCProgress.Progress(SourceName: WideString; PercentDone: Integer): Boolean;
begin
	Result := False;
	if Assigned(ProgressProc) then
		Result := ProgressProc(PluginNum, PWideChar(SourceName), nil, PercentDone) = 1;
end;

function TTCProgress.Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean;
begin
	Result := False;
	if Assigned(ProgressProc) then
		Result := ProgressProc(PluginNum, PWideChar(SourceName), PWideChar(TargetName), PercentDone) = 1;
end;

end.
