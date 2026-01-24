unit TCProgress;

{Interface for progress reporting, abstracting the concrete implementation from consumers.
	This enables dependency injection and testability by allowing mock implementations.}

interface

uses
	SysUtils,
	PLUGIN_TYPES;

type
	{Progress reporting interface for file operations.
		Returns True if operation should be cancelled, False to continue.}
	IProgress = interface
		['{C7F5FC9E-E036-4E8E-97EC-8B99EC4B6A60}']
		function Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(SourceName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(PercentDone: Integer): Boolean; overload;
		function Aborted(): Boolean;
	end;

	{Null object implementation of IProgress. All methods return False (not cancelled).
		Use when progress reporting is not needed, e.g., in tests or standalone operations.}
	TNullProgress = class(TInterfacedObject, IProgress)
	public
		function Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(SourceName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(PercentDone: Integer): Boolean; overload;
		function Aborted(): Boolean;
	end;

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

{TNullProgress}

function TNullProgress.Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean;
begin
	Result := False; {Not cancelled}
end;

function TNullProgress.Progress(SourceName: WideString; PercentDone: Integer): Boolean;
begin
	Result := False; {Not cancelled}
end;

function TNullProgress.Progress(PercentDone: Integer): Boolean;
begin
	Result := False; {Not cancelled}
end;

function TNullProgress.Aborted(): Boolean;
begin
	Result := False; {Not aborted}
end;

{TTCProgress}

constructor TTCProgress.Create(ProgressProc: TProgressProcW; PluginNum: Integer);
begin
	self.ProgressProc := ProgressProc;
	self.PluginNum := PluginNum;
end;

function TTCProgress.Aborted: Boolean;
begin
	{When nil passed as a parameter, TC won't change it}
	Result := ProgressProc(PluginNum, nil, nil, 0) = 1;
end;

function TTCProgress.Progress(PercentDone: Integer): Boolean;
begin
	Result := ProgressProc(PluginNum, nil, nil, PercentDone) = 1;
end;

function TTCProgress.Progress(SourceName: WideString; PercentDone: Integer): Boolean;
begin
	Result := ProgressProc(PluginNum, PWideChar(SourceName), nil, PercentDone) = 1;
end;

function TTCProgress.Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean;
begin
	Result := ProgressProc(PluginNum, PWideChar(SourceName), PWideChar(TargetName), PercentDone) = 1;
end;

end.
