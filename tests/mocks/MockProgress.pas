unit MockProgress;

{Shared mock IProgress for testing. Tracks call count, last parameters,
	and supports configurable abort behavior.}

interface

uses
	Progress;

type
	TMockProgress = class(TInterfacedObject, IProgress)
	private
		FAbortOnProgress: Boolean;
	public
		ProgressCalls: Integer;
		ProgressCalled: Boolean;
		LastSourceName: WideString;
		LastTargetName: WideString;
		LastPercentDone: Integer;
		LastPath: WideString;

		constructor Create;
		procedure SetAbortOnProgress(Value: Boolean);

		function Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(SourceName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(PercentDone: Integer): Boolean; overload;
		function Aborted: Boolean;
	end;

implementation

constructor TMockProgress.Create;
begin
	inherited Create;
	ProgressCalls := 0;
	ProgressCalled := False;
	FAbortOnProgress := False;
	LastPercentDone := 0;
	LastSourceName := '';
	LastTargetName := '';
	LastPath := '';
end;

procedure TMockProgress.SetAbortOnProgress(Value: Boolean);
begin
	FAbortOnProgress := Value;
end;

function TMockProgress.Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean;
begin
	Inc(ProgressCalls);
	ProgressCalled := True;
	LastSourceName := SourceName;
	LastTargetName := TargetName;
	LastPercentDone := PercentDone;
	LastPath := SourceName;
	Result := FAbortOnProgress;
end;

function TMockProgress.Progress(SourceName: WideString; PercentDone: Integer): Boolean;
begin
	Result := Progress(SourceName, '', PercentDone);
end;

function TMockProgress.Progress(PercentDone: Integer): Boolean;
begin
	Result := Progress('', '', PercentDone);
end;

function TMockProgress.Aborted: Boolean;
begin
	Result := FAbortOnProgress;
end;

end.
