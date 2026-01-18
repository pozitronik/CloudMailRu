unit IProgressInterface;

{Interface for progress reporting, abstracting the concrete implementation from consumers.
 This enables dependency injection and testability by allowing mock implementations.}

interface

type
	{Progress reporting interface for file operations.
	 Returns True if operation should be cancelled, False to continue.}
	IProgress = interface
		['{B2C3D4E5-F6A7-8901-BCDE-F23456789012}']
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

end.
