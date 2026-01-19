unit MoveOperationContextTracker;

{Tracks move operation context between FsMkDir and FsRemoveDir.
 Eliminates global state coupling by encapsulating move target tracking.}

interface

uses
	RealPath,
	IThreadStateManagerInterface,
	IMoveOperationContextTrackerInterface;

type
	TMoveOperationContextTracker = class(TInterfacedObject, IMoveOperationContextTracker)
	private
		FThreadState: IThreadStateManager;
		FMoveTarget: TRealPath;
		FHasMoveTarget: Boolean;
	public
		constructor Create(ThreadState: IThreadStateManager);

		procedure TrackMoveTarget(const TargetPath: TRealPath);
		function GetMoveTarget: TRealPath;
		function IsMoveOperation: Boolean;
		procedure ClearMoveTarget;
	end;

implementation

uses
	PLUGIN_TYPES;

constructor TMoveOperationContextTracker.Create(ThreadState: IThreadStateManager);
begin
	inherited Create;
	FThreadState := ThreadState;
	FHasMoveTarget := False;
end;

procedure TMoveOperationContextTracker.TrackMoveTarget(const TargetPath: TRealPath);
begin
	FMoveTarget := TargetPath;
	FHasMoveTarget := True;
end;

function TMoveOperationContextTracker.GetMoveTarget: TRealPath;
begin
	if FHasMoveTarget then
		Result := FMoveTarget
	else
		Result := Default(TRealPath);
end;

function TMoveOperationContextTracker.IsMoveOperation: Boolean;
begin
	Result := FThreadState.GetFsStatusInfo = FS_STATUS_OP_RENMOV_MULTI;
end;

procedure TMoveOperationContextTracker.ClearMoveTarget;
begin
	FHasMoveTarget := False;
	FMoveTarget := Default(TRealPath);
end;

end.
