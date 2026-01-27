unit MoveOperationContextTracker;

{Tracks move operation context between FsMkDir and FsRemoveDir.
	Eliminates global state coupling by encapsulating move target tracking.

	During multi-file move operations, TC:
	1. Creates destination directory (FsMkDir)
	2. Copies files
	3. Deletes source directory (FsRemoveDir)

	This tracker stores the move target path so FsRemoveDir can determine
	whether to trigger OnFileRenamed (move) vs OnFileDeleted (delete) for
	description sync purposes.}

interface

uses
	RealPath,
	ThreadStateManager;

type
	IMoveOperationContextTracker = interface
		['{892B2405-910D-477A-97D9-F807A521E964}']

		{Called by FsMkDir when directory is created during RENMOV_MULTI operation.
			Stores the target path for later use by FsRemoveDir.
			@param TargetPath The newly created directory path (move destination)}
		procedure TrackMoveTarget(const TargetPath: TRealPath);

		{Called by FsRemoveDir to get the move target path.
			@returns The tracked move target, or empty TRealPath if not in move context}
		function GetMoveTarget: TRealPath;

		{Checks if current operation context is a move (RENMOV_MULTI).
			@returns True if current thread is in RENMOV_MULTI context}
		function IsMoveOperation: Boolean;

		{Clears the tracked move target. Called after move operation completes.}
		procedure ClearMoveTarget;
	end;

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
	WFXTypes;

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
		Result := Default (TRealPath);
end;

function TMoveOperationContextTracker.IsMoveOperation: Boolean;
begin
	Result := FThreadState.GetFsStatusInfo = FS_STATUS_OP_RENMOV_MULTI;
end;

procedure TMoveOperationContextTracker.ClearMoveTarget;
begin
	FHasMoveTarget := False;
	FMoveTarget := Default (TRealPath);
end;

end.
