unit IMoveOperationContextTrackerInterface;

{Interface for tracking move operation context between FsMkDir and FsRemoveDir.

 During multi-file move operations, TC:
 1. Creates destination directory (FsMkDir)
 2. Copies files
 3. Deletes source directory (FsRemoveDir)

 This tracker stores the move target path so FsRemoveDir can determine
 whether to trigger OnFileRenamed (move) vs OnFileDeleted (delete) for
 description sync purposes.}

interface

uses
	RealPath;

type
	IMoveOperationContextTracker = interface
		['{D4E5F6A7-B8C9-0123-DEF4-567890ABCDEF}']

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

implementation

end.
