unit SameAccountMoveHandler;

{Handles move/copy operations within a single account.
 Manages overwrite behavior, skip-path blacklist for directory removal (issue #168),
 and notifies description sync on successful renames.
 Encapsulates overwrite handling, skip-path management, and description sync.}

interface

uses
	RealPath,
	CloudMailRu,
	ThreadStateManager,
	DescriptionSyncGuard;

type
	ISameAccountMoveHandler = interface
		['{A9D4E7C2-3F8B-4A6E-9C1D-5E2F8B7A3C4D}']

		{Executes move or copy operation within the same account.
		 @param Cloud The cloud connection for the account
		 @param OldPath Source path
		 @param NewPath Destination path
		 @param Move True for move, False for copy
		 @param OverWrite True to overwrite existing files
		 @return FS_FILE_* result code}
		function Execute(Cloud: TCloudMailRu; const OldPath, NewPath: TRealPath;
			Move, OverWrite: Boolean): Integer;
	end;

	TSameAccountMoveHandler = class(TInterfacedObject, ISameAccountMoveHandler)
	private
		FThreadState: IThreadStateManager;
		FDescriptionSyncGuard: IDescriptionSyncGuard;

		{Updates skip-path blacklist based on move result.
		 Adds path on FS_FILE_EXISTS (TC will try to delete source dir).
		 Removes path on FS_FILE_OK (successful overwrite).}
		procedure UpdateSkipPath(MoveResult: Integer; const OldPath: TRealPath);
	public
		constructor Create(ThreadState: IThreadStateManager; DescriptionSyncGuard: IDescriptionSyncGuard);

		function Execute(Cloud: TCloudMailRu; const OldPath, NewPath: TRealPath;
			Move, OverWrite: Boolean): Integer;
	end;

implementation

uses
	PLUGIN_TYPES;

constructor TSameAccountMoveHandler.Create(ThreadState: IThreadStateManager;
	DescriptionSyncGuard: IDescriptionSyncGuard);
begin
	inherited Create;
	FThreadState := ThreadState;
	FDescriptionSyncGuard := DescriptionSyncGuard;
end;

procedure TSameAccountMoveHandler.UpdateSkipPath(MoveResult: Integer; const OldPath: TRealPath);
begin
	if not FThreadState.HasRemoveDirSkippedPath then
		Exit;

	{TC will try to delete source directory after move if target exists.
	 Add to blacklist to prevent deletion (issue #168).}
	if MoveResult = FS_FILE_EXISTS then
		FThreadState.AddSkippedPath(OldPath.ToPath)
	{Remove from blacklist if move succeeded (user chose to overwrite)}
	else if MoveResult = FS_FILE_OK then
		FThreadState.RemoveSkippedPath(OldPath.ToPath);
end;

function TSameAccountMoveHandler.Execute(Cloud: TCloudMailRu; const OldPath, NewPath: TRealPath;
	Move, OverWrite: Boolean): Integer;
begin
	{Handle overwrite by deleting existing file first}
	if OverWrite and not Cloud.deleteFile(NewPath.Path) then
		Exit(FS_FILE_NOTSUPPORTED);

	if Move then
	begin
		Result := Cloud.FileMove(OldPath.Path, NewPath.Path);
		UpdateSkipPath(Result, OldPath);

		if Result = FS_FILE_OK then
			FDescriptionSyncGuard.OnFileRenamed(OldPath, NewPath, Cloud);
	end
	else
		Result := Cloud.FileCopy(OldPath.Path, NewPath.Path);
end;

end.
