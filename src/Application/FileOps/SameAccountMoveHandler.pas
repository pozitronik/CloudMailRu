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
	DescriptionSyncGuard,
	TimestampSyncGuard;

type
	ISameAccountMoveHandler = interface
		['{59E596BC-E9A8-456D-B38E-73D395C18BAC}']

		{Executes move or copy operation within the same account.
			@param Cloud The cloud connection for the account
			@param OldPath Source path
			@param NewPath Destination path
			@param Move True for move, False for copy
			@param OverWrite True to overwrite existing files
			@return FS_FILE_* result code}
		function Execute(Cloud: TCloudMailRu; const OldPath, NewPath: TRealPath; Move, OverWrite: Boolean): Integer;
	end;

	TSameAccountMoveHandler = class(TInterfacedObject, ISameAccountMoveHandler)
	private
		FThreadState: IThreadStateManager;
		FDescriptionSyncGuard: IDescriptionSyncGuard;
		FTimestampSyncGuard: ITimestampSyncGuard;

		{Updates skip-path blacklist based on move result.
			Adds path on FS_FILE_EXISTS (TC will try to delete source dir).
			Removes path on FS_FILE_OK (successful overwrite).}
		procedure UpdateSkipPath(MoveResult: Integer; const OldPath: TRealPath);
	public
		constructor Create(ThreadState: IThreadStateManager; DescriptionSyncGuard: IDescriptionSyncGuard; TimestampSyncGuard: ITimestampSyncGuard);

		function Execute(Cloud: TCloudMailRu; const OldPath, NewPath: TRealPath; Move, OverWrite: Boolean): Integer;
	end;

implementation

uses
	WFXTypes,
	PathHelper,
	CloudDirItem;

constructor TSameAccountMoveHandler.Create(ThreadState: IThreadStateManager; DescriptionSyncGuard: IDescriptionSyncGuard; TimestampSyncGuard: ITimestampSyncGuard);
begin
	inherited Create;
	FThreadState := ThreadState;
	FDescriptionSyncGuard := DescriptionSyncGuard;
	FTimestampSyncGuard := TimestampSyncGuard;
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

function TSameAccountMoveHandler.Execute(Cloud: TCloudMailRu; const OldPath, NewPath: TRealPath; Move, OverWrite: Boolean): Integer;
var
	DummyItem: TCloudDirItem;
begin
	{Handle overwrite by deleting existing file first}
	if OverWrite and not Cloud.FileOperations.Delete(NewPath.Path) then
		Exit(FS_FILE_NOTSUPPORTED);

	if Move then
	begin
		Result := Cloud.FileOperations.Move(OldPath.Path, NewPath.Path);
		UpdateSkipPath(Result, OldPath);

		if Result = FS_FILE_OK then
		begin
			FDescriptionSyncGuard.OnFileRenamed(OldPath, NewPath, Cloud);
			FTimestampSyncGuard.OnFileRenamed(OldPath, NewPath, Cloud);
		end;
	end
	else
	begin
		{Issue #219: For copy with rename, check if final target exists before copying.
			Without this check, copy creates intermediate file which is left orphaned
			if subsequent rename fails due to existing target.
			Only check when: rename needed AND overwrite not allowed.}
		if not OverWrite and (ExtractUniversalFileName(OldPath.Path) <> ExtractUniversalFileName(NewPath.Path)) then
			if Cloud.ListingService.StatusFile(NewPath.Path, DummyItem) then
				Exit(FS_FILE_EXISTS);

		Result := Cloud.FileOperations.Copy(OldPath.Path, NewPath.Path);
	end;
end;

end.
