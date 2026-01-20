unit DirectoryDeletionPreCheck;

{Pre-deletion validation for FsRemoveDir.

	Centralizes skip logic that was previously inline in FsRemoveDir:
	- Checks if path is in the skip list (issue #168 - partial delete handling)
	- Checks and resets the listing abort flag (user cancellation)

	Encapsulates the skip logic that determines whether directory deletion
	should proceed or be skipped. Handles:
	- Path skip list check (issue #168)
	- Listing abort flag check and reset

	The abort flag reset is a side effect that must occur when checking
	the abort condition, so it's encapsulated within the check itself.}

interface

uses
	ThreadStateManager;

type
	IDirectoryDeletionPreCheck = interface
		['{FB16D337-1FB8-41EE-B1A2-B9223A998FFB}']

		{Checks whether directory deletion should proceed.
			Also resets the abort flag if it was set (side effect).
			@param RemoteName The remote path to check
			@returns True if deletion should proceed, False if it should be skipped}
		function ShouldProceed(const RemoteName: WideString): Boolean;
	end;

	TDirectoryDeletionPreCheck = class(TInterfacedObject, IDirectoryDeletionPreCheck)
	private
		FThreadState: IThreadStateManager;
	public
		constructor Create(ThreadState: IThreadStateManager);

		function ShouldProceed(const RemoteName: WideString): Boolean;
	end;

implementation

constructor TDirectoryDeletionPreCheck.Create(ThreadState: IThreadStateManager);
begin
	inherited Create;
	FThreadState := ThreadState;
end;

function TDirectoryDeletionPreCheck.ShouldProceed(const RemoteName: WideString): Boolean;
begin
	{Файлы по удаляемому пути есть в блек-листе}
	{Files at the path being deleted are in the blacklist (issue #168)}
	if FThreadState.IsPathSkipped(RemoteName) then
		Exit(False);

	{Check if listing was aborted by user - reset flag as side effect}
	if FThreadState.GetListingAborted then
	begin
		FThreadState.SetListingAborted(False);
		Exit(False);
	end;

	Result := True;
end;

end.
