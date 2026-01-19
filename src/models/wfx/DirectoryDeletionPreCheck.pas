unit DirectoryDeletionPreCheck;

{Pre-deletion validation for FsRemoveDir.

 Centralizes skip logic that was previously inline in FsRemoveDir:
 - Checks if path is in the skip list (issue #168 - partial delete handling)
 - Checks and resets the listing abort flag (user cancellation)}

interface

uses
	IDirectoryDeletionPreCheckInterface,
	IThreadStateManagerInterface;

type
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
