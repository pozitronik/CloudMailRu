unit IDirectoryDeletionPreCheckInterface;

{Interface for pre-deletion validation in FsRemoveDir.

 Encapsulates the skip logic that determines whether directory deletion
 should proceed or be skipped. Handles:
 - Path skip list check (issue #168)
 - Listing abort flag check and reset

 The abort flag reset is a side effect that must occur when checking
 the abort condition, so it's encapsulated within the check itself.}

interface

type
	IDirectoryDeletionPreCheck = interface
		['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

		{Checks whether directory deletion should proceed.
		 Also resets the abort flag if it was set (side effect).
		 @param RemoteName The remote path to check
		 @returns True if deletion should proceed, False if it should be skipped}
		function ShouldProceed(const RemoteName: WideString): Boolean;
	end;

implementation

end.
