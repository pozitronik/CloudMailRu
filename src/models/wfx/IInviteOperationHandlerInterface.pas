unit IInviteOperationHandlerInterface;

{Interface for handling invite operations (mount, unmount, reject).
 Orchestrates dialog display and cloud operations based on user choice.}

interface

uses
	Windows,
	CMRIncomingInvite,
	CloudMailRu;

type
	{Callback type for showing invite properties dialog.
	 Returns: mrAbort=Unmount(keep), mrClose=Unmount(no keep), mrYes=Mount, mrNo=Reject}
	TShowInvitePropertiesFunc = reference to function(ParentWindow: HWND;
		const Invite: TCMRIncomingInvite): Integer;

	IInviteOperationHandler = interface
		['{D0E1F2A3-5B6C-7D8E-9F0A-1B2C3D4E5F6A}']

		{Executes invite operation based on dialog result.
		 @param ParentWindow Parent window handle for dialog
		 @param Cloud Cloud connection for the account
		 @param Invite The incoming invite to process
		 @param ShowDialog Callback to show the properties dialog
		 @return FS_EXEC_OK on success, FS_EXEC_ERROR on failure}
		function Execute(ParentWindow: HWND; Cloud: TCloudMailRu;
			const Invite: TCMRIncomingInvite;
			ShowDialog: TShowInvitePropertiesFunc): Integer;
	end;

implementation

end.
