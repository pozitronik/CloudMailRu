unit InviteOperationHandler;

{Handles invite operations (mount, unmount, reject).
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

	TInviteOperationHandler = class(TInterfacedObject, IInviteOperationHandler)
	public
		function Execute(ParentWindow: HWND; Cloud: TCloudMailRu;
			const Invite: TCMRIncomingInvite;
			ShowDialog: TShowInvitePropertiesFunc): Integer;
	end;

implementation

uses
	Controls,
	PLUGIN_TYPES;

function TInviteOperationHandler.Execute(ParentWindow: HWND; Cloud: TCloudMailRu;
	const Invite: TCMRIncomingInvite;
	ShowDialog: TShowInvitePropertiesFunc): Integer;
var
	DialogResult: Integer;
begin
	Result := FS_EXEC_OK;

	if not Assigned(Cloud) then
		Exit(FS_EXEC_ERROR);

	DialogResult := ShowDialog(ParentWindow, Invite);

	case DialogResult of
		mrAbort: {Unmount folder, keep data}
			Cloud.unmountFolder(Invite.name, True);
		mrClose: {Unmount folder, don't keep data}
			Cloud.unmountFolder(Invite.name, False);
		mrYes: {Mount folder}
			Cloud.mountFolder(Invite.name, Invite.invite_token);
		mrNo: {Reject invite}
			Cloud.rejectInvite(Invite.invite_token);
	end;
end;

end.
