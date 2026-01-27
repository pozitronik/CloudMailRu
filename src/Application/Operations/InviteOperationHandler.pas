unit InviteOperationHandler;

{Handles invite operations (mount, unmount, reject).
	Orchestrates dialog display and cloud operations based on user choice.}

interface

uses
	Windows,
	CMRIncomingInvite,
	CloudShareService;

type
	{Callback type for showing invite properties dialog.
		Returns: mrAbort=Unmount(keep), mrClose=Unmount(no keep), mrYes=Mount, mrNo=Reject}
	TShowInvitePropertiesFunc = reference to function(ParentWindow: HWND; const Invite: TCMRIncomingInvite): Integer;

	IInviteOperationHandler = interface
		['{0AD6EAD4-816C-42D4-BA71-3915C43E4C32}']

		{Executes invite operation based on dialog result.
			@param ParentWindow Parent window handle for dialog
			@param ShareService Cloud share service for invite operations
			@param Invite The incoming invite to process
			@param ShowDialog Callback to show the properties dialog
			@return FS_EXEC_OK on success, FS_EXEC_ERROR on failure}
		function Execute(ParentWindow: HWND; ShareService: ICloudShareService; const Invite: TCMRIncomingInvite; ShowDialog: TShowInvitePropertiesFunc): Integer;
	end;

	TInviteOperationHandler = class(TInterfacedObject, IInviteOperationHandler)
	public
		function Execute(ParentWindow: HWND; ShareService: ICloudShareService; const Invite: TCMRIncomingInvite; ShowDialog: TShowInvitePropertiesFunc): Integer;
	end;

implementation

uses
	Controls,
	WFXTypes;

function TInviteOperationHandler.Execute(ParentWindow: HWND; ShareService: ICloudShareService; const Invite: TCMRIncomingInvite; ShowDialog: TShowInvitePropertiesFunc): Integer;
var
	DialogResult: Integer;
begin
	Result := FS_EXEC_OK;

	DialogResult := ShowDialog(ParentWindow, Invite);

	case DialogResult of
		mrAbort: {Unmount folder, keep data}
			ShareService.Unmount(Invite.name, True);
		mrClose: {Unmount folder, don't keep data}
			ShareService.Unmount(Invite.name, False);
		mrYes: {Mount folder}
			ShareService.Mount(Invite.name, Invite.invite_token);
		mrNo: {Reject invite}
			ShareService.RejectInvite(Invite.invite_token);
	end;
end;

end.
