unit InviteOperationHandler;

{Handles invite operations: mount, unmount (keep/no-keep), reject.}

interface

uses
	Windows,
	CMRIncomingInvite,
	CloudMailRu,
	IInviteOperationHandlerInterface;

type
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
