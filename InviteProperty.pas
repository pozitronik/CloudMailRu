unit InviteProperty;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CloudMailRu, MRC_Helper;

type
	TInvitePropertyForm = class(TForm)
		InviteNameLB: TLabel;
		InviteOwnerEmailLB: TLabel;
		InviteOwnerNameLB: TLabel;
		InviteAccessLB: TLabel;
		NameLB: TLabel;
		OwnerEmailLB: TLabel;
		OwnerNameLB: TLabel;
		AccessLB: TLabel;
		SizeLB: TLabel;
		InviteSizeLB: TLabel;
		MountBTN: TButton;
		CancelBTN: TButton;
		RejectBTN: TButton;
		TokenLB: TLabel;
		InviteTokenLB: TLabel;
		UnmountCopyBTN: TButton;
		UnmountDeleteBTN: TButton;
	private
		{Private declarations}
	public
		{Public declarations}
		class function ShowProperties(parentWindow: HWND; Item: TCloudMailRuIncomingInviteInfo; AccountName: WideString = ''): integer;
	end;

implementation

{$R *.dfm}
{TInvitePropertyForm}

class function TInvitePropertyForm.ShowProperties(parentWindow: HWND; Item: TCloudMailRuIncomingInviteInfo; AccountName: WideString): integer;
var
	InvitePropertyForm: TInvitePropertyForm;
begin
	try
		InvitePropertyForm := TInvitePropertyForm.Create(nil);
		InvitePropertyForm.parentWindow := parentWindow;
		InvitePropertyForm.InviteNameLB.Caption := Item.name;
		InvitePropertyForm.InviteOwnerEmailLB.Caption := Item.owner.email;
		InvitePropertyForm.InviteOwnerNameLB.Caption := Item.owner.name;
		InvitePropertyForm.InviteAccessLB.Caption := TCloudMailRu.CloudAccessToString(Item.access);
		InvitePropertyForm.InviteSizeLB.Caption := FormatSize(Item.size, TYPE_BYTES);
		InvitePropertyForm.InviteTokenLB.Caption := Item.invite_token;
		InvitePropertyForm.Caption := AccountName + ' invite: ' + Item.name;
		if Item.home <> '' then //already mounted item
		begin
			InvitePropertyForm.TokenLB.Caption := 'Mounted as:';
			InvitePropertyForm.InviteTokenLB.Caption := Item.home;
			InvitePropertyForm.RejectBTN.Enabled := false;
		end else begin
			InvitePropertyForm.UnmountCopyBTN.Enabled := false;
			InvitePropertyForm.UnmountDeleteBTN.Enabled := false;
		end;

		result := InvitePropertyForm.ShowModal;
	finally
		FreeAndNil(InvitePropertyForm);
	end;

end;

end.
