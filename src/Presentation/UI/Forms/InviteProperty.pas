unit InviteProperty;

interface

uses
	CloudIncomingInvite,
	InvitePropertyPresenter,
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.StdCtrls;

type
	TInvitePropertyForm = class(TForm, IInvitePropertyView)
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
		FPresenter: TInvitePropertyPresenter;

		{IInvitePropertyView implementation}
		procedure SetCaption(Caption: WideString);
		procedure SetItemName(Name: WideString);
		procedure SetOwnerEmail(Email: WideString);
		procedure SetOwnerName(Name: WideString);
		procedure SetAccess(Access: WideString);
		procedure SetSize(Size: WideString);
		procedure SetTokenLabel(LabelText: WideString);
		procedure SetTokenValue(Value: WideString);
		procedure SetMountEnabled(Enabled: Boolean);
		procedure SetRejectEnabled(Enabled: Boolean);
		procedure SetUnmountCopyEnabled(Enabled: Boolean);
		procedure SetUnmountDeleteEnabled(Enabled: Boolean);
	public
		destructor Destroy; override;
		class function ShowProperties(parentWindow: HWND; Item: TCloudIncomingInvite; AccountName: WideString = ''): integer;
	end;

implementation

{$R *.dfm}

{TInvitePropertyForm - IInvitePropertyView implementation}

procedure TInvitePropertyForm.SetCaption(Caption: WideString);
begin
	self.Caption := Caption;
end;

procedure TInvitePropertyForm.SetItemName(Name: WideString);
begin
	InviteNameLB.Caption := Name;
end;

procedure TInvitePropertyForm.SetOwnerEmail(Email: WideString);
begin
	InviteOwnerEmailLB.Caption := Email;
end;

procedure TInvitePropertyForm.SetOwnerName(Name: WideString);
begin
	InviteOwnerNameLB.Caption := Name;
end;

procedure TInvitePropertyForm.SetAccess(Access: WideString);
begin
	InviteAccessLB.Caption := Access;
end;

procedure TInvitePropertyForm.SetSize(Size: WideString);
begin
	InviteSizeLB.Caption := Size;
end;

procedure TInvitePropertyForm.SetTokenLabel(LabelText: WideString);
begin
	TokenLB.Caption := LabelText;
end;

procedure TInvitePropertyForm.SetTokenValue(Value: WideString);
begin
	InviteTokenLB.Caption := Value;
end;

procedure TInvitePropertyForm.SetMountEnabled(Enabled: Boolean);
begin
	MountBTN.Enabled := Enabled;
end;

procedure TInvitePropertyForm.SetRejectEnabled(Enabled: Boolean);
begin
	RejectBTN.Enabled := Enabled;
end;

procedure TInvitePropertyForm.SetUnmountCopyEnabled(Enabled: Boolean);
begin
	UnmountCopyBTN.Enabled := Enabled;
end;

procedure TInvitePropertyForm.SetUnmountDeleteEnabled(Enabled: Boolean);
begin
	UnmountDeleteBTN.Enabled := Enabled;
end;

{TInvitePropertyForm}

destructor TInvitePropertyForm.Destroy;
begin
	FreeAndNil(FPresenter);
	inherited;
end;

class function TInvitePropertyForm.ShowProperties(parentWindow: HWND; Item: TCloudIncomingInvite; AccountName: WideString): integer;
var
	InvitePropertyForm: TInvitePropertyForm;
begin
	InvitePropertyForm := TInvitePropertyForm.Create(nil);
	try
		InvitePropertyForm.parentWindow := parentWindow;
		InvitePropertyForm.FPresenter := TInvitePropertyPresenter.Create(InvitePropertyForm);
		InvitePropertyForm.FPresenter.Initialize(Item, AccountName);
		result := InvitePropertyForm.ShowModal;
	finally
		FreeAndNil(InvitePropertyForm);
	end;
end;

end.
