unit RemoteProperty;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CloudMailRu, MRC_Helper, Vcl.Grids, Vcl.ValEdit, Vcl.Menus;

type
	TPropertyForm = class(TForm)
		PublicLinkLabel: TLabel;
		WebLink: TEdit;
		AccessCB: TCheckBox;
		OkButton: TButton;
		InvitesGB: TGroupBox;
		InvitesLE: TValueListEditor;
		InviteEmailEdit: TEdit;
		InviteEmailLabel: TLabel;
		InviteAcessCB: TComboBox;
		AccessLabel: TLabel;
		InviteBtn: TButton;
		InvitesPopup: TPopupMenu;
		ItemChangeAccess: TMenuItem;
		ItemDelete: TMenuItem;
		ItemRefresh: TMenuItem;
    N1: TMenuItem;

		procedure AccessCBClick(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		class function ShowProperty(parentWindow: HWND; RemoteProperty: TCloudMailRuDirListingItem; var Cloud: TCloudMailRu): integer;
		procedure FormActivate(Sender: TObject);
		procedure InviteBtnClick(Sender: TObject);
		procedure ItemDeleteClick(Sender: TObject);
		procedure ItemRefreshClick(Sender: TObject);
		procedure Clean1Click(Sender: TObject);
	private
		{Private declarations}
		procedure WMHotKey(var Message: TMessage); message WM_HOTKEY;
		procedure RefreshInvites();
	protected
		Props: TCloudMailRuDirListingItem;
		InvitesListing: TCloudMailRuInviteInfoListing;
		Cloud: TCloudMailRu;
	public
		{Public declarations}

	end;

var
	PropertyForm: TPropertyForm;

implementation

{$R *.dfm}
{TPropertyForm}

procedure TPropertyForm.AccessCBClick(Sender: TObject);
var
	PublicLink: WideString;
begin
	WebLink.Text := 'Wait for it...';
	AccessCB.Enabled := false; //блокируем во избежание повторных кликов
	if AccessCB.checked then
	begin
		if Self.Cloud.publishFile(Props.home, PublicLink) then
		begin
			WebLink.Text := 'https://cloud.mail.ru/public/' + PublicLink;
			Props.WebLink := PublicLink;
			WebLink.Enabled := true;
			WebLink.SetFocus;
			WebLink.SelectAll;
		end else begin
			MessageBoxW(Self.Handle, PWideChar('Error while publishing file ' + Props.home + ', see main log'), 'File publishing error', MB_OK + MB_ICONERROR);
		end;

	end else begin
		if Cloud.publishFile(Props.home, Props.WebLink, CLOUD_UNPUBLISH) then
		begin
			WebLink.Text := '';
			Props.WebLink := '';
			WebLink.Enabled := false;
		end else begin
			MessageBoxW(Self.Handle, PWideChar('Error while unpublishing file ' + Props.home + ', see main log'), 'File unpublishing error', MB_OK + MB_ICONERROR);
		end;
	end;
	AccessCB.Enabled := true;
end;

procedure TPropertyForm.Clean1Click(Sender: TObject);
begin
	while InvitesLE.Strings.Count > 0 do InvitesLE.DeleteRow(1);
end;

procedure TPropertyForm.FormActivate(Sender: TObject);
begin
	CenterWindow(Self.parentWindow, Self.Handle);
end;

procedure TPropertyForm.FormDestroy(Sender: TObject);
begin
	UnregisterHotKey((Sender as TPropertyForm).Handle, 1)
end;

procedure TPropertyForm.FormShow(Sender: TObject);
begin
	if not(Props.WebLink = '') then
	begin
		WebLink.Text := 'https://cloud.mail.ru/public/' + Props.WebLink;
		WebLink.SetFocus;
		WebLink.SelectAll;
	end;
	AccessCB.checked := not(Props.WebLink = '');
	WebLink.Enabled := AccessCB.checked;
	if Props.type_ = TYPE_DIR then
	begin
		InvitesGB.Height :=210;
		InvitesGB.Enabled:=true;
		RefreshInvites;
	end else begin
		InvitesGB.Height :=0;
		InvitesGB.Enabled:=false;
	end;

end;

procedure TPropertyForm.InviteBtnClick(Sender: TObject);
begin
	if (Cloud.shareFolder(Props.home, InviteEmailEdit.Text, InviteAcessCB.ItemIndex)) then
	begin
		RefreshInvites;
	end else begin
		MessageBoxW(Self.Handle, PWideChar('Error while inviting ' + InviteEmailEdit.Text + ' to ' + Props.home + ' folder, see main log'), 'Folder invite error', MB_OK + MB_ICONERROR);
	end;
end;

procedure TPropertyForm.ItemDeleteClick(Sender: TObject);
begin
	if Cloud.shareFolder(Props.home, InvitesLE.Keys[InvitesLE.Row], CLOUD_SHARE_NO) then
	begin
		RefreshInvites;
	end else begin
		MessageBoxW(Self.Handle, PWideChar('Error while removing access to ' + InviteEmailEdit.Text + ' from ' + Props.home + ' folder, see main log'), 'Folder unshare error', MB_OK + MB_ICONERROR);
	end;

end;

procedure TPropertyForm.ItemRefreshClick(Sender: TObject);
begin
	RefreshInvites;
end;

procedure TPropertyForm.RefreshInvites;
var
	i, InvitesCount: integer;
begin
	while InvitesLE.Strings.Count > 0 do InvitesLE.DeleteRow(1);

	if Cloud.getShareInfo(Props.home, Self.InvitesListing) then
	begin
		InvitesCount:=Length(Self.InvitesListing) - 1;
		for i := 0 to InvitesCount do
		begin
			InvitesLE.InsertRow(Self.InvitesListing[i].name, Self.InvitesListing[i].access, true);
		end;

	end else begin
		MessageBoxW(Self.Handle, PWideChar('Error while retrieving ' + Props.home + ' folder invites list, see main log'), 'Folder invite listing error', MB_OK + MB_ICONERROR);
	end;
end;

class function TPropertyForm.ShowProperty(parentWindow: HWND; RemoteProperty: TCloudMailRuDirListingItem; var Cloud: TCloudMailRu): integer; //todo do we need cloud as var parameter?
var
	PropertyForm: TPropertyForm;
begin
	try
		PropertyForm := TPropertyForm.Create(nil);
		PropertyForm.parentWindow := parentWindow;

		PropertyForm.Caption := RemoteProperty.name;
		PropertyForm.Cloud := Cloud;
		PropertyForm.Props := RemoteProperty;
		RegisterHotKey(PropertyForm.Handle, 1, 0, VK_ESCAPE);
		result := PropertyForm.Showmodal;

	finally
		FreeAndNil(PropertyForm);
	end;
end;

procedure TPropertyForm.WMHotKey(var Message: TMessage);
begin
	if (Message.LParamHi = VK_ESCAPE) and (GetForegroundWindow = Self.Handle) then Close;
end;

end.
