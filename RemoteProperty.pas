unit RemoteProperty;

interface

uses
	Plugin_types, Settings, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CloudMailRu, MRC_Helper, Vcl.Grids, Vcl.ValEdit, Vcl.Menus, Vcl.ComCtrls, Vcl.ToolWin, System.ImageList, Vcl.ImgList;

const
	WM_AFTER_SHOW = WM_USER + 300; //custom message

type
	TPropertyForm = class(TForm)
		PublicLinkLabel: TLabel;
		WebLink: TEdit;
		AccessCB: TCheckBox;
		OkButton: TButton;
		InvitesPopup: TPopupMenu;
		ItemChangeAccess: TMenuItem;
		ItemDelete: TMenuItem;
		ItemRefresh: TMenuItem;
		N1: TMenuItem;
		ExtPropertiesPC: TPageControl;
		FolderAccessTS: TTabSheet;
		DownloadLinksTS: TTabSheet;
		InviteEmailLabel: TLabel;
		AccessLabel: TLabel;
		InviteEmailEdit: TEdit;
		InviteAcessCB: TComboBox;
		InviteBtn: TButton;
		InvitesLE: TValueListEditor;
		DownloadLinksMemo: TMemo;
		DownloadLinksTB: TToolBar;
		SaveBtn: TToolButton;
		DownloadLinksIL: TImageList;
		WrapBTN: TToolButton;
		DownloadLinksSD: TSaveDialog;
		LogLabel: TLabel;
		CancelScanTB: TToolButton;
		RefreshScanTB: TToolButton;
		procedure AccessCBClick(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		class function ShowProperty(parentWindow: HWND; RemoteName: WideString; RemoteProperty: TCloudMailRuDirListingItem; Cloud: TCloudMailRu; DoUrlEncode: Boolean = true; AutoUpdateDownloadListing: Boolean = true): Integer;
		procedure FormActivate(Sender: TObject);
		procedure InviteBtnClick(Sender: TObject);
		procedure ItemDeleteClick(Sender: TObject);
		procedure ItemRefreshClick(Sender: TObject);
		procedure InvitesPopupPopup(Sender: TObject);
		procedure ItemChangeAccessClick(Sender: TObject);
		procedure WrapBTNClick(Sender: TObject);
		procedure SaveBtnClick(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure CancelScanTBClick(Sender: TObject);
		procedure RefreshScanTBClick(Sender: TObject);
	private
		{Private declarations}
		procedure WMHotKey(var Message: TMessage); message WM_HOTKEY;
		procedure WMAfterShow(var Message: TMessage); message WM_AFTER_SHOW;
		procedure RefreshInvites();
		procedure RefreshPublicShare(const Publish: Boolean);
		function FillRecursiveDownloadListing(const Path: WideString; Cloud: TCloudMailRu = nil): Boolean; //break recursion if false - cancelled
		procedure UpdateDownloadListing();
		procedure TempPublicCloudInit(publicUrl: WideString);
		function LogProc(LogText: WideString): Boolean;

	protected
		Props: TCloudMailRuDirListingItem;
		InvitesListing: TCloudMailRuInviteInfoListing;
		Cloud: TCloudMailRu;
		RemoteName: WideString;
		DoUrlEncode: Boolean;
		LogCancelledFlag: Boolean;
		AutoUpdateDownloadListing: Boolean;

		TempPublicCloud: TCloudMailRu; //Облако для получения прямых ссылок на опубликованные объекты
	public
		{Public declarations}

	end;

var
	PropertyForm: TPropertyForm;

implementation

{$R *.dfm}
{TPropertyForm}

(*Class methods*)

procedure TPropertyForm.UpdateDownloadListing;
begin
	DownloadLinksMemo.Lines.Clear;
	if self.Cloud.isPublicShare then
	begin
		if Props.type_ = TYPE_DIR then
		begin (*рекурсивно получаем все ссылки в каталоге*)
			FillRecursiveDownloadListing(IncludeTrailingPathDelimiter(self.RemoteName))
		end else begin
			DownloadLinksMemo.Lines.Text := self.Cloud.getSharedFileUrl(self.RemoteName, self.DoUrlEncode);
		end;
	end else begin
		(*У объекта есть публичная ссылка, можно получить прямые ссылки на скачивание*)
		TempPublicCloudInit(WebLink.Text);
		if Props.type_ = TYPE_DIR then
		begin (*рекурсивно получаем все ссылки в каталоге*)
			FillRecursiveDownloadListing('', self.TempPublicCloud);
		end else begin
			DownloadLinksMemo.Lines.Text := TempPublicCloud.getSharedFileUrl('', self.DoUrlEncode);
		end;
		TempPublicCloud.Free;
	end;
	LogProc('Done');
end;

function TPropertyForm.FillRecursiveDownloadListing(const Path: WideString; Cloud: TCloudMailRu = nil): Boolean;
var
	CurrentDirListing: TCloudMailRuDirListing;
	CurrentDirItemsCounter: Integer;
begin
	CancelScanTB.Enabled := true;
	RefreshScanTB.Enabled := false;
	result := true;
	if not(Assigned(Cloud)) then Cloud := self.Cloud;

	if not LogProc('Scanning ' + IncludeTrailingPathDelimiter(Path)) then exit(false);
	Cloud.getDirListing(Path, CurrentDirListing);
	ProcessMessages;
	for CurrentDirItemsCounter := 0 to length(CurrentDirListing) - 1 do
	begin
		if CurrentDirListing[CurrentDirItemsCounter].type_ = TYPE_DIR then
		begin
			result := FillRecursiveDownloadListing(IncludeTrailingPathDelimiter(Path) + CurrentDirListing[CurrentDirItemsCounter].name, Cloud);
			if not result then break;

		end else begin
			DownloadLinksMemo.Lines.Add(Cloud.getSharedFileUrl(IncludeTrailingPathDelimiter(Path) + CurrentDirListing[CurrentDirItemsCounter].name, self.DoUrlEncode));
		end;
	end;
	RefreshScanTB.Enabled := true;
	CancelScanTB.Enabled := false;
end;

procedure TPropertyForm.RefreshPublicShare(const Publish: Boolean);
var
	PublicLink: WideString;
begin
	if Publish then
	begin
		if self.Cloud.publishFile(Props.home, PublicLink) then
		begin
			WebLink.Text := PUBLIC_ACCESS_URL + PublicLink;
			Props.WebLink := PublicLink;
			WebLink.Enabled := true;
			WebLink.SetFocus;
			WebLink.SelectAll;
			ExtPropertiesPC.Visible := true;
			DownloadLinksTS.TabVisible := true;
			//UpdateDownloadListing;
		end else begin
			MessageBoxW(self.Handle, PWideChar('Error while publishing file ' + Props.home + ', see main log'), 'File publishing error', MB_OK + MB_ICONERROR);
		end;
	end else begin
		if Cloud.publishFile(Props.home, Props.WebLink, CLOUD_UNPUBLISH) then
		begin
			WebLink.Text := '';
			Props.WebLink := '';
			WebLink.Enabled := false;
			DownloadLinksTS.TabVisible := false;
			if ExtPropertiesPC.TabIndex = -1 then ExtPropertiesPC.Visible := false;

		end else begin
			MessageBoxW(self.Handle, PWideChar('Error while unpublishing file ' + Props.home + ', see main log'), 'File unpublishing error', MB_OK + MB_ICONERROR);
		end;
	end;
end;

procedure TPropertyForm.RefreshScanTBClick(Sender: TObject);
begin
	UpdateDownloadListing;
end;

procedure TPropertyForm.RefreshInvites;
var
	i, InvitesCount: Integer;
begin
	while InvitesLE.Strings.Count > 0 do InvitesLE.DeleteRow(1);
	if Cloud.getShareInfo(Props.home, self.InvitesListing) then
	begin
		InvitesCount := length(self.InvitesListing) - 1;
		for i := 0 to InvitesCount do InvitesLE.InsertRow(self.InvitesListing[i].email, TCloudMailRu.CloudAccessToString(self.InvitesListing[i].access), true);
	end else begin
		MessageBoxW(self.Handle, PWideChar('Error while retrieving ' + Props.home + ' folder invites list, see main log'), 'Folder invite listing error', MB_OK + MB_ICONERROR);
	end;
end;

procedure TPropertyForm.TempPublicCloudInit(publicUrl: WideString);
var
	TempAccountSettings: TAccountSettings;
begin
	TempAccountSettings.public_account := true;
	TempAccountSettings.public_url := publicUrl;
	self.TempPublicCloud := TCloudMailRu.Create(TempAccountSettings, 0, self.Cloud.ProxySettings, self.Cloud.ConnectTimeoutValue);
	self.TempPublicCloud.login;
end;

function TPropertyForm.LogProc(LogText: WideString): Boolean;
begin
	result := not LogCancelledFlag;
	if (result) then LogLabel.Caption := LogText
	else LogLabel.Caption := '';
	LogCancelledFlag := false;
end;

(*Controls handlers*)

procedure TPropertyForm.CancelScanTBClick(Sender: TObject);
begin
	LogCancelledFlag := true;
end;

procedure TPropertyForm.FormActivate(Sender: TObject);
begin
	CenterWindow(self.parentWindow, self.Handle);
end;

procedure TPropertyForm.FormDestroy(Sender: TObject);
begin
	UnregisterHotKey((Sender as TPropertyForm).Handle, 1)
end;

procedure TPropertyForm.FormShow(Sender: TObject);
begin
	PostMessage(self.Handle, WM_AFTER_SHOW, 0, 0);
end;

procedure TPropertyForm.InviteBtnClick(Sender: TObject);
begin
	if (Cloud.shareFolder(Props.home, InviteEmailEdit.Text, InviteAcessCB.ItemIndex)) then
	begin
		RefreshInvites;
	end else begin
		MessageBoxW(self.Handle, PWideChar('Error while inviting ' + InviteEmailEdit.Text + ' to ' + Props.home + ' folder, see main log'), 'Folder invite error', MB_OK + MB_ICONERROR);
	end;
end;

procedure TPropertyForm.InvitesPopupPopup(Sender: TObject);
var
	email, access: WideString;
begin
	email := InvitesLE.Keys[InvitesLE.Row];
	if email = '' then
	begin
		ItemChangeAccess.Visible := false;
		ItemDelete.Visible := false;
		exit;
	end else begin
		ItemChangeAccess.Visible := true;
		ItemDelete.Visible := true;
	end;

	access := InvitesLE.Values[email];
	access := TCloudMailRu.CloudAccessToString(access, true);

	ItemChangeAccess.Caption := 'Change access to ' + access;
end;

procedure TPropertyForm.ItemChangeAccessClick(Sender: TObject);
var
	email, access: WideString;
begin
	email := InvitesLE.Keys[InvitesLE.Row];
	access := InvitesLE.Values[email];
	if Cloud.shareFolder(Props.home, InvitesLE.Keys[InvitesLE.Row], TCloudMailRu.StringToCloudAccess(access, true)) then
	begin
		RefreshInvites;
	end else begin
		MessageBoxW(self.Handle, PWideChar('Error while removing access to ' + InviteEmailEdit.Text + ' from ' + Props.home + ' folder, see main log'), 'Folder unshare error', MB_OK + MB_ICONERROR);
	end;
end;

procedure TPropertyForm.ItemDeleteClick(Sender: TObject);
begin
	if Cloud.shareFolder(Props.home, InvitesLE.Keys[InvitesLE.Row], CLOUD_SHARE_NO) then
	begin
		RefreshInvites;
	end else begin
		MessageBoxW(self.Handle, PWideChar('Error while removing access to ' + InviteEmailEdit.Text + ' from ' + Props.home + ' folder, see main log'), 'Folder unshare error', MB_OK + MB_ICONERROR);
	end;

end;

procedure TPropertyForm.ItemRefreshClick(Sender: TObject);
begin
	RefreshInvites;
end;

procedure TPropertyForm.SaveBtnClick(Sender: TObject);
begin
	if (DownloadLinksSD.Execute(self.Handle)) then
	begin
		DownloadLinksMemo.Lines.SaveToFile(DownloadLinksSD.FileName);
	end;
end;

class function TPropertyForm.ShowProperty(parentWindow: HWND; RemoteName: WideString; RemoteProperty: TCloudMailRuDirListingItem; Cloud: TCloudMailRu; DoUrlEncode: Boolean = true; AutoUpdateDownloadListing: Boolean = true): Integer;
var
	PropertyForm: TPropertyForm;
begin
	try
		PropertyForm := TPropertyForm.Create(nil);
		PropertyForm.parentWindow := parentWindow;

		PropertyForm.RemoteName := RemoteName;
		PropertyForm.Caption := RemoteProperty.name;
		PropertyForm.Cloud := Cloud;
		PropertyForm.Props := RemoteProperty;
		PropertyForm.AutoUpdateDownloadListing := AutoUpdateDownloadListing;
		PropertyForm.LogCancelledFlag := false;
		PropertyForm.DoUrlEncode := DoUrlEncode;
		RegisterHotKey(PropertyForm.Handle, 1, 0, VK_ESCAPE);
		result := PropertyForm.Showmodal;

	finally
		FreeAndNil(PropertyForm);
	end;
end;

procedure TPropertyForm.WMAfterShow(var Message: TMessage);
begin
	if not(Props.WebLink = '') then
	begin
		WebLink.Text := PUBLIC_ACCESS_URL + Props.WebLink;
		WebLink.SetFocus;
		WebLink.SelectAll;
	end;
	ExtPropertiesPC.Visible := false;
	FolderAccessTS.TabVisible := false;
	DownloadLinksTS.TabVisible := false;
	if self.Cloud.isPublicShare then
	begin
		AccessCB.Enabled := false;
		AccessCB.checked := true;
		ExtPropertiesPC.Visible := true;
		DownloadLinksTS.TabVisible := true;
		if self.AutoUpdateDownloadListing then UpdateDownloadListing;
	end else begin
		AccessCB.checked := not(Props.WebLink = '');
		WebLink.Enabled := AccessCB.checked;
		if (Props.type_ = TYPE_DIR) or (Props.kind = KIND_SHARED) then
		begin
			ExtPropertiesPC.Visible := true;
			FolderAccessTS.TabVisible := true;
			RefreshInvites;
		end;
	end;
end;

procedure TPropertyForm.WMHotKey(var Message: TMessage);
begin
	if (Message.LParamHi = VK_ESCAPE) and (GetForegroundWindow = self.Handle) then Close;
end;

procedure TPropertyForm.WrapBTNClick(Sender: TObject);
begin
	if WrapBTN.Down then DownloadLinksMemo.ScrollBars := ssVertical
	else DownloadLinksMemo.ScrollBars := ssBoth;

	DownloadLinksMemo.WordWrap := WrapBTN.Down;
end;

procedure TPropertyForm.AccessCBClick(Sender: TObject);
begin
	if self.Cloud.isPublicShare then exit;
	AccessCB.Enabled := false; //блокируем во избежание повторных кликов
	WebLink.Text := 'Wait for it...';
	RefreshPublicShare(AccessCB.checked);
	if AccessCB.checked and self.AutoUpdateDownloadListing then UpdateDownloadListing;

	AccessCB.Enabled := true;
end;

end.
