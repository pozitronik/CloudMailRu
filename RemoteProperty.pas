unit RemoteProperty;

interface

uses
	Plugin_types, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CloudMailRu, MRC_Helper, Vcl.Grids, Vcl.ValEdit, Vcl.Menus, Vcl.ComCtrls, Vcl.ToolWin, System.ImageList, Vcl.ImgList;

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
		ToolButton1: TToolButton;
		WrapBTN: TToolButton;
		DownloadLinksSD: TSaveDialog;
		procedure AccessCBClick(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		class function ShowProperty(parentWindow: HWND; RemoteName: WideString; RemoteProperty: TCloudMailRuDirListingItem; var Cloud: TCloudMailRu; LogProc: TLogProcW = nil; ProgressProc: TProgressProcW = nil; PluginNum: Integer = 0): Integer;
		procedure FormActivate(Sender: TObject);
		procedure InviteBtnClick(Sender: TObject);
		procedure ItemDeleteClick(Sender: TObject);
		procedure ItemRefreshClick(Sender: TObject);
		procedure InvitesPopupPopup(Sender: TObject);
		procedure ItemChangeAccessClick(Sender: TObject);
		procedure WrapBTNClick(Sender: TObject);
		procedure SaveBtnClick(Sender: TObject);
	private
		{Private declarations}
		procedure WMHotKey(var Message: TMessage); message WM_HOTKEY;
		procedure RefreshInvites();
		procedure FillRecursiveDownloadListing(const Path: WideString);
	protected
		Props: TCloudMailRuDirListingItem;
		InvitesListing: TCloudMailRuInviteInfoListing;
		Cloud: TCloudMailRu;
		RemoteName: WideString;
		LogProc: TLogProcW;
		ProgressProc: TProgressProcW;
		PluginNum: Integer;
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
	if self.Cloud.isPublicShare then exit;

	WebLink.Text := 'Wait for it...';
	AccessCB.Enabled := false; //блокируем во избежание повторных кликов
	if AccessCB.checked then
	begin
		if self.Cloud.publishFile(Props.home, PublicLink) then
		begin
			WebLink.Text := 'https://cloud.mail.ru/public/' + PublicLink;
			Props.WebLink := PublicLink;
			WebLink.Enabled := true;
			WebLink.SetFocus;
			WebLink.SelectAll;
		end else begin
			MessageBoxW(self.Handle, PWideChar('Error while publishing file ' + Props.home + ', see main log'), 'File publishing error', MB_OK + MB_ICONERROR);
		end;

	end else begin
		if Cloud.publishFile(Props.home, Props.WebLink, CLOUD_UNPUBLISH) then
		begin
			WebLink.Text := '';
			Props.WebLink := '';
			WebLink.Enabled := false;
		end else begin
			MessageBoxW(self.Handle, PWideChar('Error while unpublishing file ' + Props.home + ', see main log'), 'File unpublishing error', MB_OK + MB_ICONERROR);
		end;
	end;
	AccessCB.Enabled := true;
end;

procedure TPropertyForm.FillRecursiveDownloadListing(const Path: WideString);
var
	CurrentDirListing: TCloudMailRuDirListing;
	CurrentDirItemsCounter: Integer;
begin
	self.LogProc(self.PluginNum, msgtype_details, PWideChar('Scanning ' + Path));
	self.ProgressProc(self.PluginNum, 'Scanning...', PWideChar(Path), 0);
	self.Cloud.getDirListing(Path, CurrentDirListing);
	ProcessMessages;
	for CurrentDirItemsCounter := 0 to length(CurrentDirListing) - 1 do
	begin
		if CurrentDirListing[CurrentDirItemsCounter].type_ = TYPE_DIR then
		begin
			self.FillRecursiveDownloadListing(IncludeTrailingPathDelimiter(Path) + CurrentDirListing[CurrentDirItemsCounter].name);
		end else begin
			DownloadLinksMemo.Lines.Add(self.Cloud.getSharedFileUrl(IncludeTrailingPathDelimiter(Path) + CurrentDirListing[CurrentDirItemsCounter].name));
		end;
		self.ProgressProc(self.PluginNum, 'Scanning...', PWideChar(Path), 100);
	end;
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
	if not(Props.WebLink = '') then
	begin
		WebLink.Text := 'https://cloud.mail.ru/public/' + Props.WebLink;
		WebLink.SetFocus;
		WebLink.SelectAll;
	end;
	DownloadLinksMemo.Lines.Clear;
	ExtPropertiesPC.Visible := false;
	FolderAccessTS.TabVisible := false;
	DownloadLinksTS.TabVisible := false;
	if self.Cloud.isPublicShare then
	begin
		AccessCB.Enabled := false;
		AccessCB.checked := true;
		ExtPropertiesPC.Visible := true;
		DownloadLinksTS.TabVisible := true;
		if Props.type_ = TYPE_DIR then
		begin (*рекурсивно получаем все ссылки в каталоге*)
			FillRecursiveDownloadListing(IncludeTrailingPathDelimiter(self.RemoteName))
		end else begin
			DownloadLinksMemo.Lines.Text := self.Cloud.getSharedFileUrl(self.RemoteName);
		end;
	end else begin
		AccessCB.checked := not(Props.WebLink = '');
		WebLink.Enabled := AccessCB.checked;
		if Props.type_ = TYPE_DIR then
		begin
			ExtPropertiesPC.Visible := true;
			FolderAccessTS.TabVisible := true;
			RefreshInvites;
		end;
	end;
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

procedure TPropertyForm.RefreshInvites;
var
	i, InvitesCount: Integer;
begin
	while InvitesLE.Strings.Count > 0 do InvitesLE.DeleteRow(1);

	if Cloud.getShareInfo(Props.home, self.InvitesListing) then
	begin
		InvitesCount := length(self.InvitesListing) - 1;
		for i := 0 to InvitesCount do
		begin
			InvitesLE.InsertRow(self.InvitesListing[i].name, TCloudMailRu.CloudAccessToString(self.InvitesListing[i].access), true);
		end;

	end else begin
		MessageBoxW(self.Handle, PWideChar('Error while retrieving ' + Props.home + ' folder invites list, see main log'), 'Folder invite listing error', MB_OK + MB_ICONERROR);
	end;
end;

procedure TPropertyForm.SaveBtnClick(Sender: TObject);
begin
	if (DownloadLinksSD.Execute(self.Handle)) then
	begin
		DownloadLinksMemo.Lines.SaveToFile(DownloadLinksSD.FileName);
	end;
end;

class function TPropertyForm.ShowProperty(parentWindow: HWND; RemoteName: WideString; RemoteProperty: TCloudMailRuDirListingItem; var Cloud: TCloudMailRu; LogProc: TLogProcW = nil; ProgressProc: TProgressProcW = nil; PluginNum: Integer = 0): Integer; //todo do we need cloud as var parameter?
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
		PropertyForm.LogProc := LogProc;
		PropertyForm.ProgressProc := ProgressProc;
		PropertyForm.PluginNum := PluginNum;
		RegisterHotKey(PropertyForm.Handle, 1, 0, VK_ESCAPE);
		result := PropertyForm.Showmodal;

	finally
		FreeAndNil(PropertyForm);
	end;
end;

procedure TPropertyForm.WMHotKey(var Message: TMessage);
begin
	if (Message.LParamHi = VK_ESCAPE) and (GetForegroundWindow = self.Handle) then Close;
end;

procedure TPropertyForm.WrapBTNClick(Sender: TObject);
begin
	if WrapBTN.Down then DownloadLinksMemo.ScrollBars := ssBoth
	else DownloadLinksMemo.ScrollBars := ssVertical;

	DownloadLinksMemo.WordWrap := WrapBTN.Down;
end;

end.
