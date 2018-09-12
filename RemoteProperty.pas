unit RemoteProperty;

interface

uses
	Plugin_types, Descriptions, CMLTypes, Settings, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CloudMailRu, MRC_Helper, Vcl.Grids, Vcl.ValEdit, Vcl.Menus, Vcl.ComCtrls, Vcl.ToolWin, System.ImageList, Vcl.ImgList, Clipbrd, HashInfo;

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
		SaveLinksTb: TToolButton;
		DownloadLinksIL: TImageList;
		WrapLinksTb: TToolButton;
		SaveDialogSD: TSaveDialog;
		LinksLogLabel: TLabel;
		CancelLinksScanTb: TToolButton;
		RefreshLinksScanTb: TToolButton;
		DescriptionTS: TTabSheet;
		DescriptionEditMemo: TMemo;
		DescriptionSaveButton: TButton;
		HashesListTS: TTabSheet;
		HashesListTB: TToolBar;
		SaveHashesTb: TToolButton;
		WrapHashesTb: TToolButton;
		HashesLogLabel: TLabel;
		CancelHashesScanTb: TToolButton;
		RefreshHashesScanTb: TToolButton;
		HashesMemo: TMemo;
		LoadHashesTb: TToolButton;
		OpenDialogOD: TOpenDialog;
		ApplyHashesTB: TToolButton;
		procedure AccessCBClick(Sender: TObject);
		class function ShowProperty(parentWindow: HWND; RemoteName: WideString; RemoteProperty: TCloudMailRuDirListingItem; Cloud: TCloudMailRu; DoUrlEncode: Boolean = true; AutoUpdateDownloadListing: Boolean = true; ShowDescription: Boolean = true; EditDescription: Boolean = true; PluginIonFileName: WideString = 'descript.ion'): Integer;
		procedure FormActivate(Sender: TObject);
		procedure InviteBtnClick(Sender: TObject);
		procedure ItemDeleteClick(Sender: TObject);
		procedure ItemRefreshClick(Sender: TObject);
		procedure InvitesPopupPopup(Sender: TObject);
		procedure ItemChangeAccessClick(Sender: TObject);
		procedure WrapLinksTbClick(Sender: TObject);
		procedure SaveLinksTbClick(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure CancelLinksScanTbClick(Sender: TObject);
		procedure RefreshLinksScanTbClick(Sender: TObject);
		procedure DescriptionSaveButtonClick(Sender: TObject);
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure PublicLinkLabelClick(Sender: TObject);
		procedure RefreshHashesScanTbClick(Sender: TObject);
		procedure CancelHashesScanTbClick(Sender: TObject);
		procedure WrapHashesTbClick(Sender: TObject);
		procedure SaveHashesTbClick(Sender: TObject);
		procedure LoadHashesTbClick(Sender: TObject);
		procedure ApplyHashesTBClick(Sender: TObject);
		procedure HashesMemoExit(Sender: TObject);
	private
		{Private declarations}
		procedure WMAfterShow(var Message: TMessage); message WM_AFTER_SHOW;
		procedure RefreshInvites();
		procedure RefreshPublicShare(const Publish: Boolean);
		function FillRecursiveDownloadListing(const Path: WideString; Cloud: TCloudMailRu = nil): Boolean; //break recursion if false - cancelled
		function FillRecursiveHashListing(const Path: WideString; Cloud: TCloudMailRu = nil; BaseDir: WideString = ''): Boolean; //break recursion if false - cancelled
		procedure UpdateDownloadListing();
		procedure UpdateHashesListing();
		procedure RefreshItemDescription();
		procedure SaveItemDescription();
		procedure TempPublicCloudInit(publicUrl: WideString);
		function LinksLogProc(LogText: WideString): Boolean;
		function HashesLogProc(LogText: WideString): Boolean;
		function GenerateHashCommand(ListingItem: TCloudMailRuDirListingItem; BaseDir: WideString = ''; Path: WideString = ''): WideString;
		procedure ApplyHashCommandList(CommandList: TStrings);

	protected
		Props: TCloudMailRuDirListingItem;
		InvitesListing: TCloudMailRuInviteInfoListing;
		Cloud: TCloudMailRu;
		RemoteName: WideString;
		DoUrlEncode: Boolean;
		LogCancelledFlag: Boolean;
		AutoUpdateDownloadListing: Boolean;
		ShowDescription: Boolean;
		EditDescription: Boolean;
		PluginIonFileName: WideString; //Переопределённое (или нет) имя файла описаний, с которым будет работать плагин

		TempPublicCloud: TCloudMailRu; //Облако для получения прямых ссылок на опубликованные объекты
	public
		{Public declarations}

	end;

var
	PropertyForm: TPropertyForm;

implementation

{$R *.dfm}
{TPropertyForm}

procedure TPropertyForm.RefreshItemDescription;
var
	CurrentDescriptions: TDescription;
	LocalPath: WideString;
begin
	DescriptionEditMemo.lines.Clear;
	LocalPath := GetTmpFileName('ion');
	if not self.Cloud.getDescriptionFile(IncludeTrailingBackslash(ExtractFileDir(self.RemoteName)) + self.PluginIonFileName, LocalPath) then
		exit;
	CurrentDescriptions := TDescription.Create(LocalPath, GetTCCommentPreferredFormat);
	CurrentDescriptions.Read;
	DescriptionEditMemo.lines.Text := CurrentDescriptions.GetValue(ExtractFileName(self.RemoteName), FORMAT_CLEAR);
	CurrentDescriptions.Destroy;
	DeleteFileW(PWideChar(LocalPath));
end;

procedure TPropertyForm.SaveHashesTbClick(Sender: TObject);
begin
	if (SaveDialogSD.Execute(self.Handle)) then
	begin
		HashesMemo.lines.SaveToFile(SaveDialogSD.FileName);
	end;
end;

procedure TPropertyForm.SaveItemDescription;
var
	CurrentDescriptions: TDescription;
	RemotePath, LocalPath: WideString;
	RemoteFileExists: Boolean;
begin
	RemotePath := IncludeTrailingBackslash(ExtractFileDir(self.RemoteName)) + self.PluginIonFileName;
	LocalPath := GetTmpFileName('ion');

	RemoteFileExists := self.Cloud.getDescriptionFile(RemotePath, LocalPath);
	CurrentDescriptions := TDescription.Create(LocalPath, GetTCCommentPreferredFormat);
	if RemoteFileExists then //если был прежний файл - его надо перечитать и удалить с сервера
	begin
		CurrentDescriptions.Read;
		self.Cloud.deleteFile(RemotePath); //Приходится удалять, потому что не знаем, как переписать
	end;
	CurrentDescriptions.SetValue(ExtractFileName(self.RemoteName), DescriptionEditMemo.lines.Text);
	CurrentDescriptions.Write();
	self.Cloud.putDesriptionFile(RemotePath, CurrentDescriptions.ionFilename);

	CurrentDescriptions.Destroy;
end;

procedure TPropertyForm.UpdateDownloadListing;
begin
	DownloadLinksMemo.lines.Clear;
	if self.Cloud.isPublicShare then
	begin
		if Props.type_ = TYPE_DIR then
		begin (*рекурсивно получаем все ссылки в каталоге*)
			FillRecursiveDownloadListing(IncludeTrailingPathDelimiter(self.RemoteName))
		end else begin
			DownloadLinksMemo.lines.Text := self.Cloud.getSharedFileUrl(self.RemoteName, self.DoUrlEncode);
		end;
	end else begin
		(*У объекта есть публичная ссылка, можно получить прямые ссылки на скачивание*)
		TempPublicCloudInit(WebLink.Text);
		if Props.type_ = TYPE_DIR then
		begin (*рекурсивно получаем все ссылки в каталоге*)
			FillRecursiveDownloadListing(EmptyWideStr, self.TempPublicCloud);
		end else begin
			DownloadLinksMemo.lines.Text := TempPublicCloud.getSharedFileUrl(EmptyWideStr, self.DoUrlEncode);
		end;
		TempPublicCloud.Free;
	end;
	LinksLogProc('Done');
end;

procedure TPropertyForm.UpdateHashesListing;
begin
	HashesMemo.lines.Clear;

	if Props.type_ = TYPE_DIR then
	begin (*рекурсивно получаем все ссылки в каталоге*)
		FillRecursiveHashListing(IncludeTrailingPathDelimiter(self.RemoteName))
	end else begin
		HashesMemo.lines.Add(GenerateHashCommand(Props));
	end;

	LinksLogProc('Done');
end;

function TPropertyForm.FillRecursiveDownloadListing(const Path: WideString; Cloud: TCloudMailRu = nil): Boolean;
var
	CurrentDirListing: TCloudMailRuDirListing;
	CurrentDirItemsCounter: Integer;
begin
	CancelLinksScanTb.Enabled := true;
	RefreshLinksScanTb.Enabled := false;
	result := true;
	if not(Assigned(Cloud)) then
		Cloud := self.Cloud;

	if not LinksLogProc('Scanning ' + IncludeTrailingPathDelimiter(Path)) then
		exit(false);
	Cloud.getDirListing(Path, CurrentDirListing);
	ProcessMessages;
	for CurrentDirItemsCounter := 0 to Length(CurrentDirListing) - 1 do
	begin
		if CurrentDirListing[CurrentDirItemsCounter].type_ = TYPE_DIR then
		begin
			result := FillRecursiveDownloadListing(IncludeTrailingPathDelimiter(Path) + CurrentDirListing[CurrentDirItemsCounter].name, Cloud);
			if not result then
				break;

		end else begin
			DownloadLinksMemo.lines.Add(Cloud.getSharedFileUrl(IncludeTrailingPathDelimiter(Path) + CurrentDirListing[CurrentDirItemsCounter].name, self.DoUrlEncode));
		end;
	end;
	RefreshLinksScanTb.Enabled := true;
	CancelLinksScanTb.Enabled := false;
end;

function TPropertyForm.FillRecursiveHashListing(const Path: WideString; Cloud: TCloudMailRu = nil; BaseDir: WideString = ''): Boolean;
var
	CurrentDirListing: TCloudMailRuDirListing;
	CurrentDirItemsCounter: Integer;
	CurrentItem: TCloudMailRuDirListingItem;
begin
	CancelHashesScanTb.Enabled := true;
	RefreshHashesScanTb.Enabled := false;
	result := true;

	if EmptyWideStr = BaseDir then
		BaseDir := Path;

	if not(Assigned(Cloud)) then
		Cloud := self.Cloud;
	if not HashesLogProc('Scanning ' + IncludeTrailingPathDelimiter(Path)) then
		exit(false);
	Cloud.getDirListing(Path, CurrentDirListing);
	ProcessMessages;
	for CurrentDirItemsCounter := 0 to Length(CurrentDirListing) - 1 do
	begin
		CurrentItem := CurrentDirListing[CurrentDirItemsCounter];
		if CurrentItem.type_ = TYPE_DIR then
		begin
			result := FillRecursiveHashListing(IncludeTrailingPathDelimiter(Path) + CurrentItem.name, Cloud, BaseDir);
			if not result then
				break;

		end else begin
			HashesMemo.lines.Add(GenerateHashCommand(CurrentItem, BaseDir, Path));
		end;
	end;
	RefreshHashesScanTb.Enabled := true;
	CancelHashesScanTb.Enabled := false;
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
			WebLink.Text := EmptyWideStr;
			Props.WebLink := EmptyWideStr;
			WebLink.Enabled := false;
			DownloadLinksTS.TabVisible := false;
			if ExtPropertiesPC.TabIndex = -1 then
				ExtPropertiesPC.Visible := false;

		end else begin
			MessageBoxW(self.Handle, PWideChar('Error while unpublishing file ' + Props.home + ', see main log'), 'File unpublishing error', MB_OK + MB_ICONERROR);
		end;
	end;
end;

procedure TPropertyForm.RefreshLinksScanTbClick(Sender: TObject);
begin
	UpdateDownloadListing;
end;

procedure TPropertyForm.RefreshHashesScanTbClick(Sender: TObject);
begin
	ApplyHashesTB.Enabled := false;
	UpdateHashesListing();
	if not self.Cloud.isPublicShare then
		ApplyHashesTB.Enabled := true;
end;

procedure TPropertyForm.RefreshInvites;
var
	i, InvitesCount: Integer;
begin
	while InvitesLE.Strings.Count > 0 do
		InvitesLE.DeleteRow(1);
	if Cloud.getShareInfo(Props.home, self.InvitesListing) then
	begin
		InvitesCount := Length(self.InvitesListing) - 1;
		for i := 0 to InvitesCount do
			InvitesLE.InsertRow(self.InvitesListing[i].email, TCloudMailRu.CloudAccessToString(self.InvitesListing[i].access), true);
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
	self.TempPublicCloud := TCloudMailRu.Create(TempAccountSettings, 0, self.Cloud.ProxySettings, self.Cloud.ConnectTimeoutValue, self.Cloud.UploadLimit, self.Cloud.DownloadLimit);
	self.TempPublicCloud.login;
end;

procedure TPropertyForm.ApplyHashesTBClick(Sender: TObject);
begin
	ApplyHashesTB.Enabled := false;
	ApplyHashCommandList(HashesMemo.lines);
	ApplyHashesTB.Enabled := true;
end;

function TPropertyForm.LinksLogProc(LogText: WideString): Boolean;
begin
	result := not LogCancelledFlag;
	if (result) then
		LinksLogLabel.Caption := LogText
	else
		LinksLogLabel.Caption := EmptyWideStr;
	LogCancelledFlag := false;
end;

procedure TPropertyForm.LoadHashesTbClick(Sender: TObject);
begin
	if (OpenDialogOD.Execute(self.Handle)) then
	begin
		HashesMemo.lines.LoadFromFile(OpenDialogOD.FileName);
	end;
end;

procedure TPropertyForm.PublicLinkLabelClick(Sender: TObject);
begin
	if AccessCB.Checked then
		Clipboard.AsText := WebLink.Text;
end;

procedure TPropertyForm.ApplyHashCommandList(CommandList: TStrings);
var
	ItemIndex: Integer;
	CurrentCommand: THashInfo;
begin
	for ItemIndex := 0 to CommandList.Count do
	begin
		ProcessMessages;
		CurrentCommand := THashInfo.Create(CommandList.Strings[ItemIndex]);
		if CurrentCommand.valid then
		begin
			if Props.kind = TYPE_DIR then //клонируем в каталог
				Cloud.cloneHash(IncludeTrailingPathDelimiter(self.RemoteName), CurrentCommand.hash, CurrentCommand.size, CurrentCommand.name)
			else //клонируем рядом
				Cloud.cloneHash(ExtractFilePath(self.RemoteName), CurrentCommand.hash, CurrentCommand.size, CurrentCommand.name)
		end else begin
			HashesLogProc('Line ' + ItemIndex.ToString + ': ' + CurrentCommand.errorString);
		end;
	end;
	PostMessage(FindTCWindow, WM_USER + 51, 540, 0); //TC does not update current panel, so we should do it this way
end;

procedure TPropertyForm.CancelHashesScanTbClick(Sender: TObject);
begin
	LogCancelledFlag := true;
end;

procedure TPropertyForm.CancelLinksScanTbClick(Sender: TObject);
begin
	LogCancelledFlag := true;
end;

procedure TPropertyForm.DescriptionSaveButtonClick(Sender: TObject);
begin
	SaveItemDescription;
end;

procedure TPropertyForm.FormActivate(Sender: TObject);
begin
	CenterWindow(self.parentWindow, self.Handle);
end;

procedure TPropertyForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	case Key of
		VK_ESCAPE:
			Close;
		VK_F2:
			begin
				if (self.ShowDescription) and (self.EditDescription) then
				begin
					SaveItemDescription;
					self.ModalResult := IDCONTINUE;
					self.CloseModal;
				end;
			end;
	end;

end;

procedure TPropertyForm.FormShow(Sender: TObject);
begin
	PostMessage(self.Handle, WM_AFTER_SHOW, 0, 0);
end;

function TPropertyForm.GenerateHashCommand(ListingItem: TCloudMailRuDirListingItem; BaseDir: WideString = ''; Path: WideString = ''): WideString;
var
	AppliedName: WideString;
begin
	(*Если задан базовый каталог, то имена отсчитываем от него*)
	if EmptyWideStr = BaseDir then
	begin
		AppliedName := ListingItem.name;
	end else begin
		if (EmptyWideStr <> Path) then
		begin
			if (Pos(BaseDir, Path) = 1) and (BaseDir <> Path) then
				AppliedName := IncludeTrailingPathDelimiter(StringReplace(Path, BaseDir, '', [])) + ListingItem.name
			else
				AppliedName := ListingItem.name;
		end;
	end;
	result := 'hash "' + ListingItem.hash + ':' + ListingItem.size.ToString + ':' + AppliedName + '"';
end;

function TPropertyForm.HashesLogProc(LogText: WideString): Boolean;
begin
	result := not LogCancelledFlag; //todo separate flags
	if (result) then
		HashesLogLabel.Caption := LogText
	else
		HashesLogLabel.Caption := EmptyWideStr;
	LogCancelledFlag := false;
end;

procedure TPropertyForm.HashesMemoExit(Sender: TObject);
begin
	if not self.Cloud.isPublicShare then
		ApplyHashesTB.Enabled := EmptyWideStr <> HashesMemo.Text;
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
	if email = EmptyWideStr then
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

procedure TPropertyForm.SaveLinksTbClick(Sender: TObject);
begin
	if (SaveDialogSD.Execute(self.Handle)) then
	begin
		DownloadLinksMemo.lines.SaveToFile(SaveDialogSD.FileName);
	end;
end;

class function TPropertyForm.ShowProperty(parentWindow: HWND; RemoteName: WideString; RemoteProperty: TCloudMailRuDirListingItem; Cloud: TCloudMailRu; DoUrlEncode: Boolean = true; AutoUpdateDownloadListing: Boolean = true; ShowDescription: Boolean = true; EditDescription: Boolean = true; PluginIonFileName: WideString = 'descript.ion'): Integer;
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
		PropertyForm.ShowDescription := ShowDescription;
		PropertyForm.EditDescription := EditDescription;
		PropertyForm.PluginIonFileName := PluginIonFileName;
		if ('descript.ion' <> PluginIonFileName) then
			PropertyForm.DescriptionTS.Caption := 'Description from ' + PluginIonFileName;

		result := PropertyForm.Showmodal;

	finally
		FreeAndNil(PropertyForm);
	end;
end;

procedure TPropertyForm.WMAfterShow(var Message: TMessage);
begin
	if not(Props.WebLink = EmptyWideStr) then
	begin
		WebLink.Text := PUBLIC_ACCESS_URL + Props.WebLink;
		WebLink.SetFocus;
		WebLink.SelectAll;
	end;

	ExtPropertiesPC.Visible := false;
	FolderAccessTS.TabVisible := false;
	DownloadLinksTS.TabVisible := false;
	DescriptionTS.TabVisible := false;
	HashesListTS.TabVisible := false;

	if self.Cloud.isPublicShare then
	begin
		AccessCB.Enabled := false;
		AccessCB.Checked := true;
		ExtPropertiesPC.Visible := true;
		DownloadLinksTS.TabVisible := true;
		LoadHashesTb.Enabled := false;
		HashesMemo.ReadOnly := true;
		ApplyHashesTB.Enabled := false;
		LoadHashesTb.Enabled := false;

		if self.AutoUpdateDownloadListing then
			UpdateDownloadListing;
	end else begin
		AccessCB.Checked := not(Props.WebLink = EmptyWideStr);
		WebLink.Enabled := AccessCB.Checked;
		if (Props.type_ = TYPE_DIR) or (Props.kind = KIND_SHARED) then
		begin
			ExtPropertiesPC.Visible := true;
			FolderAccessTS.TabVisible := true;
			RefreshInvites;
		end;

	end;
	if self.ShowDescription then
	begin
		ExtPropertiesPC.Visible := true;
		DescriptionTS.TabVisible := true;
		RefreshItemDescription;
		DescriptionEditMemo.ReadOnly := not self.EditDescription;
		DescriptionSaveButton.Enabled := self.EditDescription;

	end;
	HashesListTS.TabVisible := true;

end;

procedure TPropertyForm.WrapHashesTbClick(Sender: TObject);
begin
	if WrapHashesTb.Down then
		HashesMemo.ScrollBars := ssVertical
	else
		HashesMemo.ScrollBars := ssBoth;

	HashesMemo.WordWrap := WrapHashesTb.Down;
end;

procedure TPropertyForm.WrapLinksTbClick(Sender: TObject);
begin
	if WrapLinksTb.Down then
		DownloadLinksMemo.ScrollBars := ssVertical
	else
		DownloadLinksMemo.ScrollBars := ssBoth;

	DownloadLinksMemo.WordWrap := WrapLinksTb.Down;
end;

procedure TPropertyForm.AccessCBClick(Sender: TObject);
begin
	if self.Cloud.isPublicShare then
		exit;
	AccessCB.Enabled := false; //блокируем во избежание повторных кликов
	WebLink.Text := 'Wait for it...';
	RefreshPublicShare(AccessCB.Checked);
	if AccessCB.Checked and self.AutoUpdateDownloadListing then
		UpdateDownloadListing;

	AccessCB.Enabled := true;
end;

end.
