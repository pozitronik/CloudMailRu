unit Accounts;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Settings, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IniFiles, MRC_Helper, PLUGIN_Types, Vcl.ComCtrls, Vcl.Mask, Vcl.ExtCtrls, Vcl.Samples.Spin, System.IOUtils, AskPassword, TCPasswordManagerHelper, Registration;

type
	TAccountsForm = class(TForm)
		OptionPages: TPageControl;
		AccountsTab: TTabSheet;
		GlobalTab: TTabSheet;
		AccountsGroupBox: TGroupBox;
		AccountsList: TListBox;
		ApplyButton: TButton;
		DeleteButton: TButton;
		PreserveFileTimeCB: TCheckBox;
		UseDLLFromPluginDir: TCheckBox;
		GlobalSettingApplyBTN: TButton;
		AccountsPanel: TPanel;
		AccountNameEdit: TEdit;
		AccountNameLabel: TLabel;
		TwostepAuthCB: TCheckBox;
		SplitLargeFilesCB: TCheckBox;
		UnlimitedFileSizeCB: TCheckBox;
		UseTCPwdMngrCB: TCheckBox;
		PasswordEdit: TEdit;
		PasswordLabel: TLabel;
		EmailEdit: TEdit;
		UsernameLabel: TLabel;
		PublicAccountCB: TCheckBox;
		SharesPanel: TPanel;
		PublicUrlEdit: TEdit;
		PublicUrlLabel: TLabel;
		NetworkTab: TTabSheet;
		ProxyGB: TGroupBox;
		ProxyTypeLabel: TLabel;
		ProxyDivLabel: TLabel;
		ProxyPortLabel: TLabel;
		ProxyUserLabel: TLabel;
		ProxyPWDLabel: TLabel;
		ProxyServerLabel: TLabel;
		ProxyCB: TComboBox;
		ProxyServerEdit: TEdit;
		ProxyPortEdit: TEdit;
		ProxyUserEdit: TEdit;
		ProxyPwd: TMaskEdit;
		ProxyTCPwdMngrCB: TCheckBox;
		SocketTimeoutLabel: TLabel;
		GlobalSettingApplyBTN2: TButton;
		CloudMaxFileSizeValue: TEdit;
		CloudMaxFileSizeLabelBytes: TLabel;
		CloudMaxFileSizeCB: TCheckBox;
		ChunkOverwriteModeLabel: TLabel;
		ChunkOverwriteModeCombo: TComboBox;
		DeleteFailOnUploadModeLabel: TLabel;
		DeleteFailOnUploadModeCombo: TComboBox;
		OverwriteLocalModeLabel: TLabel;
		OverwriteLocalModeCombo: TComboBox;
		DisableMultiThreadingCB: TCheckBox;
		IconsModeCombo: TComboBox;
		IconsModeLabel: TLabel;
		SpaceInfoLoggingCB: TCheckBox;
		OperationErrorModeLabel: TLabel;
		OperationErrorModeCombo: TComboBox;
		RetryAttemptsLabel: TLabel;
		RetryWaitLabel: TLabel;
		msLabel: TLabel;
		AttemptWaitValue: TSpinEdit;
		RetryAttemptsValue: TSpinEdit;
		SocketTimeoutEdit: TSpinEdit;
		DownloadLinksEncodeCB: TCheckBox;
		AutoUpdateDownloadListingCB: TCheckBox;
		ShowTrashFoldersCB: TCheckBox;
		ShowSharedFoldersCB: TCheckBox;
		ShowInvitesFoldersCB: TCheckBox;
		ShowAccountsLabel: TLabel;
		CommentsTab: TTabSheet;
		DescriptionEnabledCB: TCheckBox;
		DescriptionEditorEnabledCB: TCheckBox;
		GlobalSettingApplyBTN3: TButton;
		DescriptionCopyToCloudCB: TCheckBox;
		DescriptionCopyFromCloudCB: TCheckBox;
		DescriptionFileNameLabel: TLabel;
		DescriptionFileNameEdit: TEdit;
		DescriptionTrackCloudFSCB: TCheckBox;
		EncryptGB: TGroupBox;
		EncryptFilenamesCB: TCheckBox;
		EncryptFilesCombo: TComboBox;
		EncryptFilesLabel: TLabel;
		EncryptFilesPwdButton: TButton;
		PrecalculateHashCB: TCheckBox;
		CheckCRCCB: TCheckBox;
		SpeedLimitGB: TGroupBox;
		UploadsBPSLabel: TLabel;
		UploadBPSEdit: TSpinEdit;
		DownloadsBPSLabel: TLabel;
		DownloadBPSEdit: TSpinEdit;
		CopyBetweenAccountsModeCombo: TComboBox;
		CopyBetweenAccountsModeLabel: TLabel;
		NewAccountBtn: TButton;
		procedure FormShow(Sender: TObject);
		procedure AccountsListClick(Sender: TObject);
		procedure ApplyButtonClick(Sender: TObject);
		procedure UpdateAccountsList();
		procedure DeleteButtonClick(Sender: TObject);
		procedure AccountsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		class procedure ShowAccounts(parentWindow: HWND; IniPath, SettingsIniFilePath: WideString; PasswordManager: TTCPasswordManager; Account: WideString);
		procedure FormActivate(Sender: TObject);
		procedure ProxyUserEditChange(Sender: TObject);
		procedure GlobalSettingApplyBTNClick(Sender: TObject);
		procedure PublicAccountCBClick(Sender: TObject);
		procedure CloudMaxFileSizeCBClick(Sender: TObject);
		procedure EncryptFilesComboChange(Sender: TObject);
		procedure EncryptFilesPwdButtonClick(Sender: TObject);
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure NewAccountBtnClick(Sender: TObject);
	private
		{Private declarations}
		procedure ApplySettings();
		function CheckValidators(): boolean;

	public
		{Public declarations}
		IniPath: WideString;
		SettingsIniFilePath: WideString;
		PasswordManager: TTCPasswordManager;
		SelectedAccount: WideString;

	end;

implementation

{$R *.dfm}

procedure TAccountsForm.UpdateAccountsList();
var
	TempList: TStringList;
begin
	TempList := TStringList.Create;
	GetAccountsListFromIniFile(IniPath, TempList);
	AccountsList.Items := TempList;
	TempList.Destroy;
	AccountsList.OnClick(self);
end;

procedure TAccountsForm.AccountsListClick(Sender: TObject);
var
	CASettings: TAccountSettings;
begin
	if (AccountsList.Items.Count > 0) and (AccountsList.ItemIndex <> -1) then
	begin
		CASettings := GetAccountSettingsFromIniFile(IniPath, AccountsList.Items[AccountsList.ItemIndex]);
		AccountNameEdit.Text := CASettings.name;
		EmailEdit.Text := CASettings.email;
		PasswordEdit.Text := CASettings.password;
		UseTCPwdMngrCB.Checked := CASettings.use_tc_password_manager;
		UnlimitedFileSizeCB.Checked := CASettings.unlimited_filesize;
		SplitLargeFilesCB.Checked := CASettings.split_large_files;
		PublicAccountCB.Checked := CASettings.public_account;
		PublicUrlEdit.Text := CASettings.public_url;
		TwostepAuthCB.Checked := CASettings.twostep_auth;
		EncryptFilesCombo.ItemIndex := CASettings.encrypt_files_mode;
		EncryptFilenamesCB.Checked := CASettings.encrypt_filenames;
		self.SelectedAccount := CASettings.name;
		EncryptFilesPwdButton.Enabled := true;
	end else begin
		AccountNameEdit.Text := EmptyWideStr;
		EmailEdit.Text := EmptyWideStr;
		PasswordEdit.Text := EmptyWideStr;
		UseTCPwdMngrCB.Checked := false;
		EncryptFilesPwdButton.Enabled := false;
	end;
	PublicAccountCB.OnClick(nil);
end;

procedure TAccountsForm.AccountsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_DELETE then
		DeleteButtonClick(nil);
end;

procedure TAccountsForm.ApplyButtonClick(Sender: TObject);
var
	CASettings: TAccountSettings;
begin
	if (AccountNameEdit.Text = EmptyWideStr) then
		exit();
	CASettings.name := AccountNameEdit.Text;
	CASettings.email := EmailEdit.Text;
	CASettings.password := PasswordEdit.Text;
	CASettings.use_tc_password_manager := UseTCPwdMngrCB.Checked;
	CASettings.unlimited_filesize := UnlimitedFileSizeCB.Checked;
	CASettings.split_large_files := SplitLargeFilesCB.Checked;
	CASettings.twostep_auth := TwostepAuthCB.Checked;
	CASettings.public_account := PublicAccountCB.Checked;
	CASettings.public_url := PublicUrlEdit.Text;
	CASettings.encrypt_files_mode := EncryptFilesCombo.ItemIndex;
	CASettings.encrypt_filenames := EncryptFilenamesCB.Checked;

	if CASettings.use_tc_password_manager then //просим TC сохранить пароль
	begin

		case PasswordManager.SetPassword(CASettings.name, CASettings.password) of
			FS_FILE_OK:
				begin //TC скушал пароль
					CASettings.password := EmptyWideStr;
				end;
			FS_FILE_NOTSUPPORTED: //нажали отмену на вводе мастер-пароля
				begin //просто выйдем
					exit();
				end;
			FS_FILE_WRITEERROR: //Сохранение не получилось по другой причине. Сохранять не будем, выйдем
				begin
					exit();
				end;
		end;
	end;

	SetAccountSettingsToIniFile(CASettings, IniPath);

	UpdateAccountsList();

end;

procedure TAccountsForm.ApplySettings;
begin
	if not CheckValidators() then
		exit;

	SetPluginSettingsValue(SettingsIniFilePath, 'LoadSSLDLLOnlyFromPluginDir', UseDLLFromPluginDir.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'PreserveFileTime', PreserveFileTimeCB.Checked);

	SetPluginSettingsValue(SettingsIniFilePath, 'DescriptionEnabled', DescriptionEnabledCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'DescriptionEditorEnabled', DescriptionEditorEnabledCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'DescriptionCopyToCloud', DescriptionCopyToCloudCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'DescriptionCopyFromCloud', DescriptionCopyFromCloudCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'DescriptionTrackCloudFS', DescriptionTrackCloudFSCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'DescriptionFileName', DescriptionFileNameEdit.Text);

	SetPluginSettingsValue(SettingsIniFilePath, 'CopyBetweenAccountsMode', CopyBetweenAccountsModeCombo.ItemIndex);

	if CloudMaxFileSizeCB.Checked then
	begin
		SetPluginSettingsValue(SettingsIniFilePath, 'CloudMaxFileSize', CloudMaxFileSizeValue.Text);
	end else begin
		SetPluginSettingsValue(SettingsIniFilePath, 'CloudMaxFileSize', null);
	end;

	SetPluginSettingsValue(SettingsIniFilePath, 'ChunkOverwriteMode', ChunkOverwriteModeCombo.ItemIndex);
	SetPluginSettingsValue(SettingsIniFilePath, 'DeleteFailOnUploadMode', DeleteFailOnUploadModeCombo.ItemIndex);
	SetPluginSettingsValue(SettingsIniFilePath, 'OverwriteLocalMode', OverwriteLocalModeCombo.ItemIndex);
	SetPluginSettingsValue(SettingsIniFilePath, 'OperationErrorMode', OperationErrorModeCombo.ItemIndex);
	SetPluginSettingsValue(SettingsIniFilePath, 'RetryAttempts', RetryAttemptsValue.Text);
	SetPluginSettingsValue(SettingsIniFilePath, 'AttemptWait', AttemptWaitValue.Text);

	SetPluginSettingsValue(SettingsIniFilePath, 'DisableMultiThreading', DisableMultiThreadingCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'LogUserSpace', SpaceInfoLoggingCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'IconsMode', IconsModeCombo.ItemIndex);

	SetPluginSettingsValue(SettingsIniFilePath, 'SocketTimeout', SocketTimeoutEdit.Text);
	SetPluginSettingsValue(SettingsIniFilePath, 'UploadBPS', UploadBPSEdit.Text);
	SetPluginSettingsValue(SettingsIniFilePath, 'DownloadBPS', DownloadBPSEdit.Text);
	SetPluginSettingsValue(SettingsIniFilePath, 'ProxyType', ProxyCB.ItemIndex);
	SetPluginSettingsValue(SettingsIniFilePath, 'ProxyServer', ProxyServerEdit.Text);
	SetPluginSettingsValue(SettingsIniFilePath, 'ProxyPort', ProxyPortEdit.Text);

	SetPluginSettingsValue(SettingsIniFilePath, 'ProxyUser', ProxyUserEdit.Text);
	SetPluginSettingsValue(SettingsIniFilePath, 'ProxyPassword', ProxyPwd.Text);
	SetPluginSettingsValue(SettingsIniFilePath, 'ProxyTCPwdMngr', ProxyTCPwdMngrCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'DownloadLinksEncode', DownloadLinksEncodeCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'AutoUpdateDownloadListing', AutoUpdateDownloadListingCB.Checked);

	SetPluginSettingsValue(SettingsIniFilePath, 'ShowTrashFolders', ShowTrashFoldersCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'ShowSharedFolders', ShowSharedFoldersCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'ShowInvitesFolders', ShowInvitesFoldersCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'PrecalculateHash', PrecalculateHashCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'CheckCRC', CheckCRCCB.Checked);

	if ProxyTCPwdMngrCB.Checked then //просим TC сохранить пароль
	begin
		case PasswordManager.SetPassword('proxy' + ProxyUserEdit.Text, ProxyPwd.Text) of
			FS_FILE_OK:
				begin //TC скушал пароль
					ProxyPwd.Text := EmptyWideStr;
					SetPluginSettingsValue(SettingsIniFilePath, 'ProxyPassword', EmptyWideStr);
				end;
			FS_FILE_NOTSUPPORTED: //нажали отмену на вводе мастер-пароля
				begin //просто выйдем
					exit();
				end;
			FS_FILE_WRITEERROR: //Сохранение не получилось по другой причине. Сохранять не будем, выйдем
				begin
					exit();
				end;
		end;
	end;
end;

function TAccountsForm.CheckValidators: boolean;
var
	MessageBaloon: TBalloonHint;
begin
	result := false;
	if not TPath.HasValidFileNameChars(DescriptionFileNameEdit.Text, false) then
	begin
		CommentsTab.Show;
		MessageBaloon := TBalloonHint.Create(self);
		MessageBaloon.HideAfter := 5000;
		MessageBaloon.Delay := 0;
		MessageBaloon.Description := 'File name must contain only valid symbols';
		MessageBaloon.ShowHint(DescriptionFileNameEdit);
		exit;
	end;
	exit(true);
end;

procedure TAccountsForm.CloudMaxFileSizeCBClick(Sender: TObject);
begin
	CloudMaxFileSizeValue.Enabled := CloudMaxFileSizeCB.Checked;
end;

procedure TAccountsForm.DeleteButtonClick(Sender: TObject);
begin
	if (AccountsList.Items.Count > 0) and (AccountsList.ItemIndex <> -1) then
	begin
		DeleteAccountFromIniFile(IniPath, AccountsList.Items[AccountsList.ItemIndex]);
		UpdateAccountsList();
	end;
end;

procedure TAccountsForm.EncryptFilesComboChange(Sender: TObject);
begin
	EncryptFilesPwdButton.Enabled := EncryptFilesCombo.ItemIndex = EncryptModeAlways;
end;

procedure TAccountsForm.EncryptFilesPwdButtonClick(Sender: TObject);
var
	CryptedGUID: WideString;
begin
	CryptedGUID := PasswordManager.StoreFileCryptPassword(self.SelectedAccount);
	if CryptedGUID <> EmptyWideStr then
		SetAccountSettingsValue(IniPath, self.SelectedAccount, 'CryptedGUID_files', CryptedGUID);

end;

procedure TAccountsForm.FormActivate(Sender: TObject);
begin
	ProxyTCPwdMngrCB.Enabled := ProxyUserEdit.Text <> EmptyWideStr;
	CenterWindow(self.parentWindow, self.Handle);
end;

procedure TAccountsForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	case Key of
		VK_ESCAPE:
			Close;
	end;
end;

procedure TAccountsForm.FormShow(Sender: TObject);
begin
	UpdateAccountsList();
	AccountsList.SetFocus;
	if AccountsList.Items.Count > 0 then
	begin
		if (self.SelectedAccount <> EmptyWideStr) and (AccountsList.Items.IndexOf(self.SelectedAccount) <> -1) then
		begin
			AccountsList.Selected[AccountsList.Items.IndexOf(self.SelectedAccount)] := true;
		end else begin
			AccountsList.Selected[0] := true;
		end;
		AccountsList.OnClick(self);
	end;

end;

procedure TAccountsForm.GlobalSettingApplyBTNClick(Sender: TObject);
begin
	ApplySettings;
end;

procedure TAccountsForm.NewAccountBtnClick(Sender: TObject);
var
	Account: TAccountSettings;
begin
	if mrOk = TRegistrationForm.ShowRegistration(self.parentWindow, Account) then
	begin

	end;
end;

procedure TAccountsForm.ProxyUserEditChange(Sender: TObject);
begin
	ProxyTCPwdMngrCB.Enabled := ProxyUserEdit.Text <> EmptyWideStr;
end;

procedure TAccountsForm.PublicAccountCBClick(Sender: TObject);
begin
	SharesPanel.Visible := PublicAccountCB.Checked;
	AccountsPanel.Visible := not PublicAccountCB.Checked;
end;

class procedure TAccountsForm.ShowAccounts(parentWindow: HWND; IniPath, SettingsIniFilePath: WideString; PasswordManager: TTCPasswordManager; Account: WideString);
var
	AccountsForm: TAccountsForm;
begin
	try
		AccountsForm := TAccountsForm.Create(nil);
		AccountsForm.parentWindow := parentWindow;
		AccountsForm.IniPath := IniPath;
		AccountsForm.SettingsIniFilePath := SettingsIniFilePath;
		AccountsForm.PasswordManager := PasswordManager;
		AccountsForm.SelectedAccount := EmptyWideStr;
		{global settings}
		AccountsForm.UseDLLFromPluginDir.Checked := GetPluginSettings(SettingsIniFilePath).LoadSSLDLLOnlyFromPluginDir;
		AccountsForm.PreserveFileTimeCB.Checked := GetPluginSettings(SettingsIniFilePath).PreserveFileTime;
		AccountsForm.DescriptionEnabledCB.Checked := GetPluginSettings(SettingsIniFilePath).DescriptionEnabled;
		AccountsForm.DescriptionEditorEnabledCB.Checked := GetPluginSettings(SettingsIniFilePath).DescriptionEditorEnabled;
		AccountsForm.DescriptionCopyToCloudCB.Checked := GetPluginSettings(SettingsIniFilePath).DescriptionCopyToCloud;
		AccountsForm.DescriptionCopyFromCloudCB.Checked := GetPluginSettings(SettingsIniFilePath).DescriptionCopyFromCloud;
		AccountsForm.DescriptionTrackCloudFSCB.Checked := GetPluginSettings(SettingsIniFilePath).DescriptionTrackCloudFS;
		AccountsForm.DescriptionFileNameEdit.Text := GetPluginSettings(SettingsIniFilePath).DescriptionFileName;

		AccountsForm.CopyBetweenAccountsModeCombo.ItemIndex := GetPluginSettings(SettingsIniFilePath).CopyBetweenAccountsMode;
		AccountsForm.SocketTimeoutEdit.Text := GetPluginSettings(SettingsIniFilePath).ConnectionSettings.SocketTimeout.ToString;
		AccountsForm.UploadBPSEdit.Text := GetPluginSettings(SettingsIniFilePath).ConnectionSettings.UploadBPS.ToString;
		AccountsForm.DownloadBPSEdit.Text := GetPluginSettings(SettingsIniFilePath).ConnectionSettings.DownloadBPS.ToString;

		AccountsForm.ProxyCB.ItemIndex := GetPluginSettings(SettingsIniFilePath).ConnectionSettings.ProxySettings.ProxyType;
		AccountsForm.ProxyServerEdit.Text := GetPluginSettings(SettingsIniFilePath).ConnectionSettings.ProxySettings.Server;
		AccountsForm.ProxyPortEdit.Text := GetPluginSettings(SettingsIniFilePath).ConnectionSettings.ProxySettings.Port.ToString;
		AccountsForm.ProxyUserEdit.Text := GetPluginSettings(SettingsIniFilePath).ConnectionSettings.ProxySettings.user;
		AccountsForm.ProxyPwd.Text := GetPluginSettings(SettingsIniFilePath).ConnectionSettings.ProxySettings.password;
		AccountsForm.ProxyTCPwdMngrCB.Checked := GetPluginSettings(SettingsIniFilePath).ConnectionSettings.ProxySettings.use_tc_password_manager;
		AccountsForm.CloudMaxFileSizeValue.Text := GetPluginSettings(SettingsIniFilePath).CloudMaxFileSize.ToString;
		if (GetPluginSettings(SettingsIniFilePath).CloudMaxFileSize <> CLOUD_MAX_FILESIZE_DEFAULT) then
		begin
			AccountsForm.CloudMaxFileSizeValue.Enabled := true;
			AccountsForm.CloudMaxFileSizeCB.Checked := true;
		end;
		AccountsForm.ChunkOverwriteModeCombo.ItemIndex := GetPluginSettings(SettingsIniFilePath).ChunkOverwriteMode;
		AccountsForm.DeleteFailOnUploadModeCombo.ItemIndex := GetPluginSettings(SettingsIniFilePath).DeleteFailOnUploadMode;
		AccountsForm.OverwriteLocalModeCombo.ItemIndex := GetPluginSettings(SettingsIniFilePath).OverwriteLocalMode;
		AccountsForm.OperationErrorModeCombo.ItemIndex := GetPluginSettings(SettingsIniFilePath).OperationErrorMode;
		AccountsForm.RetryAttemptsValue.Text := GetPluginSettings(SettingsIniFilePath).RetryAttempts.ToString;
		AccountsForm.AttemptWaitValue.Text := GetPluginSettings(SettingsIniFilePath).AttemptWait.ToString;

		AccountsForm.DisableMultiThreadingCB.Checked := GetPluginSettings(SettingsIniFilePath).DisableMultiThreading;
		AccountsForm.SpaceInfoLoggingCB.Checked := GetPluginSettings(SettingsIniFilePath).LogUserSpace;
		AccountsForm.IconsModeCombo.ItemIndex := GetPluginSettings(SettingsIniFilePath).IconsMode;

		AccountsForm.DownloadLinksEncodeCB.Checked := GetPluginSettings(SettingsIniFilePath).DownloadLinksEncode;
		AccountsForm.AutoUpdateDownloadListingCB.Checked := GetPluginSettings(SettingsIniFilePath).AutoUpdateDownloadListing;
		AccountsForm.ShowTrashFoldersCB.Checked := GetPluginSettings(SettingsIniFilePath).ShowTrashFolders;
		AccountsForm.ShowSharedFoldersCB.Checked := GetPluginSettings(SettingsIniFilePath).ShowSharedFolders;
		AccountsForm.ShowInvitesFoldersCB.Checked := GetPluginSettings(SettingsIniFilePath).ShowInvitesFolders;
		AccountsForm.PrecalculateHashCB.Checked := GetPluginSettings(SettingsIniFilePath).PrecalculateHash;
		AccountsForm.CheckCRCCB.Checked := GetPluginSettings(SettingsIniFilePath).CheckCRC;

		{global settings}
		if Account <> EmptyWideStr then
			AccountsForm.SelectedAccount := Account;
		AccountsForm.OptionPages.ActivePageIndex := 0;
		AccountsForm.ShowModal;
	finally
		FreeAndNil(AccountsForm);
	end;
end;

end.
