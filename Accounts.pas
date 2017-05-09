unit Accounts;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Settings, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IniFiles, MRC_Helper, PLUGIN_Types, Vcl.ComCtrls, Vcl.Mask, Vcl.ExtCtrls, Vcl.Samples.Spin;

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
		DescriptionEnabledCB: TCheckBox;
		OperationsViaPublicLinkEnabledCB: TCheckBox;
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
		procedure FormShow(Sender: TObject);
		procedure AccountsListClick(Sender: TObject);
		procedure ApplyButtonClick(Sender: TObject);
		procedure UpdateAccountsList();
		procedure DeleteButtonClick(Sender: TObject);
		procedure AccountsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		class procedure ShowAccounts(parentWindow: HWND; IniPath, SettingsIniFilePath: WideString; CryptProc: TCryptProcW; PluginNum, CryptoNum: Integer; Account: WideString);
		procedure FormActivate(Sender: TObject);
		procedure ProxyUserEditChange(Sender: TObject);
		procedure GlobalSettingApplyBTNClick(Sender: TObject);
		procedure PublicAccountCBClick(Sender: TObject);
		procedure CloudMaxFileSizeCBClick(Sender: TObject);
	private
		{Private declarations}
		procedure WMHotKey(var Message: TMessage); message WM_HOTKEY;
		procedure ApplySettings();
	public
		{Public declarations}
		IniPath: WideString;
		SettingsIniFilePath: WideString;
		CryptProc: TCryptProcW;
		PluginNum: Integer;
		CryptoNum: Integer;
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
	end else begin
		AccountNameEdit.Text := '';
		EmailEdit.Text := '';
		PasswordEdit.Text := '';
		UseTCPwdMngrCB.Checked := false;
	end;
	PublicAccountCB.OnClick(nil);
end;

procedure TAccountsForm.AccountsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_DELETE then DeleteButtonClick(nil);
end;

procedure TAccountsForm.ApplyButtonClick(Sender: TObject);
var
	CASettings: TAccountSettings;
begin
	if (AccountNameEdit.Text = '') then exit();
	CASettings.name := AccountNameEdit.Text;
	CASettings.email := EmailEdit.Text;
	CASettings.password := PasswordEdit.Text;
	CASettings.use_tc_password_manager := UseTCPwdMngrCB.Checked;
	CASettings.unlimited_filesize := UnlimitedFileSizeCB.Checked;
	CASettings.split_large_files := SplitLargeFilesCB.Checked;
	CASettings.twostep_auth := TwostepAuthCB.Checked;
	CASettings.public_account := PublicAccountCB.Checked;
	CASettings.public_url := PublicUrlEdit.Text;

	if CASettings.use_tc_password_manager then //просим TC сохранить пароль
	begin
		case self.CryptProc(self.PluginNum, self.CryptoNum, FS_CRYPT_SAVE_PASSWORD, PWideChar(CASettings.name), PWideChar(CASettings.password), SizeOf(CASettings.password)) of
			FS_FILE_OK:
				begin //TC скушал пароль
					CASettings.password := '';
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

	SetAccountSettingsToIniFile(IniPath, CASettings);

	UpdateAccountsList();

end;

procedure TAccountsForm.ApplySettings;
begin
	SetPluginSettingsValue(SettingsIniFilePath, 'LoadSSLDLLOnlyFromPluginDir', UseDLLFromPluginDir.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'PreserveFileTime', PreserveFileTimeCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'DescriptionEnabled', DescriptionEnabledCB.Checked);
	SetPluginSettingsValue(SettingsIniFilePath, 'OperationsViaPublicLinkEnabled', OperationsViaPublicLinkEnabledCB.Checked);
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

	if ProxyTCPwdMngrCB.Checked then //просим TC сохранить пароль
	begin
		case self.CryptProc(self.PluginNum, self.CryptoNum, FS_CRYPT_SAVE_PASSWORD, PWideChar('proxy' + ProxyUserEdit.Text), PWideChar(ProxyPwd.Text), SizeOf(ProxyPwd.Text)) of
			FS_FILE_OK:
				begin //TC скушал пароль
					ProxyPwd.Text := '';
					SetPluginSettingsValue(SettingsIniFilePath, 'ProxyPassword', '');
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

procedure TAccountsForm.FormActivate(Sender: TObject);
begin
	ProxyTCPwdMngrCB.Enabled := ProxyUserEdit.Text <> '';
	CenterWindow(self.parentWindow, self.Handle);
end;

procedure TAccountsForm.FormShow(Sender: TObject);
begin
	UpdateAccountsList();
	AccountsList.SetFocus;
	if AccountsList.Items.Count > 0 then
	begin
		if (self.SelectedAccount <> '') and (AccountsList.Items.IndexOf(self.SelectedAccount) <> -1) then
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

procedure TAccountsForm.ProxyUserEditChange(Sender: TObject);
begin
	ProxyTCPwdMngrCB.Enabled := ProxyUserEdit.Text <> '';
end;

procedure TAccountsForm.PublicAccountCBClick(Sender: TObject);
begin
	SharesPanel.Visible := PublicAccountCB.Checked;
	AccountsPanel.Visible := not PublicAccountCB.Checked;
end;

class procedure TAccountsForm.ShowAccounts(parentWindow: HWND; IniPath, SettingsIniFilePath: WideString; CryptProc: TCryptProcW; PluginNum, CryptoNum: Integer; Account: WideString);
var
	AccountsForm: TAccountsForm;
begin
	try
		AccountsForm := TAccountsForm.Create(nil);
		AccountsForm.parentWindow := parentWindow;
		AccountsForm.IniPath := IniPath;
		AccountsForm.SettingsIniFilePath := SettingsIniFilePath;
		AccountsForm.CryptProc := CryptProc;
		AccountsForm.PluginNum := PluginNum;
		AccountsForm.CryptoNum := CryptoNum;
		AccountsForm.SelectedAccount := '';
		{global settings}
		AccountsForm.UseDLLFromPluginDir.Checked := GetPluginSettings(SettingsIniFilePath).LoadSSLDLLOnlyFromPluginDir;
		AccountsForm.PreserveFileTimeCB.Checked := GetPluginSettings(SettingsIniFilePath).PreserveFileTime;
		AccountsForm.DescriptionEnabledCB.Checked := GetPluginSettings(SettingsIniFilePath).DescriptionEnabled;
		AccountsForm.OperationsViaPublicLinkEnabledCB.Checked := GetPluginSettings(SettingsIniFilePath).OperationsViaPublicLinkEnabled;
		AccountsForm.SocketTimeoutEdit.Text := GetPluginSettings(SettingsIniFilePath).SocketTimeout.ToString;
		AccountsForm.ProxyCB.ItemIndex := GetPluginSettings(SettingsIniFilePath).Proxy.ProxyType;
		AccountsForm.ProxyServerEdit.Text := GetPluginSettings(SettingsIniFilePath).Proxy.Server;
		AccountsForm.ProxyPortEdit.Text := GetPluginSettings(SettingsIniFilePath).Proxy.Port.ToString;
		AccountsForm.ProxyUserEdit.Text := GetPluginSettings(SettingsIniFilePath).Proxy.User;
		AccountsForm.ProxyPwd.Text := GetPluginSettings(SettingsIniFilePath).Proxy.password;
		AccountsForm.ProxyTCPwdMngrCB.Checked := GetPluginSettings(SettingsIniFilePath).Proxy.use_tc_password_manager;
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

		{global settings}
		if Account <> '' then AccountsForm.SelectedAccount := Account;
		RegisterHotKey(AccountsForm.Handle, 1, 0, VK_ESCAPE);
		AccountsForm.OptionPages.ActivePageIndex := 0;
		AccountsForm.ShowModal;
	finally
		FreeAndNil(AccountsForm);
	end;
end;

procedure TAccountsForm.WMHotKey(var Message: TMessage);
begin
	if (Message.LParamHi = VK_ESCAPE) and (GetForegroundWindow = self.Handle) then Close;
end;

end.
