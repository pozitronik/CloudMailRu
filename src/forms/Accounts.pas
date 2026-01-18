unit Accounts;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.StdCtrls,
	IniFiles,
	TCHelper,
	PathHelper,
	WindowsHelper,
	SETTINGS_CONSTANTS,
	PLUGIN_TYPES,
	Vcl.ComCtrls,
	Vcl.Mask,
	Vcl.ExtCtrls,
	Vcl.Samples.Spin,
	System.IOUtils,
	AskPassword,
	IPasswordManagerInterface,
	Registration,
	LANGUAGE_STRINGS,
	WSList,
	StreamingSettings,
	PluginSettingsManager,
	FileCipher,
	AccountSettings,
	AccountsManager;

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
		StreamingTab: TTabSheet;
		TExtensionsGroupBox: TGroupBox;
		StreamingExtensionsList: TListBox;
		ExtLabel: TLabel;
		StreamingExtensionEdit: TEdit;
		CommandLabel: TLabel;
		CommandPathEdit: TEdit;
		CommandPathButton: TButton;
		ParametersLabel: TLabel;
		ParametersEdit: TEdit;
		StartPathLabel: TLabel;
		StartPathEdit: TEdit;
		StreamingTypeLabel: TLabel;
		StreamingTypeCombo: TComboBox;
		ApplyExtButton: TButton;
		DeleteExtButton: TButton;
		CommandPathOpenDialog: TOpenDialog;
		UserAgentEdit: TEdit;
		ChangeUserAgentCB: TCheckBox;
		procedure FormShow(Sender: TObject);
		procedure AccountsListClick(Sender: TObject);
		procedure ApplyButtonClick(Sender: TObject);
		procedure UpdateAccountsList();
		procedure UpdateStreamingExtensionsList();
		procedure DeleteButtonClick(Sender: TObject);
		procedure AccountsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		class function ShowAccounts(ParentWindow: HWND; PasswordManager: IPasswordManager; Account: WideString): Boolean;
		procedure FormActivate(Sender: TObject);
		procedure ProxyUserEditChange(Sender: TObject);
		procedure GlobalSettingApplyBTNClick(Sender: TObject);
		procedure PublicAccountCBClick(Sender: TObject);
		procedure CloudMaxFileSizeCBClick(Sender: TObject);
		procedure EncryptFilesComboChange(Sender: TObject);
		procedure EncryptFilesPwdButtonClick(Sender: TObject);
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure NewAccountBtnClick(Sender: TObject);
		procedure StreamingExtensionsListClick(Sender: TObject);
		procedure StreamingExtensionsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure DeleteExtButtonClick(Sender: TObject);
		procedure ApplyExtButtonClick(Sender: TObject);
		procedure CommandPathButtonClick(Sender: TObject);
		procedure ChangeUserAgentCBClick(Sender: TObject);
	private
		{Private declarations}
		AccountsManager: TAccountsManager;
		SettingsManager: TPluginSettingsManager;
		PasswordManager: IPasswordManager;
		DialogParentWindow: HWND; {Parent window for password dialogs}
		SelectedAccount: WideString;
		SettingsApplied: Boolean;
		procedure ApplySettings();
		function CheckValidators(): Boolean;
		function StoreFileCryptPassword(AccountName: WideString): WideString;
	public

	end;

implementation

{$R *.dfm}

uses
	ConnectionSettings,
	ProxySettings,
	CMRConstants,
	IniConfigFile;

procedure TAccountsForm.UpdateAccountsList();
var
	TempList: TStrings;
begin
	TempList := TStringList.Create;
	try
		TempList.AddStrings(self.AccountsManager.GetAccountsList());
		AccountsList.Items := TempList;
	finally
		TempList.Free;
	end;
	AccountsList.OnClick(self);
end;

procedure TAccountsForm.UpdateStreamingExtensionsList();
var
	TempList: TStringList;
begin
	TempList := TStringList.Create;
	try
		SettingsManager.GetStreamingExtensionsList(TempList);
		StreamingExtensionsList.Items := TempList;
	finally
		TempList.Free;
	end;
end;

procedure TAccountsForm.AccountsListClick(Sender: TObject);
var
	CurrentAccountSettings: TAccountSettings;
begin
	if (AccountsList.Items.Count > 0) and (AccountsList.ItemIndex <> -1) then
	begin
		CurrentAccountSettings := AccountsManager.GetAccountSettings(AccountsList.Items[AccountsList.ItemIndex]);
		with CurrentAccountSettings do
		begin
			AccountNameEdit.Text := Account;
			EmailEdit.Text := Email;
			PasswordEdit.Text := Password;
			UseTCPwdMngrCB.Checked := UseTCPasswordManager;
			UnlimitedFileSizeCB.Checked := UnlimitedFilesize;
			SplitLargeFilesCB.Checked := SplitLargeFiles;
			PublicAccountCB.Checked := PublicAccount;
			PublicUrlEdit.Text := PublicUrl;
			EncryptFilesCombo.ItemIndex := EncryptFilesMode;
			EncryptFilenamesCB.Checked := EncryptFilenames;
			self.SelectedAccount := User;
			EncryptFilesComboChange(nil);
		end;
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
	CurrentAccountSettings: TAccountSettings;
begin
	if (AccountNameEdit.Text = EmptyWideStr) then
		exit();
	with CurrentAccountSettings do
	begin
		Account := AccountNameEdit.Text;
		Email := EmailEdit.Text;
		Password := PasswordEdit.Text;
		UseTCPasswordManager := UseTCPwdMngrCB.Checked;
		UnlimitedFilesize := UnlimitedFileSizeCB.Checked;
		SplitLargeFiles := SplitLargeFilesCB.Checked;
		TwostepAuth := False; {Deprecated: two-step auth is not used with OAuth}
		PublicAccount := PublicAccountCB.Checked;
		PublicUrl := PublicUrlEdit.Text;
		EncryptFilesMode := EncryptFilesCombo.ItemIndex;
		EncryptFilenames := EncryptFilenamesCB.Checked;
		{OAuth app password is the only supported auth method now}
		AuthMethod := CLOUD_AUTH_METHOD_OAUTH_APP;
		UseAppPassword := True;
	end;
	if CurrentAccountSettings.UseTCPasswordManager then //просим TC сохранить пароль
	begin

		case PasswordManager.SetPassword(CurrentAccountSettings.Account, CurrentAccountSettings.Password) of
			FS_FILE_OK:
				begin //TC скушал пароль
					CurrentAccountSettings.Password := EmptyWideStr;
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

	AccountsManager.SetAccountSettings(CurrentAccountSettings);

	UpdateAccountsList();

end;

procedure TAccountsForm.ApplyExtButtonClick(Sender: TObject);
var
	StreamingSettings: TStreamingSettings;
begin
	StreamingSettings.Command := CommandPathEdit.Text;
	StreamingSettings.Parameters := ParametersEdit.Text;
	StreamingSettings.StartPath := StartPathEdit.Text;
	StreamingSettings.Format := StreamingTypeCombo.ItemIndex;
	SettingsManager.SetStreamingSettings(DOT + StreamingExtensionEdit.Text, StreamingSettings);
	UpdateStreamingExtensionsList();
end;

procedure TAccountsForm.ApplySettings;
begin
	if not CheckValidators() then
		exit;

	self.SettingsManager.Settings.LoadSSLDLLOnlyFromPluginDir := UseDLLFromPluginDir.Checked;
	self.SettingsManager.Settings.PreserveFileTime := PreserveFileTimeCB.Checked;
	self.SettingsManager.Settings.DescriptionEnabled := DescriptionEnabledCB.Checked;
	self.SettingsManager.Settings.DescriptionEditorEnabled := DescriptionEditorEnabledCB.Checked;
	self.SettingsManager.Settings.DescriptionCopyToCloud := DescriptionCopyToCloudCB.Checked;
	self.SettingsManager.Settings.DescriptionCopyFromCloud := DescriptionCopyFromCloudCB.Checked;
	self.SettingsManager.Settings.DescriptionTrackCloudFS := DescriptionTrackCloudFSCB.Checked;
	self.SettingsManager.Settings.DescriptionFileName := DescriptionFileNameEdit.Text;
	self.SettingsManager.Settings.CopyBetweenAccountsMode := CopyBetweenAccountsModeCombo.ItemIndex;

	if CloudMaxFileSizeCB.Checked then
	begin
		self.SettingsManager.Settings.CloudMaxFileSize := StrToInt(CloudMaxFileSizeValue.Text);
	end else begin
		self.SettingsManager.Settings.CloudMaxFileSize := CLOUD_MAX_FILESIZE_DEFAULT;
	end;

	self.SettingsManager.Settings.ChunkOverwriteMode := ChunkOverwriteModeCombo.ItemIndex;
	self.SettingsManager.Settings.DeleteFailOnUploadMode := DeleteFailOnUploadModeCombo.ItemIndex;
	self.SettingsManager.Settings.OverwriteLocalMode := OverwriteLocalModeCombo.ItemIndex;
	self.SettingsManager.Settings.OperationErrorMode := OperationErrorModeCombo.ItemIndex;
	self.SettingsManager.Settings.RetryAttempts := StrToInt(RetryAttemptsValue.Text);
	self.SettingsManager.Settings.AttemptWait := StrToInt(AttemptWaitValue.Text);
	self.SettingsManager.Settings.DisableMultiThreading := DisableMultiThreadingCB.Checked;
	self.SettingsManager.Settings.LogUserSpace := SpaceInfoLoggingCB.Checked;
	self.SettingsManager.Settings.IconsMode := IconsModeCombo.ItemIndex;
	self.SettingsManager.Settings.ConnectionSettings.SocketTimeout := StrToInt(SocketTimeoutEdit.Text);
	self.SettingsManager.Settings.ConnectionSettings.UploadBPS := StrToInt(UploadBPSEdit.Text);
	self.SettingsManager.Settings.ConnectionSettings.DownloadBPS := StrToInt(DownloadBPSEdit.Text);
	self.SettingsManager.Settings.ConnectionSettings.ProxySettings.ProxyType := ProxyCB.ItemIndex;
	self.SettingsManager.Settings.ConnectionSettings.ProxySettings.Server := ProxyServerEdit.Text;
	self.SettingsManager.Settings.ConnectionSettings.ProxySettings.Port := StrToInt(ProxyPortEdit.Text);
	self.SettingsManager.Settings.ConnectionSettings.ProxySettings.User := ProxyUserEdit.Text;
	self.SettingsManager.Settings.ConnectionSettings.ProxySettings.Password := ProxyPwd.Text;
	self.SettingsManager.Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager := ProxyTCPwdMngrCB.Checked;

	if ChangeUserAgentCB.Checked then
		self.SettingsManager.Settings.ConnectionSettings.UserAgent := UserAgentEdit.Text;

	self.SettingsManager.Settings.DownloadLinksEncode := DownloadLinksEncodeCB.Checked;
	self.SettingsManager.Settings.AutoUpdateDownloadListing := AutoUpdateDownloadListingCB.Checked;
	self.SettingsManager.Settings.ShowTrashFolders := ShowTrashFoldersCB.Checked;
	self.SettingsManager.Settings.ShowSharedFolders := ShowSharedFoldersCB.Checked;
	self.SettingsManager.Settings.ShowInvitesFolders := ShowInvitesFoldersCB.Checked;
	self.SettingsManager.Settings.PrecalculateHash := PrecalculateHashCB.Checked;
	self.SettingsManager.Settings.CheckCRC := CheckCRCCB.Checked;
	self.SettingsManager.Save;
	self.SettingsApplied := True;
	if ProxyTCPwdMngrCB.Checked then //просим TC сохранить пароль
	begin
		case PasswordManager.SetPassword(PASSWORD_KEY_PROXY + ProxyUserEdit.Text, ProxyPwd.Text) of
			FS_FILE_OK:
				begin //TC скушал пароль
					ProxyPwd.Text := EmptyWideStr;
					self.SettingsManager.Settings.ConnectionSettings.ProxySettings.Password := EmptyWideStr;
					self.SettingsManager.Save;
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

procedure TAccountsForm.ChangeUserAgentCBClick(Sender: TObject);
begin
	UserAgentEdit.ReadOnly := not ChangeUserAgentCB.Checked;
end;


function TAccountsForm.CheckValidators: Boolean;
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
		MessageBaloon.Description := ERR_ACCOUNT_HAS_INVALID_SYMBOL;
		MessageBaloon.ShowHint(DescriptionFileNameEdit);
		exit;
	end;
	exit(True);
end;

procedure TAccountsForm.CloudMaxFileSizeCBClick(Sender: TObject);
begin
	CloudMaxFileSizeValue.Enabled := CloudMaxFileSizeCB.Checked;
end;

procedure TAccountsForm.CommandPathButtonClick(Sender: TObject);
begin
	CommandPathOpenDialog.InitialDir := ExtractUniversalFilePath(CommandPathEdit.Text);
	if CommandPathOpenDialog.Execute(FindTCWindow) then
	begin
		CommandPathEdit.Text := CommandPathOpenDialog.FileName;
	end;

end;

procedure TAccountsForm.DeleteButtonClick(Sender: TObject);
begin
	if (AccountsList.Items.Count > 0) and (AccountsList.ItemIndex <> -1) then
	begin
		AccountsManager.DeleteAccount(AccountsList.Items[AccountsList.ItemIndex]);
		UpdateAccountsList();
	end;
end;

procedure TAccountsForm.DeleteExtButtonClick(Sender: TObject);
begin
	if (StreamingExtensionsList.Items.Count > 0) and (StreamingExtensionsList.ItemIndex <> -1) then
	begin
		SettingsManager.RemoveStreamingExtension(StreamingExtensionsList.Items[StreamingExtensionsList.ItemIndex]);
		UpdateStreamingExtensionsList();
	end;
end;

procedure TAccountsForm.EncryptFilesComboChange(Sender: TObject);
begin
	EncryptFilesPwdButton.Enabled := EncryptFilesCombo.ItemIndex = EncryptModeAlways;
end;

procedure TAccountsForm.EncryptFilesPwdButtonClick(Sender: TObject);
var
	CryptedGUID: WideString;
	TempAccountSettings: TAccountSettings;
begin
	DialogParentWindow := self.Handle; {Use form handle for dialogs}
	CryptedGUID := StoreFileCryptPassword(self.SelectedAccount);
	DialogParentWindow := FindTCWindow; {Restore for subsequent dialogs}
	if CryptedGUID <> EmptyWideStr then
	begin
		TempAccountSettings := AccountsManager.GetAccountSettings(self.SelectedAccount);
		TempAccountSettings.CryptedGUIDFiles := CryptedGUID;
		AccountsManager.SetAccountSettings(TempAccountSettings);
	end;
end;

procedure TAccountsForm.FormActivate(Sender: TObject);
begin
	ProxyTCPwdMngrCB.Enabled := ProxyUserEdit.Text <> EmptyWideStr;
	CenterWindow(self.ParentWindow, self.Handle);
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
	UpdateStreamingExtensionsList();
	AccountsList.SetFocus;
	if AccountsList.Items.Count > 0 then
	begin
		if (self.SelectedAccount <> EmptyWideStr) and (AccountsList.Items.IndexOf(self.SelectedAccount) <> -1) then
		begin
			AccountsList.Selected[AccountsList.Items.IndexOf(self.SelectedAccount)] := True;
		end else begin
			AccountsList.Selected[0] := True;
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
	TempAccountSettings: TAccountSettings;
begin
	TempAccountSettings := AccountsManager.GetAccountSettings(self.SelectedAccount);
	if mrOk = TRegistrationForm.ShowRegistration(self.ParentWindow, SettingsManager.Settings.ConnectionSettings, TempAccountSettings) then
	begin
		if TempAccountSettings.UseTCPasswordManager then //просим TC сохранить пароль
			if FS_FILE_OK <> PasswordManager.SetPassword(TempAccountSettings.Account, TempAccountSettings.Password) then
				exit(); //Не удалось сохранить пароль/нажали отмену

		AccountsManager.SetAccountSettings(TempAccountSettings);
		UpdateAccountsList();
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

{The method returns True if settings were applied}
class function TAccountsForm.ShowAccounts(ParentWindow: HWND; PasswordManager: IPasswordManager; Account: WideString): Boolean;
var
	AccountsForm: TAccountsForm;
begin
	try
		AccountsForm := TAccountsForm.Create(nil);
		AccountsForm.ParentWindow := ParentWindow;
		AccountsForm.DialogParentWindow := ParentWindow; {Store for password dialogs}

		AccountsForm.SettingsManager := TPluginSettingsManager.Create();
		AccountsForm.AccountsManager := TAccountsManager.Create(TIniConfigFile.Create(AccountsForm.SettingsManager.AccountsIniFilePath));

		AccountsForm.PasswordManager := PasswordManager;
		AccountsForm.SelectedAccount := EmptyWideStr;
		{global settings}
		AccountsForm.UseDLLFromPluginDir.Checked := AccountsForm.SettingsManager.Settings.LoadSSLDLLOnlyFromPluginDir;
		AccountsForm.PreserveFileTimeCB.Checked := AccountsForm.SettingsManager.Settings.PreserveFileTime;
		AccountsForm.DescriptionEnabledCB.Checked := AccountsForm.SettingsManager.Settings.DescriptionEnabled;
		AccountsForm.DescriptionEditorEnabledCB.Checked := AccountsForm.SettingsManager.Settings.DescriptionEditorEnabled;
		AccountsForm.DescriptionCopyToCloudCB.Checked := AccountsForm.SettingsManager.Settings.DescriptionCopyToCloud;
		AccountsForm.DescriptionCopyFromCloudCB.Checked := AccountsForm.SettingsManager.Settings.DescriptionCopyFromCloud;
		AccountsForm.DescriptionTrackCloudFSCB.Checked := AccountsForm.SettingsManager.Settings.DescriptionTrackCloudFS;
		AccountsForm.DescriptionFileNameEdit.Text := AccountsForm.SettingsManager.Settings.DescriptionFileName;

		AccountsForm.CopyBetweenAccountsModeCombo.ItemIndex := AccountsForm.SettingsManager.Settings.CopyBetweenAccountsMode;
		AccountsForm.SocketTimeoutEdit.Text := AccountsForm.SettingsManager.Settings.ConnectionSettings.SocketTimeout.ToString;
		AccountsForm.UploadBPSEdit.Text := AccountsForm.SettingsManager.Settings.ConnectionSettings.UploadBPS.ToString;
		AccountsForm.DownloadBPSEdit.Text := AccountsForm.SettingsManager.Settings.ConnectionSettings.DownloadBPS.ToString;

		AccountsForm.ProxyCB.ItemIndex := AccountsForm.SettingsManager.Settings.ConnectionSettings.ProxySettings.ProxyType;
		AccountsForm.ProxyServerEdit.Text := AccountsForm.SettingsManager.Settings.ConnectionSettings.ProxySettings.Server;
		AccountsForm.ProxyPortEdit.Text := AccountsForm.SettingsManager.Settings.ConnectionSettings.ProxySettings.Port.ToString;
		AccountsForm.ProxyUserEdit.Text := AccountsForm.SettingsManager.Settings.ConnectionSettings.ProxySettings.User;
		AccountsForm.ProxyPwd.Text := AccountsForm.SettingsManager.Settings.ConnectionSettings.ProxySettings.Password;
		AccountsForm.ProxyTCPwdMngrCB.Checked := AccountsForm.SettingsManager.Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager;

		AccountsForm.UserAgentEdit.Text := AccountsForm.SettingsManager.Settings.ConnectionSettings.UserAgent;
		AccountsForm.ChangeUserAgentCB.Checked := DEFAULT_USERAGENT <> AccountsForm.UserAgentEdit.Text;
		AccountsForm.UserAgentEdit.ReadOnly := not AccountsForm.ChangeUserAgentCB.Checked;

		AccountsForm.CloudMaxFileSizeValue.Text := AccountsForm.SettingsManager.Settings.CloudMaxFileSize.ToString;

		if (AccountsForm.SettingsManager.Settings.CloudMaxFileSize <> CLOUD_MAX_FILESIZE_DEFAULT) then
		begin
			AccountsForm.CloudMaxFileSizeValue.Enabled := True;
			AccountsForm.CloudMaxFileSizeCB.Checked := True;
		end;
		AccountsForm.ChunkOverwriteModeCombo.ItemIndex := AccountsForm.SettingsManager.Settings.ChunkOverwriteMode;
		AccountsForm.DeleteFailOnUploadModeCombo.ItemIndex := AccountsForm.SettingsManager.Settings.DeleteFailOnUploadMode;
		AccountsForm.OverwriteLocalModeCombo.ItemIndex := AccountsForm.SettingsManager.Settings.OverwriteLocalMode;
		AccountsForm.OperationErrorModeCombo.ItemIndex := AccountsForm.SettingsManager.Settings.OperationErrorMode;
		AccountsForm.RetryAttemptsValue.Text := AccountsForm.SettingsManager.Settings.RetryAttempts.ToString;
		AccountsForm.AttemptWaitValue.Text := AccountsForm.SettingsManager.Settings.AttemptWait.ToString;

		AccountsForm.DisableMultiThreadingCB.Checked := AccountsForm.SettingsManager.Settings.DisableMultiThreading;
		AccountsForm.SpaceInfoLoggingCB.Checked := AccountsForm.SettingsManager.Settings.LogUserSpace;
		AccountsForm.IconsModeCombo.ItemIndex := AccountsForm.SettingsManager.Settings.IconsMode;

		AccountsForm.DownloadLinksEncodeCB.Checked := AccountsForm.SettingsManager.Settings.DownloadLinksEncode;
		AccountsForm.AutoUpdateDownloadListingCB.Checked := AccountsForm.SettingsManager.Settings.AutoUpdateDownloadListing;
		AccountsForm.ShowTrashFoldersCB.Checked := AccountsForm.SettingsManager.Settings.ShowTrashFolders;
		AccountsForm.ShowSharedFoldersCB.Checked := AccountsForm.SettingsManager.Settings.ShowSharedFolders;
		AccountsForm.ShowInvitesFoldersCB.Checked := AccountsForm.SettingsManager.Settings.ShowInvitesFolders;
		AccountsForm.PrecalculateHashCB.Checked := AccountsForm.SettingsManager.Settings.PrecalculateHash;
		AccountsForm.CheckCRCCB.Checked := AccountsForm.SettingsManager.Settings.CheckCRC;

		{global settings}
		if Account <> EmptyWideStr then
			AccountsForm.SelectedAccount := Account;
		AccountsForm.OptionPages.ActivePageIndex := 0;
		AccountsForm.ShowModal;
		result := AccountsForm.SettingsApplied;
	finally
		AccountsForm.SettingsManager.Free;
		AccountsForm.AccountsManager.Free;
		FreeAndNil(AccountsForm);
	end;
end;

{Sets/updates the file encryption password for the account, returns the password hash to save in configuration (to check the password later)}
function TAccountsForm.StoreFileCryptPassword(AccountName: WideString): WideString;
var
	CurrentPassword: WideString;
	crypt_id: WideString;
	Verb: WideString;
	StorePassword: Boolean;
begin
	StorePassword := True;
	result := EmptyWideStr;
	crypt_id := AccountName + PASSWORD_SUFFIX_FILECRYPT;
	case PasswordManager.GetPassword(crypt_id, CurrentPassword) of
		FS_FILE_OK: //пользователь знает мастер-пароль, и пароль был сохранен
			begin
				Verb := VERB_UPDATE;
			end;
		FS_FILE_READERROR: //Пользователь знает мастер-пароль, и пароль вводится впервые
			begin
				Verb := VERB_SET;
			end;
		else
			begin
				exit;
			end;
	end;
	if mrOk = TAskPasswordForm.AskPassword(Format(ASK_ENCRYPTION_PASSWORD, [Verb]), PREFIX_ASK_NEW_PASSWORD, CurrentPassword, StorePassword, True, DialogParentWindow) then
	begin
		PasswordManager.SetPassword(crypt_id, CurrentPassword);
		result := TFileCipher.GetCryptedGUID(CurrentPassword);
	end

end;

procedure TAccountsForm.StreamingExtensionsListClick(Sender: TObject);
var
	StreamingSettings: TStreamingSettings;
begin
	if (StreamingExtensionsList.Items.Count > 0) and (StreamingExtensionsList.ItemIndex <> -1) then
	begin
		StreamingSettings := SettingsManager.GetStreamingSettings(DOT + StreamingExtensionsList.Items[StreamingExtensionsList.ItemIndex]);
		//		GetStreamingOptionsFromIniFile(SettingsManager.IniFilePath, '.' + StreamingExtensionsList.Items[StreamingExtensionsList.ItemIndex], StreamingOptions); //не проверяем результат, это настройки
		StreamingExtensionEdit.Text := StreamingExtensionsList.Items[StreamingExtensionsList.ItemIndex];
		CommandPathEdit.Text := StreamingSettings.Command;
		ParametersEdit.Text := StreamingSettings.Parameters;
		StartPathEdit.Text := StreamingSettings.StartPath;
		StreamingTypeCombo.ItemIndex := StreamingSettings.Format;
	end else begin
		StreamingExtensionEdit.Text := EmptyWideStr;
		CommandPathEdit.Text := EmptyWideStr;
		ParametersEdit.Text := EmptyWideStr;
		StartPathEdit.Text := EmptyWideStr;
		StreamingTypeCombo.ItemIndex := 0
	end;
end;

procedure TAccountsForm.StreamingExtensionsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_DELETE then
		DeleteExtButton.OnClick(nil);
end;

end.
