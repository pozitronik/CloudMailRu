unit Accounts;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
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
	TCPasswordManager,
	Registration,
	CMRStrings,
	StreamingOptions,
	Settings,
	PluginSettings,
	WSList,
	NewAccountSettings;

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
		class procedure ShowAccounts(parentWindow: HWND; AccountSettings: TNewAccountSettings; Settings: TPluginSettings; PasswordManager: TTCPasswordManager; Account: WideString);
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
		procedure ApplySettings();
		function CheckValidators(): boolean;

	public
		{Public declarations}
		CurrentAccountSettings: TNewAccountSettings;
		PluginSettings: TPluginSettings;
		PasswordManager: TTCPasswordManager;
		SelectedAccount: WideString;

	end;

implementation

{$R *.dfm}

procedure TAccountsForm.UpdateAccountsList();
var
	TempList: TStrings;
begin
	TempList := TStringList.Create;
	TempList.AddStrings(self.CurrentAccountSettings.GetAccountsList());
	AccountsList.Items := TempList;
	TempList.Destroy;
	AccountsList.OnClick(self);
end;

procedure TAccountsForm.UpdateStreamingExtensionsList();
var
	TempList: TStringList;
begin
	TempList := TStringList.Create;
	GetStreamingExtensionsFromIniFile(self.PluginSettings.IniFileName, TempList); //todo: it is temporary solution while Settings unit isn't refactored
	StreamingExtensionsList.Items := TempList;
	TempList.Destroy;
end;

procedure TAccountsForm.AccountsListClick(Sender: TObject);
begin
	if (AccountsList.Items.Count > 0) and (AccountsList.ItemIndex <> -1) then
	begin

		CurrentAccountSettings.Account := AccountsList.Items[AccountsList.ItemIndex];
		AccountNameEdit.Text := CurrentAccountSettings.Account;
		EmailEdit.Text := CurrentAccountSettings.Email;
		PasswordEdit.Text := CurrentAccountSettings.Password;
		UseTCPwdMngrCB.Checked := CurrentAccountSettings.UseTCPasswordManager;
		UnlimitedFileSizeCB.Checked := CurrentAccountSettings.UnlimitedFilesize;
		SplitLargeFilesCB.Checked := CurrentAccountSettings.SplitLargeFiles;
		PublicAccountCB.Checked := CurrentAccountSettings.PublicAccount;
		PublicUrlEdit.Text := CurrentAccountSettings.PublicUrl;
		TwostepAuthCB.Checked := CurrentAccountSettings.TwostepAuth;
		EncryptFilesCombo.ItemIndex := CurrentAccountSettings.EncryptFilesMode;
		EncryptFilenamesCB.Checked := CurrentAccountSettings.EncryptFilenames;
		self.SelectedAccount := CurrentAccountSettings.Account;
		EncryptFilesComboChange(nil);
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
begin
	if (AccountNameEdit.Text = EmptyWideStr) then
		exit();
	CurrentAccountSettings.Account := AccountNameEdit.Text;
	CurrentAccountSettings.Email := EmailEdit.Text;
	CurrentAccountSettings.Password := PasswordEdit.Text;
	CurrentAccountSettings.UseTCPasswordManager := UseTCPwdMngrCB.Checked;
	CurrentAccountSettings.UnlimitedFilesize := UnlimitedFileSizeCB.Checked;
	CurrentAccountSettings.SplitLargeFiles := SplitLargeFilesCB.Checked;
	CurrentAccountSettings.TwostepAuth := TwostepAuthCB.Checked;
	CurrentAccountSettings.PublicAccount := PublicAccountCB.Checked;
	CurrentAccountSettings.PublicUrl := PublicUrlEdit.Text;
	CurrentAccountSettings.EncryptFilesMode := EncryptFilesCombo.ItemIndex;
	CurrentAccountSettings.EncryptFilenames := EncryptFilenamesCB.Checked;

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

	CurrentAccountSettings.Save;

	UpdateAccountsList();

end;

procedure TAccountsForm.ApplyExtButtonClick(Sender: TObject);
var
	StreamingOptions: TStreamingOptions;
begin
	StreamingOptions.Command := CommandPathEdit.Text;
	StreamingOptions.Parameters := ParametersEdit.Text;
	StreamingOptions.StartPath := StartPathEdit.Text;
	StreamingOptions.Format := StreamingTypeCombo.ItemIndex;

	SetStreamingOptionsToIniFile(self.PluginSettings.IniFileName, '.' + StreamingExtensionEdit.Text, StreamingOptions);
	UpdateStreamingExtensionsList();
end;

procedure TAccountsForm.ApplySettings;
begin
	if not CheckValidators() then
		exit;

	self.PluginSettings.SetSettingValue('LoadSSLDLLOnlyFromPluginDir', UseDLLFromPluginDir.Checked);
	self.PluginSettings.SetSettingValue('PreserveFileTime', PreserveFileTimeCB.Checked);

	self.PluginSettings.SetSettingValue('DescriptionEnabled', DescriptionEnabledCB.Checked);
	self.PluginSettings.SetSettingValue('DescriptionEditorEnabled', DescriptionEditorEnabledCB.Checked);
	self.PluginSettings.SetSettingValue('DescriptionCopyToCloud', DescriptionCopyToCloudCB.Checked);
	self.PluginSettings.SetSettingValue('DescriptionCopyFromCloud', DescriptionCopyFromCloudCB.Checked);
	self.PluginSettings.SetSettingValue('DescriptionTrackCloudFS', DescriptionTrackCloudFSCB.Checked);
	self.PluginSettings.SetSettingValue('DescriptionFileName', DescriptionFileNameEdit.Text);

	self.PluginSettings.SetSettingValue('CopyBetweenAccountsMode', CopyBetweenAccountsModeCombo.ItemIndex);

	if CloudMaxFileSizeCB.Checked then
	begin
		self.PluginSettings.SetSettingValue('CloudMaxFileSize', CloudMaxFileSizeValue.Text);
	end else begin
		self.PluginSettings.SetSettingValue('CloudMaxFileSize', null);
	end;

	self.PluginSettings.SetSettingValue('ChunkOverwriteMode', ChunkOverwriteModeCombo.ItemIndex);
	self.PluginSettings.SetSettingValue('DeleteFailOnUploadMode', DeleteFailOnUploadModeCombo.ItemIndex);
	self.PluginSettings.SetSettingValue('OverwriteLocalMode', OverwriteLocalModeCombo.ItemIndex);
	self.PluginSettings.SetSettingValue('OperationErrorMode', OperationErrorModeCombo.ItemIndex);
	self.PluginSettings.SetSettingValue('RetryAttempts', RetryAttemptsValue.Text);
	self.PluginSettings.SetSettingValue('AttemptWait', AttemptWaitValue.Text);

	self.PluginSettings.SetSettingValue('DisableMultiThreading', DisableMultiThreadingCB.Checked);
	self.PluginSettings.SetSettingValue('LogUserSpace', SpaceInfoLoggingCB.Checked);
	self.PluginSettings.SetSettingValue('IconsMode', IconsModeCombo.ItemIndex);

	self.PluginSettings.SetSettingValue('SocketTimeout', SocketTimeoutEdit.Text);
	self.PluginSettings.SetSettingValue('UploadBPS', UploadBPSEdit.Text);
	self.PluginSettings.SetSettingValue('DownloadBPS', DownloadBPSEdit.Text);
	self.PluginSettings.SetSettingValue('ProxyType', ProxyCB.ItemIndex);
	self.PluginSettings.SetSettingValue('ProxyServer', ProxyServerEdit.Text);
	self.PluginSettings.SetSettingValue('ProxyPort', ProxyPortEdit.Text);

	self.PluginSettings.SetSettingValue('ProxyUser', ProxyUserEdit.Text);
	self.PluginSettings.SetSettingValue('ProxyPassword', ProxyPwd.Text);
	self.PluginSettings.SetSettingValue('ProxyTCPwdMngr', ProxyTCPwdMngrCB.Checked);

	if ChangeUserAgentCB.Checked then
		self.PluginSettings.SetSettingValue('UserAgent', UserAgentEdit.Text);

	self.PluginSettings.SetSettingValue('DownloadLinksEncode', DownloadLinksEncodeCB.Checked);
	self.PluginSettings.SetSettingValue('AutoUpdateDownloadListing', AutoUpdateDownloadListingCB.Checked);

	self.PluginSettings.SetSettingValue('ShowTrashFolders', ShowTrashFoldersCB.Checked);
	self.PluginSettings.SetSettingValue('ShowSharedFolders', ShowSharedFoldersCB.Checked);
	self.PluginSettings.SetSettingValue('ShowInvitesFolders', ShowInvitesFoldersCB.Checked);
	self.PluginSettings.SetSettingValue('PrecalculateHash', PrecalculateHashCB.Checked);
	self.PluginSettings.SetSettingValue('CheckCRC', CheckCRCCB.Checked);

	if ProxyTCPwdMngrCB.Checked then //просим TC сохранить пароль
	begin
		case PasswordManager.SetPassword('proxy' + ProxyUserEdit.Text, ProxyPwd.Text) of
			FS_FILE_OK:
				begin //TC скушал пароль
					ProxyPwd.Text := EmptyWideStr;
					self.PluginSettings.SetSettingValue('ProxyPassword', EmptyWideStr);
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
		MessageBaloon.Description := ERR_ACCOUNT_HAS_INVALID_SYMBOL;
		MessageBaloon.ShowHint(DescriptionFileNameEdit);
		exit;
	end;
	exit(true);
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
		CurrentAccountSettings.DeleteAccount(AccountsList.Items[AccountsList.ItemIndex]);
		UpdateAccountsList();
	end;
end;

procedure TAccountsForm.DeleteExtButtonClick(Sender: TObject);
begin
	if (StreamingExtensionsList.Items.Count > 0) and (StreamingExtensionsList.ItemIndex <> -1) then
	begin
		DeleteStreamingExtensionsFromIniFile(self.PluginSettings.IniFileName, StreamingExtensionsList.Items[StreamingExtensionsList.ItemIndex]);
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
begin
	PasswordManager.parentWindow := self.Handle;
	CryptedGUID := PasswordManager.StoreFileCryptPassword(self.SelectedAccount);
	PasswordManager.parentWindow := FindTCWindow;
	if CryptedGUID <> EmptyWideStr then
		//    CurrentAccountSettings.Account:=self.SelectedAccount;   //should be already selected
		CurrentAccountSettings.SetSettingValue('CryptedGUID_files', CryptedGUID)
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
	UpdateStreamingExtensionsList();
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
begin
	if mrOk = TRegistrationForm.ShowRegistration(self.parentWindow, PluginSettings.ConnectionSettings, CurrentAccountSettings) then
	begin
		if CurrentAccountSettings.UseTCPasswordManager then //просим TC сохранить пароль
			if FS_FILE_OK <> PasswordManager.SetPassword(CurrentAccountSettings.Account, CurrentAccountSettings.Password) then
				exit(); //Не удалось сохранить пароль/нажали отмену

		CurrentAccountSettings.Save;
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

class procedure TAccountsForm.ShowAccounts(parentWindow: HWND; AccountSettings: TNewAccountSettings; Settings: TPluginSettings; PasswordManager: TTCPasswordManager; Account: WideString);
var
	AccountsForm: TAccountsForm;
begin
	try
		AccountsForm := TAccountsForm.Create(nil);
		AccountsForm.parentWindow := parentWindow;
		AccountsForm.CurrentAccountSettings := AccountSettings;
		AccountsForm.PluginSettings := Settings;
		AccountsForm.PasswordManager := PasswordManager;
		AccountsForm.SelectedAccount := EmptyWideStr;
		{global settings}
		AccountsForm.UseDLLFromPluginDir.Checked := AccountsForm.PluginSettings.LoadSSLDLLOnlyFromPluginDir;
		AccountsForm.PreserveFileTimeCB.Checked := AccountsForm.PluginSettings.PreserveFileTime;
		AccountsForm.DescriptionEnabledCB.Checked := AccountsForm.PluginSettings.DescriptionEnabled;
		AccountsForm.DescriptionEditorEnabledCB.Checked := AccountsForm.PluginSettings.DescriptionEditorEnabled;
		AccountsForm.DescriptionCopyToCloudCB.Checked := AccountsForm.PluginSettings.DescriptionCopyToCloud;
		AccountsForm.DescriptionCopyFromCloudCB.Checked := AccountsForm.PluginSettings.DescriptionCopyFromCloud;
		AccountsForm.DescriptionTrackCloudFSCB.Checked := AccountsForm.PluginSettings.DescriptionTrackCloudFS;
		AccountsForm.DescriptionFileNameEdit.Text := AccountsForm.PluginSettings.DescriptionFileName;

		AccountsForm.CopyBetweenAccountsModeCombo.ItemIndex := AccountsForm.PluginSettings.CopyBetweenAccountsMode;
		AccountsForm.SocketTimeoutEdit.Text := AccountsForm.PluginSettings.ConnectionSettings.SocketTimeout.ToString;
		AccountsForm.UploadBPSEdit.Text := AccountsForm.PluginSettings.ConnectionSettings.UploadBPS.ToString;
		AccountsForm.DownloadBPSEdit.Text := AccountsForm.PluginSettings.ConnectionSettings.DownloadBPS.ToString;

		AccountsForm.ProxyCB.ItemIndex := AccountsForm.PluginSettings.ConnectionSettings.ProxySettings.ProxyType;
		AccountsForm.ProxyServerEdit.Text := AccountsForm.PluginSettings.ConnectionSettings.ProxySettings.Server;
		AccountsForm.ProxyPortEdit.Text := AccountsForm.PluginSettings.ConnectionSettings.ProxySettings.Port.ToString;
		AccountsForm.ProxyUserEdit.Text := AccountsForm.PluginSettings.ConnectionSettings.ProxySettings.user;
		AccountsForm.ProxyPwd.Text := AccountsForm.PluginSettings.ConnectionSettings.ProxySettings.Password;
		AccountsForm.ProxyTCPwdMngrCB.Checked := AccountsForm.PluginSettings.ConnectionSettings.ProxySettings.use_tc_password_manager;

		AccountsForm.UserAgentEdit.Text := AccountsForm.PluginSettings.ConnectionSettings.UserAgent;
		AccountsForm.ChangeUserAgentCB.Checked := DEFAULT_USERAGENT <> AccountsForm.UserAgentEdit.Text;
		AccountsForm.UserAgentEdit.ReadOnly := not AccountsForm.ChangeUserAgentCB.Checked;

		AccountsForm.CloudMaxFileSizeValue.Text := AccountsForm.PluginSettings.CloudMaxFileSize.ToString;

		if (AccountsForm.PluginSettings.CloudMaxFileSize <> CLOUD_MAX_FILESIZE_DEFAULT) then
		begin
			AccountsForm.CloudMaxFileSizeValue.Enabled := true;
			AccountsForm.CloudMaxFileSizeCB.Checked := true;
		end;
		AccountsForm.ChunkOverwriteModeCombo.ItemIndex := AccountsForm.PluginSettings.ChunkOverwriteMode;
		AccountsForm.DeleteFailOnUploadModeCombo.ItemIndex := AccountsForm.PluginSettings.DeleteFailOnUploadMode;
		AccountsForm.OverwriteLocalModeCombo.ItemIndex := AccountsForm.PluginSettings.OverwriteLocalMode;
		AccountsForm.OperationErrorModeCombo.ItemIndex := AccountsForm.PluginSettings.OperationErrorMode;
		AccountsForm.RetryAttemptsValue.Text := AccountsForm.PluginSettings.RetryAttempts.ToString;
		AccountsForm.AttemptWaitValue.Text := AccountsForm.PluginSettings.AttemptWait.ToString;

		AccountsForm.DisableMultiThreadingCB.Checked := AccountsForm.PluginSettings.DisableMultiThreading;
		AccountsForm.SpaceInfoLoggingCB.Checked := AccountsForm.PluginSettings.LogUserSpace;
		AccountsForm.IconsModeCombo.ItemIndex := AccountsForm.PluginSettings.IconsMode;

		AccountsForm.DownloadLinksEncodeCB.Checked := AccountsForm.PluginSettings.DownloadLinksEncode;
		AccountsForm.AutoUpdateDownloadListingCB.Checked := AccountsForm.PluginSettings.AutoUpdateDownloadListing;
		AccountsForm.ShowTrashFoldersCB.Checked := AccountsForm.PluginSettings.ShowTrashFolders;
		AccountsForm.ShowSharedFoldersCB.Checked := AccountsForm.PluginSettings.ShowSharedFolders;
		AccountsForm.ShowInvitesFoldersCB.Checked := AccountsForm.PluginSettings.ShowInvitesFolders;
		AccountsForm.PrecalculateHashCB.Checked := AccountsForm.PluginSettings.PrecalculateHash;
		AccountsForm.CheckCRCCB.Checked := AccountsForm.PluginSettings.CheckCRC;

		{global settings}
		if Account <> EmptyWideStr then
			AccountsForm.SelectedAccount := Account;
		AccountsForm.OptionPages.ActivePageIndex := 0;
		AccountsForm.ShowModal;
	finally
		FreeAndNil(AccountsForm);
	end;
end;

procedure TAccountsForm.StreamingExtensionsListClick(Sender: TObject);
var
	StreamingOptions: TStreamingOptions;
begin
	if (StreamingExtensionsList.Items.Count > 0) and (StreamingExtensionsList.ItemIndex <> -1) then
	begin
		GetStreamingOptionsFromIniFile(PluginSettings.IniFileName, '.' + StreamingExtensionsList.Items[StreamingExtensionsList.ItemIndex], StreamingOptions); //не проверяем результат, это настройки
		StreamingExtensionEdit.Text := StreamingExtensionsList.Items[StreamingExtensionsList.ItemIndex];
		CommandPathEdit.Text := StreamingOptions.Command;
		ParametersEdit.Text := StreamingOptions.Parameters;
		StartPathEdit.Text := StreamingOptions.StartPath;
		StreamingTypeCombo.ItemIndex := StreamingOptions.Format;
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
