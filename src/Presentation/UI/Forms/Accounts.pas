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
	LANGUAGE_STRINGS,
	WSList,
	StreamingSettings,
	PluginSettingsManager,
	FileCipher,
	AccountSettings,
	ConnectionSettings,
	AccountsManager,
	AccountsPresenter;

type
	TAccountsForm = class(TForm, IAccountsView)
		OptionPages: TPageControl;
		AccountsTab: TTabSheet;
		GlobalTab: TTabSheet;
		AccountsGroupBox: TGroupBox;
		AccountsList: TListBox;
		ApplyButton: TButton;
		DeleteButton: TButton;
		PreserveFileTimeCB: TCheckBox;
		UseDLLFromPluginDir: TCheckBox;
		GlobalSettingsApplyBtn: TButton;
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
		NetworkSettingsApplyBtn: TButton;
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
		CommentsSettingsApplyBtn: TButton;
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
		PrecalculateHashStrategyCombo: TComboBox;
		procedure FormShow(Sender: TObject);
		procedure AccountsListClick(Sender: TObject);
		procedure ApplyButtonClick(Sender: TObject);
		procedure DeleteButtonClick(Sender: TObject);
		procedure AccountsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		class function ShowAccounts(ParentWindow: HWND; PasswordManager: IPasswordManager; Account: WideString): Boolean;
		procedure FormActivate(Sender: TObject);
		procedure ProxyUserEditChange(Sender: TObject);
		procedure GlobalSettingsApplyBtnClick(Sender: TObject);
		procedure PublicAccountCBClick(Sender: TObject);
		procedure CloudMaxFileSizeCBClick(Sender: TObject);
		procedure EncryptFilesComboChange(Sender: TObject);
		procedure EncryptFilesPwdButtonClick(Sender: TObject);
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure StreamingExtensionsListClick(Sender: TObject);
		procedure StreamingExtensionsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure DeleteExtButtonClick(Sender: TObject);
		procedure ApplyExtButtonClick(Sender: TObject);
		procedure CommandPathButtonClick(Sender: TObject);
		procedure ChangeUserAgentCBClick(Sender: TObject);
	private
		FPresenter: TAccountsPresenter;

		{IAccountsView - Account tab controls}
		procedure SetAccountsList(Accounts: TStrings);
		function GetSelectedAccountIndex: Integer;
		function GetSelectedAccountName: WideString;
		procedure SelectAccount(Index: Integer);
		procedure SetAccountName(Value: WideString);
		function GetAccountName: WideString;
		procedure SetEmail(Value: WideString);
		function GetEmail: WideString;
		procedure SetPassword(Value: WideString);
		function GetPassword: WideString;
		procedure SetUseTCPasswordManager(Value: Boolean);
		function GetUseTCPasswordManager: Boolean;
		procedure SetUnlimitedFileSize(Value: Boolean);
		function GetUnlimitedFileSize: Boolean;
		procedure SetSplitLargeFiles(Value: Boolean);
		function GetSplitLargeFiles: Boolean;
		procedure SetPublicAccount(Value: Boolean);
		function GetPublicAccount: Boolean;
		procedure SetPublicUrl(Value: WideString);
		function GetPublicUrl: WideString;
		procedure SetEncryptFilesMode(Value: Integer);
		function GetEncryptFilesMode: Integer;
		procedure SetEncryptFilenames(Value: Boolean);
		function GetEncryptFilenames: Boolean;
		procedure SetEncryptPasswordButtonEnabled(Value: Boolean);
		procedure SetAccountsPanelVisible(Value: Boolean);
		procedure SetSharesPanelVisible(Value: Boolean);

		{IAccountsView - Global settings}
		procedure SetLoadSSLFromPluginDir(Value: Boolean);
		function GetLoadSSLFromPluginDir: Boolean;
		procedure SetPreserveFileTime(Value: Boolean);
		function GetPreserveFileTime: Boolean;
		procedure SetCloudMaxFileSize(Value: Integer);
		function GetCloudMaxFileSize: Integer;
		procedure SetCloudMaxFileSizeEnabled(Value: Boolean);
		function GetCloudMaxFileSizeEnabled: Boolean;
		procedure SetCloudMaxFileSizeEditEnabled(Value: Boolean);
		procedure SetChunkOverwriteMode(Value: Integer);
		function GetChunkOverwriteMode: Integer;
		procedure SetDeleteFailOnUploadMode(Value: Integer);
		function GetDeleteFailOnUploadMode: Integer;
		procedure SetOverwriteLocalMode(Value: Integer);
		function GetOverwriteLocalMode: Integer;
		procedure SetOperationErrorMode(Value: Integer);
		function GetOperationErrorMode: Integer;
		procedure SetRetryAttempts(Value: Integer);
		function GetRetryAttempts: Integer;
		procedure SetAttemptWait(Value: Integer);
		function GetAttemptWait: Integer;
		procedure SetDisableMultiThreading(Value: Boolean);
		function GetDisableMultiThreading: Boolean;
		procedure SetLogUserSpace(Value: Boolean);
		function GetLogUserSpace: Boolean;
		procedure SetIconsMode(Value: Integer);
		function GetIconsMode: Integer;
		procedure SetCopyBetweenAccountsMode(Value: Integer);
		function GetCopyBetweenAccountsMode: Integer;
		procedure SetDownloadLinksEncode(Value: Boolean);
		function GetDownloadLinksEncode: Boolean;
		procedure SetAutoUpdateDownloadListing(Value: Boolean);
		function GetAutoUpdateDownloadListing: Boolean;
		procedure SetShowTrashFolders(Value: Boolean);
		function GetShowTrashFolders: Boolean;
		procedure SetShowSharedFolders(Value: Boolean);
		function GetShowSharedFolders: Boolean;
		procedure SetShowInvitesFolders(Value: Boolean);
		function GetShowInvitesFolders: Boolean;
		procedure SetPrecalculateHash(Value: Boolean);
		function GetPrecalculateHash: Boolean;
		procedure SetCheckCRC(Value: Boolean);
		function GetCheckCRC: Boolean;
		procedure SetHashCalculatorStrategy(Value: Integer);
		function GetHashCalculatorStrategy: Integer;

		{IAccountsView - Network settings}
		procedure SetSocketTimeout(Value: Integer);
		function GetSocketTimeout: Integer;
		procedure SetUploadBPS(Value: Integer);
		function GetUploadBPS: Integer;
		procedure SetDownloadBPS(Value: Integer);
		function GetDownloadBPS: Integer;
		procedure SetProxyType(Value: Integer);
		function GetProxyType: Integer;
		procedure SetProxyServer(Value: WideString);
		function GetProxyServer: WideString;
		procedure SetProxyPort(Value: Integer);
		function GetProxyPort: Integer;
		procedure SetProxyUser(Value: WideString);
		function GetProxyUser: WideString;
		procedure SetProxyPassword(Value: WideString);
		function GetProxyPassword: WideString;
		procedure SetProxyUseTCPasswordManager(Value: Boolean);
		function GetProxyUseTCPasswordManager: Boolean;
		procedure SetProxyTCPasswordManagerEnabled(Value: Boolean);
		procedure SetUserAgent(Value: WideString);
		function GetUserAgent: WideString;
		procedure SetChangeUserAgent(Value: Boolean);
		function GetChangeUserAgent: Boolean;
		procedure SetUserAgentReadOnly(Value: Boolean);

		{IAccountsView - Description settings}
		procedure SetDescriptionEnabled(Value: Boolean);
		function GetDescriptionEnabled: Boolean;
		procedure SetDescriptionEditorEnabled(Value: Boolean);
		function GetDescriptionEditorEnabled: Boolean;
		procedure SetDescriptionCopyToCloud(Value: Boolean);
		function GetDescriptionCopyToCloud: Boolean;
		procedure SetDescriptionCopyFromCloud(Value: Boolean);
		function GetDescriptionCopyFromCloud: Boolean;
		procedure SetDescriptionTrackCloudFS(Value: Boolean);
		function GetDescriptionTrackCloudFS: Boolean;
		procedure SetDescriptionFileName(Value: WideString);
		function GetDescriptionFileName: WideString;

		{IAccountsView - Streaming extensions}
		procedure SetStreamingExtensionsList(Extensions: TStrings);
		function GetSelectedStreamingExtensionIndex: Integer;
		function GetSelectedStreamingExtension: WideString;
		procedure SetStreamingExtension(Value: WideString);
		function GetStreamingExtension: WideString;
		procedure SetStreamingCommand(Value: WideString);
		function GetStreamingCommand: WideString;
		procedure SetStreamingParameters(Value: WideString);
		function GetStreamingParameters: WideString;
		procedure SetStreamingStartPath(Value: WideString);
		function GetStreamingStartPath: WideString;
		procedure SetStreamingType(Value: Integer);
		function GetStreamingType: Integer;
		procedure ClearStreamingFields;

		{IAccountsView - UI actions}
		procedure ShowValidationError(ControlName: WideString; Message: WideString);
		procedure ShowTab(TabIndex: Integer);
		function GetFormHandle: THandle;

		{IAccountsView - Dialogs}
		function ShowEncryptionPasswordDialog(const AccountName: WideString; var CryptedGUID: WideString): Boolean;
	public

	end;

implementation

{$R *.dfm}

uses
	ProxySettings,
	CMRConstants,
	IniConfigFile;

{IAccountsView - Account tab controls}

procedure TAccountsForm.SetAccountsList(Accounts: TStrings);
begin
	AccountsList.Items.Assign(Accounts);
end;

function TAccountsForm.GetSelectedAccountIndex: Integer;
begin
	Result := AccountsList.ItemIndex;
end;

function TAccountsForm.GetSelectedAccountName: WideString;
begin
	if (AccountsList.Items.Count > 0) and (AccountsList.ItemIndex >= 0) then
		Result := AccountsList.Items[AccountsList.ItemIndex]
	else
		Result := '';
end;

procedure TAccountsForm.SelectAccount(Index: Integer);
begin
	if (Index >= 0) and (Index < AccountsList.Items.Count) then
		AccountsList.Selected[Index] := True;
end;

procedure TAccountsForm.SetAccountName(Value: WideString);
begin
	AccountNameEdit.Text := Value;
end;

function TAccountsForm.GetAccountName: WideString;
begin
	Result := AccountNameEdit.Text;
end;

procedure TAccountsForm.SetEmail(Value: WideString);
begin
	EmailEdit.Text := Value;
end;

function TAccountsForm.GetEmail: WideString;
begin
	Result := EmailEdit.Text;
end;

procedure TAccountsForm.SetPassword(Value: WideString);
begin
	PasswordEdit.Text := Value;
end;

function TAccountsForm.GetPassword: WideString;
begin
	Result := PasswordEdit.Text;
end;

procedure TAccountsForm.SetUseTCPasswordManager(Value: Boolean);
begin
	UseTCPwdMngrCB.Checked := Value;
end;

function TAccountsForm.GetUseTCPasswordManager: Boolean;
begin
	Result := UseTCPwdMngrCB.Checked;
end;

procedure TAccountsForm.SetUnlimitedFileSize(Value: Boolean);
begin
	UnlimitedFileSizeCB.Checked := Value;
end;

function TAccountsForm.GetUnlimitedFileSize: Boolean;
begin
	Result := UnlimitedFileSizeCB.Checked;
end;

procedure TAccountsForm.SetSplitLargeFiles(Value: Boolean);
begin
	SplitLargeFilesCB.Checked := Value;
end;

function TAccountsForm.GetSplitLargeFiles: Boolean;
begin
	Result := SplitLargeFilesCB.Checked;
end;

procedure TAccountsForm.SetPublicAccount(Value: Boolean);
begin
	PublicAccountCB.Checked := Value;
end;

function TAccountsForm.GetPublicAccount: Boolean;
begin
	Result := PublicAccountCB.Checked;
end;

procedure TAccountsForm.SetPublicUrl(Value: WideString);
begin
	PublicUrlEdit.Text := Value;
end;

function TAccountsForm.GetPublicUrl: WideString;
begin
	Result := PublicUrlEdit.Text;
end;

procedure TAccountsForm.SetEncryptFilesMode(Value: Integer);
begin
	EncryptFilesCombo.ItemIndex := Value;
end;

function TAccountsForm.GetEncryptFilesMode: Integer;
begin
	Result := EncryptFilesCombo.ItemIndex;
end;

procedure TAccountsForm.SetEncryptFilenames(Value: Boolean);
begin
	EncryptFilenamesCB.Checked := Value;
end;

function TAccountsForm.GetEncryptFilenames: Boolean;
begin
	Result := EncryptFilenamesCB.Checked;
end;

procedure TAccountsForm.SetEncryptPasswordButtonEnabled(Value: Boolean);
begin
	EncryptFilesPwdButton.Enabled := Value;
end;

procedure TAccountsForm.SetAccountsPanelVisible(Value: Boolean);
begin
	AccountsPanel.Visible := Value;
end;

procedure TAccountsForm.SetSharesPanelVisible(Value: Boolean);
begin
	SharesPanel.Visible := Value;
end;

{IAccountsView - Global settings}

procedure TAccountsForm.SetLoadSSLFromPluginDir(Value: Boolean);
begin
	UseDLLFromPluginDir.Checked := Value;
end;

function TAccountsForm.GetLoadSSLFromPluginDir: Boolean;
begin
	Result := UseDLLFromPluginDir.Checked;
end;

procedure TAccountsForm.SetPreserveFileTime(Value: Boolean);
begin
	PreserveFileTimeCB.Checked := Value;
end;

function TAccountsForm.GetPreserveFileTime: Boolean;
begin
	Result := PreserveFileTimeCB.Checked;
end;

procedure TAccountsForm.SetCloudMaxFileSize(Value: Integer);
begin
	CloudMaxFileSizeValue.Text := IntToStr(Value);
end;

function TAccountsForm.GetCloudMaxFileSize: Integer;
begin
	Result := StrToIntDef(CloudMaxFileSizeValue.Text, CLOUD_MAX_FILESIZE_DEFAULT);
end;

procedure TAccountsForm.SetCloudMaxFileSizeEnabled(Value: Boolean);
begin
	CloudMaxFileSizeCB.Checked := Value;
end;

function TAccountsForm.GetCloudMaxFileSizeEnabled: Boolean;
begin
	Result := CloudMaxFileSizeCB.Checked;
end;

procedure TAccountsForm.SetCloudMaxFileSizeEditEnabled(Value: Boolean);
begin
	CloudMaxFileSizeValue.Enabled := Value;
end;

procedure TAccountsForm.SetChunkOverwriteMode(Value: Integer);
begin
	ChunkOverwriteModeCombo.ItemIndex := Value;
end;

function TAccountsForm.GetChunkOverwriteMode: Integer;
begin
	Result := ChunkOverwriteModeCombo.ItemIndex;
end;

procedure TAccountsForm.SetDeleteFailOnUploadMode(Value: Integer);
begin
	DeleteFailOnUploadModeCombo.ItemIndex := Value;
end;

function TAccountsForm.GetDeleteFailOnUploadMode: Integer;
begin
	Result := DeleteFailOnUploadModeCombo.ItemIndex;
end;

procedure TAccountsForm.SetOverwriteLocalMode(Value: Integer);
begin
	OverwriteLocalModeCombo.ItemIndex := Value;
end;

function TAccountsForm.GetOverwriteLocalMode: Integer;
begin
	Result := OverwriteLocalModeCombo.ItemIndex;
end;

procedure TAccountsForm.SetOperationErrorMode(Value: Integer);
begin
	OperationErrorModeCombo.ItemIndex := Value;
end;

function TAccountsForm.GetOperationErrorMode: Integer;
begin
	Result := OperationErrorModeCombo.ItemIndex;
end;

procedure TAccountsForm.SetRetryAttempts(Value: Integer);
begin
	RetryAttemptsValue.Text := IntToStr(Value);
end;

function TAccountsForm.GetRetryAttempts: Integer;
begin
	Result := StrToIntDef(RetryAttemptsValue.Text, 3);
end;

procedure TAccountsForm.SetAttemptWait(Value: Integer);
begin
	AttemptWaitValue.Text := IntToStr(Value);
end;

function TAccountsForm.GetAttemptWait: Integer;
begin
	Result := StrToIntDef(AttemptWaitValue.Text, 1000);
end;

procedure TAccountsForm.SetDisableMultiThreading(Value: Boolean);
begin
	DisableMultiThreadingCB.Checked := Value;
end;

function TAccountsForm.GetDisableMultiThreading: Boolean;
begin
	Result := DisableMultiThreadingCB.Checked;
end;

procedure TAccountsForm.SetLogUserSpace(Value: Boolean);
begin
	SpaceInfoLoggingCB.Checked := Value;
end;

function TAccountsForm.GetLogUserSpace: Boolean;
begin
	Result := SpaceInfoLoggingCB.Checked;
end;

procedure TAccountsForm.SetIconsMode(Value: Integer);
begin
	IconsModeCombo.ItemIndex := Value;
end;

function TAccountsForm.GetIconsMode: Integer;
begin
	Result := IconsModeCombo.ItemIndex;
end;

procedure TAccountsForm.SetCopyBetweenAccountsMode(Value: Integer);
begin
	CopyBetweenAccountsModeCombo.ItemIndex := Value;
end;

function TAccountsForm.GetCopyBetweenAccountsMode: Integer;
begin
	Result := CopyBetweenAccountsModeCombo.ItemIndex;
end;

procedure TAccountsForm.SetDownloadLinksEncode(Value: Boolean);
begin
	DownloadLinksEncodeCB.Checked := Value;
end;

function TAccountsForm.GetDownloadLinksEncode: Boolean;
begin
	Result := DownloadLinksEncodeCB.Checked;
end;

procedure TAccountsForm.SetAutoUpdateDownloadListing(Value: Boolean);
begin
	AutoUpdateDownloadListingCB.Checked := Value;
end;

function TAccountsForm.GetAutoUpdateDownloadListing: Boolean;
begin
	Result := AutoUpdateDownloadListingCB.Checked;
end;

procedure TAccountsForm.SetShowTrashFolders(Value: Boolean);
begin
	ShowTrashFoldersCB.Checked := Value;
end;

function TAccountsForm.GetShowTrashFolders: Boolean;
begin
	Result := ShowTrashFoldersCB.Checked;
end;

procedure TAccountsForm.SetShowSharedFolders(Value: Boolean);
begin
	ShowSharedFoldersCB.Checked := Value;
end;

function TAccountsForm.GetShowSharedFolders: Boolean;
begin
	Result := ShowSharedFoldersCB.Checked;
end;

procedure TAccountsForm.SetShowInvitesFolders(Value: Boolean);
begin
	ShowInvitesFoldersCB.Checked := Value;
end;

function TAccountsForm.GetShowInvitesFolders: Boolean;
begin
	Result := ShowInvitesFoldersCB.Checked;
end;

procedure TAccountsForm.SetPrecalculateHash(Value: Boolean);
begin
	PrecalculateHashCB.Checked := Value;
end;

function TAccountsForm.GetPrecalculateHash: Boolean;
begin
	Result := PrecalculateHashCB.Checked;
end;

procedure TAccountsForm.SetCheckCRC(Value: Boolean);
begin
	CheckCRCCB.Checked := Value;
end;

function TAccountsForm.GetCheckCRC: Boolean;
begin
	Result := CheckCRCCB.Checked;
end;

procedure TAccountsForm.SetHashCalculatorStrategy(Value: Integer);
begin
	PrecalculateHashStrategyCombo.ItemIndex := Value;
end;

function TAccountsForm.GetHashCalculatorStrategy: Integer;
begin
	Result := PrecalculateHashStrategyCombo.ItemIndex;
end;

{IAccountsView - Network settings}

procedure TAccountsForm.SetSocketTimeout(Value: Integer);
begin
	SocketTimeoutEdit.Text := IntToStr(Value);
end;

function TAccountsForm.GetSocketTimeout: Integer;
begin
	Result := StrToIntDef(SocketTimeoutEdit.Text, 30000);
end;

procedure TAccountsForm.SetUploadBPS(Value: Integer);
begin
	UploadBPSEdit.Text := IntToStr(Value);
end;

function TAccountsForm.GetUploadBPS: Integer;
begin
	Result := StrToIntDef(UploadBPSEdit.Text, 0);
end;

procedure TAccountsForm.SetDownloadBPS(Value: Integer);
begin
	DownloadBPSEdit.Text := IntToStr(Value);
end;

function TAccountsForm.GetDownloadBPS: Integer;
begin
	Result := StrToIntDef(DownloadBPSEdit.Text, 0);
end;

procedure TAccountsForm.SetProxyType(Value: Integer);
begin
	ProxyCB.ItemIndex := Value;
end;

function TAccountsForm.GetProxyType: Integer;
begin
	Result := ProxyCB.ItemIndex;
end;

procedure TAccountsForm.SetProxyServer(Value: WideString);
begin
	ProxyServerEdit.Text := Value;
end;

function TAccountsForm.GetProxyServer: WideString;
begin
	Result := ProxyServerEdit.Text;
end;

procedure TAccountsForm.SetProxyPort(Value: Integer);
begin
	ProxyPortEdit.Text := IntToStr(Value);
end;

function TAccountsForm.GetProxyPort: Integer;
begin
	Result := StrToIntDef(ProxyPortEdit.Text, 0);
end;

procedure TAccountsForm.SetProxyUser(Value: WideString);
begin
	ProxyUserEdit.Text := Value;
end;

function TAccountsForm.GetProxyUser: WideString;
begin
	Result := ProxyUserEdit.Text;
end;

procedure TAccountsForm.SetProxyPassword(Value: WideString);
begin
	ProxyPwd.Text := Value;
end;

function TAccountsForm.GetProxyPassword: WideString;
begin
	Result := ProxyPwd.Text;
end;

procedure TAccountsForm.SetProxyUseTCPasswordManager(Value: Boolean);
begin
	ProxyTCPwdMngrCB.Checked := Value;
end;

function TAccountsForm.GetProxyUseTCPasswordManager: Boolean;
begin
	Result := ProxyTCPwdMngrCB.Checked;
end;

procedure TAccountsForm.SetProxyTCPasswordManagerEnabled(Value: Boolean);
begin
	ProxyTCPwdMngrCB.Enabled := Value;
end;

procedure TAccountsForm.SetUserAgent(Value: WideString);
begin
	UserAgentEdit.Text := Value;
end;

function TAccountsForm.GetUserAgent: WideString;
begin
	Result := UserAgentEdit.Text;
end;

procedure TAccountsForm.SetChangeUserAgent(Value: Boolean);
begin
	ChangeUserAgentCB.Checked := Value;
end;

function TAccountsForm.GetChangeUserAgent: Boolean;
begin
	Result := ChangeUserAgentCB.Checked;
end;

procedure TAccountsForm.SetUserAgentReadOnly(Value: Boolean);
begin
	UserAgentEdit.ReadOnly := Value;
end;

{IAccountsView - Description settings}

procedure TAccountsForm.SetDescriptionEnabled(Value: Boolean);
begin
	DescriptionEnabledCB.Checked := Value;
end;

function TAccountsForm.GetDescriptionEnabled: Boolean;
begin
	Result := DescriptionEnabledCB.Checked;
end;

procedure TAccountsForm.SetDescriptionEditorEnabled(Value: Boolean);
begin
	DescriptionEditorEnabledCB.Checked := Value;
end;

function TAccountsForm.GetDescriptionEditorEnabled: Boolean;
begin
	Result := DescriptionEditorEnabledCB.Checked;
end;

procedure TAccountsForm.SetDescriptionCopyToCloud(Value: Boolean);
begin
	DescriptionCopyToCloudCB.Checked := Value;
end;

function TAccountsForm.GetDescriptionCopyToCloud: Boolean;
begin
	Result := DescriptionCopyToCloudCB.Checked;
end;

procedure TAccountsForm.SetDescriptionCopyFromCloud(Value: Boolean);
begin
	DescriptionCopyFromCloudCB.Checked := Value;
end;

function TAccountsForm.GetDescriptionCopyFromCloud: Boolean;
begin
	Result := DescriptionCopyFromCloudCB.Checked;
end;

procedure TAccountsForm.SetDescriptionTrackCloudFS(Value: Boolean);
begin
	DescriptionTrackCloudFSCB.Checked := Value;
end;

function TAccountsForm.GetDescriptionTrackCloudFS: Boolean;
begin
	Result := DescriptionTrackCloudFSCB.Checked;
end;

procedure TAccountsForm.SetDescriptionFileName(Value: WideString);
begin
	DescriptionFileNameEdit.Text := Value;
end;

function TAccountsForm.GetDescriptionFileName: WideString;
begin
	Result := DescriptionFileNameEdit.Text;
end;

{IAccountsView - Streaming extensions}

procedure TAccountsForm.SetStreamingExtensionsList(Extensions: TStrings);
begin
	StreamingExtensionsList.Items.Assign(Extensions);
end;

function TAccountsForm.GetSelectedStreamingExtensionIndex: Integer;
begin
	Result := StreamingExtensionsList.ItemIndex;
end;

function TAccountsForm.GetSelectedStreamingExtension: WideString;
begin
	if (StreamingExtensionsList.Items.Count > 0) and (StreamingExtensionsList.ItemIndex >= 0) then
		Result := StreamingExtensionsList.Items[StreamingExtensionsList.ItemIndex]
	else
		Result := '';
end;

procedure TAccountsForm.SetStreamingExtension(Value: WideString);
begin
	StreamingExtensionEdit.Text := Value;
end;

function TAccountsForm.GetStreamingExtension: WideString;
begin
	Result := StreamingExtensionEdit.Text;
end;

procedure TAccountsForm.SetStreamingCommand(Value: WideString);
begin
	CommandPathEdit.Text := Value;
end;

function TAccountsForm.GetStreamingCommand: WideString;
begin
	Result := CommandPathEdit.Text;
end;

procedure TAccountsForm.SetStreamingParameters(Value: WideString);
begin
	ParametersEdit.Text := Value;
end;

function TAccountsForm.GetStreamingParameters: WideString;
begin
	Result := ParametersEdit.Text;
end;

procedure TAccountsForm.SetStreamingStartPath(Value: WideString);
begin
	StartPathEdit.Text := Value;
end;

function TAccountsForm.GetStreamingStartPath: WideString;
begin
	Result := StartPathEdit.Text;
end;

procedure TAccountsForm.SetStreamingType(Value: Integer);
begin
	StreamingTypeCombo.ItemIndex := Value;
end;

function TAccountsForm.GetStreamingType: Integer;
begin
	Result := StreamingTypeCombo.ItemIndex;
end;

procedure TAccountsForm.ClearStreamingFields;
begin
	StreamingExtensionEdit.Text := '';
	CommandPathEdit.Text := '';
	ParametersEdit.Text := '';
	StartPathEdit.Text := '';
	StreamingTypeCombo.ItemIndex := 0;
end;

{IAccountsView - UI actions}

procedure TAccountsForm.ShowValidationError(ControlName: WideString; Message: WideString);
var
	MessageBaloon: TBalloonHint;
	TargetControl: TControl;
begin
	TargetControl := nil;
	if ControlName = 'DescriptionFileName' then
		TargetControl := DescriptionFileNameEdit;

	if Assigned(TargetControl) then
	begin
		MessageBaloon := TBalloonHint.Create(Self);
		try
			MessageBaloon.HideAfter := 5000;
			MessageBaloon.Delay := 0;
			MessageBaloon.Description := Message;
			MessageBaloon.ShowHint(TargetControl);
		finally
			{Note: TBalloonHint manages its own lifetime}
		end;
	end;
end;

procedure TAccountsForm.ShowTab(TabIndex: Integer);
begin
	if (TabIndex >= 0) and (TabIndex < OptionPages.PageCount) then
		OptionPages.ActivePageIndex := TabIndex;
end;

function TAccountsForm.GetFormHandle: THandle;
begin
	Result := Self.Handle;
end;

{Event handlers - delegate to presenter}

procedure TAccountsForm.AccountsListClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnAccountSelected;
end;

procedure TAccountsForm.AccountsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_DELETE then
		DeleteButtonClick(nil);
end;

procedure TAccountsForm.ApplyButtonClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnApplyAccountClick;
end;

procedure TAccountsForm.ApplyExtButtonClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnApplyStreamingExtensionClick;
end;

procedure TAccountsForm.ChangeUserAgentCBClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnChangeUserAgentChanged;
end;

procedure TAccountsForm.CloudMaxFileSizeCBClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnCloudMaxFileSizeCheckChanged;
end;

procedure TAccountsForm.CommandPathButtonClick(Sender: TObject);
var
	InitDir: string;
begin
	InitDir := ExtractUniversalFilePath(CommandPathEdit.Text);
	if InitDir = '' then
		InitDir := TPath.GetSharedDocumentsPath; {Fallback to known location when edit is empty}
	CommandPathOpenDialog.InitialDir := InitDir;
	if CommandPathOpenDialog.Execute(Self.ParentWindow) then
		CommandPathEdit.Text := CommandPathOpenDialog.FileName;
end;

procedure TAccountsForm.DeleteButtonClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnDeleteAccountClick;
end;

procedure TAccountsForm.DeleteExtButtonClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnDeleteStreamingExtensionClick;
end;

procedure TAccountsForm.EncryptFilesComboChange(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnEncryptModeChanged;
end;

procedure TAccountsForm.EncryptFilesPwdButtonClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnEncryptPasswordClick;
end;

procedure TAccountsForm.FormActivate(Sender: TObject);
begin
	CenterWindow(Self.ParentWindow, Self.Handle);
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
	AccountsList.SetFocus;
end;

procedure TAccountsForm.GlobalSettingsApplyBtnClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnApplyGlobalSettingsClick;
end;

procedure TAccountsForm.ProxyUserEditChange(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnProxyUserChanged;
end;

procedure TAccountsForm.PublicAccountCBClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnPublicAccountChanged;
end;

procedure TAccountsForm.StreamingExtensionsListClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnStreamingExtensionSelected;
end;

procedure TAccountsForm.StreamingExtensionsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_DELETE then
		DeleteExtButton.OnClick(nil);
end;

{IAccountsView - Dialogs}

function TAccountsForm.ShowEncryptionPasswordDialog(const AccountName: WideString; var CryptedGUID: WideString): Boolean;
var
	CurrentPassword: WideString;
	CryptId: WideString;
	Verb: WideString;
	StorePassword: Boolean;
	PasswordMgr: IPasswordManager;
begin
	Result := False;
	CryptedGUID := '';
	StorePassword := True;
	CryptId := AccountName + PASSWORD_SUFFIX_FILECRYPT;

	{Get password manager from presenter}
	PasswordMgr := FPresenter.PasswordManager;
	if not Assigned(PasswordMgr) then
		Exit;

	case PasswordMgr.GetPassword(CryptId, CurrentPassword) of
		FS_FILE_OK:
			Verb := VERB_UPDATE;
		FS_FILE_READERROR:
			Verb := VERB_SET;
		else
			Exit;
	end;

	if mrOk = TAskPasswordForm.AskPassword(Format(ASK_ENCRYPTION_PASSWORD, [Verb]), PREFIX_ASK_NEW_PASSWORD, CurrentPassword, StorePassword, True, Self.Handle) then
	begin
		PasswordMgr.SetPassword(CryptId, CurrentPassword);
		CryptedGUID := TFileCipher.GetCryptedGUID(CurrentPassword);
		Result := True;
	end;
end;

{ShowAccounts factory method}

class function TAccountsForm.ShowAccounts(ParentWindow: HWND; PasswordManager: IPasswordManager; Account: WideString): Boolean;
var
	Form: TAccountsForm;
	SettingsManager: IPluginSettingsManager;
	AccountsMgr: IAccountsManager;
	Config: TAccountsPresenterConfig;
	PluginSettingsMgr: TPluginSettingsManager;
begin
	Form := TAccountsForm.Create(nil);
	try
		Form.ParentWindow := ParentWindow;

		{Create managers - interface reference counting handles cleanup}
		PluginSettingsMgr := TPluginSettingsManager.Create();
		SettingsManager := PluginSettingsMgr;
		AccountsMgr := TAccountsManager.Create(TIniConfigFile.Create(PluginSettingsMgr.AccountsIniFilePath));

		{Create presenter config}
		Config.PasswordManager := PasswordManager;
		Config.ParentWindow := ParentWindow;

		{Create presenter}
		Form.FPresenter := TAccountsPresenter.Create(Form, AccountsMgr, SettingsManager, Config);

		try
			{Initialize presenter - loads all settings to view}
			Form.FPresenter.Initialize(Account);

			{Show dialog}
			Form.OptionPages.ActivePageIndex := 0;
			Form.ShowModal;

			Result := Form.FPresenter.SettingsApplied;
		finally
			Form.FPresenter.Free;
			{Interface references are released automatically when they go out of scope}
		end;
	finally
		Form.Free;
	end;
end;

end.
