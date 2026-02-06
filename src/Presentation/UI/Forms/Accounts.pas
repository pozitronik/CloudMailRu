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
	PathHelper,
	PluginForm,
	SettingsConstants,
	WFXTypes,
	Vcl.ComCtrls,
	Vcl.Mask,
	Vcl.ExtCtrls,
	Vcl.Samples.Spin,
	System.IOUtils,
	AskPassword,
	PasswordManager,
	LanguageStrings,
	WSList,
	StreamingSettings,
	PluginSettingsManager,
	Cipher,
	AccountSettings,
	ConnectionSettings,
	AccountsManager,
	AccountsPresenter,
	Logger;

type
	TAccountsForm = class(TPluginForm, IAccountsView)
		OptionPages: TPageControl;
		AccountsTab: TTabSheet;
		GlobalTab: TTabSheet;
		PreserveFileTimeCB: TCheckBox;
		GlobalSettingsApplyBtn: TButton;
		NetworkTab: TTabSheet;
		ProxyGB: TGroupBox;
		ProxyTypeLabel: TLabel;
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
		CommentsTab: TTabSheet;
		CommentsSettingsApplyBtn: TButton;
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
		CommandPathOpenDialog: TOpenDialog;
		UserAgentEdit: TEdit;
		ChangeUserAgentCB: TCheckBox;
		ResetUserAgentButton: TButton;
		PrecalculateHashStrategyCombo: TComboBox;
		AccountsListView: TListView;
		AddButton: TButton;
		DeleteButton: TButton;
		AccountNameLabel: TLabel;
		AccountNameEdit: TEdit;
		AccountTypeGB: TGroupBox;
		PrivateRB: TRadioButton;
		PublicRB: TRadioButton;
		AccountsPanel: TPanel;
		EmailLabel: TLabel;
		EmailEdit: TEdit;
		PasswordLabel: TLabel;
		PasswordEdit: TEdit;
		UseTCPwdMngrCB: TCheckBox;
		FileSizeGB: TGroupBox;
		UnlimitedFileSizeCB: TCheckBox;
		SplitLargeFilesCB: TCheckBox;
		EncryptGB: TGroupBox;
		EncryptFilesLabel: TLabel;
		EncryptFilesCombo: TComboBox;
		EncryptFilesPwdButton: TButton;
		SharesPanel: TPanel;
		PublicUrlLabel: TLabel;
		PublicUrlEdit: TEdit;
		ApplyButton: TButton;
		NewExtButton: TButton;
		DeleteExtButton: TButton;
		StreamingExtensionsListView: TListView;
		CipherProfileLabel: TLabel;
		CipherProfileCombo: TComboBox;
		ServerLabel: TLabel;
		ServerCombo: TComboBox;
		ServersTab: TTabSheet;
		ServerNameLabel: TLabel;
		ServerUrlLabel: TLabel;
		ServerStatusLabel: TLabel;
		ServersListView: TListView;
		ServerNameEdit: TEdit;
		ServerUrlEdit: TEdit;
		TestServerButton: TButton;
		AddServerButton: TButton;
		DeleteServerButton: TButton;
		ApplyServerButton: TButton;
		ServersButton: TButton;
		TranslationTab: TTabSheet;
		LanguageLabel: TLabel;
		LanguageList: TListBox;
		ApplyTranslationBtn: TButton;
		TranslationStatusLabel: TLabel;
		ServerParametersGB: TGroupBox;
		ApiUrlLabel: TLabel;
		OAuthUrlLabel: TLabel;
		DispatcherUrlLabel: TLabel;
		ThumbnailUrlLabel: TLabel;
		ServerPublicUrlLabel: TLabel;
		DownloadUrlLabel: TLabel;
		UploadUrlLabel: TLabel;
		ApiUrlEdit: TEdit;
		OAuthUrlEdit: TEdit;
		DispatcherUrlEdit: TEdit;
		ThumbnailUrlEdit: TEdit;
		ServerPublicUrlEdit: TEdit;
		DownloadUrlEdit: TEdit;
		UploadUrlEdit: TEdit;
		TestAccountButton: TButton;
		TestShareButton: TButton;
		SSLParametersGroupbox: TGroupBox;
		SocketTimeoutLabel: TLabel;
		SocketTimeoutEdit: TSpinEdit;
		SSLBackendCB: TComboBox;
		SSLBackendLabel: TLabel;
		UseDLLFromPluginDir: TCheckBox;
		ShowAccountsGB: TGroupBox;
		ShowInvitesFoldersCB: TCheckBox;
		ShowSharedFoldersCB: TCheckBox;
		ShowTrashFoldersCB: TCheckBox;
    FileCommentsCB: TGroupBox;
    DescriptionFileNameLabel: TLabel;
    DescriptionEnabledCB: TCheckBox;
    DescriptionEditorEnabledCB: TCheckBox;
    DescriptionCopyToCloudCB: TCheckBox;
    DescriptionCopyFromCloudCB: TCheckBox;
    DescriptionFileNameEdit: TEdit;
    DescriptionTrackCloudFSCB: TCheckBox;
    FileTimestampsCB: TGroupBox;
    TimestampCopyToCloudCB: TCheckBox;
    TimestampCopyFromCloudCB: TCheckBox;
    TimestampTrackCloudFSCB: TCheckBox;
    TimestampFileNameLabel: TLabel;
    TimestampFileNameEdit: TEdit;
    TimestampConflictModeLabel: TLabel;
    TimestampConflictModeCB: TComboBox;
		procedure FormShow(Sender: TObject);
		procedure AccountsListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure AccountsListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
		procedure AddButtonClick(Sender: TObject);
		procedure DeleteButtonClick(Sender: TObject);
		procedure ApplyButtonClick(Sender: TObject);
		procedure PrivateRBClick(Sender: TObject);
		procedure PublicRBClick(Sender: TObject);
		procedure EncryptFilesComboChange(Sender: TObject);
		procedure CipherProfileComboChange(Sender: TObject);
		procedure EncryptFilesPwdButtonClick(Sender: TObject);
		procedure FieldChanged(Sender: TObject);
		class function ShowAccounts(ParentWindow: HWND; PasswordManager: IPasswordManager; Account: WideString): Boolean;
		procedure ProxyUserEditChange(Sender: TObject);
		procedure GlobalSettingsApplyBtnClick(Sender: TObject);
		procedure CloudMaxFileSizeCBClick(Sender: TObject);
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure StreamingExtensionsListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
		procedure StreamingExtensionsListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure StreamingFieldChanged(Sender: TObject);
		procedure NewExtButtonClick(Sender: TObject);
		procedure DeleteExtButtonClick(Sender: TObject);
		procedure ApplyExtButtonClick(Sender: TObject);
		procedure CommandPathButtonClick(Sender: TObject);
		procedure ChangeUserAgentCBClick(Sender: TObject);
		procedure ResetUserAgentButtonClick(Sender: TObject);
		procedure GlobalSettingsFieldChanged(Sender: TObject);
		procedure ProxyCBChange(Sender: TObject);
		procedure ServerComboChange(Sender: TObject);
		procedure ServersListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
		procedure ServersListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure ServerFieldChanged(Sender: TObject);
		procedure AddServerButtonClick(Sender: TObject);
		procedure DeleteServerButtonClick(Sender: TObject);
		procedure ApplyServerButtonClick(Sender: TObject);
		procedure TestServerButtonClick(Sender: TObject);
		procedure TestAccountButtonClick(Sender: TObject);
		procedure TestShareButtonClick(Sender: TObject);
		procedure ServersButtonClick(Sender: TObject);
		procedure ApplyTranslationBtnClick(Sender: TObject);
	private
		FPresenter: TAccountsPresenter;
		procedure AutoFitListViewColumns(LV: TListView);
		procedure RepopulateCombo(Combo: TComboBox; const Items: array of WideString);

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
		procedure SetSSLBackend(Value: Integer);
		function GetSSLBackend: Integer;

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
		procedure SetResetUserAgentEnabled(Value: Boolean);

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

		{IAccountsView - Timestamp settings}
		procedure SetTimestampCopyToCloud(Value: Boolean);
		function GetTimestampCopyToCloud: Boolean;
		procedure SetTimestampCopyFromCloud(Value: Boolean);
		function GetTimestampCopyFromCloud: Boolean;
		procedure SetTimestampTrackCloudFS(Value: Boolean);
		function GetTimestampTrackCloudFS: Boolean;
		procedure SetTimestampFileName(Value: WideString);
		function GetTimestampFileName: WideString;
		procedure SetTimestampConflictMode(Value: Integer);
		function GetTimestampConflictMode: Integer;

		{IAccountsView - Streaming extensions}
		procedure SetStreamingExtensionsList(const Items: TArray<TStreamingDisplayItem>);
		function GetSelectedStreamingExtensionIndex: Integer;
		function GetSelectedStreamingExtensionName: WideString;
		procedure SelectStreamingExtension(Index: Integer);
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
		procedure SetStreamingApplyButtonEnabled(Value: Boolean);
		function ConfirmDiscardStreamingChanges(const ExtensionName: WideString): TConfirmSaveResult;

		{IAccountsView - Global settings apply state}
		procedure SetGlobalSettingsApplyEnabled(Value: Boolean);

		{IAccountsView - Proxy controls state}
		procedure SetProxyControlsEnabled(Value: Boolean);

		{IAccountsView - UI actions}
		procedure ShowDescriptionFileNameError(Message: WideString);
		procedure ShowTab(TabIndex: Integer);
		function GetFormHandle: THandle;

		{IAccountsView - Dialogs}
		function ShowEncryptionPasswordDialog(const AccountName: WideString; var CryptedGUID: WideString): Boolean;

		{IAccountsView - Accounts tab}
		procedure SetAccountsList(const Items: TArray<TAccountDisplayItem>);
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
		procedure SetIsPrivate(Value: Boolean);
		function GetIsPrivate: Boolean;
		procedure SetPublicUrl(Value: WideString);
		function GetPublicUrl: WideString;
		procedure SetEncryptFilesMode(Value: Integer);
		function GetEncryptFilesMode: Integer;
		procedure SetEncryptPasswordButtonEnabled(Value: Boolean);
		procedure SetAccountsPanelVisible(Value: Boolean);
		procedure SetSharesPanelVisible(Value: Boolean);
		procedure SetApplyButtonEnabled(Value: Boolean);
		procedure SetTestAccountButtonEnabled(Value: Boolean);
		procedure SetTestAccountButtonCaption(const Value: WideString);
		procedure ShowTestAccountError(const Error: WideString);
		procedure SetTestShareButtonEnabled(Value: Boolean);
		procedure SetTestShareButtonCaption(const Value: WideString);
		procedure ShowTestShareError(const Error: WideString);
		function ConfirmDiscardAccountChanges(const AccountName: WideString): TConfirmSaveResult;
		procedure ShowAccountNameError(Message: WideString);
		function ConfirmAccountOverwrite(const AccountName: WideString): Boolean;

		{IAccountsView - Cipher profile}
		procedure SetCipherProfileItems(const Items: TArray<WideString>);
		procedure SetCipherProfileIndex(Value: Integer);
		function GetCipherProfileIndex: Integer;
		procedure SetCipherProfileEnabled(Value: Boolean);
		function ShowCipherChangeWarning: Boolean;

		{IAccountsView - Server combobox on accounts tab}
		procedure SetServerComboItems(const Items: TArray<WideString>);
		procedure SetServerComboIndex(Value: Integer);
		function GetServerComboIndex: Integer;
		function GetServerComboName: WideString;

		{IAccountsView - Servers tab list}
		procedure SetServersList(const Items: TArray<TServerDisplayItem>);
		function GetSelectedServerIndex: Integer;
		function GetSelectedServerName: WideString;
		procedure SelectServer(Index: Integer);

		{IAccountsView - Servers tab detail fields}
		procedure SetServerName(Value: WideString);
		function GetServerName: WideString;
		procedure SetServerUrl(Value: WideString);
		function GetServerUrl: WideString;
		procedure SetServerApiUrl(Value: WideString);
		function GetServerApiUrl: WideString;
		procedure SetServerOAuthUrl(Value: WideString);
		function GetServerOAuthUrl: WideString;
		procedure SetServerDispatcherUrl(Value: WideString);
		function GetServerDispatcherUrl: WideString;
		procedure SetServerThumbnailUrl(Value: WideString);
		function GetServerThumbnailUrl: WideString;
		procedure SetServerPublicUrl(Value: WideString);
		function GetServerPublicUrl: WideString;
		procedure SetServerDownloadUrl(Value: WideString);
		function GetServerDownloadUrl: WideString;
		procedure SetServerUploadUrl(Value: WideString);
		function GetServerUploadUrl: WideString;
		procedure SetServerStatus(const Value: WideString; IsSuccess: Boolean);

		{IAccountsView - Servers tab buttons}
		procedure SetServerApplyButtonEnabled(Value: Boolean);
		function ConfirmDiscardServerChanges(const ServerName: WideString): TConfirmSaveResult;

		{IAccountsView - Translation tab}
		procedure SetAvailableLanguages(const DisplayNames: TArray<WideString>);
		function GetSelectedLanguageIndex: Integer;
		procedure SetSelectedLanguageIndex(Value: Integer);
		procedure SetTranslationStatus(const Status: WideString);

		{IAccountsView - Translation support}
		procedure UpdateFormCaptions;
	public

	end;

implementation

{$R *.dfm}

uses
	System.UITypes,
	ProxySettings,
	CloudConstants,
	ConfigFile,
	ServerProfileManager,
	ServerConfigFetcher;

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

procedure TAccountsForm.SetSSLBackend(Value: Integer);
begin
	SSLBackendCB.ItemIndex := Value;
end;

function TAccountsForm.GetSSLBackend: Integer;
begin
	Result := SSLBackendCB.ItemIndex;
end;

{IAccountsView - Network settings}

procedure TAccountsForm.SetSocketTimeout(Value: Integer);
begin
	SocketTimeoutEdit.Text := IntToStr(Value);
end;

function TAccountsForm.GetSocketTimeout: Integer;
begin
	Result := StrToIntDef(SocketTimeoutEdit.Text, DEFAULT_SOCKET_TIMEOUT);
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

procedure TAccountsForm.SetResetUserAgentEnabled(Value: Boolean);
begin
	ResetUserAgentButton.Enabled := Value;
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

{IAccountsView - Timestamp settings}

procedure TAccountsForm.SetTimestampCopyToCloud(Value: Boolean);
begin
	TimestampCopyToCloudCB.Checked := Value;
end;

function TAccountsForm.GetTimestampCopyToCloud: Boolean;
begin
	Result := TimestampCopyToCloudCB.Checked;
end;

procedure TAccountsForm.SetTimestampCopyFromCloud(Value: Boolean);
begin
	TimestampCopyFromCloudCB.Checked := Value;
end;

function TAccountsForm.GetTimestampCopyFromCloud: Boolean;
begin
	Result := TimestampCopyFromCloudCB.Checked;
end;

procedure TAccountsForm.SetTimestampTrackCloudFS(Value: Boolean);
begin
	TimestampTrackCloudFSCB.Checked := Value;
end;

function TAccountsForm.GetTimestampTrackCloudFS: Boolean;
begin
	Result := TimestampTrackCloudFSCB.Checked;
end;

procedure TAccountsForm.SetTimestampFileName(Value: WideString);
begin
	TimestampFileNameEdit.Text := Value;
end;

function TAccountsForm.GetTimestampFileName: WideString;
begin
	Result := TimestampFileNameEdit.Text;
end;

procedure TAccountsForm.SetTimestampConflictMode(Value: Integer);
begin
	TimestampConflictModeCB.ItemIndex := Value;
end;

function TAccountsForm.GetTimestampConflictMode: Integer;
begin
	Result := TimestampConflictModeCB.ItemIndex;
end;

{IAccountsView - Streaming extensions}

procedure TAccountsForm.SetStreamingExtensionsList(const Items: TArray<TStreamingDisplayItem>);
var
	I: Integer;
	LI: TListItem;
begin
	StreamingExtensionsListView.Items.BeginUpdate;
	try
		StreamingExtensionsListView.Items.Clear;
		for I := 0 to High(Items) do
		begin
			LI := StreamingExtensionsListView.Items.Add;
			LI.Caption := Items[I].Extension;
			LI.SubItems.Add(Items[I].TypeLabel);
		end;
	finally
		StreamingExtensionsListView.Items.EndUpdate;
	end;
	AutoFitListViewColumns(StreamingExtensionsListView);
end;

function TAccountsForm.GetSelectedStreamingExtensionIndex: Integer;
begin
	if StreamingExtensionsListView.Selected <> nil then
		Result := StreamingExtensionsListView.Selected.Index
	else
		Result := -1;
end;

function TAccountsForm.GetSelectedStreamingExtensionName: WideString;
begin
	if StreamingExtensionsListView.Selected <> nil then
		Result := StreamingExtensionsListView.Selected.Caption
	else
		Result := '';
end;

procedure TAccountsForm.SelectStreamingExtension(Index: Integer);
begin
	if (Index >= 0) and (Index < StreamingExtensionsListView.Items.Count) then
	begin
		StreamingExtensionsListView.Selected := StreamingExtensionsListView.Items[Index];
		StreamingExtensionsListView.ItemFocused := StreamingExtensionsListView.Items[Index];
	end else begin
		StreamingExtensionsListView.Selected := nil;
		StreamingExtensionsListView.ItemFocused := nil;
	end;
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

procedure TAccountsForm.SetStreamingApplyButtonEnabled(Value: Boolean);
begin
	ApplyExtButton.Enabled := Value;
end;

function TAccountsForm.ConfirmDiscardStreamingChanges(const ExtensionName: WideString): TConfirmSaveResult;
begin
	case MessageDlg(Format(ASK_SAVE_STREAMING_CHANGES, [ExtensionName]), mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
		mrYes:
			Result := csrSave;
		mrNo:
			Result := csrDiscard;
		else
			Result := csrCancel;
	end;
end;

{IAccountsView - Global settings apply state}

procedure TAccountsForm.SetGlobalSettingsApplyEnabled(Value: Boolean);
begin
	GlobalSettingsApplyBtn.Enabled := Value;
	NetworkSettingsApplyBtn.Enabled := Value;
	CommentsSettingsApplyBtn.Enabled := Value;
end;

{IAccountsView - Proxy controls state}

procedure TAccountsForm.SetProxyControlsEnabled(Value: Boolean);
begin
	ProxyServerEdit.Enabled := Value;
	ProxyPortEdit.Enabled := Value;
	ProxyUserEdit.Enabled := Value;
	ProxyPwd.Enabled := Value;
	{ProxyTCPwdMngrCB state is refined by OnProxyUserChanged}
end;

{AutoFitListViewColumns - fills first column to remaining width}

procedure TAccountsForm.AutoFitListViewColumns(LV: TListView);
var
	I, OtherColumnsWidth: Integer;
begin
	if LV.Columns.Count < 2 then
		Exit;
	OtherColumnsWidth := 0;
	for I := 1 to LV.Columns.Count - 1 do
		OtherColumnsWidth := OtherColumnsWidth + LV.Column[I].Width;
	LV.Column[0].Width := LV.ClientWidth - OtherColumnsWidth;
end;

{IAccountsView - UI actions}

procedure TAccountsForm.ShowDescriptionFileNameError(Message: WideString);
var
	MessageBalloon: TBalloonHint;
begin
	MessageBalloon := TBalloonHint.Create(Self);
	MessageBalloon.HideAfter := 5000;
	MessageBalloon.Delay := 0;
	MessageBalloon.Description := Message;
	MessageBalloon.ShowHint(DescriptionFileNameEdit);
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

{IAccountsView - Accounts tab}

procedure TAccountsForm.SetAccountsList(const Items: TArray<TAccountDisplayItem>);
var
	I: Integer;
	LI: TListItem;
begin
	AccountsListView.Items.BeginUpdate;
	try
		AccountsListView.Items.Clear;
		for I := 0 to High(Items) do
		begin
			LI := AccountsListView.Items.Add;
			LI.Caption := Items[I].Name;
			LI.SubItems.Add(Items[I].TypeLabel);
			LI.SubItems.Add(Items[I].ServerLabel);
		end;
	finally
		AccountsListView.Items.EndUpdate;
	end;
	AutoFitListViewColumns(AccountsListView);
end;

function TAccountsForm.GetSelectedAccountIndex: Integer;
begin
	if AccountsListView.Selected <> nil then
		Result := AccountsListView.Selected.Index
	else
		Result := -1;
end;

function TAccountsForm.GetSelectedAccountName: WideString;
begin
	if AccountsListView.Selected <> nil then
		Result := AccountsListView.Selected.Caption
	else
		Result := '';
end;

procedure TAccountsForm.SelectAccount(Index: Integer);
begin
	if (Index >= 0) and (Index < AccountsListView.Items.Count) then
	begin
		AccountsListView.Selected := AccountsListView.Items[Index];
		AccountsListView.ItemFocused := AccountsListView.Items[Index];
	end else begin
		AccountsListView.Selected := nil;
		AccountsListView.ItemFocused := nil;
	end;
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

procedure TAccountsForm.SetIsPrivate(Value: Boolean);
begin
	PrivateRB.Checked := Value;
	PublicRB.Checked := not Value;
end;

function TAccountsForm.GetIsPrivate: Boolean;
begin
	Result := PrivateRB.Checked;
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

procedure TAccountsForm.SetApplyButtonEnabled(Value: Boolean);
begin
	ApplyButton.Enabled := Value;
end;

procedure TAccountsForm.SetTestAccountButtonEnabled(Value: Boolean);
begin
	TestAccountButton.Enabled := Value;
end;

procedure TAccountsForm.SetTestAccountButtonCaption(const Value: WideString);
begin
	TestAccountButton.Caption := Value;
end;

procedure TAccountsForm.ShowTestAccountError(const Error: WideString);
begin
	MessageDlg(Error, mtError, [mbOK], 0);
end;

procedure TAccountsForm.SetTestShareButtonEnabled(Value: Boolean);
begin
	TestShareButton.Enabled := Value;
end;

procedure TAccountsForm.SetTestShareButtonCaption(const Value: WideString);
begin
	TestShareButton.Caption := Value;
end;

procedure TAccountsForm.ShowTestShareError(const Error: WideString);
begin
	MessageDlg(Error, mtError, [mbOK], 0);
end;

function TAccountsForm.ConfirmDiscardAccountChanges(const AccountName: WideString): TConfirmSaveResult;
begin
	case MessageDlg(Format(ASK_SAVE_ACCOUNT_CHANGES, [AccountName]), mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
		mrYes:
			Result := csrSave;
		mrNo:
			Result := csrDiscard;
		else
			Result := csrCancel;
	end;
end;

procedure TAccountsForm.ShowAccountNameError(Message: WideString);
var
	MessageBalloon: TBalloonHint;
begin
	MessageBalloon := TBalloonHint.Create(Self);
	MessageBalloon.HideAfter := 5000;
	MessageBalloon.Delay := 0;
	MessageBalloon.Description := Message;
	MessageBalloon.ShowHint(AccountNameEdit);
end;

function TAccountsForm.ConfirmAccountOverwrite(const AccountName: WideString): Boolean;
begin
	Result := MessageDlg(Format(ASK_OVERWRITE_ACCOUNT, [AccountName]), mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

{IAccountsView - Cipher profile}

procedure TAccountsForm.SetCipherProfileItems(const Items: TArray<WideString>);
var
	I: Integer;
begin
	CipherProfileCombo.Items.BeginUpdate;
	try
		CipherProfileCombo.Items.Clear;
		for I := 0 to High(Items) do
			CipherProfileCombo.Items.Add(Items[I]);
	finally
		CipherProfileCombo.Items.EndUpdate;
	end;
	if CipherProfileCombo.Items.Count > 0 then
		CipherProfileCombo.ItemIndex := 0;
end;

procedure TAccountsForm.SetCipherProfileIndex(Value: Integer);
begin
	CipherProfileCombo.ItemIndex := Value;
end;

function TAccountsForm.GetCipherProfileIndex: Integer;
begin
	Result := CipherProfileCombo.ItemIndex;
end;

procedure TAccountsForm.SetCipherProfileEnabled(Value: Boolean);
begin
	CipherProfileCombo.Enabled := Value;
end;

function TAccountsForm.ShowCipherChangeWarning: Boolean;
begin
	Result := MessageDlg(WARN_CIPHER_CHANGE, mtWarning, [mbYes, mbNo], 0) = mrYes;
end;

{IAccountsView - Server combobox on accounts tab}

procedure TAccountsForm.SetServerComboItems(const Items: TArray<WideString>);
var
	I: Integer;
begin
	ServerCombo.Items.BeginUpdate;
	try
		ServerCombo.Items.Clear;
		for I := 0 to High(Items) do
			ServerCombo.Items.Add(Items[I]);
	finally
		ServerCombo.Items.EndUpdate;
	end;
	if ServerCombo.Items.Count > 0 then
		ServerCombo.ItemIndex := 0;
end;

procedure TAccountsForm.SetServerComboIndex(Value: Integer);
begin
	ServerCombo.ItemIndex := Value;
end;

function TAccountsForm.GetServerComboIndex: Integer;
begin
	Result := ServerCombo.ItemIndex;
end;

function TAccountsForm.GetServerComboName: WideString;
begin
	if (ServerCombo.ItemIndex >= 0) and (ServerCombo.ItemIndex < ServerCombo.Items.Count) then
		Result := ServerCombo.Items[ServerCombo.ItemIndex]
	else
		Result := '';
end;

{IAccountsView - Servers tab list}

procedure TAccountsForm.SetServersList(const Items: TArray<TServerDisplayItem>);
var
	I: Integer;
	LI: TListItem;
begin
	ServersListView.Items.BeginUpdate;
	try
		ServersListView.Items.Clear;
		for I := 0 to High(Items) do
		begin
			LI := ServersListView.Items.Add;
			LI.Caption := Items[I].Name;
			LI.SubItems.Add(Items[I].Url);
		end;
	finally
		ServersListView.Items.EndUpdate;
	end;
	AutoFitListViewColumns(ServersListView);
end;

function TAccountsForm.GetSelectedServerIndex: Integer;
begin
	if ServersListView.Selected <> nil then
		Result := ServersListView.Selected.Index
	else
		Result := -1;
end;

function TAccountsForm.GetSelectedServerName: WideString;
begin
	if ServersListView.Selected <> nil then
		Result := ServersListView.Selected.Caption
	else
		Result := '';
end;

procedure TAccountsForm.SelectServer(Index: Integer);
begin
	if (Index >= 0) and (Index < ServersListView.Items.Count) then
	begin
		ServersListView.Selected := ServersListView.Items[Index];
		ServersListView.ItemFocused := ServersListView.Items[Index];
	end else begin
		ServersListView.Selected := nil;
		ServersListView.ItemFocused := nil;
	end;
end;

{IAccountsView - Servers tab detail fields}

procedure TAccountsForm.SetServerName(Value: WideString);
begin
	ServerNameEdit.Text := Value;
end;

function TAccountsForm.GetServerName: WideString;
begin
	Result := ServerNameEdit.Text;
end;

procedure TAccountsForm.SetServerUrl(Value: WideString);
begin
	ServerUrlEdit.Text := Value;
end;

function TAccountsForm.GetServerUrl: WideString;
begin
	Result := ServerUrlEdit.Text;
end;

procedure TAccountsForm.SetServerApiUrl(Value: WideString);
begin
	ApiUrlEdit.Text := Value;
end;

function TAccountsForm.GetServerApiUrl: WideString;
begin
	Result := ApiUrlEdit.Text;
end;

procedure TAccountsForm.SetServerOAuthUrl(Value: WideString);
begin
	OAuthUrlEdit.Text := Value;
end;

function TAccountsForm.GetServerOAuthUrl: WideString;
begin
	Result := OAuthUrlEdit.Text;
end;

procedure TAccountsForm.SetServerDispatcherUrl(Value: WideString);
begin
	DispatcherUrlEdit.Text := Value;
end;

function TAccountsForm.GetServerDispatcherUrl: WideString;
begin
	Result := DispatcherUrlEdit.Text;
end;

procedure TAccountsForm.SetServerThumbnailUrl(Value: WideString);
begin
	ThumbnailUrlEdit.Text := Value;
end;

function TAccountsForm.GetServerThumbnailUrl: WideString;
begin
	Result := ThumbnailUrlEdit.Text;
end;

procedure TAccountsForm.SetServerPublicUrl(Value: WideString);
begin
	ServerPublicUrlEdit.Text := Value;
end;

function TAccountsForm.GetServerPublicUrl: WideString;
begin
	Result := ServerPublicUrlEdit.Text;
end;

procedure TAccountsForm.SetServerDownloadUrl(Value: WideString);
begin
	DownloadUrlEdit.Text := Value;
end;

function TAccountsForm.GetServerDownloadUrl: WideString;
begin
	Result := DownloadUrlEdit.Text;
end;

procedure TAccountsForm.SetServerUploadUrl(Value: WideString);
begin
	UploadUrlEdit.Text := Value;
end;

function TAccountsForm.GetServerUploadUrl: WideString;
begin
	Result := UploadUrlEdit.Text;
end;

procedure TAccountsForm.SetServerStatus(const Value: WideString; IsSuccess: Boolean);
begin
	ServerStatusLabel.Caption := Value;
	if IsSuccess then
		ServerStatusLabel.Font.Color := clGreen
	else
		ServerStatusLabel.Font.Color := clRed;
end;

{IAccountsView - Servers tab buttons}

procedure TAccountsForm.SetServerApplyButtonEnabled(Value: Boolean);
begin
	ApplyServerButton.Enabled := Value;
end;

function TAccountsForm.ConfirmDiscardServerChanges(const ServerName: WideString): TConfirmSaveResult;
begin
	case MessageDlg(Format(ASK_SAVE_SERVER_CHANGES, [ServerName]), mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
		mrYes:
			Result := csrSave;
		mrNo:
			Result := csrDiscard;
		else
			Result := csrCancel;
	end;
end;

{Event handlers - Accounts tab}

procedure TAccountsForm.AccountsListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_DELETE then
		DeleteButton.OnClick(nil);
end;

procedure TAccountsForm.AccountsListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
	if Selected then
		FPresenter.OnAccountSelected;
end;

procedure TAccountsForm.AddButtonClick(Sender: TObject);
begin
	FPresenter.OnAddAccountClick;
end;

procedure TAccountsForm.DeleteButtonClick(Sender: TObject);
begin
	FPresenter.OnDeleteAccountClick;
end;

procedure TAccountsForm.ApplyButtonClick(Sender: TObject);
begin
	FPresenter.OnApplyAccountClick;
end;

procedure TAccountsForm.PrivateRBClick(Sender: TObject);
begin
	FPresenter.OnAccountTypeChanged;
end;

procedure TAccountsForm.PublicRBClick(Sender: TObject);
begin
	FPresenter.OnAccountTypeChanged;
end;

procedure TAccountsForm.EncryptFilesComboChange(Sender: TObject);
begin
	FPresenter.OnEncryptModeChanged;
end;

procedure TAccountsForm.CipherProfileComboChange(Sender: TObject);
begin
	FPresenter.OnCipherProfileChanged;
end;

procedure TAccountsForm.EncryptFilesPwdButtonClick(Sender: TObject);
begin
	FPresenter.OnEncryptPasswordClick;
end;

procedure TAccountsForm.FieldChanged(Sender: TObject);
begin
	FPresenter.OnFieldChanged;
end;

{Event handlers - other tabs}

procedure TAccountsForm.ApplyExtButtonClick(Sender: TObject);
begin
	FPresenter.OnApplyStreamingExtensionClick;
end;

procedure TAccountsForm.ChangeUserAgentCBClick(Sender: TObject);
begin
	FPresenter.OnChangeUserAgentChanged;
end;

procedure TAccountsForm.ResetUserAgentButtonClick(Sender: TObject);
begin
	FPresenter.OnResetUserAgentClick;
end;

procedure TAccountsForm.GlobalSettingsFieldChanged(Sender: TObject);
begin
	FPresenter.OnGlobalSettingsFieldChanged;
end;

procedure TAccountsForm.ProxyCBChange(Sender: TObject);
begin
	FPresenter.OnProxyTypeChanged;
end;

procedure TAccountsForm.CloudMaxFileSizeCBClick(Sender: TObject);
begin
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

procedure TAccountsForm.DeleteExtButtonClick(Sender: TObject);
begin
	FPresenter.OnDeleteStreamingExtensionClick;
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
	AccountsListView.SetFocus;
end;

procedure TAccountsForm.GlobalSettingsApplyBtnClick(Sender: TObject);
begin
	FPresenter.OnApplyGlobalSettingsClick;
end;

procedure TAccountsForm.ProxyUserEditChange(Sender: TObject);
begin
	FPresenter.OnProxyUserChanged;
end;

procedure TAccountsForm.StreamingExtensionsListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
	if Selected then
		FPresenter.OnStreamingExtensionSelected;
end;

procedure TAccountsForm.StreamingExtensionsListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_DELETE then
		DeleteExtButton.OnClick(nil);
end;

procedure TAccountsForm.StreamingFieldChanged(Sender: TObject);
begin
	FPresenter.OnStreamingFieldChanged;
end;

procedure TAccountsForm.NewExtButtonClick(Sender: TObject);
begin
	FPresenter.OnAddStreamingExtensionClick;
end;

{Event handlers - Server combobox on accounts tab}

procedure TAccountsForm.ServerComboChange(Sender: TObject);
begin
	FPresenter.OnServerComboChanged;
end;

{Event handlers - Servers tab}

procedure TAccountsForm.ServersListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
	if Selected then
		FPresenter.OnServerSelected;
end;

procedure TAccountsForm.ServersListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_DELETE then
		DeleteServerButton.OnClick(nil);
end;

procedure TAccountsForm.ServerFieldChanged(Sender: TObject);
begin
	FPresenter.OnServerFieldChanged;
end;

procedure TAccountsForm.AddServerButtonClick(Sender: TObject);
begin
	FPresenter.OnAddServerClick;
end;

procedure TAccountsForm.DeleteServerButtonClick(Sender: TObject);
begin
	FPresenter.OnDeleteServerClick;
end;

procedure TAccountsForm.ApplyServerButtonClick(Sender: TObject);
begin
	FPresenter.OnApplyServerClick;
end;

procedure TAccountsForm.TestServerButtonClick(Sender: TObject);
begin
	FPresenter.OnTestServerClick;
end;

procedure TAccountsForm.TestAccountButtonClick(Sender: TObject);
begin
	FPresenter.OnTestAccountClick;
end;

procedure TAccountsForm.TestShareButtonClick(Sender: TObject);
begin
	FPresenter.OnTestShareClick;
end;

procedure TAccountsForm.ServersButtonClick(Sender: TObject);
begin
	FPresenter.OnServersButtonClick;
end;

{IAccountsView - Translation tab}

procedure TAccountsForm.SetAvailableLanguages(const DisplayNames: TArray<WideString>);
var
	I: Integer;
begin
	LanguageList.Items.BeginUpdate;
	try
		LanguageList.Items.Clear;
		for I := 0 to High(DisplayNames) do
			LanguageList.Items.Add(DisplayNames[I]);
	finally
		LanguageList.Items.EndUpdate;
	end;
	if LanguageList.Items.Count > 0 then
		LanguageList.ItemIndex := 0;
end;

function TAccountsForm.GetSelectedLanguageIndex: Integer;
begin
	Result := LanguageList.ItemIndex;
end;

procedure TAccountsForm.SetSelectedLanguageIndex(Value: Integer);
begin
	if (Value >= 0) and (Value < LanguageList.Items.Count) then
		LanguageList.ItemIndex := Value
	else if LanguageList.Items.Count > 0 then
		LanguageList.ItemIndex := 0;
end;

procedure TAccountsForm.SetTranslationStatus(const Status: WideString);
begin
	TranslationStatusLabel.Caption := Status;
end;

{Combo repopulation helper: preserves ItemIndex, suppresses OnChange}

procedure TAccountsForm.RepopulateCombo(Combo: TComboBox; const Items: array of WideString);
var
	SavedIndex: Integer;
	SavedOnChange: TNotifyEvent;
	I: Integer;
begin
	SavedIndex := Combo.ItemIndex;
	SavedOnChange := Combo.OnChange;
	Combo.OnChange := nil;
	try
		Combo.Items.BeginUpdate;
		try
			Combo.Items.Clear;
			for I := 0 to High(Items) do
				Combo.Items.Add(Items[I]);
		finally
			Combo.Items.EndUpdate;
		end;
		if (SavedIndex >= 0) and (SavedIndex < Combo.Items.Count) then
			Combo.ItemIndex := SavedIndex
		else if Combo.Items.Count > 0 then
			Combo.ItemIndex := 0;
	finally
		Combo.OnChange := SavedOnChange;
	end;
end;

{IAccountsView - Translation support}

procedure TAccountsForm.UpdateFormCaptions;
begin
	{Form title}
	Caption := DFM_FORM_TITLE;

	{Tab captions}
	AccountsTab.Caption := DFM_TAB_ACCOUNTS;
	GlobalTab.Caption := DFM_TAB_GLOBAL_SETTINGS;
	NetworkTab.Caption := DFM_TAB_NETWORK_SETTINGS;
	CommentsTab.Caption := DFM_TAB_METADATA;
	StreamingTab.Caption := DFM_TAB_STREAMING;
	TranslationTab.Caption := DFM_TAB_TRANSLATION;

	{Apply buttons - all share the same translated label}
	ApplyButton.Caption := DFM_BTN_APPLY;
	GlobalSettingsApplyBtn.Caption := DFM_BTN_APPLY;
	NetworkSettingsApplyBtn.Caption := DFM_BTN_APPLY;
	CommentsSettingsApplyBtn.Caption := DFM_BTN_APPLY;
	ApplyExtButton.Caption := DFM_BTN_APPLY;
	ApplyTranslationBtn.Caption := DFM_BTN_APPLY;

	{New/Delete buttons}
	AddButton.Caption := DFM_BTN_NEW;
	DeleteButton.Caption := DFM_BTN_DELETE;
	NewExtButton.Caption := DFM_BTN_NEW;
	DeleteExtButton.Caption := DFM_BTN_DELETE;
	EncryptFilesPwdButton.Caption := DFM_BTN_SET_PASSWORD;
	ResetUserAgentButton.Caption := DFM_BTN_RESET_UA;

	{Accounts tab - labels, groups, radios, checkboxes}
	AccountNameLabel.Caption := DFM_LBL_ACCOUNT_NAME;
	PublicUrlLabel.Caption := DFM_LBL_PUBLIC_URL;
	EmailLabel.Caption := DFM_LBL_EMAIL;
	PasswordLabel.Caption := DFM_LBL_APP_PASSWORD;
	AccountTypeGB.Caption := DFM_GB_ACCOUNT_TYPE;
	PrivateRB.Caption := DFM_RB_PRIVATE;
	PublicRB.Caption := DFM_RB_PUBLIC;
	UseTCPwdMngrCB.Caption := DFM_CB_TC_PASSWORD;
	FileSizeGB.Caption := DFM_GB_FILE_SIZE;
	UnlimitedFileSizeCB.Caption := DFM_CB_UNLIMITED_SIZE;
	SplitLargeFilesCB.Caption := DFM_CB_SPLIT_FILES;
	EncryptGB.Caption := DFM_GB_ENCRYPTION;
	EncryptFilesLabel.Caption := DFM_LBL_ENCRYPT_FILES;
	CipherProfileLabel.Caption := DFM_LBL_ENCRYPT_BACKEND;
	AccountsListView.Columns[0].Caption := DFM_COL_ACCOUNT;
	AccountsListView.Columns[1].Caption := DFM_COL_TYPE;
	AccountsListView.Columns[2].Caption := DFM_COL_ACCOUNT_SERVER;

	{Global settings tab}
	CloudMaxFileSizeLabelBytes.Caption := DFM_LBL_BYTES;
	ChunkOverwriteModeLabel.Caption := DFM_LBL_CHUNK_OVERWRITE;
	DeleteFailOnUploadModeLabel.Caption := DFM_LBL_DELETE_FAIL_UPLOAD;
	OverwriteLocalModeLabel.Caption := DFM_LBL_OVERWRITE_LOCAL;
	IconsModeLabel.Caption := DFM_LBL_ICONS_MODE;
	OperationErrorModeLabel.Caption := DFM_LBL_OPERATION_ERROR;
	RetryAttemptsLabel.Caption := DFM_LBL_RETRY_ATTEMPTS;
	RetryWaitLabel.Caption := DFM_LBL_RETRY_WAIT;
	msLabel.Caption := DFM_LBL_MS;
	ShowAccountsGB.Caption := DFM_GB_SHOW_ACCOUNTS;
	CopyBetweenAccountsModeLabel.Caption := DFM_LBL_COPY_BETWEEN;
	PreserveFileTimeCB.Caption := DFM_CB_PRESERVE_TIME;
	UseDLLFromPluginDir.Caption := DFM_CB_LOAD_SSL;
	SpaceInfoLoggingCB.Caption := DFM_CB_LOG_SPACE;
	CloudMaxFileSizeCB.Caption := DFM_CB_OVERRIDE_SPLIT;
	DisableMultiThreadingCB.Caption := DFM_CB_DISABLE_MT;
	ShowTrashFoldersCB.Caption := DFM_CB_TRASH;
	ShowSharedFoldersCB.Caption := DFM_CB_SHARED;
	ShowInvitesFoldersCB.Caption := DFM_CB_INVITES;
	PrecalculateHashCB.Caption := DFM_CB_PRECALC_HASH;
	CheckCRCCB.Caption := DFM_CB_CHECK_CRC;

	{Network settings tab}
	SSLParametersGroupbox.Caption := DFM_GB_SSL;
	SSLBackendLabel.Caption := DFM_LBL_SSL_BACKEND;
	SocketTimeoutLabel.Caption := DFM_LBL_SOCKET_TIMEOUT;
	ProxyGB.Caption := DFM_GB_PROXY;
	ProxyTypeLabel.Caption := DFM_LBL_PROXY_TYPE;
	ProxyPortLabel.Caption := DFM_LBL_PROXY_PORT;
	ProxyUserLabel.Caption := DFM_LBL_PROXY_USER;
	ProxyPWDLabel.Caption := DFM_LBL_PROXY_PASSWORD;
	ProxyServerLabel.Caption := DFM_LBL_PROXY_SERVER;
	ProxyTCPwdMngrCB.Caption := DFM_CB_PROXY_TC_PASSWORD;
	SpeedLimitGB.Caption := DFM_GB_SPEED_LIMITS;
	UploadsBPSLabel.Caption := DFM_LBL_UPLOAD_BPS;
	DownloadsBPSLabel.Caption := DFM_LBL_DOWNLOAD_BPS;
	ChangeUserAgentCB.Caption := DFM_CB_CHANGE_UA;

	{Metadata tab - File comments groupbox}
	FileCommentsCB.Caption := DFM_GB_FILE_COMMENTS;
	DescriptionFileNameLabel.Caption := DFM_LBL_DESC_FILENAME;
	DescriptionEnabledCB.Caption := DFM_CB_DESC_ENABLED;
	DescriptionEditorEnabledCB.Caption := DFM_CB_DESC_EDITOR;
	DescriptionCopyToCloudCB.Caption := DFM_CB_DESC_COPY_TO;
	DescriptionCopyFromCloudCB.Caption := DFM_CB_DESC_COPY_FROM;
	DescriptionTrackCloudFSCB.Caption := DFM_CB_DESC_TRACK;

	{Metadata tab - File timestamps groupbox}
	FileTimestampsCB.Caption := DFM_GB_FILE_TIMESTAMPS;
	TimestampCopyToCloudCB.Caption := DFM_CB_TS_COPY_TO;
	TimestampCopyFromCloudCB.Caption := DFM_CB_TS_COPY_FROM;
	TimestampTrackCloudFSCB.Caption := DFM_CB_TS_TRACK;
	TimestampFileNameLabel.Caption := DFM_LBL_TS_FILENAME;
	TimestampConflictModeLabel.Caption := DFM_LBL_TS_CONFLICT;
	RepopulateCombo(TimestampConflictModeCB, [DFM_OPT_TS_USE_STORED, DFM_OPT_TS_USE_SERVER]);

	{Streaming tab}
	ExtLabel.Caption := DFM_LBL_FILE_EXT;
	CommandLabel.Caption := DFM_LBL_COMMAND;
	ParametersLabel.Caption := DFM_LBL_PARAMETERS;
	StartPathLabel.Caption := DFM_LBL_START_PATH;
	StreamingTypeLabel.Caption := DFM_LBL_STREAMING_TYPE;
	StreamingExtensionsListView.Columns[0].Caption := DFM_COL_EXTENSION;
	StreamingExtensionsListView.Columns[1].Caption := DFM_COL_TYPE;

	{Servers tab}
	ServersTab.Caption := DFM_TAB_SERVERS;
	ServerLabel.Caption := DFM_LBL_SERVER;
	ServerNameLabel.Caption := DFM_LBL_SERVER_NAME;
	ServerUrlLabel.Caption := DFM_LBL_SERVER_URL;
	ServerParametersGB.Caption := DFM_GB_SERVER_PARAMETERS;
	ApiUrlLabel.Caption := DFM_LBL_API_URL;
	OAuthUrlLabel.Caption := DFM_LBL_OAUTH_URL;
	DispatcherUrlLabel.Caption := DFM_LBL_DISPATCHER_URL;
	ThumbnailUrlLabel.Caption := DFM_LBL_THUMBNAIL_URL;
	ServerPublicUrlLabel.Caption := DFM_LBL_PUBLIC_URL_SERVER;
	DownloadUrlLabel.Caption := DFM_LBL_DOWNLOAD_URL;
	UploadUrlLabel.Caption := DFM_LBL_UPLOAD_URL;
	TestServerButton.Caption := DFM_BTN_TEST_SERVER;
	ServersButton.Caption := DFM_BTN_SERVERS;
	AddServerButton.Caption := DFM_BTN_SERVER_NEW;
	DeleteServerButton.Caption := DFM_BTN_SERVER_DELETE;
	ApplyServerButton.Caption := DFM_BTN_SERVER_APPLY;
	ServersListView.Columns[0].Caption := DFM_COL_SERVER_NAME;
	ServersListView.Columns[1].Caption := DFM_COL_SERVER_URL;

	{Translation tab}
	LanguageLabel.Caption := DFM_LBL_LANGUAGE;

	{Combobox items}
	RepopulateCombo(SSLBackendCB, [DFM_OPT_SSL_AUTO, DFM_OPT_SSL_INDY, DFM_OPT_SSL_INDYSEC]);
	RepopulateCombo(EncryptFilesCombo, [DFM_OPT_ENCRYPT_NO, DFM_OPT_ENCRYPT_ALWAYS, DFM_OPT_ENCRYPT_ASK_ONCE]);
	RepopulateCombo(ChunkOverwriteModeCombo, [DFM_OPT_SILENTLY_OVERWRITE, DFM_OPT_IGNORE, DFM_OPT_ABORT_OPERATION]);
	RepopulateCombo(DeleteFailOnUploadModeCombo, [DFM_OPT_ASK_USER, DFM_OPT_IGNORE_FILE, DFM_OPT_ABORT_OPERATION, DFM_OPT_UNSET_RO_IGNORE, DFM_OPT_UNSET_RO_ABORT]);
	RepopulateCombo(OverwriteLocalModeCombo, [DFM_OPT_ASK_USER, DFM_OPT_IGNORE_FILE, DFM_OPT_SILENTLY_OVERWRITE]);
	RepopulateCombo(IconsModeCombo, [DFM_OPT_ICONS_DEFAULT, DFM_OPT_ICONS_INTERNAL, DFM_OPT_ICONS_INTERNAL_OVERLAY, DFM_OPT_ICONS_EXTERNAL, DFM_OPT_ICONS_EXTERNAL_OVERLAY]);
	RepopulateCombo(OperationErrorModeCombo, [DFM_OPT_ASK_USER, DFM_OPT_IGNORE_FILE, DFM_OPT_ABORT_OPERATION, DFM_OPT_RETRY]);
	RepopulateCombo(CopyBetweenAccountsModeCombo, [DFM_OPT_COPY_DISABLED, DFM_OPT_COPY_VIA_HASH, DFM_OPT_COPY_VIA_LINK]);
	RepopulateCombo(PrecalculateHashStrategyCombo, [DFM_OPT_HASH_AUTO, DFM_OPT_HASH_DELPHI, DFM_OPT_HASH_BCRYPT, DFM_OPT_HASH_OPENSSL]);
	RepopulateCombo(ProxyCB, [DFM_OPT_NO_PROXY, DFM_OPT_SOCKS5, DFM_OPT_SOCKS4, DFM_OPT_HTTP]);
	RepopulateCombo(StreamingTypeCombo, [DFM_OPT_STREAM_NONE, DFM_OPT_STREAM_DISABLED, DFM_OPT_STREAM_M3U8, DFM_OPT_STREAM_DEFAULT, DFM_OPT_STREAM_WEB]);
end;

{Event handlers - Translation tab}

procedure TAccountsForm.ApplyTranslationBtnClick(Sender: TObject);
begin
	FPresenter.OnApplyTranslationClick;
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

	PasswordMgr := FPresenter.PasswordManager;
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
	ServerProfileMgr: IServerProfileManager;
	Config: TAccountsPresenterConfig;
	PluginSettingsMgr: TPluginSettingsManager;
begin
	Form := TAccountsForm.Create(nil);
	try
		Form.ParentWindow := ParentWindow;

		{Create managers - interface reference counting handles cleanup}
		PluginSettingsMgr := TPluginSettingsManager.Create();
		SettingsManager := PluginSettingsMgr;
		AccountsMgr := TAccountsManager.Create(TIniConfigFile.Create(PluginSettingsMgr.AccountsIniFilePath), TNullLogger.Create);
		ServerProfileMgr := TServerProfileManager.Create(TIniConfigFile.Create(PluginSettingsMgr.IniFilePath));

		{Create presenter config}
		Config.PasswordManager := PasswordManager;
		Config.ServerConfigFetcher := TServerConfigFetcher.Create;
		Config.ParentWindow := ParentWindow;
		Config.LanguageDir := PluginSettingsMgr.ApplicationPath + 'language\';

		{Create presenter}
		Form.FPresenter := TAccountsPresenter.Create(Form, AccountsMgr, SettingsManager, ServerProfileMgr, Config);

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
