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
	SettingsConstants,
	WFXTypes,
	Vcl.ComCtrls,
	Vcl.Mask,
	Vcl.ExtCtrls,
	Vcl.Samples.Spin,
	System.IOUtils,
	AskPassword,
	TCPasswordManager,
	LanguageStrings,
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
		PreserveFileTimeCB: TCheckBox;
		UseDLLFromPluginDir: TCheckBox;
		GlobalSettingsApplyBtn: TButton;
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
		EncryptFilenamesCB: TCheckBox;
		SharesPanel: TPanel;
		PublicUrlLabel: TLabel;
		PublicUrlEdit: TEdit;
		ApplyButton: TButton;
    NewExtButton: TButton;
    DeleteExtButton: TButton;
    StreamingExtensionsListView: TListView;
    EncryptBackendLabel: TLabel;
    EncryptBackendCombo: TComboBox;
		procedure FormShow(Sender: TObject);
		procedure AccountsListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure AccountsListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
		procedure AddButtonClick(Sender: TObject);
		procedure DeleteButtonClick(Sender: TObject);
		procedure ApplyButtonClick(Sender: TObject);
		procedure PrivateRBClick(Sender: TObject);
		procedure PublicRBClick(Sender: TObject);
		procedure EncryptFilesComboChange(Sender: TObject);
		procedure EncryptFilesPwdButtonClick(Sender: TObject);
		procedure FieldChanged(Sender: TObject);
		class function ShowAccounts(ParentWindow: HWND; PasswordManager: IPasswordManager; Account: WideString): Boolean;
		procedure FormActivate(Sender: TObject);
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
	private
		FPresenter: TAccountsPresenter;
		procedure AutoFitListViewColumns(LV: TListView);

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
		procedure SetEncryptFilenames(Value: Boolean);
		function GetEncryptFilenames: Boolean;
		procedure SetEncryptPasswordButtonEnabled(Value: Boolean);
		procedure SetEncryptFilenamesCBEnabled(Value: Boolean);
		procedure SetAccountsPanelVisible(Value: Boolean);
		procedure SetSharesPanelVisible(Value: Boolean);
		procedure SetApplyButtonEnabled(Value: Boolean);
		function ConfirmDiscardAccountChanges(const AccountName: WideString): TConfirmSaveResult;
	public

	end;

implementation

{$R *.dfm}

uses
	System.UITypes,
	ProxySettings,
	CloudConstants,
	IniConfigFile;

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
	end
	else
	begin
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
begin
	if LV.Columns.Count >= 2 then
		LV.Column[0].Width := LV.ClientWidth - LV.Column[1].Width;
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
	end
	else
	begin
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

procedure TAccountsForm.SetEncryptFilenamesCBEnabled(Value: Boolean);
begin
	EncryptFilenamesCB.Enabled := Value;
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
