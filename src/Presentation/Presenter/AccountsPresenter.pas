unit AccountsPresenter;

{Presenter for Accounts settings dialog.
	Handles account CRUD, global settings, streaming extensions, and password management.
	Uses MVP pattern to separate business logic from the view (form).}

interface

uses
	System.Classes,
	System.SysUtils,
	AccountSettings,
	StreamingSettings,
	PluginSettings,
	ConnectionSettings,
	ProxySettings,
	TCPasswordManager,
	AccountsManager,
	PluginSettingsManager,
	WFXTypes;

type
	{View interface for the Accounts dialog}
	IAccountsView = interface
		['{B2C3D4E5-F6A7-8901-BCDE-F23456789012}']
		{Account tab controls}
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

		{Global settings - General tab}
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

		{Network settings}
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

		{Description settings}
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

		{Streaming extensions}
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

		{UI actions}
		procedure ShowValidationError(ControlName: WideString; Message: WideString);
		procedure ShowTab(TabIndex: Integer);
		function GetFormHandle: THandle;

		{Dialogs - view is responsible for showing dialogs}
		function ShowEncryptionPasswordDialog(const AccountName: WideString; var CryptedGUID: WideString): Boolean;
	end;

	{Configuration for accounts presenter - injected dependencies}
	TAccountsPresenterConfig = record
		PasswordManager: IPasswordManager;
		ParentWindow: THandle;
	end;

	TAccountsPresenter = class
	private
		FView: IAccountsView;
		FAccountsManager: IAccountsManager;
		FSettingsManager: IPluginSettingsManager;
		FPasswordManager: IPasswordManager;
		FParentWindow: THandle;
		FSelectedAccount: WideString;
		FSettingsApplied: Boolean;

		procedure LoadAccountToView(const AccountName: WideString);
		procedure ClearAccountFields;
		procedure RefreshAccountsList;
		function SavePasswordToManager(const AccountKey, Password: WideString): Boolean;
		function ValidateGlobalSettings: Boolean;
	public
		constructor Create(AView: IAccountsView; AAccountsManager: IAccountsManager; ASettingsManager: IPluginSettingsManager; AConfig: TAccountsPresenterConfig);

		{Initialization}
		procedure Initialize(const InitialAccount: WideString = '');
		procedure LoadGlobalSettingsToView;
		procedure LoadStreamingExtensionsToView;

		{Account operations}
		procedure OnAccountSelected;
		procedure OnApplyAccountClick;
		procedure OnDeleteAccountClick;
		procedure OnPublicAccountChanged;
		procedure OnEncryptModeChanged;
		procedure OnEncryptPasswordClick;

		{Global settings operations}
		procedure OnApplyGlobalSettingsClick;
		procedure OnCloudMaxFileSizeCheckChanged;
		procedure OnProxyUserChanged;
		procedure OnChangeUserAgentChanged;

		{Streaming extensions operations}
		procedure OnStreamingExtensionSelected;
		procedure OnApplyStreamingExtensionClick;
		procedure OnDeleteStreamingExtensionClick;

		{Properties}
		property SettingsApplied: Boolean read FSettingsApplied;
		property SelectedAccount: WideString read FSelectedAccount;
		property PasswordManager: IPasswordManager read FPasswordManager;
	end;

implementation

uses
	CMRConstants,
	LanguageStrings,
	SettingsConstants,
	System.IOUtils,
	WSList;

const
	DOT = '.';

constructor TAccountsPresenter.Create(AView: IAccountsView; AAccountsManager: IAccountsManager; ASettingsManager: IPluginSettingsManager; AConfig: TAccountsPresenterConfig);
begin
	inherited Create;
	FView := AView;
	FAccountsManager := AAccountsManager;
	FSettingsManager := ASettingsManager;
	FPasswordManager := AConfig.PasswordManager;
	FParentWindow := AConfig.ParentWindow;
	FSelectedAccount := '';
	FSettingsApplied := False;
end;

procedure TAccountsPresenter.Initialize(const InitialAccount: WideString);
var
	AccountsList: TStringList;
	AccountsArray: TWSList;
	I, Index: Integer;
begin
	FSelectedAccount := InitialAccount;

	{Load accounts list}
	AccountsArray := FAccountsManager.GetAccountsList;
	AccountsList := TStringList.Create;
	try
		for I := 0 to AccountsArray.Count - 1 do
			AccountsList.Add(AccountsArray[I]);
		FView.SetAccountsList(AccountsList);
	finally
		AccountsList.Free;
	end;

	{Select initial account}
	Index := -1;
	if FSelectedAccount <> '' then
	begin
		for I := 0 to AccountsArray.Count - 1 do
			if AccountsArray[I] = FSelectedAccount then
			begin
				Index := I;
				Break;
			end;

		if Index >= 0 then
			FView.SelectAccount(Index)
		else if AccountsArray.Count > 0 then
			FView.SelectAccount(0);
	end
	else if AccountsArray.Count > 0 then
		FView.SelectAccount(0);

	{Load global settings}
	LoadGlobalSettingsToView;

	{Load streaming extensions}
	LoadStreamingExtensionsToView;

	{Trigger account selection to populate fields}
	OnAccountSelected;
end;

procedure TAccountsPresenter.LoadGlobalSettingsToView;
var
	Settings: TPluginSettings;
begin
	Settings := FSettingsManager.GetSettings;

	{General settings}
	FView.SetLoadSSLFromPluginDir(Settings.LoadSSLDLLOnlyFromPluginDir);
	FView.SetPreserveFileTime(Settings.PreserveFileTime);
	FView.SetCopyBetweenAccountsMode(Settings.CopyBetweenAccountsMode);

	{Cloud max file size}
	FView.SetCloudMaxFileSize(Settings.CloudMaxFileSize);
	FView.SetCloudMaxFileSizeEnabled(Settings.CloudMaxFileSize <> CLOUD_MAX_FILESIZE_DEFAULT);
	FView.SetCloudMaxFileSizeEditEnabled(Settings.CloudMaxFileSize <> CLOUD_MAX_FILESIZE_DEFAULT);

	{Operation modes}
	FView.SetChunkOverwriteMode(Settings.ChunkOverwriteMode);
	FView.SetDeleteFailOnUploadMode(Settings.DeleteFailOnUploadMode);
	FView.SetOverwriteLocalMode(Settings.OverwriteLocalMode);
	FView.SetOperationErrorMode(Settings.OperationErrorMode);
	FView.SetRetryAttempts(Settings.RetryAttempts);
	FView.SetAttemptWait(Settings.AttemptWait);

	{Threading and logging}
	FView.SetDisableMultiThreading(Settings.DisableMultiThreading);
	FView.SetLogUserSpace(Settings.LogUserSpace);
	FView.SetIconsMode(Settings.IconsMode);

	{Download/display settings}
	FView.SetDownloadLinksEncode(Settings.DownloadLinksEncode);
	FView.SetAutoUpdateDownloadListing(Settings.AutoUpdateDownloadListing);
	FView.SetShowTrashFolders(Settings.ShowTrashFolders);
	FView.SetShowSharedFolders(Settings.ShowSharedFolders);
	FView.SetShowInvitesFolders(Settings.ShowInvitesFolders);
	FView.SetPrecalculateHash(Settings.PrecalculateHash);
	FView.SetCheckCRC(Settings.CheckCRC);
	FView.SetHashCalculatorStrategy(Settings.HashCalculatorStrategy);

	{Network settings}
	FView.SetSocketTimeout(Settings.ConnectionSettings.SocketTimeout);
	FView.SetUploadBPS(Settings.ConnectionSettings.UploadBPS);
	FView.SetDownloadBPS(Settings.ConnectionSettings.DownloadBPS);

	{Proxy settings}
	FView.SetProxyType(Settings.ConnectionSettings.ProxySettings.ProxyType);
	FView.SetProxyServer(Settings.ConnectionSettings.ProxySettings.Server);
	FView.SetProxyPort(Settings.ConnectionSettings.ProxySettings.Port);
	FView.SetProxyUser(Settings.ConnectionSettings.ProxySettings.User);
	FView.SetProxyPassword(Settings.ConnectionSettings.ProxySettings.Password);
	FView.SetProxyUseTCPasswordManager(Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager);
	FView.SetProxyTCPasswordManagerEnabled(Settings.ConnectionSettings.ProxySettings.User <> '');

	{User agent}
	FView.SetUserAgent(Settings.ConnectionSettings.UserAgent);
	FView.SetChangeUserAgent(Settings.ConnectionSettings.UserAgent <> DEFAULT_USERAGENT);
	FView.SetUserAgentReadOnly(Settings.ConnectionSettings.UserAgent = DEFAULT_USERAGENT);

	{Description settings}
	FView.SetDescriptionEnabled(Settings.DescriptionEnabled);
	FView.SetDescriptionEditorEnabled(Settings.DescriptionEditorEnabled);
	FView.SetDescriptionCopyToCloud(Settings.DescriptionCopyToCloud);
	FView.SetDescriptionCopyFromCloud(Settings.DescriptionCopyFromCloud);
	FView.SetDescriptionTrackCloudFS(Settings.DescriptionTrackCloudFS);
	FView.SetDescriptionFileName(Settings.DescriptionFileName);
end;

procedure TAccountsPresenter.LoadStreamingExtensionsToView;
var
	ExtList: TStringList;
begin
	ExtList := TStringList.Create;
	try
		FSettingsManager.GetStreamingExtensionsList(ExtList);
		FView.SetStreamingExtensionsList(ExtList);
	finally
		ExtList.Free;
	end;
end;

procedure TAccountsPresenter.LoadAccountToView(const AccountName: WideString);
var
	AccSettings: TAccountSettings;
begin
	AccSettings := FAccountsManager.GetAccountSettings(AccountName);
	FSelectedAccount := AccSettings.User;

	FView.SetAccountName(AccSettings.Account);
	FView.SetEmail(AccSettings.Email);
	FView.SetPassword(AccSettings.Password);
	FView.SetUseTCPasswordManager(AccSettings.UseTCPasswordManager);
	FView.SetUnlimitedFileSize(AccSettings.UnlimitedFilesize);
	FView.SetSplitLargeFiles(AccSettings.SplitLargeFiles);
	FView.SetPublicAccount(AccSettings.PublicAccount);
	FView.SetPublicUrl(AccSettings.PublicUrl);
	FView.SetEncryptFilesMode(AccSettings.EncryptFilesMode);
	FView.SetEncryptFilenames(AccSettings.EncryptFilenames);

	{Update encrypt button state}
	OnEncryptModeChanged;
	{Update panels visibility}
	OnPublicAccountChanged;
end;

procedure TAccountsPresenter.ClearAccountFields;
begin
	FView.SetAccountName('');
	FView.SetEmail('');
	FView.SetPassword('');
	FView.SetUseTCPasswordManager(False);
	FView.SetEncryptPasswordButtonEnabled(False);
end;

procedure TAccountsPresenter.RefreshAccountsList;
var
	AccountsList: TStringList;
	AccountsArray: TWSList;
	I: Integer;
begin
	AccountsArray := FAccountsManager.GetAccountsList;
	AccountsList := TStringList.Create;
	try
		for I := 0 to AccountsArray.Count - 1 do
			AccountsList.Add(AccountsArray[I]);
		FView.SetAccountsList(AccountsList);
	finally
		AccountsList.Free;
	end;
end;

function TAccountsPresenter.SavePasswordToManager(const AccountKey, Password: WideString): Boolean;
begin
	Result := False;
	case FPasswordManager.SetPassword(AccountKey, Password) of
		FS_FILE_OK:
			Result := True;
		FS_FILE_NOTSUPPORTED,
		FS_FILE_WRITEERROR:
			Result := False;
	end;
end;

function TAccountsPresenter.ValidateGlobalSettings: Boolean;
var
	FileName: WideString;
begin
	Result := False;
	FileName := FView.GetDescriptionFileName;
	if not TPath.HasValidFileNameChars(FileName, False) then
	begin
		FView.ShowTab(3); {Comments tab}
		FView.ShowValidationError('DescriptionFileName', ERR_ACCOUNT_HAS_INVALID_SYMBOL);
		Exit;
	end;
	Result := True;
end;

procedure TAccountsPresenter.OnAccountSelected;
var
	AccountName: WideString;
begin
	AccountName := FView.GetSelectedAccountName;
	if AccountName <> '' then
		LoadAccountToView(AccountName)
	else
		ClearAccountFields;

	OnPublicAccountChanged;
end;

procedure TAccountsPresenter.OnApplyAccountClick;
var
	AccSettings: TAccountSettings;
begin
	if FView.GetAccountName = '' then
		Exit;

	{Build account settings from view}
	AccSettings.Account := FView.GetAccountName;
	AccSettings.Email := FView.GetEmail;
	AccSettings.Password := FView.GetPassword;
	AccSettings.UseTCPasswordManager := FView.GetUseTCPasswordManager;
	AccSettings.UnlimitedFilesize := FView.GetUnlimitedFileSize;
	AccSettings.SplitLargeFiles := FView.GetSplitLargeFiles;
	AccSettings.TwostepAuth := False; {Deprecated}
	AccSettings.PublicAccount := FView.GetPublicAccount;
	AccSettings.PublicUrl := FView.GetPublicUrl;
	AccSettings.EncryptFilesMode := FView.GetEncryptFilesMode;
	AccSettings.EncryptFilenames := FView.GetEncryptFilenames;
	AccSettings.AuthMethod := CLOUD_AUTH_METHOD_OAUTH_APP;
	AccSettings.UseAppPassword := True;

	{Handle TC password manager}
	if AccSettings.UseTCPasswordManager then
	begin
		if not SavePasswordToManager(AccSettings.Account, AccSettings.Password) then
			Exit;
		AccSettings.Password := '';
	end;

	{Save account}
	FAccountsManager.SetAccountSettings(AccSettings);

	{Refresh list}
	RefreshAccountsList;

	OnAccountSelected;
end;

procedure TAccountsPresenter.OnDeleteAccountClick;
var
	AccountName: WideString;
begin
	AccountName := FView.GetSelectedAccountName;
	if AccountName = '' then
		Exit;

	FAccountsManager.DeleteAccount(AccountName);

	{Refresh list}
	RefreshAccountsList;

	OnAccountSelected;
end;

procedure TAccountsPresenter.OnPublicAccountChanged;
begin
	FView.SetSharesPanelVisible(FView.GetPublicAccount);
	FView.SetAccountsPanelVisible(not FView.GetPublicAccount);
end;

procedure TAccountsPresenter.OnEncryptModeChanged;
begin
	FView.SetEncryptPasswordButtonEnabled(FView.GetEncryptFilesMode = EncryptModeAlways);
end;

procedure TAccountsPresenter.OnEncryptPasswordClick;
var
	CryptedGUID: WideString;
	AccSettings: TAccountSettings;
begin
	if FView.ShowEncryptionPasswordDialog(FSelectedAccount, CryptedGUID) then
	begin
		if CryptedGUID <> '' then
		begin
			AccSettings := FAccountsManager.GetAccountSettings(FSelectedAccount);
			AccSettings.CryptedGUIDFiles := CryptedGUID;
			FAccountsManager.SetAccountSettings(AccSettings);
		end;
	end;
end;

procedure TAccountsPresenter.OnApplyGlobalSettingsClick;
var
	Settings: TPluginSettings;
begin
	if not ValidateGlobalSettings then
		Exit;

	Settings := FSettingsManager.GetSettings;

	{General settings}
	Settings.LoadSSLDLLOnlyFromPluginDir := FView.GetLoadSSLFromPluginDir;
	Settings.PreserveFileTime := FView.GetPreserveFileTime;
	Settings.CopyBetweenAccountsMode := FView.GetCopyBetweenAccountsMode;

	{Cloud max file size}
	if FView.GetCloudMaxFileSizeEnabled then
		Settings.CloudMaxFileSize := FView.GetCloudMaxFileSize
	else
		Settings.CloudMaxFileSize := CLOUD_MAX_FILESIZE_DEFAULT;

	{Operation modes}
	Settings.ChunkOverwriteMode := FView.GetChunkOverwriteMode;
	Settings.DeleteFailOnUploadMode := FView.GetDeleteFailOnUploadMode;
	Settings.OverwriteLocalMode := FView.GetOverwriteLocalMode;
	Settings.OperationErrorMode := FView.GetOperationErrorMode;
	Settings.RetryAttempts := FView.GetRetryAttempts;
	Settings.AttemptWait := FView.GetAttemptWait;

	{Threading and logging}
	Settings.DisableMultiThreading := FView.GetDisableMultiThreading;
	Settings.LogUserSpace := FView.GetLogUserSpace;
	Settings.IconsMode := FView.GetIconsMode;

	{Download/display settings}
	Settings.DownloadLinksEncode := FView.GetDownloadLinksEncode;
	Settings.AutoUpdateDownloadListing := FView.GetAutoUpdateDownloadListing;
	Settings.ShowTrashFolders := FView.GetShowTrashFolders;
	Settings.ShowSharedFolders := FView.GetShowSharedFolders;
	Settings.ShowInvitesFolders := FView.GetShowInvitesFolders;
	Settings.PrecalculateHash := FView.GetPrecalculateHash;
	Settings.CheckCRC := FView.GetCheckCRC;
	Settings.HashCalculatorStrategy := FView.GetHashCalculatorStrategy;

	{Network settings}
	Settings.ConnectionSettings.SocketTimeout := FView.GetSocketTimeout;
	Settings.ConnectionSettings.UploadBPS := FView.GetUploadBPS;
	Settings.ConnectionSettings.DownloadBPS := FView.GetDownloadBPS;

	{Proxy settings}
	Settings.ConnectionSettings.ProxySettings.ProxyType := FView.GetProxyType;
	Settings.ConnectionSettings.ProxySettings.Server := FView.GetProxyServer;
	Settings.ConnectionSettings.ProxySettings.Port := FView.GetProxyPort;
	Settings.ConnectionSettings.ProxySettings.User := FView.GetProxyUser;
	Settings.ConnectionSettings.ProxySettings.Password := FView.GetProxyPassword;
	Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager := FView.GetProxyUseTCPasswordManager;

	{User agent}
	if FView.GetChangeUserAgent then
		Settings.ConnectionSettings.UserAgent := FView.GetUserAgent;

	{Description settings}
	Settings.DescriptionEnabled := FView.GetDescriptionEnabled;
	Settings.DescriptionEditorEnabled := FView.GetDescriptionEditorEnabled;
	Settings.DescriptionCopyToCloud := FView.GetDescriptionCopyToCloud;
	Settings.DescriptionCopyFromCloud := FView.GetDescriptionCopyFromCloud;
	Settings.DescriptionTrackCloudFS := FView.GetDescriptionTrackCloudFS;
	Settings.DescriptionFileName := FView.GetDescriptionFileName;

	{Save settings}
	FSettingsManager.SetSettings(Settings);
	FSettingsManager.Save;
	FSettingsApplied := True;

	{Handle proxy password with TC password manager}
	if FView.GetProxyUseTCPasswordManager then
	begin
		if SavePasswordToManager(PASSWORD_KEY_PROXY + FView.GetProxyUser, FView.GetProxyPassword) then
		begin
			FView.SetProxyPassword('');
			Settings.ConnectionSettings.ProxySettings.Password := '';
			FSettingsManager.SetSettings(Settings);
			FSettingsManager.Save;
		end;
	end;
end;

procedure TAccountsPresenter.OnCloudMaxFileSizeCheckChanged;
begin
	FView.SetCloudMaxFileSizeEditEnabled(FView.GetCloudMaxFileSizeEnabled);
end;

procedure TAccountsPresenter.OnProxyUserChanged;
begin
	FView.SetProxyTCPasswordManagerEnabled(FView.GetProxyUser <> '');
end;

procedure TAccountsPresenter.OnChangeUserAgentChanged;
begin
	FView.SetUserAgentReadOnly(not FView.GetChangeUserAgent);
end;

procedure TAccountsPresenter.OnStreamingExtensionSelected;
var
	ExtName: WideString;
	ExtSettings: TStreamingSettings;
begin
	ExtName := FView.GetSelectedStreamingExtension;
	if ExtName <> '' then
	begin
		ExtSettings := FSettingsManager.GetStreamingSettings(DOT + ExtName);
		FView.SetStreamingExtension(ExtName);
		FView.SetStreamingCommand(ExtSettings.Command);
		FView.SetStreamingParameters(ExtSettings.Parameters);
		FView.SetStreamingStartPath(ExtSettings.StartPath);
		FView.SetStreamingType(ExtSettings.Format);
	end
	else
		FView.ClearStreamingFields;
end;

procedure TAccountsPresenter.OnApplyStreamingExtensionClick;
var
	ExtSettings: TStreamingSettings;
	ExtName: WideString;
begin
	ExtName := FView.GetStreamingExtension;
	if ExtName = '' then
		Exit;

	ExtSettings.Command := FView.GetStreamingCommand;
	ExtSettings.Parameters := FView.GetStreamingParameters;
	ExtSettings.StartPath := FView.GetStreamingStartPath;
	ExtSettings.Format := FView.GetStreamingType;

	FSettingsManager.SetStreamingSettings(DOT + ExtName, ExtSettings);
	LoadStreamingExtensionsToView;
end;

procedure TAccountsPresenter.OnDeleteStreamingExtensionClick;
var
	ExtName: WideString;
begin
	ExtName := FView.GetSelectedStreamingExtension;
	if ExtName = '' then
		Exit;

	FSettingsManager.RemoveStreamingExtension(ExtName);
	LoadStreamingExtensionsToView;
end;

end.
