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
	PasswordManager,
	AccountsManager,
	PluginSettingsManager,
	WFXTypes;

type
	{Display data for the accounts tab TListView}
	TAccountDisplayItem = record
		Name: WideString;
		TypeLabel: WideString;
		EncryptionLabel: WideString;
	end;

	{Display data for the streaming extensions TListView}
	TStreamingDisplayItem = record
		Extension: WideString;
		TypeLabel: WideString;
	end;

	{Result of the confirm-discard-changes dialog}
	TConfirmSaveResult = (csrSave, csrDiscard, csrCancel);

	{View interface for the Accounts dialog}
	IAccountsView = interface
		['{6C670119-9EF1-441B-8FB1-7997531C92F2}']

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
		procedure SetResetUserAgentEnabled(Value: Boolean);

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

		{Global settings apply state}
		procedure SetGlobalSettingsApplyEnabled(Value: Boolean);

		{Proxy controls state}
		procedure SetProxyControlsEnabled(Value: Boolean);

		{UI actions}
		procedure ShowDescriptionFileNameError(Message: WideString);
		procedure ShowTab(TabIndex: Integer);
		function GetFormHandle: THandle;

		{Dialogs - view is responsible for showing dialogs}
		function ShowEncryptionPasswordDialog(const AccountName: WideString; var CryptedGUID: WideString): Boolean;

		{Accounts tab}
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
		function ConfirmDiscardAccountChanges(const AccountName: WideString): TConfirmSaveResult;
		procedure ShowAccountNameError(Message: WideString);
		function ConfirmAccountOverwrite(const AccountName: WideString): Boolean;

		{Cipher profile combo}
		procedure SetCipherProfileItems(const Items: TArray<WideString>);
		procedure SetCipherProfileIndex(Value: Integer);
		function GetCipherProfileIndex: Integer;
		procedure SetCipherProfileEnabled(Value: Boolean);
		function ShowCipherChangeWarning: Boolean;
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
		FSettingsApplied: Boolean;

		{Dirty tracking state - accounts tab}
		FSelectedAccount: WideString;
		FDirty: Boolean;
		FUpdating: Boolean;
		FCancellingSwitch: Boolean;

		{Dirty tracking state - global settings (shared across Global/Network/Comments tabs)}
		FGlobalSettingsDirty: Boolean;
		FGlobalSettingsUpdating: Boolean;

		{Dirty tracking state - streaming tab}
		FStreamingSelectedExtension: WideString;
		FStreamingDirty: Boolean;
		FStreamingUpdating: Boolean;
		FStreamingCancellingSwitch: Boolean;

		{Cipher profile state}
		FCipherProfileIds: TArray<WideString>;
		FPreviousCipherProfileIndex: Integer;

		function SavePasswordToManager(const AccountKey, Password: WideString): Boolean;
		function ValidateGlobalSettings: Boolean;

		{Accounts tab helpers}
		procedure RefreshAccountsList;
		procedure LoadAccountToView(const AccountName: WideString);
		procedure ClearAccountFields;
		function EncryptionModeToLabel(Mode: Integer): WideString;
		procedure SetDirty(Value: Boolean);
		function SaveAccountFromView: Boolean;
		procedure SelectAccountByName(const Name: WideString);
		function CheckDirty: Boolean;

		{Cipher profile helpers}
		procedure PopulateCipherProfiles;
		function CipherProfileIdToIndex(const ProfileId: WideString): Integer;
		function IndexToCipherProfileId(Index: Integer): WideString;

		{Global settings dirty tracking}
		procedure SetGlobalSettingsDirty(Value: Boolean);

		{Streaming tab helpers}
		procedure SetStreamingDirty(Value: Boolean);
		procedure LoadStreamingExtensionToView(const ExtName: WideString);
		procedure ClearStreamingFields;
		function SaveStreamingExtensionFromView: Boolean;
		procedure SelectStreamingExtensionByName(const Name: WideString);
		function CheckStreamingDirty: Boolean;
		function StreamingFormatToLabel(Format: Integer): WideString;
	public
		constructor Create(AView: IAccountsView; AAccountsManager: IAccountsManager; ASettingsManager: IPluginSettingsManager; AConfig: TAccountsPresenterConfig);

		{Initialization}
		procedure Initialize(const InitialAccount: WideString = '');
		procedure LoadGlobalSettingsToView;
		procedure RefreshStreamingExtensionsList;

		{Account operations}
		procedure OnAccountSelected;
		procedure OnAddAccountClick;
		procedure OnDeleteAccountClick;
		procedure OnApplyAccountClick;
		procedure OnAccountTypeChanged;
		procedure OnEncryptModeChanged;
		procedure OnEncryptPasswordClick;
		procedure OnCipherProfileChanged;
		procedure OnFieldChanged;

		{Global settings operations}
		procedure OnApplyGlobalSettingsClick;
		procedure OnCloudMaxFileSizeCheckChanged;
		procedure OnProxyUserChanged;
		procedure OnProxyTypeChanged;
		procedure OnChangeUserAgentChanged;
		procedure OnResetUserAgentClick;
		procedure OnGlobalSettingsFieldChanged;

		{Streaming extensions operations}
		procedure OnStreamingExtensionSelected;
		procedure OnAddStreamingExtensionClick;
		procedure OnApplyStreamingExtensionClick;
		procedure OnDeleteStreamingExtensionClick;
		procedure OnStreamingFieldChanged;

		{Properties}
		property SettingsApplied: Boolean read FSettingsApplied;
		property SelectedAccount: WideString read FSelectedAccount;
		property PasswordManager: IPasswordManager read FPasswordManager;
	end;

implementation

uses
	CloudConstants,
	LanguageStrings,
	SettingsConstants,
	CipherProfile,
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
	FSettingsApplied := False;
	FSelectedAccount := '';
	FDirty := False;
	FUpdating := False;
	FCancellingSwitch := False;
	FGlobalSettingsDirty := False;
	FGlobalSettingsUpdating := False;
	FStreamingSelectedExtension := '';
	FStreamingDirty := False;
	FStreamingUpdating := False;
	FStreamingCancellingSwitch := False;
end;

procedure TAccountsPresenter.Initialize(const InitialAccount: WideString);
var
	AccountsArray: TWSList;
	I, Index: Integer;
begin
	AccountsArray := FAccountsManager.GetAccountsList;

	{Find initial account index}
	Index := -1;
	if InitialAccount <> '' then
		for I := 0 to AccountsArray.Count - 1 do
			if AccountsArray[I] = InitialAccount then
			begin
				Index := I;
				Break;
			end;

	{Populate cipher profile combo}
	PopulateCipherProfiles;

	{Load global settings}
	LoadGlobalSettingsToView;

	{Load streaming extensions}
	RefreshStreamingExtensionsList;

	{Initialize accounts tab}
	FUpdating := True;
	try
		RefreshAccountsList;
		if Index >= 0 then
			FView.SelectAccount(Index)
		else if AccountsArray.Count > 0 then
			FView.SelectAccount(0);
	finally
		FUpdating := False;
	end;
	OnAccountSelected;
	SetDirty(False);
end;

procedure TAccountsPresenter.LoadGlobalSettingsToView;
var
	Settings: TPluginSettings;
begin
	FGlobalSettingsUpdating := True;
	try
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
		FView.SetProxyTCPasswordManagerEnabled(
			(Settings.ConnectionSettings.ProxySettings.User <> '') and
			(Settings.ConnectionSettings.ProxySettings.ProxyType <> ProxyNone)
		);
		FView.SetProxyControlsEnabled(Settings.ConnectionSettings.ProxySettings.ProxyType <> ProxyNone);

		{User agent}
		FView.SetUserAgent(Settings.ConnectionSettings.UserAgent);
		FView.SetChangeUserAgent(Settings.ConnectionSettings.UserAgent <> DEFAULT_USERAGENT);
		FView.SetUserAgentReadOnly(Settings.ConnectionSettings.UserAgent = DEFAULT_USERAGENT);
		FView.SetResetUserAgentEnabled(Settings.ConnectionSettings.UserAgent <> DEFAULT_USERAGENT);

		{Description settings}
		FView.SetDescriptionEnabled(Settings.DescriptionEnabled);
		FView.SetDescriptionEditorEnabled(Settings.DescriptionEditorEnabled);
		FView.SetDescriptionCopyToCloud(Settings.DescriptionCopyToCloud);
		FView.SetDescriptionCopyFromCloud(Settings.DescriptionCopyFromCloud);
		FView.SetDescriptionTrackCloudFS(Settings.DescriptionTrackCloudFS);
		FView.SetDescriptionFileName(Settings.DescriptionFileName);
	finally
		FGlobalSettingsUpdating := False;
	end;
	SetGlobalSettingsDirty(False);
end;

procedure TAccountsPresenter.RefreshStreamingExtensionsList;
var
	ExtList: TStringList;
	Items: TArray<TStreamingDisplayItem>;
	ExtSettings: TStreamingSettings;
	I: Integer;
begin
	ExtList := TStringList.Create;
	try
		FSettingsManager.GetStreamingExtensionsList(ExtList);
		SetLength(Items, ExtList.Count);
		for I := 0 to ExtList.Count - 1 do
		begin
			Items[I].Extension := ExtList[I];
			ExtSettings := FSettingsManager.GetStreamingSettings(DOT + ExtList[I]);
			Items[I].TypeLabel := StreamingFormatToLabel(ExtSettings.Format);
		end;
		FView.SetStreamingExtensionsList(Items);
	finally
		ExtList.Free;
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
		FView.ShowDescriptionFileNameError(ERR_ACCOUNT_HAS_INVALID_SYMBOL);
		Exit;
	end;
	Result := True;
end;

{Accounts tab helpers}

function TAccountsPresenter.EncryptionModeToLabel(Mode: Integer): WideString;
begin
	case Mode of
		EncryptModeAlways: Result := 'Alw';
		EncryptModeAskOnce: Result := 'Ask';
	else
		Result := 'No';
	end;
end;

procedure TAccountsPresenter.RefreshAccountsList;
var
	AccountsArray: TWSList;
	Items: TArray<TAccountDisplayItem>;
	AccSettings: TAccountSettings;
	I: Integer;
begin
	AccountsArray := FAccountsManager.GetAccountsList;
	SetLength(Items, AccountsArray.Count);
	for I := 0 to AccountsArray.Count - 1 do
	begin
		AccSettings := FAccountsManager.GetAccountSettings(AccountsArray[I]);
		Items[I].Name := AccountsArray[I];
		if AccSettings.PublicAccount then
			Items[I].TypeLabel := 'Public'
		else
			Items[I].TypeLabel := 'Private';
		Items[I].EncryptionLabel := EncryptionModeToLabel(AccSettings.EncryptFilesMode);
	end;
	FView.SetAccountsList(Items);
end;

procedure TAccountsPresenter.LoadAccountToView(const AccountName: WideString);
var
	AccSettings: TAccountSettings;
	WasUpdating: Boolean;
begin
	WasUpdating := FUpdating;
	FUpdating := True;
	try
		AccSettings := FAccountsManager.GetAccountSettings(AccountName);
		FSelectedAccount := AccountName;

		FView.SetAccountName(AccSettings.Account);
		FView.SetIsPrivate(not AccSettings.PublicAccount);
		FView.SetEmail(AccSettings.Email);
		FView.SetPassword(AccSettings.Password);
		FView.SetUseTCPasswordManager(AccSettings.UseTCPasswordManager);
		FView.SetUnlimitedFileSize(AccSettings.UnlimitedFilesize);
		FView.SetSplitLargeFiles(AccSettings.SplitLargeFiles);
		FView.SetPublicUrl(AccSettings.PublicUrl);
		FView.SetEncryptFilesMode(AccSettings.EncryptFilesMode);
		FView.SetCipherProfileIndex(CipherProfileIdToIndex(AccSettings.CipherProfileId));
		FPreviousCipherProfileIndex := FView.GetCipherProfileIndex;

		{Update encrypt controls state}
		OnEncryptModeChanged;
		{Update panels visibility}
		OnAccountTypeChanged;
	finally
		FUpdating := WasUpdating;
	end;
	SetDirty(False);
end;

procedure TAccountsPresenter.ClearAccountFields;
var
	WasUpdating: Boolean;
begin
	WasUpdating := FUpdating;
	FUpdating := True;
	try
		FSelectedAccount := '';
		FView.SetAccountName('');
		FView.SetIsPrivate(True);
		FView.SetEmail('');
		FView.SetPassword('');
		FView.SetUseTCPasswordManager(False);
		FView.SetUnlimitedFileSize(False);
		FView.SetSplitLargeFiles(False);
		FView.SetPublicUrl('');
		FView.SetEncryptFilesMode(EncryptModeNone);
		FView.SetCipherProfileIndex(0);
		FPreviousCipherProfileIndex := 0;
		FView.SetEncryptPasswordButtonEnabled(False);
		FView.SetCipherProfileEnabled(False);
		OnAccountTypeChanged;
	finally
		FUpdating := WasUpdating;
	end;
	SetDirty(False);
end;

{Accounts dirty-tracking block (SetDirty..OnDeleteAccountClick) mirrors
 the Streaming block below. The duplication is intentional: both tabs
 manage independent UI state with subtle comctl32 focus-transfer quirks,
 and keeping them explicit avoids fragile abstraction over visual form logic.}
procedure TAccountsPresenter.SetDirty(Value: Boolean);
begin
	FDirty := Value;
	FView.SetApplyButtonEnabled(Value);
end;

procedure TAccountsPresenter.OnFieldChanged;
begin
	if not FUpdating then
		SetDirty(True);
end;

function TAccountsPresenter.SaveAccountFromView: Boolean;
var
	AccSettings: TAccountSettings;
	AccountName: WideString;
	IsRename, IsDuplicate: Boolean;
	AccountsList: TWSList;
begin
	Result := False;

	AccountName := FView.GetAccountName;
	if AccountName = '' then
		Exit;

	{INI section names cannot contain bracket characters}
	if (Pos('[', AccountName) > 0) or (Pos(']', AccountName) > 0) then
	begin
		FView.ShowAccountNameError(ERR_ACCOUNT_NAME_INVALID_CHARS);
		Exit;
	end;

	IsRename := (FSelectedAccount <> '') and (FSelectedAccount <> AccountName);

	{Check if target name collides with an existing account (not ourselves)}
	IsDuplicate := False;
	AccountsList := FAccountsManager.GetAccountsList;
	if AccountsList.Contains(AccountName) and (AccountName <> FSelectedAccount) then
		IsDuplicate := True;

	if IsDuplicate then
	begin
		if not FView.ConfirmAccountOverwrite(AccountName) then
			Exit;
		{User confirmed: delete the target before rename/save so it is cleanly replaced}
		FAccountsManager.DeleteAccount(AccountName);
	end;

	AccSettings := Default(TAccountSettings);
	AccSettings.Account := AccountName;
	AccSettings.PublicAccount := not FView.GetIsPrivate;
	AccSettings.Email := FView.GetEmail;
	AccSettings.Password := FView.GetPassword;
	AccSettings.UseTCPasswordManager := FView.GetUseTCPasswordManager;
	AccSettings.UnlimitedFilesize := FView.GetUnlimitedFileSize;
	AccSettings.SplitLargeFiles := FView.GetSplitLargeFiles;
	AccSettings.PublicUrl := FView.GetPublicUrl;
	AccSettings.EncryptFilesMode := FView.GetEncryptFilesMode;
	AccSettings.CipherProfileId := IndexToCipherProfileId(FView.GetCipherProfileIndex);
	AccSettings.AuthMethod := CLOUD_AUTH_METHOD_OAUTH_APP;
	AccSettings.UseAppPassword := True;

	if AccSettings.UseTCPasswordManager then
	begin
		if not SavePasswordToManager(AccSettings.Account, AccSettings.Password) then
			Exit;
		AccSettings.Password := '';
	end;

	{Detect account rename: old section must be replaced with the new one}
	if IsRename then
		FAccountsManager.RenameAccount(FSelectedAccount, AccSettings.Account);

	FAccountsManager.SetAccountSettings(AccSettings);
	Result := True;
end;

procedure TAccountsPresenter.SelectAccountByName(const Name: WideString);
var
	AccountsArray: TWSList;
	I: Integer;
begin
	if Name = '' then
		Exit;

	AccountsArray := FAccountsManager.GetAccountsList;
	for I := 0 to AccountsArray.Count - 1 do
		if AccountsArray[I] = Name then
		begin
			FView.SelectAccount(I);
			Exit;
		end;
end;

function TAccountsPresenter.CheckDirty: Boolean;
begin
	{Returns True if it is safe to proceed (not dirty, or user chose Save/Discard).
	 Returns False if user chose Cancel (abort the operation).}
	Result := True;
	if not FDirty then
		Exit;

	{FUpdating blocks re-entrant OnAccountSelected during dialog focus
	 transfer and SelectAccountByName. FCancellingSwitch (set in Cancel
	 branch) survives beyond this method to block a post-handler comctl32 event.}
	FUpdating := True;
	try
		case FView.ConfirmDiscardAccountChanges(FSelectedAccount) of
			csrSave:
				SaveAccountFromView;
			csrCancel:
			begin
				FCancellingSwitch := True;
				SelectAccountByName(FSelectedAccount);
				Result := False;
			end;
			{csrDiscard: just proceed}
		end;
	finally
		FUpdating := False;
	end;
end;

{Account operations}

procedure TAccountsPresenter.OnAccountSelected;
var
	AccountName: WideString;
begin
	{Skip when called re-entrantly from programmatic selection changes}
	if FUpdating then
		Exit;

	{Suppress the post-handler comctl32 event that re-selects the clicked item
	 after Cancel in CheckDirty. Reselect the previous account.}
	if FCancellingSwitch then
	begin
		FCancellingSwitch := False;
		FUpdating := True;
		try
			SelectAccountByName(FSelectedAccount);
		finally
			FUpdating := False;
		end;
		Exit;
	end;

	if not CheckDirty then
		Exit;

	AccountName := FView.GetSelectedAccountName;
	if AccountName <> '' then
		LoadAccountToView(AccountName)
	else
		ClearAccountFields;
end;

procedure TAccountsPresenter.OnAddAccountClick;
begin
	if not CheckDirty then
		Exit;

	{Deselect list and clear fields for new account entry}
	FView.SelectAccount(-1);
	ClearAccountFields;
end;

procedure TAccountsPresenter.OnDeleteAccountClick;
var
	AccountName: WideString;
begin
	AccountName := FView.GetSelectedAccountName;
	if AccountName = '' then
		Exit;

	FAccountsManager.DeleteAccount(AccountName);
	SetDirty(False);

	RefreshAccountsList;
	OnAccountSelected;
end;

procedure TAccountsPresenter.OnApplyAccountClick;
var
	SavedName: WideString;
begin
	SavedName := FView.GetAccountName;
	if not SaveAccountFromView then
		Exit;

	SetDirty(False);

	{Refresh list and reselect the saved account}
	RefreshAccountsList;

	FUpdating := True;
	try
		SelectAccountByName(SavedName);
	finally
		FUpdating := False;
	end;
	OnAccountSelected;
end;

procedure TAccountsPresenter.OnAccountTypeChanged;
begin
	FView.SetSharesPanelVisible(not FView.GetIsPrivate);
	FView.SetAccountsPanelVisible(FView.GetIsPrivate);
	OnFieldChanged;
end;

procedure TAccountsPresenter.OnEncryptModeChanged;
begin
	FView.SetEncryptPasswordButtonEnabled(FView.GetEncryptFilesMode = EncryptModeAlways);
	FView.SetCipherProfileEnabled(FView.GetEncryptFilesMode <> EncryptModeNone);
	OnFieldChanged;
end;

procedure TAccountsPresenter.OnEncryptPasswordClick;
var
	CryptedGUID: WideString;
	AccountName: WideString;
begin
	AccountName := FView.GetSelectedAccountName;
	if AccountName = '' then
		Exit;

	if FView.ShowEncryptionPasswordDialog(AccountName, CryptedGUID) then
	begin
		if CryptedGUID <> '' then
			FAccountsManager.SetCryptedGUID(AccountName, CryptedGUID);
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
		Settings.ConnectionSettings.UserAgent := FView.GetUserAgent
	else
		Settings.ConnectionSettings.UserAgent := DEFAULT_USERAGENT;

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
	SetGlobalSettingsDirty(False);

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

procedure TAccountsPresenter.SetGlobalSettingsDirty(Value: Boolean);
begin
	FGlobalSettingsDirty := Value;
	FView.SetGlobalSettingsApplyEnabled(Value);
end;

procedure TAccountsPresenter.OnGlobalSettingsFieldChanged;
begin
	if not FGlobalSettingsUpdating then
		SetGlobalSettingsDirty(True);
end;

procedure TAccountsPresenter.OnCloudMaxFileSizeCheckChanged;
begin
	FView.SetCloudMaxFileSizeEditEnabled(FView.GetCloudMaxFileSizeEnabled);
	OnGlobalSettingsFieldChanged;
end;

procedure TAccountsPresenter.OnProxyUserChanged;
begin
	FView.SetProxyTCPasswordManagerEnabled(
		(FView.GetProxyUser <> '') and (FView.GetProxyType <> ProxyNone)
	);
	OnGlobalSettingsFieldChanged;
end;

procedure TAccountsPresenter.OnProxyTypeChanged;
begin
	FView.SetProxyControlsEnabled(FView.GetProxyType <> ProxyNone);
	OnProxyUserChanged;
end;

procedure TAccountsPresenter.OnChangeUserAgentChanged;
begin
	FView.SetUserAgentReadOnly(not FView.GetChangeUserAgent);
	FView.SetResetUserAgentEnabled(FView.GetUserAgent <> DEFAULT_USERAGENT);
	OnGlobalSettingsFieldChanged;
end;

procedure TAccountsPresenter.OnResetUserAgentClick;
begin
	FView.SetUserAgent(DEFAULT_USERAGENT);
	FView.SetChangeUserAgent(False);
	FView.SetUserAgentReadOnly(True);
	FView.SetResetUserAgentEnabled(False);
	OnGlobalSettingsFieldChanged;
end;

{Cipher profile helpers}

procedure TAccountsPresenter.PopulateCipherProfiles;
var
	Profiles: TArray<TCipherProfile>;
	DisplayNames: TArray<WideString>;
	I: Integer;
begin
	Profiles := TCipherProfileRegistry.GetProfiles;
	SetLength(FCipherProfileIds, Length(Profiles));
	SetLength(DisplayNames, Length(Profiles));
	for I := 0 to High(Profiles) do
	begin
		FCipherProfileIds[I] := Profiles[I].Id;
		DisplayNames[I] := Profiles[I].DisplayName;
	end;
	FView.SetCipherProfileItems(DisplayNames);
end;

function TAccountsPresenter.CipherProfileIdToIndex(const ProfileId: WideString): Integer;
var
	I: Integer;
begin
	for I := 0 to High(FCipherProfileIds) do
		if FCipherProfileIds[I] = ProfileId then
			Exit(I);
	{Unknown or empty => default (first) profile}
	Result := 0;
end;

function TAccountsPresenter.IndexToCipherProfileId(Index: Integer): WideString;
begin
	if (Index >= 0) and (Index <= High(FCipherProfileIds)) then
		Result := FCipherProfileIds[Index]
	else
		Result := CIPHER_PROFILE_LEGACY_DEFAULT;
end;

procedure TAccountsPresenter.OnCipherProfileChanged;
var
	AccSettings: TAccountSettings;
begin
	if FUpdating then
		Exit;

	{Warn when changing profile on an account that already has encrypted files}
	if FSelectedAccount <> '' then
	begin
		AccSettings := FAccountsManager.GetAccountSettings(FSelectedAccount);
		if (AccSettings.CryptedGUIDFiles <> '') and (FView.GetCipherProfileIndex <> FPreviousCipherProfileIndex) then
		begin
			if not FView.ShowCipherChangeWarning then
			begin
				{User cancelled -- revert to previous selection}
				FUpdating := True;
				try
					FView.SetCipherProfileIndex(FPreviousCipherProfileIndex);
				finally
					FUpdating := False;
				end;
				Exit;
			end;
		end;
	end;

	FPreviousCipherProfileIndex := FView.GetCipherProfileIndex;
	OnFieldChanged;
end;

{Streaming tab helpers}

function TAccountsPresenter.StreamingFormatToLabel(Format: Integer): WideString;
begin
	case Format of
		0: Result := 'None';
		1: Result := 'Off';
		2: Result := 'M3U8';
		3: Result := 'Link';
		4: Result := 'Web';
	else
		Result := '?';
	end;
end;

{Streaming dirty-tracking block -- mirrors the Accounts block above.
 See comment there for why the duplication is intentional.}
procedure TAccountsPresenter.SetStreamingDirty(Value: Boolean);
begin
	FStreamingDirty := Value;
	FView.SetStreamingApplyButtonEnabled(Value);
end;

procedure TAccountsPresenter.LoadStreamingExtensionToView(const ExtName: WideString);
var
	ExtSettings: TStreamingSettings;
	WasUpdating: Boolean;
begin
	WasUpdating := FStreamingUpdating;
	FStreamingUpdating := True;
	try
		ExtSettings := FSettingsManager.GetStreamingSettings(DOT + ExtName);
		FStreamingSelectedExtension := ExtName;

		FView.SetStreamingExtension(ExtName);
		FView.SetStreamingCommand(ExtSettings.Command);
		FView.SetStreamingParameters(ExtSettings.Parameters);
		FView.SetStreamingStartPath(ExtSettings.StartPath);
		FView.SetStreamingType(ExtSettings.Format);
	finally
		FStreamingUpdating := WasUpdating;
	end;
	SetStreamingDirty(False);
end;

procedure TAccountsPresenter.ClearStreamingFields;
var
	WasUpdating: Boolean;
begin
	WasUpdating := FStreamingUpdating;
	FStreamingUpdating := True;
	try
		FStreamingSelectedExtension := '';
		FView.SetStreamingExtension('');
		FView.SetStreamingCommand('');
		FView.SetStreamingParameters('');
		FView.SetStreamingStartPath('');
		FView.SetStreamingType(0);
	finally
		FStreamingUpdating := WasUpdating;
	end;
	SetStreamingDirty(False);
end;

function TAccountsPresenter.SaveStreamingExtensionFromView: Boolean;
var
	ExtSettings: TStreamingSettings;
	ExtName: WideString;
begin
	Result := False;

	ExtName := FView.GetStreamingExtension;
	if ExtName = '' then
		Exit;

	ExtSettings.Command := FView.GetStreamingCommand;
	ExtSettings.Parameters := FView.GetStreamingParameters;
	ExtSettings.StartPath := FView.GetStreamingStartPath;
	ExtSettings.Format := FView.GetStreamingType;

	FSettingsManager.SetStreamingSettings(DOT + ExtName, ExtSettings);
	Result := True;
end;

procedure TAccountsPresenter.SelectStreamingExtensionByName(const Name: WideString);
var
	ExtList: TStringList;
	I: Integer;
begin
	if Name = '' then
		Exit;

	ExtList := TStringList.Create;
	try
		FSettingsManager.GetStreamingExtensionsList(ExtList);
		for I := 0 to ExtList.Count - 1 do
			if ExtList[I] = Name then
			begin
				FView.SelectStreamingExtension(I);
				Exit;
			end;
	finally
		ExtList.Free;
	end;
end;

function TAccountsPresenter.CheckStreamingDirty: Boolean;
begin
	{Returns True if it is safe to proceed (not dirty, or user chose Save/Discard).
	 Returns False if user chose Cancel (abort the operation).}
	Result := True;
	if not FStreamingDirty then
		Exit;

	FStreamingUpdating := True;
	try
		case FView.ConfirmDiscardStreamingChanges(FStreamingSelectedExtension) of
			csrSave:
				SaveStreamingExtensionFromView;
			csrCancel:
			begin
				FStreamingCancellingSwitch := True;
				SelectStreamingExtensionByName(FStreamingSelectedExtension);
				Result := False;
			end;
			{csrDiscard: just proceed}
		end;
	finally
		FStreamingUpdating := False;
	end;
end;

{Streaming extensions operations}

procedure TAccountsPresenter.OnStreamingFieldChanged;
begin
	if not FStreamingUpdating then
		SetStreamingDirty(True);
end;

procedure TAccountsPresenter.OnStreamingExtensionSelected;
var
	ExtName: WideString;
begin
	if FStreamingUpdating then
		Exit;

	{Suppress spurious post-handler event after Cancel}
	if FStreamingCancellingSwitch then
	begin
		FStreamingCancellingSwitch := False;
		FStreamingUpdating := True;
		try
			SelectStreamingExtensionByName(FStreamingSelectedExtension);
		finally
			FStreamingUpdating := False;
		end;
		Exit;
	end;

	if not CheckStreamingDirty then
		Exit;

	ExtName := FView.GetSelectedStreamingExtensionName;
	if ExtName <> '' then
		LoadStreamingExtensionToView(ExtName)
	else
		ClearStreamingFields;
end;

procedure TAccountsPresenter.OnAddStreamingExtensionClick;
begin
	if not CheckStreamingDirty then
		Exit;

	{Deselect list and clear fields for new extension entry}
	FView.SelectStreamingExtension(-1);
	ClearStreamingFields;
end;

procedure TAccountsPresenter.OnApplyStreamingExtensionClick;
var
	SavedName: WideString;
begin
	SavedName := FView.GetStreamingExtension;
	if not SaveStreamingExtensionFromView then
		Exit;

	SetStreamingDirty(False);

	{Refresh list and reselect the saved extension}
	RefreshStreamingExtensionsList;

	FStreamingUpdating := True;
	try
		SelectStreamingExtensionByName(SavedName);
	finally
		FStreamingUpdating := False;
	end;
	OnStreamingExtensionSelected;
end;

procedure TAccountsPresenter.OnDeleteStreamingExtensionClick;
var
	ExtName: WideString;
begin
	ExtName := FView.GetSelectedStreamingExtensionName;
	if ExtName = '' then
		Exit;

	FSettingsManager.RemoveStreamingExtension(ExtName);
	SetStreamingDirty(False);

	RefreshStreamingExtensionsList;
	OnStreamingExtensionSelected;
end;

end.
