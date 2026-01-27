unit AccountsPresenterTest;

interface

uses
	DUnitX.TestFramework,
	AccountsPresenter,
	AccountSettings,
	StreamingSettings,
	PluginSettings,
	ConnectionSettings,
	ProxySettings,
	TCPasswordManager,
	AccountsManager,
	PluginSettingsManager,
	WFXTypes,
	System.Classes,
	System.SysUtils,
	System.Generics.Collections,
	Winapi.Windows;

type
	{Mock implementation of IAccountsView for testing}
	TMockAccountsView = class(TInterfacedObject, IAccountsView)
	private
		FAccountsList: TStringList;
		FSelectedAccountIndex: Integer;
		FAccountName: WideString;
		FEmail: WideString;
		FPassword: WideString;
		FUseTCPasswordManager: Boolean;
		FUnlimitedFileSize: Boolean;
		FSplitLargeFiles: Boolean;
		FPublicAccount: Boolean;
		FPublicUrl: WideString;
		FEncryptFilesMode: Integer;
		FEncryptFilenames: Boolean;
		FEncryptPasswordButtonEnabled: Boolean;
		FAccountsPanelVisible: Boolean;
		FSharesPanelVisible: Boolean;

		{Global settings}
		FLoadSSLFromPluginDir: Boolean;
		FPreserveFileTime: Boolean;
		FCloudMaxFileSize: Integer;
		FCloudMaxFileSizeEnabled: Boolean;
		FCloudMaxFileSizeEditEnabled: Boolean;
		FChunkOverwriteMode: Integer;
		FDeleteFailOnUploadMode: Integer;
		FOverwriteLocalMode: Integer;
		FOperationErrorMode: Integer;
		FRetryAttempts: Integer;
		FAttemptWait: Integer;
		FDisableMultiThreading: Boolean;
		FLogUserSpace: Boolean;
		FIconsMode: Integer;
		FCopyBetweenAccountsMode: Integer;
		FDownloadLinksEncode: Boolean;
		FAutoUpdateDownloadListing: Boolean;
		FShowTrashFolders: Boolean;
		FShowSharedFolders: Boolean;
		FShowInvitesFolders: Boolean;
		FPrecalculateHash: Boolean;
		FCheckCRC: Boolean;
		FHashCalculatorStrategy: Integer;

		{Network settings}
		FSocketTimeout: Integer;
		FUploadBPS: Integer;
		FDownloadBPS: Integer;
		FProxyType: Integer;
		FProxyServer: WideString;
		FProxyPort: Integer;
		FProxyUser: WideString;
		FProxyPassword: WideString;
		FProxyUseTCPasswordManager: Boolean;
		FProxyTCPasswordManagerEnabled: Boolean;
		FUserAgent: WideString;
		FChangeUserAgent: Boolean;
		FUserAgentReadOnly: Boolean;

		{Description settings}
		FDescriptionEnabled: Boolean;
		FDescriptionEditorEnabled: Boolean;
		FDescriptionCopyToCloud: Boolean;
		FDescriptionCopyFromCloud: Boolean;
		FDescriptionTrackCloudFS: Boolean;
		FDescriptionFileName: WideString;

		{Streaming extensions}
		FStreamingExtensionsList: TStringList;
		FSelectedStreamingExtensionIndex: Integer;
		FStreamingExtension: WideString;
		FStreamingCommand: WideString;
		FStreamingParameters: WideString;
		FStreamingStartPath: WideString;
		FStreamingType: Integer;

		{UI actions}
		FValidationErrorControlName: WideString;
		FValidationErrorMessage: WideString;
		FShownTabIndex: Integer;
	public
		constructor Create;
		destructor Destroy; override;

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

		{Test access properties}
		property AccountsList: TStringList read FAccountsList;
		property SelectedAccountIndex: Integer read FSelectedAccountIndex write FSelectedAccountIndex;
		property AccountsPanelVisible: Boolean read FAccountsPanelVisible;
		property SharesPanelVisible: Boolean read FSharesPanelVisible;
		property EncryptPasswordButtonEnabled: Boolean read FEncryptPasswordButtonEnabled;
		property ValidationErrorControlName: WideString read FValidationErrorControlName;
		property ValidationErrorMessage: WideString read FValidationErrorMessage;
		property ShownTabIndex: Integer read FShownTabIndex;
		property CloudMaxFileSizeEditEnabled: Boolean read FCloudMaxFileSizeEditEnabled;
		property ProxyTCPasswordManagerEnabled: Boolean read FProxyTCPasswordManagerEnabled;
		property UserAgentReadOnly: Boolean read FUserAgentReadOnly;
		property StreamingExtensionsList: TStringList read FStreamingExtensionsList;
		property SelectedStreamingExtensionIndex: Integer read FSelectedStreamingExtensionIndex write FSelectedStreamingExtensionIndex;
	end;

	{Mock password manager for testing}
	TMockPasswordManager = class(TInterfacedObject, IPasswordManager)
	private
		FPasswords: TDictionary<WideString, WideString>;
		FSetPasswordResult: Integer;
		FGetPasswordResult: Integer;
	public
		constructor Create;
		destructor Destroy; override;

		function GetPassword(AccountKey: WideString; var Password: WideString): Integer;
		function SetPassword(AccountKey: WideString; Password: WideString): Integer;

		property SetPasswordResult: Integer read FSetPasswordResult write FSetPasswordResult;
		property GetPasswordResult: Integer read FGetPasswordResult write FGetPasswordResult;
		property Passwords: TDictionary<WideString, WideString> read FPasswords;
	end;

	[TestFixture]
	TAccountsPresenterTest = class
	private
		FView: TMockAccountsView;
		FViewRef: IAccountsView;
		FPasswordManager: TMockPasswordManager;
		FPasswordManagerRef: IPasswordManager;
		FPresenter: TAccountsPresenter;
		FAccountsManager: IAccountsManager;
		FSettingsManager: IPluginSettingsManager;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Initialization tests}
		[Test]
		procedure TestInitializeLoadsGlobalSettings;
		[Test]
		procedure TestInitializeLoadsStreamingExtensions;

		{Account operations tests}
		[Test]
		procedure TestOnPublicAccountChangedShowsSharesPanel;
		[Test]
		procedure TestOnPublicAccountChangedHidesAccountsPanel;
		[Test]
		procedure TestOnEncryptModeChangedEnablesPasswordButton;
		[Test]
		procedure TestOnEncryptModeChangedDisablesPasswordButton;

		{Global settings tests}
		[Test]
		procedure TestOnCloudMaxFileSizeCheckChangedEnablesEdit;
		[Test]
		procedure TestOnCloudMaxFileSizeCheckChangedDisablesEdit;
		[Test]
		procedure TestOnProxyUserChangedEnablesPasswordManager;
		[Test]
		procedure TestOnProxyUserChangedDisablesPasswordManager;
		[Test]
		procedure TestOnChangeUserAgentChangedEnablesEdit;
		[Test]
		procedure TestOnChangeUserAgentChangedDisablesEdit;

		{Account selection and loading tests}
		[Test]
		procedure TestInitializeWithInitialAccountSelectsIt;
		[Test]
		procedure TestInitializeWithUnknownAccountSelectsFirst;
		[Test]
		procedure TestOnAccountSelectedLoadsAccountData;
		[Test]
		procedure TestOnAccountSelectedClearsFieldsWhenEmpty;

		{Account CRUD tests}
		[Test]
		procedure TestOnApplyAccountClickSavesAccount;
		[Test]
		procedure TestOnApplyAccountClickWithEmptyNameDoesNothing;
		[Test]
		procedure TestOnApplyAccountClickWithTCPasswordManager;
		[Test]
		procedure TestOnApplyAccountClickWithTCPasswordManagerFailure;
		[Test]
		procedure TestOnDeleteAccountClickRemovesAccount;
		[Test]
		procedure TestOnDeleteAccountClickWithEmptySelectionDoesNothing;

		{Encryption tests}
		[Test]
		procedure TestOnEncryptPasswordClickSetsGUID;
		[Test]
		procedure TestOnEncryptPasswordClickCancelledDoesNothing;
		[Test]
		procedure TestOnEncryptModeChangedWithAskOnce;

		{Global settings apply tests}
		[Test]
		procedure TestOnApplyGlobalSettingsClickSavesSettings;
		[Test]
		procedure TestOnApplyGlobalSettingsClickSetsAppliedFlag;
		[Test]
		procedure TestOnApplyGlobalSettingsClickWithInvalidDescriptionFileName;
		[Test]
		procedure TestOnApplyGlobalSettingsClickWithCustomMaxFileSize;
		[Test]
		procedure TestOnApplyGlobalSettingsClickWithDefaultMaxFileSize;
		[Test]
		procedure TestOnApplyGlobalSettingsClickWithProxyPasswordManager;
		[Test]
		procedure TestOnApplyGlobalSettingsClickWithCustomUserAgent;
		[Test]
		procedure TestOnApplyGlobalSettingsClickWithDefaultUserAgent;

		{Streaming extension tests}
		[Test]
		procedure TestOnStreamingExtensionSelectedLoadsSettings;
		[Test]
		procedure TestOnStreamingExtensionSelectedClearsFieldsWhenEmpty;
		[Test]
		procedure TestOnApplyStreamingExtensionClickSavesExtension;
		[Test]
		procedure TestOnApplyStreamingExtensionClickWithEmptyNameDoesNothing;
		[Test]
		procedure TestOnDeleteStreamingExtensionClickRemovesExtension;
		[Test]
		procedure TestOnDeleteStreamingExtensionClickWithEmptySelectionDoesNothing;
		[Test]
		procedure TestLoadStreamingExtensionsToViewPopulatesList;

		{LoadGlobalSettingsToView tests}
		[Test]
		procedure TestLoadGlobalSettingsLoadsAllSettings;
		[Test]
		procedure TestLoadGlobalSettingsWithCustomCloudMaxFileSize;
		[Test]
		procedure TestLoadGlobalSettingsWithCustomUserAgent;
		[Test]
		procedure TestLoadGlobalSettingsWithProxyUser;
	end;

implementation

uses
	IniConfigFile,
	CMRConstants,
	SettingsConstants,
	WSList;

{TMockAccountsView}

constructor TMockAccountsView.Create;
begin
	inherited Create;
	FAccountsList := TStringList.Create;
	FStreamingExtensionsList := TStringList.Create;
	FSelectedAccountIndex := -1;
	FSelectedStreamingExtensionIndex := -1;
	FShownTabIndex := -1;
end;

destructor TMockAccountsView.Destroy;
begin
	FAccountsList.Free;
	FStreamingExtensionsList.Free;
	inherited;
end;

procedure TMockAccountsView.SetAccountsList(Accounts: TStrings);
begin
	FAccountsList.Assign(Accounts);
end;

function TMockAccountsView.GetSelectedAccountIndex: Integer;
begin
	Result := FSelectedAccountIndex;
end;

function TMockAccountsView.GetSelectedAccountName: WideString;
begin
	if (FSelectedAccountIndex >= 0) and (FSelectedAccountIndex < FAccountsList.Count) then
		Result := FAccountsList[FSelectedAccountIndex]
	else
		Result := '';
end;

procedure TMockAccountsView.SelectAccount(Index: Integer);
begin
	FSelectedAccountIndex := Index;
end;

procedure TMockAccountsView.SetAccountName(Value: WideString);
begin
	FAccountName := Value;
end;

function TMockAccountsView.GetAccountName: WideString;
begin
	Result := FAccountName;
end;

procedure TMockAccountsView.SetEmail(Value: WideString);
begin
	FEmail := Value;
end;

function TMockAccountsView.GetEmail: WideString;
begin
	Result := FEmail;
end;

procedure TMockAccountsView.SetPassword(Value: WideString);
begin
	FPassword := Value;
end;

function TMockAccountsView.GetPassword: WideString;
begin
	Result := FPassword;
end;

procedure TMockAccountsView.SetUseTCPasswordManager(Value: Boolean);
begin
	FUseTCPasswordManager := Value;
end;

function TMockAccountsView.GetUseTCPasswordManager: Boolean;
begin
	Result := FUseTCPasswordManager;
end;

procedure TMockAccountsView.SetUnlimitedFileSize(Value: Boolean);
begin
	FUnlimitedFileSize := Value;
end;

function TMockAccountsView.GetUnlimitedFileSize: Boolean;
begin
	Result := FUnlimitedFileSize;
end;

procedure TMockAccountsView.SetSplitLargeFiles(Value: Boolean);
begin
	FSplitLargeFiles := Value;
end;

function TMockAccountsView.GetSplitLargeFiles: Boolean;
begin
	Result := FSplitLargeFiles;
end;

procedure TMockAccountsView.SetPublicAccount(Value: Boolean);
begin
	FPublicAccount := Value;
end;

function TMockAccountsView.GetPublicAccount: Boolean;
begin
	Result := FPublicAccount;
end;

procedure TMockAccountsView.SetPublicUrl(Value: WideString);
begin
	FPublicUrl := Value;
end;

function TMockAccountsView.GetPublicUrl: WideString;
begin
	Result := FPublicUrl;
end;

procedure TMockAccountsView.SetEncryptFilesMode(Value: Integer);
begin
	FEncryptFilesMode := Value;
end;

function TMockAccountsView.GetEncryptFilesMode: Integer;
begin
	Result := FEncryptFilesMode;
end;

procedure TMockAccountsView.SetEncryptFilenames(Value: Boolean);
begin
	FEncryptFilenames := Value;
end;

function TMockAccountsView.GetEncryptFilenames: Boolean;
begin
	Result := FEncryptFilenames;
end;

procedure TMockAccountsView.SetEncryptPasswordButtonEnabled(Value: Boolean);
begin
	FEncryptPasswordButtonEnabled := Value;
end;

procedure TMockAccountsView.SetAccountsPanelVisible(Value: Boolean);
begin
	FAccountsPanelVisible := Value;
end;

procedure TMockAccountsView.SetSharesPanelVisible(Value: Boolean);
begin
	FSharesPanelVisible := Value;
end;

{Global settings}

procedure TMockAccountsView.SetLoadSSLFromPluginDir(Value: Boolean);
begin
	FLoadSSLFromPluginDir := Value;
end;

function TMockAccountsView.GetLoadSSLFromPluginDir: Boolean;
begin
	Result := FLoadSSLFromPluginDir;
end;

procedure TMockAccountsView.SetPreserveFileTime(Value: Boolean);
begin
	FPreserveFileTime := Value;
end;

function TMockAccountsView.GetPreserveFileTime: Boolean;
begin
	Result := FPreserveFileTime;
end;

procedure TMockAccountsView.SetCloudMaxFileSize(Value: Integer);
begin
	FCloudMaxFileSize := Value;
end;

function TMockAccountsView.GetCloudMaxFileSize: Integer;
begin
	Result := FCloudMaxFileSize;
end;

procedure TMockAccountsView.SetCloudMaxFileSizeEnabled(Value: Boolean);
begin
	FCloudMaxFileSizeEnabled := Value;
end;

function TMockAccountsView.GetCloudMaxFileSizeEnabled: Boolean;
begin
	Result := FCloudMaxFileSizeEnabled;
end;

procedure TMockAccountsView.SetCloudMaxFileSizeEditEnabled(Value: Boolean);
begin
	FCloudMaxFileSizeEditEnabled := Value;
end;

procedure TMockAccountsView.SetChunkOverwriteMode(Value: Integer);
begin
	FChunkOverwriteMode := Value;
end;

function TMockAccountsView.GetChunkOverwriteMode: Integer;
begin
	Result := FChunkOverwriteMode;
end;

procedure TMockAccountsView.SetDeleteFailOnUploadMode(Value: Integer);
begin
	FDeleteFailOnUploadMode := Value;
end;

function TMockAccountsView.GetDeleteFailOnUploadMode: Integer;
begin
	Result := FDeleteFailOnUploadMode;
end;

procedure TMockAccountsView.SetOverwriteLocalMode(Value: Integer);
begin
	FOverwriteLocalMode := Value;
end;

function TMockAccountsView.GetOverwriteLocalMode: Integer;
begin
	Result := FOverwriteLocalMode;
end;

procedure TMockAccountsView.SetOperationErrorMode(Value: Integer);
begin
	FOperationErrorMode := Value;
end;

function TMockAccountsView.GetOperationErrorMode: Integer;
begin
	Result := FOperationErrorMode;
end;

procedure TMockAccountsView.SetRetryAttempts(Value: Integer);
begin
	FRetryAttempts := Value;
end;

function TMockAccountsView.GetRetryAttempts: Integer;
begin
	Result := FRetryAttempts;
end;

procedure TMockAccountsView.SetAttemptWait(Value: Integer);
begin
	FAttemptWait := Value;
end;

function TMockAccountsView.GetAttemptWait: Integer;
begin
	Result := FAttemptWait;
end;

procedure TMockAccountsView.SetDisableMultiThreading(Value: Boolean);
begin
	FDisableMultiThreading := Value;
end;

function TMockAccountsView.GetDisableMultiThreading: Boolean;
begin
	Result := FDisableMultiThreading;
end;

procedure TMockAccountsView.SetLogUserSpace(Value: Boolean);
begin
	FLogUserSpace := Value;
end;

function TMockAccountsView.GetLogUserSpace: Boolean;
begin
	Result := FLogUserSpace;
end;

procedure TMockAccountsView.SetIconsMode(Value: Integer);
begin
	FIconsMode := Value;
end;

function TMockAccountsView.GetIconsMode: Integer;
begin
	Result := FIconsMode;
end;

procedure TMockAccountsView.SetCopyBetweenAccountsMode(Value: Integer);
begin
	FCopyBetweenAccountsMode := Value;
end;

function TMockAccountsView.GetCopyBetweenAccountsMode: Integer;
begin
	Result := FCopyBetweenAccountsMode;
end;

procedure TMockAccountsView.SetDownloadLinksEncode(Value: Boolean);
begin
	FDownloadLinksEncode := Value;
end;

function TMockAccountsView.GetDownloadLinksEncode: Boolean;
begin
	Result := FDownloadLinksEncode;
end;

procedure TMockAccountsView.SetAutoUpdateDownloadListing(Value: Boolean);
begin
	FAutoUpdateDownloadListing := Value;
end;

function TMockAccountsView.GetAutoUpdateDownloadListing: Boolean;
begin
	Result := FAutoUpdateDownloadListing;
end;

procedure TMockAccountsView.SetShowTrashFolders(Value: Boolean);
begin
	FShowTrashFolders := Value;
end;

function TMockAccountsView.GetShowTrashFolders: Boolean;
begin
	Result := FShowTrashFolders;
end;

procedure TMockAccountsView.SetShowSharedFolders(Value: Boolean);
begin
	FShowSharedFolders := Value;
end;

function TMockAccountsView.GetShowSharedFolders: Boolean;
begin
	Result := FShowSharedFolders;
end;

procedure TMockAccountsView.SetShowInvitesFolders(Value: Boolean);
begin
	FShowInvitesFolders := Value;
end;

function TMockAccountsView.GetShowInvitesFolders: Boolean;
begin
	Result := FShowInvitesFolders;
end;

procedure TMockAccountsView.SetPrecalculateHash(Value: Boolean);
begin
	FPrecalculateHash := Value;
end;

function TMockAccountsView.GetPrecalculateHash: Boolean;
begin
	Result := FPrecalculateHash;
end;

procedure TMockAccountsView.SetCheckCRC(Value: Boolean);
begin
	FCheckCRC := Value;
end;

function TMockAccountsView.GetCheckCRC: Boolean;
begin
	Result := FCheckCRC;
end;

procedure TMockAccountsView.SetHashCalculatorStrategy(Value: Integer);
begin
	FHashCalculatorStrategy := Value;
end;

function TMockAccountsView.GetHashCalculatorStrategy: Integer;
begin
	Result := FHashCalculatorStrategy;
end;

{Network settings}

procedure TMockAccountsView.SetSocketTimeout(Value: Integer);
begin
	FSocketTimeout := Value;
end;

function TMockAccountsView.GetSocketTimeout: Integer;
begin
	Result := FSocketTimeout;
end;

procedure TMockAccountsView.SetUploadBPS(Value: Integer);
begin
	FUploadBPS := Value;
end;

function TMockAccountsView.GetUploadBPS: Integer;
begin
	Result := FUploadBPS;
end;

procedure TMockAccountsView.SetDownloadBPS(Value: Integer);
begin
	FDownloadBPS := Value;
end;

function TMockAccountsView.GetDownloadBPS: Integer;
begin
	Result := FDownloadBPS;
end;

procedure TMockAccountsView.SetProxyType(Value: Integer);
begin
	FProxyType := Value;
end;

function TMockAccountsView.GetProxyType: Integer;
begin
	Result := FProxyType;
end;

procedure TMockAccountsView.SetProxyServer(Value: WideString);
begin
	FProxyServer := Value;
end;

function TMockAccountsView.GetProxyServer: WideString;
begin
	Result := FProxyServer;
end;

procedure TMockAccountsView.SetProxyPort(Value: Integer);
begin
	FProxyPort := Value;
end;

function TMockAccountsView.GetProxyPort: Integer;
begin
	Result := FProxyPort;
end;

procedure TMockAccountsView.SetProxyUser(Value: WideString);
begin
	FProxyUser := Value;
end;

function TMockAccountsView.GetProxyUser: WideString;
begin
	Result := FProxyUser;
end;

procedure TMockAccountsView.SetProxyPassword(Value: WideString);
begin
	FProxyPassword := Value;
end;

function TMockAccountsView.GetProxyPassword: WideString;
begin
	Result := FProxyPassword;
end;

procedure TMockAccountsView.SetProxyUseTCPasswordManager(Value: Boolean);
begin
	FProxyUseTCPasswordManager := Value;
end;

function TMockAccountsView.GetProxyUseTCPasswordManager: Boolean;
begin
	Result := FProxyUseTCPasswordManager;
end;

procedure TMockAccountsView.SetProxyTCPasswordManagerEnabled(Value: Boolean);
begin
	FProxyTCPasswordManagerEnabled := Value;
end;

procedure TMockAccountsView.SetUserAgent(Value: WideString);
begin
	FUserAgent := Value;
end;

function TMockAccountsView.GetUserAgent: WideString;
begin
	Result := FUserAgent;
end;

procedure TMockAccountsView.SetChangeUserAgent(Value: Boolean);
begin
	FChangeUserAgent := Value;
end;

function TMockAccountsView.GetChangeUserAgent: Boolean;
begin
	Result := FChangeUserAgent;
end;

procedure TMockAccountsView.SetUserAgentReadOnly(Value: Boolean);
begin
	FUserAgentReadOnly := Value;
end;

{Description settings}

procedure TMockAccountsView.SetDescriptionEnabled(Value: Boolean);
begin
	FDescriptionEnabled := Value;
end;

function TMockAccountsView.GetDescriptionEnabled: Boolean;
begin
	Result := FDescriptionEnabled;
end;

procedure TMockAccountsView.SetDescriptionEditorEnabled(Value: Boolean);
begin
	FDescriptionEditorEnabled := Value;
end;

function TMockAccountsView.GetDescriptionEditorEnabled: Boolean;
begin
	Result := FDescriptionEditorEnabled;
end;

procedure TMockAccountsView.SetDescriptionCopyToCloud(Value: Boolean);
begin
	FDescriptionCopyToCloud := Value;
end;

function TMockAccountsView.GetDescriptionCopyToCloud: Boolean;
begin
	Result := FDescriptionCopyToCloud;
end;

procedure TMockAccountsView.SetDescriptionCopyFromCloud(Value: Boolean);
begin
	FDescriptionCopyFromCloud := Value;
end;

function TMockAccountsView.GetDescriptionCopyFromCloud: Boolean;
begin
	Result := FDescriptionCopyFromCloud;
end;

procedure TMockAccountsView.SetDescriptionTrackCloudFS(Value: Boolean);
begin
	FDescriptionTrackCloudFS := Value;
end;

function TMockAccountsView.GetDescriptionTrackCloudFS: Boolean;
begin
	Result := FDescriptionTrackCloudFS;
end;

procedure TMockAccountsView.SetDescriptionFileName(Value: WideString);
begin
	FDescriptionFileName := Value;
end;

function TMockAccountsView.GetDescriptionFileName: WideString;
begin
	Result := FDescriptionFileName;
end;

{Streaming extensions}

procedure TMockAccountsView.SetStreamingExtensionsList(Extensions: TStrings);
begin
	FStreamingExtensionsList.Assign(Extensions);
end;

function TMockAccountsView.GetSelectedStreamingExtensionIndex: Integer;
begin
	Result := FSelectedStreamingExtensionIndex;
end;

function TMockAccountsView.GetSelectedStreamingExtension: WideString;
begin
	if (FSelectedStreamingExtensionIndex >= 0) and (FSelectedStreamingExtensionIndex < FStreamingExtensionsList.Count) then
		Result := FStreamingExtensionsList[FSelectedStreamingExtensionIndex]
	else
		Result := '';
end;

procedure TMockAccountsView.SetStreamingExtension(Value: WideString);
begin
	FStreamingExtension := Value;
end;

function TMockAccountsView.GetStreamingExtension: WideString;
begin
	Result := FStreamingExtension;
end;

procedure TMockAccountsView.SetStreamingCommand(Value: WideString);
begin
	FStreamingCommand := Value;
end;

function TMockAccountsView.GetStreamingCommand: WideString;
begin
	Result := FStreamingCommand;
end;

procedure TMockAccountsView.SetStreamingParameters(Value: WideString);
begin
	FStreamingParameters := Value;
end;

function TMockAccountsView.GetStreamingParameters: WideString;
begin
	Result := FStreamingParameters;
end;

procedure TMockAccountsView.SetStreamingStartPath(Value: WideString);
begin
	FStreamingStartPath := Value;
end;

function TMockAccountsView.GetStreamingStartPath: WideString;
begin
	Result := FStreamingStartPath;
end;

procedure TMockAccountsView.SetStreamingType(Value: Integer);
begin
	FStreamingType := Value;
end;

function TMockAccountsView.GetStreamingType: Integer;
begin
	Result := FStreamingType;
end;

procedure TMockAccountsView.ClearStreamingFields;
begin
	FStreamingExtension := '';
	FStreamingCommand := '';
	FStreamingParameters := '';
	FStreamingStartPath := '';
	FStreamingType := 0;
end;

{UI actions}

procedure TMockAccountsView.ShowValidationError(ControlName: WideString; Message: WideString);
begin
	FValidationErrorControlName := ControlName;
	FValidationErrorMessage := Message;
end;

procedure TMockAccountsView.ShowTab(TabIndex: Integer);
begin
	FShownTabIndex := TabIndex;
end;

function TMockAccountsView.GetFormHandle: THandle;
begin
	Result := 0;
end;

function TMockAccountsView.ShowEncryptionPasswordDialog(const AccountName: WideString; var CryptedGUID: WideString): Boolean;
begin
	{Mock: always return False (dialog cancelled)}
	CryptedGUID := '';
	Result := False;
end;

{TMockPasswordManager}

constructor TMockPasswordManager.Create;
begin
	inherited Create;
	FPasswords := TDictionary<WideString, WideString>.Create;
	FSetPasswordResult := FS_FILE_OK;
	FGetPasswordResult := FS_FILE_READERROR;
end;

destructor TMockPasswordManager.Destroy;
begin
	FPasswords.Free;
	inherited;
end;

function TMockPasswordManager.GetPassword(AccountKey: WideString; var Password: WideString): Integer;
begin
	if FPasswords.TryGetValue(AccountKey, Password) then
		Result := FS_FILE_OK
	else
		Result := FGetPasswordResult;
end;

function TMockPasswordManager.SetPassword(AccountKey: WideString; Password: WideString): Integer;
begin
	if FSetPasswordResult = FS_FILE_OK then
		FPasswords.AddOrSetValue(AccountKey, Password);
	Result := FSetPasswordResult;
end;

{TAccountsPresenterTest}

procedure TAccountsPresenterTest.Setup;
var
	Config: TAccountsPresenterConfig;
begin
	FView := TMockAccountsView.Create;
	FViewRef := FView;
	FPasswordManager := TMockPasswordManager.Create;
	FPasswordManagerRef := FPasswordManager;

	{Create settings manager with memory config to avoid file access}
	FSettingsManager := TPluginSettingsManager.Create(TMemoryConfigFile.Create);
	FAccountsManager := TAccountsManager.Create(TMemoryConfigFile.Create);

	Config.PasswordManager := FPasswordManagerRef;
	Config.ParentWindow := 0;

	FPresenter := TAccountsPresenter.Create(
		FViewRef,
		FAccountsManager,
		FSettingsManager,
		Config
	);
end;

procedure TAccountsPresenterTest.TearDown;
begin
	FPresenter.Free;
	{Interface references - let reference counting handle cleanup}
	FAccountsManager := nil;
	FSettingsManager := nil;
	FViewRef := nil;
	FPasswordManagerRef := nil;
end;

procedure TAccountsPresenterTest.TestInitializeLoadsGlobalSettings;
begin
	{The presenter should load global settings from settings manager during initialization}
	{This test verifies that Initialize() completes without errors}
	FPresenter.Initialize('');

	{Verify the presenter initialized - settings were applied flag should start as False}
	Assert.IsFalse(FPresenter.SettingsApplied, 'Settings should not be applied yet');
end;

procedure TAccountsPresenterTest.TestInitializeLoadsStreamingExtensions;
begin
	{Initialize presenter}
	FPresenter.Initialize('');

	{Streaming extensions list should be populated (even if empty)}
	Assert.IsNotNull(FView.StreamingExtensionsList, 'Streaming extensions list should exist');
end;

procedure TAccountsPresenterTest.TestOnPublicAccountChangedShowsSharesPanel;
begin
	FPresenter.Initialize('');
	FView.SetPublicAccount(True);

	FPresenter.OnPublicAccountChanged;

	Assert.IsTrue(FView.SharesPanelVisible, 'Shares panel should be visible for public account');
end;

procedure TAccountsPresenterTest.TestOnPublicAccountChangedHidesAccountsPanel;
begin
	FPresenter.Initialize('');
	FView.SetPublicAccount(True);

	FPresenter.OnPublicAccountChanged;

	Assert.IsFalse(FView.AccountsPanelVisible, 'Accounts panel should be hidden for public account');
end;

procedure TAccountsPresenterTest.TestOnEncryptModeChangedEnablesPasswordButton;
begin
	FPresenter.Initialize('');
	FView.SetEncryptFilesMode(EncryptModeAlways);

	FPresenter.OnEncryptModeChanged;

	Assert.IsTrue(FView.EncryptPasswordButtonEnabled, 'Password button should be enabled for EncryptModeAlways');
end;

procedure TAccountsPresenterTest.TestOnEncryptModeChangedDisablesPasswordButton;
begin
	FPresenter.Initialize('');
	FView.SetEncryptFilesMode(EncryptModeNone);

	FPresenter.OnEncryptModeChanged;

	Assert.IsFalse(FView.EncryptPasswordButtonEnabled, 'Password button should be disabled for EncryptModeNone');
end;

procedure TAccountsPresenterTest.TestOnCloudMaxFileSizeCheckChangedEnablesEdit;
begin
	FPresenter.Initialize('');
	FView.SetCloudMaxFileSizeEnabled(True);

	FPresenter.OnCloudMaxFileSizeCheckChanged;

	Assert.IsTrue(FView.CloudMaxFileSizeEditEnabled, 'Edit should be enabled when checkbox is checked');
end;

procedure TAccountsPresenterTest.TestOnCloudMaxFileSizeCheckChangedDisablesEdit;
begin
	FPresenter.Initialize('');
	FView.SetCloudMaxFileSizeEnabled(False);

	FPresenter.OnCloudMaxFileSizeCheckChanged;

	Assert.IsFalse(FView.CloudMaxFileSizeEditEnabled, 'Edit should be disabled when checkbox is unchecked');
end;

procedure TAccountsPresenterTest.TestOnProxyUserChangedEnablesPasswordManager;
begin
	FPresenter.Initialize('');
	FView.SetProxyUser('testuser');

	FPresenter.OnProxyUserChanged;

	Assert.IsTrue(FView.ProxyTCPasswordManagerEnabled, 'Password manager checkbox should be enabled when user is set');
end;

procedure TAccountsPresenterTest.TestOnProxyUserChangedDisablesPasswordManager;
begin
	FPresenter.Initialize('');
	FView.SetProxyUser('');

	FPresenter.OnProxyUserChanged;

	Assert.IsFalse(FView.ProxyTCPasswordManagerEnabled, 'Password manager checkbox should be disabled when user is empty');
end;

procedure TAccountsPresenterTest.TestOnChangeUserAgentChangedEnablesEdit;
begin
	FPresenter.Initialize('');
	FView.SetChangeUserAgent(True);

	FPresenter.OnChangeUserAgentChanged;

	Assert.IsFalse(FView.UserAgentReadOnly, 'User agent edit should be writable when checkbox is checked');
end;

procedure TAccountsPresenterTest.TestOnChangeUserAgentChangedDisablesEdit;
begin
	FPresenter.Initialize('');
	FView.SetChangeUserAgent(False);

	FPresenter.OnChangeUserAgentChanged;

	Assert.IsTrue(FView.UserAgentReadOnly, 'User agent edit should be read-only when checkbox is unchecked');
end;

{Account selection and loading tests}

procedure TAccountsPresenterTest.TestInitializeWithInitialAccountSelectsIt;
var
	AccSettings: TAccountSettings;
begin
	{Setup: Create two accounts}
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'Account1';
	AccSettings.Email := 'user1@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings.Account := 'Account2';
	AccSettings.Email := 'user2@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	{Initialize with second account as initial}
	FPresenter.Initialize('Account2');

	{Verify second account is selected}
	Assert.AreEqual(1, FView.SelectedAccountIndex, 'Second account should be selected');
	Assert.AreEqual('user2@mail.ru', FView.GetEmail, 'Email should be from Account2');
end;

procedure TAccountsPresenterTest.TestInitializeWithUnknownAccountSelectsFirst;
var
	AccSettings: TAccountSettings;
begin
	{Setup: Create an account}
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'Account1';
	AccSettings.Email := 'user1@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	{Initialize with unknown account}
	FPresenter.Initialize('UnknownAccount');

	{Verify first account is selected as fallback}
	Assert.AreEqual(0, FView.SelectedAccountIndex, 'First account should be selected as fallback');
end;

procedure TAccountsPresenterTest.TestOnAccountSelectedLoadsAccountData;
var
	AccSettings: TAccountSettings;
begin
	{Setup: Create account with specific settings}
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'TestAccount';
	AccSettings.Email := 'test@mail.ru';
	AccSettings.Password := 'secret';
	AccSettings.UseTCPasswordManager := True;
	AccSettings.UnlimitedFilesize := True;
	AccSettings.SplitLargeFiles := True;
	AccSettings.PublicAccount := False;
	AccSettings.EncryptFilesMode := EncryptModeAlways;
	AccSettings.EncryptFilenames := True;
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('TestAccount');

	{Verify all fields are loaded}
	Assert.AreEqual('TestAccount', FView.GetAccountName, 'Account name should match');
	Assert.AreEqual('test@mail.ru', FView.GetEmail, 'Email should match');
	Assert.AreEqual('secret', FView.GetPassword, 'Password should match');
	Assert.IsTrue(FView.GetUseTCPasswordManager, 'TC password manager should be enabled');
	Assert.IsTrue(FView.GetUnlimitedFileSize, 'Unlimited file size should be enabled');
	Assert.IsTrue(FView.GetSplitLargeFiles, 'Split large files should be enabled');
	Assert.IsFalse(FView.GetPublicAccount, 'Public account should be disabled');
	Assert.AreEqual(EncryptModeAlways, FView.GetEncryptFilesMode, 'Encrypt mode should match');
	Assert.IsTrue(FView.GetEncryptFilenames, 'Encrypt filenames should be enabled');
end;

procedure TAccountsPresenterTest.TestOnAccountSelectedClearsFieldsWhenEmpty;
begin
	{Initialize with no accounts}
	FPresenter.Initialize('');

	{Verify fields are cleared}
	Assert.AreEqual('', FView.GetAccountName, 'Account name should be empty');
	Assert.AreEqual('', FView.GetEmail, 'Email should be empty');
	Assert.AreEqual('', FView.GetPassword, 'Password should be empty');
	Assert.IsFalse(FView.GetUseTCPasswordManager, 'TC password manager should be disabled');
	Assert.IsFalse(FView.EncryptPasswordButtonEnabled, 'Encrypt button should be disabled');
end;

{Account CRUD tests}

procedure TAccountsPresenterTest.TestOnApplyAccountClickSavesAccount;
var
	SavedSettings: TAccountSettings;
begin
	FPresenter.Initialize('');

	{Fill in account data via view}
	FView.SetAccountName('NewAccount');
	FView.SetEmail('new@mail.ru');
	FView.SetPassword('newpassword');
	FView.SetUseTCPasswordManager(False);
	FView.SetUnlimitedFileSize(True);
	FView.SetSplitLargeFiles(True);
	FView.SetPublicAccount(False);
	FView.SetEncryptFilesMode(EncryptModeNone);

	FPresenter.OnApplyAccountClick;

	{Verify account was saved}
	SavedSettings := FAccountsManager.GetAccountSettings('NewAccount');
	Assert.AreEqual('NewAccount', SavedSettings.Account, 'Account name should be saved');
	Assert.AreEqual('new@mail.ru', SavedSettings.Email, 'Email should be saved');
	Assert.AreEqual('newpassword', SavedSettings.Password, 'Password should be saved');
	Assert.IsTrue(SavedSettings.UnlimitedFilesize, 'Unlimited filesize should be saved');
	Assert.IsTrue(SavedSettings.SplitLargeFiles, 'Split large files should be saved');
end;

procedure TAccountsPresenterTest.TestOnApplyAccountClickWithEmptyNameDoesNothing;
var
	AccountsList: TWSList;
begin
	FPresenter.Initialize('');

	{Try to apply with empty name}
	FView.SetAccountName('');
	FView.SetEmail('test@mail.ru');

	FPresenter.OnApplyAccountClick;

	{Verify no account was created}
	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(0, AccountsList.Count, 'No account should be created with empty name');
end;

procedure TAccountsPresenterTest.TestOnApplyAccountClickWithTCPasswordManager;
var
	SavedSettings: TAccountSettings;
begin
	FPresenter.Initialize('');

	{Fill in account data with TC password manager enabled}
	FView.SetAccountName('TCAccount');
	FView.SetEmail('tc@mail.ru');
	FView.SetPassword('tcpassword');
	FView.SetUseTCPasswordManager(True);

	FPresenter.OnApplyAccountClick;

	{Verify password was stored in password manager}
	Assert.IsTrue(FPasswordManager.Passwords.ContainsKey('TCAccount'), 'Password should be in manager');
	Assert.AreEqual('tcpassword', FPasswordManager.Passwords['TCAccount'], 'Password value should match');

	{Verify password is cleared in saved settings}
	SavedSettings := FAccountsManager.GetAccountSettings('TCAccount');
	Assert.AreEqual('', SavedSettings.Password, 'Password in settings should be empty');
	Assert.IsTrue(SavedSettings.UseTCPasswordManager, 'TC password manager flag should be set');
end;

procedure TAccountsPresenterTest.TestOnApplyAccountClickWithTCPasswordManagerFailure;
var
	AccountsList: TWSList;
begin
	{Configure password manager to fail}
	FPasswordManager.SetPasswordResult := FS_FILE_WRITEERROR;

	FPresenter.Initialize('');

	{Try to save account with TC password manager}
	FView.SetAccountName('FailAccount');
	FView.SetEmail('fail@mail.ru');
	FView.SetPassword('failpassword');
	FView.SetUseTCPasswordManager(True);

	FPresenter.OnApplyAccountClick;

	{Verify account was NOT saved due to password manager failure}
	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(0, AccountsList.Count, 'Account should not be saved when password manager fails');
end;

procedure TAccountsPresenterTest.TestOnDeleteAccountClickRemovesAccount;
var
	AccSettings: TAccountSettings;
	AccountsList: TWSList;
begin
	{Setup: Create account}
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'ToDelete';
	AccSettings.Email := 'delete@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('ToDelete');

	{Delete the account}
	FPresenter.OnDeleteAccountClick;

	{Verify account was removed}
	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(0, AccountsList.Count, 'Account should be deleted');
end;

procedure TAccountsPresenterTest.TestOnDeleteAccountClickWithEmptySelectionDoesNothing;
var
	AccSettings: TAccountSettings;
	AccountsList: TWSList;
begin
	{Setup: Create account}
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'KeepMe';
	AccSettings.Email := 'keep@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('');
	{Clear selection}
	FView.SelectedAccountIndex := -1;

	FPresenter.OnDeleteAccountClick;

	{Verify account still exists}
	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(1, AccountsList.Count, 'Account should not be deleted with empty selection');
end;

{Encryption tests}

procedure TAccountsPresenterTest.TestOnEncryptPasswordClickSetsGUID;
begin
	{Note: The mock dialog always returns False, so we can only test
	 that the method doesn't crash. A more sophisticated mock would
	 simulate setting the GUID.}
	FPresenter.Initialize('');

	{Should not crash}
	FPresenter.OnEncryptPasswordClick;
	Assert.Pass('OnEncryptPasswordClick should complete without error');
end;

procedure TAccountsPresenterTest.TestOnEncryptPasswordClickCancelledDoesNothing;
var
	AccSettings: TAccountSettings;
begin
	{Setup: Create account with GUID (GUID is saved separately via SetCryptedGUID)}
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'EncryptTest';
	AccSettings.Email := 'encrypt@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);
	FAccountsManager.SetCryptedGUID('EncryptTest', 'original-guid');

	FPresenter.Initialize('EncryptTest');

	{Dialog returns False, GUID should remain unchanged}
	FPresenter.OnEncryptPasswordClick;

	{Verify GUID is unchanged}
	AccSettings := FAccountsManager.GetAccountSettings('EncryptTest');
	Assert.AreEqual('original-guid', AccSettings.CryptedGUIDFiles, 'GUID should be unchanged when dialog cancelled');
end;

procedure TAccountsPresenterTest.TestOnEncryptModeChangedWithAskOnce;
begin
	FPresenter.Initialize('');
	FView.SetEncryptFilesMode(EncryptModeAskOnce);

	FPresenter.OnEncryptModeChanged;

	{AskOnce mode should NOT enable the password button (only Always does)}
	Assert.IsFalse(FView.EncryptPasswordButtonEnabled, 'Password button should be disabled for EncryptModeAskOnce');
end;

{Global settings apply tests}

procedure TAccountsPresenterTest.TestOnApplyGlobalSettingsClickSavesSettings;
var
	Settings: TPluginSettings;
begin
	FPresenter.Initialize('');

	{Set various settings via view}
	FView.SetLoadSSLFromPluginDir(True);
	FView.SetPreserveFileTime(True);
	FView.SetRetryAttempts(5);
	FView.SetAttemptWait(2000);
	FView.SetIconsMode(2);
	FView.SetDescriptionEnabled(True);
	FView.SetDescriptionFileName('descript.ion');

	FPresenter.OnApplyGlobalSettingsClick;

	{Verify settings were saved}
	Settings := FSettingsManager.GetSettings;
	Assert.IsTrue(Settings.LoadSSLDLLOnlyFromPluginDir, 'LoadSSLDLLOnlyFromPluginDir should be saved');
	Assert.IsTrue(Settings.PreserveFileTime, 'PreserveFileTime should be saved');
	Assert.AreEqual(5, Settings.RetryAttempts, 'RetryAttempts should be saved');
	Assert.AreEqual(2000, Settings.AttemptWait, 'AttemptWait should be saved');
	Assert.AreEqual(2, Settings.IconsMode, 'IconsMode should be saved');
	Assert.IsTrue(Settings.DescriptionEnabled, 'DescriptionEnabled should be saved');
	Assert.AreEqual('descript.ion', Settings.DescriptionFileName, 'DescriptionFileName should be saved');
end;

procedure TAccountsPresenterTest.TestOnApplyGlobalSettingsClickSetsAppliedFlag;
begin
	FPresenter.Initialize('');
	Assert.IsFalse(FPresenter.SettingsApplied, 'SettingsApplied should be False initially');

	FView.SetDescriptionFileName('valid.txt');
	FPresenter.OnApplyGlobalSettingsClick;

	Assert.IsTrue(FPresenter.SettingsApplied, 'SettingsApplied should be True after apply');
end;

procedure TAccountsPresenterTest.TestOnApplyGlobalSettingsClickWithInvalidDescriptionFileName;
begin
	FPresenter.Initialize('');

	{Set invalid filename with forbidden characters}
	FView.SetDescriptionFileName('invalid<>:file.txt');

	FPresenter.OnApplyGlobalSettingsClick;

	{Verify validation error was shown}
	Assert.AreEqual('DescriptionFileName', FView.ValidationErrorControlName, 'Validation should target DescriptionFileName');
	Assert.AreEqual(3, FView.ShownTabIndex, 'Should switch to Comments tab (index 3)');
	Assert.IsFalse(FPresenter.SettingsApplied, 'SettingsApplied should remain False on validation error');
end;

procedure TAccountsPresenterTest.TestOnApplyGlobalSettingsClickWithCustomMaxFileSize;
var
	Settings: TPluginSettings;
begin
	FPresenter.Initialize('');

	{Enable custom max file size}
	FView.SetCloudMaxFileSizeEnabled(True);
	FView.SetCloudMaxFileSize(1073741824); {1 GB}
	FView.SetDescriptionFileName('valid.txt');

	FPresenter.OnApplyGlobalSettingsClick;

	Settings := FSettingsManager.GetSettings;
	Assert.AreEqual(Int64(1073741824), Settings.CloudMaxFileSize, 'Custom CloudMaxFileSize should be saved');
end;

procedure TAccountsPresenterTest.TestOnApplyGlobalSettingsClickWithDefaultMaxFileSize;
var
	Settings: TPluginSettings;
begin
	FPresenter.Initialize('');

	{Disable custom max file size (use default)}
	FView.SetCloudMaxFileSizeEnabled(False);
	FView.SetCloudMaxFileSize(1073741824); {This should be ignored}
	FView.SetDescriptionFileName('valid.txt');

	FPresenter.OnApplyGlobalSettingsClick;

	Settings := FSettingsManager.GetSettings;
	Assert.AreEqual(Int64(CLOUD_MAX_FILESIZE_DEFAULT), Settings.CloudMaxFileSize, 'Default CloudMaxFileSize should be used');
end;

procedure TAccountsPresenterTest.TestOnApplyGlobalSettingsClickWithProxyPasswordManager;
var
	Settings: TPluginSettings;
begin
	FPresenter.Initialize('');

	{Enable proxy with TC password manager}
	FView.SetProxyUser('proxyuser');
	FView.SetProxyPassword('proxypass');
	FView.SetProxyUseTCPasswordManager(True);
	FView.SetDescriptionFileName('valid.txt');

	FPresenter.OnApplyGlobalSettingsClick;

	{Verify password was stored in manager}
	Assert.IsTrue(FPasswordManager.Passwords.ContainsKey(PASSWORD_KEY_PROXY + 'proxyuser'),
		'Proxy password should be in manager');
	Assert.AreEqual('proxypass', FPasswordManager.Passwords[PASSWORD_KEY_PROXY + 'proxyuser'],
		'Proxy password value should match');

	{Verify password is cleared in settings}
	Settings := FSettingsManager.GetSettings;
	Assert.AreEqual('', Settings.ConnectionSettings.ProxySettings.Password, 'Proxy password in settings should be empty');
end;

procedure TAccountsPresenterTest.TestOnApplyGlobalSettingsClickWithCustomUserAgent;
var
	Settings: TPluginSettings;
begin
	FPresenter.Initialize('');

	FView.SetChangeUserAgent(True);
	FView.SetUserAgent('CustomAgent/1.0');
	FView.SetDescriptionFileName('valid.txt');

	FPresenter.OnApplyGlobalSettingsClick;

	Settings := FSettingsManager.GetSettings;
	Assert.AreEqual('CustomAgent/1.0', Settings.ConnectionSettings.UserAgent, 'Custom user agent should be saved');
end;

procedure TAccountsPresenterTest.TestOnApplyGlobalSettingsClickWithDefaultUserAgent;
var
	Settings: TPluginSettings;
	OriginalSettings: TPluginSettings;
begin
	{Setup: Set a custom user agent first}
	OriginalSettings := FSettingsManager.GetSettings;
	OriginalSettings.ConnectionSettings.UserAgent := 'OriginalAgent/1.0';
	FSettingsManager.SetSettings(OriginalSettings);

	FPresenter.Initialize('');

	{Uncheck change user agent - should preserve existing}
	FView.SetChangeUserAgent(False);
	FView.SetUserAgent('Ignored/1.0');
	FView.SetDescriptionFileName('valid.txt');

	FPresenter.OnApplyGlobalSettingsClick;

	Settings := FSettingsManager.GetSettings;
	{When ChangeUserAgent is False, the presenter doesn't update UserAgent}
	Assert.AreEqual('OriginalAgent/1.0', Settings.ConnectionSettings.UserAgent,
		'User agent should not be changed when ChangeUserAgent is unchecked');
end;

{Streaming extension tests}

procedure TAccountsPresenterTest.TestOnStreamingExtensionSelectedLoadsSettings;
var
	ExtSettings: TStreamingSettings;
begin
	{Setup: Create streaming extension}
	ExtSettings.Command := 'vlc.exe';
	ExtSettings.Parameters := '%url%';
	ExtSettings.StartPath := 'C:\VLC';
	ExtSettings.Format := 3;
	FSettingsManager.SetStreamingSettings('.mp4', ExtSettings);

	FPresenter.Initialize('');
	{Simulate selecting mp4 extension}
	FView.StreamingExtensionsList.Add('mp4');
	FView.SelectedStreamingExtensionIndex := 0;

	FPresenter.OnStreamingExtensionSelected;

	Assert.AreEqual('mp4', FView.GetStreamingExtension, 'Extension name should be loaded');
	Assert.AreEqual('vlc.exe', FView.GetStreamingCommand, 'Command should be loaded');
	Assert.AreEqual('%url%', FView.GetStreamingParameters, 'Parameters should be loaded');
	Assert.AreEqual('C:\VLC', FView.GetStreamingStartPath, 'Start path should be loaded');
	Assert.AreEqual(3, FView.GetStreamingType, 'Streaming type should be loaded');
end;

procedure TAccountsPresenterTest.TestOnStreamingExtensionSelectedClearsFieldsWhenEmpty;
begin
	FPresenter.Initialize('');

	{Set some values first}
	FView.SetStreamingCommand('old_command');
	FView.SetStreamingParameters('old_params');

	{Clear selection}
	FView.SelectedStreamingExtensionIndex := -1;

	FPresenter.OnStreamingExtensionSelected;

	Assert.AreEqual('', FView.GetStreamingExtension, 'Extension should be cleared');
	Assert.AreEqual('', FView.GetStreamingCommand, 'Command should be cleared');
	Assert.AreEqual('', FView.GetStreamingParameters, 'Parameters should be cleared');
end;

procedure TAccountsPresenterTest.TestOnApplyStreamingExtensionClickSavesExtension;
var
	ExtSettings: TStreamingSettings;
begin
	FPresenter.Initialize('');

	{Fill in extension data}
	FView.SetStreamingExtension('avi');
	FView.SetStreamingCommand('mpc-hc.exe');
	FView.SetStreamingParameters('--fullscreen %url%');
	FView.SetStreamingStartPath('C:\MPC');
	FView.SetStreamingType(2);

	FPresenter.OnApplyStreamingExtensionClick;

	{Verify extension was saved}
	ExtSettings := FSettingsManager.GetStreamingSettings('.avi');
	Assert.AreEqual('mpc-hc.exe', ExtSettings.Command, 'Command should be saved');
	Assert.AreEqual('--fullscreen %url%', ExtSettings.Parameters, 'Parameters should be saved');
	Assert.AreEqual('C:\MPC', ExtSettings.StartPath, 'Start path should be saved');
	Assert.AreEqual(2, ExtSettings.Format, 'Format should be saved');
end;

procedure TAccountsPresenterTest.TestOnApplyStreamingExtensionClickWithEmptyNameDoesNothing;
var
	ExtList: TStringList;
begin
	FPresenter.Initialize('');

	{Try to apply with empty extension name}
	FView.SetStreamingExtension('');
	FView.SetStreamingCommand('test.exe');

	FPresenter.OnApplyStreamingExtensionClick;

	{Verify no extension was created}
	ExtList := TStringList.Create;
	try
		FSettingsManager.GetStreamingExtensionsList(ExtList);
		Assert.AreEqual(0, ExtList.Count, 'No extension should be created with empty name');
	finally
		ExtList.Free;
	end;
end;

procedure TAccountsPresenterTest.TestOnDeleteStreamingExtensionClickRemovesExtension;
var
	ExtSettings: TStreamingSettings;
	ExtList: TStringList;
begin
	{Setup: Create extension}
	ExtSettings.Command := 'vlc.exe';
	ExtSettings.Parameters := '%url%';
	ExtSettings.StartPath := '';
	ExtSettings.Format := 1;
	FSettingsManager.SetStreamingSettings('.mkv', ExtSettings);

	FPresenter.Initialize('');
	{Select the extension}
	FView.StreamingExtensionsList.Add('mkv');
	FView.SelectedStreamingExtensionIndex := 0;

	FPresenter.OnDeleteStreamingExtensionClick;

	{Verify extension was removed}
	ExtList := TStringList.Create;
	try
		FSettingsManager.GetStreamingExtensionsList(ExtList);
		Assert.AreEqual(0, ExtList.Count, 'Extension should be deleted');
	finally
		ExtList.Free;
	end;
end;

procedure TAccountsPresenterTest.TestOnDeleteStreamingExtensionClickWithEmptySelectionDoesNothing;
var
	ExtSettings: TStreamingSettings;
	ExtList: TStringList;
begin
	{Setup: Create extension}
	ExtSettings.Command := 'vlc.exe';
	ExtSettings.Parameters := '%url%';
	ExtSettings.StartPath := '';
	ExtSettings.Format := 1;
	FSettingsManager.SetStreamingSettings('.webm', ExtSettings);

	FPresenter.Initialize('');
	{Clear selection}
	FView.SelectedStreamingExtensionIndex := -1;

	FPresenter.OnDeleteStreamingExtensionClick;

	{Verify extension still exists}
	ExtList := TStringList.Create;
	try
		FSettingsManager.GetStreamingExtensionsList(ExtList);
		Assert.AreEqual(1, ExtList.Count, 'Extension should not be deleted with empty selection');
	finally
		ExtList.Free;
	end;
end;

procedure TAccountsPresenterTest.TestLoadStreamingExtensionsToViewPopulatesList;
var
	ExtSettings: TStreamingSettings;
begin
	{Setup: Create multiple extensions}
	ExtSettings.Command := 'player.exe';
	ExtSettings.Parameters := '';
	ExtSettings.StartPath := '';
	ExtSettings.Format := 1;

	FSettingsManager.SetStreamingSettings('.mp4', ExtSettings);
	FSettingsManager.SetStreamingSettings('.avi', ExtSettings);
	FSettingsManager.SetStreamingSettings('.mkv', ExtSettings);

	FPresenter.Initialize('');

	{Verify extensions are in the view's list}
	Assert.AreEqual(3, FView.StreamingExtensionsList.Count, 'All extensions should be loaded');
end;

{LoadGlobalSettingsToView tests}

procedure TAccountsPresenterTest.TestLoadGlobalSettingsLoadsAllSettings;
var
	Settings: TPluginSettings;
begin
	{Setup: Configure various settings}
	Settings := FSettingsManager.GetSettings;
	Settings.LoadSSLDLLOnlyFromPluginDir := True;
	Settings.PreserveFileTime := True;
	Settings.RetryAttempts := 3;
	Settings.AttemptWait := 1500;
	Settings.IconsMode := 2;
	Settings.DisableMultiThreading := True;
	Settings.ShowTrashFolders := True;
	Settings.ShowSharedFolders := True;
	Settings.ShowInvitesFolders := True;
	Settings.PrecalculateHash := True;
	Settings.CheckCRC := True;
	Settings.DescriptionEnabled := True;
	Settings.DescriptionCopyToCloud := True;
	Settings.DescriptionCopyFromCloud := True;
	FSettingsManager.SetSettings(Settings);

	FPresenter.Initialize('');

	{Verify all settings are loaded to view}
	Assert.IsTrue(FView.GetLoadSSLFromPluginDir, 'LoadSSLFromPluginDir should be loaded');
	Assert.IsTrue(FView.GetPreserveFileTime, 'PreserveFileTime should be loaded');
	Assert.AreEqual(3, FView.GetRetryAttempts, 'RetryAttempts should be loaded');
	Assert.AreEqual(1500, FView.GetAttemptWait, 'AttemptWait should be loaded');
	Assert.AreEqual(2, FView.GetIconsMode, 'IconsMode should be loaded');
	Assert.IsTrue(FView.GetDisableMultiThreading, 'DisableMultiThreading should be loaded');
	Assert.IsTrue(FView.GetShowTrashFolders, 'ShowTrashFolders should be loaded');
	Assert.IsTrue(FView.GetShowSharedFolders, 'ShowSharedFolders should be loaded');
	Assert.IsTrue(FView.GetShowInvitesFolders, 'ShowInvitesFolders should be loaded');
	Assert.IsTrue(FView.GetPrecalculateHash, 'PrecalculateHash should be loaded');
	Assert.IsTrue(FView.GetCheckCRC, 'CheckCRC should be loaded');
	Assert.IsTrue(FView.GetDescriptionEnabled, 'DescriptionEnabled should be loaded');
	Assert.IsTrue(FView.GetDescriptionCopyToCloud, 'DescriptionCopyToCloud should be loaded');
	Assert.IsTrue(FView.GetDescriptionCopyFromCloud, 'DescriptionCopyFromCloud should be loaded');
end;

procedure TAccountsPresenterTest.TestLoadGlobalSettingsWithCustomCloudMaxFileSize;
var
	Settings: TPluginSettings;
begin
	{Setup: Set custom max file size}
	Settings := FSettingsManager.GetSettings;
	Settings.CloudMaxFileSize := 1073741824; {1 GB}
	FSettingsManager.SetSettings(Settings);

	FPresenter.Initialize('');

	{Verify custom size is loaded with checkbox enabled}
	Assert.AreEqual(1073741824, FView.GetCloudMaxFileSize, 'CloudMaxFileSize should be loaded');
	Assert.IsTrue(FView.GetCloudMaxFileSizeEnabled, 'CloudMaxFileSize checkbox should be enabled');
	Assert.IsTrue(FView.CloudMaxFileSizeEditEnabled, 'CloudMaxFileSize edit should be enabled');
end;

procedure TAccountsPresenterTest.TestLoadGlobalSettingsWithCustomUserAgent;
var
	Settings: TPluginSettings;
begin
	{Setup: Set custom user agent}
	Settings := FSettingsManager.GetSettings;
	Settings.ConnectionSettings.UserAgent := 'CustomBrowser/2.0';
	FSettingsManager.SetSettings(Settings);

	FPresenter.Initialize('');

	Assert.AreEqual('CustomBrowser/2.0', FView.GetUserAgent, 'Custom user agent should be loaded');
	Assert.IsTrue(FView.GetChangeUserAgent, 'ChangeUserAgent should be checked for custom UA');
	Assert.IsFalse(FView.UserAgentReadOnly, 'User agent edit should be writable for custom UA');
end;

procedure TAccountsPresenterTest.TestLoadGlobalSettingsWithProxyUser;
var
	Settings: TPluginSettings;
begin
	{Setup: Set proxy user}
	Settings := FSettingsManager.GetSettings;
	Settings.ConnectionSettings.ProxySettings.User := 'proxyuser';
	Settings.ConnectionSettings.ProxySettings.Password := 'proxypass';
	Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager := True;
	FSettingsManager.SetSettings(Settings);

	FPresenter.Initialize('');

	Assert.AreEqual('proxyuser', FView.GetProxyUser, 'Proxy user should be loaded');
	Assert.AreEqual('proxypass', FView.GetProxyPassword, 'Proxy password should be loaded');
	Assert.IsTrue(FView.GetProxyUseTCPasswordManager, 'Proxy TC password manager should be loaded');
	Assert.IsTrue(FView.ProxyTCPasswordManagerEnabled, 'Proxy TC password manager checkbox should be enabled');
end;

initialization
	TDUnitX.RegisterTestFixture(TAccountsPresenterTest);

end.
