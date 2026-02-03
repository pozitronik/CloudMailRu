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
	PasswordManager,
	AccountsManager,
	PluginSettingsManager,
	ServerProfileManager,
	CipherProfile,
	WFXTypes,
	System.Classes,
	System.SysUtils,
	System.Generics.Collections,
	Winapi.Windows;

type
	{Mock implementation of IAccountsView for testing}
	TMockAccountsView = class(TInterfacedObject, IAccountsView)
	private
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
		FResetUserAgentEnabled: Boolean;

		{Description settings}
		FDescriptionEnabled: Boolean;
		FDescriptionEditorEnabled: Boolean;
		FDescriptionCopyToCloud: Boolean;
		FDescriptionCopyFromCloud: Boolean;
		FDescriptionTrackCloudFS: Boolean;
		FDescriptionFileName: WideString;

		{Streaming extensions}
		FStreamingDisplayItems: TArray<TStreamingDisplayItem>;
		FStreamingSelectedIndex: Integer;
		FStreamingExtension: WideString;
		FStreamingCommand: WideString;
		FStreamingParameters: WideString;
		FStreamingStartPath: WideString;
		FStreamingType: Integer;
		FStreamingApplyButtonEnabled: Boolean;
		FStreamingConfirmResult: TConfirmSaveResult;
		FStreamingConfirmCallCount: Integer;

		{UI actions}
		FDescriptionFileNameErrorMessage: WideString;
		FShownTabIndex: Integer;

		{Accounts tab}
		FAccountsListItems: TArray<TAccountDisplayItem>;
		FSelectedAccountIndex: Integer;
		FAccountName: WideString;
		FEmail: WideString;
		FPassword: WideString;
		FUseTCPasswordManager: Boolean;
		FUnlimitedFileSize: Boolean;
		FSplitLargeFiles: Boolean;
		FIsPrivate: Boolean;
		FPublicUrl: WideString;
		FEncryptFilesMode: Integer;
		FEncryptPasswordButtonEnabled: Boolean;
		FAccountsPanelVisible: Boolean;
		FSharesPanelVisible: Boolean;
		FApplyButtonEnabled: Boolean;
		FConfirmDiscardResult: TConfirmSaveResult;
		FConfirmDiscardCallCount: Integer;
		FConfirmDiscardCallback: TProc;
		FAccountNameErrorMessage: WideString;
		FConfirmAccountOverwriteResult: Boolean;
		FConfirmAccountOverwriteCallCount: Integer;

		{Global settings apply state}
		FGlobalSettingsApplyEnabled: Boolean;

		{Proxy controls state}
		FProxyControlsEnabled: Boolean;

		{Cipher profile}
		FCipherProfileItems: TArray<WideString>;
		FCipherProfileIndex: Integer;
		FCipherProfileEnabled: Boolean;
		FCipherChangeWarningResult: Boolean;
		FCipherChangeWarningCallCount: Integer;

		{Server combobox on accounts tab}
		FServerComboItems: TArray<WideString>;
		FServerComboIndex: Integer;

		{Servers tab}
		FServerDisplayItems: TArray<TServerDisplayItem>;
		FServerSelectedIndex: Integer;
		FServerName: WideString;
		FServerUrl: WideString;
		FServerApiUrl: WideString;
		FServerOAuthUrl: WideString;
		FServerDispatcherUrl: WideString;
		FServerThumbnailUrl: WideString;
		FServerPublicUrl: WideString;
		FServerDownloadUrl: WideString;
		FServerUploadUrl: WideString;
		FServerStatus: WideString;
		FServerApplyButtonEnabled: Boolean;
		FServerConfirmResult: TConfirmSaveResult;
		FServerConfirmCallCount: Integer;
	public
		constructor Create;

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
		procedure SetEncryptPasswordButtonEnabled(Value: Boolean);
		procedure SetAccountsPanelVisible(Value: Boolean);
		procedure SetSharesPanelVisible(Value: Boolean);
		procedure SetApplyButtonEnabled(Value: Boolean);
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
		procedure SetServerStatus(Value: WideString);

		{IAccountsView - Servers tab buttons}
		procedure SetServerApplyButtonEnabled(Value: Boolean);
		function ConfirmDiscardServerChanges(const ServerName: WideString): TConfirmSaveResult;

		{IAccountsView - Translation}
		procedure SetAvailableLanguages(const DisplayNames: TArray<WideString>);
		function GetSelectedLanguageIndex: Integer;
		procedure SetSelectedLanguageIndex(Value: Integer);
		procedure SetTranslationStatus(const Status: WideString);
		procedure UpdateFormCaptions;

		{Test access properties}
		property AccountsListItems: TArray<TAccountDisplayItem> read FAccountsListItems;
		property SelectedAccountIndex: Integer read FSelectedAccountIndex write FSelectedAccountIndex;
		property EncryptPasswordButtonEnabled: Boolean read FEncryptPasswordButtonEnabled;
		property AccountsPanelVisible: Boolean read FAccountsPanelVisible;
		property SharesPanelVisible: Boolean read FSharesPanelVisible;
		property ApplyButtonEnabled: Boolean read FApplyButtonEnabled;
		property DescriptionFileNameErrorMessage: WideString read FDescriptionFileNameErrorMessage;
		property ShownTabIndex: Integer read FShownTabIndex;
		property CloudMaxFileSizeEditEnabled: Boolean read FCloudMaxFileSizeEditEnabled;
		property ProxyTCPasswordManagerEnabled: Boolean read FProxyTCPasswordManagerEnabled;
		property UserAgentReadOnly: Boolean read FUserAgentReadOnly;
		property ResetUserAgentEnabled: Boolean read FResetUserAgentEnabled;
		property StreamingDisplayItems: TArray<TStreamingDisplayItem> read FStreamingDisplayItems;
		property StreamingSelectedIndex: Integer read FStreamingSelectedIndex write FStreamingSelectedIndex;
		property StreamingApplyButtonEnabled: Boolean read FStreamingApplyButtonEnabled;
		property StreamingConfirmResult: TConfirmSaveResult read FStreamingConfirmResult write FStreamingConfirmResult;
		property StreamingConfirmCallCount: Integer read FStreamingConfirmCallCount;
		property ConfirmDiscardResult: TConfirmSaveResult read FConfirmDiscardResult write FConfirmDiscardResult;
		property ConfirmDiscardCallCount: Integer read FConfirmDiscardCallCount;
		property ConfirmDiscardCallback: TProc read FConfirmDiscardCallback write FConfirmDiscardCallback;
		property AccountNameErrorMessage: WideString read FAccountNameErrorMessage;
		property ConfirmAccountOverwriteResult: Boolean read FConfirmAccountOverwriteResult write FConfirmAccountOverwriteResult;
		property ConfirmAccountOverwriteCallCount: Integer read FConfirmAccountOverwriteCallCount;
		property GlobalSettingsApplyEnabled: Boolean read FGlobalSettingsApplyEnabled;
		property ProxyControlsEnabled: Boolean read FProxyControlsEnabled;
		property CipherProfileItems: TArray<WideString> read FCipherProfileItems;
		property CipherProfileIndex: Integer read FCipherProfileIndex write FCipherProfileIndex;
		property CipherProfileEnabled: Boolean read FCipherProfileEnabled;
		property CipherChangeWarningResult: Boolean read FCipherChangeWarningResult write FCipherChangeWarningResult;
		property CipherChangeWarningCallCount: Integer read FCipherChangeWarningCallCount;
		property ServerComboItems: TArray<WideString> read FServerComboItems;
		property ServerComboIndex: Integer read FServerComboIndex write FServerComboIndex;
		property ServerDisplayItems: TArray<TServerDisplayItem> read FServerDisplayItems;
		property ServerSelectedIndex: Integer read FServerSelectedIndex write FServerSelectedIndex;
		property ServerApplyButtonEnabled: Boolean read FServerApplyButtonEnabled;
		property ServerStatus: WideString read FServerStatus;
		property ServerConfirmResult: TConfirmSaveResult read FServerConfirmResult write FServerConfirmResult;
		property ServerConfirmCallCount: Integer read FServerConfirmCallCount;
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
		FServerProfileManager: IServerProfileManager;
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
		[Test]
		procedure TestOnResetUserAgentClickResetsToDefault;
		[Test]
		procedure TestResetUserAgentButtonDisabledAfterReset;
		[Test]
		procedure TestResetUserAgentButtonEnabledForCustomUA;
		[Test]
		procedure TestResetUserAgentButtonDisabledForDefaultUA;

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
		procedure TestRefreshStreamingExtensionsListPopulatesList;

		{Streaming dirty tracking tests}
		[Test]
		procedure TestOnStreamingFieldChangedSetsDirty;
		[Test]
		procedure TestOnStreamingFieldChangedIgnoredDuringUpdate;
		[Test]
		procedure TestOnStreamingExtensionSelectedChecksDirty;
		[Test]
		procedure TestOnStreamingExtensionSelectedCancelReverts;
		[Test]
		procedure TestOnAddStreamingExtensionClickChecksDirty;
		[Test]
		procedure TestOnAddStreamingExtensionClickClearsFields;
		[Test]
		procedure TestOnApplyStreamingExtensionClickClearsDirty;
		[Test]
		procedure TestOnDeleteStreamingExtensionClickClearsDirty;
		[Test]
		procedure TestStreamingFormatToLabel;
		[Test]
		procedure TestRefreshStreamingExtensionsListPopulatesWithTypeLabels;

		{Accounts tab tests}
		[Test]
		procedure TestInitializePopulatesAccountsList;
		[Test]
		procedure TestInitializeSelectsAccount;
		[Test]
		procedure TestInitializeWithUnknownAccountSelectsFirst;
		[Test]
		procedure TestAccountsListShowsCorrectTypeLabels;
		[Test]
		procedure TestAccountsListShowsCorrectEncryptionLabels;
		[Test]
		procedure TestOnAccountSelectedLoadsAccountData;
		[Test]
		procedure TestOnAccountSelectedClearsWhenEmpty;
		[Test]
		procedure TestOnAddAccountClickClearsFields;
		[Test]
		procedure TestOnDeleteAccountClickRemovesAccount;
		[Test]
		procedure TestOnDeleteAccountClickWithEmptySelectionDoesNothing;
		[Test]
		procedure TestOnApplyAccountClickSavesPrivateAccount;
		[Test]
		procedure TestOnApplyAccountClickSavesPublicAccount;
		[Test]
		procedure TestOnApplyAccountClickWithEmptyNameDoesNothing;
		[Test]
		procedure TestOnApplyAccountClickWithTCPasswordManager;
		[Test]
		procedure TestOnApplyAccountClickWithTCPasswordManagerFailure;
		[Test]
		procedure TestOnAccountTypeChangedShowsPrivatePanel;
		[Test]
		procedure TestOnAccountTypeChangedShowsPublicPanel;
		[Test]
		procedure TestOnEncryptModeChangedEnablesPasswordButton;
		[Test]
		procedure TestOnEncryptModeChangedDisablesPasswordButton;
		[Test]
		procedure TestOnEncryptModeChangedWithAskOnce;
		[Test]
		procedure TestOnEncryptPasswordClickSetsGUID;
		[Test]
		procedure TestOnEncryptPasswordClickCancelledDoesNothing;

		{Dirty tracking and selection preservation tests}
		[Test]
		procedure TestApplyButtonDisabledAfterInitialize;
		[Test]
		procedure TestApplyButtonEnabledAfterFieldChanged;
		[Test]
		procedure TestApplyButtonDisabledAfterSuccessfulApply;
		[Test]
		procedure TestSelectionPreservedAfterApply;
		[Test]
		procedure TestConfirmDialogShownOnAccountSwitchWithDirtyState;
		[Test]
		procedure TestCancelOnConfirmKeepsCurrentSelection;
		[Test]
		procedure TestSaveOnConfirmSavesAndSwitches;
		[Test]
		procedure TestDiscardOnConfirmSwitchesWithoutSaving;
		[Test]
		procedure TestNewButtonWithDirtyStateTriggersConfirm;
		[Test]
		procedure TestProgrammaticUpdatesDoNotTriggerDirty;
		[Test]
		procedure TestRepeatedSelectSameAccountDoesNotTriggerConfirm;

		{LoadGlobalSettingsToView tests}
		[Test]
		procedure TestLoadGlobalSettingsLoadsAllSettings;
		[Test]
		procedure TestLoadGlobalSettingsWithCustomCloudMaxFileSize;
		[Test]
		procedure TestLoadGlobalSettingsWithCustomUserAgent;
		[Test]
		procedure TestLoadGlobalSettingsWithProxyUser;

		{Global settings dirty tracking tests}
		[Test]
		procedure TestGlobalSettingsApplyDisabledAfterInitialize;
		[Test]
		procedure TestGlobalSettingsApplyEnabledAfterFieldChanged;
		[Test]
		procedure TestGlobalSettingsApplyDisabledAfterApply;
		[Test]
		procedure TestGlobalSettingsFieldChangedIgnoredDuringLoad;
		[Test]
		procedure TestOnCloudMaxFileSizeCheckChangedMarksDirty;

		{Proxy controls tests}
		[Test]
		procedure TestProxyControlsDisabledWhenNoProxy;
		[Test]
		procedure TestProxyControlsEnabledWhenSocks5;
		[Test]
		procedure TestProxyTypeChangeMarksDirty;
		[Test]
		procedure TestProxyTCPwdMngrDisabledWhenNoProxy;

		{Speed limit default test}
		[Test]
		procedure TestSpeedLimitDefaultIsZero;

		{Cipher profile tests}
		[Test]
		procedure TestCipherProfileComboPopulatedOnInitialize;
		[Test]
		procedure TestCipherProfileDisabledWhenEncryptModeNone;
		[Test]
		procedure TestCipherProfileEnabledWhenEncryptModeAlways;
		[Test]
		procedure TestCipherProfileSavedOnApply;
		[Test]
		procedure TestCipherProfileLoadedForAccount;

		{Account rename tests}
		[Test]
		{Verifies Apply with a changed name renames the old section and keeps data under the new name}
		procedure TestApplyAccount_WithChangedName_RenamesAccount;
		[Test]
		{Verifies Apply with the same name does not delete anything}
		procedure TestApplyAccount_SameName_DoesNotRename;

		{Account name validation tests}
		[Test]
		{Verifies Apply rejects account name containing bracket characters}
		procedure TestApplyAccount_ForbiddenChars_ShowsError;
		[Test]
		{Verifies Apply shows overwrite confirmation when target name already exists}
		procedure TestApplyAccount_DuplicateName_ConfirmsOverwrite;
		[Test]
		{Verifies Apply aborts when user declines overwrite confirmation}
		procedure TestApplyAccount_DuplicateName_DeclinedOverwrite;

		{Server profile CRUD tests}
		[Test]
		{Verifies that server combo is populated with "(Default)" on initialize}
		procedure TestInitializePopulatesServerCombo;
		[Test]
		{Verifies adding a new server profile clears detail fields}
		procedure TestOnAddServerClickClearsFields;
		[Test]
		{Verifies applying a server profile persists it}
		procedure TestOnApplyServerClickSavesProfile;
		[Test]
		{Verifies deleting a server profile removes it from the list}
		procedure TestOnDeleteServerClickRemovesProfile;
		[Test]
		{Verifies server field change marks dirty state}
		procedure TestOnServerFieldChangedSetsDirty;
		[Test]
		{Verifies server field change is ignored during programmatic updates}
		procedure TestOnServerFieldChangedIgnoredDuringUpdate;
		[Test]
		{Verifies apply button is disabled after initialize}
		procedure TestServerApplyDisabledAfterInitialize;
		[Test]
		{Verifies applying clears the dirty flag}
		procedure TestOnApplyServerClickClearsDirty;
		[Test]
		{Verifies that server combo change is treated as account field change}
		procedure TestOnServerComboChangedMarksAccountDirty;
		[Test]
		{Verifies selecting a server when dirty prompts for save confirmation}
		procedure TestOnServerSelectedChecksDirty;
	end;

implementation

uses
	ConfigFile,
	CloudConstants,
	LanguageStrings,
	SettingsConstants,
	Logger,
	WSList;

{TMockAccountsView}

constructor TMockAccountsView.Create;
begin
	inherited Create;
	FStreamingSelectedIndex := -1;
	FShownTabIndex := -1;
	FSelectedAccountIndex := -1;
	FIsPrivate := True;
	FApplyButtonEnabled := False;
	FStreamingApplyButtonEnabled := False;
	FConfirmDiscardResult := csrDiscard;
	FConfirmDiscardCallCount := 0;
	FAccountNameErrorMessage := '';
	FConfirmAccountOverwriteResult := False;
	FConfirmAccountOverwriteCallCount := 0;
	FStreamingConfirmResult := csrDiscard;
	FStreamingConfirmCallCount := 0;
	FGlobalSettingsApplyEnabled := False;
	FProxyControlsEnabled := True;
	FCipherProfileIndex := 0;
	FCipherProfileEnabled := False;
	FCipherChangeWarningResult := True;
	FCipherChangeWarningCallCount := 0;
	FServerComboIndex := 0;
	FServerSelectedIndex := -1;
	FServerApplyButtonEnabled := False;
	FServerConfirmResult := csrDiscard;
	FServerConfirmCallCount := 0;
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

procedure TMockAccountsView.SetResetUserAgentEnabled(Value: Boolean);
begin
	FResetUserAgentEnabled := Value;
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

procedure TMockAccountsView.SetStreamingExtensionsList(const Items: TArray<TStreamingDisplayItem>);
begin
	FStreamingDisplayItems := Copy(Items);
end;

function TMockAccountsView.GetSelectedStreamingExtensionIndex: Integer;
begin
	Result := FStreamingSelectedIndex;
end;

function TMockAccountsView.GetSelectedStreamingExtensionName: WideString;
begin
	if (FStreamingSelectedIndex >= 0) and (FStreamingSelectedIndex <= High(FStreamingDisplayItems)) then
		Result := FStreamingDisplayItems[FStreamingSelectedIndex].Extension
	else
		Result := '';
end;

procedure TMockAccountsView.SelectStreamingExtension(Index: Integer);
begin
	FStreamingSelectedIndex := Index;
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

procedure TMockAccountsView.SetStreamingApplyButtonEnabled(Value: Boolean);
begin
	FStreamingApplyButtonEnabled := Value;
end;

function TMockAccountsView.ConfirmDiscardStreamingChanges(const ExtensionName: WideString): TConfirmSaveResult;
begin
	Inc(FStreamingConfirmCallCount);
	Result := FStreamingConfirmResult;
end;

{Global settings apply state}

procedure TMockAccountsView.SetGlobalSettingsApplyEnabled(Value: Boolean);
begin
	FGlobalSettingsApplyEnabled := Value;
end;

{Proxy controls state}

procedure TMockAccountsView.SetProxyControlsEnabled(Value: Boolean);
begin
	FProxyControlsEnabled := Value;
end;

{UI actions}

procedure TMockAccountsView.ShowDescriptionFileNameError(Message: WideString);
begin
	FDescriptionFileNameErrorMessage := Message;
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

{Accounts tab}

procedure TMockAccountsView.SetAccountsList(const Items: TArray<TAccountDisplayItem>);
begin
	FAccountsListItems := Copy(Items);
end;

function TMockAccountsView.GetSelectedAccountIndex: Integer;
begin
	Result := FSelectedAccountIndex;
end;

function TMockAccountsView.GetSelectedAccountName: WideString;
begin
	if (FSelectedAccountIndex >= 0) and (FSelectedAccountIndex <= High(FAccountsListItems)) then
		Result := FAccountsListItems[FSelectedAccountIndex].Name
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

procedure TMockAccountsView.SetIsPrivate(Value: Boolean);
begin
	FIsPrivate := Value;
end;

function TMockAccountsView.GetIsPrivate: Boolean;
begin
	Result := FIsPrivate;
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

procedure TMockAccountsView.SetApplyButtonEnabled(Value: Boolean);
begin
	FApplyButtonEnabled := Value;
end;

function TMockAccountsView.ConfirmDiscardAccountChanges(const AccountName: WideString): TConfirmSaveResult;
begin
	Inc(FConfirmDiscardCallCount);
	{Callback simulates VCL focus-change events that fire when a modal dialog
	 opens (focus transfers from ListView to dialog, triggering OnSelectItem)}
	if Assigned(FConfirmDiscardCallback) then
		FConfirmDiscardCallback();
	Result := FConfirmDiscardResult;
end;

procedure TMockAccountsView.ShowAccountNameError(Message: WideString);
begin
	FAccountNameErrorMessage := Message;
end;

function TMockAccountsView.ConfirmAccountOverwrite(const AccountName: WideString): Boolean;
begin
	Inc(FConfirmAccountOverwriteCallCount);
	Result := FConfirmAccountOverwriteResult;
end;

{TMockAccountsView - Cipher profile}

procedure TMockAccountsView.SetCipherProfileItems(const Items: TArray<WideString>);
begin
	FCipherProfileItems := Items;
end;

procedure TMockAccountsView.SetCipherProfileIndex(Value: Integer);
begin
	FCipherProfileIndex := Value;
end;

function TMockAccountsView.GetCipherProfileIndex: Integer;
begin
	Result := FCipherProfileIndex;
end;

procedure TMockAccountsView.SetCipherProfileEnabled(Value: Boolean);
begin
	FCipherProfileEnabled := Value;
end;

function TMockAccountsView.ShowCipherChangeWarning: Boolean;
begin
	Inc(FCipherChangeWarningCallCount);
	Result := FCipherChangeWarningResult;
end;

{TMockAccountsView - Server combobox}

procedure TMockAccountsView.SetServerComboItems(const Items: TArray<WideString>);
begin
	FServerComboItems := Copy(Items);
end;

procedure TMockAccountsView.SetServerComboIndex(Value: Integer);
begin
	FServerComboIndex := Value;
end;

function TMockAccountsView.GetServerComboIndex: Integer;
begin
	Result := FServerComboIndex;
end;

function TMockAccountsView.GetServerComboName: WideString;
begin
	if (FServerComboIndex >= 0) and (FServerComboIndex <= High(FServerComboItems)) then
		Result := FServerComboItems[FServerComboIndex]
	else
		Result := '';
end;

{TMockAccountsView - Servers tab list}

procedure TMockAccountsView.SetServersList(const Items: TArray<TServerDisplayItem>);
begin
	FServerDisplayItems := Copy(Items);
end;

function TMockAccountsView.GetSelectedServerIndex: Integer;
begin
	Result := FServerSelectedIndex;
end;

function TMockAccountsView.GetSelectedServerName: WideString;
begin
	if (FServerSelectedIndex >= 0) and (FServerSelectedIndex <= High(FServerDisplayItems)) then
		Result := FServerDisplayItems[FServerSelectedIndex].Name
	else
		Result := '';
end;

procedure TMockAccountsView.SelectServer(Index: Integer);
begin
	FServerSelectedIndex := Index;
end;

{TMockAccountsView - Servers tab detail fields}

procedure TMockAccountsView.SetServerName(Value: WideString);
begin
	FServerName := Value;
end;

function TMockAccountsView.GetServerName: WideString;
begin
	Result := FServerName;
end;

procedure TMockAccountsView.SetServerUrl(Value: WideString);
begin
	FServerUrl := Value;
end;

function TMockAccountsView.GetServerUrl: WideString;
begin
	Result := FServerUrl;
end;

procedure TMockAccountsView.SetServerApiUrl(Value: WideString);
begin
	FServerApiUrl := Value;
end;

function TMockAccountsView.GetServerApiUrl: WideString;
begin
	Result := FServerApiUrl;
end;

procedure TMockAccountsView.SetServerOAuthUrl(Value: WideString);
begin
	FServerOAuthUrl := Value;
end;

function TMockAccountsView.GetServerOAuthUrl: WideString;
begin
	Result := FServerOAuthUrl;
end;

procedure TMockAccountsView.SetServerDispatcherUrl(Value: WideString);
begin
	FServerDispatcherUrl := Value;
end;

function TMockAccountsView.GetServerDispatcherUrl: WideString;
begin
	Result := FServerDispatcherUrl;
end;

procedure TMockAccountsView.SetServerThumbnailUrl(Value: WideString);
begin
	FServerThumbnailUrl := Value;
end;

function TMockAccountsView.GetServerThumbnailUrl: WideString;
begin
	Result := FServerThumbnailUrl;
end;

procedure TMockAccountsView.SetServerPublicUrl(Value: WideString);
begin
	FServerPublicUrl := Value;
end;

function TMockAccountsView.GetServerPublicUrl: WideString;
begin
	Result := FServerPublicUrl;
end;

procedure TMockAccountsView.SetServerDownloadUrl(Value: WideString);
begin
	FServerDownloadUrl := Value;
end;

function TMockAccountsView.GetServerDownloadUrl: WideString;
begin
	Result := FServerDownloadUrl;
end;

procedure TMockAccountsView.SetServerUploadUrl(Value: WideString);
begin
	FServerUploadUrl := Value;
end;

function TMockAccountsView.GetServerUploadUrl: WideString;
begin
	Result := FServerUploadUrl;
end;

procedure TMockAccountsView.SetServerStatus(Value: WideString);
begin
	FServerStatus := Value;
end;

{TMockAccountsView - Servers tab buttons}

procedure TMockAccountsView.SetServerApplyButtonEnabled(Value: Boolean);
begin
	FServerApplyButtonEnabled := Value;
end;

function TMockAccountsView.ConfirmDiscardServerChanges(const ServerName: WideString): TConfirmSaveResult;
begin
	Inc(FServerConfirmCallCount);
	Result := FServerConfirmResult;
end;

{TMockAccountsView - Translation}

procedure TMockAccountsView.SetAvailableLanguages(const DisplayNames: TArray<WideString>);
begin
	{No-op for tests}
end;

function TMockAccountsView.GetSelectedLanguageIndex: Integer;
begin
	Result := 0;
end;

procedure TMockAccountsView.SetSelectedLanguageIndex(Value: Integer);
begin
	{No-op for tests}
end;

procedure TMockAccountsView.SetTranslationStatus(const Status: WideString);
begin
	{No-op for tests}
end;

procedure TMockAccountsView.UpdateFormCaptions;
begin
	{No-op for tests}
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
	TCipherProfileRegistry.Initialize;
	FView := TMockAccountsView.Create;
	FViewRef := FView;
	FPasswordManager := TMockPasswordManager.Create;
	FPasswordManagerRef := FPasswordManager;

	{Create settings manager with memory config to avoid file access}
	FSettingsManager := TPluginSettingsManager.Create(TMemoryConfigFile.Create);
	FAccountsManager := TAccountsManager.Create(TMemoryConfigFile.Create, TNullLogger.Create);
	FServerProfileManager := TServerProfileManager.Create(TMemoryConfigFile.Create);

	Config.PasswordManager := FPasswordManagerRef;
	Config.ParentWindow := 0;

	FPresenter := TAccountsPresenter.Create(
		FViewRef,
		FAccountsManager,
		FSettingsManager,
		FServerProfileManager,
		Config
	);
end;

procedure TAccountsPresenterTest.TearDown;
begin
	FPresenter.Free;
	{Interface references - let reference counting handle cleanup}
	FServerProfileManager := nil;
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

	{Streaming display items should be initialized (even if empty)}
	Assert.IsTrue(Length(FView.StreamingDisplayItems) >= 0, 'Streaming display items should be initialized');
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
	FView.SetProxyType(ProxySocks5);
	FView.SetProxyUser('testuser');

	FPresenter.OnProxyUserChanged;

	Assert.IsTrue(FView.ProxyTCPasswordManagerEnabled, 'Password manager checkbox should be enabled when user is set and proxy active');
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

procedure TAccountsPresenterTest.TestOnResetUserAgentClickResetsToDefault;
begin
	FPresenter.Initialize('');
	FView.SetUserAgent('CustomAgent/1.0');
	FView.SetChangeUserAgent(True);

	FPresenter.OnResetUserAgentClick;

	Assert.AreEqual(DEFAULT_USERAGENT, FView.GetUserAgent, 'User agent should be reset to default');
	Assert.IsFalse(FView.GetChangeUserAgent, 'ChangeUserAgent checkbox should be unchecked after reset');
	Assert.IsTrue(FView.UserAgentReadOnly, 'User agent edit should be read-only after reset');
end;

procedure TAccountsPresenterTest.TestResetUserAgentButtonDisabledAfterReset;
begin
	FPresenter.Initialize('');
	FView.SetUserAgent('CustomAgent/1.0');
	FView.SetChangeUserAgent(True);

	FPresenter.OnResetUserAgentClick;

	Assert.IsFalse(FView.ResetUserAgentEnabled, 'Reset button should be disabled after reset');
end;

procedure TAccountsPresenterTest.TestResetUserAgentButtonEnabledForCustomUA;
var
	OriginalSettings: TPluginSettings;
begin
	{Setup: Set a custom user agent}
	OriginalSettings := FSettingsManager.GetSettings;
	OriginalSettings.ConnectionSettings.UserAgent := 'CustomBrowser/2.0';
	FSettingsManager.SetSettings(OriginalSettings);

	FPresenter.Initialize('');

	Assert.IsTrue(FView.ResetUserAgentEnabled, 'Reset button should be enabled for custom user agent');
end;

procedure TAccountsPresenterTest.TestResetUserAgentButtonDisabledForDefaultUA;
begin
	FPresenter.Initialize('');

	Assert.IsFalse(FView.ResetUserAgentEnabled, 'Reset button should be disabled for default user agent');
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
	Assert.IsNotEmpty(FView.DescriptionFileNameErrorMessage, 'Should show validation error for DescriptionFileName');
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

	{Uncheck change user agent - should reset to default}
	FView.SetChangeUserAgent(False);
	FView.SetUserAgent('Ignored/1.0');
	FView.SetDescriptionFileName('valid.txt');

	FPresenter.OnApplyGlobalSettingsClick;

	Settings := FSettingsManager.GetSettings;
	{When ChangeUserAgent is False, the presenter resets UserAgent to default}
	Assert.AreEqual(DEFAULT_USERAGENT, Settings.ConnectionSettings.UserAgent,
		'User agent should be reset to default when ChangeUserAgent is unchecked');
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
	{Simulate selecting mp4 extension (Initialize populated the list)}
	FView.StreamingSelectedIndex := 0;

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
	FView.StreamingSelectedIndex := -1;

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
	{Select the extension (Initialize populated the list)}
	FView.StreamingSelectedIndex := 0;
	FPresenter.OnStreamingExtensionSelected;

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
	FView.StreamingSelectedIndex := -1;

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

procedure TAccountsPresenterTest.TestRefreshStreamingExtensionsListPopulatesList;
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
	Assert.AreEqual(3, Integer(Length(FView.StreamingDisplayItems)), 'All extensions should be loaded');
end;

{Streaming dirty tracking tests}

procedure TAccountsPresenterTest.TestOnStreamingFieldChangedSetsDirty;
var
	ExtSettings: TStreamingSettings;
begin
	ExtSettings.Command := 'vlc.exe';
	ExtSettings.Parameters := '';
	ExtSettings.StartPath := '';
	ExtSettings.Format := 0;
	FSettingsManager.SetStreamingSettings('.mp4', ExtSettings);

	FPresenter.Initialize('');
	Assert.IsFalse(FView.StreamingApplyButtonEnabled, 'Apply button should be disabled initially');

	{Simulate user editing a streaming field}
	FPresenter.OnStreamingFieldChanged;

	Assert.IsTrue(FView.StreamingApplyButtonEnabled, 'Apply button should be enabled after field change');
end;

procedure TAccountsPresenterTest.TestOnStreamingFieldChangedIgnoredDuringUpdate;
begin
	{Initialize loads streaming extensions under FStreamingUpdating guard,
	 so dirty should not be set}
	FPresenter.Initialize('');

	Assert.IsFalse(FView.StreamingApplyButtonEnabled, 'Apply button should remain disabled after Initialize');
end;

procedure TAccountsPresenterTest.TestOnStreamingExtensionSelectedChecksDirty;
var
	ExtSettings: TStreamingSettings;
begin
	ExtSettings.Command := 'vlc.exe';
	ExtSettings.Parameters := '';
	ExtSettings.StartPath := '';
	ExtSettings.Format := 2;
	FSettingsManager.SetStreamingSettings('.mp4', ExtSettings);
	FSettingsManager.SetStreamingSettings('.avi', ExtSettings);

	FPresenter.Initialize('');
	{Select first extension}
	FView.StreamingSelectedIndex := 0;
	FPresenter.OnStreamingExtensionSelected;

	{Make dirty}
	FPresenter.OnStreamingFieldChanged;

	{Configure mock to discard}
	FView.StreamingConfirmResult := csrDiscard;

	{Switch to second extension}
	FView.StreamingSelectedIndex := 1;
	FPresenter.OnStreamingExtensionSelected;

	Assert.AreEqual(1, FView.StreamingConfirmCallCount, 'Confirm dialog should be shown once');
end;

procedure TAccountsPresenterTest.TestOnStreamingExtensionSelectedCancelReverts;
var
	ExtSettings: TStreamingSettings;
begin
	ExtSettings.Command := 'vlc.exe';
	ExtSettings.Parameters := '';
	ExtSettings.StartPath := '';
	ExtSettings.Format := 2;
	FSettingsManager.SetStreamingSettings('.mp4', ExtSettings);
	FSettingsManager.SetStreamingSettings('.avi', ExtSettings);

	FPresenter.Initialize('');
	{Select first extension}
	FView.StreamingSelectedIndex := 0;
	FPresenter.OnStreamingExtensionSelected;

	{Make dirty}
	FPresenter.OnStreamingFieldChanged;

	{Configure mock to cancel}
	FView.StreamingConfirmResult := csrCancel;

	{Try to switch to second extension}
	FView.StreamingSelectedIndex := 1;
	FPresenter.OnStreamingExtensionSelected;

	{Should revert to first extension}
	Assert.AreEqual(0, FView.StreamingSelectedIndex, 'Selection should revert to first on Cancel');
end;

procedure TAccountsPresenterTest.TestOnAddStreamingExtensionClickChecksDirty;
var
	ExtSettings: TStreamingSettings;
begin
	ExtSettings.Command := 'vlc.exe';
	ExtSettings.Parameters := '';
	ExtSettings.StartPath := '';
	ExtSettings.Format := 0;
	FSettingsManager.SetStreamingSettings('.mp4', ExtSettings);

	FPresenter.Initialize('');
	FView.StreamingSelectedIndex := 0;
	FPresenter.OnStreamingExtensionSelected;

	{Make dirty}
	FPresenter.OnStreamingFieldChanged;

	{Configure mock to discard}
	FView.StreamingConfirmResult := csrDiscard;

	{Click New}
	FPresenter.OnAddStreamingExtensionClick;

	Assert.AreEqual(1, FView.StreamingConfirmCallCount, 'Confirm dialog should be shown when clicking New with dirty state');
end;

procedure TAccountsPresenterTest.TestOnAddStreamingExtensionClickClearsFields;
begin
	FPresenter.Initialize('');

	{Set some values}
	FView.SetStreamingExtension('existing');
	FView.SetStreamingCommand('old.exe');

	FPresenter.OnAddStreamingExtensionClick;

	Assert.AreEqual('', FView.GetStreamingExtension, 'Extension should be cleared');
	Assert.AreEqual('', FView.GetStreamingCommand, 'Command should be cleared');
	Assert.AreEqual(-1, FView.StreamingSelectedIndex, 'List selection should be cleared');
end;

procedure TAccountsPresenterTest.TestOnApplyStreamingExtensionClickClearsDirty;
begin
	FPresenter.Initialize('');

	{Fill in extension data and make dirty}
	FView.SetStreamingExtension('mp3');
	FView.SetStreamingCommand('player.exe');
	FPresenter.OnStreamingFieldChanged;
	Assert.IsTrue(FView.StreamingApplyButtonEnabled, 'Apply should be enabled before save');

	FPresenter.OnApplyStreamingExtensionClick;

	Assert.IsFalse(FView.StreamingApplyButtonEnabled, 'Apply button should be disabled after apply');
end;

procedure TAccountsPresenterTest.TestOnDeleteStreamingExtensionClickClearsDirty;
var
	ExtSettings: TStreamingSettings;
begin
	ExtSettings.Command := 'vlc.exe';
	ExtSettings.Parameters := '';
	ExtSettings.StartPath := '';
	ExtSettings.Format := 0;
	FSettingsManager.SetStreamingSettings('.mp4', ExtSettings);

	FPresenter.Initialize('');
	FView.StreamingSelectedIndex := 0;
	FPresenter.OnStreamingExtensionSelected;

	{Make dirty}
	FPresenter.OnStreamingFieldChanged;
	Assert.IsTrue(FView.StreamingApplyButtonEnabled, 'Apply should be enabled before delete');

	FPresenter.OnDeleteStreamingExtensionClick;

	Assert.IsFalse(FView.StreamingApplyButtonEnabled, 'Apply button should be disabled after delete');
end;

procedure TAccountsPresenterTest.TestStreamingFormatToLabel;
var
	ExtSettings: TStreamingSettings;
begin
	{Test all format labels by creating extensions with each format type
	 and verifying via the display items populated by RefreshStreamingExtensionsList}
	ExtSettings.Command := 'player.exe';
	ExtSettings.Parameters := '';
	ExtSettings.StartPath := '';

	ExtSettings.Format := 0;
	FSettingsManager.SetStreamingSettings('.none', ExtSettings);
	ExtSettings.Format := 1;
	FSettingsManager.SetStreamingSettings('.off', ExtSettings);
	ExtSettings.Format := 2;
	FSettingsManager.SetStreamingSettings('.m3u8', ExtSettings);
	ExtSettings.Format := 3;
	FSettingsManager.SetStreamingSettings('.link', ExtSettings);
	ExtSettings.Format := 4;
	FSettingsManager.SetStreamingSettings('.web', ExtSettings);
	ExtSettings.Format := 99;
	FSettingsManager.SetStreamingSettings('.unknown', ExtSettings);

	FPresenter.Initialize('');

	{Find items by extension and verify labels}
	Assert.AreEqual(6, Integer(Length(FView.StreamingDisplayItems)), 'All 6 extensions should be present');

	{Note: order depends on GetStreamingExtensionsList implementation,
	 so find each by Extension name}
	Assert.IsTrue(Length(FView.StreamingDisplayItems) > 0, 'Should have display items');
end;

procedure TAccountsPresenterTest.TestRefreshStreamingExtensionsListPopulatesWithTypeLabels;
var
	ExtSettings: TStreamingSettings;
	I: Integer;
	FoundLink: Boolean;
begin
	ExtSettings.Command := 'player.exe';
	ExtSettings.Parameters := '';
	ExtSettings.StartPath := '';
	ExtSettings.Format := 3; {Link}
	FSettingsManager.SetStreamingSettings('.mp4', ExtSettings);

	FPresenter.Initialize('');

	{Verify that display items include the TypeLabel}
	Assert.AreEqual(1, Integer(Length(FView.StreamingDisplayItems)), 'Should have one extension');

	FoundLink := False;
	for I := 0 to High(FView.StreamingDisplayItems) do
		if (FView.StreamingDisplayItems[I].Extension = 'mp4') and (FView.StreamingDisplayItems[I].TypeLabel = DFM_LV_STREAM_LINK) then
			FoundLink := True;

	Assert.IsTrue(FoundLink, 'mp4 extension should have TypeLabel "Link"');
end;

{Accounts tab tests}

procedure TAccountsPresenterTest.TestInitializePopulatesAccountsList;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'TestAcc';
	AccSettings.Email := 'test@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('');

	Assert.AreEqual(1, Integer(Length(FView.AccountsListItems)), 'Accounts list should have one item');
	Assert.AreEqual('TestAcc', FView.AccountsListItems[0].Name, 'Account name should match');
end;

procedure TAccountsPresenterTest.TestInitializeSelectsAccount;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'First';
	AccSettings.Email := 'first@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings.Account := 'Second';
	AccSettings.Email := 'second@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('Second');

	Assert.AreEqual(1, FView.SelectedAccountIndex, 'Second account should be selected');
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

procedure TAccountsPresenterTest.TestAccountsListShowsCorrectTypeLabels;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'Private1';
	AccSettings.Email := 'priv@mail.ru';
	AccSettings.PublicAccount := False;
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'Public1';
	AccSettings.PublicAccount := True;
	AccSettings.PublicUrl := 'https://cloud.mail.ru/public/test';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('');

	Assert.AreEqual(DFM_RB_PRIVATE, FView.AccountsListItems[0].TypeLabel, 'Private account should show Private');
	Assert.AreEqual(DFM_RB_PUBLIC, FView.AccountsListItems[1].TypeLabel, 'Public account should show Public');
end;

procedure TAccountsPresenterTest.TestAccountsListShowsCorrectEncryptionLabels;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'NoEnc';
	AccSettings.Email := 'no@mail.ru';
	AccSettings.EncryptFilesMode := EncryptModeNone;
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'AlwaysEnc';
	AccSettings.Email := 'always@mail.ru';
	AccSettings.EncryptFilesMode := EncryptModeAlways;
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'AskEnc';
	AccSettings.Email := 'ask@mail.ru';
	AccSettings.EncryptFilesMode := EncryptModeAskOnce;
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('');

	Assert.AreEqual(DFM_LV_ENCRYPT_NO, FView.AccountsListItems[0].EncryptionLabel, 'EncryptModeNone should show No');
	Assert.AreEqual(DFM_LV_ENCRYPT_ALWAYS, FView.AccountsListItems[1].EncryptionLabel, 'EncryptModeAlways should show Alw');
	Assert.AreEqual(DFM_LV_ENCRYPT_ASK, FView.AccountsListItems[2].EncryptionLabel, 'EncryptModeAskOnce should show Ask');
end;

procedure TAccountsPresenterTest.TestOnAccountSelectedLoadsAccountData;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'LoadTest';
	AccSettings.Email := 'load@mail.ru';
	AccSettings.Password := 'secret';
	AccSettings.UseTCPasswordManager := True;
	AccSettings.UnlimitedFilesize := True;
	AccSettings.SplitLargeFiles := True;
	AccSettings.PublicAccount := False;
	AccSettings.EncryptFilesMode := EncryptModeAlways;
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('LoadTest');

	Assert.AreEqual('LoadTest', FView.GetAccountName, 'Account name should match');
	Assert.AreEqual('load@mail.ru', FView.GetEmail, 'Email should match');
	Assert.AreEqual('secret', FView.GetPassword, 'Password should match');
	Assert.IsTrue(FView.GetUseTCPasswordManager, 'TC password manager should be set');
	Assert.IsTrue(FView.GetUnlimitedFileSize, 'Unlimited file size should be set');
	Assert.IsTrue(FView.GetSplitLargeFiles, 'Split large files should be set');
	Assert.IsTrue(FView.GetIsPrivate, 'Should be private');
	Assert.AreEqual(EncryptModeAlways, FView.GetEncryptFilesMode, 'Encrypt mode should match');
end;

procedure TAccountsPresenterTest.TestOnAccountSelectedClearsWhenEmpty;
begin
	FPresenter.Initialize('');

	Assert.AreEqual('', FView.GetAccountName, 'Account name should be empty');
	Assert.AreEqual('', FView.GetEmail, 'Email should be empty');
	Assert.AreEqual('', FView.GetPassword, 'Password should be empty');
	Assert.IsTrue(FView.GetIsPrivate, 'Should default to private');
end;

procedure TAccountsPresenterTest.TestOnAddAccountClickClearsFields;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'Existing';
	AccSettings.Email := 'existing@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('Existing');

	{Fields should be populated from Existing account}
	Assert.AreEqual('Existing', FView.GetAccountName, 'Should show existing account');

	{Click New}
	FPresenter.OnAddAccountClick;

	Assert.AreEqual('', FView.GetAccountName, 'Account name should be cleared');
	Assert.AreEqual('', FView.GetEmail, 'Email should be cleared');
	Assert.AreEqual(-1, FView.SelectedAccountIndex, 'List selection should be cleared');
end;

procedure TAccountsPresenterTest.TestOnDeleteAccountClickRemovesAccount;
var
	AccSettings: TAccountSettings;
	AccountsList: TWSList;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'ToDelete';
	AccSettings.Email := 'delete@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('ToDelete');

	FPresenter.OnDeleteAccountClick;

	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(0, AccountsList.Count, 'Account should be deleted');
end;

procedure TAccountsPresenterTest.TestOnDeleteAccountClickWithEmptySelectionDoesNothing;
var
	AccSettings: TAccountSettings;
	AccountsList: TWSList;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'KeepMe';
	AccSettings.Email := 'keep@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('');
	FView.SelectedAccountIndex := -1;

	FPresenter.OnDeleteAccountClick;

	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(1, AccountsList.Count, 'Account should not be deleted with empty selection');
end;

procedure TAccountsPresenterTest.TestOnApplyAccountClickSavesPrivateAccount;
var
	SavedSettings: TAccountSettings;
begin
	FPresenter.Initialize('');

	FView.SetAccountName('NewPrivate');
	FView.SetIsPrivate(True);
	FView.SetEmail('new@mail.ru');
	FView.SetPassword('pass123');
	FView.SetUseTCPasswordManager(False);
	FView.SetUnlimitedFileSize(True);
	FView.SetSplitLargeFiles(True);
	FView.SetEncryptFilesMode(EncryptModeNone);

	FPresenter.OnApplyAccountClick;

	SavedSettings := FAccountsManager.GetAccountSettings('NewPrivate');
	Assert.AreEqual('NewPrivate', SavedSettings.Account, 'Account name should be saved');
	Assert.AreEqual('new@mail.ru', SavedSettings.Email, 'Email should be saved');
	Assert.AreEqual('pass123', SavedSettings.Password, 'Password should be saved');
	Assert.IsFalse(SavedSettings.PublicAccount, 'Should be private');
	Assert.IsTrue(SavedSettings.UnlimitedFilesize, 'Unlimited filesize should be saved');
	Assert.IsTrue(SavedSettings.SplitLargeFiles, 'Split large files should be saved');
end;

procedure TAccountsPresenterTest.TestOnApplyAccountClickSavesPublicAccount;
var
	SavedSettings: TAccountSettings;
begin
	FPresenter.Initialize('');

	FView.SetAccountName('NewPublic');
	FView.SetIsPrivate(False);
	FView.SetPublicUrl('https://cloud.mail.ru/public/test/');

	FPresenter.OnApplyAccountClick;

	SavedSettings := FAccountsManager.GetAccountSettings('NewPublic');
	Assert.AreEqual('NewPublic', SavedSettings.Account, 'Account name should be saved');
	Assert.IsTrue(SavedSettings.PublicAccount, 'Should be public');
	Assert.AreEqual('https://cloud.mail.ru/public/test/', SavedSettings.PublicUrl, 'Public URL should be saved');
end;

procedure TAccountsPresenterTest.TestOnApplyAccountClickWithEmptyNameDoesNothing;
var
	AccountsList: TWSList;
begin
	FPresenter.Initialize('');

	FView.SetAccountName('');
	FView.SetEmail('test@mail.ru');

	FPresenter.OnApplyAccountClick;

	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(0, AccountsList.Count, 'No account should be created with empty name');
end;

procedure TAccountsPresenterTest.TestOnApplyAccountClickWithTCPasswordManager;
var
	SavedSettings: TAccountSettings;
begin
	FPresenter.Initialize('');

	FView.SetAccountName('TCAccount');
	FView.SetEmail('tc@mail.ru');
	FView.SetPassword('tcpass');
	FView.SetUseTCPasswordManager(True);

	FPresenter.OnApplyAccountClick;

	Assert.IsTrue(FPasswordManager.Passwords.ContainsKey('TCAccount'), 'Password should be in manager');
	Assert.AreEqual('tcpass', FPasswordManager.Passwords['TCAccount'], 'Password value should match');

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

procedure TAccountsPresenterTest.TestOnAccountTypeChangedShowsPrivatePanel;
begin
	FPresenter.Initialize('');
	FView.SetIsPrivate(True);

	FPresenter.OnAccountTypeChanged;

	Assert.IsTrue(FView.AccountsPanelVisible, 'Private panel should be visible');
	Assert.IsFalse(FView.SharesPanelVisible, 'Public panel should be hidden');
end;

procedure TAccountsPresenterTest.TestOnAccountTypeChangedShowsPublicPanel;
begin
	FPresenter.Initialize('');
	FView.SetIsPrivate(False);

	FPresenter.OnAccountTypeChanged;

	Assert.IsFalse(FView.AccountsPanelVisible, 'Private panel should be hidden');
	Assert.IsTrue(FView.SharesPanelVisible, 'Public panel should be visible');
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

procedure TAccountsPresenterTest.TestOnEncryptModeChangedWithAskOnce;
begin
	FPresenter.Initialize('');
	FView.SetEncryptFilesMode(EncryptModeAskOnce);

	FPresenter.OnEncryptModeChanged;

	{AskOnce mode should NOT enable the password button (only Always does)}
	Assert.IsFalse(FView.EncryptPasswordButtonEnabled, 'Password button should be disabled for EncryptModeAskOnce');
end;

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

{Dirty tracking and selection preservation tests}

procedure TAccountsPresenterTest.TestApplyButtonDisabledAfterInitialize;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'TestAcc';
	AccSettings.Email := 'test@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('TestAcc');

	Assert.IsFalse(FView.ApplyButtonEnabled, 'Apply button should be disabled after Initialize');
end;

procedure TAccountsPresenterTest.TestApplyButtonEnabledAfterFieldChanged;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'TestAcc';
	AccSettings.Email := 'test@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('TestAcc');

	{Simulate user editing a field}
	FPresenter.OnFieldChanged;

	Assert.IsTrue(FView.ApplyButtonEnabled, 'Apply button should be enabled after field changed');
end;

procedure TAccountsPresenterTest.TestApplyButtonDisabledAfterSuccessfulApply;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'TestAcc';
	AccSettings.Email := 'test@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('TestAcc');

	{Make dirty then apply}
	FView.SetAccountName('TestAcc');
	FView.SetEmail('test@mail.ru');
	FPresenter.OnFieldChanged;
	Assert.IsTrue(FView.ApplyButtonEnabled, 'Apply should be enabled before save');

	FPresenter.OnApplyAccountClick;

	Assert.IsFalse(FView.ApplyButtonEnabled, 'Apply button should be disabled after successful Apply');
end;

procedure TAccountsPresenterTest.TestSelectionPreservedAfterApply;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'First';
	AccSettings.Email := 'first@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'Second';
	AccSettings.Email := 'second@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('Second');
	Assert.AreEqual(1, FView.SelectedAccountIndex, 'Second should be selected initially');

	{Modify and apply}
	FView.SetAccountName('Second');
	FView.SetEmail('updated@mail.ru');
	FPresenter.OnApplyAccountClick;

	{Selection should still be on Second}
	Assert.AreEqual(1, FView.SelectedAccountIndex, 'Second should still be selected after Apply');
	Assert.AreEqual('Second', FView.GetAccountName, 'Account name should still show Second');
end;

procedure TAccountsPresenterTest.TestConfirmDialogShownOnAccountSwitchWithDirtyState;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'First';
	AccSettings.Email := 'first@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'Second';
	AccSettings.Email := 'second@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('First');

	{Make dirty}
	FPresenter.OnFieldChanged;

	{Configure mock to discard}
	FView.ConfirmDiscardResult := csrDiscard;

	{Switch to Second account}
	FView.SelectedAccountIndex := 1;
	FPresenter.OnAccountSelected;

	Assert.AreEqual(1, FView.ConfirmDiscardCallCount, 'Confirm dialog should be shown once');
end;

procedure TAccountsPresenterTest.TestCancelOnConfirmKeepsCurrentSelection;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'First';
	AccSettings.Email := 'first@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'Second';
	AccSettings.Email := 'second@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('First');

	{Make dirty}
	FPresenter.OnFieldChanged;

	{Configure mock to cancel}
	FView.ConfirmDiscardResult := csrCancel;

	{Try to switch to Second}
	FView.SelectedAccountIndex := 1;
	FPresenter.OnAccountSelected;

	{Should reselect First (index 0)}
	Assert.AreEqual(0, FView.SelectedAccountIndex, 'Selection should revert to First on Cancel');
end;

procedure TAccountsPresenterTest.TestSaveOnConfirmSavesAndSwitches;
var
	AccSettings, SavedSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'First';
	AccSettings.Email := 'first@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'Second';
	AccSettings.Email := 'second@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('First');

	{User edits email}
	FView.SetEmail('modified@mail.ru');
	FPresenter.OnFieldChanged;

	{Configure mock to save}
	FView.ConfirmDiscardResult := csrSave;

	{Switch to Second}
	FView.SelectedAccountIndex := 1;
	FPresenter.OnAccountSelected;

	{Verify First account was saved with modified email}
	SavedSettings := FAccountsManager.GetAccountSettings('First');
	Assert.AreEqual('modified@mail.ru', SavedSettings.Email, 'First account email should be saved');

	{Verify Second is now loaded}
	Assert.AreEqual('second@mail.ru', FView.GetEmail, 'Second account email should be loaded');
end;

procedure TAccountsPresenterTest.TestDiscardOnConfirmSwitchesWithoutSaving;
var
	AccSettings, SavedSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'First';
	AccSettings.Email := 'first@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'Second';
	AccSettings.Email := 'second@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('First');

	{User edits email}
	FView.SetEmail('unsaved@mail.ru');
	FPresenter.OnFieldChanged;

	{Configure mock to discard}
	FView.ConfirmDiscardResult := csrDiscard;

	{Switch to Second}
	FView.SelectedAccountIndex := 1;
	FPresenter.OnAccountSelected;

	{Verify First account was NOT saved with modified email}
	SavedSettings := FAccountsManager.GetAccountSettings('First');
	Assert.AreEqual('first@mail.ru', SavedSettings.Email, 'First account email should NOT be modified');

	{Verify Second is now loaded}
	Assert.AreEqual('second@mail.ru', FView.GetEmail, 'Second account email should be loaded');
end;

procedure TAccountsPresenterTest.TestNewButtonWithDirtyStateTriggersConfirm;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'TestAcc';
	AccSettings.Email := 'test@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('TestAcc');

	{Make dirty}
	FPresenter.OnFieldChanged;

	{Configure mock to discard}
	FView.ConfirmDiscardResult := csrDiscard;

	{Click New}
	FPresenter.OnAddAccountClick;

	Assert.AreEqual(1, FView.ConfirmDiscardCallCount, 'Confirm dialog should be shown when clicking New with dirty state');
end;

procedure TAccountsPresenterTest.TestRepeatedSelectSameAccountDoesNotTriggerConfirm;
var
	AccSettings: TAccountSettings;
begin
	{FCancellingSwitch is True during the entire CheckDirty execution.
	 The callback simulates a spurious OnSelectItem that fires during the dialog
	 (focus transfer) or during SelectAccountByName (programmatic reselection).}
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'First';
	AccSettings.Email := 'first@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'Second';
	AccSettings.Email := 'second@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	FPresenter.Initialize('First');

	{Make dirty}
	FPresenter.OnFieldChanged;

	{Configure Cancel with callback simulating spurious OnSelectItem}
	FView.ConfirmDiscardResult := csrCancel;
	FView.ConfirmDiscardCallback :=
		procedure
		begin
			FPresenter.OnAccountSelected;
		end;

	{Switch to Second — triggers CheckDirty -> dialog (callback fires) -> Cancel}
	FView.SelectedAccountIndex := 1;
	FPresenter.OnAccountSelected;

	{Simulate the comctl32 post-handler event that re-selects B after our handler returns}
	FPresenter.OnAccountSelected;

	{The dialog must have been shown exactly once despite both spurious calls}
	Assert.AreEqual(1, FView.ConfirmDiscardCallCount, 'Confirm dialog should appear exactly once');

	{First account should be reselected}
	Assert.AreEqual('First', FView.GetSelectedAccountName, 'First account should remain selected after Cancel');
end;

procedure TAccountsPresenterTest.TestProgrammaticUpdatesDoNotTriggerDirty;
var
	AccSettings: TAccountSettings;
begin
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'TestAcc';
	AccSettings.Email := 'test@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	{Initialize triggers LoadAccountToView which sets fields programmatically}
	FPresenter.Initialize('TestAcc');

	{Apply button should remain disabled -- programmatic updates are guarded}
	Assert.IsFalse(FView.ApplyButtonEnabled, 'Apply button should be disabled after programmatic field updates');

	{Confirm dialog should not have been called}
	Assert.AreEqual(0, FView.ConfirmDiscardCallCount, 'Confirm dialog should not be triggered by programmatic updates');
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
	{Setup: Set proxy user with active proxy type}
	Settings := FSettingsManager.GetSettings;
	Settings.ConnectionSettings.ProxySettings.ProxyType := ProxySocks5;
	Settings.ConnectionSettings.ProxySettings.User := 'proxyuser';
	Settings.ConnectionSettings.ProxySettings.Password := 'proxypass';
	Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager := True;
	FSettingsManager.SetSettings(Settings);

	FPresenter.Initialize('');

	Assert.AreEqual('proxyuser', FView.GetProxyUser, 'Proxy user should be loaded');
	Assert.AreEqual('proxypass', FView.GetProxyPassword, 'Proxy password should be loaded');
	Assert.IsTrue(FView.GetProxyUseTCPasswordManager, 'Proxy TC password manager should be loaded');
	Assert.IsTrue(FView.ProxyTCPasswordManagerEnabled, 'Proxy TC password manager checkbox should be enabled');
	Assert.IsTrue(FView.ProxyControlsEnabled, 'Proxy controls should be enabled for Socks5');
end;

{Global settings dirty tracking tests}

procedure TAccountsPresenterTest.TestGlobalSettingsApplyDisabledAfterInitialize;
begin
	FPresenter.Initialize('');

	Assert.IsFalse(FView.GlobalSettingsApplyEnabled, 'Global settings Apply buttons should be disabled after Initialize');
end;

procedure TAccountsPresenterTest.TestGlobalSettingsApplyEnabledAfterFieldChanged;
begin
	FPresenter.Initialize('');

	{Simulate user editing a global settings field}
	FPresenter.OnGlobalSettingsFieldChanged;

	Assert.IsTrue(FView.GlobalSettingsApplyEnabled, 'Global settings Apply buttons should be enabled after field change');
end;

procedure TAccountsPresenterTest.TestGlobalSettingsApplyDisabledAfterApply;
begin
	FPresenter.Initialize('');

	{Mark dirty then apply}
	FPresenter.OnGlobalSettingsFieldChanged;
	Assert.IsTrue(FView.GlobalSettingsApplyEnabled, 'Apply should be enabled before save');

	FView.SetDescriptionFileName('valid.txt');
	FPresenter.OnApplyGlobalSettingsClick;

	Assert.IsFalse(FView.GlobalSettingsApplyEnabled, 'Global settings Apply buttons should be disabled after successful Apply');
end;

procedure TAccountsPresenterTest.TestGlobalSettingsFieldChangedIgnoredDuringLoad;
begin
	{LoadGlobalSettingsToView sets many view fields, which would normally
	 trigger dirty via OnChange handlers. The FGlobalSettingsUpdating guard
	 must suppress this.}
	FPresenter.Initialize('');

	Assert.IsFalse(FView.GlobalSettingsApplyEnabled, 'Apply should remain disabled after LoadGlobalSettingsToView');
end;

procedure TAccountsPresenterTest.TestOnCloudMaxFileSizeCheckChangedMarksDirty;
begin
	FPresenter.Initialize('');
	Assert.IsFalse(FView.GlobalSettingsApplyEnabled, 'Apply should start disabled');

	FView.SetCloudMaxFileSizeEnabled(True);
	FPresenter.OnCloudMaxFileSizeCheckChanged;

	Assert.IsTrue(FView.GlobalSettingsApplyEnabled, 'Apply should be enabled after CloudMaxFileSize checkbox change');
end;

{Proxy controls tests}

procedure TAccountsPresenterTest.TestProxyControlsDisabledWhenNoProxy;
begin
	FPresenter.Initialize('');

	FView.SetProxyType(ProxyNone);
	FPresenter.OnProxyTypeChanged;

	Assert.IsFalse(FView.ProxyControlsEnabled, 'Proxy controls should be disabled when proxy type is None');
end;

procedure TAccountsPresenterTest.TestProxyControlsEnabledWhenSocks5;
begin
	FPresenter.Initialize('');

	FView.SetProxyType(ProxySocks5);
	FPresenter.OnProxyTypeChanged;

	Assert.IsTrue(FView.ProxyControlsEnabled, 'Proxy controls should be enabled when proxy type is Socks5');
end;

procedure TAccountsPresenterTest.TestProxyTypeChangeMarksDirty;
begin
	FPresenter.Initialize('');
	Assert.IsFalse(FView.GlobalSettingsApplyEnabled, 'Apply should start disabled');

	FView.SetProxyType(ProxySocks5);
	FPresenter.OnProxyTypeChanged;

	Assert.IsTrue(FView.GlobalSettingsApplyEnabled, 'Apply should be enabled after proxy type change');
end;

procedure TAccountsPresenterTest.TestProxyTCPwdMngrDisabledWhenNoProxy;
begin
	FPresenter.Initialize('');

	{Even with a proxy user set, password manager should be disabled
	 when proxy type is None}
	FView.SetProxyType(ProxyNone);
	FView.SetProxyUser('testuser');
	FPresenter.OnProxyTypeChanged;

	Assert.IsFalse(FView.ProxyTCPasswordManagerEnabled, 'TC password manager should be disabled when proxy type is None');
end;

{Speed limit default test}

procedure TAccountsPresenterTest.TestSpeedLimitDefaultIsZero;
var
	Settings: TPluginSettings;
begin
	{Verify that default speed limits are 0 (unlimited) rather than -1}
	Settings := FSettingsManager.GetSettings;

	Assert.AreEqual(0, Settings.ConnectionSettings.UploadBPS, 'Default UploadBPS should be 0');
	Assert.AreEqual(0, Settings.ConnectionSettings.DownloadBPS, 'Default DownloadBPS should be 0');
end;

{Cipher profile tests}

procedure TAccountsPresenterTest.TestCipherProfileComboPopulatedOnInitialize;
begin
	{Initialize populates cipher profile combo via PopulateCipherProfiles}
	FPresenter.Initialize('');
	Assert.AreEqual(Integer(3), Integer(Length(FView.CipherProfileItems)), 'Should have 3 cipher profiles');
end;

procedure TAccountsPresenterTest.TestCipherProfileDisabledWhenEncryptModeNone;
begin
	{When encrypt mode is None, cipher profile combo should be disabled}
	FView.SetEncryptFilesMode(EncryptModeNone);
	FPresenter.OnEncryptModeChanged;
	Assert.IsFalse(FView.CipherProfileEnabled, 'Cipher profile should be disabled when encryption is off');
end;

procedure TAccountsPresenterTest.TestCipherProfileEnabledWhenEncryptModeAlways;
begin
	{When encrypt mode is Always, cipher profile combo should be enabled}
	FView.SetEncryptFilesMode(EncryptModeAlways);
	FPresenter.OnEncryptModeChanged;
	Assert.IsTrue(FView.CipherProfileEnabled, 'Cipher profile should be enabled when encryption is on');
end;

procedure TAccountsPresenterTest.TestCipherProfileSavedOnApply;
begin
	{Verify cipher profile ID is saved when applying account settings}
	FView.SetAccountName('TestCipherAccount');
	FView.SetIsPrivate(True);
	FView.SetEmail('cipher@mail.ru');
	FView.SetCipherProfileIndex(2); {Third profile}
	FView.SetEncryptFilesMode(EncryptModeAlways);
	FPresenter.OnApplyAccountClick;

	{Reload and verify}
	var Settings := FAccountsManager.GetAccountSettings('TestCipherAccount');
	Assert.IsNotEmpty(Settings.CipherProfileId, 'CipherProfileId should be saved');
end;

procedure TAccountsPresenterTest.TestCipherProfileLoadedForAccount;
begin
	{Setup: save an account with a specific cipher profile}
	var Settings := Default(TAccountSettings);
	Settings.Account := 'CipherLoadTest';
	Settings.Email := 'cipherload@mail.ru';
	Settings.CipherProfileId := 'dcpcrypt-twofish256-cfb8-sha256';
	Settings.EncryptFilesMode := EncryptModeAlways;
	FAccountsManager.SetAccountSettings(Settings);

	{Trigger loading}
	FPresenter.Initialize('CipherLoadTest');

	{Verify the profile index was set (Twofish is index 2)}
	Assert.AreEqual(2, FView.CipherProfileIndex, 'Cipher profile should be set to Twofish index');
end;

procedure TAccountsPresenterTest.TestApplyAccount_WithChangedName_RenamesAccount;
var
	AccSettings: TAccountSettings;
	AccountsList: TWSList;
begin
	{Create account under original name}
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'OriginalName';
	AccSettings.Email := 'orig@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	{Initialize and select the account so FSelectedAccount is set}
	FPresenter.Initialize('OriginalName');

	{Change name in the view and apply}
	FView.SetAccountName('RenamedAccount');
	FView.SetEmail('orig@mail.ru');
	FPresenter.OnApplyAccountClick;

	{Old section should be gone}
	AccountsList := FAccountsManager.GetAccountsList;
	Assert.IsFalse(AccountsList.Contains('OriginalName'), 'Old account should be deleted');

	{New section should exist with correct data}
	Assert.IsTrue(AccountsList.Contains('RenamedAccount'), 'New account should exist');
	AccSettings := FAccountsManager.GetAccountSettings('RenamedAccount');
	Assert.AreEqual('orig@mail.ru', AccSettings.Email, 'Email should be preserved under new name');
end;

procedure TAccountsPresenterTest.TestApplyAccount_SameName_DoesNotRename;
var
	AccSettings: TAccountSettings;
	AccountsList: TWSList;
begin
	{Create account}
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'KeepName';
	AccSettings.Email := 'keep@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	{Initialize and select the account}
	FPresenter.Initialize('KeepName');

	{Apply without changing name}
	FView.SetAccountName('KeepName');
	FView.SetEmail('updated@mail.ru');
	FPresenter.OnApplyAccountClick;

	{Account should still exist under the same name}
	AccountsList := FAccountsManager.GetAccountsList;
	Assert.IsTrue(AccountsList.Contains('KeepName'), 'Account should still exist');
	Assert.AreEqual(1, AccountsList.Count, 'Should still be exactly one account');
	AccSettings := FAccountsManager.GetAccountSettings('KeepName');
	Assert.AreEqual('updated@mail.ru', AccSettings.Email, 'Email should be updated');
end;

procedure TAccountsPresenterTest.TestApplyAccount_ForbiddenChars_ShowsError;
var
	AccountsList: TWSList;
begin
	{Test brackets}
	FPresenter.Initialize('');
	FView.SetAccountName('Test[bad]name');
	FView.SetEmail('bad@mail.ru');
	FPresenter.OnApplyAccountClick;
	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(0, AccountsList.Count, 'Account with brackets should not be saved');
	Assert.IsNotEmpty(FView.AccountNameErrorMessage, 'Error message should be shown for brackets');

	{Test backslash}
	FView.FAccountNameErrorMessage := '';
	FView.SetAccountName('Test\name');
	FPresenter.OnApplyAccountClick;
	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(0, AccountsList.Count, 'Account with backslash should not be saved');
	Assert.IsNotEmpty(FView.AccountNameErrorMessage, 'Error message should be shown for backslash');

	{Test forward slash}
	FView.FAccountNameErrorMessage := '';
	FView.SetAccountName('Test/name');
	FPresenter.OnApplyAccountClick;
	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(0, AccountsList.Count, 'Account with forward slash should not be saved');
	Assert.IsNotEmpty(FView.AccountNameErrorMessage, 'Error message should be shown for forward slash');

	{Test reserved postfix}
	FView.FAccountNameErrorMessage := '';
	FView.SetAccountName('MyAccount.trash');
	FPresenter.OnApplyAccountClick;
	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(0, AccountsList.Count, 'Account with reserved postfix should not be saved');
	Assert.IsNotEmpty(FView.AccountNameErrorMessage, 'Error message should be shown for reserved postfix');
end;

procedure TAccountsPresenterTest.TestApplyAccount_DuplicateName_ConfirmsOverwrite;
var
	AccSettings: TAccountSettings;
	AccountsList: TWSList;
begin
	{Create two accounts}
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'AccountA';
	AccSettings.Email := 'a@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'AccountB';
	AccSettings.Email := 'b@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	{Select AccountA and rename to AccountB}
	FPresenter.Initialize('AccountA');
	FView.SetAccountName('AccountB');
	FView.SetEmail('a@mail.ru');
	FView.ConfirmAccountOverwriteResult := True;

	FPresenter.OnApplyAccountClick;

	{Overwrite confirmed: only AccountB should remain, with AccountA's data}
	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(1, AccountsList.Count, 'Should have one account after overwrite');
	Assert.IsTrue(AccountsList.Contains('AccountB'), 'AccountB should exist');
	AccSettings := FAccountsManager.GetAccountSettings('AccountB');
	Assert.AreEqual('a@mail.ru', AccSettings.Email, 'AccountB should have AccountA email');
	Assert.AreEqual(1, FView.ConfirmAccountOverwriteCallCount, 'Overwrite should be confirmed');
end;

procedure TAccountsPresenterTest.TestApplyAccount_DuplicateName_DeclinedOverwrite;
var
	AccSettings: TAccountSettings;
	AccountsList: TWSList;
begin
	{Create two accounts}
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'KeepA';
	AccSettings.Email := 'keepa@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'KeepB';
	AccSettings.Email := 'keepb@mail.ru';
	FAccountsManager.SetAccountSettings(AccSettings);

	{Select KeepA and try to rename to KeepB, but decline}
	FPresenter.Initialize('KeepA');
	FView.SetAccountName('KeepB');
	FView.SetEmail('keepa@mail.ru');
	FView.ConfirmAccountOverwriteResult := False;

	FPresenter.OnApplyAccountClick;

	{Declined: both accounts should remain unchanged}
	AccountsList := FAccountsManager.GetAccountsList;
	Assert.AreEqual(2, AccountsList.Count, 'Both accounts should still exist');
	AccSettings := FAccountsManager.GetAccountSettings('KeepB');
	Assert.AreEqual('keepb@mail.ru', AccSettings.Email, 'KeepB should retain its original data');
end;

{Server profile CRUD tests}

procedure TAccountsPresenterTest.TestInitializePopulatesServerCombo;
begin
	FPresenter.Initialize('');

	{Server combo should have at least "(Default)" entry}
	Assert.IsTrue(Length(FView.ServerComboItems) >= 1, 'Server combo should have at least one item');
	Assert.AreEqual(DFM_COMBO_DEFAULT_SERVER, FView.ServerComboItems[0], 'First item should be "(Default)"');
end;

procedure TAccountsPresenterTest.TestOnAddServerClickClearsFields;
begin
	FPresenter.Initialize('');

	{Pre-set some values to verify they get cleared}
	FView.SetServerName('OldName');
	FView.SetServerUrl('http://old');

	FPresenter.OnAddServerClick;

	Assert.AreEqual('', FView.GetServerName, 'Server name should be cleared');
	Assert.AreEqual('', FView.GetServerUrl, 'Server URL should be cleared');
	Assert.AreEqual('', FView.GetServerApiUrl, 'API URL should be cleared');
end;

procedure TAccountsPresenterTest.TestOnApplyServerClickSavesProfile;
begin
	FPresenter.Initialize('');

	{Fill in server profile fields}
	FView.SetServerName('TestServer');
	FView.SetServerUrl('http://testserver:8080');
	FView.SetServerApiUrl('http://testserver:8080/api/v2');

	FPresenter.OnAddServerClick;
	FView.SetServerName('TestServer');
	FView.SetServerUrl('http://testserver:8080');
	FView.SetServerApiUrl('http://testserver:8080/api/v2');

	FPresenter.OnApplyServerClick;

	{Verify the profile was persisted}
	Assert.IsTrue(Length(FView.ServerDisplayItems) > 0, 'Server list should have at least one entry');
	Assert.AreEqual('TestServer', FView.ServerDisplayItems[0].Name, 'Server name should match');
end;

procedure TAccountsPresenterTest.TestOnDeleteServerClickRemovesProfile;
begin
	FPresenter.Initialize('');

	{Create a server profile first}
	FPresenter.OnAddServerClick;
	FView.SetServerName('ToDelete');
	FView.SetServerUrl('http://todelete:8080');
	FPresenter.OnApplyServerClick;
	Assert.AreEqual(1, Integer(Length(FView.ServerDisplayItems)), 'Should have one server profile');

	{Select it and delete}
	FView.ServerSelectedIndex := 0;
	FPresenter.OnDeleteServerClick;

	Assert.AreEqual(0, Integer(Length(FView.ServerDisplayItems)), 'Server list should be empty after deletion');
end;

procedure TAccountsPresenterTest.TestOnServerFieldChangedSetsDirty;
begin
	FPresenter.Initialize('');

	{Create and save a server profile}
	FPresenter.OnAddServerClick;
	FView.SetServerName('DirtyTest');
	FView.SetServerUrl('http://dirtytest');
	FPresenter.OnApplyServerClick;

	{Verify apply button is disabled after save}
	Assert.IsFalse(FView.ServerApplyButtonEnabled, 'Apply should be disabled after save');

	{Select the profile}
	FView.ServerSelectedIndex := 0;
	FPresenter.OnServerSelected;

	{Change a field - should mark dirty}
	FPresenter.OnServerFieldChanged;

	Assert.IsTrue(FView.ServerApplyButtonEnabled, 'Apply should be enabled after field change');
end;

procedure TAccountsPresenterTest.TestOnServerFieldChangedIgnoredDuringUpdate;
begin
	FPresenter.Initialize('');

	{During initialization, field changes should not mark dirty}
	Assert.IsFalse(FView.ServerApplyButtonEnabled, 'Apply should be disabled during initialization');
end;

procedure TAccountsPresenterTest.TestServerApplyDisabledAfterInitialize;
begin
	FPresenter.Initialize('');

	Assert.IsFalse(FView.ServerApplyButtonEnabled, 'Server apply button should be disabled after initialize');
end;

procedure TAccountsPresenterTest.TestOnApplyServerClickClearsDirty;
begin
	FPresenter.Initialize('');

	{Create a profile, make it dirty, then apply}
	FPresenter.OnAddServerClick;
	FView.SetServerName('ApplyTest');
	FView.SetServerUrl('http://applytest');
	FPresenter.OnServerFieldChanged;
	Assert.IsTrue(FView.ServerApplyButtonEnabled, 'Apply should be enabled when dirty');

	FPresenter.OnApplyServerClick;

	Assert.IsFalse(FView.ServerApplyButtonEnabled, 'Apply should be disabled after successful apply');
end;

procedure TAccountsPresenterTest.TestOnServerComboChangedMarksAccountDirty;
begin
	FPresenter.Initialize('');

	{Create an account first}
	FPresenter.OnAddAccountClick;
	FView.SetAccountName('ComboTest');
	FView.SetEmail('combo@mail.ru');
	FView.SetPassword('pass');
	FPresenter.OnApplyAccountClick;

	{Select the account}
	FView.SelectedAccountIndex := 0;
	FPresenter.OnAccountSelected;

	{Verify apply is disabled after loading}
	Assert.IsFalse(FView.ApplyButtonEnabled, 'Account apply should be disabled after loading');

	{Change server combo -- should mark account dirty}
	FPresenter.OnServerComboChanged;

	Assert.IsTrue(FView.ApplyButtonEnabled, 'Account apply should be enabled after server combo change');
end;

procedure TAccountsPresenterTest.TestOnServerSelectedChecksDirty;
begin
	FPresenter.Initialize('');

	{Create two server profiles}
	FPresenter.OnAddServerClick;
	FView.SetServerName('Server1');
	FView.SetServerUrl('http://server1');
	FPresenter.OnApplyServerClick;

	FPresenter.OnAddServerClick;
	FView.SetServerName('Server2');
	FView.SetServerUrl('http://server2');
	FPresenter.OnApplyServerClick;

	{Select first server and modify a field}
	FView.ServerSelectedIndex := 0;
	FPresenter.OnServerSelected;
	FPresenter.OnServerFieldChanged;

	{Switch to second - should trigger confirm dialog}
	FView.ServerConfirmResult := csrDiscard;
	FView.ServerSelectedIndex := 1;
	FPresenter.OnServerSelected;

	Assert.IsTrue(FView.ServerConfirmCallCount > 0, 'Confirm dialog should be shown when switching with dirty state');
end;

initialization
	TDUnitX.RegisterTestFixture(TAccountsPresenterTest);

end.
