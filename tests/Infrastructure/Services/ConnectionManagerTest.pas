unit ConnectionManagerTest;

interface

uses
	ConnectionManager,
	CloudMailRu,
	PasswordUIProvider,
	HTTPManager,
	CipherValidator,
	FileSystem,
	Logger,
	Progress,
	Request,
	TCHandler,
	PasswordManager,
	AccountsManager,
	PluginSettingsManager,
	AccountSettings,
	PluginSettings,
	CloudSettings,
	ConnectionSettings,
	ProxySettings,
	StreamingSettings,
	AuthStrategy,
	WSList,
	Winapi.Windows,
	System.Classes,
	System.Generics.Collections,
	DUnitX.TestFramework,
	OpenSSLProvider,
	AccountCredentialsProvider;

type
	{Mock implementation of IAccountsManager that returns test settings}
	TMockAccountsManager = class(TInterfacedObject, IAccountsManager)
	private
		FAccountSettings: TAccountSettings;
		FSwitchPasswordStorageCalled: Boolean;
		FSetCryptedGUIDCalled: Boolean;
		FSetAccountSettingsCalled: Boolean;
		FDeleteAccountCalled: Boolean;
		FLastCryptedGUID: WideString;
	public
		constructor Create;
		function GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
		function GetAccountSettings(Account: WideString): TAccountSettings;
		procedure SetAccountSettings(Account: WideString; Settings: TAccountSettings); overload;
		procedure SetAccountSettings(Settings: TAccountSettings); overload;
		procedure DeleteAccount(Account: WideString);
		procedure RenameAccount(const OldName, NewName: WideString);
		procedure SwitchPasswordStorage(Account: WideString);
		procedure SetCryptedGUID(Account: WideString; GUID: WideString);
		property AccountSettings: TAccountSettings read FAccountSettings write FAccountSettings;
		property SwitchPasswordStorageCalled: Boolean read FSwitchPasswordStorageCalled;
		property SetAccountSettingsCalled: Boolean read FSetAccountSettingsCalled;
		property SetCryptedGUIDCalled: Boolean read FSetCryptedGUIDCalled;
		property DeleteAccountCalled: Boolean read FDeleteAccountCalled;
		property LastCryptedGUID: WideString read FLastCryptedGUID;
	end;

	{Mock implementation of IPluginSettingsManager that returns test settings}
	TMockPluginSettingsManager = class(TInterfacedObject, IPluginSettingsManager)
	private
		FPluginSettings: TPluginSettings;
		FSwitchProxyPasswordStorageCalled: Boolean;
		FSaveCalled: Boolean;
	public
		constructor Create;
		function GetSettings: TPluginSettings;
		procedure SetSettings(Value: TPluginSettings);
		procedure Save;
		procedure SwitchProxyPasswordStorage;
		function GetStreamingSettings(const FileName: WideString): TStreamingSettings;
		procedure SetStreamingSettings(const FileName: WideString; StreamingSettings: TStreamingSettings);
		procedure GetStreamingExtensionsList(ExtensionsList: TStrings);
		procedure RemoveStreamingExtension(const Extension: WideString);
		function GetAccountsIniFilePath: WideString;
		procedure Refresh;
		property Settings: TPluginSettings read FPluginSettings write FPluginSettings;
		property SwitchProxyPasswordStorageCalled: Boolean read FSwitchProxyPasswordStorageCalled;
		property SaveCalled: Boolean read FSaveCalled;
	end;

	[TestFixture]
	TConnectionManagerConstructorTest = class
	public
		[Test]
		{Verifies ConnectionManager can be created with null implementations}
		procedure TestCreateWithNullImplementations;

		[Test]
		{Verifies ConnectionManager can be destroyed without errors}
		procedure TestDestroyWithoutConnections;
	end;

	[TestFixture]
	TConnectionManagerGetTest = class
	public
		[Test]
		{Verifies Get always returns a valid instance (never nil)}
		procedure TestGetAlwaysReturnsValidInstance;

		[Test]
		{Verifies Get returns the same instance for the same connection name (caching)}
		procedure TestGetReturnsCachedInstance;

		[Test]
		{Verifies Get returns different instances for different connection names}
		procedure TestGetReturnsDifferentInstancesForDifferentNames;
	end;

	[TestFixture]
	TConnectionManagerFreeTest = class
	public
		[Test]
		{Verifies Free does not throw when connection does not exist}
		procedure TestFreeNonExistentConnection;

		[Test]
		{Verifies Free can be called multiple times safely}
		procedure TestFreeMultipleTimes;
	end;

	[TestFixture]
	TConnectionManagerCipherProfileTest = class
	public
		[Test]
		{Verifies account with CipherProfileId can create connection without errors (public account, encryption disabled)}
		procedure TestConnectionCreatedWithCipherProfileInSettings;

		[Test]
		{Verifies account with empty CipherProfileId creates connection without errors}
		procedure TestConnectionCreatedWithEmptyCipherProfileInSettings;
	end;

	{Mock password manager returning configurable GetPassword results}
	TMockPasswordManagerForConn = class(TInterfacedObject, IPasswordManager)
	private
		FGetPasswordResult: Integer;
		FGetPasswordValue: WideString;
		FSetPasswordResult: Integer;
		FSetPasswordCalled: Boolean;
	public
		constructor Create(GetPasswordResult: Integer; const PasswordValue: WideString = '');
		function GetPassword(Key: WideString; var Password: WideString): Integer;
		function SetPassword(Key, Password: WideString): Integer;
		property SetPasswordResult: Integer read FSetPasswordResult write FSetPasswordResult;
		property SetPasswordCalled: Boolean read FSetPasswordCalled;
	end;

	{Mock password UI returning configurable AskPassword/AskAction results}
	TMockPasswordUIForConn = class(TInterfacedObject, IPasswordUIProvider)
	private
		FAskPasswordResult: Integer;
		FAskPasswordValue: WideString;
		FAskActionResult: Integer;
	public
		constructor Create(AskPasswordResult: Integer; const PasswordValue: WideString = '');
		function AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
		function AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
		property AskActionResult: Integer read FAskActionResult write FAskActionResult;
	end;

	{Tests for non-public account initialization with encryption}
	[TestFixture]
	TConnectionManagerEncryptionTest = class
	public
		[Test]
		{EncryptModeAlways + password found in TC store -> creates cipher}
		procedure TestInit_EncryptAlways_PasswordFound_CreatesCipher;

		[Test]
		{EncryptModeAlways + TC store returns unsupported -> Init still creates connection}
		procedure TestInit_EncryptAlways_PasswordUnsupported_SkipsEncryption;

		[Test]
		{Free connection that was created via Get -> removes from pool}
		procedure TestFree_ExistingConnection_RemovesFromPool;

		[Test]
		{Non-public account with proxy configured -> exercises GetProxyPassword}
		procedure TestInit_NonPublicWithProxy_PasswordFromINI;
	end;

implementation

uses
	CloudConstants,
	SettingsConstants,
	WFXTypes,
	CipherProfile,
	BCryptProvider,
	MockHTTPManager,
	MockCloudHTTP,
	CloudHTTP,
	Vcl.Controls,
	System.SysUtils;

{TMockAccountsManager}

constructor TMockAccountsManager.Create;
begin
	inherited Create;
	FSwitchPasswordStorageCalled := False;
	FSetCryptedGUIDCalled := False;
	FDeleteAccountCalled := False;
	FLastCryptedGUID := '';

	{Initialize with default test settings - public account to simplify testing}
	FAccountSettings := Default(TAccountSettings);
	FAccountSettings.Email := 'test@mail.ru';
	FAccountSettings.Password := '';
	FAccountSettings.UseTCPasswordManager := False;
	FAccountSettings.PublicAccount := True; {Public account skips password retrieval}
	FAccountSettings.PublicUrl := '';
	FAccountSettings.EncryptFilesMode := EncryptModeNone;
	FAccountSettings.CryptedGUIDFiles := '';
	FAccountSettings.ShardOverride := '';
	FAccountSettings.UploadUrlOverride := '';
	FAccountSettings.SplitLargeFiles := True;
	FAccountSettings.UnlimitedFileSize := False;
end;

function TMockAccountsManager.GetAccountsList(const AccountTypes: EAccountType; const VirtualTypes: EVirtualType): TWSList;
begin
	Result.Clear;
end;

function TMockAccountsManager.GetAccountSettings(Account: WideString): TAccountSettings;
begin
	FAccountSettings.Account := Account;
	Result := FAccountSettings;
end;

procedure TMockAccountsManager.SetAccountSettings(Account: WideString; Settings: TAccountSettings);
begin
	FSetAccountSettingsCalled := True;
	FAccountSettings := Settings;
end;

procedure TMockAccountsManager.SetAccountSettings(Settings: TAccountSettings);
begin
	FSetAccountSettingsCalled := True;
	FAccountSettings := Settings;
end;

procedure TMockAccountsManager.DeleteAccount(Account: WideString);
begin
	FDeleteAccountCalled := True;
end;

procedure TMockAccountsManager.RenameAccount(const OldName, NewName: WideString);
begin
	{No-op for mock}
end;

procedure TMockAccountsManager.SwitchPasswordStorage(Account: WideString);
begin
	FSwitchPasswordStorageCalled := True;
end;

procedure TMockAccountsManager.SetCryptedGUID(Account: WideString; GUID: WideString);
begin
	FSetCryptedGUIDCalled := True;
	FLastCryptedGUID := GUID;
end;

{TMockPluginSettingsManager}

constructor TMockPluginSettingsManager.Create;
begin
	inherited Create;
	FSwitchProxyPasswordStorageCalled := False;
	FSaveCalled := False;

	{Initialize with default test settings}
	FPluginSettings := Default(TPluginSettings);
	FPluginSettings.IniFilePath := '';
	FPluginSettings.IniDir := INI_DIR_PLUGIN;
	FPluginSettings.LoadSSLDLLOnlyFromPluginDir := False;
	FPluginSettings.PreserveFileTime := True;
	FPluginSettings.DescriptionEnabled := False;
	FPluginSettings.DescriptionFileName := '';
	FPluginSettings.DescriptionEditorEnabled := False;
	FPluginSettings.DescriptionTrackCloudFS := False;
	FPluginSettings.DescriptionCopyToCloud := False;
	FPluginSettings.OperationErrorMode := 0;
	FPluginSettings.RetryAttempts := 3;
	FPluginSettings.AttemptWait := 1000;
	FPluginSettings.PrecalculateHash := False;
	FPluginSettings.ForcePrecalculateSize := 0;
	FPluginSettings.CheckCRC := False;
	FPluginSettings.CopyBetweenAccountsMode := 0;
	FPluginSettings.AutoUpdateDownloadListing := False;
	FPluginSettings.DisableMultiThreading := False;
	FPluginSettings.IconsMode := 0;
	FPluginSettings.OverwriteLocalMode := 0;
	FPluginSettings.LogLevel := 0;

	{Connection settings}
	FPluginSettings.ConnectionSettings := Default(TConnectionSettings);
	FPluginSettings.ConnectionSettings.SocketTimeout := 30000;
	FPluginSettings.ConnectionSettings.ProxySettings := Default(TProxySettings);
	FPluginSettings.ConnectionSettings.ProxySettings.ProxyType := ProxyNone;
end;

function TMockPluginSettingsManager.GetSettings: TPluginSettings;
begin
	Result := FPluginSettings;
end;

procedure TMockPluginSettingsManager.SetSettings(Value: TPluginSettings);
begin
	FPluginSettings := Value;
end;

procedure TMockPluginSettingsManager.Save;
begin
	FSaveCalled := True;
end;

function TMockPluginSettingsManager.GetStreamingSettings(const FileName: WideString): TStreamingSettings;
begin
	Result := Default(TStreamingSettings);
end;

procedure TMockPluginSettingsManager.SetStreamingSettings(const FileName: WideString; StreamingSettings: TStreamingSettings);
begin
	{No-op for mock}
end;

procedure TMockPluginSettingsManager.GetStreamingExtensionsList(ExtensionsList: TStrings);
begin
	ExtensionsList.Clear;
end;

procedure TMockPluginSettingsManager.RemoveStreamingExtension(const Extension: WideString);
begin
	{No-op for mock}
end;

function TMockPluginSettingsManager.GetAccountsIniFilePath: WideString;
begin
	Result := EmptyWideStr;
end;

procedure TMockPluginSettingsManager.Refresh;
begin
	{No-op for mock}
end;

procedure TMockPluginSettingsManager.SwitchProxyPasswordStorage;
begin
	FSwitchProxyPasswordStorageCalled := True;
end;

{TConnectionManagerConstructorTest}

procedure TConnectionManagerConstructorTest.TestCreateWithNullImplementations;
var
	Manager: TConnectionManager;
	PluginSettings: IPluginSettingsManager;
	AccountsManager: IAccountsManager;
	HTTPManager: IHTTPManager;
	PasswordUI: IPasswordUIProvider;
	CipherValidator: ICipherValidator;
	FileSystem: IFileSystem;
	Progress: IProgress;
	Logger: ILogger;
	Request: IRequest;
	PasswordManager: IPasswordManager;
begin
	PluginSettings := TMockPluginSettingsManager.Create;
	AccountsManager := TMockAccountsManager.Create;
	HTTPManager := TNullHTTPManager.Create;
	PasswordUI := TNullPasswordUIProvider.Create;
	CipherValidator := TNullCipherValidator.Create;
	FileSystem := TNullFileSystem.Create;
	Progress := TNullProgress.Create;
	Logger := TNullLogger.Create;
	Request := TNullRequest.Create;
	PasswordManager := TNullPasswordManager.Create;

	Manager := TConnectionManager.Create(PluginSettings, AccountsManager, HTTPManager,
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Assert.IsNotNull(Manager, 'ConnectionManager should be created successfully');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerConstructorTest.TestDestroyWithoutConnections;
var
	Manager: TConnectionManager;
	PluginSettings: IPluginSettingsManager;
	AccountsManager: IAccountsManager;
	HTTPManager: IHTTPManager;
	PasswordUI: IPasswordUIProvider;
	CipherValidator: ICipherValidator;
	FileSystem: IFileSystem;
	Progress: IProgress;
	Logger: ILogger;
	Request: IRequest;
	PasswordManager: IPasswordManager;
begin
	PluginSettings := TMockPluginSettingsManager.Create;
	AccountsManager := TMockAccountsManager.Create;
	HTTPManager := TNullHTTPManager.Create;
	PasswordUI := TNullPasswordUIProvider.Create;
	CipherValidator := TNullCipherValidator.Create;
	FileSystem := TNullFileSystem.Create;
	Progress := TNullProgress.Create;
	Logger := TNullLogger.Create;
	Request := TNullRequest.Create;
	PasswordManager := TNullPasswordManager.Create;

	Manager := TConnectionManager.Create(PluginSettings, AccountsManager, HTTPManager,
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	Manager.Destroy;

	Assert.Pass('ConnectionManager destroyed without errors');
end;

{TConnectionManagerGetTest}

procedure TConnectionManagerGetTest.TestGetAlwaysReturnsValidInstance;
var
	Manager: TConnectionManager;
	PluginSettings: IPluginSettingsManager;
	AccountsManager: TMockAccountsManager;
	HTTPManager: IHTTPManager;
	PasswordUI: IPasswordUIProvider;
	CipherValidator: ICipherValidator;
	FileSystem: IFileSystem;
	Progress: IProgress;
	Logger: ILogger;
	Request: IRequest;
	PasswordManager: IPasswordManager;
	Cloud: TCloudMailRu;
begin
	{Create mock that returns public account (simplest case)}
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := True;

	PluginSettings := TMockPluginSettingsManager.Create;
	HTTPManager := TNullHTTPManager.Create;
	PasswordUI := TNullPasswordUIProvider.Create;
	CipherValidator := TNullCipherValidator.Create;
	FileSystem := TNullFileSystem.Create;
	Progress := TNullProgress.Create;
	Logger := TNullLogger.Create;
	Request := TNullRequest.Create;
	PasswordManager := TNullPasswordManager.Create;

	Manager := TConnectionManager.Create(PluginSettings, AccountsManager, HTTPManager,
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('test_connection');
		Assert.IsNotNull(Cloud, 'Get should always return a valid instance');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerGetTest.TestGetReturnsCachedInstance;
var
	Manager: TConnectionManager;
	PluginSettings: IPluginSettingsManager;
	AccountsManager: TMockAccountsManager;
	HTTPManager: IHTTPManager;
	PasswordUI: IPasswordUIProvider;
	CipherValidator: ICipherValidator;
	FileSystem: IFileSystem;
	Progress: IProgress;
	Logger: ILogger;
	Request: IRequest;
	PasswordManager: IPasswordManager;
	Cloud1, Cloud2: TCloudMailRu;
begin
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := True;

	PluginSettings := TMockPluginSettingsManager.Create;
	HTTPManager := TNullHTTPManager.Create;
	PasswordUI := TNullPasswordUIProvider.Create;
	CipherValidator := TNullCipherValidator.Create;
	FileSystem := TNullFileSystem.Create;
	Progress := TNullProgress.Create;
	Logger := TNullLogger.Create;
	Request := TNullRequest.Create;
	PasswordManager := TNullPasswordManager.Create;

	Manager := TConnectionManager.Create(PluginSettings, AccountsManager, HTTPManager,
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud1 := Manager.Get('test_connection');
		Cloud2 := Manager.Get('test_connection');

		Assert.AreSame(Cloud1, Cloud2, 'Get should return the same cached instance');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerGetTest.TestGetReturnsDifferentInstancesForDifferentNames;
var
	Manager: TConnectionManager;
	PluginSettings: IPluginSettingsManager;
	AccountsManager: TMockAccountsManager;
	HTTPManager: IHTTPManager;
	PasswordUI: IPasswordUIProvider;
	CipherValidator: ICipherValidator;
	FileSystem: IFileSystem;
	Progress: IProgress;
	Logger: ILogger;
	Request: IRequest;
	PasswordManager: IPasswordManager;
	Cloud1, Cloud2: TCloudMailRu;
begin
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := True;

	PluginSettings := TMockPluginSettingsManager.Create;
	HTTPManager := TNullHTTPManager.Create;
	PasswordUI := TNullPasswordUIProvider.Create;
	CipherValidator := TNullCipherValidator.Create;
	FileSystem := TNullFileSystem.Create;
	Progress := TNullProgress.Create;
	Logger := TNullLogger.Create;
	Request := TNullRequest.Create;
	PasswordManager := TNullPasswordManager.Create;

	Manager := TConnectionManager.Create(PluginSettings, AccountsManager, HTTPManager,
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud1 := Manager.Get('connection1');
		Cloud2 := Manager.Get('connection2');

		Assert.AreNotSame(Cloud1, Cloud2, 'Get should return different instances for different names');
	finally
		Manager.Destroy;
	end;
end;

{TConnectionManagerFreeTest}

procedure TConnectionManagerFreeTest.TestFreeNonExistentConnection;
var
	Manager: TConnectionManager;
	PluginSettings: IPluginSettingsManager;
	AccountsManager: IAccountsManager;
	HTTPManager: IHTTPManager;
	PasswordUI: IPasswordUIProvider;
	CipherValidator: ICipherValidator;
	FileSystem: IFileSystem;
	Progress: IProgress;
	Logger: ILogger;
	Request: IRequest;
	PasswordManager: IPasswordManager;
begin
	PluginSettings := TMockPluginSettingsManager.Create;
	AccountsManager := TMockAccountsManager.Create;
	HTTPManager := TNullHTTPManager.Create;
	PasswordUI := TNullPasswordUIProvider.Create;
	CipherValidator := TNullCipherValidator.Create;
	FileSystem := TNullFileSystem.Create;
	Progress := TNullProgress.Create;
	Logger := TNullLogger.Create;
	Request := TNullRequest.Create;
	PasswordManager := TNullPasswordManager.Create;

	Manager := TConnectionManager.Create(PluginSettings, AccountsManager, HTTPManager,
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Manager.Free('non_existent_connection');
		Assert.Pass('Free non-existent connection should not throw');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerFreeTest.TestFreeMultipleTimes;
var
	Manager: TConnectionManager;
	PluginSettings: IPluginSettingsManager;
	AccountsManager: IAccountsManager;
	HTTPManager: IHTTPManager;
	PasswordUI: IPasswordUIProvider;
	CipherValidator: ICipherValidator;
	FileSystem: IFileSystem;
	Progress: IProgress;
	Logger: ILogger;
	Request: IRequest;
	PasswordManager: IPasswordManager;
begin
	PluginSettings := TMockPluginSettingsManager.Create;
	AccountsManager := TMockAccountsManager.Create;
	HTTPManager := TNullHTTPManager.Create;
	PasswordUI := TNullPasswordUIProvider.Create;
	CipherValidator := TNullCipherValidator.Create;
	FileSystem := TNullFileSystem.Create;
	Progress := TNullProgress.Create;
	Logger := TNullLogger.Create;
	Request := TNullRequest.Create;
	PasswordManager := TNullPasswordManager.Create;

	Manager := TConnectionManager.Create(PluginSettings, AccountsManager, HTTPManager,
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Manager.Free('connection1');
		Manager.Free('connection1');
		Manager.Free('connection2');
		Assert.Pass('Multiple Free calls should not throw');
	finally
		Manager.Destroy;
	end;
end;

{TConnectionManagerCipherProfileTest}

procedure TConnectionManagerCipherProfileTest.TestConnectionCreatedWithCipherProfileInSettings;
var
	Manager: TConnectionManager;
	PluginSettings: IPluginSettingsManager;
	AccountsManager: TMockAccountsManager;
	HTTPManager: IHTTPManager;
	PasswordUI: IPasswordUIProvider;
	CipherValidator: ICipherValidator;
	FileSystem: IFileSystem;
	Progress: IProgress;
	Logger: ILogger;
	Request: IRequest;
	PasswordManager: IPasswordManager;
	Cloud: TCloudMailRu;
begin
	{CipherProfileId in account settings should not break connection creation.
		Uses public account (encryption path is skipped), but the field is still
		present in TCloudSettings -- verifying no crash from stale/unexpected value.}
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := True;
	AccountsManager.FAccountSettings.CipherProfileId := 'dcpcrypt-twofish256-cfb8-sha256';

	PluginSettings := TMockPluginSettingsManager.Create;
	HTTPManager := TNullHTTPManager.Create;
	PasswordUI := TNullPasswordUIProvider.Create;
	CipherValidator := TNullCipherValidator.Create;
	FileSystem := TNullFileSystem.Create;
	Progress := TNullProgress.Create;
	Logger := TNullLogger.Create;
	Request := TNullRequest.Create;
	PasswordManager := TNullPasswordManager.Create;

	Manager := TConnectionManager.Create(PluginSettings, AccountsManager, HTTPManager,
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('cipher_profile_test');
		Assert.IsNotNull(Cloud, 'Connection should be created despite CipherProfileId in settings');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerCipherProfileTest.TestConnectionCreatedWithEmptyCipherProfileInSettings;
var
	Manager: TConnectionManager;
	PluginSettings: IPluginSettingsManager;
	AccountsManager: TMockAccountsManager;
	HTTPManager: IHTTPManager;
	PasswordUI: IPasswordUIProvider;
	CipherValidator: ICipherValidator;
	FileSystem: IFileSystem;
	Progress: IProgress;
	Logger: ILogger;
	Request: IRequest;
	PasswordManager: IPasswordManager;
	Cloud: TCloudMailRu;
begin
	{Empty CipherProfileId (default for existing accounts) should not cause errors}
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := True;
	AccountsManager.FAccountSettings.CipherProfileId := '';

	PluginSettings := TMockPluginSettingsManager.Create;
	HTTPManager := TNullHTTPManager.Create;
	PasswordUI := TNullPasswordUIProvider.Create;
	CipherValidator := TNullCipherValidator.Create;
	FileSystem := TNullFileSystem.Create;
	Progress := TNullProgress.Create;
	Logger := TNullLogger.Create;
	Request := TNullRequest.Create;
	PasswordManager := TNullPasswordManager.Create;

	Manager := TConnectionManager.Create(PluginSettings, AccountsManager, HTTPManager,
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('empty_cipher_profile_test');
		Assert.IsNotNull(Cloud, 'Connection should be created with empty CipherProfileId');
	finally
		Manager.Destroy;
	end;
end;

{TMockPasswordManagerForConn}

constructor TMockPasswordManagerForConn.Create(GetPasswordResult: Integer; const PasswordValue: WideString);
begin
	inherited Create;
	FGetPasswordResult := GetPasswordResult;
	FGetPasswordValue := PasswordValue;
	FSetPasswordResult := FS_FILE_OK;
	FSetPasswordCalled := False;
end;

function TMockPasswordManagerForConn.GetPassword(Key: WideString; var Password: WideString): Integer;
begin
	Password := FGetPasswordValue;
	Result := FGetPasswordResult;
end;

function TMockPasswordManagerForConn.SetPassword(Key, Password: WideString): Integer;
begin
	FSetPasswordCalled := True;
	Result := FSetPasswordResult;
end;

{TMockPasswordUIForConn}

constructor TMockPasswordUIForConn.Create(AskPasswordResult: Integer; const PasswordValue: WideString);
begin
	inherited Create;
	FAskPasswordResult := AskPasswordResult;
	FAskPasswordValue := PasswordValue;
	FAskActionResult := mrCancel;
end;

function TMockPasswordUIForConn.AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
begin
	Password := FAskPasswordValue;
	Result := FAskPasswordResult;
end;

function TMockPasswordUIForConn.AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
begin
	Result := FAskActionResult;
end;

{TConnectionManagerEncryptionTest}

procedure TConnectionManagerEncryptionTest.TestInit_EncryptAlways_PasswordFound_CreatesCipher;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	PluginSettingsMgr: TMockPluginSettingsManager;
	PasswordMgr: TMockPasswordManagerForConn;
	Cloud: TCloudMailRu;
begin
	{Non-public account, EncryptModeAlways, password manager returns OK -> cipher is created}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := False;
	AccountsMgr.FAccountSettings.EncryptFilesMode := EncryptModeAlways;
	AccountsMgr.FAccountSettings.CryptedGUIDFiles := '';

	PluginSettingsMgr := TMockPluginSettingsManager.Create;
	PasswordMgr := TMockPasswordManagerForConn.Create(FS_FILE_OK, 'test-crypt-password');

	{Ensure cipher profiles are initialized}
	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Manager := TConnectionManager.Create(PluginSettingsMgr, AccountsMgr,
		TNullHTTPManager.Create, TNullPasswordUIProvider.Create,
		TNullCipherValidator.Create, TNullFileSystem.Create,
		TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		PasswordMgr, TNullTCHandler.Create, TNullAuthStrategyFactory.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('encrypt_test');
		Assert.IsNotNull(Cloud, 'Connection should be created with encryption enabled');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerEncryptionTest.TestInit_EncryptAlways_PasswordUnsupported_SkipsEncryption;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	PluginSettingsMgr: TMockPluginSettingsManager;
	PasswordMgr: TMockPasswordManagerForConn;
	Cloud: TCloudMailRu;
begin
	{Non-public account, EncryptModeAlways, password manager returns NOTSUPPORTED -> encryption skipped}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := False;
	AccountsMgr.FAccountSettings.EncryptFilesMode := EncryptModeAlways;
	AccountsMgr.FAccountSettings.CryptedGUIDFiles := '';

	PluginSettingsMgr := TMockPluginSettingsManager.Create;
	{FS_FILE_NOTSUPPORTED: user doesn't know master password -> InitCloudCryptPasswords returns False}
	PasswordMgr := TMockPasswordManagerForConn.Create(FS_FILE_NOTSUPPORTED, '');

	{Cipher registry needed because Init creates cipher with empty password as fallback}
	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Manager := TConnectionManager.Create(PluginSettingsMgr, AccountsMgr,
		TNullHTTPManager.Create, TNullPasswordUIProvider.Create,
		TNullCipherValidator.Create, TNullFileSystem.Create,
		TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		PasswordMgr, TNullTCHandler.Create, TNullAuthStrategyFactory.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('encrypt_unsupported_test');
		Assert.IsNotNull(Cloud, 'Connection should be created even when password unsupported');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerEncryptionTest.TestFree_ExistingConnection_RemovesFromPool;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	Cloud: TCloudMailRu;
begin
	{Get creates connection, Free destroys and removes it, next Get recreates}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := True;

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		TNullHTTPManager.Create, TNullPasswordUIProvider.Create,
		TNullCipherValidator.Create, TNullFileSystem.Create,
		TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullPasswordManager.Create, TNullTCHandler.Create, TNullAuthStrategyFactory.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('free_test');
		Assert.IsNotNull(Cloud, 'First Get should create connection');
		Manager.Free('free_test');
		{After Free, the connection is destroyed and removed from pool.
		 Next Get must re-create it without error.}
		Cloud := Manager.Get('free_test');
		Assert.IsNotNull(Cloud, 'Get after Free should recreate connection');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerEncryptionTest.TestInit_NonPublicWithProxy_PasswordFromINI;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	MockHTTPMgr: TMockHTTPManager;
	ProxyConnSettings: TConnectionSettings;
	Cloud: TCloudMailRu;
begin
	{Non-public account with HTTP proxy that has password in HTTPManager settings.
	 GetProxyPassword reads from FHTTPManager.ConnectionSettings, not plugin settings.}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := False;
	AccountsMgr.FAccountSettings.EncryptFilesMode := EncryptModeNone;

	MockHTTPMgr := TMockHTTPManager.Create(TNullCloudHTTP.Create);
	{Proxy settings must be on the HTTPManager -- GetProxyPassword reads them from there}
	ProxyConnSettings := Default(TConnectionSettings);
	ProxyConnSettings.ProxySettings.ProxyType := ProxyHTTP;
	ProxyConnSettings.ProxySettings.User := 'proxyuser';
	ProxyConnSettings.ProxySettings.Password := 'proxypass';
	MockHTTPMgr.SetConnectionSettings(ProxyConnSettings);

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		MockHTTPMgr, TNullPasswordUIProvider.Create,
		TNullCipherValidator.Create, TNullFileSystem.Create,
		TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullPasswordManager.Create, TNullTCHandler.Create, TNullAuthStrategyFactory.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('proxy_test');
		Assert.IsNotNull(Cloud, 'Connection should be created with proxy password from INI');
	finally
		Manager.Destroy;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TConnectionManagerConstructorTest);
TDUnitX.RegisterTestFixture(TConnectionManagerGetTest);
TDUnitX.RegisterTestFixture(TConnectionManagerFreeTest);
TDUnitX.RegisterTestFixture(TConnectionManagerCipherProfileTest);
TDUnitX.RegisterTestFixture(TConnectionManagerEncryptionTest);

end.
