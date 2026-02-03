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

	{Mock password manager returning configurable GetPassword results.
		Supports a queue for returning different results on successive calls.}
	TMockPasswordManagerForConn = class(TInterfacedObject, IPasswordManager)
	private
		FGetPasswordResult: Integer;
		FGetPasswordValue: WideString;
		FSetPasswordResult: Integer;
		FSetPasswordCalled: Boolean;
		FGetPasswordQueue: TList<TPair<Integer, WideString>>;
	public
		constructor Create(GetPasswordResult: Integer; const PasswordValue: WideString = '');
		destructor Destroy; override;
		function GetPassword(Key: WideString; var Password: WideString): Integer;
		function SetPassword(Key, Password: WideString): Integer;
		{Enqueues a (ResultCode, PasswordValue) pair for GetPassword to dequeue on the next call}
		procedure QueueGetPassword(ResultCode: Integer; const PasswordValue: WideString);
		property SetPasswordResult: Integer read FSetPasswordResult write FSetPasswordResult;
		property SetPasswordCalled: Boolean read FSetPasswordCalled;
	end;

	{Mock cipher validator with queue-based CheckPasswordGUID results}
	TMockCipherValidatorForConn = class(TInterfacedObject, ICipherValidator)
	private
		FCheckQueue: TList<Boolean>;
		FCryptedGUIDResult: WideString;
	public
		constructor Create;
		destructor Destroy; override;
		function GetCryptedGUID(const Password: WideString): WideString;
		function CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean;
		{Enqueues a Boolean result for the next CheckPasswordGUID call}
		procedure QueueCheckResult(Value: Boolean);
		property CryptedGUIDResult: WideString read FCryptedGUIDResult write FCryptedGUIDResult;
	end;

	{Mock logger that captures log calls for assertion}
	TMockLoggerForConn = class(TInterfacedObject, ILogger)
	private
		FLogCalls: Integer;
		FLastLogLevel: Integer;
		FLastMsgType: Integer;
		FLastLogString: WideString;
	public
		constructor Create;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
		property LogCalls: Integer read FLogCalls;
		property LastLogLevel: Integer read FLastLogLevel;
		property LastMsgType: Integer read FLastMsgType;
		property LastLogString: WideString read FLastLogString;
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

		[Test]
		{EncryptModeAlways + FS_FILE_READERROR falls through to AskOnce, user cancels}
		procedure TestInit_EncryptAlways_ReadError_FallsToAskOnce;

		[Test]
		{GUID mismatch in Init logs wrong-password error via logger}
		procedure TestInit_GUIDMismatch_LogsWrongPassword;

		[Test]
		{GUID mismatch in GetFilesPassword, user chooses to update GUID}
		procedure TestGetFilesPassword_GUIDMismatch_UserUpdatesGUID;

		[Test]
		{GUID mismatch in GetFilesPassword, user ignores mismatch}
		procedure TestGetFilesPassword_GUIDMismatch_UserIgnores;

		[Test]
		{GUID mismatch in GetFilesPassword, user retries password entry}
		procedure TestGetFilesPassword_GUIDMismatch_UserRetries;

		[Test]
		{Proxy password from TC password manager -> sets HTTPManager proxy password}
		procedure TestGetProxyPassword_TCManagerFound;

		[Test]
		{Proxy with empty password, user enters, SetPassword succeeds -> SwitchProxyPasswordStorage called}
		procedure TestGetProxyPassword_EmptyPwd_UserEnters_SaveOK;

		[Test]
		{Proxy with empty password, user enters, SetPassword fails -> SwitchProxyPasswordStorage not called}
		procedure TestGetProxyPassword_EmptyPwd_UserEnters_SaveFails;
	end;

implementation

uses
	CloudConstants,
	SettingsConstants,
	LanguageStrings,
	WFXTypes,
	CipherProfile,
	BCryptProvider,
	MockHTTPManager,
	MockCloudHTTP,
	CloudHTTP,
	Vcl.Controls,
	System.UITypes,
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
	FGetPasswordQueue := TList<TPair<Integer, WideString>>.Create;
end;

destructor TMockPasswordManagerForConn.Destroy;
begin
	FreeAndNil(FGetPasswordQueue);
	inherited;
end;

function TMockPasswordManagerForConn.GetPassword(Key: WideString; var Password: WideString): Integer;
var
	Pair: TPair<Integer, WideString>;
begin
	{Dequeue from queue if available, otherwise use fixed values}
	if FGetPasswordQueue.Count > 0 then
	begin
		Pair := FGetPasswordQueue[0];
		FGetPasswordQueue.Delete(0);
		Password := Pair.Value;
		Result := Pair.Key;
	end else
	begin
		Password := FGetPasswordValue;
		Result := FGetPasswordResult;
	end;
end;

function TMockPasswordManagerForConn.SetPassword(Key, Password: WideString): Integer;
begin
	FSetPasswordCalled := True;
	Result := FSetPasswordResult;
end;

procedure TMockPasswordManagerForConn.QueueGetPassword(ResultCode: Integer; const PasswordValue: WideString);
begin
	FGetPasswordQueue.Add(TPair<Integer, WideString>.Create(ResultCode, PasswordValue));
end;

{TMockCipherValidatorForConn}

constructor TMockCipherValidatorForConn.Create;
begin
	inherited Create;
	FCheckQueue := TList<Boolean>.Create;
	FCryptedGUIDResult := '';
end;

destructor TMockCipherValidatorForConn.Destroy;
begin
	FreeAndNil(FCheckQueue);
	inherited;
end;

function TMockCipherValidatorForConn.GetCryptedGUID(const Password: WideString): WideString;
begin
	Result := FCryptedGUIDResult;
end;

function TMockCipherValidatorForConn.CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean;
begin
	{Dequeue from queue if available, otherwise return True}
	if FCheckQueue.Count > 0 then
	begin
		Result := FCheckQueue[0];
		FCheckQueue.Delete(0);
	end else
		Result := True;
end;

procedure TMockCipherValidatorForConn.QueueCheckResult(Value: Boolean);
begin
	FCheckQueue.Add(Value);
end;

{TMockLoggerForConn}

constructor TMockLoggerForConn.Create;
begin
	inherited Create;
	FLogCalls := 0;
	FLastLogLevel := 0;
	FLastMsgType := 0;
	FLastLogString := '';
end;

procedure TMockLoggerForConn.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	Inc(FLogCalls);
	FLastLogLevel := LogLevel;
	FLastMsgType := MsgType;
	FLastLogString := LogString;
end;

procedure TMockLoggerForConn.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
begin
	Log(LogLevel, MsgType, Format(LogString, Args));
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

procedure TConnectionManagerEncryptionTest.TestInit_EncryptAlways_ReadError_FallsToAskOnce;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	PasswordMgr: TMockPasswordManagerForConn;
	PasswordUI: TMockPasswordUIForConn;
	Cloud: TCloudMailRu;
begin
	{FS_FILE_READERROR from TC password store switches to AskOnce mode.
	 User cancels AskPassword -> GetFilesPassword returns False, but Init still completes.}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := False;
	AccountsMgr.FAccountSettings.EncryptFilesMode := EncryptModeAlways;
	AccountsMgr.FAccountSettings.CryptedGUIDFiles := '';

	PasswordMgr := TMockPasswordManagerForConn.Create(FS_FILE_READERROR, '');
	PasswordUI := TMockPasswordUIForConn.Create(mrCancel);

	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		TNullHTTPManager.Create, PasswordUI,
		TNullCipherValidator.Create, TNullFileSystem.Create,
		TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		PasswordMgr, TNullTCHandler.Create, TNullAuthStrategyFactory.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('readerror_test');
		Assert.IsNotNull(Cloud, 'Connection should be created even when password retrieval fails');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerEncryptionTest.TestInit_GUIDMismatch_LogsWrongPassword;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	PasswordMgr: TMockPasswordManagerForConn;
	MockValidator: TMockCipherValidatorForConn;
	MockLog: TMockLoggerForConn;
	Cloud: TCloudMailRu;
begin
	{When GUID check fails in Init (line 172), logger receives ERR_WRONG_ENCRYPT_PASSWORD.
	 CipherValidator queue: [True, False] -- True for GetFilesPassword (line 241),
	 False for Init (line 172).}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := False;
	AccountsMgr.FAccountSettings.EncryptFilesMode := EncryptModeAlways;
	AccountsMgr.FAccountSettings.CryptedGUIDFiles := 'stored-guid';

	PasswordMgr := TMockPasswordManagerForConn.Create(FS_FILE_OK, 'test-password');

	MockValidator := TMockCipherValidatorForConn.Create;
	MockValidator.QueueCheckResult(True); {GetFilesPassword line 241 -- passes}
	MockValidator.QueueCheckResult(False); {Init line 172 -- fails -> logs error}

	MockLog := TMockLoggerForConn.Create;

	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		TNullHTTPManager.Create, TNullPasswordUIProvider.Create,
		MockValidator, TNullFileSystem.Create,
		TNullProgress.Create, MockLog, TNullRequest.Create,
		PasswordMgr, TNullTCHandler.Create, TNullAuthStrategyFactory.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('guid_mismatch_log_test');
		Assert.IsNotNull(Cloud, 'Connection should be created despite GUID mismatch');
		{The CONNECT log + the error log}
		Assert.IsTrue(MockLog.LogCalls >= 2, 'Logger should have received at least 2 calls');
		Assert.AreEqual(LOG_LEVEL_ERROR, MockLog.LastLogLevel, 'Last log should be error level');
		Assert.AreEqual(msgtype_importanterror, MockLog.LastMsgType, 'Last log should be important error');
		Assert.AreEqual(ERR_WRONG_ENCRYPT_PASSWORD, MockLog.LastLogString, 'Last log should be wrong password message');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerEncryptionTest.TestGetFilesPassword_GUIDMismatch_UserUpdatesGUID;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	PasswordMgr: TMockPasswordManagerForConn;
	MockValidator: TMockCipherValidatorForConn;
	PasswordUI: TMockPasswordUIForConn;
	Cloud: TCloudMailRu;
begin
	{GUID mismatch in GetFilesPassword, user selects mrYes (update GUID).
	 CipherValidator queue: [False, True] -- False triggers AskAction dialog,
	 True for Init's GUID check (line 172). GetCryptedGUID returns 'new-guid'.}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := False;
	AccountsMgr.FAccountSettings.EncryptFilesMode := EncryptModeAlways;
	AccountsMgr.FAccountSettings.CryptedGUIDFiles := 'old-guid';

	PasswordMgr := TMockPasswordManagerForConn.Create(FS_FILE_OK, 'test-password');

	MockValidator := TMockCipherValidatorForConn.Create;
	MockValidator.QueueCheckResult(False); {GetFilesPassword line 241 -- mismatch}
	MockValidator.QueueCheckResult(True); {Init line 172 -- passes after GUID update}
	MockValidator.CryptedGUIDResult := 'new-guid';

	PasswordUI := TMockPasswordUIForConn.Create(mrCancel);
	PasswordUI.AskActionResult := mrYes;

	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		TNullHTTPManager.Create, PasswordUI,
		MockValidator, TNullFileSystem.Create,
		TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		PasswordMgr, TNullTCHandler.Create, TNullAuthStrategyFactory.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('guid_update_test');
		Assert.IsNotNull(Cloud, 'Connection should be created after GUID update');
		Assert.IsTrue(AccountsMgr.SetCryptedGUIDCalled, 'SetCryptedGUID should have been called');
		Assert.AreEqual('new-guid', string(AccountsMgr.LastCryptedGUID), 'GUID should be updated to new value');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerEncryptionTest.TestGetFilesPassword_GUIDMismatch_UserIgnores;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	PasswordMgr: TMockPasswordManagerForConn;
	MockValidator: TMockCipherValidatorForConn;
	PasswordUI: TMockPasswordUIForConn;
	Cloud: TCloudMailRu;
begin
	{GUID mismatch in GetFilesPassword, user selects mrNo (ignore).
	 CipherValidator queue: [False, False] -- both calls fail (GetFilesPassword and Init).}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := False;
	AccountsMgr.FAccountSettings.EncryptFilesMode := EncryptModeAlways;
	AccountsMgr.FAccountSettings.CryptedGUIDFiles := 'old-guid';

	PasswordMgr := TMockPasswordManagerForConn.Create(FS_FILE_OK, 'test-password');

	MockValidator := TMockCipherValidatorForConn.Create;
	MockValidator.QueueCheckResult(False); {GetFilesPassword line 241 -- mismatch}
	MockValidator.QueueCheckResult(False); {Init line 172 -- also fails -> logs error}

	PasswordUI := TMockPasswordUIForConn.Create(mrCancel);
	PasswordUI.AskActionResult := mrNo;

	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		TNullHTTPManager.Create, PasswordUI,
		MockValidator, TNullFileSystem.Create,
		TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		PasswordMgr, TNullTCHandler.Create, TNullAuthStrategyFactory.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('guid_ignore_test');
		Assert.IsNotNull(Cloud, 'Connection should be created when user ignores GUID mismatch');
		Assert.IsFalse(AccountsMgr.SetCryptedGUIDCalled, 'SetCryptedGUID should not be called on ignore');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerEncryptionTest.TestGetFilesPassword_GUIDMismatch_UserRetries;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	PasswordMgr: TMockPasswordManagerForConn;
	MockValidator: TMockCipherValidatorForConn;
	PasswordUI: TMockPasswordUIForConn;
	Cloud: TCloudMailRu;
begin
	{GUID mismatch in GetFilesPassword, user selects mrRetry.
	 1st iteration: CheckPasswordGUID=False -> AskAction=mrRetry -> PasswordActionRetry=True.
	 2nd iteration: new password fetched from queue, CheckPasswordGUID=True -> loop exits.
	 Init's GUID check also passes.}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := False;
	AccountsMgr.FAccountSettings.EncryptFilesMode := EncryptModeAlways;
	AccountsMgr.FAccountSettings.CryptedGUIDFiles := 'stored-guid';

	PasswordMgr := TMockPasswordManagerForConn.Create(FS_FILE_OK, 'correct-password');
	{Queue: 1st GetPassword returns 'wrong' password, fallback returns 'correct-password'}
	PasswordMgr.QueueGetPassword(FS_FILE_OK, 'wrong-password');

	MockValidator := TMockCipherValidatorForConn.Create;
	MockValidator.QueueCheckResult(False); {1st iteration GetFilesPassword -- mismatch with 'wrong'}
	MockValidator.QueueCheckResult(True); {2nd iteration GetFilesPassword -- matches with 'correct'}
	MockValidator.QueueCheckResult(True); {Init line 172 -- passes}

	PasswordUI := TMockPasswordUIForConn.Create(mrCancel);
	PasswordUI.AskActionResult := mrRetry;

	TCipherProfileRegistry.Reset;
	TCipherProfileRegistry.Initialize(nil, TBCryptProvider.Create);

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		TNullHTTPManager.Create, PasswordUI,
		MockValidator, TNullFileSystem.Create,
		TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		PasswordMgr, TNullTCHandler.Create, TNullAuthStrategyFactory.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('guid_retry_test');
		Assert.IsNotNull(Cloud, 'Connection should be created after retry with correct password');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerEncryptionTest.TestGetProxyPassword_TCManagerFound;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	MockHTTPMgr: TMockHTTPManager;
	PasswordMgr: TMockPasswordManagerForConn;
	ProxyConnSettings: TConnectionSettings;
	Cloud: TCloudMailRu;
begin
	{Non-public account, proxy with UseTCPasswordManager=True and password found in TC store.
	 GetProxyPassword retrieves password and sets it on HTTPManager.}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := False;
	AccountsMgr.FAccountSettings.EncryptFilesMode := EncryptModeNone;

	MockHTTPMgr := TMockHTTPManager.Create(TNullCloudHTTP.Create);
	ProxyConnSettings := Default(TConnectionSettings);
	ProxyConnSettings.ProxySettings.ProxyType := ProxyHTTP;
	ProxyConnSettings.ProxySettings.User := 'user';
	ProxyConnSettings.ProxySettings.Password := '';
	ProxyConnSettings.ProxySettings.UseTCPasswordManager := True;
	MockHTTPMgr.SetConnectionSettings(ProxyConnSettings);

	PasswordMgr := TMockPasswordManagerForConn.Create(FS_FILE_OK, 'tc-proxy-pass');

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		MockHTTPMgr, TNullPasswordUIProvider.Create,
		TNullCipherValidator.Create, TNullFileSystem.Create,
		TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		PasswordMgr, TNullTCHandler.Create, TNullAuthStrategyFactory.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('proxy_tc_test');
		Assert.IsNotNull(Cloud, 'Connection should be created with proxy password from TC manager');
		Assert.AreEqual('tc-proxy-pass', string(MockHTTPMgr.GetConnectionSettings.ProxySettings.Password),
			'Proxy password should be set on HTTPManager');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerEncryptionTest.TestGetProxyPassword_EmptyPwd_UserEnters_SaveOK;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	MockHTTPMgr: TMockHTTPManager;
	PasswordMgr: TMockPasswordManagerForConn;
	PasswordUI: TMockPasswordUIForConn;
	PluginSettingsMgr: TMockPluginSettingsManager;
	ProxyConnSettings: TConnectionSettings;
	Cloud: TCloudMailRu;
begin
	{Proxy with empty password, not using TC password manager.
	 User enters password via AskPassword, SetPassword succeeds -> SwitchProxyPasswordStorage called.}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := False;
	AccountsMgr.FAccountSettings.EncryptFilesMode := EncryptModeNone;

	MockHTTPMgr := TMockHTTPManager.Create(TNullCloudHTTP.Create);
	ProxyConnSettings := Default(TConnectionSettings);
	ProxyConnSettings.ProxySettings.ProxyType := ProxyHTTP;
	ProxyConnSettings.ProxySettings.User := 'user';
	ProxyConnSettings.ProxySettings.Password := '';
	ProxyConnSettings.ProxySettings.UseTCPasswordManager := False;
	MockHTTPMgr.SetConnectionSettings(ProxyConnSettings);

	PasswordMgr := TMockPasswordManagerForConn.Create(FS_FILE_NOTFOUND, '');
	PasswordMgr.SetPasswordResult := FS_FILE_OK;

	PasswordUI := TMockPasswordUIForConn.Create(mrOk, 'user-proxy-pass');

	PluginSettingsMgr := TMockPluginSettingsManager.Create;

	Manager := TConnectionManager.Create(PluginSettingsMgr, AccountsMgr,
		MockHTTPMgr, PasswordUI,
		TNullCipherValidator.Create, TNullFileSystem.Create,
		TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		PasswordMgr, TNullTCHandler.Create, TNullAuthStrategyFactory.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('proxy_save_ok_test');
		Assert.IsNotNull(Cloud, 'Connection should be created after user enters proxy password');
		Assert.IsTrue(PasswordMgr.SetPasswordCalled, 'SetPassword should have been called');
		Assert.IsTrue(PluginSettingsMgr.SwitchProxyPasswordStorageCalled,
			'SwitchProxyPasswordStorage should be called when password saved to TC store');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerEncryptionTest.TestGetProxyPassword_EmptyPwd_UserEnters_SaveFails;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	MockHTTPMgr: TMockHTTPManager;
	PasswordMgr: TMockPasswordManagerForConn;
	PasswordUI: TMockPasswordUIForConn;
	PluginSettingsMgr: TMockPluginSettingsManager;
	ProxyConnSettings: TConnectionSettings;
	Cloud: TCloudMailRu;
begin
	{Same as SaveOK but SetPassword returns FS_FILE_WRITEERROR -> SwitchProxyPasswordStorage not called.}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := False;
	AccountsMgr.FAccountSettings.EncryptFilesMode := EncryptModeNone;

	MockHTTPMgr := TMockHTTPManager.Create(TNullCloudHTTP.Create);
	ProxyConnSettings := Default(TConnectionSettings);
	ProxyConnSettings.ProxySettings.ProxyType := ProxyHTTP;
	ProxyConnSettings.ProxySettings.User := 'user';
	ProxyConnSettings.ProxySettings.Password := '';
	ProxyConnSettings.ProxySettings.UseTCPasswordManager := False;
	MockHTTPMgr.SetConnectionSettings(ProxyConnSettings);

	PasswordMgr := TMockPasswordManagerForConn.Create(FS_FILE_NOTFOUND, '');
	PasswordMgr.SetPasswordResult := FS_FILE_WRITEERROR;

	PasswordUI := TMockPasswordUIForConn.Create(mrOk, 'user-proxy-pass');

	PluginSettingsMgr := TMockPluginSettingsManager.Create;

	Manager := TConnectionManager.Create(PluginSettingsMgr, AccountsMgr,
		MockHTTPMgr, PasswordUI,
		TNullCipherValidator.Create, TNullFileSystem.Create,
		TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		PasswordMgr, TNullTCHandler.Create, TNullAuthStrategyFactory.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	try
		Cloud := Manager.Get('proxy_save_fail_test');
		Assert.IsNotNull(Cloud, 'Connection should be created even when password save fails');
		Assert.IsTrue(PasswordMgr.SetPasswordCalled, 'SetPassword should have been called');
		Assert.IsFalse(PluginSettingsMgr.SwitchProxyPasswordStorageCalled,
			'SwitchProxyPasswordStorage should NOT be called when password save fails');
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
