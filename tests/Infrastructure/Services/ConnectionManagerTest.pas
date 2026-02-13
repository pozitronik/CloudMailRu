unit ConnectionManagerTest;

interface

uses
	ConnectionManager,
	CloudMailRu,
	HTTPManager,
	FileSystem,
	Logger,
	Progress,
	Request,
	TCHandler,
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
	AccountCredentialsProvider,
	ServerProfileManager,
	FileEncryptionResolver,
	ProxyPasswordResolver;

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
		FGetAccountSettingsCallCount: Integer;
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
		property GetAccountSettingsCallCount: Integer read FGetAccountSettingsCallCount;
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

		[Test]
		{Free connection that was created via Get -> removes from pool}
		procedure TestFree_ExistingConnection_RemovesFromPool;
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

	[TestFixture]
	TConnectionManagerInvalidateTest = class
	public
		[Test]
		{InvalidateAll on empty pool does not crash}
		procedure TestInvalidateAll_EmptyPool_NoCrash;

		[Test]
		{After InvalidateAll, Get recreates the connection (returns different instance)}
		procedure TestInvalidateAll_GetReturns_NewInstance;

		[Test]
		{Connections created after InvalidateAll are not affected}
		procedure TestInvalidateAll_DoesNotAffect_NewConnections;

		[Test]
		{Free clears stale entry -- no double-free on subsequent Get}
		procedure TestFree_ClearsStaleEntry;

		[Test]
		{Multiple InvalidateAll calls are safe and idempotent}
		procedure TestInvalidateAll_MultipleCalls_Idempotent;
	end;

implementation

uses
	CloudConstants,
	SettingsConstants,
	LanguageStrings,
	WFXTypes,
	CipherProfile,
	BCryptProvider,
	System.SysUtils;

{TMockAccountsManager}

constructor TMockAccountsManager.Create;
begin
	inherited Create;
	FSwitchPasswordStorageCalled := False;
	FSetCryptedGUIDCalled := False;
	FDeleteAccountCalled := False;
	FLastCryptedGUID := '';
	FGetAccountSettingsCallCount := 0;

	{Initialize with default test settings - public account to simplify testing}
	FAccountSettings := Default(TAccountSettings);
	FAccountSettings.Email := 'test@mail.ru';
	FAccountSettings.Password := '';
	FAccountSettings.UseTCPasswordManager := False;
	FAccountSettings.PublicAccount := True; {Public account skips password retrieval}
	FAccountSettings.PublicUrl := '';
	FAccountSettings.EncryptFiles := False;
	FAccountSettings.CryptedGUIDFiles := '';
	FAccountSettings.Server := '';
	FAccountSettings.SplitLargeFiles := True;
	FAccountSettings.UnlimitedFileSize := False;
end;

function TMockAccountsManager.GetAccountsList(const AccountTypes: EAccountType; const VirtualTypes: EVirtualType): TWSList;
begin
	Result.Clear;
end;

function TMockAccountsManager.GetAccountSettings(Account: WideString): TAccountSettings;
begin
	Inc(FGetAccountSettingsCallCount);
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
begin
	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, TMockAccountsManager.Create,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
	try
		Assert.IsNotNull(Manager, 'ConnectionManager should be created successfully');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerConstructorTest.TestDestroyWithoutConnections;
var
	Manager: TConnectionManager;
begin
	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, TMockAccountsManager.Create,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
	Manager.Destroy;

	Assert.Pass('ConnectionManager destroyed without errors');
end;

{TConnectionManagerGetTest}

procedure TConnectionManagerGetTest.TestGetAlwaysReturnsValidInstance;
var
	Manager: TConnectionManager;
	AccountsManager: TMockAccountsManager;
	Cloud: TCloudMailRu;
begin
	{Create mock that returns public account (simplest case)}
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := True;

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsManager,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
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
	AccountsManager: TMockAccountsManager;
	Cloud1, Cloud2: TCloudMailRu;
begin
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := True;

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsManager,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
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
	AccountsManager: TMockAccountsManager;
	Cloud1, Cloud2: TCloudMailRu;
begin
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := True;

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsManager,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
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
begin
	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, TMockAccountsManager.Create,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
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
begin
	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, TMockAccountsManager.Create,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
	try
		Manager.Free('connection1');
		Manager.Free('connection1');
		Manager.Free('connection2');
		Assert.Pass('Multiple Free calls should not throw');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerFreeTest.TestFree_ExistingConnection_RemovesFromPool;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	Cloud: TCloudMailRu;
begin
	{Get creates connection, Free destroys and removes it, next Get recreates}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := True;

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
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

{TConnectionManagerCipherProfileTest}

procedure TConnectionManagerCipherProfileTest.TestConnectionCreatedWithCipherProfileInSettings;
var
	Manager: TConnectionManager;
	AccountsManager: TMockAccountsManager;
	Cloud: TCloudMailRu;
begin
	{CipherProfileId in account settings should not break connection creation.
		Uses public account (encryption path is skipped), but the field is still
		present in TCloudSettings -- verifying no crash from stale/unexpected value.}
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := True;
	AccountsManager.FAccountSettings.CipherProfileId := 'dcpcrypt-twofish256-cfb8-sha256';

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsManager,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
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
	AccountsManager: TMockAccountsManager;
	Cloud: TCloudMailRu;
begin
	{Empty CipherProfileId (default for existing accounts) should not cause errors}
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := True;
	AccountsManager.FAccountSettings.CipherProfileId := '';

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsManager,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
	try
		Cloud := Manager.Get('empty_cipher_profile_test');
		Assert.IsNotNull(Cloud, 'Connection should be created with empty CipherProfileId');
	finally
		Manager.Destroy;
	end;
end;

{TConnectionManagerInvalidateTest}

procedure TConnectionManagerInvalidateTest.TestInvalidateAll_EmptyPool_NoCrash;
var
	Manager: TConnectionManager;
begin
	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, TMockAccountsManager.Create,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
	try
		Manager.InvalidateAll;
		Assert.Pass('InvalidateAll on empty pool should not crash');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerInvalidateTest.TestInvalidateAll_GetReturns_NewInstance;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	Cloud: TCloudMailRu;
	CallCountBefore: Integer;
begin
	{Verify that Get after InvalidateAll triggers Init (which calls GetAccountSettings again)}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := True;

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
	try
		Cloud := Manager.Get('test_conn');
		Assert.IsNotNull(Cloud, 'First Get should return valid instance');
		Assert.AreEqual(1, AccountsMgr.GetAccountSettingsCallCount, 'Init should be called once on first Get');

		Manager.InvalidateAll;
		CallCountBefore := AccountsMgr.GetAccountSettingsCallCount;

		Cloud := Manager.Get('test_conn');
		Assert.IsNotNull(Cloud, 'Get after InvalidateAll should return valid instance');
		Assert.AreEqual(CallCountBefore + 1, AccountsMgr.GetAccountSettingsCallCount, 'Init should be called again after InvalidateAll');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerInvalidateTest.TestInvalidateAll_DoesNotAffect_NewConnections;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	CloudB1, CloudB2: TCloudMailRu;
begin
	{Create A, invalidate, then create B (new). B should not be stale.}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := True;

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
	try
		Manager.Get('conn_a');
		Manager.InvalidateAll; {Only conn_a is marked stale}

		CloudB1 := Manager.Get('conn_b'); {Created after invalidation - not stale}
		CloudB2 := Manager.Get('conn_b');
		Assert.AreSame(CloudB1, CloudB2, 'Connection created after InvalidateAll should be cached normally');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerInvalidateTest.TestFree_ClearsStaleEntry;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	Cloud: TCloudMailRu;
	CallCountAfterFree: Integer;
begin
	{Invalidate A, then Free(A), then Get(A) -- should create fresh without double-free}
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := True;

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
	try
		Manager.Get('free_stale');
		Assert.AreEqual(1, AccountsMgr.GetAccountSettingsCallCount, 'Init called once on first Get');

		Manager.InvalidateAll;
		Manager.Free('free_stale'); {Destroys instance and clears stale entry}
		CallCountAfterFree := AccountsMgr.GetAccountSettingsCallCount;

		Cloud := Manager.Get('free_stale');
		Assert.IsNotNull(Cloud, 'Get after Free of stale connection should return fresh instance');
		Assert.AreEqual(CallCountAfterFree + 1, AccountsMgr.GetAccountSettingsCallCount, 'Init should be called again after Free+Get');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerInvalidateTest.TestInvalidateAll_MultipleCalls_Idempotent;
var
	Manager: TConnectionManager;
	AccountsMgr: TMockAccountsManager;
	Cloud: TCloudMailRu;
	CallCountBefore: Integer;
begin
	AccountsMgr := TMockAccountsManager.Create;
	AccountsMgr.FAccountSettings.PublicAccount := True;

	Manager := TConnectionManager.Create(TMockPluginSettingsManager.Create, AccountsMgr,
		TNullHTTPManager.Create, TNullFileEncryptionResolver.Create, TNullProxyPasswordResolver.Create,
		TNullFileSystem.Create, TNullProgress.Create, TNullLogger.Create, TNullRequest.Create,
		TNullTCHandler.Create, TNullAuthStrategyFactory.Create, TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create, TNullServerProfileManager.Create);
	try
		Manager.Get('multi_inv');
		Manager.InvalidateAll;
		Manager.InvalidateAll; {Second call should be safe}
		Manager.InvalidateAll; {Third call should be safe}
		CallCountBefore := AccountsMgr.GetAccountSettingsCallCount;

		Cloud := Manager.Get('multi_inv');
		Assert.IsNotNull(Cloud, 'Get after multiple InvalidateAll should return valid instance');
		Assert.AreEqual(CallCountBefore + 1, AccountsMgr.GetAccountSettingsCallCount, 'Init should be called exactly once despite multiple InvalidateAll');
	finally
		Manager.Destroy;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TConnectionManagerConstructorTest);
TDUnitX.RegisterTestFixture(TConnectionManagerGetTest);
TDUnitX.RegisterTestFixture(TConnectionManagerFreeTest);
TDUnitX.RegisterTestFixture(TConnectionManagerCipherProfileTest);
TDUnitX.RegisterTestFixture(TConnectionManagerInvalidateTest);

end.
