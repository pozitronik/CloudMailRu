unit ConnectionManagerTest;

interface

uses
	ConnectionManager,
	CloudMailRu,
	PasswordUIProvider,
	HTTPManager,
	CipherValidator,
	WindowsFileSystem,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	TCPasswordManager,
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
	System.Classes,
	DUnitX.TestFramework;

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
		{Verifies Get returns nil when password UI cancels (TNullPasswordUIProvider returns mrCancel)}
		procedure TestGetReturnsNilWhenPasswordCancelled;

		[Test]
		{Verifies Get sets OperationResult to error when password UI cancels}
		procedure TestGetSetsErrorResultWhenPasswordCancelled;

		[Test]
		{Verifies Get returns nil for same connection on repeated calls when init fails}
		procedure TestGetReturnsNilConsistently;
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

implementation

uses
	CMRConstants,
	SettingsConstants,
	System.SysUtils,
	System.Generics.Collections;

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
	FAccountSettings.TwostepAuth := False;
	FAccountSettings.PublicAccount := True; {Public account skips password retrieval}
	FAccountSettings.PublicUrl := '';
	FAccountSettings.EncryptFilesMode := EncryptModeNone;
	FAccountSettings.EncryptFileNames := False;
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
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create);
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
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create);
	Manager.Destroy;

	Assert.Pass('ConnectionManager destroyed without errors');
end;

{TConnectionManagerGetTest}

procedure TConnectionManagerGetTest.TestGetReturnsNilWhenPasswordCancelled;
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
	OperationResult: Integer;
begin
	{Create mock that returns non-public account (requires password)}
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := False;
	AccountsManager.FAccountSettings.Password := ''; {Empty password will trigger UI prompt}

	PluginSettings := TMockPluginSettingsManager.Create;
	HTTPManager := TNullHTTPManager.Create;
	PasswordUI := TNullPasswordUIProvider.Create; {Returns mrCancel}
	CipherValidator := TNullCipherValidator.Create;
	FileSystem := TNullFileSystem.Create;
	Progress := TNullProgress.Create;
	Logger := TNullLogger.Create;
	Request := TNullRequest.Create;
	PasswordManager := TNullPasswordManager.Create;

	Manager := TConnectionManager.Create(PluginSettings, AccountsManager, HTTPManager,
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create);
	try
		Cloud := Manager.Get('test_connection', OperationResult);
		Assert.IsNull(Cloud, 'Get should return nil when password UI is cancelled');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerGetTest.TestGetSetsErrorResultWhenPasswordCancelled;
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
	OperationResult: Integer;
begin
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := False;
	AccountsManager.FAccountSettings.Password := '';

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
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create);
	try
		Cloud := Manager.Get('test_connection', OperationResult);
		Assert.IsNull(Cloud, 'Cloud should be nil when password retrieval fails');
		Assert.AreNotEqual(CLOUD_OPERATION_OK, OperationResult,
			'OperationResult should not be OK when password retrieval fails');
	finally
		Manager.Destroy;
	end;
end;

procedure TConnectionManagerGetTest.TestGetReturnsNilConsistently;
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
	Result1, Result2: Integer;
begin
	AccountsManager := TMockAccountsManager.Create;
	AccountsManager.FAccountSettings.PublicAccount := False;
	AccountsManager.FAccountSettings.Password := '';

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
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create);
	try
		Cloud1 := Manager.Get('test_connection', Result1);
		Cloud2 := Manager.Get('test_connection', Result2);

		Assert.IsNull(Cloud1, 'First Get should return nil');
		Assert.IsNull(Cloud2, 'Second Get should also return nil');
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
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create);
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
		PasswordUI, CipherValidator, FileSystem, Progress, Logger, Request, PasswordManager, TNullTCHandler.Create, TNullAuthStrategyFactory.Create);
	try
		Manager.Free('connection1');
		Manager.Free('connection1');
		Manager.Free('connection2');
		Assert.Pass('Multiple Free calls should not throw');
	finally
		Manager.Destroy;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TConnectionManagerConstructorTest);
TDUnitX.RegisterTestFixture(TConnectionManagerGetTest);
TDUnitX.RegisterTestFixture(TConnectionManagerFreeTest);

end.
