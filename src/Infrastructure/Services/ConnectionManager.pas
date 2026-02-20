unit ConnectionManager;

{Connection pool manager for cloud connections by account name.
	Creates connections on first access, returns existing connections on subsequent calls.
	Combines interface definition with implementation.}

interface

uses
	CloudMailRu,
	CloudConstants,
	CloudEndpoints,
	LanguageStrings,
	TCHandler,
	Logger,
	Progress,
	Request,
	AccountsManager,
	PluginSettingsManager,
	FileSystem,
	Windows,
	Vcl.Controls,
	SettingsConstants,
	WFXTypes,
	AccountSettings,
	CloudSettings,
	HTTPManager,
	CipherProfile,
	Cipher,
	AuthStrategy,
	CookiePersistence,
	OpenSSLProvider,
	AccountCredentialsProvider,
	ServerProfileManager,
	FileEncryptionResolver,
	ProxyPasswordResolver,
	System.Generics.Collections,
	SysUtils;

type
	IConnectionManager = interface
		['{DE5905AB-A410-42C0-A517-C30EC573D7FD}']

		{Returns cloud connection for account, creating if needed.
			Always returns a valid instance (never nil).
			Authorization happens when caller invokes Cloud.Authorize().
			@param ConnectionName Account name to get connection for
			@return TCloudMailRu instance}
		function Get(ConnectionName: WideString): TCloudMailRu;

		{Releases connection for account if it exists.
			@param ConnectionName Account name to release connection for}
		procedure Free(ConnectionName: WideString);

		{Marks all current connections as stale.
			On next Get(), stale connections are destroyed and recreated with fresh settings.
			Active background operations continue safely with their existing instance references.}
		procedure InvalidateAll;
	end;

	TConnectionManager = class(TInterfacedObject, IConnectionManager)
	private
		FConnections: TDictionary<WideString, TCloudMailRu>; {It is better to encapsulate the dictionary}
		FStaleConnections: TList<WideString>; {Connection names marked for recreation on next Get()}
		FHTTPManager: IHTTPManager;
		FPluginSettingsManager: IPluginSettingsManager;
		FAccountsManager: IAccountsManager;
		FServerProfileManager: IServerProfileManager;
		FFileEncryptionResolver: IFileEncryptionResolver;
		FProxyPasswordResolver: IProxyPasswordResolver;
		FFileSystem: IFileSystem;
		FTCHandler: ITCHandler;
		FAuthStrategyFactory: IAuthStrategyFactory;
		FOpenSSLProvider: IOpenSSLProvider;
		FAccountCredentialsProvider: IAccountCredentialsProvider;

		FLogger: ILogger;
		FProgress: IProgress;
		FRequest: IRequest;

		function Init(ConnectionName: WideString): TCloudMailRu; {Create a connection by its name}
	public
		constructor Create(PluginSettingsManager: IPluginSettingsManager; AccountsManager: IAccountsManager; HTTPManager: IHTTPManager; FileEncryptionResolver: IFileEncryptionResolver; ProxyPasswordResolver: IProxyPasswordResolver; FileSystem: IFileSystem; Progress: IProgress; Logger: ILogger; Request: IRequest; TCHandler: ITCHandler; AuthStrategyFactory: IAuthStrategyFactory; OpenSSLProvider: IOpenSSLProvider; AccountCredentialsProvider: IAccountCredentialsProvider; ServerProfileManager: IServerProfileManager);
		destructor Destroy(); override;
		function Get(ConnectionName: WideString): TCloudMailRu; {Return the cloud connection by its name, always returns a valid instance}
		procedure Free(ConnectionName: WideString); {Free a connection by its name, if present}
		procedure InvalidateAll; {Marks all current connections as stale}
	end;

implementation

{TConnectionManager}
constructor TConnectionManager.Create(PluginSettingsManager: IPluginSettingsManager; AccountsManager: IAccountsManager; HTTPManager: IHTTPManager; FileEncryptionResolver: IFileEncryptionResolver; ProxyPasswordResolver: IProxyPasswordResolver; FileSystem: IFileSystem; Progress: IProgress; Logger: ILogger; Request: IRequest; TCHandler: ITCHandler; AuthStrategyFactory: IAuthStrategyFactory; OpenSSLProvider: IOpenSSLProvider; AccountCredentialsProvider: IAccountCredentialsProvider; ServerProfileManager: IServerProfileManager);
begin
	FConnections := TDictionary<WideString, TCloudMailRu>.Create;
	FStaleConnections := TList<WideString>.Create;
	FPluginSettingsManager := PluginSettingsManager;
	FAccountsManager := AccountsManager;
	FServerProfileManager := ServerProfileManager;
	FHTTPManager := HTTPManager;
	FFileEncryptionResolver := FileEncryptionResolver;
	FProxyPasswordResolver := ProxyPasswordResolver;
	FFileSystem := FileSystem;
	FProgress := Progress;
	FLogger := Logger;
	FRequest := Request;
	FTCHandler := TCHandler;
	FAuthStrategyFactory := AuthStrategyFactory;
	FOpenSSLProvider := OpenSSLProvider;
	FAccountCredentialsProvider := AccountCredentialsProvider;
end;

destructor TConnectionManager.Destroy;
var
	Item: TPair<WideString, TCloudMailRu>;
begin
	for Item in FConnections do
		Item.Value.Destroy;

	FreeAndNil(FConnections);
	FreeAndNil(FStaleConnections);

	{Release interface references}
	FHTTPManager := nil;
	FPluginSettingsManager := nil;
	FAccountsManager := nil;
	FServerProfileManager := nil;
	FFileEncryptionResolver := nil;
	FProxyPasswordResolver := nil;
	FAuthStrategyFactory := nil;
	FOpenSSLProvider := nil;
	FAccountCredentialsProvider := nil;

	inherited;
end;

procedure TConnectionManager.Free(ConnectionName: WideString);
begin
	if FConnections.ContainsKey(ConnectionName) then
	begin
		FConnections.Items[ConnectionName].Free;
		FConnections.Remove(ConnectionName);
	end;
	FStaleConnections.Remove(ConnectionName);
end;

function TConnectionManager.Get(ConnectionName: WideString): TCloudMailRu;
begin
	{Destroy stale connection so it gets recreated with fresh settings}
	if FStaleConnections.Contains(ConnectionName) then
	begin
		Free(ConnectionName); {Also removes from FStaleConnections}
	end;

	if not FConnections.TryGetValue(ConnectionName, Result) then
	begin
		Result := Init(ConnectionName);
		FConnections.AddOrSetValue(ConnectionName, Result);
	end;
end;

procedure TConnectionManager.InvalidateAll;
var
	Key: WideString;
begin
	for Key in FConnections.Keys do
		if not FStaleConnections.Contains(Key) then
			FStaleConnections.Add(Key);
end;

function TConnectionManager.Init(ConnectionName: WideString): TCloudMailRu;
var
	CloudSettings: TCloudSettings;
	AuthStrategy: IAuthStrategy;
	Cipher: ICipher;
	AccountSettingsData: TAccountSettings;
	Endpoints: TCloudEndpoints;
begin
	{Resolve endpoints from account's server profile before creating CloudSettings}
	AccountSettingsData := FAccountsManager.GetAccountSettings(ConnectionName);
	Endpoints := FServerProfileManager.ResolveEndpoints(AccountSettingsData.Server);

	{Create CloudSettings using factory method - combines plugin settings, account settings and endpoints}
	CloudSettings := TCloudSettings.CreateFromSettings(FPluginSettingsManager.GetSettings, AccountSettingsData, Endpoints);

	{Compute cookie file path for VK ID session persistence}
	if AccountSettingsData.PersistCookies and (AccountSettingsData.AuthMethod = CLOUD_AUTH_METHOD_VKID) then
		CloudSettings.CookieFilePath := TCookiePersistence.BuildFilePath(
			ExtractFilePath(FPluginSettingsManager.GetAccountsIniFilePath), AccountSettingsData.Account)
	else
		CloudSettings.CookieFilePath := '';

	{For non-public accounts, resolve proxy password. Account password is retrieved by TCloudMailRu.Authorize()}
	if not CloudSettings.AccountSettings.PublicAccount then
		FProxyPasswordResolver.ResolvePassword;

	FLogger.Log(LOG_LEVEL_CONNECT, MSGTYPE_CONNECT, 'CONNECT \%s', [ConnectionName]);

	{Resolve cipher - returns real cipher or null cipher depending on encryption settings}
	Cipher := FFileEncryptionResolver.ResolveCipher(ConnectionName, CloudSettings);

	{Create appropriate auth strategy via factory - dispatches by account's auth method}
	AuthStrategy := FAuthStrategyFactory.CreateStrategy(AccountSettingsData.AuthMethod);

	{Create cloud instance - authorization happens later via Cloud.Authorize()}
	Result := TCloudMailRu.Create(CloudSettings, FHTTPManager, function: TThreadID begin Result := GetCurrentThreadID; end, AuthStrategy, FFileSystem, FLogger, FProgress, FRequest, FTCHandler, Cipher, FOpenSSLProvider, FAccountCredentialsProvider);
end;

end.
