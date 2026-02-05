unit IntegrationTestBase;

{Base class for all integration tests.
	Provides cloud instance management, test isolation, and automatic cleanup.
	Tests inherit from this class to get consistent setup/teardown behavior.}

interface

uses
	System.SysUtils,
	System.Classes,
	System.Generics.Collections,
	DUnitX.TestFramework,
	CloudMailRu,
	CloudSettings,
	CloudEndpoints,
	AccountSettings,
	ConnectionSettings,
	CloudConstants,
	CloudDirItemList,
	ServerProfileManager,
	ServerConfigFetcher,
	IntegrationTestConfig,
	TestDataGenerator,
	TestHelper,
	AuthStrategy,
	OAuthAppAuthStrategy,
	FileSystem,
	HTTPManager,
	SSLHandlerFactory,
	IndySSLHandlerFactory,
	Logger,
	Progress,
	Request,
	TCHandler,
	Cipher,
	DCPrijndael,
	DCPsha1,
	SettingsConstants,
	OpenSSLProvider,
	AccountCredentialsProvider;

type
	{Base class for integration tests with cloud setup/teardown}
	TIntegrationTestBase = class
	protected
		FConfig: TIntegrationTestConfig;
		FPrimaryCloud: TCloudMailRu;
		FSecondaryCloud: TCloudMailRu;
		FPublicCloud: TCloudMailRu;
		FTestRunFolder: WideString;
		FCreatedPaths: TStringList;

		{Create a cloud instance for the primary account.
			@param Encrypted If True, enables file encryption
			@return Configured TCloudMailRu instance (caller owns it)}
		function CreatePrimaryCloud(Encrypted: Boolean = False): TCloudMailRu;

		{Create a cloud instance for the secondary account.
			@return Configured TCloudMailRu instance (caller owns it)
			@raises EAssertionFailed if secondary account not configured}
		function CreateSecondaryCloud: TCloudMailRu;

		{Create a cloud instance for public account access.
			@return Configured TCloudMailRu instance (caller owns it)
			@raises EAssertionFailed if public URL not configured}
		function CreatePublicCloud: TCloudMailRu;

		{Generate a unique cloud path within the test run folder.
			@param BaseName Base name for the path (will be made unique)
			@return Full cloud path like '/IntegrationTests/Run_xxx/BaseName_yyy'}
		function UniqueCloudPath(const BaseName: WideString): WideString;

		{Track a cloud path for automatic cleanup in TearDown.
			@param CloudPath Path to track for deletion}
		procedure TrackForCleanup(const CloudPath: WideString);

		{Clean up all tracked paths. Called automatically in TearDown.}
		procedure CleanupTrackedPaths;

		{Delete a cloud item (file or folder) with retry.
			@param Cloud Cloud instance to use
			@param Path Path to delete
			@return True if deleted or not found, False on error}
		function SafeDeleteCloudItem(Cloud: TCloudMailRu; const Path: WideString): Boolean;

		{Resolve cloud endpoints from a server URL.
			When ServerUrl is empty, returns default cloud.mail.ru endpoints.
			@param ServerUrl Custom server base URL (may be empty)
			@return Resolved TCloudEndpoints record}
		function ResolveEndpoints(const ServerUrl: WideString): TCloudEndpoints;

		{Create cloud settings for a given account.
			@param Email Account email
			@param Password Account password
			@param UseAppPassword True if using app password (OAuth)
			@param Encrypted Enable encryption
			@param ServerUrl Custom server URL for endpoint resolution (empty = defaults)
			@return Configured TCloudSettings record}
		function CreateCloudSettings(const Email, Password: WideString; UseAppPassword: Boolean;
			Encrypted: Boolean = False; const ServerUrl: WideString = ''): TCloudSettings;

		{Check if test should be skipped due to missing configuration.
			Use at the start of tests that require specific config.}
		procedure RequireSecondaryAccount;
		procedure RequirePublicUrl;
		procedure RequireEncryption;
	public
		{Setup runs once before all tests in the fixture}
		[SetupFixture]
		procedure SetupFixture;

		{TearDown runs once after all tests in the fixture}
		[TearDownFixture]
		procedure TearDownFixture;

		{Setup runs before each individual test}
		[Setup]
		procedure Setup;

		{TearDown runs after each individual test}
		[TearDown]
		procedure TearDown;
	end;

implementation

uses
	WFXTypes,
	IdSSLOpenSSLHeaders;

{TIntegrationTestBase}

procedure TIntegrationTestBase.SetupFixture;
var
	ExePath, SSLPath: string;
begin
	{Set up SSL DLL path - tests run from Win64/Debug, DLLs are in Win64/Debug/x64}
	ExePath := ExtractFilePath(ParamStr(0));
	SSLPath := ExePath + 'x64';
	if DirectoryExists(SSLPath) then
		IdOpenSSLSetLibPath(SSLPath)
	else
		WriteLn('[IntegrationTest] WARNING: SSL path not found: ' + SSLPath);

	FConfig := TIntegrationTestConfig.Instance;
	Assert.IsNotNull(FConfig, 'Integration test config not loaded');


	{Create unique test run folder}
	FTestRunFolder := FConfig.TestFolder + '/' + TTestDataGenerator.GenerateUniqueFolderName('Run');

	FCreatedPaths := TStringList.Create;
	FCreatedPaths.Duplicates := dupIgnore;
	FCreatedPaths.Sorted := True;

	{Create primary cloud and ensure test folder exists}
	FPrimaryCloud := CreatePrimaryCloud;
	if not FPrimaryCloud.Login then
	begin
		Assert.Fail('Primary account login failed');
		Exit;
	end;

	{Create test run folder in cloud}
	if not FPrimaryCloud.FileOperations.CreateDirectory(FTestRunFolder) then
	begin
		Assert.Fail('Failed to create test run folder: ' + FTestRunFolder);
		Exit;
	end;
end;

procedure TIntegrationTestBase.TearDownFixture;
begin
	{Cleanup test run folder if configured}
	if FConfig.CleanupAfterTests and Assigned(FPrimaryCloud) then
	begin
		{Delete the entire test run folder}
		FPrimaryCloud.FileOperations.RemoveDirectory(FTestRunFolder);
	end;

	FreeAndNil(FPrimaryCloud);
	FreeAndNil(FSecondaryCloud);
	FreeAndNil(FPublicCloud);
	FreeAndNil(FCreatedPaths);
end;

procedure TIntegrationTestBase.Setup;
begin
	{Each test starts with a clean tracking list}
	FCreatedPaths.Clear;
end;

procedure TIntegrationTestBase.TearDown;
begin
	{Clean up any paths created during this test}
	CleanupTrackedPaths;
end;

function TIntegrationTestBase.ResolveEndpoints(const ServerUrl: WideString): TCloudEndpoints;
var
	Fetcher: TServerConfigFetcher;
	ErrorMsg: WideString;
begin
	if ServerUrl <> '' then
	begin
		Fetcher := TServerConfigFetcher.Create;
		try
			if not Fetcher.Fetch(ServerUrl, Result, ErrorMsg) then
				Assert.Fail('Failed to resolve endpoints from ' + ServerUrl + ': ' + ErrorMsg);
		finally
			Fetcher.Free;
		end;
	end
	else
		Result := TCloudEndpoints.CreateDefaults;
end;

function TIntegrationTestBase.CreateCloudSettings(const Email, Password: WideString; UseAppPassword: Boolean;
	Encrypted: Boolean; const ServerUrl: WideString): TCloudSettings;
begin
	Result := Default(TCloudSettings);
	Result.Endpoints := ResolveEndpoints(ServerUrl);

	{Account settings}
	Result.AccountSettings.Email := Email;
	Result.AccountSettings.Password := Password;
	Result.AccountSettings.UseAppPassword := UseAppPassword;
	Result.AccountSettings.AuthMethod := CLOUD_AUTH_METHOD_OAUTH_APP;
	Result.AccountSettings.PublicAccount := False;

	{Encryption settings}
	if Encrypted and FConfig.HasEncryptionConfig then
	begin
		Result.AccountSettings.EncryptFilesMode := EncryptModeAlways;
		Result.CryptFilesPassword := FConfig.EncryptionPassword;
	end
	else
	begin
		Result.AccountSettings.EncryptFilesMode := EncryptModeNone;
	end;

	{Connection settings - use reasonable defaults}
	Result.ConnectionSettings.SocketTimeout := 30000;
	Result.ConnectionSettings.ProxySettings.ProxyType := ProxyNone;
	Result.ConnectionSettings.UserAgent := DEFAULT_USERAGENT;

	{Cloud operation settings}
	Result.PrecalculateHash := True;
	Result.ForcePrecalculateSize := 0;
	Result.CheckCRC := True;
	Result.HashCalculatorStrategy := 0; {Auto}
	Result.CloudMaxFileSize := CLOUD_MAX_FILESIZE_DEFAULT;
	Result.OperationErrorMode := 0; {Ask - but we'll handle errors in tests}
	Result.RetryAttempts := 3;
	Result.AttemptWait := 1000;

	{Override CloudMaxFileSize for chunked upload testing if configured}
	if FConfig.CloudMaxFileSizeOverride > 0 then
	begin
		Result.CloudMaxFileSize := FConfig.CloudMaxFileSizeOverride;
		Result.AccountSettings.SplitLargeFiles := True; {Enable chunking when size override is set}
	end;
end;

function TIntegrationTestBase.CreatePrimaryCloud(Encrypted: Boolean): TCloudMailRu;
var
	Settings: TCloudSettings;
	AuthStrategy: IAuthStrategy;
	Cipher: ICipher;
begin
	Settings := CreateCloudSettings(
		FConfig.PrimaryEmail,
		FConfig.PrimaryPassword,
		FConfig.PrimaryUseAppPassword,
		Encrypted,
		FConfig.ServerUrl);

	if FConfig.PrimaryUseAppPassword then
		AuthStrategy := TOAuthAppAuthStrategy.Create
	else
		AuthStrategy := TNullAuthStrategy.Create;

	if Encrypted and FConfig.HasEncryptionConfig then
		Cipher := TFileCipher.Create(FConfig.EncryptionPassword, TDCP_rijndael, TDCP_sha1)
	else
		Cipher := TNullCipher.Create;

	var Logger: ILogger := TNullLogger.Create;
	var Progress: IProgress := TNullProgress.Create;
	Result := TCloudMailRu.Create(
		Settings,
		TSingleThreadHTTPManager.Create(Settings.ConnectionSettings, TIndySSLHandlerFactory.Create, Logger, Progress),
		TestThreadID(),
		AuthStrategy,
		TWindowsFileSystem.Create,
		Logger,
		Progress,
		TNullRequest.Create,
		TNullTCHandler.Create,
		Cipher,
		TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create);
end;

function TIntegrationTestBase.CreateSecondaryCloud: TCloudMailRu;
var
	Settings: TCloudSettings;
	AuthStrategy: IAuthStrategy;
begin
	Assert.IsTrue(FConfig.HasSecondaryAccount, 'Secondary account not configured');

	Settings := CreateCloudSettings(
		FConfig.SecondaryEmail,
		FConfig.SecondaryPassword,
		FConfig.SecondaryUseAppPassword,
		False,
		FConfig.GetEffectiveSecondaryServerUrl);

	if FConfig.SecondaryUseAppPassword then
		AuthStrategy := TOAuthAppAuthStrategy.Create
	else
		AuthStrategy := TNullAuthStrategy.Create;

	var Logger: ILogger := TNullLogger.Create;
	var Progress: IProgress := TNullProgress.Create;
	Result := TCloudMailRu.Create(
		Settings,
		TSingleThreadHTTPManager.Create(Settings.ConnectionSettings, TIndySSLHandlerFactory.Create, Logger, Progress),
		TestThreadID(),
		AuthStrategy,
		TWindowsFileSystem.Create,
		Logger,
		Progress,
		TNullRequest.Create,
		TNullTCHandler.Create,
		TNullCipher.Create,
		TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create);
end;

function TIntegrationTestBase.CreatePublicCloud: TCloudMailRu;
var
	Settings: TCloudSettings;
begin
	Assert.IsTrue(FConfig.HasPublicUrl, 'Public URL not configured');

	Settings := Default(TCloudSettings);
	Settings.Endpoints := ResolveEndpoints(FConfig.ServerUrl);
	Settings.AccountSettings.PublicAccount := True;
	Settings.AccountSettings.PublicUrl := FConfig.PublicUrl;

	var Logger: ILogger := TNullLogger.Create;
	var Progress: IProgress := TNullProgress.Create;
	Result := TCloudMailRu.Create(
		Settings,
		TSingleThreadHTTPManager.Create(Settings.ConnectionSettings, TIndySSLHandlerFactory.Create, Logger, Progress),
		TestThreadID(),
		TNullAuthStrategy.Create,
		TWindowsFileSystem.Create,
		Logger,
		Progress,
		TNullRequest.Create,
		TNullTCHandler.Create,
		TNullCipher.Create,
		TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create);
end;

function TIntegrationTestBase.UniqueCloudPath(const BaseName: WideString): WideString;
begin
	Result := FTestRunFolder + '/' + TTestDataGenerator.GenerateUniqueFilename(BaseName, '');
end;

procedure TIntegrationTestBase.TrackForCleanup(const CloudPath: WideString);
begin
	FCreatedPaths.Add(CloudPath);
end;

procedure TIntegrationTestBase.CleanupTrackedPaths;
var
	I: Integer;
begin
	if not Assigned(FPrimaryCloud) then
		Exit;

	{Delete in reverse order (files before their parent folders)}
	for I := FCreatedPaths.Count - 1 downto 0 do
		SafeDeleteCloudItem(FPrimaryCloud, FCreatedPaths[I]);

	FCreatedPaths.Clear;
end;

function TIntegrationTestBase.SafeDeleteCloudItem(Cloud: TCloudMailRu; const Path: WideString): Boolean;
begin
	{Try to delete as file first, then as directory}
	if Cloud.FileOperations.Delete(Path) then
		Exit(True);

	{If file delete failed, try directory delete}
	Result := Cloud.FileOperations.RemoveDirectory(Path);
end;

procedure TIntegrationTestBase.RequireSecondaryAccount;
begin
	if not FConfig.HasSecondaryAccount then
	begin
		Assert.Pass('SKIPPED: Secondary account not configured');
		Abort; {Prevent further execution - Assert.Pass doesn't stop execution}
	end;
end;

procedure TIntegrationTestBase.RequirePublicUrl;
begin
	if not FConfig.HasPublicUrl then
	begin
		Assert.Pass('SKIPPED: Public URL not configured');
		Abort;
	end;
end;

procedure TIntegrationTestBase.RequireEncryption;
begin
	if not FConfig.HasEncryptionConfig then
	begin
		Assert.Pass('SKIPPED: Encryption not configured');
		Abort;
	end;
end;

end.
