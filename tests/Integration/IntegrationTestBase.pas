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
	AccountSettings,
	ConnectionSettings,
	CloudConstants,
	CloudDirItemList,
	IntegrationTestConfig,
	TestDataGenerator,
	AuthStrategy,
	OAuthAppAuthStrategy,
	WindowsFileSystem,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	FileCipher,
	SettingsConstants;

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
			@param EncryptFilenames If True, enables filename encryption (requires Encrypted=True)
			@return Configured TCloudMailRu instance (caller owns it)}
		function CreatePrimaryCloud(Encrypted: Boolean = False; EncryptFilenames: Boolean = False): TCloudMailRu;

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

		{Create cloud settings for a given account.
			@param Email Account email
			@param Password Account password
			@param UseAppPassword True if using app password (OAuth)
			@param Encrypted Enable encryption
			@param EncryptFilenames Enable filename encryption
			@return Configured TCloudSettings record}
		function CreateCloudSettings(const Email, Password: WideString; UseAppPassword: Boolean;
			Encrypted: Boolean = False; EncryptFilenames: Boolean = False): TCloudSettings;

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
	Assert.IsTrue(FPrimaryCloud.Login, 'Primary account login failed');

	{Create test run folder in cloud}
	if not FPrimaryCloud.FileOps.CreateDirectory(FTestRunFolder) then
		Assert.Fail('Failed to create test run folder: ' + FTestRunFolder);
end;

procedure TIntegrationTestBase.TearDownFixture;
begin
	{Cleanup test run folder if configured}
	if FConfig.CleanupAfterTests and Assigned(FPrimaryCloud) then
	begin
		{Delete the entire test run folder}
		FPrimaryCloud.FileOps.RemoveDirectory(FTestRunFolder);
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

function TIntegrationTestBase.CreateCloudSettings(const Email, Password: WideString; UseAppPassword: Boolean;
	Encrypted: Boolean; EncryptFilenames: Boolean): TCloudSettings;
begin
	Result := Default(TCloudSettings);

	{Account settings}
	Result.AccountSettings.Email := Email;
	Result.AccountSettings.Password := Password;
	Result.AccountSettings.UseAppPassword := UseAppPassword;
	if UseAppPassword then
		Result.AccountSettings.AuthMethod := CLOUD_AUTH_METHOD_OAUTH_APP
	else
		Result.AccountSettings.AuthMethod := CLOUD_AUTH_METHOD_API;
	Result.AccountSettings.PublicAccount := False;

	{Encryption settings}
	if Encrypted and FConfig.HasEncryptionConfig then
	begin
		Result.AccountSettings.EncryptFilesMode := EncryptModeAlways;
		Result.AccountSettings.EncryptFileNames := EncryptFilenames;
		Result.CryptFilesPassword := FConfig.EncryptionPassword;
	end
	else
	begin
		Result.AccountSettings.EncryptFilesMode := EncryptModeNone;
		Result.AccountSettings.EncryptFileNames := False;
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
		Result.CloudMaxFileSize := FConfig.CloudMaxFileSizeOverride;
end;

function TIntegrationTestBase.CreatePrimaryCloud(Encrypted: Boolean; EncryptFilenames: Boolean): TCloudMailRu;
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
		EncryptFilenames);

	if FConfig.PrimaryUseAppPassword then
		AuthStrategy := TOAuthAppAuthStrategy.Create
	else
		AuthStrategy := TNullAuthStrategy.Create;

	if Encrypted and FConfig.HasEncryptionConfig then
		Cipher := TFileCipher.Create(FConfig.EncryptionPassword)
	else
		Cipher := nil;

	Result := TCloudMailRu.Create(
		Settings,
		nil, {No connection manager - use atomic connections}
		AuthStrategy,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		Cipher);
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
		False);

	if FConfig.SecondaryUseAppPassword then
		AuthStrategy := TOAuthAppAuthStrategy.Create
	else
		AuthStrategy := TNullAuthStrategy.Create;

	Result := TCloudMailRu.Create(
		Settings,
		nil,
		AuthStrategy,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		nil);
end;

function TIntegrationTestBase.CreatePublicCloud: TCloudMailRu;
var
	Settings: TCloudSettings;
begin
	Assert.IsTrue(FConfig.HasPublicUrl, 'Public URL not configured');

	Settings := Default(TCloudSettings);
	Settings.AccountSettings.PublicAccount := True;
	Settings.AccountSettings.PublicUrl := FConfig.PublicUrl;

	Result := TCloudMailRu.Create(
		Settings,
		nil,
		TNullAuthStrategy.Create,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		nil);
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
	if Cloud.FileOps.Delete(Path) then
		Exit(True);

	{If file delete failed, try directory delete}
	Result := Cloud.FileOps.RemoveDirectory(Path);
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
