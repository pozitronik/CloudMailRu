unit IntegrationTestConfig;

{Configuration loader for integration tests.
	Reads credentials and settings from IntegrationTest.ini file.
	Integration tests are completely disabled when configuration is missing or invalid.}

interface

uses
	System.SysUtils,
	System.Classes,
	System.IniFiles,
	System.IOUtils,
	MockServerManager;

type
	{Configuration record for integration test settings}
	TIntegrationTestConfig = class
	private
		FEnabled: Boolean;
		FTestFolder: WideString;
		FCleanupAfterTests: Boolean;

		{Primary account credentials}
		FPrimaryEmail: WideString;
		FPrimaryPassword: WideString;
		FPrimaryUseAppPassword: Boolean;

		{Secondary account credentials}
		FSecondaryEmail: WideString;
		FSecondaryPassword: WideString;
		FSecondaryUseAppPassword: Boolean;

		{Public account settings}
		FPublicUrl: WideString;

		{Encryption settings}
		FEncryptionPassword: WideString;
		FTestEncryptedFilenames: Boolean;

		{Chunked upload settings}
		FCloudMaxFileSizeOverride: Int64;
		FTestChunkedFileSize: Int64;

		{Custom server settings}
		FServerUrl: WideString;
		FSecondaryServerUrl: WideString;

		{Mock server settings}
		FTuchaPath: WideString;

		class var FInstance: TIntegrationTestConfig;
		class var FConfigLoaded: Boolean;
		class var FConfigValid: Boolean;
		class var FSkipReason: WideString;

		class function GetConfigFilePath: WideString; static;
		class procedure EnsureLoaded; static;
		function Validate: Boolean;
	public
		destructor Destroy; override;

		{Load configuration from INI file. Returns nil if config is invalid or missing.}
		class function Load: TIntegrationTestConfig; static;

		{Check if integration tests should run (config exists, enabled, and valid)}
		class function IsEnabled: Boolean; static;

		{Get the reason why integration tests are skipped (empty if enabled)}
		class function GetSkipReason: WideString; static;

		{Output warning to console if integration tests are skipped}
		class procedure WarnIfSkipped; static;

		{Get the singleton instance (only valid if IsEnabled returns True)}
		class function Instance: TIntegrationTestConfig; static;

		{Configuration properties}
		property Enabled: Boolean read FEnabled;
		property TestFolder: WideString read FTestFolder;
		property CleanupAfterTests: Boolean read FCleanupAfterTests;

		property PrimaryEmail: WideString read FPrimaryEmail;
		property PrimaryPassword: WideString read FPrimaryPassword;
		property PrimaryUseAppPassword: Boolean read FPrimaryUseAppPassword;

		property SecondaryEmail: WideString read FSecondaryEmail;
		property SecondaryPassword: WideString read FSecondaryPassword;
		property SecondaryUseAppPassword: Boolean read FSecondaryUseAppPassword;

		property PublicUrl: WideString read FPublicUrl;

		property EncryptionPassword: WideString read FEncryptionPassword;
		property TestEncryptedFilenames: Boolean read FTestEncryptedFilenames;

		property CloudMaxFileSizeOverride: Int64 read FCloudMaxFileSizeOverride;
		property TestChunkedFileSize: Int64 read FTestChunkedFileSize;

		property ServerUrl: WideString read FServerUrl;
		property SecondaryServerUrl: WideString read FSecondaryServerUrl;

		property TuchaPath: WideString read FTuchaPath;

		{Check if secondary account is configured}
		function HasSecondaryAccount: Boolean;

		{Check if public URL is configured}
		function HasPublicUrl: Boolean;

		{Check if encryption testing is configured}
		function HasEncryptionConfig: Boolean;

		{Check if a custom server URL is configured for the primary account}
		function HasCustomServer: Boolean;

		{Check if a custom server URL is configured specifically for the secondary account}
		function HasSecondaryServer: Boolean;

		{Check if mock server (tucha) path is configured for automatic server management}
		function HasMockServer: Boolean;

		{Get the effective server URL for the secondary account.
			Falls back to primary server URL when secondary is not explicitly configured.}
		function GetEffectiveSecondaryServerUrl: WideString;
	end;

const
	{Default values}
	DEFAULT_TEST_FOLDER = '/IntegrationTests';
	DEFAULT_CLOUD_MAX_FILE_SIZE_OVERRIDE = 1048576;  {1MB - triggers chunking for small files}
	DEFAULT_TEST_CHUNKED_FILE_SIZE = 3145728;  {3MB - will create 3 chunks with 1MB limit}

implementation

{TIntegrationTestConfig}

destructor TIntegrationTestConfig.Destroy;
begin
	inherited;
end;

class function TIntegrationTestConfig.GetConfigFilePath: WideString;
begin
	{Config file is in tests/data/ directory, same level as other test data}
	Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\data\IntegrationTest.ini');
	Result := TPath.GetFullPath(Result);
end;

{Helper to read boolean from INI - TIniFile.ReadBool only accepts 1/0, not true/false}
function ReadIniBool(IniFile: TIniFile; const Section, Key: string; Default: Boolean): Boolean;
var
	Value: string;
begin
	Value := IniFile.ReadString(Section, Key, '');
	if Value = '' then
		Result := Default
	else
		Result := SameText(Value, 'true') or SameText(Value, '1') or SameText(Value, 'yes');
end;

class procedure TIntegrationTestConfig.EnsureLoaded;
var
	ConfigPath: WideString;
	IniFile: TIniFile;
begin
	if FConfigLoaded then
		Exit;

	FConfigLoaded := True;
	FConfigValid := False;
	FSkipReason := '';

	ConfigPath := GetConfigFilePath;
	if not TFile.Exists(ConfigPath) then
	begin
		FSkipReason := 'Config file not found: ' + ConfigPath;
		Exit;
	end;

	FInstance := TIntegrationTestConfig.Create;

	IniFile := TIniFile.Create(ConfigPath);
	try
		{Main settings}
		FInstance.FEnabled := ReadIniBool(IniFile, 'IntegrationTests', 'Enabled', False);
		FInstance.FTestFolder := IniFile.ReadString('IntegrationTests', 'TestFolder', DEFAULT_TEST_FOLDER);
		FInstance.FCleanupAfterTests := ReadIniBool(IniFile, 'IntegrationTests', 'CleanupAfterTests', True);

		{Primary account}
		FInstance.FPrimaryEmail := IniFile.ReadString('PrimaryAccount', 'Email', '');
		FInstance.FPrimaryPassword := IniFile.ReadString('PrimaryAccount', 'AppPassword', '');
		FInstance.FPrimaryUseAppPassword := ReadIniBool(IniFile, 'PrimaryAccount', 'UseAppPassword', True);

		{Secondary account}
		FInstance.FSecondaryEmail := IniFile.ReadString('SecondaryAccount', 'Email', '');
		FInstance.FSecondaryPassword := IniFile.ReadString('SecondaryAccount', 'AppPassword', '');
		FInstance.FSecondaryUseAppPassword := ReadIniBool(IniFile, 'SecondaryAccount', 'UseAppPassword', True);

		{Public account}
		FInstance.FPublicUrl := IniFile.ReadString('PublicAccount', 'PublicUrl', '');

		{Encryption}
		FInstance.FEncryptionPassword := IniFile.ReadString('Encryption', 'TestPassword', '');
		FInstance.FTestEncryptedFilenames := ReadIniBool(IniFile, 'Encryption', 'TestEncryptedFilenames', False);

		{Chunked upload}
		FInstance.FCloudMaxFileSizeOverride := IniFile.ReadInteger('ChunkedUpload', 'CloudMaxFileSizeOverride', DEFAULT_CLOUD_MAX_FILE_SIZE_OVERRIDE);
		FInstance.FTestChunkedFileSize := IniFile.ReadInteger('ChunkedUpload', 'TestChunkedFileSize', DEFAULT_TEST_CHUNKED_FILE_SIZE);

		{Custom server}
		FInstance.FServerUrl := IniFile.ReadString('Server', 'ServerUrl', '');
		FInstance.FSecondaryServerUrl := IniFile.ReadString('SecondaryServer', 'ServerUrl', '');

		{Mock server}
		FInstance.FTuchaPath := IniFile.ReadString('MockServer', 'TuchaPath', '');

		{When mock server is configured, auto-fill defaults for fields left empty.
			This allows minimal INI: just Enabled=true and TuchaPath=<path>.}
		if FInstance.HasMockServer then
		begin
			if FInstance.FServerUrl = '' then
				FInstance.FServerUrl := DEFAULT_MOCK_SERVER_URL;
			if FInstance.FSecondaryServerUrl = '' then
				FInstance.FSecondaryServerUrl := DEFAULT_MOCK_SERVER_URL;
			if FInstance.FPrimaryEmail = '' then
				FInstance.FPrimaryEmail := DEFAULT_MOCK_PRIMARY_EMAIL;
			if FInstance.FPrimaryPassword = '' then
				FInstance.FPrimaryPassword := DEFAULT_MOCK_PRIMARY_PASSWORD;
			FInstance.FPrimaryUseAppPassword := True;
			if FInstance.FSecondaryEmail = '' then
				FInstance.FSecondaryEmail := DEFAULT_MOCK_SECONDARY_EMAIL;
			if FInstance.FSecondaryPassword = '' then
				FInstance.FSecondaryPassword := DEFAULT_MOCK_SECONDARY_PASSWORD;
			FInstance.FSecondaryUseAppPassword := True;
		end;

		FConfigValid := FInstance.Validate;

		if not FInstance.FEnabled then
			FSkipReason := 'Integration tests disabled in config (Enabled=false)'
		else if not FConfigValid then
			FSkipReason := 'Invalid config: primary account credentials required (set manually or via TuchaPath for mock server)';
	finally
		IniFile.Free;
	end;
end;

function TIntegrationTestConfig.Validate: Boolean;
begin
	{Minimal validation: enabled flag and primary account credentials}
	Result := FEnabled
		and (FPrimaryEmail <> '')
		and (FPrimaryPassword <> '');
end;

class function TIntegrationTestConfig.Load: TIntegrationTestConfig;
begin
	EnsureLoaded;
	if FConfigValid then
		Result := FInstance
	else
		Result := nil;
end;

class function TIntegrationTestConfig.IsEnabled: Boolean;
begin
	EnsureLoaded;
	Result := FConfigValid and Assigned(FInstance) and FInstance.FEnabled;
end;

class function TIntegrationTestConfig.GetSkipReason: WideString;
begin
	EnsureLoaded;
	Result := FSkipReason;
end;

class procedure TIntegrationTestConfig.WarnIfSkipped;
begin
	EnsureLoaded;
	if not IsEnabled then
	begin
		WriteLn('[Integration Tests] Integration tests skipped');
		if FSkipReason <> '' then
			WriteLn('[Integration Tests] Reason: ', FSkipReason);
	end;
end;

class function TIntegrationTestConfig.Instance: TIntegrationTestConfig;
begin
	EnsureLoaded;
	Result := FInstance;
end;

function TIntegrationTestConfig.HasSecondaryAccount: Boolean;
begin
	Result := (FSecondaryEmail <> '') and (FSecondaryPassword <> '');
end;

function TIntegrationTestConfig.HasPublicUrl: Boolean;
begin
	Result := FPublicUrl <> '';
end;

function TIntegrationTestConfig.HasEncryptionConfig: Boolean;
begin
	Result := FEncryptionPassword <> '';
end;

function TIntegrationTestConfig.HasCustomServer: Boolean;
begin
	Result := FServerUrl <> '';
end;

function TIntegrationTestConfig.HasSecondaryServer: Boolean;
begin
	Result := FSecondaryServerUrl <> '';
end;

function TIntegrationTestConfig.HasMockServer: Boolean;
begin
	Result := FTuchaPath <> '';
end;

function TIntegrationTestConfig.GetEffectiveSecondaryServerUrl: WideString;
begin
	if FSecondaryServerUrl <> '' then
		Result := FSecondaryServerUrl
	else
		Result := FServerUrl;
end;

initialization
	{Output warning if integration tests are skipped - this runs early during test startup}
	TIntegrationTestConfig.WarnIfSkipped;

finalization
	if Assigned(TIntegrationTestConfig.FInstance) then
		TIntegrationTestConfig.FInstance.Free;

end.
