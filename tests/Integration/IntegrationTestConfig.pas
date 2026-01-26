unit IntegrationTestConfig;

{Configuration loader for integration tests.
	Reads credentials and settings from IntegrationTest.ini file.
	Integration tests are completely disabled when configuration is missing or invalid.}

interface

uses
	System.SysUtils,
	System.Classes,
	System.IniFiles,
	System.IOUtils;

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

		class var FInstance: TIntegrationTestConfig;
		class var FConfigLoaded: Boolean;
		class var FConfigValid: Boolean;

		class function GetConfigFilePath: WideString; static;
		class procedure EnsureLoaded; static;
		function Validate: Boolean;
	public
		destructor Destroy; override;

		{Load configuration from INI file. Returns nil if config is invalid or missing.}
		class function Load: TIntegrationTestConfig; static;

		{Check if integration tests should run (config exists, enabled, and valid)}
		class function IsEnabled: Boolean; static;

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

		{Check if secondary account is configured}
		function HasSecondaryAccount: Boolean;

		{Check if public URL is configured}
		function HasPublicUrl: Boolean;

		{Check if encryption testing is configured}
		function HasEncryptionConfig: Boolean;
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

class procedure TIntegrationTestConfig.EnsureLoaded;
var
	ConfigPath: WideString;
	IniFile: TIniFile;
begin
	if FConfigLoaded then
		Exit;

	FConfigLoaded := True;
	FConfigValid := False;

	ConfigPath := GetConfigFilePath;
	if not TFile.Exists(ConfigPath) then
		Exit;

	FInstance := TIntegrationTestConfig.Create;

	IniFile := TIniFile.Create(ConfigPath);
	try
		{Main settings}
		FInstance.FEnabled := IniFile.ReadBool('IntegrationTests', 'Enabled', False);
		FInstance.FTestFolder := IniFile.ReadString('IntegrationTests', 'TestFolder', DEFAULT_TEST_FOLDER);
		FInstance.FCleanupAfterTests := IniFile.ReadBool('IntegrationTests', 'CleanupAfterTests', True);

		{Primary account}
		FInstance.FPrimaryEmail := IniFile.ReadString('PrimaryAccount', 'Email', '');
		FInstance.FPrimaryPassword := IniFile.ReadString('PrimaryAccount', 'AppPassword', '');
		FInstance.FPrimaryUseAppPassword := IniFile.ReadBool('PrimaryAccount', 'UseAppPassword', True);

		{Secondary account}
		FInstance.FSecondaryEmail := IniFile.ReadString('SecondaryAccount', 'Email', '');
		FInstance.FSecondaryPassword := IniFile.ReadString('SecondaryAccount', 'AppPassword', '');
		FInstance.FSecondaryUseAppPassword := IniFile.ReadBool('SecondaryAccount', 'UseAppPassword', True);

		{Public account}
		FInstance.FPublicUrl := IniFile.ReadString('PublicAccount', 'PublicUrl', '');

		{Encryption}
		FInstance.FEncryptionPassword := IniFile.ReadString('Encryption', 'TestPassword', '');
		FInstance.FTestEncryptedFilenames := IniFile.ReadBool('Encryption', 'TestEncryptedFilenames', False);

		{Chunked upload}
		FInstance.FCloudMaxFileSizeOverride := IniFile.ReadInteger('ChunkedUpload', 'CloudMaxFileSizeOverride', DEFAULT_CLOUD_MAX_FILE_SIZE_OVERRIDE);
		FInstance.FTestChunkedFileSize := IniFile.ReadInteger('ChunkedUpload', 'TestChunkedFileSize', DEFAULT_TEST_CHUNKED_FILE_SIZE);

		FConfigValid := FInstance.Validate;
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
	Result := FConfigValid and FInstance.FEnabled;
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

initialization

finalization
	if Assigned(TIntegrationTestConfig.FInstance) then
		TIntegrationTestConfig.FInstance.Free;

end.
