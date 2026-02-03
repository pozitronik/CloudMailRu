unit PluginSettingsManagerTest;

interface

uses
	PluginSettings,
	PluginSettingsManager,
	SettingsConstants,
	StreamingSettings,
	CloudConstants,
	TestHelper,
	SysUtils,
	IOUtils,
	Classes,
	ConnectionSettings,
	ConfigFile,
	Environment,
	DUnitX.TestFramework;

type

	{Mock environment for testing path resolution logic}
	TMockEnvironment = class(TInterfacedObject, IEnvironment)
	private
		FAppData: WideString;
		FModulePath: WideString;
		FPluginConfigExists: Boolean;
		FDirectoryWriteable: Boolean;
		FDirectoryExists: Boolean;
		FCreatedDirectories: TStringList;
		FIniDirSetting: Integer;
	public
		constructor Create;
		destructor Destroy; override;

		function GetEnvironmentVariable(const Name: WideString): WideString;
		function GetModulePath: WideString;
		function FileExists(const Path: WideString): Boolean;
		function DirectoryExists(const Path: WideString): Boolean;
		function IsDirectoryWriteable(const Path: WideString): Boolean;
		procedure CreateDirectory(const Path: WideString);

		property AppData: WideString read FAppData write FAppData;
		property ModulePath: WideString read FModulePath write FModulePath;
		property PluginConfigExists: Boolean read FPluginConfigExists write FPluginConfigExists;
		property DirectoryWriteable: Boolean read FDirectoryWriteable write FDirectoryWriteable;
		property DirExists: Boolean read FDirectoryExists write FDirectoryExists;
		property CreatedDirectories: TStringList read FCreatedDirectories;
		property IniDirSetting: Integer read FIniDirSetting write FIniDirSetting;
	end;

	[TestFixture]
	TPluginSettingsManagerTest = class
		AppDir: WideString;
		AppDataSubDir: WideString;
	private const
		FP_SETTINGS_INI = 'Settings.ini';
		FP_SETTINGS_REDIRECT_INI = 'SettingsRedirect.ini';
	public
		[Setup]
		procedure Setup;

		[Test]
		procedure TestCreateFromKnownFile;

		[Test]
		procedure TestCreateDefaults;

		[Test]
		procedure TestCreateFindFile;
	end;

	[TestFixture]
	TPluginSettingsManagerSaveTest = class
	private
		FConfigFile: TMemoryConfigFile;
		FConfigFileRef: IConfigFile;
		FManager: TPluginSettingsManager;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure Test_Save_WritesAllSettings;

		[Test]
		procedure Test_Save_SkipsDefaultValues;

		[Test]
		procedure Test_SaveThenRefresh_RoundTrip;

		[Test]
		procedure Test_Save_WritesProxySettings;

		[Test]
		procedure Test_Save_WritesConnectionSettings;

		[Test]
		procedure Test_Save_WritesHashCalculatorStrategy;

		[Test]
		procedure Test_HashCalculatorStrategy_DefaultsToAuto;
	end;

	[TestFixture]
	TPluginSettingsManagerStreamingTest = class
	private
		FConfigFile: TMemoryConfigFile;
		FConfigFileRef: IConfigFile;
		FManager: TPluginSettingsManager;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure Test_GetStreamingSettings_WhenNoSection_ReturnsUnset;

		[Test]
		procedure Test_GetStreamingSettings_ReadsAllFields;

		[Test]
		procedure Test_SetStreamingSettings_WritesAllFields;

		[Test]
		procedure Test_SetStreamingSettings_EmptyExtension_DoesNotWrite;

		[Test]
		procedure Test_GetStreamingExtensionsList_ReturnsOnlyStreamingSections;

		[Test]
		procedure Test_GetStreamingExtensionsList_EmptyWhenNoSections;

		[Test]
		procedure Test_RemoveStreamingExtension_ErasesSection;

		[Test]
		procedure Test_StreamingSettings_RoundTrip;
	end;

	[TestFixture]
	TPluginSettingsManagerMiscTest = class
	private
		FConfigFile: TMemoryConfigFile;
		FConfigFileRef: IConfigFile;
		FManager: TPluginSettingsManager;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure Test_GetSettings_ReturnsCurrentSettings;

		[Test]
		procedure Test_GetAccountsIniFilePath_ReturnsCorrectPath;

		[Test]
		procedure Test_SwitchProxyPasswordStorage_EnablesTCManager;

		[Test]
		procedure Test_SwitchProxyPasswordStorage_DeletesPassword;

		[Test]
		procedure Test_IniFilePath_ReturnsConfigFilePath;
	end;

	[TestFixture]
	TPluginSettingsManagerEnvironmentTest = class
	private
		FMockEnv: TMockEnvironment;
		FMockEnvRef: IEnvironment;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure Test_Create_NoConfigFile_WriteableDir_UsesPluginDir;

		[Test]
		procedure Test_Create_NoConfigFile_NotWriteable_UsesAppData;

		[Test]
		procedure Test_Create_CreatesDirectoryIfNotExists;
	end;

	[TestFixture]
	TNullPluginSettingsManagerTest = class
	public
		[Test]
		procedure TestImplementsIPluginSettingsManager;

		[Test]
		procedure TestGetSettingsReturnsDefault;

		[Test]
		procedure TestSwitchProxyPasswordStorageNoOp;

		[Test]
		procedure TestMultipleCalls;

		[Test]
		{Verifies SetSettings stores and retrieves the value}
		procedure TestSetSettings_StoresValue;
		[Test]
		{Verifies Save completes without exception}
		procedure TestSave_NoOp;
		[Test]
		{Verifies GetStreamingSettings returns default record}
		procedure TestGetStreamingSettings_ReturnsDefault;
		[Test]
		{Verifies SetStreamingSettings completes without exception}
		procedure TestSetStreamingSettings_NoOp;
		[Test]
		{Verifies GetStreamingExtensionsList clears the list}
		procedure TestGetStreamingExtensionsList_ClearsList;
		[Test]
		{Verifies RemoveStreamingExtension completes without exception}
		procedure TestRemoveStreamingExtension_NoOp;
		[Test]
		{Verifies GetAccountsIniFilePath returns empty string}
		procedure TestGetAccountsIniFilePath_ReturnsEmpty;
		[Test]
		{Verifies Refresh completes without exception}
		procedure TestRefresh_NoOp;
	end;

implementation

{TMockEnvironment}

constructor TMockEnvironment.Create;
begin
	inherited Create;
	FAppData := 'C:\Users\Test\AppData\Roaming';
	FModulePath := 'C:\Program Files\Plugin\';
	FPluginConfigExists := False;
	FDirectoryWriteable := True;
	FDirectoryExists := True;
	FCreatedDirectories := TStringList.Create;
	FIniDirSetting := INI_DIR_PLUGIN;
end;

destructor TMockEnvironment.Destroy;
begin
	FCreatedDirectories.Free;
	inherited;
end;

function TMockEnvironment.GetEnvironmentVariable(const Name: WideString): WideString;
begin
	if SameText(Name, 'APPDATA') then
		Result := FAppData
	else
		Result := '';
end;

function TMockEnvironment.GetModulePath: WideString;
begin
	Result := FModulePath;
end;

function TMockEnvironment.FileExists(const Path: WideString): Boolean;
begin
	Result := FPluginConfigExists and (Pos(String(PLUGIN_CONFIG_FILE_NAME), String(Path)) > 0);
end;

function TMockEnvironment.DirectoryExists(const Path: WideString): Boolean;
begin
	Result := FDirectoryExists;
end;

function TMockEnvironment.IsDirectoryWriteable(const Path: WideString): Boolean;
begin
	Result := FDirectoryWriteable;
end;

procedure TMockEnvironment.CreateDirectory(const Path: WideString);
begin
	FCreatedDirectories.Add(Path);
end;

{TPluginSettingsManagerTest}

procedure TPluginSettingsManagerTest.Setup;
begin
	AppDir := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(hInstance)));
	AppDataSubDir := IncludeTrailingBackslash(IncludeTrailingBackslash(SysUtils.GetEnvironmentVariable('APPDATA')) + APPDATA_DIR_NAME);

	if FileExists(self.AppDir + PLUGIN_CONFIG_FILE_NAME) then
		DeleteFile(self.AppDir + PLUGIN_CONFIG_FILE_NAME);
end;

procedure TPluginSettingsManagerTest.TestCreateDefaults;
var
	TempSettingsManager: TPluginSettingsManager;
begin
	TempSettingsManager := TPluginSettingsManager.Create(TIniConfigFile.Create(''));

	Assert.IsFalse(TempSettingsManager.Settings.DescriptionEnabled);
	Assert.AreEqual(1000, TempSettingsManager.Settings.AttemptWait);
	Assert.AreEqual('descript.ion', TempSettingsManager.Settings.DescriptionFileName);
	Assert.IsFalse(TempSettingsManager.Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager);
	Assert.AreEqual(DEFAULT_USERAGENT, TempSettingsManager.Settings.ConnectionSettings.UserAgent);
	Assert.AreEqual(0, TempSettingsManager.Settings.ConnectionSettings.ProxySettings.Port);
	TempSettingsManager.Free;
end;

procedure TPluginSettingsManagerTest.TestCreateFindFile;
var
	TempSettingsManager: TPluginSettingsManager;
begin
	TempSettingsManager := TPluginSettingsManager.Create();

	Assert.AreEqual(self.AppDir, TempSettingsManager.ApplicationPath);
	Assert.isTrue(TempSettingsManager.Settings.IniDir = INI_DIR_PLUGIN);

	TempSettingsManager.Free;

	TFile.Copy(DataPath(FP_SETTINGS_REDIRECT_INI), self.AppDir + PLUGIN_CONFIG_FILE_NAME);

	TempSettingsManager := TPluginSettingsManager.Create();

	Assert.AreEqual(self.AppDataSubDir, TempSettingsManager.IniFileDir);

	TempSettingsManager.Free;
end;

procedure TPluginSettingsManagerTest.TestCreateFromKnownFile;
var
	TempSettingsManager: TPluginSettingsManager;
begin
	TempSettingsManager := TPluginSettingsManager.Create(TIniConfigFile.Create(DataPath(FP_SETTINGS_INI)));

	Assert.isTrue(TempSettingsManager.Settings.DescriptionEnabled);
	Assert.AreEqual(1000, TempSettingsManager.Settings.AttemptWait);
	Assert.AreEqual('descript.ed', TempSettingsManager.Settings.DescriptionFileName);
	Assert.IsFalse(TempSettingsManager.Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager);
	Assert.AreEqual('There''s no spoon', TempSettingsManager.Settings.ConnectionSettings.UserAgent);
	Assert.AreEqual(5000, TempSettingsManager.Settings.ConnectionSettings.ProxySettings.Port);
	TempSettingsManager.Free;
end;

{TPluginSettingsManagerSaveTest}

procedure TPluginSettingsManagerSaveTest.Setup;
begin
	FConfigFile := TMemoryConfigFile.Create('C:\Test\MailRuCloud.global.ini');
	FConfigFileRef := FConfigFile;
	FManager := TPluginSettingsManager.Create(FConfigFileRef);
end;

procedure TPluginSettingsManagerSaveTest.TearDown;
begin
	FManager.Free;
	FConfigFileRef := nil;
end;

procedure TPluginSettingsManagerSaveTest.Test_Save_WritesAllSettings;
begin
	FManager.Settings.DescriptionEnabled := True;
	FManager.Settings.DescriptionFileName := 'custom.ion';
	FManager.Settings.RetryAttempts := 5;
	FManager.Settings.LogLevel := LOG_LEVEL_DEBUG;

	FManager.Save;

	Assert.AreEqual('1', FConfigFile.ReadString('Main', 'DescriptionEnabled', ''));
	Assert.AreEqual('custom.ion', FConfigFile.ReadString('Main', 'DescriptionFileName', ''));
	Assert.AreEqual(5, FConfigFile.ReadInteger('Main', 'RetryAttempts', 0));
	Assert.AreEqual(LOG_LEVEL_DEBUG, FConfigFile.ReadInteger('Main', 'LogLevel', 0));
end;

procedure TPluginSettingsManagerSaveTest.Test_Save_SkipsDefaultValues;
begin
	{Set all to defaults}
	FManager.Settings.DescriptionEnabled := False;
	FManager.Settings.RetryAttempts := 1;
	FManager.Settings.AttemptWait := 1000;

	FManager.Save;

	{Default values should not be written (WriteIfNotDefault behavior)}
	Assert.AreEqual('fallback', FConfigFile.ReadString('Main', 'DescriptionEnabled', 'fallback'));
	Assert.AreEqual(999, FConfigFile.ReadInteger('Main', 'RetryAttempts', 999));
	Assert.AreEqual(999, FConfigFile.ReadInteger('Main', 'AttemptWait', 999));
end;

procedure TPluginSettingsManagerSaveTest.Test_SaveThenRefresh_RoundTrip;
begin
	{Set non-default values}
	FManager.Settings.DescriptionEnabled := True;
	FManager.Settings.DescriptionFileName := 'roundtrip.ion';
	FManager.Settings.CloudMaxFileSize := 123456789;
	FManager.Settings.IconsMode := IconsModeInternal;

	FManager.Save;

	{Reset to defaults}
	FManager.Settings.DescriptionEnabled := False;
	FManager.Settings.DescriptionFileName := '';
	FManager.Settings.CloudMaxFileSize := 0;
	FManager.Settings.IconsMode := 0;

	{Refresh should reload saved values}
	FManager.Refresh;

	Assert.IsTrue(FManager.Settings.DescriptionEnabled);
	Assert.AreEqual('roundtrip.ion', FManager.Settings.DescriptionFileName);
	Assert.AreEqual(Int64(123456789), FManager.Settings.CloudMaxFileSize);
	Assert.AreEqual(IconsModeInternal, FManager.Settings.IconsMode);
end;

procedure TPluginSettingsManagerSaveTest.Test_Save_WritesProxySettings;
begin
	FManager.Settings.ConnectionSettings.ProxySettings.ProxyType := ProxyHTTP;
	FManager.Settings.ConnectionSettings.ProxySettings.Server := 'proxy.example.com';
	FManager.Settings.ConnectionSettings.ProxySettings.Port := 8080;
	FManager.Settings.ConnectionSettings.ProxySettings.User := 'proxyuser';
	FManager.Settings.ConnectionSettings.ProxySettings.Password := 'proxypass';
	FManager.Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager := True;

	FManager.Save;

	Assert.AreEqual(ProxyHTTP, FConfigFile.ReadInteger('Main', 'ProxyType', 0));
	Assert.AreEqual('proxy.example.com', FConfigFile.ReadString('Main', 'ProxyServer', ''));
	Assert.AreEqual(8080, FConfigFile.ReadInteger('Main', 'ProxyPort', 0));
	Assert.AreEqual('proxyuser', FConfigFile.ReadString('Main', 'ProxyUser', ''));
	Assert.AreEqual('proxypass', FConfigFile.ReadString('Main', 'ProxyPassword', ''));
	Assert.IsTrue(FConfigFile.ReadBool('Main', 'ProxyTCPwdMngr', False));
end;

procedure TPluginSettingsManagerSaveTest.Test_Save_WritesConnectionSettings;
begin
	FManager.Settings.ConnectionSettings.SocketTimeout := 60000;
	FManager.Settings.ConnectionSettings.UploadBPS := 1000000;
	FManager.Settings.ConnectionSettings.DownloadBPS := 2000000;
	FManager.Settings.ConnectionSettings.UserAgent := 'CustomAgent/1.0';

	FManager.Save;

	Assert.AreEqual(60000, FConfigFile.ReadInteger('Main', 'SocketTimeout', 0));
	Assert.AreEqual(1000000, FConfigFile.ReadInteger('Main', 'UploadBPS', 0));
	Assert.AreEqual(2000000, FConfigFile.ReadInteger('Main', 'DownloadBPS', 0));
	Assert.AreEqual('CustomAgent/1.0', FConfigFile.ReadString('Main', 'UserAgent', ''));
end;

procedure TPluginSettingsManagerSaveTest.Test_Save_WritesHashCalculatorStrategy;
begin
	FManager.Settings.HashCalculatorStrategy := HashStrategyBCrypt;

	FManager.Save;

	Assert.AreEqual(HashStrategyBCrypt, FConfigFile.ReadInteger('Main', 'HashCalculatorStrategy', 0));
end;

procedure TPluginSettingsManagerSaveTest.Test_HashCalculatorStrategy_DefaultsToAuto;
var
	DefaultManager: TPluginSettingsManager;
begin
	{Create manager with empty config - should use defaults}
	DefaultManager := TPluginSettingsManager.Create(TMemoryConfigFile.Create(''));
	try
		Assert.AreEqual(HashStrategyAuto, DefaultManager.Settings.HashCalculatorStrategy);
	finally
		DefaultManager.Free;
	end;
end;

{TPluginSettingsManagerStreamingTest}

procedure TPluginSettingsManagerStreamingTest.Setup;
begin
	FConfigFile := TMemoryConfigFile.Create('C:\Test\MailRuCloud.global.ini');
	FConfigFileRef := FConfigFile;
	FManager := TPluginSettingsManager.Create(FConfigFileRef);
end;

procedure TPluginSettingsManagerStreamingTest.TearDown;
begin
	FManager.Free;
	FConfigFileRef := nil;
end;

procedure TPluginSettingsManagerStreamingTest.Test_GetStreamingSettings_WhenNoSection_ReturnsUnset;
var
	StreamSettings: TStreamingSettings;
begin
	StreamSettings := FManager.GetStreamingSettings('video.mp4');

	Assert.AreEqual(STREAMING_FORMAT_UNSET, StreamSettings.Format);
	Assert.AreEqual('', StreamSettings.Command);
end;

procedure TPluginSettingsManagerStreamingTest.Test_GetStreamingSettings_ReadsAllFields;
var
	StreamSettings: TStreamingSettings;
begin
	FConfigFile.WriteString('Streaming:mp4', 'Command', 'vlc.exe');
	FConfigFile.WriteString('Streaming:mp4', 'Parameters', '%url%');
	FConfigFile.WriteString('Streaming:mp4', 'StartPath', 'C:\VLC');
	FConfigFile.WriteInteger('Streaming:mp4', 'Format', STREAMING_FORMAT_PLAYLIST);

	StreamSettings := FManager.GetStreamingSettings('movie.mp4');

	Assert.AreEqual('vlc.exe', StreamSettings.Command);
	Assert.AreEqual('%url%', StreamSettings.Parameters);
	Assert.AreEqual('C:\VLC', StreamSettings.StartPath);
	Assert.AreEqual(STREAMING_FORMAT_PLAYLIST, StreamSettings.Format);
end;

procedure TPluginSettingsManagerStreamingTest.Test_SetStreamingSettings_WritesAllFields;
var
	StreamSettings: TStreamingSettings;
begin
	StreamSettings.Command := 'mpv.exe';
	StreamSettings.Parameters := '--url=%url%';
	StreamSettings.StartPath := 'D:\MPV';
	StreamSettings.Format := STREAMING_FORMAT_DEFAULT;

	FManager.SetStreamingSettings('audio.mp3', StreamSettings);

	Assert.AreEqual('mpv.exe', FConfigFile.ReadString('Streaming:mp3', 'Command', ''));
	Assert.AreEqual('--url=%url%', FConfigFile.ReadString('Streaming:mp3', 'Parameters', ''));
	Assert.AreEqual('D:\MPV', FConfigFile.ReadString('Streaming:mp3', 'StartPath', ''));
	Assert.AreEqual(STREAMING_FORMAT_DEFAULT, FConfigFile.ReadInteger('Streaming:mp3', 'Format', 0));
end;

procedure TPluginSettingsManagerStreamingTest.Test_SetStreamingSettings_EmptyExtension_DoesNotWrite;
var
	StreamSettings: TStreamingSettings;
begin
	StreamSettings.Command := 'test.exe';
	StreamSettings.Format := STREAMING_FORMAT_PLAYLIST;

	{File without extension}
	FManager.SetStreamingSettings('noextension', StreamSettings);

	{Should not create any streaming section}
	Assert.IsFalse(FConfigFile.SectionExists('Streaming:'));
	Assert.IsFalse(FConfigFile.SectionExists('Streaming:noextension'));
end;

procedure TPluginSettingsManagerStreamingTest.Test_GetStreamingExtensionsList_ReturnsOnlyStreamingSections;
var
	Extensions: TStringList;
begin
	FConfigFile.WriteString('Main', 'SomeKey', 'value');
	FConfigFile.WriteString('Streaming:mp4', 'Command', 'vlc.exe');
	FConfigFile.WriteString('Streaming:avi', 'Command', 'vlc.exe');
	FConfigFile.WriteString('Streaming:mkv', 'Command', 'vlc.exe');
	FConfigFile.WriteString('OtherSection', 'Key', 'value');

	Extensions := TStringList.Create;
	try
		FManager.GetStreamingExtensionsList(Extensions);

		Assert.AreEqual(3, Extensions.Count);
		Assert.IsTrue(Extensions.IndexOf('mp4') >= 0);
		Assert.IsTrue(Extensions.IndexOf('avi') >= 0);
		Assert.IsTrue(Extensions.IndexOf('mkv') >= 0);
	finally
		Extensions.Free;
	end;
end;

procedure TPluginSettingsManagerStreamingTest.Test_GetStreamingExtensionsList_EmptyWhenNoSections;
var
	Extensions: TStringList;
begin
	FConfigFile.WriteString('Main', 'SomeKey', 'value');

	Extensions := TStringList.Create;
	try
		FManager.GetStreamingExtensionsList(Extensions);
		Assert.AreEqual(0, Extensions.Count);
	finally
		Extensions.Free;
	end;
end;

procedure TPluginSettingsManagerStreamingTest.Test_RemoveStreamingExtension_ErasesSection;
begin
	FConfigFile.WriteString('Streaming:mp4', 'Command', 'vlc.exe');
	FConfigFile.WriteString('Streaming:mp4', 'Format', '2');
	Assert.IsTrue(FConfigFile.SectionExists('Streaming:mp4'));

	FManager.RemoveStreamingExtension('mp4');

	Assert.IsFalse(FConfigFile.SectionExists('Streaming:mp4'));
end;

procedure TPluginSettingsManagerStreamingTest.Test_StreamingSettings_RoundTrip;
var
	Original, Retrieved: TStreamingSettings;
begin
	Original.Command := 'ffplay.exe';
	Original.Parameters := '-autoexit %url%';
	Original.StartPath := 'C:\FFmpeg\bin';
	Original.Format := STREAMING_FORMAT_WEBLINK_VIEW;

	FManager.SetStreamingSettings('video.webm', Original);
	Retrieved := FManager.GetStreamingSettings('another.webm');

	Assert.AreEqual(Original.Command, Retrieved.Command);
	Assert.AreEqual(Original.Parameters, Retrieved.Parameters);
	Assert.AreEqual(Original.StartPath, Retrieved.StartPath);
	Assert.AreEqual(Original.Format, Retrieved.Format);
end;

{TPluginSettingsManagerMiscTest}

procedure TPluginSettingsManagerMiscTest.Setup;
begin
	FConfigFile := TMemoryConfigFile.Create('C:\Test\Config\MailRuCloud.global.ini');
	FConfigFileRef := FConfigFile;
	FManager := TPluginSettingsManager.Create(FConfigFileRef);
end;

procedure TPluginSettingsManagerMiscTest.TearDown;
begin
	FManager.Free;
	FConfigFileRef := nil;
end;

procedure TPluginSettingsManagerMiscTest.Test_GetSettings_ReturnsCurrentSettings;
var
	Retrieved: TPluginSettings;
begin
	FManager.Settings.LogLevel := 999;
	FManager.Settings.RetryAttempts := 10;

	Retrieved := FManager.GetSettings;

	Assert.AreEqual(999, Retrieved.LogLevel);
	Assert.AreEqual(10, Retrieved.RetryAttempts);
end;

procedure TPluginSettingsManagerMiscTest.Test_GetAccountsIniFilePath_ReturnsCorrectPath;
begin
	{IniFileDir is C:\Test\Config\ based on ConfigFile path}
	Assert.AreEqual('C:\Test\Config\' + ACCOUNTS_CONFIG_FILE_NAME, FManager.AccountsIniFilePath);
end;

procedure TPluginSettingsManagerMiscTest.Test_SwitchProxyPasswordStorage_EnablesTCManager;
begin
	FConfigFile.WriteBool('Main', 'ProxyTCPwdMngr', False);

	FManager.SwitchProxyPasswordStorage;

	Assert.IsTrue(FConfigFile.ReadBool('Main', 'ProxyTCPwdMngr', False));
end;

procedure TPluginSettingsManagerMiscTest.Test_SwitchProxyPasswordStorage_DeletesPassword;
begin
	FConfigFile.WriteString('Main', 'ProxyPassword', 'secret');
	Assert.AreEqual('secret', FConfigFile.ReadString('Main', 'ProxyPassword', ''));

	FManager.SwitchProxyPasswordStorage;

	Assert.AreEqual('', FConfigFile.ReadString('Main', 'ProxyPassword', ''));
end;

procedure TPluginSettingsManagerMiscTest.Test_IniFilePath_ReturnsConfigFilePath;
begin
	Assert.AreEqual('C:\Test\Config\MailRuCloud.global.ini', FManager.IniFilePath);
end;

{TPluginSettingsManagerEnvironmentTest}

procedure TPluginSettingsManagerEnvironmentTest.Setup;
begin
	FMockEnv := TMockEnvironment.Create;
	FMockEnvRef := FMockEnv;
end;

procedure TPluginSettingsManagerEnvironmentTest.TearDown;
begin
	FMockEnvRef := nil;
end;

procedure TPluginSettingsManagerEnvironmentTest.Test_Create_NoConfigFile_WriteableDir_UsesPluginDir;
var
	Manager: TPluginSettingsManager;
begin
	FMockEnv.PluginConfigExists := False;
	FMockEnv.DirectoryWriteable := True;
	FMockEnv.ModulePath := 'D:\Plugins\CloudMailRu\';

	Manager := TPluginSettingsManager.Create(FMockEnvRef);
	try
		Assert.AreEqual('D:\Plugins\CloudMailRu\', Manager.IniFileDir);
	finally
		Manager.Free;
	end;
end;

procedure TPluginSettingsManagerEnvironmentTest.Test_Create_NoConfigFile_NotWriteable_UsesAppData;
var
	Manager: TPluginSettingsManager;
	ExpectedDir: WideString;
begin
	FMockEnv.PluginConfigExists := False;
	FMockEnv.DirectoryWriteable := False;
	FMockEnv.AppData := 'C:\Users\TestUser\AppData\Roaming';
	FMockEnv.ModulePath := 'C:\Program Files\Plugin\';

	ExpectedDir := 'C:\Users\TestUser\AppData\Roaming\' + APPDATA_DIR_NAME + '\';

	Manager := TPluginSettingsManager.Create(FMockEnvRef);
	try
		Assert.AreEqual(ExpectedDir, Manager.IniFileDir);
	finally
		Manager.Free;
	end;
end;

procedure TPluginSettingsManagerEnvironmentTest.Test_Create_CreatesDirectoryIfNotExists;
var
	Manager: TPluginSettingsManager;
begin
	FMockEnv.PluginConfigExists := False;
	FMockEnv.DirectoryWriteable := False;
	FMockEnv.DirExists := False;
	FMockEnv.AppData := 'C:\Users\Test\AppData\Roaming';

	Manager := TPluginSettingsManager.Create(FMockEnvRef);
	try
		Assert.AreEqual(1, FMockEnv.CreatedDirectories.Count);
		Assert.IsTrue(Pos(String(APPDATA_DIR_NAME), FMockEnv.CreatedDirectories[0]) > 0);
	finally
		Manager.Free;
	end;
end;

{TNullPluginSettingsManagerTest}

procedure TNullPluginSettingsManagerTest.TestImplementsIPluginSettingsManager;
var
	SettingsManager: IPluginSettingsManager;
begin
	SettingsManager := TNullPluginSettingsManager.Create;
	Assert.IsNotNull(SettingsManager);
end;

procedure TNullPluginSettingsManagerTest.TestGetSettingsReturnsDefault;
var
	SettingsManager: IPluginSettingsManager;
	Settings: TPluginSettings;
begin
	SettingsManager := TNullPluginSettingsManager.Create;
	Settings := SettingsManager.GetSettings;
	Assert.AreEqual(0, Settings.LogLevel);
	Assert.AreEqual(Int64(0), Settings.CloudMaxFileSize);
	Assert.IsFalse(Settings.PrecalculateHash);
end;

procedure TNullPluginSettingsManagerTest.TestSwitchProxyPasswordStorageNoOp;
var
	SettingsManager: IPluginSettingsManager;
begin
	SettingsManager := TNullPluginSettingsManager.Create;
	SettingsManager.SwitchProxyPasswordStorage;
	Assert.Pass;
end;

procedure TNullPluginSettingsManagerTest.TestMultipleCalls;
var
	SettingsManager: IPluginSettingsManager;
	Settings1, Settings2: TPluginSettings;
begin
	SettingsManager := TNullPluginSettingsManager.Create;

	Settings1 := SettingsManager.GetSettings;
	SettingsManager.SwitchProxyPasswordStorage;
	Settings2 := SettingsManager.GetSettings;

	Assert.AreEqual(Settings1.LogLevel, Settings2.LogLevel);
	Assert.Pass;
end;

procedure TNullPluginSettingsManagerTest.TestSetSettings_StoresValue;
var
	SettingsManager: IPluginSettingsManager;
	Settings: TPluginSettings;
begin
	SettingsManager := TNullPluginSettingsManager.Create;
	Settings := Default(TPluginSettings);
	Settings.LogLevel := 5;
	SettingsManager.SetSettings(Settings);
	Assert.AreEqual(5, SettingsManager.GetSettings.LogLevel);
end;

procedure TNullPluginSettingsManagerTest.TestSave_NoOp;
var
	SettingsManager: IPluginSettingsManager;
begin
	SettingsManager := TNullPluginSettingsManager.Create;
	SettingsManager.Save;
	Assert.Pass;
end;

procedure TNullPluginSettingsManagerTest.TestGetStreamingSettings_ReturnsDefault;
var
	SettingsManager: IPluginSettingsManager;
	StreamSettings: TStreamingSettings;
begin
	SettingsManager := TNullPluginSettingsManager.Create;
	StreamSettings := SettingsManager.GetStreamingSettings('test.mp3');
	Assert.AreEqual(EmptyWideStr, StreamSettings.Command);
end;

procedure TNullPluginSettingsManagerTest.TestSetStreamingSettings_NoOp;
var
	SettingsManager: IPluginSettingsManager;
	StreamSettings: TStreamingSettings;
begin
	SettingsManager := TNullPluginSettingsManager.Create;
	StreamSettings := Default(TStreamingSettings);
	SettingsManager.SetStreamingSettings('test.mp3', StreamSettings);
	Assert.Pass;
end;

procedure TNullPluginSettingsManagerTest.TestGetStreamingExtensionsList_ClearsList;
var
	SettingsManager: IPluginSettingsManager;
	Extensions: TStringList;
begin
	SettingsManager := TNullPluginSettingsManager.Create;
	Extensions := TStringList.Create;
	try
		Extensions.Add('pre-existing');
		SettingsManager.GetStreamingExtensionsList(Extensions);
		Assert.AreEqual(0, Extensions.Count, 'List should be cleared');
	finally
		Extensions.Free;
	end;
end;

procedure TNullPluginSettingsManagerTest.TestRemoveStreamingExtension_NoOp;
var
	SettingsManager: IPluginSettingsManager;
begin
	SettingsManager := TNullPluginSettingsManager.Create;
	SettingsManager.RemoveStreamingExtension('mp3');
	Assert.Pass;
end;

procedure TNullPluginSettingsManagerTest.TestGetAccountsIniFilePath_ReturnsEmpty;
var
	SettingsManager: IPluginSettingsManager;
begin
	SettingsManager := TNullPluginSettingsManager.Create;
	Assert.AreEqual(EmptyWideStr, SettingsManager.GetAccountsIniFilePath);
end;

procedure TNullPluginSettingsManagerTest.TestRefresh_NoOp;
var
	SettingsManager: IPluginSettingsManager;
begin
	SettingsManager := TNullPluginSettingsManager.Create;
	SettingsManager.Refresh;
	Assert.Pass;
end;

initialization

TDUnitX.RegisterTestFixture(TPluginSettingsManagerTest);
TDUnitX.RegisterTestFixture(TPluginSettingsManagerSaveTest);
TDUnitX.RegisterTestFixture(TPluginSettingsManagerStreamingTest);
TDUnitX.RegisterTestFixture(TPluginSettingsManagerMiscTest);
TDUnitX.RegisterTestFixture(TPluginSettingsManagerEnvironmentTest);
TDUnitX.RegisterTestFixture(TNullPluginSettingsManagerTest);

end.
