unit PluginSettingsManagerTest;

interface

uses
	PluginSettings,
	PluginSettingsManager,
	SETTINGS_CONSTANTS,
	TestHelper,
	SysUtils,
	IOUtils,
	ConnectionSettings,
	DUnitX.TestFramework;

type

	[TestFixture]
	TPluginSettingsManagerTest = class
		AppDir: WideString; //the current test binary directory
		AppDataSubDir: WideString; //the subdirectory in AppData
	private const
		FP_SETTINGS_INI = 'Settings.ini';
		FP_SETTINGS_REDIRECT_INI = 'SettingsRedirect.ini';
	public
		[Setup]
		procedure Setup;

		[Test]
		procedure TestCreateFromKnownFile; {When the ini file name is known, it can be used in the simple way}

		[Test]
		procedure TestCreateDefaults; {When there's no file, default values should be loaded}

		[Test]
		procedure TestCreateFindFile; {When the ini file name isn't known, constructor looks for it in the application directory and appdata folder}

	end;

implementation

procedure TPluginSettingsManagerTest.Setup;
begin
	AppDir := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(hInstance)));
	AppDataSubDir := IncludeTrailingBackslash(IncludeTrailingBackslash(SysUtils.GetEnvironmentVariable('APPDATA')) + APPDATA_DIR_NAME);

	{cleans the previous run artefacts}
	if FileExists(self.AppDir + PLUGIN_CONFIG_FILE_NAME) then
		DeleteFile(self.AppDir + PLUGIN_CONFIG_FILE_NAME);

end;

procedure TPluginSettingsManagerTest.TestCreateDefaults;
var
	TempSettingsManager: TPluginSettingsManager;
begin
	TempSettingsManager := TPluginSettingsManager.Create('');

	{peek some randoms of different data types}
	Assert.IsFalse(TempSettingsManager.Settings.DescriptionEnabled); //boolean
	Assert.AreEqual(1000, TempSettingsManager.Settings.AttemptWait); //integer
	Assert.AreEqual('descript.ion', TempSettingsManager.Settings.DescriptionFileName); //string
	Assert.IsFalse(TempSettingsManager.Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager); //subrecord boolean
	Assert.AreEqual(DEFAULT_USERAGENT, TempSettingsManager.Settings.ConnectionSettings.UserAgent); //subrecord string
	Assert.AreEqual(0, TempSettingsManager.Settings.ConnectionSettings.ProxySettings.Port); //subrecord string
	TempSettingsManager.Free;
end;

procedure TPluginSettingsManagerTest.TestCreateFindFile;
var
	TempSettingsManager: TPluginSettingsManager;
begin
	TempSettingsManager := TPluginSettingsManager.Create();

	{It finds no config, but writeable application directory, and uses it}
	Assert.AreEqual(self.AppDir, TempSettingsManager.ApplicationPath);
	Assert.AreEqual(INI_DIR_PLUGIN, TempSettingsManager.Settings.IniDir);

	TempSettingsManager.Free;

	{Copies the ini with only one directive (redirect to the appdata) to the application dir}
	TFile.Copy(DataPath(FP_SETTINGS_REDIRECT_INI), self.AppDir + PLUGIN_CONFIG_FILE_NAME);

	TempSettingsManager := TPluginSettingsManager.Create();

	{It finds a config, where IniPath is set to one, and interprets it as a redirection to the AppData}
	Assert.AreEqual(self.AppDataSubDir, TempSettingsManager.IniFileDir);

	{The IniPath in the redirected file can be any}
	//	Assert.AreEqual(INI_PATH_APPDATA, TempSettingsManager.IniPath);

	TempSettingsManager.Free;

end;

procedure TPluginSettingsManagerTest.TestCreateFromKnownFile;
var
	TempSettingsManager: TPluginSettingsManager;
begin
	TempSettingsManager := TPluginSettingsManager.Create(DataPath(FP_SETTINGS_INI));

	{peek some randoms of different data types}
	Assert.IsTrue(TempSettingsManager.Settings.DescriptionEnabled); //boolean
	Assert.AreEqual(1000, TempSettingsManager.Settings.AttemptWait); //integer
	Assert.AreEqual('descript.ed', TempSettingsManager.Settings.DescriptionFileName); //string
	Assert.IsFalse(TempSettingsManager.Settings.ConnectionSettings.ProxySettings.UseTCPasswordManager); //subrecord boolean
	Assert.AreEqual('There''s no spoon', TempSettingsManager.Settings.ConnectionSettings.UserAgent); //subrecord string
	Assert.AreEqual(5000, TempSettingsManager.Settings.ConnectionSettings.ProxySettings.Port); //subrecord string
	TempSettingsManager.Free;

end;


initialization

TDUnitX.RegisterTestFixture(TPluginSettingsManagerTest);

end.
