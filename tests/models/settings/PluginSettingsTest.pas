unit PluginSettingsTest;

interface

uses
	PluginSettings,
	SETTINGS_CONSTANTS,
	TestHelper,
	SysUtils,
	IOUtils,
	ConnectionSettings,
	DUnitX.TestFramework;

type

	[TestFixture]
	TPluginSettingsTest = class
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

		[Test]
		procedure TestSetValue;

		[Test]
		procedure TestSaveOnChange;

		[Test]
		procedure TestSaveOnChangeDisabled;
	end;

implementation

procedure TPluginSettingsTest.Setup;
begin
	AppDir := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(hInstance)));
	AppDataSubDir := IncludeTrailingBackslash(IncludeTrailingBackslash(SysUtils.GetEnvironmentVariable('APPDATA')) + APPDATA_DIR_NAME);

	{cleans the previous run artefacts}
	if FileExists(self.AppDir + PLUGIN_CONFIG_FILE_NAME) then
		DeleteFile(self.AppDir + PLUGIN_CONFIG_FILE_NAME);

end;

procedure TPluginSettingsTest.TestCreateDefaults;
var
	MRCSetting: TPluginSettings;
begin
	MRCSetting := TPluginSettings.Create('');

	{peek some randoms of different data types}
	Assert.IsFalse(MRCSetting.DescriptionEnabled); //boolean
	Assert.AreEqual(1000, MRCSetting.AttemptWait); //integer
	Assert.AreEqual('descript.ion', MRCSetting.DescriptionFileName); //string
	Assert.IsFalse(MRCSetting.ConnectionSettings.ProxySettings.use_tc_password_manager); //subrecord boolean
	Assert.AreEqual(DEFAULT_USERAGENT, MRCSetting.ConnectionSettings.UserAgent); //subrecord string
	Assert.AreEqual(0, MRCSetting.ConnectionSettings.ProxySettings.Port); //subrecord string
	MRCSetting.Free;
end;

procedure TPluginSettingsTest.TestCreateFindFile;
var
	MRCSetting: TPluginSettings;
begin
	MRCSetting := TPluginSettings.Create();

	{It finds no config, but writeable application directory, and uses it}
	Assert.AreEqual(self.AppDir, MRCSetting.PluginPath);
	Assert.AreEqual(INI_PATH_PLUGIN_DIR, MRCSetting.IniPath);

	MRCSetting.Free;

	{Copies the ini with only one directive (redirect to the appdata) to the application dir}
	TFile.Copy(DataPath(FP_SETTINGS_REDIRECT_INI), self.AppDir + PLUGIN_CONFIG_FILE_NAME);

	MRCSetting := TPluginSettings.Create(); //use a open class to acces the private properties

	{It finds a config, where IniPath is set to one, and interprets it as a redirection to the AppData}
	Assert.AreEqual(self.AppDataSubDir, MRCSetting.IniDir);

	{The IniPath in the redirected file can be any}
	//	Assert.AreEqual(INI_PATH_APPDATA, MRCSetting.IniPath);

	MRCSetting.Free;

end;

procedure TPluginSettingsTest.TestCreateFromKnownFile;
var
	MRCSetting: TPluginSettings;
begin
	MRCSetting := TPluginSettings.Create(DataPath(FP_SETTINGS_INI));

	{peek some randoms of different data types}
	Assert.IsTrue(MRCSetting.DescriptionEnabled); //boolean
	Assert.AreEqual(1000, MRCSetting.AttemptWait); //integer
	Assert.AreEqual('descript.ed', MRCSetting.DescriptionFileName); //string
	Assert.IsFalse(MRCSetting.ConnectionSettings.ProxySettings.use_tc_password_manager); //subrecord boolean
	Assert.AreEqual('There''s no spoon', MRCSetting.ConnectionSettings.UserAgent); //subrecord string
	Assert.AreEqual(5000, MRCSetting.ConnectionSettings.ProxySettings.Port); //subrecord string
	MRCSetting.Free;

end;

procedure TPluginSettingsTest.TestSaveOnChange;
var
	MRCSetting: TPluginSettings;
	ConnectionSettings: TConnectionSettings;
begin
	MRCSetting := TPluginSettings.Create(); //creates a file in the test exe dir
	MRCSetting.SaveOnChange := True;
	Assert.IsTrue(MRCSetting.SaveOnChange);
	Assert.IsFalse(MRCSetting.DescriptionEnabled);
	Assert.AreEqual(DEFAULT_USERAGENT, MRCSetting.ConnectionSettings.UserAgent);

	MRCSetting.DescriptionEnabled := True;

	ConnectionSettings := MRCSetting.ConnectionSettings;
	ConnectionSettings.UserAgent := 'BugZilla 100/500';
	MRCSetting.ConnectionSettings := ConnectionSettings;

	MRCSetting.Free;

	MRCSetting := TPluginSettings.Create();

	Assert.IsTrue(MRCSetting.DescriptionEnabled);
	Assert.AreEqual('BugZilla 100/500', MRCSetting.ConnectionSettings.UserAgent);

	MRCSetting.Free;
end;

procedure TPluginSettingsTest.TestSaveOnChangeDisabled;
var
	MRCSetting: TPluginSettings;
	ConnectionSettings: TConnectionSettings;
begin
	MRCSetting := TPluginSettings.Create();

	Assert.IsFalse(MRCSetting.SaveOnChange);
	Assert.IsFalse(MRCSetting.DescriptionEnabled);
	Assert.AreEqual(DEFAULT_USERAGENT, MRCSetting.ConnectionSettings.UserAgent);

	MRCSetting.DescriptionEnabled := True;

	ConnectionSettings := MRCSetting.ConnectionSettings;
	ConnectionSettings.UserAgent := 'BugZilla 100/500';
	MRCSetting.ConnectionSettings := ConnectionSettings;

	MRCSetting.Free;

	MRCSetting := TPluginSettings.Create();

	{See that nothing changed, cause SaveOnChange is disabled}
	Assert.IsFalse(MRCSetting.DescriptionEnabled);
	Assert.AreEqual(DEFAULT_USERAGENT, MRCSetting.ConnectionSettings.UserAgent);

	MRCSetting.Free;
end;

procedure TPluginSettingsTest.TestSetValue;
var
	MRCSetting: TPluginSettings;
begin
	MRCSetting := TPluginSettings.Create(); //creates a file in the test exe dir

	Assert.IsFalse(MRCSetting.DescriptionEnabled);
	MRCSetting.SetSettingValue('DescriptionEnabled', True);

	MRCSetting.Free;

	MRCSetting := TPluginSettings.Create(); //creates a file in the test exe dir

	Assert.IsTrue(MRCSetting.DescriptionEnabled);
	MRCSetting.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TPluginSettingsTest);

end.
