unit MRCSettingsTest;

interface

uses
	MRCSettings,
	SETTINGS_CONSTANTS,
	TestHelper,
	SysUtils,
	IOUtils,
	DUnitX.TestFramework;

type

	[TestFixture]
	TMRCSettingsTest = class
		AppDir: WideString; //the current test binary directory
		AppDataSubDir: WideString; //the subdirectory in AppData
	private const
		FP_SETTINGS_INI = 'Settings.ini';
		FP_SETTINGS_REDIRECT_INI = 'SettingsRedirect.ini';
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestCreateFromKnownFile; {When the ini file name is known, it can be used in the simple way}

		[Test]
		procedure TestCreateDefaults; {When there's no file, default values should be loaded}

		[Test]
		procedure TestCreateFindFile; {When the ini file name isn't known, constructor looks for it in the application directory and appdata folder}
	end;

implementation

procedure TMRCSettingsTest.Setup;
begin
	AppDir := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(hInstance)));
	AppDataSubDir := IncludeTrailingBackslash(IncludeTrailingBackslash(SysUtils.GetEnvironmentVariable('APPDATA')) + APPDATA_DIR_NAME);

	{cleans the previous run artefacts}
	if FileExists(self.AppDir + PLUGIN_CONFIG_FILE_NAME) then
		DeleteFile(self.AppDir + PLUGIN_CONFIG_FILE_NAME);

end;

procedure TMRCSettingsTest.TearDown;
begin
end;

procedure TMRCSettingsTest.TestCreateDefaults;
var
	MRCSetting: TMRCSettings;
begin
	MRCSetting := TMRCSettings.Create('');

	{peek some randoms of different data types}
	Assert.IsFalse(MRCSetting.DescriptionEnabled); //boolean
	Assert.AreEqual(1000, MRCSetting.AttemptWait); //integer
	Assert.AreEqual('descript.ion', MRCSetting.DescriptionFileName_); //string
	Assert.IsFalse(MRCSetting.ConnectionSettings.ProxySettings.use_tc_password_manager); //subrecord boolean
	Assert.AreEqual(DEFAULT_USERAGENT, MRCSetting.ConnectionSettings.UserAgent); //subrecord string
	Assert.AreEqual(0, MRCSetting.ConnectionSettings.ProxySettings.Port); //subrecord string
	MRCSetting.Free;
end;

procedure TMRCSettingsTest.TestCreateFindFile;
var
	MRCSetting: TMRCSettings;
begin
	MRCSetting := TMRCSettings.Create();

	{It finds no config, but writeable application directory, and uses it}
	Assert.AreEqual(self.AppDir, MRCSetting.PluginPath);
	Assert.AreEqual(INI_PATH_PLUGIN_DIR, MRCSetting.IniPath);

	MRCSetting.Free;

	{Copies the ini with only one directive (redirect to the appdata) to the application dir}
	TFile.Copy(DataPath(FP_SETTINGS_REDIRECT_INI), self.AppDir + PLUGIN_CONFIG_FILE_NAME);

	MRCSetting := TMRCSettings.Create(); //use a open class to acces the private properties

	{It finds a config, where IniPath is set to one, and interprets it as a redirection to the AppData}
	Assert.AreEqual(self.AppDataSubDir, MRCSetting.IniDir);

	{The IniPath in the redirected file can be any}
	//	Assert.AreEqual(INI_PATH_APPDATA, MRCSetting.IniPath);

	MRCSetting.Free;

end;

procedure TMRCSettingsTest.TestCreateFromKnownFile;
var
	MRCSetting: TMRCSettings;
begin
	MRCSetting := TMRCSettings.Create(DataPath(FP_SETTINGS_INI));

	{peek some randoms of different data types}
	Assert.IsTrue(MRCSetting.DescriptionEnabled); //boolean
	Assert.AreEqual(1000, MRCSetting.AttemptWait); //integer
	Assert.AreEqual('descript.ed', MRCSetting.DescriptionFileName_); //string
	Assert.IsFalse(MRCSetting.ConnectionSettings.ProxySettings.use_tc_password_manager); //subrecord boolean
	Assert.AreEqual('There''s no spoon', MRCSetting.ConnectionSettings.UserAgent); //subrecord string
	Assert.AreEqual(5000, MRCSetting.ConnectionSettings.ProxySettings.Port); //subrecord string
	MRCSetting.Free;

end;

initialization

TDUnitX.RegisterTestFixture(TMRCSettingsTest);

end.
