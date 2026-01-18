unit NullPluginSettingsManagerTest;

interface

uses
	IPluginSettingsManagerInterface,
	PluginSettings,
	SysUtils,
	DUnitX.TestFramework;

type
	[TestFixture]
	TNullPluginSettingsManagerTest = class
	public
		[Test]
		{Verifies TNullPluginSettingsManager can be assigned to IPluginSettingsManager variable}
		procedure TestImplementsIPluginSettingsManager;

		[Test]
		{Verifies GetSettings returns default settings}
		procedure TestGetSettingsReturnsDefault;

		[Test]
		{Verifies SwitchProxyPasswordStorage completes without exception}
		procedure TestSwitchProxyPasswordStorageNoOp;

		[Test]
		{Verifies multiple calls work correctly}
		procedure TestMultipleCalls;
	end;

implementation

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
	{Default values should be zero/false/empty}
	Assert.AreEqual(0, Settings.LogLevel);
	Assert.AreEqual(Int64(0), Settings.CloudMaxFileSize);
	Assert.IsFalse(Settings.PrecalculateHash);
end;

procedure TNullPluginSettingsManagerTest.TestSwitchProxyPasswordStorageNoOp;
var
	SettingsManager: IPluginSettingsManager;
begin
	SettingsManager := TNullPluginSettingsManager.Create;
	{Should complete without exception}
	SettingsManager.SwitchProxyPasswordStorage;
	Assert.Pass('SwitchProxyPasswordStorage completed without exception');
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

	{Both should return the same default values}
	Assert.AreEqual(Settings1.LogLevel, Settings2.LogLevel);

	Assert.Pass('Multiple calls completed without exception');
end;

initialization

TDUnitX.RegisterTestFixture(TNullPluginSettingsManagerTest);

end.
