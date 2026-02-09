unit CloudSettingsTest;

interface

uses
	CloudSettings,
	PluginSettings,
	AccountSettings,
	ConnectionSettings,
	SysUtils,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCloudSettingsTest = class
	public
		[Test]
		{Verifies CreateFromSettings correctly copies ConnectionSettings}
		procedure TestCreateFromSettingsCopiesConnectionSettings;

		[Test]
		{Verifies CreateFromSettings correctly copies AccountSettings}
		procedure TestCreateFromSettingsCopiesAccountSettings;

		[Test]
		{Verifies CreateFromSettings correctly copies PrecalculateHash}
		procedure TestCreateFromSettingsCopiesPrecalculateHash;

		[Test]
		{Verifies CreateFromSettings correctly copies ForcePrecalculateSize}
		procedure TestCreateFromSettingsCopiesForcePrecalculateSize;

		[Test]
		{Verifies CreateFromSettings correctly copies CheckCRC}
		procedure TestCreateFromSettingsCopiesCheckCRC;

		[Test]
		{Verifies CreateFromSettings correctly copies CloudMaxFileSize}
		procedure TestCreateFromSettingsCopiesCloudMaxFileSize;

		[Test]
		{Verifies CreateFromSettings correctly copies OperationErrorMode}
		procedure TestCreateFromSettingsCopiesOperationErrorMode;

		[Test]
		{Verifies CreateFromSettings correctly copies RetryAttempts}
		procedure TestCreateFromSettingsCopiesRetryAttempts;

		[Test]
		{Verifies CreateFromSettings correctly copies AttemptWait}
		procedure TestCreateFromSettingsCopiesAttemptWait;

		[Test]
		{Verifies CreateFromSettings leaves CryptFilesPassword empty}
		procedure TestCreateFromSettingsLeavesCryptFilesPasswordEmpty;

		[Test]
		{Verifies CreateFromSettings with default settings}
		procedure TestCreateFromSettingsWithDefaults;
	end;

implementation

procedure TCloudSettingsTest.TestCreateFromSettingsCopiesConnectionSettings;
var
	PluginSettings: TPluginSettings;
	AccSettings: TAccountSettings;
	Result: TCloudSettings;
begin
	PluginSettings := Default(TPluginSettings);
	PluginSettings.ConnectionSettings.SocketTimeout := 30000;
	AccSettings := Default(TAccountSettings);

	Result := TCloudSettings.CreateFromSettings(PluginSettings, AccSettings);

	Assert.AreEqual(30000, Result.ConnectionSettings.SocketTimeout);
end;

procedure TCloudSettingsTest.TestCreateFromSettingsCopiesAccountSettings;
var
	PluginSettings: TPluginSettings;
	AccSettings: TAccountSettings;
	Result: TCloudSettings;
begin
	PluginSettings := Default(TPluginSettings);
	AccSettings := Default(TAccountSettings);
	AccSettings.Account := 'test_account';
	AccSettings.Email := 'test@example.com';

	Result := TCloudSettings.CreateFromSettings(PluginSettings, AccSettings);

	Assert.AreEqual('test_account', Result.AccountSettings.Account);
	Assert.AreEqual('test@example.com', Result.AccountSettings.Email);
end;

procedure TCloudSettingsTest.TestCreateFromSettingsCopiesPrecalculateHash;
var
	PluginSettings: TPluginSettings;
	AccSettings: TAccountSettings;
	Result: TCloudSettings;
begin
	PluginSettings := Default(TPluginSettings);
	PluginSettings.PrecalculateHash := True;
	AccSettings := Default(TAccountSettings);

	Result := TCloudSettings.CreateFromSettings(PluginSettings, AccSettings);

	Assert.IsTrue(Result.PrecalculateHash);
end;

procedure TCloudSettingsTest.TestCreateFromSettingsCopiesForcePrecalculateSize;
var
	PluginSettings: TPluginSettings;
	AccSettings: TAccountSettings;
	Result: TCloudSettings;
begin
	PluginSettings := Default(TPluginSettings);
	PluginSettings.ForcePrecalculateSize := 1048576;
	AccSettings := Default(TAccountSettings);

	Result := TCloudSettings.CreateFromSettings(PluginSettings, AccSettings);

	Assert.AreEqual(Int64(1048576), Result.ForcePrecalculateSize);
end;

procedure TCloudSettingsTest.TestCreateFromSettingsCopiesCheckCRC;
var
	PluginSettings: TPluginSettings;
	AccSettings: TAccountSettings;
	Result: TCloudSettings;
begin
	PluginSettings := Default(TPluginSettings);
	PluginSettings.CheckCRC := True;
	AccSettings := Default(TAccountSettings);

	Result := TCloudSettings.CreateFromSettings(PluginSettings, AccSettings);

	Assert.IsTrue(Result.CheckCRC);
end;

procedure TCloudSettingsTest.TestCreateFromSettingsCopiesCloudMaxFileSize;
var
	PluginSettings: TPluginSettings;
	AccSettings: TAccountSettings;
	Result: TCloudSettings;
begin
	PluginSettings := Default(TPluginSettings);
	AccSettings := Default(TAccountSettings);
	AccSettings.CloudMaxFileSize := 2147483648;

	Result := TCloudSettings.CreateFromSettings(PluginSettings, AccSettings);

	Assert.AreEqual(Int64(2147483648), Result.CloudMaxFileSize);
end;

procedure TCloudSettingsTest.TestCreateFromSettingsCopiesOperationErrorMode;
var
	PluginSettings: TPluginSettings;
	AccSettings: TAccountSettings;
	Result: TCloudSettings;
begin
	PluginSettings := Default(TPluginSettings);
	PluginSettings.OperationErrorMode := 2;
	AccSettings := Default(TAccountSettings);

	Result := TCloudSettings.CreateFromSettings(PluginSettings, AccSettings);

	Assert.AreEqual(2, Result.OperationErrorMode);
end;

procedure TCloudSettingsTest.TestCreateFromSettingsCopiesRetryAttempts;
var
	PluginSettings: TPluginSettings;
	AccSettings: TAccountSettings;
	Result: TCloudSettings;
begin
	PluginSettings := Default(TPluginSettings);
	PluginSettings.RetryAttempts := 5;
	AccSettings := Default(TAccountSettings);

	Result := TCloudSettings.CreateFromSettings(PluginSettings, AccSettings);

	Assert.AreEqual(5, Result.RetryAttempts);
end;

procedure TCloudSettingsTest.TestCreateFromSettingsCopiesAttemptWait;
var
	PluginSettings: TPluginSettings;
	AccSettings: TAccountSettings;
	Result: TCloudSettings;
begin
	PluginSettings := Default(TPluginSettings);
	PluginSettings.AttemptWait := 1000;
	AccSettings := Default(TAccountSettings);

	Result := TCloudSettings.CreateFromSettings(PluginSettings, AccSettings);

	Assert.AreEqual(1000, Result.AttemptWait);
end;

procedure TCloudSettingsTest.TestCreateFromSettingsLeavesCryptFilesPasswordEmpty;
var
	PluginSettings: TPluginSettings;
	AccSettings: TAccountSettings;
	Result: TCloudSettings;
begin
	PluginSettings := Default(TPluginSettings);
	AccSettings := Default(TAccountSettings);

	Result := TCloudSettings.CreateFromSettings(PluginSettings, AccSettings);

	Assert.AreEqual('', Result.CryptFilesPassword, 'CryptFilesPassword should be empty after factory creation');
end;

procedure TCloudSettingsTest.TestCreateFromSettingsWithDefaults;
var
	PluginSettings: TPluginSettings;
	AccSettings: TAccountSettings;
	Result: TCloudSettings;
begin
	PluginSettings := Default(TPluginSettings);
	AccSettings := Default(TAccountSettings);

	Result := TCloudSettings.CreateFromSettings(PluginSettings, AccSettings);

	{All values should be defaults}
	Assert.AreEqual(0, Result.OperationErrorMode);
	Assert.AreEqual(0, Result.RetryAttempts);
	Assert.IsFalse(Result.PrecalculateHash);
	Assert.IsFalse(Result.CheckCRC);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudSettingsTest);

end.
