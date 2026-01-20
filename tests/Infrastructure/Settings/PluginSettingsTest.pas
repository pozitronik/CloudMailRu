unit PluginSettingsTest;

interface

uses
	PluginSettings,
	AccountSettings,
	DUnitX.TestFramework;

type

	[TestFixture]
	TPluginSettingsTest = class
	public
		{ GetEnabledVirtualTypes tests }
		[Test]
		procedure TestEnabledVirtualTypesNone;
		[Test]
		procedure TestEnabledVirtualTypesTrashOnly;
		[Test]
		procedure TestEnabledVirtualTypesSharedOnly;
		[Test]
		procedure TestEnabledVirtualTypesInvitesOnly;
		[Test]
		procedure TestEnabledVirtualTypesTrashAndShared;
		[Test]
		procedure TestEnabledVirtualTypesAll;
	end;

implementation

{ GetEnabledVirtualTypes tests }

procedure TPluginSettingsTest.TestEnabledVirtualTypesNone;
var
	Settings: TPluginSettings;
begin
	{ All virtual folders disabled }
	Settings := Default(TPluginSettings);
	Settings.ShowTrashFolders := False;
	Settings.ShowSharedFolders := False;
	Settings.ShowInvitesFolders := False;

	Assert.AreEqual(0, Integer(Byte(Settings.EnabledVirtualTypes)));
end;

procedure TPluginSettingsTest.TestEnabledVirtualTypesTrashOnly;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ShowTrashFolders := True;
	Settings.ShowSharedFolders := False;
	Settings.ShowInvitesFolders := False;

	Assert.IsTrue(VTTrash in Settings.EnabledVirtualTypes);
	Assert.IsFalse(VTShared in Settings.EnabledVirtualTypes);
	Assert.IsFalse(VTInvites in Settings.EnabledVirtualTypes);
end;

procedure TPluginSettingsTest.TestEnabledVirtualTypesSharedOnly;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ShowTrashFolders := False;
	Settings.ShowSharedFolders := True;
	Settings.ShowInvitesFolders := False;

	Assert.IsFalse(VTTrash in Settings.EnabledVirtualTypes);
	Assert.IsTrue(VTShared in Settings.EnabledVirtualTypes);
	Assert.IsFalse(VTInvites in Settings.EnabledVirtualTypes);
end;

procedure TPluginSettingsTest.TestEnabledVirtualTypesInvitesOnly;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ShowTrashFolders := False;
	Settings.ShowSharedFolders := False;
	Settings.ShowInvitesFolders := True;

	Assert.IsFalse(VTTrash in Settings.EnabledVirtualTypes);
	Assert.IsFalse(VTShared in Settings.EnabledVirtualTypes);
	Assert.IsTrue(VTInvites in Settings.EnabledVirtualTypes);
end;

procedure TPluginSettingsTest.TestEnabledVirtualTypesTrashAndShared;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ShowTrashFolders := True;
	Settings.ShowSharedFolders := True;
	Settings.ShowInvitesFolders := False;

	Assert.IsTrue(VTTrash in Settings.EnabledVirtualTypes);
	Assert.IsTrue(VTShared in Settings.EnabledVirtualTypes);
	Assert.IsFalse(VTInvites in Settings.EnabledVirtualTypes);
end;

procedure TPluginSettingsTest.TestEnabledVirtualTypesAll;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ShowTrashFolders := True;
	Settings.ShowSharedFolders := True;
	Settings.ShowInvitesFolders := True;

	Assert.IsTrue(VTTrash in Settings.EnabledVirtualTypes);
	Assert.IsTrue(VTShared in Settings.EnabledVirtualTypes);
	Assert.IsTrue(VTInvites in Settings.EnabledVirtualTypes);
end;

initialization

TDUnitX.RegisterTestFixture(TPluginSettingsTest);

end.
