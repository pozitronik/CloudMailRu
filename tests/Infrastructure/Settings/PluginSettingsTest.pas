unit PluginSettingsTest;

interface

uses
	PluginSettings,
	AccountSettings,
	SettingsConstants,
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

		{ IsThumbnailExtension tests }
		[Test]
		procedure TestIsThumbnailExtension_MatchesJpg;
		[Test]
		procedure TestIsThumbnailExtension_MatchesPng;
		[Test]
		procedure TestIsThumbnailExtension_MatchesMp4;
		[Test]
		procedure TestIsThumbnailExtension_CaseInsensitive;
		[Test]
		procedure TestIsThumbnailExtension_RejectsUnknown;
		[Test]
		procedure TestIsThumbnailExtension_RejectsEmpty;
		[Test]
		procedure TestIsThumbnailExtension_EmptyList;
		[Test]
		procedure TestIsThumbnailExtension_CustomList;
		[Test]
		procedure TestIsThumbnailExtension_NoSubstringMatch;
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

{ IsThumbnailExtension tests }

procedure TPluginSettingsTest.TestIsThumbnailExtension_MatchesJpg;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ThumbnailExtensions := DEFAULT_THUMBNAIL_EXTENSIONS;
	Settings.BuildThumbnailExtList;
	Assert.IsTrue(Settings.IsThumbnailExtension('.jpg'));
end;

procedure TPluginSettingsTest.TestIsThumbnailExtension_MatchesPng;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ThumbnailExtensions := DEFAULT_THUMBNAIL_EXTENSIONS;
	Settings.BuildThumbnailExtList;
	Assert.IsTrue(Settings.IsThumbnailExtension('.png'));
end;

procedure TPluginSettingsTest.TestIsThumbnailExtension_MatchesMp4;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ThumbnailExtensions := DEFAULT_THUMBNAIL_EXTENSIONS;
	Settings.BuildThumbnailExtList;
	Assert.IsTrue(Settings.IsThumbnailExtension('.mp4'));
end;

procedure TPluginSettingsTest.TestIsThumbnailExtension_CaseInsensitive;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ThumbnailExtensions := DEFAULT_THUMBNAIL_EXTENSIONS;
	Settings.BuildThumbnailExtList;
	Assert.IsTrue(Settings.IsThumbnailExtension('.JPG'));
	Assert.IsTrue(Settings.IsThumbnailExtension('.Png'));
	Assert.IsTrue(Settings.IsThumbnailExtension('.HEIC'));
end;

procedure TPluginSettingsTest.TestIsThumbnailExtension_RejectsUnknown;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ThumbnailExtensions := DEFAULT_THUMBNAIL_EXTENSIONS;
	Settings.BuildThumbnailExtList;
	Assert.IsFalse(Settings.IsThumbnailExtension('.txt'));
	Assert.IsFalse(Settings.IsThumbnailExtension('.doc'));
	Assert.IsFalse(Settings.IsThumbnailExtension('.zip'));
end;

procedure TPluginSettingsTest.TestIsThumbnailExtension_RejectsEmpty;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ThumbnailExtensions := DEFAULT_THUMBNAIL_EXTENSIONS;
	Settings.BuildThumbnailExtList;
	Assert.IsFalse(Settings.IsThumbnailExtension(''));
end;

procedure TPluginSettingsTest.TestIsThumbnailExtension_EmptyList;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ThumbnailExtensions := '';
	Settings.BuildThumbnailExtList;
	Assert.IsFalse(Settings.IsThumbnailExtension('.jpg'));
end;

procedure TPluginSettingsTest.TestIsThumbnailExtension_NoSubstringMatch;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ThumbnailExtensions := '.jpg,.png';
	Settings.BuildThumbnailExtList;
	{.jpga contains .jpg as substring but must not match}
	Assert.IsFalse(Settings.IsThumbnailExtension('.jpga'));
	Assert.IsFalse(Settings.IsThumbnailExtension('.pngo'));
end;

procedure TPluginSettingsTest.TestIsThumbnailExtension_CustomList;
var
	Settings: TPluginSettings;
begin
	Settings := Default(TPluginSettings);
	Settings.ThumbnailExtensions := '.svg,.psd';
	Settings.BuildThumbnailExtList;
	Assert.IsTrue(Settings.IsThumbnailExtension('.svg'));
	Assert.IsTrue(Settings.IsThumbnailExtension('.psd'));
	Assert.IsFalse(Settings.IsThumbnailExtension('.jpg'));
end;

initialization

TDUnitX.RegisterTestFixture(TPluginSettingsTest);

end.
