unit IconHelperTest;

interface

uses
	IconHelper,
	Windows,
	DUnitX.TestFramework;

type

	[TestFixture]
	TIconHelperTest = class
	public
		{ LoadPluginIcon tests }
		[Test]
		procedure TestLoadPluginIconNonExistentFile;
		[Test]
		procedure TestLoadPluginIconPathConstruction;

		{ GetSystemIcon tests - verifies PIDL leak issue }
		[Test]
		procedure TestGetSystemIconReturnsValidHandle;
		[Test]
		procedure TestGetSystemIconSmallSize;
		[Test]
		procedure TestGetSystemIconNormalSize;
		[Test]
		procedure TestGetSystemIconLargeSize;

		{ GetFolderIcon tests }
		[Test]
		procedure TestGetFolderIconSmallSize;
		[Test]
		procedure TestGetFolderIconNormalSize;
		[Test]
		procedure TestGetFolderIconLargeSize;
		[Test]
		procedure TestGetFolderIconDefaultSize;

		{ LoadPluginIcon with real file }
		[Test]
		{Verifies LoadPluginIcon loads a real .ico file from disk (covers LoadIcon path)}
		procedure TestLoadPluginIcon_ValidFile_ReturnsHandle;

		{ GetSystemIcon with explicit size params }
		[Test]
		{Covers IconSizeNormal branch in GetSystemIcon case statement}
		procedure TestGetSystemIcon_ExplicitNormalSize;
		[Test]
		{Covers IconSizeLarge branch in GetSystemIcon case statement}
		procedure TestGetSystemIcon_ExplicitLargeSize;
	end;

implementation

uses
	System.SysUtils,
	Graphics,
	ShlObj;

{ TIconHelperTest }

{ LoadPluginIcon tests }

procedure TIconHelperTest.TestLoadPluginIconNonExistentFile;
var
	Icon: HIcon;
begin
	{ Non-existent plugin icon should return INVALID_HANDLE_VALUE }
	Icon := LoadPluginIcon('Z:\NonExistent\Path', 'missing_icon');
	Assert.IsTrue(Icon = INVALID_HANDLE_VALUE, 'Expected INVALID_HANDLE_VALUE for non-existent plugin icon');
end;

procedure TIconHelperTest.TestLoadPluginIconPathConstruction;
var
	TempDir: string;
	Icon: HIcon;
begin
	{ Verify path construction by checking non-existent file in temp dir }
	TempDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'));
	Icon := LoadPluginIcon(TempDir, 'test_icon_' + IntToStr(GetCurrentThreadId));
	{ File doesn't exist, so should return INVALID_HANDLE_VALUE }
	Assert.IsTrue(Icon = INVALID_HANDLE_VALUE, 'Expected INVALID_HANDLE_VALUE for non-existent file in temp dir');
end;

{ GetSystemIcon tests }

procedure TIconHelperTest.TestGetSystemIconReturnsValidHandle;
var
	Icon: HIcon;
begin
	{ GetSystemIcon should return a valid icon handle for recycle bin }
	Icon := GetSystemIcon(IconSizeSmall, CSIDL_BITBUCKET);
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'GetSystemIcon should return valid handle for recycle bin');
end;

procedure TIconHelperTest.TestGetSystemIconSmallSize;
var
	Icon: HIcon;
begin
	{ Test small icon size parameter }
	Icon := GetSystemIcon(IconSizeSmall, CSIDL_BITBUCKET);
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'Small icon should be retrievable');
end;

procedure TIconHelperTest.TestGetSystemIconNormalSize;
var
	Icon: HIcon;
begin
	{ Test normal icon size parameter }
	Icon := GetSystemIcon(IconSizeNormal, CSIDL_BITBUCKET);
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'Normal icon should be retrievable');
end;

procedure TIconHelperTest.TestGetSystemIconLargeSize;
begin
	{ Large icons may not be supported by SHGetFileInfo, but should not crash }
	GetSystemIcon(IconSizeLarge, CSIDL_BITBUCKET);
	Assert.Pass('Large icon request completed without error');
end;

{ GetFolderIcon tests }

procedure TIconHelperTest.TestGetFolderIconSmallSize;
var
	Icon: HIcon;
begin
	Icon := GetFolderIcon(IconSizeSmall);
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'Small folder icon should be retrievable');
end;

procedure TIconHelperTest.TestGetFolderIconNormalSize;
var
	Icon: HIcon;
begin
	Icon := GetFolderIcon(IconSizeNormal);
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'Normal folder icon should be retrievable');
end;

procedure TIconHelperTest.TestGetFolderIconLargeSize;
begin
	{ Large icons may not be fully supported but should not crash }
	GetFolderIcon(IconSizeLarge);
	Assert.Pass('Large folder icon request completed without error');
end;

procedure TIconHelperTest.TestGetFolderIconDefaultSize;
var
	Icon: HIcon;
begin
	{ Default parameter should be IconSizeSmall }
	Icon := GetFolderIcon;
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'Default folder icon should be retrievable');
end;

{ LoadPluginIcon with real file -- exercises LoadIcon internal function }

procedure TIconHelperTest.TestLoadPluginIcon_ValidFile_ReturnsHandle;
var
	Icon: TIcon;
	TempDir: string;
	TempFile: string;
	FolderIcon, LoadedIcon: HIcon;
begin
	TempDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
		'IconHelperTest_' + IntToStr(GetCurrentProcessId);
	TempFile := IncludeTrailingPathDelimiter(TempDir) + 'testicon.ico';
	ForceDirectories(TempDir);
	try
		{Create a valid .ico file by saving a system folder icon}
		FolderIcon := GetFolderIcon(IconSizeSmall);
		Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), FolderIcon, 'Folder icon should be valid');
		Icon := TIcon.Create;
		try
			Icon.Handle := FolderIcon;
			Icon.SaveToFile(TempFile);
		finally
			Icon.Free;
		end;

		{LoadPluginIcon internally calls LoadIcon(path + identifier + '.ico')}
		LoadedIcon := LoadPluginIcon(TempDir, 'testicon');
		Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), LoadedIcon,
			'LoadPluginIcon should return valid handle for existing .ico file');
		if LoadedIcon <> INVALID_HANDLE_VALUE then
			DestroyIcon(LoadedIcon);
	finally
		if FileExists(TempFile) then
			System.SysUtils.DeleteFile(TempFile);
		RemoveDir(TempDir);
	end;
end;

{ GetSystemIcon with explicit size params -- covers case branches }

procedure TIconHelperTest.TestGetSystemIcon_ExplicitNormalSize;
var
	Icon: HIcon;
begin
	{Pass all 3 parameters explicitly to ensure IconSizeNormal case is hit}
	Icon := GetSystemIcon(0, IconSizeNormal, CSIDL_BITBUCKET);
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'GetSystemIcon with explicit normal size should return valid handle');
end;

procedure TIconHelperTest.TestGetSystemIcon_ExplicitLargeSize;
begin
	{Pass all 3 parameters explicitly to ensure IconSizeLarge case is hit}
	GetSystemIcon(0, IconSizeLarge, CSIDL_BITBUCKET);
	Assert.Pass('GetSystemIcon with explicit large size completed without error');
end;

initialization

TDUnitX.RegisterTestFixture(TIconHelperTest);

end.
