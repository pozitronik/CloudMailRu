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
		{ LoadIcon tests }
		[Test]
		procedure TestLoadIconNonExistentFile;
		[Test]
		procedure TestLoadIconEmptyPath;

		{ LoadPluginIcon tests }
		[Test]
		procedure TestLoadPluginIconNonExistentFile;
		[Test]
		procedure TestLoadPluginIconPathConstruction;
	end;

implementation

uses
	System.SysUtils;

{ TIconHelperTest }

{ LoadIcon tests }

procedure TIconHelperTest.TestLoadIconNonExistentFile;
var
	Icon: HIcon;
begin
	{ Loading non-existent file should return INVALID_HANDLE_VALUE }
	Icon := IconHelper.LoadIcon('Z:\NonExistent\Path\icon.ico');
	Assert.IsTrue(Icon = INVALID_HANDLE_VALUE, 'Expected INVALID_HANDLE_VALUE for non-existent file');
end;

procedure TIconHelperTest.TestLoadIconEmptyPath;
var
	Icon: HIcon;
begin
	{ Empty path should return INVALID_HANDLE_VALUE }
	Icon := IconHelper.LoadIcon('');
	Assert.IsTrue(Icon = INVALID_HANDLE_VALUE, 'Expected INVALID_HANDLE_VALUE for empty path');
end;

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

initialization

TDUnitX.RegisterTestFixture(TIconHelperTest);

end.
