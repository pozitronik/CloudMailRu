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
	end;

implementation

uses
	System.SysUtils;

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

initialization

TDUnitX.RegisterTestFixture(TIconHelperTest);

end.
