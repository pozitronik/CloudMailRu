unit WindowsHelperTest;

interface

uses
	WindowsHelper,
	Windows,
	DUnitX.TestFramework;

type

	[TestFixture]
	TWindowsHelperTest = class
	public
		{ GetFindDataEmptyDir tests }
		[Test]
		procedure TestGetFindDataEmptyDirDefaultName;
		[Test]
		procedure TestGetFindDataEmptyDirCustomName;
		[Test]
		procedure TestGetFindDataEmptyDirHasDirectoryAttribute;
		[Test]
		procedure TestGetFindDataEmptyDirSizeIsZero;

	end;

implementation

{ GetFindDataEmptyDir tests }

procedure TWindowsHelperTest.TestGetFindDataEmptyDirDefaultName;
var
	FindData: tWIN32FINDDATAW;
begin
	{ Default name is '.' }
	FindData := GetFindDataEmptyDir;
	Assert.AreEqual('.', string(FindData.cFileName));
end;

procedure TWindowsHelperTest.TestGetFindDataEmptyDirCustomName;
var
	FindData: tWIN32FINDDATAW;
begin
	{ Custom directory name }
	FindData := GetFindDataEmptyDir('MyFolder');
	Assert.AreEqual('MyFolder', string(FindData.cFileName));
end;

procedure TWindowsHelperTest.TestGetFindDataEmptyDirHasDirectoryAttribute;
var
	FindData: tWIN32FINDDATAW;
begin
	{ Should have FILE_ATTRIBUTE_DIRECTORY flag set }
	FindData := GetFindDataEmptyDir;
	Assert.IsTrue((FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;

procedure TWindowsHelperTest.TestGetFindDataEmptyDirSizeIsZero;
var
	FindData: tWIN32FINDDATAW;
begin
	{ Empty directory should have zero size }
	FindData := GetFindDataEmptyDir;
	Assert.AreEqual(Cardinal(0), FindData.nFileSizeLow);
	Assert.AreEqual(Cardinal(0), FindData.nFileSizeHigh);
end;

initialization

TDUnitX.RegisterTestFixture(TWindowsHelperTest);

end.
