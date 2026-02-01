unit WFXTypesTest;

interface

uses
	WFXTypes,
	Windows,
	DUnitX.TestFramework;

type

	[TestFixture]
	TWFXTypesTest = class
	public
		{ InitAsEmptyDir tests }
		[Test]
		procedure TestInitAsEmptyDirDefaultName;
		[Test]
		procedure TestInitAsEmptyDirCustomName;
		[Test]
		procedure TestInitAsEmptyDirHasDirectoryAttribute;
		[Test]
		procedure TestInitAsEmptyDirSizeIsZero;
	end;

implementation

{ TWFXTypesTest }

procedure TWFXTypesTest.TestInitAsEmptyDirDefaultName;
var
	FindData: tWIN32FINDDATAW;
begin
	{ Default name is '.' }
	FindData.InitAsEmptyDir;
	Assert.AreEqual('.', string(FindData.cFileName));
end;

procedure TWFXTypesTest.TestInitAsEmptyDirCustomName;
var
	FindData: tWIN32FINDDATAW;
begin
	{ Custom directory name }
	FindData.InitAsEmptyDir('MyFolder');
	Assert.AreEqual('MyFolder', string(FindData.cFileName));
end;

procedure TWFXTypesTest.TestInitAsEmptyDirHasDirectoryAttribute;
var
	FindData: tWIN32FINDDATAW;
begin
	{ Should have FILE_ATTRIBUTE_DIRECTORY flag set }
	FindData.InitAsEmptyDir;
	Assert.IsTrue((FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;

procedure TWFXTypesTest.TestInitAsEmptyDirSizeIsZero;
var
	FindData: tWIN32FINDDATAW;
begin
	{ Empty directory should have zero size }
	FindData.InitAsEmptyDir;
	Assert.AreEqual(Cardinal(0), FindData.nFileSizeLow);
	Assert.AreEqual(Cardinal(0), FindData.nFileSizeHigh);
end;

initialization

TDUnitX.RegisterTestFixture(TWFXTypesTest);

end.
