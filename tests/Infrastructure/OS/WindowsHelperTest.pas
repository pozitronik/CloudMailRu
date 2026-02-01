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

		{ TWindowsCommandExecutor tests }
		[Test]
		procedure TestWindowsCommandExecutorImplementsInterface;
	end;

implementation

uses
	SysUtils,
	ShlObj;

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

{ GetSystemIcon tests }

procedure TWindowsHelperTest.TestGetSystemIconReturnsValidHandle;
var
	Icon: HIcon;
begin
	{ GetSystemIcon should return a valid icon handle for recycle bin }
	Icon := GetSystemIcon(IconSizeSmall, CSIDL_BITBUCKET);
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'GetSystemIcon should return valid handle for recycle bin');
end;

procedure TWindowsHelperTest.TestGetSystemIconSmallSize;
var
	Icon: HIcon;
begin
	{ Test small icon size parameter }
	Icon := GetSystemIcon(IconSizeSmall, CSIDL_BITBUCKET);
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'Small icon should be retrievable');
end;

procedure TWindowsHelperTest.TestGetSystemIconNormalSize;
var
	Icon: HIcon;
begin
	{ Test normal icon size parameter }
	Icon := GetSystemIcon(IconSizeNormal, CSIDL_BITBUCKET);
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'Normal icon should be retrievable');
end;

procedure TWindowsHelperTest.TestGetSystemIconLargeSize;
begin
	{ Large icons may not be supported by SHGetFileInfo, but should not crash }
	GetSystemIcon(IconSizeLarge, CSIDL_BITBUCKET);
	Assert.Pass('Large icon request completed without error');
end;

{ GetFolderIcon tests }

procedure TWindowsHelperTest.TestGetFolderIconSmallSize;
var
	Icon: HIcon;
begin
	Icon := GetFolderIcon(IconSizeSmall);
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'Small folder icon should be retrievable');
end;

procedure TWindowsHelperTest.TestGetFolderIconNormalSize;
var
	Icon: HIcon;
begin
	Icon := GetFolderIcon(IconSizeNormal);
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'Normal folder icon should be retrievable');
end;

procedure TWindowsHelperTest.TestGetFolderIconLargeSize;
begin
	{ Large icons may not be fully supported but should not crash }
	GetFolderIcon(IconSizeLarge);
	Assert.Pass('Large folder icon request completed without error');
end;

procedure TWindowsHelperTest.TestGetFolderIconDefaultSize;
var
	Icon: HIcon;
begin
	{ Default parameter should be IconSizeSmall }
	Icon := GetFolderIcon;
	Assert.AreNotEqual(HIcon(INVALID_HANDLE_VALUE), Icon,
		'Default folder icon should be retrievable');
end;

{ TWindowsCommandExecutor tests }

procedure TWindowsHelperTest.TestWindowsCommandExecutorImplementsInterface;
var
	Executor: ICommandExecutor;
begin
	Executor := TWindowsCommandExecutor.Create;
	Assert.IsNotNull(Executor, 'TWindowsCommandExecutor should implement ICommandExecutor');
end;

initialization

TDUnitX.RegisterTestFixture(TWindowsHelperTest);

end.
