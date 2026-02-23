unit CloudDirItemTest;

interface

uses
	CloudDirItem,
	CloudDirItemList,
	CloudDirItemListJsonAdapter,
	CloudConstants,
	TestHelper,
	SysUtils,
	Windows,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCloudDirItemTest = class
	private
		FDirectoryListingJSON: WideString;
		FBrokenJSON: WideString;
	public
		[Setup]
		procedure Setup;
		[Test]
		procedure TestNoneCreation;
		[Test]
		procedure TestIsNoneProperty;
		[Test]
		procedure TestIsDirProperty;
		[Test]
		procedure TestIsFileProperty;
		[Test]
		procedure TestIsPublishedProperty;

		{ToFindData tests}
		[Test]
		procedure TestToFindData_FileWithMtime;
		[Test]
		procedure TestToFindData_Directory;
		[Test]
		procedure TestToFindData_DirectoryAsSymlink;
		[Test]
		procedure TestToFindData_TrashBinItem;
		[Test]
		procedure TestToFindData_TrashBinDirectory;
		[Test]
		procedure TestToFindData_SharedFolder;
		[Test]
		procedure TestToFindData_LargeFileSize;
		[Test]
		procedure TestToFindData_FileName;
	end;

	[TestFixture]
	TCloudDirItemListTest = class
	private
		FDirectoryListingJSON: WideString;
		FBrokenJSON: WideString;
	public
		[Setup]
		procedure Setup;
		[Test]
		procedure TestFromJSONValid;
		[Test]
		procedure TestFromJSONBroken;
		[Test]
		procedure TestFromJSONEmpty;
		[Test]
		procedure TestFindByName;
		[Test]
		procedure TestFindByNameNotFound;
		[Test]
		procedure TestFindByHomePath;
		[Test]
		procedure TestFindByHomePathNotFound;
		[Test]
		procedure TestFileItemProperties;
		[Test]
		procedure TestFolderItemProperties;
	end;

implementation

{ TCloudDirItemTest }

procedure TCloudDirItemTest.Setup;
begin
	DataFileContents('DIRECTORY_LISTING.JSON', FDirectoryListingJSON);
	DataFileContents('BROKEN_DIRECTORY_LISTING.JSON', FBrokenJSON);
end;

procedure TCloudDirItemTest.TestNoneCreation;
var
	Item: TCloudDirItem;
begin
	Item := TCloudDirItem.None;
	Assert.IsTrue(Item.isNone);
	Assert.IsEmpty(Item.name);
	Assert.IsEmpty(Item.type_);
end;

procedure TCloudDirItemTest.TestIsNoneProperty;
var
	Item: TCloudDirItem;
begin
	Item := TCloudDirItem.None;
	Assert.IsTrue(Item.isNone);

	Item.name := 'somefile.txt';
	Assert.IsFalse(Item.isNone);
end;

procedure TCloudDirItemTest.TestIsDirProperty;
var
	Item: TCloudDirItem;
begin
	Item := TCloudDirItem.None;
	Item.type_ := TYPE_DIR;
	Assert.IsTrue(Item.isDir);
	Assert.IsFalse(Item.isFile);

	Item.type_ := TYPE_FILE;
	Assert.IsFalse(Item.isDir);
end;

procedure TCloudDirItemTest.TestIsFileProperty;
var
	Item: TCloudDirItem;
begin
	Item := TCloudDirItem.None;
	Item.type_ := TYPE_FILE;
	Assert.IsTrue(Item.isFile);
	Assert.IsFalse(Item.isDir);

	Item.type_ := TYPE_DIR;
	Assert.IsFalse(Item.isFile);
end;

procedure TCloudDirItemTest.TestIsPublishedProperty;
var
	Item: TCloudDirItem;
begin
	Item := TCloudDirItem.None;
	Assert.IsFalse(Item.isPublished);

	Item.weblink := '/public/ABC123';
	Assert.IsTrue(Item.isPublished);
end;

procedure TCloudDirItemTest.TestToFindData_FileWithMtime;
var
	Item: TCloudDirItem;
	FindData: tWIN32FINDDATAW;
begin
	Item := TCloudDirItem.None;
	Item.name := 'test.txt';
	Item.type_ := TYPE_FILE;
	Item.size := 1234;
	Item.mtime := 1700000000; {Unix timestamp}

	FindData := Item.ToFindData;

	Assert.AreEqual(DWORD(0), FindData.dwFileAttributes);
	Assert.AreEqual(DWORD(1234), FindData.nFileSizeLow);
	Assert.AreEqual(DWORD(0), FindData.nFileSizeHigh);
	Assert.AreNotEqual(Int64(0), Int64(FindData.ftLastWriteTime.dwLowDateTime) or Int64(FindData.ftLastWriteTime.dwHighDateTime shl 32));
end;

procedure TCloudDirItemTest.TestToFindData_Directory;
var
	Item: TCloudDirItem;
	FindData: tWIN32FINDDATAW;
begin
	Item := TCloudDirItem.None;
	Item.name := 'mydir';
	Item.type_ := TYPE_DIR;

	FindData := Item.ToFindData(False); {DirsAsSymlinks = False}

	Assert.AreEqual(FILE_ATTRIBUTE_DIRECTORY, FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY);
end;

procedure TCloudDirItemTest.TestToFindData_DirectoryAsSymlink;
var
	Item: TCloudDirItem;
	FindData: tWIN32FINDDATAW;
begin
	Item := TCloudDirItem.None;
	Item.name := 'mydir';
	Item.type_ := TYPE_DIR;

	FindData := Item.ToFindData(True); {DirsAsSymlinks = True}

	Assert.AreEqual(DWORD(0), FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY);
end;

procedure TCloudDirItemTest.TestToFindData_TrashBinItem;
var
	Item: TCloudDirItem;
	FindData: tWIN32FINDDATAW;
begin
	Item := TCloudDirItem.None;
	Item.name := 'deleted_file.txt';
	Item.type_ := TYPE_FILE;
	Item.deleted_from := '/original/path';
	Item.deleted_at := 1700000000;

	FindData := Item.ToFindData;

	{Deleted file should have creation time set}
	Assert.AreNotEqual(Int64(0), Int64(FindData.ftCreationTime.dwLowDateTime) or Int64(FindData.ftCreationTime.dwHighDateTime shl 32));
end;

procedure TCloudDirItemTest.TestToFindData_TrashBinDirectory;
var
	Item: TCloudDirItem;
	FindData: tWIN32FINDDATAW;
begin
	Item := TCloudDirItem.None;
	Item.name := 'deleted_folder';
	Item.type_ := TYPE_DIR;
	Item.deleted_from := '/original/path';
	Item.deleted_at := 1700000000;

	FindData := Item.ToFindData;

	{ Deleted directories should have FILE_ATTRIBUTE_DIRECTORY set }
	Assert.AreEqual(FILE_ATTRIBUTE_DIRECTORY, FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY);
	{ And should have creation time set from deleted_at }
	Assert.AreNotEqual(Int64(0), Int64(FindData.ftCreationTime.dwLowDateTime) or
		Int64(FindData.ftCreationTime.dwHighDateTime shl 32));
end;

procedure TCloudDirItemTest.TestToFindData_SharedFolder;
var
	Item: TCloudDirItem;
	FindData: tWIN32FINDDATAW;
begin
	Item := TCloudDirItem.None;
	Item.name := 'shared_folder';
	Item.type_ := TYPE_FILE; {Shared items might not be TYPE_DIR}
	Item.kind := KIND_SHARED;

	FindData := Item.ToFindData(False);

	{Shared folders should have directory attribute}
	Assert.AreEqual(FILE_ATTRIBUTE_DIRECTORY, FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY);
end;

procedure TCloudDirItemTest.TestToFindData_LargeFileSize;
var
	Item: TCloudDirItem;
	FindData: tWIN32FINDDATAW;
	LargeSize: Int64;
begin
	Item := TCloudDirItem.None;
	Item.name := 'large_file.iso';
	Item.type_ := TYPE_FILE;
	LargeSize := Int64(10) * 1024 * 1024 * 1024; {10 GB}
	Item.size := LargeSize;
	Item.mtime := 1700000000;

	FindData := Item.ToFindData;

	Assert.AreEqual(DWORD(LargeSize and $FFFFFFFF), FindData.nFileSizeLow);
	Assert.AreEqual(DWORD((LargeSize shr 32) and $FFFFFFFF), FindData.nFileSizeHigh);
end;

procedure TCloudDirItemTest.TestToFindData_FileName;
var
	Item: TCloudDirItem;
	FindData: tWIN32FINDDATAW;
begin
	Item := TCloudDirItem.None;
	Item.name := 'My Document.txt';
	Item.type_ := TYPE_FILE;
	Item.size := 100;
	Item.mtime := 1700000000;

	FindData := Item.ToFindData;

	Assert.AreEqual('My Document.txt', string(FindData.cFileName));
end;

{ TCloudDirItemListTest }

procedure TCloudDirItemListTest.Setup;
begin
	DataFileContents('DIRECTORY_LISTING.JSON', FDirectoryListingJSON);
	DataFileContents('BROKEN_DIRECTORY_LISTING.JSON', FBrokenJSON);
end;

procedure TCloudDirItemListTest.TestFromJSONValid;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List));
	Assert.AreEqual(Integer(2), Integer(Length(List)));
end;

procedure TCloudDirItemListTest.TestFromJSONBroken;
var
	List: TCloudDirItemList;
begin
	Assert.IsFalse(TCloudDirItemListJsonAdapter.Parse(FBrokenJSON, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudDirItemListTest.TestFromJSONEmpty;
var
	List: TCloudDirItemList;
begin
	Assert.IsFalse(TCloudDirItemListJsonAdapter.Parse('', List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudDirItemListTest.TestFindByName;
var
	List: TCloudDirItemList;
	Found: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List);

	Found := List.FindByName('subdir');
	Assert.IsFalse(Found.isNone);
	Assert.AreEqual('subdir', Found.name);
	Assert.IsTrue(Found.isDir);
end;

procedure TCloudDirItemListTest.TestFindByNameNotFound;
var
	List: TCloudDirItemList;
	Found: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List);

	Found := List.FindByName('nonexistent');
	Assert.IsTrue(Found.isNone);
end;

procedure TCloudDirItemListTest.TestFindByHomePath;
var
	List: TCloudDirItemList;
	Found: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List);

	{ FindByHomePath converts backslashes to forward slashes and prepends / }
	Found := List.FindByHomePath('TEST_DIR\sign.png');
	Assert.IsFalse(Found.isNone);
	Assert.AreEqual('sign.png', Found.name);
end;

procedure TCloudDirItemListTest.TestFindByHomePathNotFound;
var
	List: TCloudDirItemList;
	Found: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List);

	Found := List.FindByHomePath('nonexistent\path');
	Assert.IsTrue(Found.isNone);
end;

procedure TCloudDirItemListTest.TestFileItemProperties;
var
	List: TCloudDirItemList;
	FileItem: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List);

	FileItem := List.FindByName('sign.png');
	Assert.IsFalse(FileItem.isNone);
	Assert.IsTrue(FileItem.isFile);
	Assert.AreEqual(TYPE_FILE, FileItem.type_);
	Assert.AreEqual(Int64(43961), FileItem.size);
	Assert.AreEqual('C172C6E2FF47284FF33F348FEA7EECE532F6C051', FileItem.hash);
	Assert.AreEqual('pass', FileItem.virus_scan);
	Assert.AreEqual(Int64(1700490201), FileItem.mtime);
end;

procedure TCloudDirItemListTest.TestFolderItemProperties;
var
	List: TCloudDirItemList;
	FolderItem: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List);

	FolderItem := List.FindByName('subdir');
	Assert.IsFalse(FolderItem.isNone);
	Assert.IsTrue(FolderItem.isDir);
	Assert.AreEqual(TYPE_DIR, FolderItem.type_);
	Assert.AreEqual(Integer(1), FolderItem.folders_count);
	Assert.AreEqual(Integer(0), FolderItem.files_count);
	Assert.AreEqual('/TEST_DIR/subdir', FolderItem.home);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudDirItemTest);
TDUnitX.RegisterTestFixture(TCloudDirItemListTest);

end.
