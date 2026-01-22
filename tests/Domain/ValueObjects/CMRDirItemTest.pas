unit CMRDirItemTest;

interface

uses
	CMRDirItem,
	CMRDirItemList,
	CMRDirItemListJsonAdapter,
	CMRConstants,
	TestHelper,
	SysUtils,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCMRDirItemTest = class
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
	end;

	[TestFixture]
	TCMRDirItemListTest = class
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

{ TCMRDirItemTest }

procedure TCMRDirItemTest.Setup;
begin
	DataFileContents('DIRECTORY_LISTING.JSON', FDirectoryListingJSON);
	DataFileContents('BROKEN_DIRECTORY_LISTING.JSON', FBrokenJSON);
end;

procedure TCMRDirItemTest.TestNoneCreation;
var
	Item: TCMRDirItem;
begin
	Item := Item.None;
	Assert.IsTrue(Item.isNone);
	Assert.IsEmpty(Item.name);
	Assert.IsEmpty(Item.type_);
end;

procedure TCMRDirItemTest.TestIsNoneProperty;
var
	Item: TCMRDirItem;
begin
	Item := Item.None;
	Assert.IsTrue(Item.isNone);

	Item.name := 'somefile.txt';
	Assert.IsFalse(Item.isNone);
end;

procedure TCMRDirItemTest.TestIsDirProperty;
var
	Item: TCMRDirItem;
begin
	Item := Item.None;
	Item.type_ := TYPE_DIR;
	Assert.IsTrue(Item.isDir);
	Assert.IsFalse(Item.isFile);

	Item.type_ := TYPE_FILE;
	Assert.IsFalse(Item.isDir);
end;

procedure TCMRDirItemTest.TestIsFileProperty;
var
	Item: TCMRDirItem;
begin
	Item := Item.None;
	Item.type_ := TYPE_FILE;
	Assert.IsTrue(Item.isFile);
	Assert.IsFalse(Item.isDir);

	Item.type_ := TYPE_DIR;
	Assert.IsFalse(Item.isFile);
end;

procedure TCMRDirItemTest.TestIsPublishedProperty;
var
	Item: TCMRDirItem;
begin
	Item := Item.None;
	Assert.IsFalse(Item.isPublished);

	Item.weblink := '/public/ABC123';
	Assert.IsTrue(Item.isPublished);
end;

{ TCMRDirItemListTest }

procedure TCMRDirItemListTest.Setup;
begin
	DataFileContents('DIRECTORY_LISTING.JSON', FDirectoryListingJSON);
	DataFileContents('BROKEN_DIRECTORY_LISTING.JSON', FBrokenJSON);
end;

procedure TCMRDirItemListTest.TestFromJSONValid;
var
	List: TCMRDirItemList;
begin
	Assert.IsTrue(TCMRDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List));
	Assert.AreEqual(Integer(2), Integer(Length(List)));
end;

procedure TCMRDirItemListTest.TestFromJSONBroken;
var
	List: TCMRDirItemList;
begin
	Assert.IsFalse(TCMRDirItemListJsonAdapter.Parse(FBrokenJSON, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRDirItemListTest.TestFromJSONEmpty;
var
	List: TCMRDirItemList;
begin
	Assert.IsFalse(TCMRDirItemListJsonAdapter.Parse('', List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRDirItemListTest.TestFindByName;
var
	List: TCMRDirItemList;
	Found: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List);

	Found := List.FindByName('subdir');
	Assert.IsFalse(Found.isNone);
	Assert.AreEqual('subdir', Found.name);
	Assert.IsTrue(Found.isDir);
end;

procedure TCMRDirItemListTest.TestFindByNameNotFound;
var
	List: TCMRDirItemList;
	Found: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List);

	Found := List.FindByName('nonexistent');
	Assert.IsTrue(Found.isNone);
end;

procedure TCMRDirItemListTest.TestFindByHomePath;
var
	List: TCMRDirItemList;
	Found: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List);

	{ FindByHomePath converts backslashes to forward slashes and prepends / }
	Found := List.FindByHomePath('TEST_DIR\sign.png');
	Assert.IsFalse(Found.isNone);
	Assert.AreEqual('sign.png', Found.name);
end;

procedure TCMRDirItemListTest.TestFindByHomePathNotFound;
var
	List: TCMRDirItemList;
	Found: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List);

	Found := List.FindByHomePath('nonexistent\path');
	Assert.IsTrue(Found.isNone);
end;

procedure TCMRDirItemListTest.TestFileItemProperties;
var
	List: TCMRDirItemList;
	FileItem: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List);

	FileItem := List.FindByName('sign.png');
	Assert.IsFalse(FileItem.isNone);
	Assert.IsTrue(FileItem.isFile);
	Assert.AreEqual(TYPE_FILE, FileItem.type_);
	Assert.AreEqual(Int64(43961), FileItem.size);
	Assert.AreEqual('C172C6E2FF47284FF33F348FEA7EECE532F6C051', FileItem.hash);
	Assert.AreEqual('pass', FileItem.virus_scan);
	Assert.AreEqual(Int64(1700490201), FileItem.mtime);
end;

procedure TCMRDirItemListTest.TestFolderItemProperties;
var
	List: TCMRDirItemList;
	FolderItem: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(FDirectoryListingJSON, List);

	FolderItem := List.FindByName('subdir');
	Assert.IsFalse(FolderItem.isNone);
	Assert.IsTrue(FolderItem.isDir);
	Assert.AreEqual(TYPE_DIR, FolderItem.type_);
	Assert.AreEqual(Integer(1), FolderItem.folders_count);
	Assert.AreEqual(Integer(0), FolderItem.files_count);
	Assert.AreEqual('/TEST_DIR/subdir', FolderItem.home);
end;

initialization

TDUnitX.RegisterTestFixture(TCMRDirItemTest);
TDUnitX.RegisterTestFixture(TCMRDirItemListTest);

end.
