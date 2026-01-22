unit CMRDirItemListJsonAdapterTest;

interface

uses
	CMRDirItem,
	CMRDirItemList,
	CMRDirItemListJsonAdapter,
	CMRConstants,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCMRDirItemListJsonAdapterTest = class
	private
		const
			{JSON for directory listing with folder and file}
			JSON_DIRECTORY_LISTING = '{"status":200,"body":{"list":[' +
				'{"count":{"folders":1,"files":0},"tree":"316234396237373230303030","name":"subdir","grev":13501,"size":668,"kind":"folder","rev":13500,"type":"folder","home":"/TEST_DIR/subdir"},' +
				'{"mtime":1700490201,"virus_scan":"pass","name":"sign.png","size":43961,"hash":"C172C6E2FF47284FF33F348FEA7EECE532F6C051","kind":"file","type":"file","home":"/TEST_DIR/sign.png"}' +
				']}}';

			{Single folder item}
			JSON_SINGLE_FOLDER = '{"status":200,"body":{"list":[' +
				'{"count":{"folders":2,"files":5},"tree":"abc123","name":"Documents","grev":100,"size":1024,"kind":"folder","rev":99,"type":"folder","home":"/Documents"}' +
				']}}';

			{Single file item}
			JSON_SINGLE_FILE = '{"status":200,"body":{"list":[' +
				'{"mtime":1700000000,"virus_scan":"pass","name":"test.txt","size":500,"hash":"ABCDEF123456","kind":"file","type":"file","home":"/test.txt"}' +
				']}}';

			{Empty list}
			JSON_EMPTY_LIST = '{"status":200,"body":{"list":[]}}';

			{Invalid JSON}
			JSON_INVALID = 'not valid json';

			{Empty string}
			JSON_EMPTY = '';

			{Trash item with deleted_at/from fields}
			JSON_TRASH_ITEM = '{"status":200,"body":{"list":[' +
				'{"name":"deleted_file.txt","size":100,"type":"file","home":"/deleted_file.txt","deleted_at":1700000000,"deleted_from":"/original/path","deleted_by":12345}' +
				']}}';

			{Multiple items for search tests}
			JSON_MULTIPLE_ITEMS = '{"status":200,"body":{"list":[' +
				'{"name":"first.txt","size":100,"type":"file","home":"/folder/first.txt"},' +
				'{"name":"second.txt","size":200,"type":"file","home":"/folder/second.txt"},' +
				'{"name":"third.txt","size":300,"type":"file","home":"/folder/third.txt"}' +
				']}}';
	public
		[Test]
		procedure TestParse_DirectoryListing_ReturnsTrue;
		[Test]
		procedure TestParse_DirectoryListing_ParsesCorrectCount;
		[Test]
		procedure TestParse_SingleFolder_ParsesAllFields;
		[Test]
		procedure TestParse_SingleFile_ParsesAllFields;
		[Test]
		procedure TestParse_EmptyList_ReturnsTrue;
		[Test]
		procedure TestParse_EmptyList_ReturnsEmptyArray;
		[Test]
		procedure TestParse_InvalidJSON_ReturnsFalse;
		[Test]
		procedure TestParse_EmptyString_ReturnsFalse;
		[Test]
		procedure TestParse_TrashItem_ParsesDeletedFields;
		[Test]
		procedure TestParse_MultipleItems_ParsesAllItems;
		[Test]
		procedure TestParse_FolderItem_HasCorrectType;
		[Test]
		procedure TestParse_FileItem_HasCorrectType;
	end;

implementation

procedure TCMRDirItemListJsonAdapterTest.TestParse_DirectoryListing_ReturnsTrue;
var
	List: TCMRDirItemList;
begin
	Assert.IsTrue(TCMRDirItemListJsonAdapter.Parse(JSON_DIRECTORY_LISTING, List));
end;

procedure TCMRDirItemListJsonAdapterTest.TestParse_DirectoryListing_ParsesCorrectCount;
var
	List: TCMRDirItemList;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_DIRECTORY_LISTING, List);
	Assert.AreEqual(Integer(2), Integer(Length(List)));
end;

procedure TCMRDirItemListJsonAdapterTest.TestParse_SingleFolder_ParsesAllFields;
var
	List: TCMRDirItemList;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_SINGLE_FOLDER, List);
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	Assert.AreEqual('Documents', List[0].name);
	Assert.AreEqual('folder', List[0].type_);
	Assert.AreEqual('/Documents', List[0].home);
	Assert.AreEqual(Int64(1024), List[0].size);
	Assert.AreEqual(Integer(2), List[0].folders_count);
	Assert.AreEqual(Integer(5), List[0].files_count);
	Assert.AreEqual('abc123', List[0].tree);
	Assert.AreEqual(100, List[0].grev);
	Assert.AreEqual(99, List[0].rev);
end;

procedure TCMRDirItemListJsonAdapterTest.TestParse_SingleFile_ParsesAllFields;
var
	List: TCMRDirItemList;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_SINGLE_FILE, List);
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	Assert.AreEqual('test.txt', List[0].name);
	Assert.AreEqual('file', List[0].type_);
	Assert.AreEqual('/test.txt', List[0].home);
	Assert.AreEqual(Int64(500), List[0].size);
	Assert.AreEqual('ABCDEF123456', List[0].hash);
	Assert.AreEqual('pass', List[0].virus_scan);
	Assert.AreEqual(Int64(1700000000), List[0].mtime);
end;

procedure TCMRDirItemListJsonAdapterTest.TestParse_EmptyList_ReturnsTrue;
var
	List: TCMRDirItemList;
begin
	Assert.IsTrue(TCMRDirItemListJsonAdapter.Parse(JSON_EMPTY_LIST, List));
end;

procedure TCMRDirItemListJsonAdapterTest.TestParse_EmptyList_ReturnsEmptyArray;
var
	List: TCMRDirItemList;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_EMPTY_LIST, List);
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRDirItemListJsonAdapterTest.TestParse_InvalidJSON_ReturnsFalse;
var
	List: TCMRDirItemList;
begin
	Assert.IsFalse(TCMRDirItemListJsonAdapter.Parse(JSON_INVALID, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRDirItemListJsonAdapterTest.TestParse_EmptyString_ReturnsFalse;
var
	List: TCMRDirItemList;
begin
	Assert.IsFalse(TCMRDirItemListJsonAdapter.Parse(JSON_EMPTY, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRDirItemListJsonAdapterTest.TestParse_TrashItem_ParsesDeletedFields;
var
	List: TCMRDirItemList;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_TRASH_ITEM, List);
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	Assert.AreEqual('deleted_file.txt', List[0].name);
	Assert.AreEqual(Integer(1700000000), List[0].deleted_at);
	Assert.AreEqual('/original/path', List[0].deleted_from);
end;

procedure TCMRDirItemListJsonAdapterTest.TestParse_MultipleItems_ParsesAllItems;
var
	List: TCMRDirItemList;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List);
	Assert.AreEqual(Integer(3), Integer(Length(List)));

	Assert.AreEqual('first.txt', List[0].name);
	Assert.AreEqual(Int64(100), List[0].size);

	Assert.AreEqual('second.txt', List[1].name);
	Assert.AreEqual(Int64(200), List[1].size);

	Assert.AreEqual('third.txt', List[2].name);
	Assert.AreEqual(Int64(300), List[2].size);
end;

procedure TCMRDirItemListJsonAdapterTest.TestParse_FolderItem_HasCorrectType;
var
	List: TCMRDirItemList;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_SINGLE_FOLDER, List);
	Assert.IsTrue(List[0].isDir);
	Assert.IsFalse(List[0].isFile);
end;

procedure TCMRDirItemListJsonAdapterTest.TestParse_FileItem_HasCorrectType;
var
	List: TCMRDirItemList;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_SINGLE_FILE, List);
	Assert.IsTrue(List[0].isFile);
	Assert.IsFalse(List[0].isDir);
end;

initialization

TDUnitX.RegisterTestFixture(TCMRDirItemListJsonAdapterTest);

end.
