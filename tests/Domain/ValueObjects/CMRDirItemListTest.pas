unit CMRDirItemListTest;

interface

uses
	CMRDirItemList,
	CMRDirItemListJsonAdapter,
	CMRDirItem,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCMRDirItemListTest = class
	public
		[Test]
		procedure TestFromJSONValid;
		[Test]
		procedure TestFromJSONWithFolder;
		[Test]
		procedure TestFromJSONWithFile;
		[Test]
		procedure TestFromJSONWithTrashItem;
		[Test]
		procedure TestFromJSONEmpty;
		[Test]
		procedure TestFromJSONInvalid;
		[Test]
		procedure TestFindByNameFound;
		[Test]
		procedure TestFindByNameNotFound;
		[Test]
		procedure TestFindByNameEmptyList;
		[Test]
		procedure TestFindByHomePathFound;
		[Test]
		procedure TestFindByHomePathWithBackslash;
		[Test]
		procedure TestFindByHomePathNotFound;
		[Test]
		procedure TestFindByHomePathEmptyList;
		[Test]
		procedure TestFromJSONMultipleItems;
	end;

implementation

const
	{ Directory listing with folder and file }
	JSON_DIRECTORY_LISTING = '{"status":200,"body":{"list":[' +
		'{"count":{"folders":1,"files":0},"tree":"316234396237373230303030","name":"subdir","grev":13501,"size":668,"kind":"folder","rev":13500,"type":"folder","home":"/TEST_DIR/subdir"},' +
		'{"mtime":1700490201,"virus_scan":"pass","name":"sign.png","size":43961,"hash":"C172C6E2FF47284FF33F348FEA7EECE532F6C051","kind":"file","type":"file","home":"/TEST_DIR/sign.png"}' +
		']}}';

	{ Single folder item }
	JSON_FOLDER_ITEM = '{"status":200,"body":{"list":[' +
		'{"count":{"folders":2,"files":5},"tree":"abc123","name":"Documents","grev":100,"size":1024,"kind":"folder","rev":99,"type":"folder","home":"/Documents"}' +
		']}}';

	{ Single file item }
	JSON_FILE_ITEM = '{"status":200,"body":{"list":[' +
		'{"mtime":1700000000,"virus_scan":"pass","name":"test.txt","size":500,"hash":"ABCDEF123456","kind":"file","type":"file","home":"/test.txt"}' +
		']}}';

	{ Trash item with deleted_at/from fields }
	JSON_TRASH_ITEM = '{"status":200,"body":{"list":[' +
		'{"name":"deleted_file.txt","size":100,"type":"file","home":"/deleted_file.txt","deleted_at":1700000000,"deleted_from":"/original/path","deleted_by":12345}' +
		']}}';

	{ Empty list }
	JSON_EMPTY_LIST = '{"status":200,"body":{"list":[]}}';

	{ Multiple items for search tests }
	JSON_MULTIPLE_ITEMS = '{"status":200,"body":{"list":[' +
		'{"name":"first.txt","size":100,"type":"file","home":"/folder/first.txt"},' +
		'{"name":"second.txt","size":200,"type":"file","home":"/folder/second.txt"},' +
		'{"name":"third.txt","size":300,"type":"file","home":"/folder/third.txt"}' +
		']}}';

procedure TCMRDirItemListTest.TestFromJSONValid;
var
	List: TCMRDirItemList;
begin
	Assert.IsTrue(TCMRDirItemListJsonAdapter.Parse(JSON_DIRECTORY_LISTING, List));

	Assert.AreEqual(Integer(2), Integer(Length(List)));
end;

procedure TCMRDirItemListTest.TestFromJSONWithFolder;
var
	List: TCMRDirItemList;
begin
	Assert.IsTrue(TCMRDirItemListJsonAdapter.Parse(JSON_FOLDER_ITEM, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	Assert.AreEqual('Documents', List[0].name);
	Assert.AreEqual('folder', List[0].type_);
	Assert.AreEqual('/Documents', List[0].home);
	Assert.AreEqual(Int64(1024), List[0].size);
	Assert.AreEqual(Integer(2), List[0].folders_count);
	Assert.AreEqual(Integer(5), List[0].files_count);
	Assert.AreEqual('abc123', List[0].tree);
end;

procedure TCMRDirItemListTest.TestFromJSONWithFile;
var
	List: TCMRDirItemList;
begin
	Assert.IsTrue(TCMRDirItemListJsonAdapter.Parse(JSON_FILE_ITEM, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	Assert.AreEqual('test.txt', List[0].name);
	Assert.AreEqual('file', List[0].type_);
	Assert.AreEqual('/test.txt', List[0].home);
	Assert.AreEqual(Int64(500), List[0].size);
	Assert.AreEqual('ABCDEF123456', List[0].hash);
	Assert.AreEqual('pass', List[0].virus_scan);
	Assert.AreEqual(Int64(1700000000), List[0].mtime);
end;

procedure TCMRDirItemListTest.TestFromJSONWithTrashItem;
var
	List: TCMRDirItemList;
begin
	Assert.IsTrue(TCMRDirItemListJsonAdapter.Parse(JSON_TRASH_ITEM, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	Assert.AreEqual('deleted_file.txt', List[0].name);
	Assert.AreEqual(Integer(1700000000), List[0].deleted_at);
	Assert.AreEqual('/original/path', List[0].deleted_from);
	{ deleted_by is integer in the record - test uses integer comparison }
end;

procedure TCMRDirItemListTest.TestFromJSONEmpty;
var
	List: TCMRDirItemList;
begin
	Assert.IsTrue(TCMRDirItemListJsonAdapter.Parse(JSON_EMPTY_LIST, List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRDirItemListTest.TestFromJSONInvalid;
var
	List: TCMRDirItemList;
begin
	Assert.IsFalse(TCMRDirItemListJsonAdapter.Parse('invalid json', List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRDirItemListTest.TestFindByNameFound;
var
	List: TCMRDirItemList;
	Item: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List);

	Item := List.FindByName('second.txt');

	Assert.IsFalse(Item.isNone);
	Assert.AreEqual('second.txt', Item.name);
	Assert.AreEqual(Int64(200), Item.size);
end;

procedure TCMRDirItemListTest.TestFindByNameNotFound;
var
	List: TCMRDirItemList;
	Item: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List);

	Item := List.FindByName('nonexistent.txt');

	Assert.IsTrue(Item.isNone);
end;

procedure TCMRDirItemListTest.TestFindByNameEmptyList;
var
	List: TCMRDirItemList;
	Item: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_EMPTY_LIST, List);

	Item := List.FindByName('anything');

	Assert.IsTrue(Item.isNone);
end;

procedure TCMRDirItemListTest.TestFindByHomePathFound;
var
	List: TCMRDirItemList;
	Item: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List);

	{ FindByHomePath adds leading slash, so we pass path without it }
	Item := List.FindByHomePath('folder/second.txt');

	Assert.IsFalse(Item.isNone);
	Assert.AreEqual('second.txt', Item.name);
end;

procedure TCMRDirItemListTest.TestFindByHomePathWithBackslash;
var
	List: TCMRDirItemList;
	Item: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List);

	{ FindByHomePath should convert backslashes to forward slashes }
	Item := List.FindByHomePath('folder\third.txt');

	Assert.IsFalse(Item.isNone);
	Assert.AreEqual('third.txt', Item.name);
end;

procedure TCMRDirItemListTest.TestFindByHomePathNotFound;
var
	List: TCMRDirItemList;
	Item: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List);

	Item := List.FindByHomePath('nonexistent/path.txt');

	Assert.IsTrue(Item.isNone);
end;

procedure TCMRDirItemListTest.TestFindByHomePathEmptyList;
var
	List: TCMRDirItemList;
	Item: TCMRDirItem;
begin
	TCMRDirItemListJsonAdapter.Parse(JSON_EMPTY_LIST, List);

	Item := List.FindByHomePath('any/path');

	Assert.IsTrue(Item.isNone);
end;

procedure TCMRDirItemListTest.TestFromJSONMultipleItems;
var
	List: TCMRDirItemList;
begin
	Assert.IsTrue(TCMRDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List));

	Assert.AreEqual(Integer(3), Integer(Length(List)));
	Assert.AreEqual('first.txt', List[0].name);
	Assert.AreEqual('second.txt', List[1].name);
	Assert.AreEqual('third.txt', List[2].name);
end;

initialization

TDUnitX.RegisterTestFixture(TCMRDirItemListTest);

end.
