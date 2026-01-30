unit CloudDirItemListTest;

interface

uses
	CloudDirItemList,
	CloudDirItemListJsonAdapter,
	CloudDirItem,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCloudDirItemListTest = class
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
		{Append tests}
		[Test]
		procedure TestAppend_EmptyToEmpty_StaysEmpty;
		[Test]
		procedure TestAppend_ItemsToEmpty_CopiesAll;
		[Test]
		procedure TestAppend_ItemsToExisting_Accumulates;
		[Test]
		procedure TestAppend_EmptyToExisting_NoChange;
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

procedure TCloudDirItemListTest.TestFromJSONValid;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_DIRECTORY_LISTING, List));

	Assert.AreEqual(Integer(2), Integer(Length(List)));
end;

procedure TCloudDirItemListTest.TestFromJSONWithFolder;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_FOLDER_ITEM, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	Assert.AreEqual('Documents', List[0].name);
	Assert.AreEqual('folder', List[0].type_);
	Assert.AreEqual('/Documents', List[0].home);
	Assert.AreEqual(Int64(1024), List[0].size);
	Assert.AreEqual(Integer(2), List[0].folders_count);
	Assert.AreEqual(Integer(5), List[0].files_count);
	Assert.AreEqual('abc123', List[0].tree);
end;

procedure TCloudDirItemListTest.TestFromJSONWithFile;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_FILE_ITEM, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	Assert.AreEqual('test.txt', List[0].name);
	Assert.AreEqual('file', List[0].type_);
	Assert.AreEqual('/test.txt', List[0].home);
	Assert.AreEqual(Int64(500), List[0].size);
	Assert.AreEqual('ABCDEF123456', List[0].hash);
	Assert.AreEqual('pass', List[0].virus_scan);
	Assert.AreEqual(Int64(1700000000), List[0].mtime);
end;

procedure TCloudDirItemListTest.TestFromJSONWithTrashItem;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_TRASH_ITEM, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	Assert.AreEqual('deleted_file.txt', List[0].name);
	Assert.AreEqual(Integer(1700000000), List[0].deleted_at);
	Assert.AreEqual('/original/path', List[0].deleted_from);
	{ deleted_by is integer in the record - test uses integer comparison }
end;

procedure TCloudDirItemListTest.TestFromJSONEmpty;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_EMPTY_LIST, List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudDirItemListTest.TestFromJSONInvalid;
var
	List: TCloudDirItemList;
begin
	Assert.IsFalse(TCloudDirItemListJsonAdapter.Parse('invalid json', List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudDirItemListTest.TestFindByNameFound;
var
	List: TCloudDirItemList;
	Item: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List);

	Item := List.FindByName('second.txt');

	Assert.IsFalse(Item.isNone);
	Assert.AreEqual('second.txt', Item.name);
	Assert.AreEqual(Int64(200), Item.size);
end;

procedure TCloudDirItemListTest.TestFindByNameNotFound;
var
	List: TCloudDirItemList;
	Item: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List);

	Item := List.FindByName('nonexistent.txt');

	Assert.IsTrue(Item.isNone);
end;

procedure TCloudDirItemListTest.TestFindByNameEmptyList;
var
	List: TCloudDirItemList;
	Item: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_EMPTY_LIST, List);

	Item := List.FindByName('anything');

	Assert.IsTrue(Item.isNone);
end;

procedure TCloudDirItemListTest.TestFindByHomePathFound;
var
	List: TCloudDirItemList;
	Item: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List);

	{ FindByHomePath adds leading slash, so we pass path without it }
	Item := List.FindByHomePath('folder/second.txt');

	Assert.IsFalse(Item.isNone);
	Assert.AreEqual('second.txt', Item.name);
end;

procedure TCloudDirItemListTest.TestFindByHomePathWithBackslash;
var
	List: TCloudDirItemList;
	Item: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List);

	{ FindByHomePath should convert backslashes to forward slashes }
	Item := List.FindByHomePath('folder\third.txt');

	Assert.IsFalse(Item.isNone);
	Assert.AreEqual('third.txt', Item.name);
end;

procedure TCloudDirItemListTest.TestFindByHomePathNotFound;
var
	List: TCloudDirItemList;
	Item: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List);

	Item := List.FindByHomePath('nonexistent/path.txt');

	Assert.IsTrue(Item.isNone);
end;

procedure TCloudDirItemListTest.TestFindByHomePathEmptyList;
var
	List: TCloudDirItemList;
	Item: TCloudDirItem;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_EMPTY_LIST, List);

	Item := List.FindByHomePath('any/path');

	Assert.IsTrue(Item.isNone);
end;

procedure TCloudDirItemListTest.TestFromJSONMultipleItems;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List));

	Assert.AreEqual(Integer(3), Integer(Length(List)));
	Assert.AreEqual('first.txt', List[0].name);
	Assert.AreEqual('second.txt', List[1].name);
	Assert.AreEqual('third.txt', List[2].name);
end;

{Append tests}

procedure TCloudDirItemListTest.TestAppend_EmptyToEmpty_StaysEmpty;
var
	List, Source: TCloudDirItemList;
begin
	SetLength(List, 0);
	SetLength(Source, 0);

	List.Append(Source);

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudDirItemListTest.TestAppend_ItemsToEmpty_CopiesAll;
var
	List, Source: TCloudDirItemList;
begin
	SetLength(List, 0);
	TCloudDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, Source);

	List.Append(Source);

	Assert.AreEqual(Integer(3), Integer(Length(List)));
	Assert.AreEqual('first.txt', List[0].name);
	Assert.AreEqual('second.txt', List[1].name);
	Assert.AreEqual('third.txt', List[2].name);
end;

procedure TCloudDirItemListTest.TestAppend_ItemsToExisting_Accumulates;
var
	List, Source: TCloudDirItemList;
begin
	{Start with 1 file item}
	TCloudDirItemListJsonAdapter.Parse(JSON_FILE_ITEM, List);
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	{Append 3 more items}
	TCloudDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, Source);
	List.Append(Source);

	Assert.AreEqual(Integer(4), Integer(Length(List)), 'Should have 1 + 3 = 4 items');
	Assert.AreEqual('test.txt', List[0].name, 'Original item preserved');
	Assert.AreEqual('first.txt', List[1].name, 'First appended item');
	Assert.AreEqual('third.txt', List[3].name, 'Last appended item');
end;

procedure TCloudDirItemListTest.TestAppend_EmptyToExisting_NoChange;
var
	List, Source: TCloudDirItemList;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_FILE_ITEM, List);
	SetLength(Source, 0);

	List.Append(Source);

	Assert.AreEqual(Integer(1), Integer(Length(List)), 'Length unchanged');
	Assert.AreEqual('test.txt', List[0].name, 'Content preserved');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudDirItemListTest);

end.
