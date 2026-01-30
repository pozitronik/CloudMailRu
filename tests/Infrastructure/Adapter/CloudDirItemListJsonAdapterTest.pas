unit CloudDirItemListJsonAdapterTest;

interface

uses
	CloudDirItem,
	CloudDirItemList,
	CloudDirItemListJsonAdapter,
	CloudConstants,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCloudDirItemListJsonAdapterTest = class
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

			{List is not an array}
			JSON_LIST_NOT_ARRAY = '{"status":200,"body":{"list":"not an array"}}';

			{Missing list property}
			JSON_NO_LIST = '{"status":200,"body":{}}';

			{Body is not an object}
			JSON_BODY_NOT_OBJECT = '{"status":200,"body":"string body"}';

			{Folder without count - verify count defaults}
			JSON_FOLDER_NO_COUNT = '{"status":200,"body":{"list":[' +
				'{"name":"NoCountFolder","type":"folder","home":"/NoCountFolder"}' +
				']}}';

			{Item with all optional fields missing}
			JSON_MINIMAL_ITEM = '{"status":200,"body":{"list":[' +
				'{"name":"minimal.txt","type":"file","home":"/minimal.txt"}' +
				']}}';

			{Unicode in file names}
			JSON_UNICODE_NAMES = '{"status":200,"body":{"list":[' +
				'{"name":"\u0424\u0430\u0439\u043B.txt","type":"file","home":"/\u0424\u0430\u0439\u043B.txt"}' +
				']}}';

			{Large size values}
			JSON_LARGE_SIZE = '{"status":200,"body":{"list":[' +
				'{"name":"huge.bin","type":"file","home":"/huge.bin","size":9223372036854775807}' +
				']}}';

			{Directory listing with body.count for ExpectedCount tests}
			JSON_WITH_COUNT = '{"status":200,"body":{"count":{"folders":1,"files":2},"list":[' +
				'{"name":"dir1","type":"folder","home":"/dir1"},' +
				'{"name":"a.txt","type":"file","home":"/a.txt"},' +
				'{"name":"b.txt","type":"file","home":"/b.txt"}' +
				']}}';

			{Directory listing without body.count}
			JSON_WITHOUT_COUNT = '{"status":200,"body":{"list":[' +
				'{"name":"file.txt","type":"file","home":"/file.txt"}' +
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
		{Edge cases}
		[Test]
		procedure TestParse_ListNotArray_ReturnsFalse;
		[Test]
		procedure TestParse_NoListProperty_ReturnsFalse;
		[Test]
		procedure TestParse_BodyNotObject_ReturnsFalse;
		[Test]
		procedure TestParse_FolderNoCount_CountsDefaultToZero;
		[Test]
		procedure TestParse_MinimalItem_DefaultsApplied;
		[Test]
		procedure TestParse_UnicodeNames_ParsedCorrectly;
		[Test]
		procedure TestParse_LargeSize_ParsedCorrectly;
		{ExpectedCount tests}
		[Test]
		procedure TestParse_WithCount_ReturnsExpectedCount;
		[Test]
		procedure TestParse_WithoutCount_ReturnsZeroExpectedCount;
		[Test]
		procedure TestParse_WithCount_InvalidJSON_ReturnsZeroExpectedCount;
	end;

implementation

procedure TCloudDirItemListJsonAdapterTest.TestParse_DirectoryListing_ReturnsTrue;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_DIRECTORY_LISTING, List));
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_DirectoryListing_ParsesCorrectCount;
var
	List: TCloudDirItemList;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_DIRECTORY_LISTING, List);
	Assert.AreEqual(Integer(2), Integer(Length(List)));
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_SingleFolder_ParsesAllFields;
var
	List: TCloudDirItemList;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_SINGLE_FOLDER, List);
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

procedure TCloudDirItemListJsonAdapterTest.TestParse_SingleFile_ParsesAllFields;
var
	List: TCloudDirItemList;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_SINGLE_FILE, List);
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	Assert.AreEqual('test.txt', List[0].name);
	Assert.AreEqual('file', List[0].type_);
	Assert.AreEqual('/test.txt', List[0].home);
	Assert.AreEqual(Int64(500), List[0].size);
	Assert.AreEqual('ABCDEF123456', List[0].hash);
	Assert.AreEqual('pass', List[0].virus_scan);
	Assert.AreEqual(Int64(1700000000), List[0].mtime);
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_EmptyList_ReturnsTrue;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_EMPTY_LIST, List));
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_EmptyList_ReturnsEmptyArray;
var
	List: TCloudDirItemList;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_EMPTY_LIST, List);
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_InvalidJSON_ReturnsFalse;
var
	List: TCloudDirItemList;
begin
	Assert.IsFalse(TCloudDirItemListJsonAdapter.Parse(JSON_INVALID, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_EmptyString_ReturnsFalse;
var
	List: TCloudDirItemList;
begin
	Assert.IsFalse(TCloudDirItemListJsonAdapter.Parse(JSON_EMPTY, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_TrashItem_ParsesDeletedFields;
var
	List: TCloudDirItemList;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_TRASH_ITEM, List);
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	Assert.AreEqual('deleted_file.txt', List[0].name);
	Assert.AreEqual(Integer(1700000000), List[0].deleted_at);
	Assert.AreEqual('/original/path', List[0].deleted_from);
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_MultipleItems_ParsesAllItems;
var
	List: TCloudDirItemList;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_MULTIPLE_ITEMS, List);
	Assert.AreEqual(Integer(3), Integer(Length(List)));

	Assert.AreEqual('first.txt', List[0].name);
	Assert.AreEqual(Int64(100), List[0].size);

	Assert.AreEqual('second.txt', List[1].name);
	Assert.AreEqual(Int64(200), List[1].size);

	Assert.AreEqual('third.txt', List[2].name);
	Assert.AreEqual(Int64(300), List[2].size);
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_FolderItem_HasCorrectType;
var
	List: TCloudDirItemList;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_SINGLE_FOLDER, List);
	Assert.IsTrue(List[0].isDir);
	Assert.IsFalse(List[0].isFile);
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_FileItem_HasCorrectType;
var
	List: TCloudDirItemList;
begin
	TCloudDirItemListJsonAdapter.Parse(JSON_SINGLE_FILE, List);
	Assert.IsTrue(List[0].isFile);
	Assert.IsFalse(List[0].isDir);
end;

{Edge cases}

procedure TCloudDirItemListJsonAdapterTest.TestParse_ListNotArray_ReturnsFalse;
var
	List: TCloudDirItemList;
begin
	Assert.IsFalse(TCloudDirItemListJsonAdapter.Parse(JSON_LIST_NOT_ARRAY, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_NoListProperty_ReturnsFalse;
var
	List: TCloudDirItemList;
begin
	Assert.IsFalse(TCloudDirItemListJsonAdapter.Parse(JSON_NO_LIST, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_BodyNotObject_ReturnsFalse;
var
	List: TCloudDirItemList;
begin
	Assert.IsFalse(TCloudDirItemListJsonAdapter.Parse(JSON_BODY_NOT_OBJECT, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_FolderNoCount_CountsDefaultToZero;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_FOLDER_NO_COUNT, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	{Folder without count object should have zero counts}
	Assert.AreEqual(0, List[0].folders_count);
	Assert.AreEqual(0, List[0].files_count);
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_MinimalItem_DefaultsApplied;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_MINIMAL_ITEM, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	{Required fields present}
	Assert.AreEqual('minimal.txt', List[0].name);
	Assert.AreEqual('/minimal.txt', List[0].home);

	{Optional fields should have defaults}
	Assert.AreEqual(Int64(0), List[0].size);
	Assert.AreEqual('', List[0].hash);
	Assert.AreEqual('', List[0].weblink);
	Assert.AreEqual(Int64(0), List[0].mtime);
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_UnicodeNames_ParsedCorrectly;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_UNICODE_NAMES, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	{Delphi System.JSON decodes \uXXXX escape sequences natively}
	Assert.AreEqual(#$0424#$0430#$0439#$043B + '.txt', List[0].name);
	Assert.AreEqual('/' + #$0424#$0430#$0439#$043B + '.txt', List[0].home);
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_LargeSize_ParsedCorrectly;
var
	List: TCloudDirItemList;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_LARGE_SIZE, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	{Max Int64 value}
	Assert.AreEqual(Int64(9223372036854775807), List[0].size);
end;

{ExpectedCount tests}

procedure TCloudDirItemListJsonAdapterTest.TestParse_WithCount_ReturnsExpectedCount;
var
	List: TCloudDirItemList;
	ExpectedCount: Integer;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_WITH_COUNT, List, ExpectedCount));
	Assert.AreEqual(Integer(3), Integer(Length(List)), 'Should parse all 3 items');
	Assert.AreEqual(Integer(3), ExpectedCount, 'ExpectedCount should be files + folders = 2 + 1');
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_WithoutCount_ReturnsZeroExpectedCount;
var
	List: TCloudDirItemList;
	ExpectedCount: Integer;
begin
	Assert.IsTrue(TCloudDirItemListJsonAdapter.Parse(JSON_WITHOUT_COUNT, List, ExpectedCount));
	Assert.AreEqual(Integer(1), Integer(Length(List)), 'Should parse 1 item');
	Assert.AreEqual(Integer(0), ExpectedCount, 'ExpectedCount should be 0 when body.count is absent');
end;

procedure TCloudDirItemListJsonAdapterTest.TestParse_WithCount_InvalidJSON_ReturnsZeroExpectedCount;
var
	List: TCloudDirItemList;
	ExpectedCount: Integer;
begin
	Assert.IsFalse(TCloudDirItemListJsonAdapter.Parse(JSON_INVALID, List, ExpectedCount));
	Assert.AreEqual(Integer(0), ExpectedCount, 'ExpectedCount should be 0 on parse failure');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudDirItemListJsonAdapterTest);

end.
