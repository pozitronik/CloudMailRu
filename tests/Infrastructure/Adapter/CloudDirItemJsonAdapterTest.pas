unit CloudDirItemJsonAdapterTest;

interface

uses
	CloudDirItem,
	CloudDirItemJsonAdapter,
	CloudConstants,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCloudDirItemJsonAdapterTest = class
	private
		const
			{JSON for a file item}
			JSON_FILE_ITEM =
				'{"email":"test@mail.ru","body":{' +
				'"size":12345,' +
				'"kind":"file",' +
				'"weblink":"abc123",' +
				'"type":"file",' +
				'"home":"/test/file.txt",' +
				'"name":"file.txt",' +
				'"mtime":1609459200,' +
				'"virus_scan":"pass",' +
				'"hash":"ABCDEF1234567890"' +
				'},"status":200}';

			{JSON for a folder item}
			JSON_FOLDER_ITEM =
				'{"email":"test@mail.ru","body":{' +
				'"size":0,' +
				'"kind":"folder",' +
				'"weblink":"",' +
				'"type":"folder",' +
				'"home":"/test/folder",' +
				'"name":"folder",' +
				'"tree":"/",' +
				'"grev":1,' +
				'"rev":2,' +
				'"count":{"folders":3,"files":5}' +
				'},"status":200}';

			{Invalid JSON}
			JSON_INVALID = 'not valid json';

			{Empty JSON}
			JSON_EMPTY = '';
	public
		[Test]
		procedure TestParse_FileItem_ReturnsTrue;
		[Test]
		procedure TestParse_FileItem_ParsesAllFields;
		[Test]
		procedure TestParse_FolderItem_ReturnsTrue;
		[Test]
		procedure TestParse_FolderItem_ParsesAllFields;
		[Test]
		procedure TestParse_FolderItem_ParsesCount;
		[Test]
		procedure TestParse_InvalidJSON_ReturnsFalse;
		[Test]
		procedure TestParse_EmptyJSON_ReturnsFalse;
		[Test]
		procedure TestParse_FileItem_IsFileTrue;
		[Test]
		procedure TestParse_FolderItem_IsDirTrue;
	end;

implementation

procedure TCloudDirItemJsonAdapterTest.TestParse_FileItem_ReturnsTrue;
var
	Item: TCloudDirItem;
begin
	Assert.IsTrue(TCloudDirItemJsonAdapter.Parse(JSON_FILE_ITEM, Item));
end;

procedure TCloudDirItemJsonAdapterTest.TestParse_FileItem_ParsesAllFields;
var
	Item: TCloudDirItem;
begin
	TCloudDirItemJsonAdapter.Parse(JSON_FILE_ITEM, Item);

	Assert.AreEqual(Int64(12345), Item.size);
	Assert.AreEqual(WideString('file'), Item.kind);
	Assert.AreEqual(WideString('abc123'), Item.weblink);
	Assert.AreEqual(WideString('file'), Item.type_);
	Assert.AreEqual(WideString('/test/file.txt'), Item.home);
	Assert.AreEqual(WideString('file.txt'), Item.name);
	Assert.AreEqual(Int64(1609459200), Item.mtime);
	Assert.AreEqual(WideString('pass'), Item.virus_scan);
	Assert.AreEqual(WideString('ABCDEF1234567890'), Item.hash);
end;

procedure TCloudDirItemJsonAdapterTest.TestParse_FolderItem_ReturnsTrue;
var
	Item: TCloudDirItem;
begin
	Assert.IsTrue(TCloudDirItemJsonAdapter.Parse(JSON_FOLDER_ITEM, Item));
end;

procedure TCloudDirItemJsonAdapterTest.TestParse_FolderItem_ParsesAllFields;
var
	Item: TCloudDirItem;
begin
	TCloudDirItemJsonAdapter.Parse(JSON_FOLDER_ITEM, Item);

	Assert.AreEqual(Int64(0), Item.size);
	Assert.AreEqual(WideString('folder'), Item.kind);
	Assert.AreEqual(WideString('folder'), Item.type_);
	Assert.AreEqual(WideString('/test/folder'), Item.home);
	Assert.AreEqual(WideString('folder'), Item.name);
	Assert.AreEqual(WideString('/'), Item.tree);
	Assert.AreEqual(1, Item.grev);
	Assert.AreEqual(2, Item.rev);
end;

procedure TCloudDirItemJsonAdapterTest.TestParse_FolderItem_ParsesCount;
var
	Item: TCloudDirItem;
begin
	TCloudDirItemJsonAdapter.Parse(JSON_FOLDER_ITEM, Item);

	Assert.AreEqual(3, Item.folders_count);
	Assert.AreEqual(5, Item.files_count);
end;

procedure TCloudDirItemJsonAdapterTest.TestParse_InvalidJSON_ReturnsFalse;
var
	Item: TCloudDirItem;
begin
	Assert.IsFalse(TCloudDirItemJsonAdapter.Parse(JSON_INVALID, Item));
end;

procedure TCloudDirItemJsonAdapterTest.TestParse_EmptyJSON_ReturnsFalse;
var
	Item: TCloudDirItem;
begin
	Assert.IsFalse(TCloudDirItemJsonAdapter.Parse(JSON_EMPTY, Item));
end;

procedure TCloudDirItemJsonAdapterTest.TestParse_FileItem_IsFileTrue;
var
	Item: TCloudDirItem;
begin
	TCloudDirItemJsonAdapter.Parse(JSON_FILE_ITEM, Item);

	Assert.IsTrue(Item.isFile);
	Assert.IsFalse(Item.isDir);
end;

procedure TCloudDirItemJsonAdapterTest.TestParse_FolderItem_IsDirTrue;
var
	Item: TCloudDirItem;
begin
	TCloudDirItemJsonAdapter.Parse(JSON_FOLDER_ITEM, Item);

	Assert.IsTrue(Item.isDir);
	Assert.IsFalse(Item.isFile);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudDirItemJsonAdapterTest);

end.
