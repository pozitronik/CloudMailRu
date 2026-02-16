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

			{Folder with count object but missing inner fields}
			JSON_FOLDER_PARTIAL_COUNT =
				'{"email":"test@mail.ru","body":{' +
				'"type":"folder",' +
				'"home":"/test/folder",' +
				'"name":"folder",' +
				'"count":{}' +
				'},"status":200}';

			{Folder with count containing only folders field}
			JSON_FOLDER_COUNT_ONLY_FOLDERS =
				'{"email":"test@mail.ru","body":{' +
				'"type":"folder",' +
				'"home":"/test/folder",' +
				'"name":"folder",' +
				'"count":{"folders":7}' +
				'},"status":200}';

			{Folder without count object}
			JSON_FOLDER_NO_COUNT =
				'{"email":"test@mail.ru","body":{' +
				'"type":"folder",' +
				'"home":"/test/folder",' +
				'"name":"folder"' +
				'},"status":200}';

			{File with minimal fields}
			JSON_FILE_MINIMAL =
				'{"email":"test@mail.ru","body":{' +
				'"type":"file",' +
				'"home":"/test/file.txt",' +
				'"name":"file.txt"' +
				'},"status":200}';

			{JSON with null values for optional fields}
			JSON_FILE_NULL_FIELDS =
				'{"email":"test@mail.ru","body":{' +
				'"type":"file",' +
				'"home":"/test/file.txt",' +
				'"name":"file.txt",' +
				'"weblink":null,' +
				'"hash":null' +
				'},"status":200}';
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
		{Nested object edge cases}
		[Test]
		procedure TestParse_FolderPartialCount_DefaultsToZero;
		[Test]
		procedure TestParse_FolderCountOnlyFolders_FilesDefaultsToZero;
		[Test]
		procedure TestParse_FolderNoCount_CountsRemainZero;
		[Test]
		procedure TestParse_FileMinimal_DefaultsApplied;
		[Test]
		procedure TestParse_FileNullFields_HandledGracefully;

		{Roundtrip serialization tests}
		[Test]
		{Serialize a file item, parse back, verify all fields survive roundtrip}
		procedure TestToJSON_File_Roundtrip;
		[Test]
		{Serialize a folder item with count, parse back, verify all fields survive roundtrip}
		procedure TestToJSON_Dir_Roundtrip;
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

{Nested object edge cases}

procedure TCloudDirItemJsonAdapterTest.TestParse_FolderPartialCount_DefaultsToZero;
var
	Item: TCloudDirItem;
begin
	TCloudDirItemJsonAdapter.Parse(JSON_FOLDER_PARTIAL_COUNT, Item);

	{Empty count object - inner fields should default to 0}
	Assert.AreEqual(0, Item.folders_count);
	Assert.AreEqual(0, Item.files_count);
end;

procedure TCloudDirItemJsonAdapterTest.TestParse_FolderCountOnlyFolders_FilesDefaultsToZero;
var
	Item: TCloudDirItem;
begin
	TCloudDirItemJsonAdapter.Parse(JSON_FOLDER_COUNT_ONLY_FOLDERS, Item);

	{Only folders in count - files should default to 0}
	Assert.AreEqual(7, Item.folders_count);
	Assert.AreEqual(0, Item.files_count);
end;

procedure TCloudDirItemJsonAdapterTest.TestParse_FolderNoCount_CountsRemainZero;
var
	Item: TCloudDirItem;
begin
	TCloudDirItemJsonAdapter.Parse(JSON_FOLDER_NO_COUNT, Item);

	{No count object at all - should remain 0}
	Assert.AreEqual(0, Item.folders_count);
	Assert.AreEqual(0, Item.files_count);
end;

procedure TCloudDirItemJsonAdapterTest.TestParse_FileMinimal_DefaultsApplied;
var
	Item: TCloudDirItem;
begin
	Assert.IsTrue(TCloudDirItemJsonAdapter.Parse(JSON_FILE_MINIMAL, Item));

	{Minimal fields present}
	Assert.AreEqual(WideString('file.txt'), Item.name);
	Assert.AreEqual(WideString('/test/file.txt'), Item.home);
	Assert.AreEqual(WideString('file'), Item.type_);

	{Optional fields should have default values}
	Assert.AreEqual(Int64(0), Item.size);
	Assert.AreEqual(WideString(''), Item.hash);
	Assert.AreEqual(WideString(''), Item.weblink);
end;

procedure TCloudDirItemJsonAdapterTest.TestParse_FileNullFields_HandledGracefully;
var
	Item: TCloudDirItem;
begin
	Assert.IsTrue(TCloudDirItemJsonAdapter.Parse(JSON_FILE_NULL_FIELDS, Item));

	{TSafeJSON correctly returns empty string for null JSON values}
	Assert.AreEqual(WideString(''), Item.weblink);
	Assert.AreEqual(WideString(''), Item.hash);
end;

procedure TCloudDirItemJsonAdapterTest.TestToJSON_File_Roundtrip;
var
	Original, Parsed: TCloudDirItem;
	JSON: WideString;
begin
	Original := Default(TCloudDirItem);
	Original.size := 98765;
	Original.kind := 'file';
	Original.weblink := 'link123';
	Original.type_ := 'file';
	Original.home := '/docs/report.pdf';
	Original.name := 'report.pdf';
	Original.grev := 5;
	Original.rev := 3;
	Original.mtime := 1700000000;
	Original.virus_scan := 'pass';
	Original.hash := 'DEADBEEF12345678';

	JSON := TCloudDirItemJsonAdapter.ItemToJSON(Original);

	{Parse the serialized JSON back -- ItemToJSON produces a flat object,
	 so wrap it in the body structure that Parse expects}
	Assert.IsTrue(TCloudDirItemJsonAdapter.Parse(
		'{"status":200,"body":' + JSON + '}', Parsed));

	Assert.AreEqual(Int64(98765), Parsed.size);
	Assert.AreEqual(WideString('file'), Parsed.kind);
	Assert.AreEqual(WideString('link123'), Parsed.weblink);
	Assert.AreEqual(WideString('file'), Parsed.type_);
	Assert.AreEqual(WideString('/docs/report.pdf'), Parsed.home);
	Assert.AreEqual(WideString('report.pdf'), Parsed.name);
	Assert.AreEqual(5, Parsed.grev);
	Assert.AreEqual(3, Parsed.rev);
	Assert.AreEqual(Int64(1700000000), Parsed.mtime);
	Assert.AreEqual(WideString('pass'), Parsed.virus_scan);
	Assert.AreEqual(WideString('DEADBEEF12345678'), Parsed.hash);
end;

procedure TCloudDirItemJsonAdapterTest.TestToJSON_Dir_Roundtrip;
var
	Original, Parsed: TCloudDirItem;
	JSON: WideString;
begin
	Original := Default(TCloudDirItem);
	Original.size := 4096;
	Original.kind := 'folder';
	Original.weblink := '';
	Original.type_ := 'folder';
	Original.home := '/photos/vacation';
	Original.name := 'vacation';
	Original.grev := 42;
	Original.rev := 41;
	Original.tree := '/photos';
	Original.folders_count := 3;
	Original.files_count := 17;

	JSON := TCloudDirItemJsonAdapter.ItemToJSON(Original);

	Assert.IsTrue(TCloudDirItemJsonAdapter.Parse(
		'{"status":200,"body":' + JSON + '}', Parsed));

	Assert.AreEqual(Int64(4096), Parsed.size);
	Assert.AreEqual(WideString('folder'), Parsed.kind);
	Assert.AreEqual(WideString('folder'), Parsed.type_);
	Assert.AreEqual(WideString('/photos/vacation'), Parsed.home);
	Assert.AreEqual(WideString('vacation'), Parsed.name);
	Assert.AreEqual(42, Parsed.grev);
	Assert.AreEqual(41, Parsed.rev);
	Assert.AreEqual(WideString('/photos'), Parsed.tree);
	Assert.AreEqual(3, Parsed.folders_count);
	Assert.AreEqual(17, Parsed.files_count);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudDirItemJsonAdapterTest);

end.
