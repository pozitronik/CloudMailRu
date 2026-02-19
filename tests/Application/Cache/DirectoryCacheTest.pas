unit DirectoryCacheTest;

interface

uses
	DUnitX.TestFramework,
	DirectoryCache,
	CloudDirItemList,
	CloudDirItem,
	SysUtils;

type
	[TestFixture]
	TDiskDirectoryCacheTest = class
	private
		FCache: TDiskDirectoryCache;
		FCacheDir: WideString;

		{Creates a listing with Count items named 'file_0', 'file_1', etc.}
		function MakeListing(Count: Integer): TCloudDirItemList;
		{Recursively deletes a directory and its contents}
		procedure DeleteCacheDir;
		{Returns the cache dir hash filename (without extension) for a given path}
		function HashForPath(const Path: WideString): WideString;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		{Put + TryGet returns cached listing via disk serialization}
		procedure Test_PutThenGet_ReturnsCachedListing;

		[Test]
		{TryGet miss returns False}
		procedure Test_TryGet_Miss_ReturnsFalse;

		[Test]
		{TTL expiration removes entry}
		procedure Test_TTL_Expiration;

		[Test]
		{TTL expiration deletes both .json and .meta files from disk}
		procedure Test_TTL_Expiration_CleansUpFiles;

		[Test]
		{InvalidatePath removes specific entry}
		procedure Test_InvalidatePath_RemovesEntry;

		[Test]
		{InvalidateAll clears everything}
		procedure Test_InvalidateAll_ClearsAll;

		[Test]
		{TryGet returns a copy -- modifying returned array does not affect cache}
		procedure Test_TryGet_ReturnsCopy;

		[Test]
		{Case-insensitive path keys}
		procedure Test_CaseInsensitivePaths;

		[Test]
		{Put overwrites existing entry}
		procedure Test_Put_OverwritesExisting;

		[Test]
		{InvalidatePath on non-existent path does not raise}
		procedure Test_InvalidatePath_NonExistent_NoError;

		[Test]
		{Put creates .json and .meta sidecar files in cache dir}
		procedure Test_Put_CreatesSidecarFiles;

		[Test]
		{TryGet with missing .json but existing .meta cleans up orphaned meta}
		procedure Test_TryGet_MissingDataFile_CleansUpMeta;

		[Test]
		{TryGet with corrupted JSON deletes the entry}
		procedure Test_TryGet_CorruptedJSON_DeletesEntry;

		[Test]
		{TryGet with corrupted meta (empty Path) returns False}
		procedure Test_TryGet_CorruptedMeta_ReturnsFalse;

		[Test]
		{PutThenGet preserves all TCloudDirItem fields through serialization}
		procedure Test_PutThenGet_PreservesAllFields;
	end;

	[TestFixture]
	TNullDirectoryCacheTest = class
	public
		[Test]
		{TryGet always returns False}
		procedure Test_TryGet_AlwaysReturnsFalse;

		[Test]
		{Put does not store anything}
		procedure Test_Put_DoesNotStore;

		[Test]
		{InvalidatePath completes without exception}
		procedure Test_InvalidatePath_NoOp;

		[Test]
		{InvalidateAll completes without exception}
		procedure Test_InvalidateAll_NoOp;
	end;

implementation

uses
	IOUtils, Classes, DateUtils, System.Hash;

{TDiskDirectoryCacheTest}

function TDiskDirectoryCacheTest.MakeListing(Count: Integer): TCloudDirItemList;
var
	I: Integer;
	Item: TCloudDirItem;
begin
	SetLength(Result, Count);
	for I := 0 to Count - 1 do
	begin
		Item := Default(TCloudDirItem);
		Item.name := 'file_' + IntToStr(I);
		Item.size := I * 100;
		Item.type_ := 'file';
		Item.home := '/test/file_' + IntToStr(I);
		Result[I] := Item;
	end;
end;

function TDiskDirectoryCacheTest.HashForPath(const Path: WideString): WideString;
begin
	Result := THashMD5.GetHashString(string(WideLowerCase(Path)));
end;

procedure TDiskDirectoryCacheTest.DeleteCacheDir;
begin
	if DirectoryExists(FCacheDir) then
		TDirectory.Delete(FCacheDir, True);
end;

procedure TDiskDirectoryCacheTest.Setup;
var
	GUID: TGUID;
begin
	CreateGUID(GUID);
	FCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuCacheTest_' + GUIDToString(GUID);
	FCache := TDiskDirectoryCache.Create(FCacheDir, 60, 50);
end;

procedure TDiskDirectoryCacheTest.TearDown;
begin
	FreeAndNil(FCache);
	DeleteCacheDir;
end;

procedure TDiskDirectoryCacheTest.Test_PutThenGet_ReturnsCachedListing;
var
	Original, Retrieved: TCloudDirItemList;
begin
	Original := MakeListing(3);
	FCache.Put('\account\photos', Original);

	Assert.IsTrue(FCache.TryGet('\account\photos', Retrieved));
	Assert.AreEqual(3, Integer(Length(Retrieved)));
	Assert.AreEqual('file_0', string(Retrieved[0].name));
	Assert.AreEqual('file_1', string(Retrieved[1].name));
	Assert.AreEqual('file_2', string(Retrieved[2].name));
end;

procedure TDiskDirectoryCacheTest.Test_TryGet_Miss_ReturnsFalse;
var
	Retrieved: TCloudDirItemList;
begin
	Assert.IsFalse(FCache.TryGet('\nonexistent\path', Retrieved));
	Assert.IsTrue(Retrieved = nil);
end;

procedure TDiskDirectoryCacheTest.Test_TTL_Expiration;
var
	ShortTTLCache: TDiskDirectoryCache;
	Listing, Retrieved: TCloudDirItemList;
	ShortCacheDir: WideString;
	GUID: TGUID;
begin
	CreateGUID(GUID);
	ShortCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuCacheTest_TTL_' + GUIDToString(GUID);
	ShortTTLCache := TDiskDirectoryCache.Create(ShortCacheDir, 1, 50);
	try
		Listing := MakeListing(1);
		ShortTTLCache.Put('\path', Listing);

		Assert.IsTrue(ShortTTLCache.TryGet('\path', Retrieved), 'Should be cached immediately');

		Sleep(1100);

		Assert.IsFalse(ShortTTLCache.TryGet('\path', Retrieved), 'Should be expired after TTL');
	finally
		ShortTTLCache.Free;
		if DirectoryExists(ShortCacheDir) then
			TDirectory.Delete(ShortCacheDir, True);
	end;
end;

procedure TDiskDirectoryCacheTest.Test_TTL_Expiration_CleansUpFiles;
var
	ShortTTLCache: TDiskDirectoryCache;
	Listing, Retrieved: TCloudDirItemList;
	ShortCacheDir: WideString;
	Hash: WideString;
	GUID: TGUID;
begin
	CreateGUID(GUID);
	ShortCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuCacheTest_TTLClean_' + GUIDToString(GUID);
	ShortTTLCache := TDiskDirectoryCache.Create(ShortCacheDir, 1, 50);
	try
		Listing := MakeListing(1);
		ShortTTLCache.Put('\cleanup\path', Listing);
		Hash := HashForPath('\cleanup\path');

		Assert.IsTrue(FileExists(IncludeTrailingPathDelimiter(ShortCacheDir) + Hash + '.json'), 'JSON file should exist after Put');
		Assert.IsTrue(FileExists(IncludeTrailingPathDelimiter(ShortCacheDir) + Hash + '.meta'), 'Meta file should exist after Put');

		Sleep(1100);

		{TryGet on expired entry triggers cleanup}
		ShortTTLCache.TryGet('\cleanup\path', Retrieved);

		Assert.IsFalse(FileExists(IncludeTrailingPathDelimiter(ShortCacheDir) + Hash + '.json'), 'JSON file should be deleted after expiry');
		Assert.IsFalse(FileExists(IncludeTrailingPathDelimiter(ShortCacheDir) + Hash + '.meta'), 'Meta file should be deleted after expiry');
	finally
		ShortTTLCache.Free;
		if DirectoryExists(ShortCacheDir) then
			TDirectory.Delete(ShortCacheDir, True);
	end;
end;

procedure TDiskDirectoryCacheTest.Test_InvalidatePath_RemovesEntry;
var
	Listing, Retrieved: TCloudDirItemList;
begin
	Listing := MakeListing(2);
	FCache.Put('\account\dir1', Listing);
	FCache.Put('\account\dir2', Listing);

	FCache.InvalidatePath('\account\dir1');

	Assert.IsFalse(FCache.TryGet('\account\dir1', Retrieved), 'Invalidated path should miss');
	Assert.IsTrue(FCache.TryGet('\account\dir2', Retrieved), 'Other path should remain');
end;

procedure TDiskDirectoryCacheTest.Test_InvalidateAll_ClearsAll;
var
	Listing, Retrieved: TCloudDirItemList;
begin
	Listing := MakeListing(1);
	FCache.Put('\account\dir1', Listing);
	FCache.Put('\account\dir2', Listing);
	FCache.Put('\account\dir3', Listing);

	FCache.InvalidateAll;

	Assert.IsFalse(FCache.TryGet('\account\dir1', Retrieved));
	Assert.IsFalse(FCache.TryGet('\account\dir2', Retrieved));
	Assert.IsFalse(FCache.TryGet('\account\dir3', Retrieved));
end;

procedure TDiskDirectoryCacheTest.Test_TryGet_ReturnsCopy;
var
	Original, Retrieved1, Retrieved2: TCloudDirItemList;
begin
	Original := MakeListing(3);
	FCache.Put('\path', Original);

	{Get and modify the first copy}
	Assert.IsTrue(FCache.TryGet('\path', Retrieved1));
	Delete(Retrieved1, 0, 1);
	Assert.AreEqual(2, Integer(Length(Retrieved1)), 'Modified copy should have 2 items');

	{Get again -- should still have all 3 items (re-deserialized from disk)}
	Assert.IsTrue(FCache.TryGet('\path', Retrieved2));
	Assert.AreEqual(3, Integer(Length(Retrieved2)), 'Cache should be unaffected by caller modifications');
end;

procedure TDiskDirectoryCacheTest.Test_CaseInsensitivePaths;
var
	Listing, Retrieved: TCloudDirItemList;
begin
	Listing := MakeListing(2);
	FCache.Put('\Account\Photos', Listing);

	Assert.IsTrue(FCache.TryGet('\account\photos', Retrieved), 'Lowercase should match');
	Assert.IsTrue(FCache.TryGet('\ACCOUNT\PHOTOS', Retrieved), 'Uppercase should match');
	Assert.AreEqual(2, Integer(Length(Retrieved)));
end;

procedure TDiskDirectoryCacheTest.Test_Put_OverwritesExisting;
var
	Listing1, Listing2, Retrieved: TCloudDirItemList;
begin
	Listing1 := MakeListing(2);
	Listing2 := MakeListing(5);

	FCache.Put('\path', Listing1);
	FCache.Put('\path', Listing2);

	Assert.IsTrue(FCache.TryGet('\path', Retrieved));
	Assert.AreEqual(5, Integer(Length(Retrieved)), 'Should return the latest listing');
end;

procedure TDiskDirectoryCacheTest.Test_InvalidatePath_NonExistent_NoError;
begin
	FCache.InvalidatePath('\does\not\exist');
	Assert.Pass('InvalidatePath on non-existent path should not raise');
end;

procedure TDiskDirectoryCacheTest.Test_Put_CreatesSidecarFiles;
var
	Listing: TCloudDirItemList;
	SR: TSearchRec;
	JsonCount, MetaCount: Integer;
begin
	Listing := MakeListing(2);
	FCache.Put('\account\test', Listing);

	{Count .json and .meta files in cache dir}
	JsonCount := 0;
	MetaCount := 0;
	if FindFirst(FCacheDir + '\*.json', faAnyFile, SR) = 0 then
	begin
		repeat
			Inc(JsonCount);
		until FindNext(SR) <> 0;
		FindClose(SR);
	end;
	if FindFirst(FCacheDir + '\*.meta', faAnyFile, SR) = 0 then
	begin
		repeat
			Inc(MetaCount);
		until FindNext(SR) <> 0;
		FindClose(SR);
	end;

	Assert.AreEqual(1, JsonCount, 'Should have one .json file');
	Assert.AreEqual(1, MetaCount, 'Should have one .meta file');
end;

procedure TDiskDirectoryCacheTest.Test_TryGet_MissingDataFile_CleansUpMeta;
var
	Listing, Retrieved: TCloudDirItemList;
	Hash, CacheDir: WideString;
begin
	Listing := MakeListing(2);
	FCache.Put('\orphan\path', Listing);
	Hash := HashForPath('\orphan\path');
	CacheDir := IncludeTrailingPathDelimiter(FCacheDir);

	{Delete the .json data file, leaving .meta orphaned}
	Assert.IsTrue(DeleteFile(CacheDir + Hash + '.json'), 'Should delete JSON file');
	Assert.IsTrue(FileExists(CacheDir + Hash + '.meta'), 'Meta should still exist');

	{TryGet should return False and clean up the orphaned .meta}
	Assert.IsFalse(FCache.TryGet('\orphan\path', Retrieved), 'Should miss when data file is gone');
	Assert.IsFalse(FileExists(CacheDir + Hash + '.meta'), 'Orphaned meta should be cleaned up');
end;

procedure TDiskDirectoryCacheTest.Test_TryGet_CorruptedJSON_DeletesEntry;
var
	Listing, Retrieved: TCloudDirItemList;
	Hash, CacheDir, JsonPath: WideString;
	Writer: TStreamWriter;
begin
	Listing := MakeListing(2);
	FCache.Put('\corrupt\path', Listing);
	Hash := HashForPath('\corrupt\path');
	CacheDir := IncludeTrailingPathDelimiter(FCacheDir);
	JsonPath := CacheDir + Hash + '.json';

	{Overwrite .json with garbage}
	Writer := TStreamWriter.Create(JsonPath, False, TEncoding.UTF8);
	try
		Writer.Write('NOT VALID JSON AT ALL');
	finally
		Writer.Free;
	end;

	{TryGet should return False and clean up the corrupt entry}
	Assert.IsFalse(FCache.TryGet('\corrupt\path', Retrieved), 'Should return False for corrupted JSON');
	Assert.IsFalse(FileExists(JsonPath), 'Corrupted JSON file should be deleted');
	Assert.IsFalse(FileExists(CacheDir + Hash + '.meta'), 'Meta for corrupted entry should be deleted');
end;

procedure TDiskDirectoryCacheTest.Test_TryGet_CorruptedMeta_ReturnsFalse;
var
	Listing, Retrieved: TCloudDirItemList;
	Hash, CacheDir, MetaPath: WideString;
	Writer: TStreamWriter;
begin
	Listing := MakeListing(1);
	FCache.Put('\badmeta\path', Listing);
	Hash := HashForPath('\badmeta\path');
	CacheDir := IncludeTrailingPathDelimiter(FCacheDir);
	MetaPath := CacheDir + Hash + '.meta';

	{Overwrite .meta with content that has no Path= line (ReadMeta returns False)}
	Writer := TStreamWriter.Create(MetaPath, False, TEncoding.UTF8);
	try
		Writer.Write('ExpiresAt=99999999.0' + sLineBreak + 'DataSize=100');
	finally
		Writer.Free;
	end;

	{ReadMeta should return False because Path is empty}
	Assert.IsFalse(FCache.TryGet('\badmeta\path', Retrieved), 'Should return False for meta without Path');
end;

procedure TDiskDirectoryCacheTest.Test_PutThenGet_PreservesAllFields;
var
	Items: TCloudDirItemList;
	Retrieved: TCloudDirItemList;
begin
	SetLength(Items, 2);

	{File item: hash, mtime, weblink are serialized}
	Items[0] := Default(TCloudDirItem);
	Items[0].name := 'document.pdf';
	Items[0].type_ := 'file';
	Items[0].home := '/docs/document.pdf';
	Items[0].size := 123456;
	Items[0].hash := 'ABCDEF0123456789';
	Items[0].mtime := 1700000000;
	Items[0].weblink := 'public/abc123';

	{Folder item: tree, folders_count, files_count are serialized}
	Items[1] := Default(TCloudDirItem);
	Items[1].name := 'subdir';
	Items[1].type_ := 'folder';
	Items[1].home := '/docs/subdir';
	Items[1].size := 0;
	Items[1].tree := '/docs';
	Items[1].folders_count := 3;
	Items[1].files_count := 15;

	FCache.Put('\docs', Items);
	Assert.IsTrue(FCache.TryGet('\docs', Retrieved));
	Assert.AreEqual(2, Integer(Length(Retrieved)));

	{Verify file fields}
	Assert.AreEqual(WideString('document.pdf'), Retrieved[0].name);
	Assert.AreEqual(WideString('file'), Retrieved[0].type_);
	Assert.AreEqual(WideString('/docs/document.pdf'), Retrieved[0].home);
	Assert.AreEqual(Int64(123456), Retrieved[0].size);
	Assert.AreEqual(WideString('ABCDEF0123456789'), Retrieved[0].hash);
	Assert.AreEqual(Int64(1700000000), Retrieved[0].mtime);
	Assert.AreEqual(WideString('public/abc123'), Retrieved[0].weblink);

	{Verify folder fields}
	Assert.AreEqual(WideString('subdir'), Retrieved[1].name);
	Assert.AreEqual(WideString('folder'), Retrieved[1].type_);
	Assert.AreEqual(WideString('/docs/subdir'), Retrieved[1].home);
	Assert.AreEqual(WideString('/docs'), Retrieved[1].tree);
	Assert.AreEqual(3, Retrieved[1].folders_count);
	Assert.AreEqual(15, Retrieved[1].files_count);
end;

{TNullDirectoryCacheTest}

procedure TNullDirectoryCacheTest.Test_TryGet_AlwaysReturnsFalse;
var
	Cache: IDirectoryCache;
	Listing: TCloudDirItemList;
begin
	Cache := TNullDirectoryCache.Create;
	Assert.IsFalse(Cache.TryGet('\any\path', Listing));
end;

procedure TNullDirectoryCacheTest.Test_Put_DoesNotStore;
var
	Cache: IDirectoryCache;
	Listing, Retrieved: TCloudDirItemList;
	Item: TCloudDirItem;
begin
	Cache := TNullDirectoryCache.Create;
	SetLength(Listing, 1);
	Item := Default(TCloudDirItem);
	Item.name := 'test';
	Listing[0] := Item;

	Cache.Put('\path', Listing);
	Assert.IsFalse(Cache.TryGet('\path', Retrieved), 'Null cache should never return cached data');
end;

procedure TNullDirectoryCacheTest.Test_InvalidatePath_NoOp;
var
	Cache: IDirectoryCache;
begin
	Cache := TNullDirectoryCache.Create;
	Cache.InvalidatePath('\path');
	Assert.Pass;
end;

procedure TNullDirectoryCacheTest.Test_InvalidateAll_NoOp;
var
	Cache: IDirectoryCache;
begin
	Cache := TNullDirectoryCache.Create;
	Cache.InvalidateAll;
	Assert.Pass;
end;

initialization

TDUnitX.RegisterTestFixture(TDiskDirectoryCacheTest);
TDUnitX.RegisterTestFixture(TNullDirectoryCacheTest);

end.
