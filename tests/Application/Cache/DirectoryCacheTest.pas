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
		{InvalidateAll on empty cache completes without error}
		procedure Test_InvalidateAll_EmptyCache_NoError;

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
		{Size eviction: oldest entries are evicted when over MaxSizeMB limit}
		procedure Test_SizeEviction_RemovesOldestEntries;

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

		[Test]
		{Eviction stops mid-loop once total drops below limit (Break path)}
		procedure Test_SizeEviction_BreaksMidLoop;

		[Test]
		{Eviction skips entries whose .meta is corrupt (ReadMeta returns False)}
		procedure Test_SizeEviction_SkipsCorruptMeta;

		[Test]
		{Eviction comparator handles entries with identical ExpiresAt}
		procedure Test_SizeEviction_SameExpiresAt;
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

procedure TDiskDirectoryCacheTest.Test_InvalidateAll_EmptyCache_NoError;
begin
	{Should not raise on a cache with no entries}
	FCache.InvalidateAll;
	Assert.Pass('InvalidateAll on empty cache should not raise');
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

procedure TDiskDirectoryCacheTest.Test_SizeEviction_RemovesOldestEntries;
var
	SmallCache: TDiskDirectoryCache;
	SmallCacheDir: WideString;
	GUID: TGUID;
	Listing, Retrieved: TCloudDirItemList;
	I: Integer;
begin
	{Create a cache with very small max size (1 KB) to trigger eviction quickly}
	CreateGUID(GUID);
	SmallCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuCacheTest_Evict_' + GUIDToString(GUID);
	{MaxSizeMB=0 would not work, use a trick: create with 1 MB max then fill beyond}
	SmallCache := TDiskDirectoryCache.Create(SmallCacheDir, 3600, 1);
	try
		{Put several large listings to exceed 1 MB total}
		for I := 1 to 10 do
		begin
			Listing := MakeListing(500); {Each listing serializes to several KB}
			SmallCache.Put('\bigdir' + IntToStr(I), Listing);
			Sleep(20); {Ensure distinct ExpiresAt for eviction ordering}
		end;

		{After eviction, earliest entries should be gone while latest should remain}
		Assert.IsTrue(SmallCache.TryGet('\bigdir10', Retrieved), 'Most recent entry should survive eviction');
	finally
		SmallCache.Free;
		if DirectoryExists(SmallCacheDir) then
			TDirectory.Delete(SmallCacheDir, True);
	end;
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

procedure TDiskDirectoryCacheTest.Test_SizeEviction_BreaksMidLoop;
var
	TinyCache: TDiskDirectoryCache;
	TinyCacheDir, CDir: WideString;
	GUID: TGUID;
	Listing, Retrieved: TCloudDirItemList;
	I: Integer;
	SurvivedCount: Integer;
begin
	// Create cache with 1 MB limit, add entries that collectively exceed it,
	// but not all entries need to be evicted to get back under the limit.
	// This exercises the Break path at line 232-233 of EvictIfOverSize.
	CreateGUID(GUID);
	TinyCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuCacheTest_MidLoop_' + GUIDToString(GUID);
	TinyCache := TDiskDirectoryCache.Create(TinyCacheDir, 3600, 1);
	try
		// Put 20 listings with 500 items each (~20-30KB each); total > 1 MB
		for I := 1 to 20 do
		begin
			Listing := MakeListing(500);
			TinyCache.Put('\dir' + IntToStr(I), Listing);
			Sleep(15); // ensure distinct ExpiresAt ordering
		end;

		// After eviction, some entries should survive (Break mid-loop),
		// while earliest should be gone
		SurvivedCount := 0;
		for I := 1 to 20 do
		begin
			if TinyCache.TryGet('\dir' + IntToStr(I), Retrieved) then
				Inc(SurvivedCount);
		end;

		Assert.IsTrue(SurvivedCount > 0, 'Some entries should survive eviction');
		Assert.IsTrue(SurvivedCount < 20, 'Not all entries should survive -- some must be evicted');
		// Most recent entry should definitely survive
		Assert.IsTrue(TinyCache.TryGet('\dir20', Retrieved), 'Most recent entry should survive');
	finally
		TinyCache.Free;
		if DirectoryExists(TinyCacheDir) then
			TDirectory.Delete(TinyCacheDir, True);
	end;
end;

procedure TDiskDirectoryCacheTest.Test_SizeEviction_SkipsCorruptMeta;
var
	TinyCache: TDiskDirectoryCache;
	TinyCacheDir, CDir, Hash: WideString;
	GUID: TGUID;
	Listing, Retrieved: TCloudDirItemList;
	Writer: TStreamWriter;
	I: Integer;
begin
	// Create cache, add entries that exceed the limit.
	// Corrupt one .meta file so ReadMeta returns False during eviction scan.
	// Eviction should skip the corrupt entry and still evict valid old entries.
	CreateGUID(GUID);
	TinyCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuCacheTest_CorruptEvict_' + GUIDToString(GUID);
	TinyCache := TDiskDirectoryCache.Create(TinyCacheDir, 3600, 1);
	try
		CDir := IncludeTrailingPathDelimiter(TinyCacheDir);

		// Put several entries to fill cache
		for I := 1 to 15 do
		begin
			Listing := MakeListing(500);
			TinyCache.Put('\evdir' + IntToStr(I), Listing);
			Sleep(15);
		end;

		// Corrupt the meta for one of the middle entries (no Path= line)
		Hash := HashForPath('\evdir8');
		Writer := TStreamWriter.Create(CDir + Hash + '.meta', False, TEncoding.UTF8);
		try
			Writer.Write('SomeGarbage=value' + sLineBreak + 'MoreGarbage=123');
		finally
			Writer.Free;
		end;

		// Add one more entry to trigger eviction again
		Listing := MakeListing(500);
		TinyCache.Put('\evdir_final', Listing);

		// The final entry should survive eviction
		Assert.IsTrue(TinyCache.TryGet('\evdir_final', Retrieved),
			'Final entry should survive eviction even with corrupt meta in the mix');
	finally
		TinyCache.Free;
		if DirectoryExists(TinyCacheDir) then
			TDirectory.Delete(TinyCacheDir, True);
	end;
end;

procedure TDiskDirectoryCacheTest.Test_SizeEviction_SameExpiresAt;
var
	TinyCache: TDiskDirectoryCache;
	TinyCacheDir, CDir, Hash: WideString;
	GUID: TGUID;
	Listing, Retrieved: TCloudDirItemList;
	Writer: TStreamWriter;
	MetaLines: TStringList;
	SharedExpiry: string;
	I: Integer;
	SurvivedCount: Integer;
begin
	// Create entries with identical ExpiresAt to exercise the comparator Result := 0 branch.
	// We manually write meta files with the same ExpiresAt value.
	CreateGUID(GUID);
	TinyCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuCacheTest_SameExpiry_' + GUIDToString(GUID);
	TinyCache := TDiskDirectoryCache.Create(TinyCacheDir, 3600, 1);
	try
		CDir := IncludeTrailingPathDelimiter(TinyCacheDir);
		SharedExpiry := FloatToStr(Now + 3600 / SecsPerDay);

		// Put entries normally first (to create .json files)
		for I := 1 to 15 do
		begin
			Listing := MakeListing(500);
			TinyCache.Put('\samedir' + IntToStr(I), Listing);
		end;

		// Overwrite all .meta files with identical ExpiresAt
		for I := 1 to 15 do
		begin
			Hash := HashForPath('\samedir' + IntToStr(I));
			MetaLines := TStringList.Create;
			try
				MetaLines.Add('Path=' + WideLowerCase('\samedir' + IntToStr(I)));
				MetaLines.Add('ExpiresAt=' + SharedExpiry);
				MetaLines.Add('DataSize=30000');
				MetaLines.SaveToFile(CDir + Hash + '.meta', TEncoding.UTF8);
			finally
				MetaLines.Free;
			end;
		end;

		// Add one more entry to trigger eviction (this one gets a new ExpiresAt)
		Listing := MakeListing(500);
		TinyCache.Put('\samedir_trigger', Listing);

		// Eviction should work without errors even when all ExpiresAt are equal
		// At least the trigger entry should survive
		SurvivedCount := 0;
		for I := 1 to 15 do
		begin
			if TinyCache.TryGet('\samedir' + IntToStr(I), Retrieved) then
				Inc(SurvivedCount);
		end;
		Assert.IsTrue(SurvivedCount < 15,
			'Some entries with same ExpiresAt should be evicted');
		Assert.IsTrue(TinyCache.TryGet('\samedir_trigger', Retrieved),
			'Trigger entry should survive');
	finally
		TinyCache.Free;
		if DirectoryExists(TinyCacheDir) then
			TDirectory.Delete(TinyCacheDir, True);
	end;
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
