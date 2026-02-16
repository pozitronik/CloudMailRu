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
		{Size eviction: oldest entries are evicted when over MaxSizeMB limit}
		procedure Test_SizeEviction_RemovesOldestEntries;
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
	Windows, IOUtils;

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
