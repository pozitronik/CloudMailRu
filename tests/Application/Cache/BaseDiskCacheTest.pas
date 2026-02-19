unit BaseDiskCacheTest;

interface

uses
	DUnitX.TestFramework,
	BaseDiskCache,
	SysUtils;

type
	{Minimal concrete subclass for testing the abstract base}
	TTestDiskCache = class(TBaseDiskCache)
	protected
		function GetDataFileExtension: WideString; override;
	public
		procedure TestWriteBaseMeta(const Hash: WideString; DataSize: Int64);
		function TestReadBaseMeta(const Hash: WideString; out ExpiresAt: TDateTime; out DataSize: Int64): Boolean;
		procedure TestEvictIfOverSize;
		procedure TestDeleteEntry(const Hash: WideString);
		procedure TestDoInvalidateAll;
		function TestGetTotalCacheSize: Int64;
		function TestDataFilePath(const Hash: WideString): WideString;
		function TestMetaFilePath(const Hash: WideString): WideString;
	end;

	[TestFixture]
	TBaseDiskCacheTest = class
	private
		FCache: TTestDiskCache;
		FCacheDir: WideString;

		{Creates a data file with the given content}
		procedure CreateDataFile(const Hash: WideString; const Content: string);
		{Recursively deletes a directory and its contents}
		procedure DeleteCacheDir;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		{Constructor creates the cache directory}
		procedure Test_Constructor_CreatesDirectory;

		[Test]
		{DataFilePath uses the subclass extension}
		procedure Test_DataFilePath_UsesExtension;

		[Test]
		{MetaFilePath always uses .meta extension}
		procedure Test_MetaFilePath_UsesMeta;

		[Test]
		{WriteBaseMeta + ReadBaseMeta round-trip}
		procedure Test_WriteReadBaseMeta_RoundTrip;

		[Test]
		{ReadBaseMeta returns False for missing file}
		procedure Test_ReadBaseMeta_MissingFile_ReturnsFalse;

		[Test]
		{ReadBaseMeta returns False for corrupt meta (ExpiresAt=0)}
		procedure Test_ReadBaseMeta_CorruptMeta_ReturnsFalse;

		[Test]
		{DeleteEntry removes both data and meta files}
		procedure Test_DeleteEntry_RemovesBothFiles;

		[Test]
		{GetTotalCacheSize sums data files}
		procedure Test_GetTotalCacheSize_SumsDataFiles;

		[Test]
		{EvictIfOverSize exits early when under limit}
		procedure Test_EvictIfOverSize_UnderLimit_NoEviction;

		[Test]
		{EvictIfOverSize evicts oldest entries when over limit}
		procedure Test_EvictIfOverSize_EvictsOldestEntries;

		[Test]
		{EvictIfOverSize breaks mid-loop once under limit}
		procedure Test_EvictIfOverSize_BreaksMidLoop;

		[Test]
		{EvictIfOverSize skips entries with corrupt meta}
		procedure Test_EvictIfOverSize_SkipsCorruptMeta;

		[Test]
		{EvictIfOverSize handles entries with identical ExpiresAt}
		procedure Test_EvictIfOverSize_SameExpiresAt;

		[Test]
		{DoInvalidateAll clears all data and meta files}
		procedure Test_DoInvalidateAll_ClearsAllFiles;

		[Test]
		{DoInvalidateAll on empty cache completes without error}
		procedure Test_DoInvalidateAll_EmptyCache_NoError;
	end;

implementation

uses
	IOUtils, Classes, DateUtils;

{TTestDiskCache}

function TTestDiskCache.GetDataFileExtension: WideString;
begin
	Result := '.test';
end;

procedure TTestDiskCache.TestWriteBaseMeta(const Hash: WideString; DataSize: Int64);
begin
	WriteBaseMeta(Hash, DataSize);
end;

function TTestDiskCache.TestReadBaseMeta(const Hash: WideString; out ExpiresAt: TDateTime; out DataSize: Int64): Boolean;
begin
	Result := ReadBaseMeta(Hash, ExpiresAt, DataSize);
end;

procedure TTestDiskCache.TestEvictIfOverSize;
begin
	EvictIfOverSize;
end;

procedure TTestDiskCache.TestDeleteEntry(const Hash: WideString);
begin
	DeleteEntry(Hash);
end;

procedure TTestDiskCache.TestDoInvalidateAll;
begin
	DoInvalidateAll;
end;

function TTestDiskCache.TestGetTotalCacheSize: Int64;
begin
	Result := GetTotalCacheSize;
end;

function TTestDiskCache.TestDataFilePath(const Hash: WideString): WideString;
begin
	Result := DataFilePath(Hash);
end;

function TTestDiskCache.TestMetaFilePath(const Hash: WideString): WideString;
begin
	Result := MetaFilePath(Hash);
end;

{TBaseDiskCacheTest}

procedure TBaseDiskCacheTest.CreateDataFile(const Hash: WideString; const Content: string);
var
	Writer: TStreamWriter;
begin
	Writer := TStreamWriter.Create(FCache.TestDataFilePath(Hash), False, TEncoding.UTF8);
	try
		Writer.Write(Content);
	finally
		Writer.Free;
	end;
end;

procedure TBaseDiskCacheTest.DeleteCacheDir;
begin
	if DirectoryExists(FCacheDir) then
		TDirectory.Delete(FCacheDir, True);
end;

procedure TBaseDiskCacheTest.Setup;
var
	GUID: TGUID;
begin
	CreateGUID(GUID);
	FCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuBaseCacheTest_' + GUIDToString(GUID);
	FCache := TTestDiskCache.Create(FCacheDir, 60, 50);
end;

procedure TBaseDiskCacheTest.TearDown;
begin
	FreeAndNil(FCache);
	DeleteCacheDir;
end;

procedure TBaseDiskCacheTest.Test_Constructor_CreatesDirectory;
begin
	Assert.IsTrue(DirectoryExists(FCacheDir), 'Constructor should create cache directory');
end;

procedure TBaseDiskCacheTest.Test_DataFilePath_UsesExtension;
begin
	Assert.IsTrue(string(FCache.TestDataFilePath('abc')).EndsWith('abc.test'),
		'DataFilePath should use subclass extension');
end;

procedure TBaseDiskCacheTest.Test_MetaFilePath_UsesMeta;
begin
	Assert.IsTrue(string(FCache.TestMetaFilePath('abc')).EndsWith('abc.meta'),
		'MetaFilePath should use .meta extension');
end;

procedure TBaseDiskCacheTest.Test_WriteReadBaseMeta_RoundTrip;
var
	ExpiresAt: TDateTime;
	DataSize: Int64;
begin
	FCache.TestWriteBaseMeta('HASH1', 42000);

	Assert.IsTrue(FCache.TestReadBaseMeta('HASH1', ExpiresAt, DataSize));
	Assert.IsTrue(ExpiresAt > Now, 'ExpiresAt should be in the future');
	Assert.AreEqual(Int64(42000), DataSize);
end;

procedure TBaseDiskCacheTest.Test_ReadBaseMeta_MissingFile_ReturnsFalse;
var
	ExpiresAt: TDateTime;
	DataSize: Int64;
begin
	Assert.IsFalse(FCache.TestReadBaseMeta('NONEXISTENT', ExpiresAt, DataSize));
end;

procedure TBaseDiskCacheTest.Test_ReadBaseMeta_CorruptMeta_ReturnsFalse;
var
	ExpiresAt: TDateTime;
	DataSize: Int64;
	Writer: TStreamWriter;
begin
	{Write a meta file with no ExpiresAt line}
	Writer := TStreamWriter.Create(FCache.TestMetaFilePath('CORRUPT'), False, TEncoding.UTF8);
	try
		Writer.Write('DataSize=100');
	finally
		Writer.Free;
	end;

	Assert.IsFalse(FCache.TestReadBaseMeta('CORRUPT', ExpiresAt, DataSize),
		'Should return False when ExpiresAt is missing/zero');
end;

procedure TBaseDiskCacheTest.Test_DeleteEntry_RemovesBothFiles;
begin
	CreateDataFile('DEL1', 'data content');
	FCache.TestWriteBaseMeta('DEL1', 100);

	Assert.IsTrue(FileExists(FCache.TestDataFilePath('DEL1')), 'Data file should exist');
	Assert.IsTrue(FileExists(FCache.TestMetaFilePath('DEL1')), 'Meta file should exist');

	FCache.TestDeleteEntry('DEL1');

	Assert.IsFalse(FileExists(FCache.TestDataFilePath('DEL1')), 'Data file should be deleted');
	Assert.IsFalse(FileExists(FCache.TestMetaFilePath('DEL1')), 'Meta file should be deleted');
end;

procedure TBaseDiskCacheTest.Test_GetTotalCacheSize_SumsDataFiles;
var
	TotalSize: Int64;
begin
	CreateDataFile('SIZE1', StringOfChar('A', 1000));
	CreateDataFile('SIZE2', StringOfChar('B', 2000));

	TotalSize := FCache.TestGetTotalCacheSize;
	{BOM + content, allow small tolerance for encoding overhead}
	Assert.IsTrue(TotalSize >= 3000, 'Total size should be at least 3000 bytes');
end;

procedure TBaseDiskCacheTest.Test_EvictIfOverSize_UnderLimit_NoEviction;
begin
	{Create a small file, well under 50 MB limit}
	CreateDataFile('SMALL', 'tiny data');
	FCache.TestWriteBaseMeta('SMALL', 9);

	FCache.TestEvictIfOverSize;

	Assert.IsTrue(FileExists(FCache.TestDataFilePath('SMALL')), 'Entry should survive when under limit');
end;

procedure TBaseDiskCacheTest.Test_EvictIfOverSize_EvictsOldestEntries;
var
	SmallCache: TTestDiskCache;
	SmallCacheDir: WideString;
	GUID: TGUID;
	I: Integer;
	Content: string;
begin
	{Create cache with 1 MB limit}
	CreateGUID(GUID);
	SmallCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuBaseCacheTest_Evict_' + GUIDToString(GUID);
	SmallCache := TTestDiskCache.Create(SmallCacheDir, 3600, 1);
	try
		Content := StringOfChar('X', 200 * 1024); // 200 KB each
		for I := 1 to 8 do
		begin
			TFile.WriteAllText(SmallCache.TestDataFilePath('EVICT' + IntToStr(I)), Content);
			SmallCache.TestWriteBaseMeta('EVICT' + IntToStr(I), 200 * 1024);
			Sleep(20); // ensure distinct ExpiresAt
		end;

		SmallCache.TestEvictIfOverSize;

		{Most recent entry should survive}
		Assert.IsTrue(FileExists(SmallCache.TestDataFilePath('EVICT8')),
			'Most recent entry should survive eviction');
		{Earliest entry should be evicted}
		Assert.IsFalse(FileExists(SmallCache.TestDataFilePath('EVICT1')),
			'Earliest entry should be evicted');
	finally
		SmallCache.Free;
		if DirectoryExists(SmallCacheDir) then
			TDirectory.Delete(SmallCacheDir, True);
	end;
end;

procedure TBaseDiskCacheTest.Test_EvictIfOverSize_BreaksMidLoop;
var
	SmallCache: TTestDiskCache;
	SmallCacheDir: WideString;
	GUID: TGUID;
	I, SurvivedCount: Integer;
	Content: string;
begin
	CreateGUID(GUID);
	SmallCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuBaseCacheTest_MidLoop_' + GUIDToString(GUID);
	SmallCache := TTestDiskCache.Create(SmallCacheDir, 3600, 1);
	try
		Content := StringOfChar('Y', 100 * 1024); // 100 KB each, 15 entries = 1.5 MB
		for I := 1 to 15 do
		begin
			TFile.WriteAllText(SmallCache.TestDataFilePath('BREAK' + IntToStr(I)), Content);
			SmallCache.TestWriteBaseMeta('BREAK' + IntToStr(I), 100 * 1024);
			Sleep(15);
		end;

		SmallCache.TestEvictIfOverSize;

		SurvivedCount := 0;
		for I := 1 to 15 do
		begin
			if FileExists(SmallCache.TestDataFilePath('BREAK' + IntToStr(I))) then
				Inc(SurvivedCount);
		end;

		Assert.IsTrue(SurvivedCount > 0, 'Some entries should survive eviction');
		Assert.IsTrue(SurvivedCount < 15, 'Not all entries should survive -- some must be evicted');
		Assert.IsTrue(FileExists(SmallCache.TestDataFilePath('BREAK15')),
			'Most recent entry should survive');
	finally
		SmallCache.Free;
		if DirectoryExists(SmallCacheDir) then
			TDirectory.Delete(SmallCacheDir, True);
	end;
end;

procedure TBaseDiskCacheTest.Test_EvictIfOverSize_SkipsCorruptMeta;
var
	SmallCache: TTestDiskCache;
	SmallCacheDir: WideString;
	GUID: TGUID;
	I: Integer;
	Content: string;
	Writer: TStreamWriter;
begin
	CreateGUID(GUID);
	SmallCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuBaseCacheTest_CorruptEvict_' + GUIDToString(GUID);
	SmallCache := TTestDiskCache.Create(SmallCacheDir, 3600, 1);
	try
		Content := StringOfChar('Z', 200 * 1024);
		for I := 1 to 6 do
		begin
			TFile.WriteAllText(SmallCache.TestDataFilePath('CORR' + IntToStr(I)), Content);
			SmallCache.TestWriteBaseMeta('CORR' + IntToStr(I), 200 * 1024);
			Sleep(20);
		end;

		{Corrupt the .meta for a middle entry}
		Writer := TStreamWriter.Create(SmallCache.TestMetaFilePath('CORR3'), False, TEncoding.UTF8);
		try
			Writer.Write('Garbage=nothing');
		finally
			Writer.Free;
		end;

		{Add one more entry to trigger eviction}
		TFile.WriteAllText(SmallCache.TestDataFilePath('CORR_FINAL'), Content);
		SmallCache.TestWriteBaseMeta('CORR_FINAL', 200 * 1024);

		SmallCache.TestEvictIfOverSize;

		Assert.IsTrue(FileExists(SmallCache.TestDataFilePath('CORR_FINAL')),
			'Final entry should survive eviction even with corrupt meta in the mix');
	finally
		SmallCache.Free;
		if DirectoryExists(SmallCacheDir) then
			TDirectory.Delete(SmallCacheDir, True);
	end;
end;

procedure TBaseDiskCacheTest.Test_EvictIfOverSize_SameExpiresAt;
var
	SmallCache: TTestDiskCache;
	SmallCacheDir: WideString;
	GUID: TGUID;
	I, SurvivedCount: Integer;
	Content: string;
	MetaLines: TStringList;
	SharedExpiry: string;
begin
	CreateGUID(GUID);
	SmallCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuBaseCacheTest_SameExpiry_' + GUIDToString(GUID);
	SmallCache := TTestDiskCache.Create(SmallCacheDir, 3600, 1);
	try
		Content := StringOfChar('W', 200 * 1024);
		SharedExpiry := FloatToStr(Now + 3600 / SecsPerDay);

		{Create entries with data files}
		for I := 1 to 8 do
			TFile.WriteAllText(SmallCache.TestDataFilePath('SAME' + IntToStr(I)), Content);

		{Write all .meta files with identical ExpiresAt}
		for I := 1 to 8 do
		begin
			MetaLines := TStringList.Create;
			try
				MetaLines.Add('ExpiresAt=' + SharedExpiry);
				MetaLines.Add('DataSize=' + IntToStr(200 * 1024));
				MetaLines.SaveToFile(SmallCache.TestMetaFilePath('SAME' + IntToStr(I)), TEncoding.UTF8);
			finally
				MetaLines.Free;
			end;
		end;

		SmallCache.TestEvictIfOverSize;

		SurvivedCount := 0;
		for I := 1 to 8 do
		begin
			if FileExists(SmallCache.TestDataFilePath('SAME' + IntToStr(I))) then
				Inc(SurvivedCount);
		end;

		Assert.IsTrue(SurvivedCount < 8,
			'Some entries with same ExpiresAt should be evicted');
		Assert.IsTrue(SurvivedCount > 0,
			'Not all entries should be evicted');
	finally
		SmallCache.Free;
		if DirectoryExists(SmallCacheDir) then
			TDirectory.Delete(SmallCacheDir, True);
	end;
end;

procedure TBaseDiskCacheTest.Test_DoInvalidateAll_ClearsAllFiles;
var
	SR: TSearchRec;
	FileCount: Integer;
begin
	CreateDataFile('INV1', 'data1');
	FCache.TestWriteBaseMeta('INV1', 5);
	CreateDataFile('INV2', 'data2');
	FCache.TestWriteBaseMeta('INV2', 5);

	FCache.TestDoInvalidateAll;

	FileCount := 0;
	if FindFirst(IncludeTrailingPathDelimiter(FCacheDir) + '*.*', faAnyFile, SR) = 0 then
	begin
		repeat
			if (SR.Name <> '.') and (SR.Name <> '..') then
				Inc(FileCount);
		until FindNext(SR) <> 0;
		FindClose(SR);
	end;

	Assert.AreEqual(0, FileCount, 'All cache files should be removed');
end;

procedure TBaseDiskCacheTest.Test_DoInvalidateAll_EmptyCache_NoError;
begin
	FCache.TestDoInvalidateAll;
	Assert.Pass('DoInvalidateAll on empty cache should not raise');
end;

initialization

TDUnitX.RegisterTestFixture(TBaseDiskCacheTest);

end.
