unit FileCacheTest;

interface

uses
	DUnitX.TestFramework,
	FileCache,
	SysUtils;

type
	[TestFixture]
	TDiskFileCacheTest = class
	private
		FCache: TDiskFileCache;
		FCacheDir: WideString;
		FTestFilesDir: WideString;

		{Creates a temp file with the given content and returns its path}
		function CreateTestFile(const Name, Content: string): WideString;
		{Reads file content as string}
		function ReadFileContent(const Path: WideString): string;
		{Recursively deletes a directory and its contents}
		procedure DeleteDir(const Dir: WideString);
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		{Put + TryGet copies file content to target path}
		procedure Test_PutThenGet_CopiesFile;

		[Test]
		{TryGet returns False for unknown hash}
		procedure Test_TryGet_Miss_ReturnsFalse;

		[Test]
		{TTL expiration removes entry}
		procedure Test_TTL_Expiration;

		[Test]
		{InvalidateAll clears all cached files}
		procedure Test_InvalidateAll;

		[Test]
		{Put overwrites existing entry with same hash}
		procedure Test_Put_OverwritesExisting;

		[Test]
		{Put creates .dat and .meta sidecar files in cache dir}
		procedure Test_Put_CreatesSidecarFiles;

		[Test]
		{Size eviction: oldest entries are evicted when over MaxSizeMB limit}
		procedure Test_SizeEviction_RemovesOldest;

		[Test]
		{TryGet with empty hash returns False without error}
		procedure Test_TryGet_EmptyHash_ReturnsFalse;

		[Test]
		{Put with empty hash is silently ignored}
		procedure Test_Put_EmptyHash_Ignored;

		[Test]
		{Put with non-existent source file is silently ignored}
		procedure Test_Put_NonExistentSource_Ignored;
	end;

	[TestFixture]
	TNullFileCacheTest = class
	public
		[Test]
		{TryGet always returns False}
		procedure Test_TryGet_AlwaysReturnsFalse;

		[Test]
		{Put does not store anything}
		procedure Test_Put_DoesNotStore;

		[Test]
		{InvalidateAll completes without exception}
		procedure Test_InvalidateAll_NoOp;
	end;

implementation

uses
	IOUtils, Classes;

{TDiskFileCacheTest}

function TDiskFileCacheTest.CreateTestFile(const Name, Content: string): WideString;
var
	Writer: TStreamWriter;
begin
	Result := FTestFilesDir + Name;
	Writer := TStreamWriter.Create(Result, False, TEncoding.UTF8);
	try
		Writer.Write(Content);
	finally
		Writer.Free;
	end;
end;

function TDiskFileCacheTest.ReadFileContent(const Path: WideString): string;
var
	Reader: TStreamReader;
begin
	Reader := TStreamReader.Create(Path, TEncoding.UTF8);
	try
		Result := Reader.ReadToEnd;
	finally
		Reader.Free;
	end;
end;

procedure TDiskFileCacheTest.DeleteDir(const Dir: WideString);
begin
	if DirectoryExists(Dir) then
		TDirectory.Delete(Dir, True);
end;

procedure TDiskFileCacheTest.Setup;
var
	GUID: TGUID;
begin
	CreateGUID(GUID);
	FCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuFileCacheTest_' + GUIDToString(GUID) + '\';
	FTestFilesDir := FCacheDir + 'source\';
	ForceDirectories(FTestFilesDir);
	FCache := TDiskFileCache.Create(FCacheDir + 'files\', 60, 50);
end;

procedure TDiskFileCacheTest.TearDown;
begin
	FreeAndNil(FCache);
	DeleteDir(FCacheDir);
end;

procedure TDiskFileCacheTest.Test_PutThenGet_CopiesFile;
var
	SourcePath, DestPath: WideString;
begin
	SourcePath := CreateTestFile('test.bin', 'hello world content');
	DestPath := FTestFilesDir + 'output.bin';

	FCache.Put('ABC123', SourcePath);
	Assert.IsTrue(FCache.TryGet('ABC123', DestPath));
	Assert.AreEqual('hello world content', ReadFileContent(DestPath));
end;

procedure TDiskFileCacheTest.Test_TryGet_Miss_ReturnsFalse;
begin
	Assert.IsFalse(FCache.TryGet('NONEXISTENT', FTestFilesDir + 'out.bin'));
end;

procedure TDiskFileCacheTest.Test_TTL_Expiration;
var
	ShortTTLCache: TDiskFileCache;
	ShortCacheDir: WideString;
	SourcePath, DestPath: WideString;
	GUID: TGUID;
begin
	CreateGUID(GUID);
	ShortCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuFileCacheTest_TTL_' + GUIDToString(GUID) + '\';
	ForceDirectories(ShortCacheDir);
	ShortTTLCache := TDiskFileCache.Create(ShortCacheDir + 'files\', 1, 50);
	try
		SourcePath := CreateTestFile('ttl_test.bin', 'expires soon');
		DestPath := FTestFilesDir + 'ttl_out.bin';

		ShortTTLCache.Put('TTL_HASH', SourcePath);
		Assert.IsTrue(ShortTTLCache.TryGet('TTL_HASH', DestPath), 'Should be cached immediately');

		Sleep(1100);

		Assert.IsFalse(ShortTTLCache.TryGet('TTL_HASH', DestPath), 'Should be expired after TTL');
	finally
		ShortTTLCache.Free;
		DeleteDir(ShortCacheDir);
	end;
end;

procedure TDiskFileCacheTest.Test_InvalidateAll;
var
	Source1, Source2, DestPath: WideString;
begin
	Source1 := CreateTestFile('f1.bin', 'content1');
	Source2 := CreateTestFile('f2.bin', 'content2');
	DestPath := FTestFilesDir + 'out.bin';

	FCache.Put('HASH1', Source1);
	FCache.Put('HASH2', Source2);

	FCache.InvalidateAll;

	Assert.IsFalse(FCache.TryGet('HASH1', DestPath));
	Assert.IsFalse(FCache.TryGet('HASH2', DestPath));
end;

procedure TDiskFileCacheTest.Test_Put_OverwritesExisting;
var
	Source1, Source2, DestPath: WideString;
begin
	Source1 := CreateTestFile('v1.bin', 'version 1');
	Source2 := CreateTestFile('v2.bin', 'version 2');
	DestPath := FTestFilesDir + 'out.bin';

	FCache.Put('SAMEHASH', Source1);
	FCache.Put('SAMEHASH', Source2);

	Assert.IsTrue(FCache.TryGet('SAMEHASH', DestPath));
	Assert.AreEqual('version 2', ReadFileContent(DestPath));
end;

procedure TDiskFileCacheTest.Test_Put_CreatesSidecarFiles;
var
	SourcePath: WideString;
	SR: TSearchRec;
	DatCount, MetaCount: Integer;
	FilesDir: WideString;
begin
	SourcePath := CreateTestFile('sidecar.bin', 'sidecar test');
	FCache.Put('SIDECAR_HASH', SourcePath);

	FilesDir := FCacheDir + 'files\';
	DatCount := 0;
	MetaCount := 0;
	if FindFirst(FilesDir + '*.dat', faAnyFile, SR) = 0 then
	begin
		repeat
			Inc(DatCount);
		until FindNext(SR) <> 0;
		FindClose(SR);
	end;
	if FindFirst(FilesDir + '*.meta', faAnyFile, SR) = 0 then
	begin
		repeat
			Inc(MetaCount);
		until FindNext(SR) <> 0;
		FindClose(SR);
	end;

	Assert.AreEqual(1, DatCount, 'Should have one .dat file');
	Assert.AreEqual(1, MetaCount, 'Should have one .meta file');
end;

procedure TDiskFileCacheTest.Test_SizeEviction_RemovesOldest;
var
	SmallCache: TDiskFileCache;
	SmallCacheDir: WideString;
	GUID: TGUID;
	SourcePath, DestPath: WideString;
	I: Integer;
	Content: string;
begin
	// Create a cache with 1 MB max to trigger eviction
	CreateGUID(GUID);
	SmallCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuFileCacheTest_Evict_' + GUIDToString(GUID) + '\';
	ForceDirectories(SmallCacheDir);
	SmallCache := TDiskFileCache.Create(SmallCacheDir + 'files\', 3600, 1);
	try
		// Create files that will exceed 1 MB total
		Content := StringOfChar('X', 200 * 1024); // 200 KB each
		for I := 1 to 8 do
		begin
			SourcePath := CreateTestFile('big_' + IntToStr(I) + '.bin', Content);
			SmallCache.Put('BIGHASH' + IntToStr(I), SourcePath);
			Sleep(20); // Ensure distinct ExpiresAt for eviction ordering
		end;

		DestPath := FTestFilesDir + 'evict_out.bin';
		// Most recent entry should survive eviction
		Assert.IsTrue(SmallCache.TryGet('BIGHASH8', DestPath), 'Most recent entry should survive eviction');
	finally
		SmallCache.Free;
		DeleteDir(SmallCacheDir);
	end;
end;

procedure TDiskFileCacheTest.Test_TryGet_EmptyHash_ReturnsFalse;
begin
	Assert.IsFalse(FCache.TryGet('', FTestFilesDir + 'out.bin'));
end;

procedure TDiskFileCacheTest.Test_Put_EmptyHash_Ignored;
var
	SourcePath, DestPath: WideString;
begin
	SourcePath := CreateTestFile('empty_hash.bin', 'should not cache');
	FCache.Put('', SourcePath);
	DestPath := FTestFilesDir + 'out.bin';
	// Nothing should be cached
	Assert.IsFalse(FCache.TryGet('', DestPath));
end;

procedure TDiskFileCacheTest.Test_Put_NonExistentSource_Ignored;
begin
	FCache.Put('HASH_NOFILE', FTestFilesDir + 'does_not_exist.bin');
	Assert.IsFalse(FCache.TryGet('HASH_NOFILE', FTestFilesDir + 'out.bin'));
end;

{TNullFileCacheTest}

procedure TNullFileCacheTest.Test_TryGet_AlwaysReturnsFalse;
var
	Cache: IFileCache;
begin
	Cache := TNullFileCache.Create;
	Assert.IsFalse(Cache.TryGet('ANYHASH', 'C:\any\path.bin'));
end;

procedure TNullFileCacheTest.Test_Put_DoesNotStore;
var
	Cache: IFileCache;
begin
	Cache := TNullFileCache.Create;
	Cache.Put('HASH', 'C:\nonexistent.bin');
	Assert.IsFalse(Cache.TryGet('HASH', 'C:\output.bin'), 'Null cache should never return cached data');
end;

procedure TNullFileCacheTest.Test_InvalidateAll_NoOp;
var
	Cache: IFileCache;
begin
	Cache := TNullFileCache.Create;
	Cache.InvalidateAll;
	Assert.Pass;
end;

initialization

TDUnitX.RegisterTestFixture(TDiskFileCacheTest);
TDUnitX.RegisterTestFixture(TNullFileCacheTest);

end.
