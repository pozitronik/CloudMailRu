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
		FFilesDir: WideString;
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
		{TTL expiration deletes both .dat and .meta files from disk}
		procedure Test_TTL_Expiration_CleansUpFiles;

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
		{TryGet with empty hash returns False without error}
		procedure Test_TryGet_EmptyHash_ReturnsFalse;

		[Test]
		{Put with empty hash is silently ignored}
		procedure Test_Put_EmptyHash_Ignored;

		[Test]
		{Put with non-existent source file is silently ignored}
		procedure Test_Put_NonExistentSource_Ignored;

		[Test]
		{TryGet with missing .dat but existing .meta cleans up orphaned meta}
		procedure Test_TryGet_MissingDatFile_CleansUpMeta;

		[Test]
		{TryGet with corrupted meta (ExpiresAt=0) returns False}
		procedure Test_TryGet_CorruptedMeta_ReturnsFalse;

		[Test]
		{Multiple independent hashes coexist without interference}
		procedure Test_MultipleHashes_Independent;

		[Test]
		{Put with CopyFileW failure does not write meta}
		procedure Test_Put_CopyFailure_NoMeta;

		[Test]
		{TryGet with CopyFileW failure returns False}
		procedure Test_TryGet_CopyFailure_ReturnsFalse;
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
	IOUtils, Classes, DateUtils;

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
	FFilesDir := FCacheDir + 'files\';
	ForceDirectories(FTestFilesDir);
	FCache := TDiskFileCache.Create(FFilesDir, 60, 50);
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

procedure TDiskFileCacheTest.Test_TTL_Expiration_CleansUpFiles;
var
	ShortTTLCache: TDiskFileCache;
	ShortCacheDir, FilesDir: WideString;
	SourcePath, DestPath: WideString;
	GUID: TGUID;
begin
	CreateGUID(GUID);
	ShortCacheDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuFileCacheTest_TTLClean_' + GUIDToString(GUID) + '\';
	FilesDir := ShortCacheDir + 'files\';
	ForceDirectories(ShortCacheDir);
	ShortTTLCache := TDiskFileCache.Create(FilesDir, 1, 50);
	try
		SourcePath := CreateTestFile('ttl_cleanup.bin', 'will expire');
		DestPath := FTestFilesDir + 'ttl_cleanup_out.bin';

		ShortTTLCache.Put('CLEANUP_HASH', SourcePath);

		Assert.IsTrue(FileExists(FilesDir + 'CLEANUP_HASH.dat'), 'Dat file should exist after Put');
		Assert.IsTrue(FileExists(FilesDir + 'CLEANUP_HASH.meta'), 'Meta file should exist after Put');

		Sleep(1100);

		{TryGet on expired entry triggers cleanup}
		ShortTTLCache.TryGet('CLEANUP_HASH', DestPath);

		Assert.IsFalse(FileExists(FilesDir + 'CLEANUP_HASH.dat'), 'Dat file should be deleted after expiry');
		Assert.IsFalse(FileExists(FilesDir + 'CLEANUP_HASH.meta'), 'Meta file should be deleted after expiry');
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
begin
	SourcePath := CreateTestFile('sidecar.bin', 'sidecar test');
	FCache.Put('SIDECAR_HASH', SourcePath);

	DatCount := 0;
	MetaCount := 0;
	if FindFirst(FFilesDir + '*.dat', faAnyFile, SR) = 0 then
	begin
		repeat
			Inc(DatCount);
		until FindNext(SR) <> 0;
		FindClose(SR);
	end;
	if FindFirst(FFilesDir + '*.meta', faAnyFile, SR) = 0 then
	begin
		repeat
			Inc(MetaCount);
		until FindNext(SR) <> 0;
		FindClose(SR);
	end;

	Assert.AreEqual(1, DatCount, 'Should have one .dat file');
	Assert.AreEqual(1, MetaCount, 'Should have one .meta file');
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
	Assert.IsFalse(FCache.TryGet('', DestPath));
end;

procedure TDiskFileCacheTest.Test_Put_NonExistentSource_Ignored;
begin
	FCache.Put('HASH_NOFILE', FTestFilesDir + 'does_not_exist.bin');
	Assert.IsFalse(FCache.TryGet('HASH_NOFILE', FTestFilesDir + 'out.bin'));
end;

procedure TDiskFileCacheTest.Test_TryGet_MissingDatFile_CleansUpMeta;
var
	SourcePath, DestPath: WideString;
begin
	SourcePath := CreateTestFile('orphan.bin', 'orphan content');
	FCache.Put('ORPHAN_HASH', SourcePath);

	{Delete the .dat file, leaving .meta orphaned}
	Assert.IsTrue(DeleteFile(FFilesDir + 'ORPHAN_HASH.dat'), 'Should delete dat file');
	Assert.IsTrue(FileExists(FFilesDir + 'ORPHAN_HASH.meta'), 'Meta should still exist');

	DestPath := FTestFilesDir + 'orphan_out.bin';
	Assert.IsFalse(FCache.TryGet('ORPHAN_HASH', DestPath), 'Should miss when dat file is gone');
	Assert.IsFalse(FileExists(FFilesDir + 'ORPHAN_HASH.meta'), 'Orphaned meta should be cleaned up');
end;

procedure TDiskFileCacheTest.Test_TryGet_CorruptedMeta_ReturnsFalse;
var
	SourcePath, DestPath: WideString;
	Writer: TStreamWriter;
begin
	SourcePath := CreateTestFile('badmeta.bin', 'bad meta content');
	FCache.Put('BADMETA_HASH', SourcePath);

	{Overwrite .meta with content where ExpiresAt=0 (ReadMeta returns False)}
	Writer := TStreamWriter.Create(FFilesDir + 'BADMETA_HASH.meta', False, TEncoding.UTF8);
	try
		Writer.Write('DataSize=100');
	finally
		Writer.Free;
	end;

	DestPath := FTestFilesDir + 'badmeta_out.bin';
	Assert.IsFalse(FCache.TryGet('BADMETA_HASH', DestPath), 'Should return False for meta without ExpiresAt');
end;

procedure TDiskFileCacheTest.Test_MultipleHashes_Independent;
var
	Source1, Source2, Dest1, Dest2: WideString;
begin
	Source1 := CreateTestFile('multi1.bin', 'content alpha');
	Source2 := CreateTestFile('multi2.bin', 'content beta');
	Dest1 := FTestFilesDir + 'out1.bin';
	Dest2 := FTestFilesDir + 'out2.bin';

	FCache.Put('HASH_ALPHA', Source1);
	FCache.Put('HASH_BETA', Source2);

	Assert.IsTrue(FCache.TryGet('HASH_ALPHA', Dest1));
	Assert.IsTrue(FCache.TryGet('HASH_BETA', Dest2));
	Assert.AreEqual('content alpha', ReadFileContent(Dest1));
	Assert.AreEqual('content beta', ReadFileContent(Dest2));
end;

procedure TDiskFileCacheTest.Test_Put_CopyFailure_NoMeta;
var
	SourcePath: WideString;
begin
	SourcePath := CreateTestFile('copy_fail.bin', 'content');

	{Try to put with hash that would write to a non-existent subdirectory.
	 CopyFileW will fail because the cache dir + hash is valid,
	 so instead we use a cache with a non-existent directory.}
	var FailCache := TDiskFileCache.Create(FCacheDir + 'nonexistent_sub\deeper\', 60, 50);
	try
		{ForceDirectories in Create should have created the dir. Delete it to simulate failure.}
		if DirectoryExists(FCacheDir + 'nonexistent_sub\deeper\') then
			TDirectory.Delete(FCacheDir + 'nonexistent_sub\deeper\', True);
		if DirectoryExists(FCacheDir + 'nonexistent_sub\') then
			TDirectory.Delete(FCacheDir + 'nonexistent_sub\', True);

		FailCache.Put('FAIL_HASH', SourcePath);

		{CopyFileW should fail because dest dir doesn't exist; no meta should be written}
		Assert.IsFalse(FileExists(FCacheDir + 'nonexistent_sub\deeper\FAIL_HASH.meta'),
			'Meta should not be written when copy fails');
	finally
		FailCache.Free;
	end;
end;

procedure TDiskFileCacheTest.Test_TryGet_CopyFailure_ReturnsFalse;
var
	SourcePath, DestPath: WideString;
begin
	SourcePath := CreateTestFile('copy_get_fail.bin', 'content');
	FCache.Put('COPY_FAIL_HASH', SourcePath);

	{Try to get to a path in a non-existent directory -- CopyFileW fails}
	DestPath := FCacheDir + 'nonexistent_output_dir\out.bin';
	Assert.IsFalse(FCache.TryGet('COPY_FAIL_HASH', DestPath),
		'Should return False when CopyFileW cannot write to destination');
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
