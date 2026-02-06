unit FileSystemTest;

{Tests for FileSystem unit - IFileSystem implementations.
 Tests TNullFileSystem, TMemoryFileSystem, TWindowsFileSystem, and TOwningStreamReader.}

interface

uses
	FileSystem,
	System.SysUtils,
	System.Classes,
	DUnitX.TestFramework;

type
	{Tests for TNullFileSystem - null implementation that returns empty/false}
	[TestFixture]
	TNullFileSystemTest = class
	private
		FFileSystem: TNullFileSystem;
		FFileSystemRef: IFileSystem;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestFileExists_AlwaysReturnsFalse;
		[Test]
		procedure TestGetFileSize_AlwaysReturnsMinusOne;
		[Test]
		procedure TestCreateEmptyFile_NoOp;
		[Test]
		procedure TestDeleteFile_NoOp;
		[Test]
		procedure TestReadFileHeader_ReturnsEmptyArray;
		[Test]
		procedure TestReadAllText_ReturnsEmptyString;
		[Test]
		procedure TestReadAllLines_ReturnsEmptyStringList;
		[Test]
		procedure TestWriteAllText_NoOp;
		[Test]
		procedure TestWriteAllLines_NoOp;
		[Test]
		procedure TestOpenTextReader_ReturnsEmptyReader;
		[Test]
		procedure TestGetTmpFileName_ReturnsEmpty;
		[Test]
		procedure TestSetFileTime_NoOp;
		[Test]
		procedure TestImplementsIFileSystem;
		[Test]
		procedure TestFindFiles_ReturnsEmptyList;
		[Test]
		procedure TestGetFileModTime_ReturnsZero;
	end;

	{Tests for TMemoryFileSystem - in-memory implementation for testing}
	[TestFixture]
	TMemoryFileSystemTest = class
	private
		FFileSystem: TMemoryFileSystem;
		FFileSystemRef: IFileSystem;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ FileExists tests }
		[Test]
		procedure TestFileExists_NonExistent_ReturnsFalse;
		[Test]
		procedure TestFileExists_Existing_ReturnsTrue;

		{ GetFileSize tests }
		[Test]
		procedure TestGetFileSize_NonExistent_ReturnsMinusOne;
		[Test]
		procedure TestGetFileSize_EmptyFile_ReturnsZero;
		[Test]
		procedure TestGetFileSize_WithContent_ReturnsCorrectSize;

		{ CreateEmptyFile tests }
		[Test]
		procedure TestCreateEmptyFile_CreatesFile;
		[Test]
		procedure TestCreateEmptyFile_OverwritesExisting;

		{ DeleteFile tests }
		[Test]
		procedure TestDeleteFile_RemovesFile;
		[Test]
		procedure TestDeleteFile_NonExistent_NoError;

		{ ReadFileHeader tests }
		[Test]
		procedure TestReadFileHeader_EmptyFile_ReturnsEmpty;
		[Test]
		procedure TestReadFileHeader_ShortFile_ReturnsAllContent;
		[Test]
		procedure TestReadFileHeader_LongFile_ReturnsTruncated;

		{ ReadAllText tests }
		[Test]
		procedure TestReadAllText_NonExistent_ReturnsEmpty;
		[Test]
		procedure TestReadAllText_WithContent_ReturnsContent;

		{ ReadAllLines tests }
		[Test]
		procedure TestReadAllLines_NonExistent_ReturnsEmptyList;
		[Test]
		procedure TestReadAllLines_MultipleLines_ParsesCorrectly;

		{ WriteAllText tests }
		[Test]
		procedure TestWriteAllText_CreatesFile;
		[Test]
		procedure TestWriteAllText_OverwritesExisting;

		{ WriteAllLines tests }
		[Test]
		procedure TestWriteAllLines_CreatesFile;

		{ OpenTextReader tests }
		[Test]
		procedure TestOpenTextReader_ReturnsWorkingReader;

		{ GetTmpFileName tests }
		[Test]
		procedure TestGetTmpFileName_ReturnsUniquePaths;
		[Test]
		procedure TestGetTmpFileName_CreatesFile;
		[Test]
		procedure TestGetTmpFileName_IncludesPrefix;

		{ Helper methods tests }
		[Test]
		procedure TestSetFileContent_GetFileContent_RoundTrip;
		[Test]
		procedure TestClear_RemovesAllFiles;
		[Test]
		procedure TestCaseInsensitive;
		[Test]
		procedure TestSetFileTime_TracksCalls;
		[Test]
		procedure TestImplementsIFileSystem;

		{ FindFiles tests }
		[Test]
		procedure TestFindFiles_MatchesPattern;
		[Test]
		procedure TestFindFiles_NoMatches_ReturnsEmpty;
		[Test]
		procedure TestFindFiles_MultipleMatches;

		{ GetFileModTime tests }
		[Test]
		procedure TestGetFileModTime_NoValue_ReturnsZero;
		[Test]
		procedure TestGetFileModTime_WithValue_ReturnsStored;
		[Test]
		procedure TestGetFileModTime_ClearedByReset;
	end;

	{Tests for TWindowsFileSystem - actual file system implementation}
	[TestFixture]
	TWindowsFileSystemTest = class
	private
		FFileSystem: TWindowsFileSystem;
		FFileSystemRef: IFileSystem;
		FTempDir: String;
		FTempFiles: TStringList;

		function CreateTempFile(const Content: String = ''): String;
		procedure CleanupTempFiles;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ FileExists tests }
		[Test]
		procedure TestFileExists_NonExistent_ReturnsFalse;
		[Test]
		procedure TestFileExists_Existing_ReturnsTrue;

		{ GetFileSize tests }
		[Test]
		procedure TestGetFileSize_NonExistent_ReturnsMinusOne;
		[Test]
		procedure TestGetFileSize_EmptyFile_ReturnsZero;
		[Test]
		procedure TestGetFileSize_WithContent_ReturnsCorrectSize;

		{ CreateEmptyFile tests }
		[Test]
		procedure TestCreateEmptyFile_CreatesFile;

		{ DeleteFile tests }
		[Test]
		procedure TestDeleteFile_RemovesFile;
		[Test]
		procedure TestDeleteFile_NonExistent_NoError;

		{ ReadFileHeader tests }
		[Test]
		procedure TestReadFileHeader_NonExistent_ReturnsEmpty;
		[Test]
		procedure TestReadFileHeader_ShortFile_ReturnsAllContent;
		[Test]
		procedure TestReadFileHeader_LongFile_ReturnsTruncated;

		{ ReadAllText tests }
		[Test]
		procedure TestReadAllText_NonExistent_ReturnsEmpty;
		[Test]
		procedure TestReadAllText_WithContent_ReturnsContent;

		{ ReadAllLines tests }
		[Test]
		procedure TestReadAllLines_NonExistent_ReturnsEmptyList;
		[Test]
		procedure TestReadAllLines_MultipleLines_ParsesCorrectly;

		{ WriteAllText tests }
		[Test]
		procedure TestWriteAllText_CreatesFile;
		[Test]
		procedure TestWriteAllText_ReadBack_Matches;

		{ WriteAllLines tests }
		[Test]
		procedure TestWriteAllLines_CreatesFile;

		{ OpenTextReader tests }
		[Test]
		procedure TestOpenTextReader_ReturnsWorkingReader;

		{ GetTmpFileName tests }
		[Test]
		procedure TestGetTmpFileName_ReturnsNonEmpty;
		[Test]
		procedure TestGetTmpFileName_CreatesFile;
		[Test]
		procedure TestGetTmpFileName_WithPrefix;
		[Test]
		procedure TestGetTmpFileName_ReturnsUniquePaths;

		{ SetFileTime tests }
		[Test]
		procedure TestSetFileTime_SetsTimestamp;
		[Test]
		procedure TestSetFileTime_NonExistentFile_NoError;

		[Test]
		procedure TestImplementsIFileSystem;

		{ GetFileModTime tests }
		[Test]
		procedure TestGetFileModTime_NonExistent_ReturnsZero;
		[Test]
		procedure TestGetFileModTime_ExistingFile_ReturnsNonZero;
	end;

	{Tests for TOwningStreamReader - stream reader that owns its stream}
	[TestFixture]
	TOwningStreamReaderTest = class
	public
		[Test]
		procedure TestCreate_ReadsContent;
		[Test]
		procedure TestDestroy_FreesStream;
	end;

implementation

uses
	Windows;

{ TNullFileSystemTest }

procedure TNullFileSystemTest.Setup;
begin
	FFileSystem := TNullFileSystem.Create;
	FFileSystemRef := FFileSystem;
end;

procedure TNullFileSystemTest.TearDown;
begin
	FFileSystemRef := nil;
end;

procedure TNullFileSystemTest.TestFileExists_AlwaysReturnsFalse;
begin
	Assert.IsFalse(FFileSystem.FileExists('C:\any\path\file.txt'));
	Assert.IsFalse(FFileSystem.FileExists(''));
	Assert.IsFalse(FFileSystem.FileExists('nonexistent'));
end;

procedure TNullFileSystemTest.TestGetFileSize_AlwaysReturnsMinusOne;
begin
	Assert.AreEqual(Int64(-1), FFileSystem.GetFileSize('C:\any\file.txt'));
end;

procedure TNullFileSystemTest.TestCreateEmptyFile_NoOp;
begin
	{Should not raise exception}
	FFileSystem.CreateEmptyFile('C:\any\file.txt');
	Assert.IsFalse(FFileSystem.FileExists('C:\any\file.txt'), 'Null implementation should not create files');
end;

procedure TNullFileSystemTest.TestDeleteFile_NoOp;
begin
	{Should not raise exception}
	FFileSystem.DeleteFile('C:\any\file.txt');
	Assert.Pass('DeleteFile should complete without error');
end;

procedure TNullFileSystemTest.TestReadFileHeader_ReturnsEmptyArray;
var
	Header: TBytes;
begin
	Header := FFileSystem.ReadFileHeader('C:\any\file.txt', 100);
	Assert.AreEqual(Integer(0), Integer(Length(Header)), 'Should return empty array');
end;

procedure TNullFileSystemTest.TestReadAllText_ReturnsEmptyString;
begin
	Assert.AreEqual('', String(FFileSystem.ReadAllText('C:\any\file.txt', TEncoding.UTF8)));
end;

procedure TNullFileSystemTest.TestReadAllLines_ReturnsEmptyStringList;
var
	Lines: TStringList;
begin
	Lines := FFileSystem.ReadAllLines('C:\any\file.txt', TEncoding.UTF8);
	try
		Assert.AreEqual(0, Lines.Count, 'Should return empty string list');
	finally
		Lines.Free;
	end;
end;

procedure TNullFileSystemTest.TestWriteAllText_NoOp;
begin
	{Should not raise exception}
	FFileSystem.WriteAllText('C:\any\file.txt', 'content', TEncoding.UTF8);
	Assert.Pass('WriteAllText should complete without error');
end;

procedure TNullFileSystemTest.TestWriteAllLines_NoOp;
var
	Lines: TStringList;
begin
	Lines := TStringList.Create;
	try
		Lines.Add('Line 1');
		FFileSystem.WriteAllLines('C:\any\file.txt', Lines, TEncoding.UTF8);
		Assert.Pass('WriteAllLines should complete without error');
	finally
		Lines.Free;
	end;
end;

procedure TNullFileSystemTest.TestOpenTextReader_ReturnsEmptyReader;
var
	Reader: TStreamReader;
begin
	Reader := FFileSystem.OpenTextReader('C:\any\file.txt', TEncoding.UTF8);
	try
		Assert.IsTrue(Reader.EndOfStream, 'Reader should be at end (empty stream)');
	finally
		Reader.Free;
	end;
end;

procedure TNullFileSystemTest.TestGetTmpFileName_ReturnsEmpty;
begin
	Assert.AreEqual('', String(FFileSystem.GetTmpFileName));
end;

procedure TNullFileSystemTest.TestSetFileTime_NoOp;
var
	FileTime: TFileTime;
begin
	FileTime.dwLowDateTime := 0;
	FileTime.dwHighDateTime := 0;
	{Should not raise exception}
	FFileSystem.SetFileTime('C:\any\file.txt', FileTime);
	Assert.Pass('SetFileTime should complete without error');
end;

procedure TNullFileSystemTest.TestImplementsIFileSystem;
var
	Intf: IFileSystem;
begin
	Intf := TNullFileSystem.Create;
	Assert.IsNotNull(Intf, 'Should implement IFileSystem');
end;

procedure TNullFileSystemTest.TestFindFiles_ReturnsEmptyList;
var
	Files: TStringList;
begin
	Files := FFileSystem.FindFiles('C:\any\*.lng');
	try
		Assert.AreEqual(0, Files.Count, 'Null implementation should return empty list');
	finally
		Files.Free;
	end;
end;

procedure TNullFileSystemTest.TestGetFileModTime_ReturnsZero;
begin
	Assert.AreEqual(Int64(0), FFileSystem.GetFileModTime('C:\any\file.txt'));
end;

{ TMemoryFileSystemTest }

procedure TMemoryFileSystemTest.Setup;
begin
	FFileSystem := TMemoryFileSystem.Create;
	FFileSystemRef := FFileSystem;
end;

procedure TMemoryFileSystemTest.TearDown;
begin
	FFileSystemRef := nil;
end;

procedure TMemoryFileSystemTest.TestFileExists_NonExistent_ReturnsFalse;
begin
	Assert.IsFalse(FFileSystem.FileExists('C:\nonexistent.txt'));
end;

procedure TMemoryFileSystemTest.TestFileExists_Existing_ReturnsTrue;
begin
	FFileSystem.SetFileContent('C:\test.txt', 'content');
	Assert.IsTrue(FFileSystem.FileExists('C:\test.txt'));
end;

procedure TMemoryFileSystemTest.TestGetFileSize_NonExistent_ReturnsMinusOne;
begin
	Assert.AreEqual(Int64(-1), FFileSystem.GetFileSize('C:\nonexistent.txt'));
end;

procedure TMemoryFileSystemTest.TestGetFileSize_EmptyFile_ReturnsZero;
begin
	FFileSystem.SetFileContent('C:\empty.txt', '');
	Assert.AreEqual(Int64(0), FFileSystem.GetFileSize('C:\empty.txt'));
end;

procedure TMemoryFileSystemTest.TestGetFileSize_WithContent_ReturnsCorrectSize;
begin
	FFileSystem.SetFileContent('C:\test.txt', 'Hello');
	Assert.AreEqual(Int64(5), FFileSystem.GetFileSize('C:\test.txt'));
end;

procedure TMemoryFileSystemTest.TestCreateEmptyFile_CreatesFile;
begin
	FFileSystem.CreateEmptyFile('C:\new.txt');
	Assert.IsTrue(FFileSystem.FileExists('C:\new.txt'));
	Assert.AreEqual('', FFileSystem.GetFileContent('C:\new.txt'));
end;

procedure TMemoryFileSystemTest.TestCreateEmptyFile_OverwritesExisting;
begin
	FFileSystem.SetFileContent('C:\test.txt', 'old content');
	FFileSystem.CreateEmptyFile('C:\test.txt');
	Assert.AreEqual('', FFileSystem.GetFileContent('C:\test.txt'));
end;

procedure TMemoryFileSystemTest.TestDeleteFile_RemovesFile;
begin
	FFileSystem.SetFileContent('C:\test.txt', 'content');
	FFileSystem.DeleteFile('C:\test.txt');
	Assert.IsFalse(FFileSystem.FileExists('C:\test.txt'));
end;

procedure TMemoryFileSystemTest.TestDeleteFile_NonExistent_NoError;
begin
	FFileSystem.DeleteFile('C:\nonexistent.txt');
	Assert.Pass('Should not raise exception');
end;

procedure TMemoryFileSystemTest.TestReadFileHeader_EmptyFile_ReturnsEmpty;
var
	Header: TBytes;
begin
	FFileSystem.SetFileContent('C:\empty.txt', '');
	Header := FFileSystem.ReadFileHeader('C:\empty.txt', 10);
	Assert.AreEqual(Integer(0), Integer(Length(Header)));
end;

procedure TMemoryFileSystemTest.TestReadFileHeader_ShortFile_ReturnsAllContent;
var
	Header: TBytes;
begin
	FFileSystem.SetFileContent('C:\short.txt', 'ABC');
	Header := FFileSystem.ReadFileHeader('C:\short.txt', 10);
	Assert.AreEqual(Integer(3), Integer(Length(Header)));
end;

procedure TMemoryFileSystemTest.TestReadFileHeader_LongFile_ReturnsTruncated;
var
	Header: TBytes;
begin
	FFileSystem.SetFileContent('C:\long.txt', 'ABCDEFGHIJ');
	Header := FFileSystem.ReadFileHeader('C:\long.txt', 5);
	Assert.AreEqual(Integer(5), Integer(Length(Header)));
end;

procedure TMemoryFileSystemTest.TestReadAllText_NonExistent_ReturnsEmpty;
begin
	Assert.AreEqual('', FFileSystem.GetFileContent('C:\nonexistent.txt'));
end;

procedure TMemoryFileSystemTest.TestReadAllText_WithContent_ReturnsContent;
begin
	FFileSystem.SetFileContent('C:\test.txt', 'Hello World');
	Assert.AreEqual('Hello World', String(FFileSystem.ReadAllText('C:\test.txt', TEncoding.UTF8)));
end;

procedure TMemoryFileSystemTest.TestReadAllLines_NonExistent_ReturnsEmptyList;
var
	Lines: TStringList;
begin
	Lines := FFileSystem.ReadAllLines('C:\nonexistent.txt', TEncoding.UTF8);
	try
		Assert.AreEqual(0, Lines.Count);
	finally
		Lines.Free;
	end;
end;

procedure TMemoryFileSystemTest.TestReadAllLines_MultipleLines_ParsesCorrectly;
var
	Lines: TStringList;
begin
	FFileSystem.SetFileContent('C:\test.txt', 'Line1'#13#10'Line2'#13#10'Line3');
	Lines := FFileSystem.ReadAllLines('C:\test.txt', TEncoding.UTF8);
	try
		Assert.AreEqual(3, Lines.Count);
		Assert.AreEqual('Line1', Lines[0]);
		Assert.AreEqual('Line2', Lines[1]);
		Assert.AreEqual('Line3', Lines[2]);
	finally
		Lines.Free;
	end;
end;

procedure TMemoryFileSystemTest.TestWriteAllText_CreatesFile;
begin
	FFileSystem.WriteAllText('C:\new.txt', 'content', TEncoding.UTF8);
	Assert.IsTrue(FFileSystem.FileExists('C:\new.txt'));
	Assert.AreEqual('content', FFileSystem.GetFileContent('C:\new.txt'));
end;

procedure TMemoryFileSystemTest.TestWriteAllText_OverwritesExisting;
begin
	FFileSystem.SetFileContent('C:\test.txt', 'old');
	FFileSystem.WriteAllText('C:\test.txt', 'new', TEncoding.UTF8);
	Assert.AreEqual('new', FFileSystem.GetFileContent('C:\test.txt'));
end;

procedure TMemoryFileSystemTest.TestWriteAllLines_CreatesFile;
var
	Lines: TStringList;
begin
	Lines := TStringList.Create;
	try
		Lines.Add('Line1');
		Lines.Add('Line2');
		FFileSystem.WriteAllLines('C:\test.txt', Lines, TEncoding.UTF8);
		Assert.IsTrue(FFileSystem.FileExists('C:\test.txt'));
	finally
		Lines.Free;
	end;
end;

procedure TMemoryFileSystemTest.TestOpenTextReader_ReturnsWorkingReader;
var
	Reader: TStreamReader;
begin
	FFileSystem.SetFileContent('C:\test.txt', 'Hello');
	Reader := FFileSystem.OpenTextReader('C:\test.txt', TEncoding.UTF8);
	try
		Assert.AreEqual('Hello', Reader.ReadToEnd);
	finally
		Reader.Free;
	end;
end;

procedure TMemoryFileSystemTest.TestGetTmpFileName_ReturnsUniquePaths;
var
	Path1, Path2: WideString;
begin
	Path1 := FFileSystem.GetTmpFileName('TST');
	Path2 := FFileSystem.GetTmpFileName('TST');
	Assert.AreNotEqual(String(Path1), String(Path2), 'Each call should return a unique path');
end;

procedure TMemoryFileSystemTest.TestGetTmpFileName_CreatesFile;
var
	TmpPath: WideString;
begin
	TmpPath := FFileSystem.GetTmpFileName;
	Assert.IsTrue(FFileSystem.FileExists(TmpPath), 'Temp file should exist after creation');
end;

procedure TMemoryFileSystemTest.TestGetTmpFileName_IncludesPrefix;
var
	TmpPath: WideString;
begin
	TmpPath := FFileSystem.GetTmpFileName('PFX');
	Assert.StartsWith('PFX', String(TmpPath), 'Path should start with prefix');
end;

procedure TMemoryFileSystemTest.TestSetFileContent_GetFileContent_RoundTrip;
begin
	FFileSystem.SetFileContent('C:\test.txt', 'Test Content');
	Assert.AreEqual('Test Content', FFileSystem.GetFileContent('C:\test.txt'));
end;

procedure TMemoryFileSystemTest.TestClear_RemovesAllFiles;
begin
	FFileSystem.SetFileContent('C:\file1.txt', 'content1');
	FFileSystem.SetFileContent('C:\file2.txt', 'content2');
	FFileSystem.Clear;
	Assert.IsFalse(FFileSystem.FileExists('C:\file1.txt'));
	Assert.IsFalse(FFileSystem.FileExists('C:\file2.txt'));
end;

procedure TMemoryFileSystemTest.TestCaseInsensitive;
begin
	FFileSystem.SetFileContent('C:\Test.txt', 'content');
	Assert.IsTrue(FFileSystem.FileExists('C:\TEST.TXT'), 'Should be case insensitive');
	Assert.IsTrue(FFileSystem.FileExists('C:\test.txt'), 'Should be case insensitive');
end;

procedure TMemoryFileSystemTest.TestSetFileTime_TracksCalls;
var
	FileTime: TFileTime;
begin
	FileTime.dwLowDateTime := 0;
	FileTime.dwHighDateTime := 0;
	Assert.AreEqual(0, FFileSystem.SetFileTimeCalls, 'Should start at zero');
	FFileSystem.SetFileTime('C:\test.txt', FileTime);
	Assert.AreEqual(1, FFileSystem.SetFileTimeCalls, 'Should track call');
	FFileSystem.SetFileTime('C:\test2.txt', FileTime);
	Assert.AreEqual(2, FFileSystem.SetFileTimeCalls, 'Should track second call');
end;

procedure TMemoryFileSystemTest.TestImplementsIFileSystem;
var
	Intf: IFileSystem;
begin
	Intf := TMemoryFileSystem.Create;
	Assert.IsNotNull(Intf, 'Should implement IFileSystem');
end;

procedure TMemoryFileSystemTest.TestFindFiles_MatchesPattern;
var
	Files: TStringList;
begin
	FFileSystem.SetFileContent('C:\lang\english.lng', 'content');
	FFileSystem.SetFileContent('C:\lang\readme.txt', 'content');
	Files := FFileSystem.FindFiles('C:\lang\*.lng');
	try
		Assert.AreEqual(1, Files.Count);
		Assert.AreEqual('C:\lang\english.lng', Files[0]);
	finally
		Files.Free;
	end;
end;

procedure TMemoryFileSystemTest.TestFindFiles_NoMatches_ReturnsEmpty;
var
	Files: TStringList;
begin
	FFileSystem.SetFileContent('C:\lang\readme.txt', 'content');
	Files := FFileSystem.FindFiles('C:\lang\*.lng');
	try
		Assert.AreEqual(0, Files.Count);
	finally
		Files.Free;
	end;
end;

procedure TMemoryFileSystemTest.TestFindFiles_MultipleMatches;
var
	Files: TStringList;
begin
	FFileSystem.SetFileContent('C:\lang\english.lng', 'content1');
	FFileSystem.SetFileContent('C:\lang\russian.lng', 'content2');
	FFileSystem.SetFileContent('C:\lang\readme.txt', 'content3');
	Files := FFileSystem.FindFiles('C:\lang\*.lng');
	try
		Assert.AreEqual(2, Files.Count);
	finally
		Files.Free;
	end;
end;

procedure TMemoryFileSystemTest.TestGetFileModTime_NoValue_ReturnsZero;
begin
	Assert.AreEqual(Int64(0), FFileSystem.GetFileModTime('C:\nonexistent.txt'));
end;

procedure TMemoryFileSystemTest.TestGetFileModTime_WithValue_ReturnsStored;
begin
	FFileSystem.SetFileModTimeValue('C:\test.txt', 1704067200);
	Assert.AreEqual(Int64(1704067200), FFileSystem.GetFileModTime('C:\test.txt'));
end;

procedure TMemoryFileSystemTest.TestGetFileModTime_ClearedByReset;
begin
	FFileSystem.SetFileModTimeValue('C:\test.txt', 1704067200);
	FFileSystem.Clear;
	Assert.AreEqual(Int64(0), FFileSystem.GetFileModTime('C:\test.txt'));
end;

{ TWindowsFileSystemTest }

procedure TWindowsFileSystemTest.Setup;
begin
	FFileSystem := TWindowsFileSystem.Create;
	FFileSystemRef := FFileSystem;
	FTempDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'));
	FTempFiles := TStringList.Create;
end;

procedure TWindowsFileSystemTest.TearDown;
begin
	CleanupTempFiles;
	FTempFiles.Free;
	FFileSystemRef := nil;
end;

function TWindowsFileSystemTest.CreateTempFile(const Content: String): String;
var
	FileStream: TFileStream;
	ContentBytes: TBytes;
begin
	Result := FTempDir + 'wfstest_' + IntToStr(GetTickCount) + '_' + IntToStr(FTempFiles.Count) + '.tmp';
	FTempFiles.Add(Result);

	{Write raw bytes without BOM}
	FileStream := TFileStream.Create(Result, fmCreate);
	try
		if Content <> '' then
		begin
			ContentBytes := TEncoding.UTF8.GetBytes(Content);
			FileStream.Write(ContentBytes[0], Length(ContentBytes));
		end;
	finally
		FileStream.Free;
	end;
end;

procedure TWindowsFileSystemTest.CleanupTempFiles;
var
	i: Integer;
begin
	for i := 0 to FTempFiles.Count - 1 do
		if System.SysUtils.FileExists(FTempFiles[i]) then
			System.SysUtils.DeleteFile(FTempFiles[i]);
end;

procedure TWindowsFileSystemTest.TestFileExists_NonExistent_ReturnsFalse;
begin
	Assert.IsFalse(FFileSystem.FileExists(FTempDir + 'nonexistent_file_12345.txt'));
end;

procedure TWindowsFileSystemTest.TestFileExists_Existing_ReturnsTrue;
var
	TempFile: String;
begin
	TempFile := CreateTempFile('test');
	Assert.IsTrue(FFileSystem.FileExists(TempFile));
end;

procedure TWindowsFileSystemTest.TestGetFileSize_NonExistent_ReturnsMinusOne;
begin
	Assert.AreEqual(Int64(-1), FFileSystem.GetFileSize(FTempDir + 'nonexistent_file_12345.txt'));
end;

procedure TWindowsFileSystemTest.TestGetFileSize_EmptyFile_ReturnsZero;
var
	TempFile: String;
begin
	TempFile := CreateTempFile('');
	Assert.AreEqual(Int64(0), FFileSystem.GetFileSize(TempFile));
end;

procedure TWindowsFileSystemTest.TestGetFileSize_WithContent_ReturnsCorrectSize;
var
	TempFile: String;
begin
	{UTF-8 encoding: 'Hello' = 5 bytes}
	TempFile := CreateTempFile('Hello');
	Assert.AreEqual(Int64(5), FFileSystem.GetFileSize(TempFile));
end;

procedure TWindowsFileSystemTest.TestCreateEmptyFile_CreatesFile;
var
	TempFile: String;
begin
	TempFile := FTempDir + 'wfstest_create_' + IntToStr(GetTickCount) + '.tmp';
	FTempFiles.Add(TempFile);

	FFileSystem.CreateEmptyFile(TempFile);
	Assert.IsTrue(System.SysUtils.FileExists(TempFile), 'File should be created');
	Assert.AreEqual(Int64(0), FFileSystem.GetFileSize(TempFile), 'File should be empty');
end;

procedure TWindowsFileSystemTest.TestDeleteFile_RemovesFile;
var
	TempFile: String;
begin
	TempFile := CreateTempFile('content');
	FFileSystem.DeleteFile(TempFile);
	Assert.IsFalse(System.SysUtils.FileExists(TempFile), 'File should be deleted');
end;

procedure TWindowsFileSystemTest.TestDeleteFile_NonExistent_NoError;
begin
	FFileSystem.DeleteFile(FTempDir + 'nonexistent_file_12345.txt');
	Assert.Pass('Should not raise exception');
end;

procedure TWindowsFileSystemTest.TestReadFileHeader_NonExistent_ReturnsEmpty;
var
	Header: TBytes;
begin
	Header := FFileSystem.ReadFileHeader(FTempDir + 'nonexistent_file_12345.txt', 10);
	Assert.AreEqual(Integer(0), Integer(Length(Header)));
end;

procedure TWindowsFileSystemTest.TestReadFileHeader_ShortFile_ReturnsAllContent;
var
	TempFile: String;
	Header: TBytes;
begin
	TempFile := CreateTempFile('ABC');
	Header := FFileSystem.ReadFileHeader(TempFile, 10);
	Assert.AreEqual(Integer(3), Integer(Length(Header)));
	Assert.AreEqual(Byte(Ord('A')), Header[0]);
	Assert.AreEqual(Byte(Ord('B')), Header[1]);
	Assert.AreEqual(Byte(Ord('C')), Header[2]);
end;

procedure TWindowsFileSystemTest.TestReadFileHeader_LongFile_ReturnsTruncated;
var
	TempFile: String;
	Header: TBytes;
begin
	TempFile := CreateTempFile('ABCDEFGHIJ');
	Header := FFileSystem.ReadFileHeader(TempFile, 5);
	Assert.AreEqual(Integer(5), Integer(Length(Header)));
end;

procedure TWindowsFileSystemTest.TestReadAllText_NonExistent_ReturnsEmpty;
begin
	Assert.AreEqual('', String(FFileSystem.ReadAllText(FTempDir + 'nonexistent_file_12345.txt', TEncoding.UTF8)));
end;

procedure TWindowsFileSystemTest.TestReadAllText_WithContent_ReturnsContent;
var
	TempFile: String;
begin
	TempFile := CreateTempFile('Hello World');
	Assert.AreEqual('Hello World', String(FFileSystem.ReadAllText(TempFile, TEncoding.UTF8)));
end;

procedure TWindowsFileSystemTest.TestReadAllLines_NonExistent_ReturnsEmptyList;
var
	Lines: TStringList;
begin
	Lines := FFileSystem.ReadAllLines(FTempDir + 'nonexistent_file_12345.txt', TEncoding.UTF8);
	try
		Assert.AreEqual(0, Lines.Count);
	finally
		Lines.Free;
	end;
end;

procedure TWindowsFileSystemTest.TestReadAllLines_MultipleLines_ParsesCorrectly;
var
	TempFile: String;
	Lines: TStringList;
begin
	TempFile := CreateTempFile('Line1'#13#10'Line2'#13#10'Line3');
	Lines := FFileSystem.ReadAllLines(TempFile, TEncoding.UTF8);
	try
		Assert.AreEqual(3, Lines.Count);
		Assert.AreEqual('Line1', Lines[0]);
		Assert.AreEqual('Line2', Lines[1]);
		Assert.AreEqual('Line3', Lines[2]);
	finally
		Lines.Free;
	end;
end;

procedure TWindowsFileSystemTest.TestWriteAllText_CreatesFile;
var
	TempFile: String;
begin
	TempFile := FTempDir + 'wfstest_write_' + IntToStr(GetTickCount) + '.tmp';
	FTempFiles.Add(TempFile);

	FFileSystem.WriteAllText(TempFile, 'test content', TEncoding.UTF8);
	Assert.IsTrue(System.SysUtils.FileExists(TempFile));
end;

procedure TWindowsFileSystemTest.TestWriteAllText_ReadBack_Matches;
var
	TempFile: String;
begin
	TempFile := FTempDir + 'wfstest_write_' + IntToStr(GetTickCount) + '.tmp';
	FTempFiles.Add(TempFile);

	FFileSystem.WriteAllText(TempFile, 'Hello World', TEncoding.UTF8);
	Assert.AreEqual('Hello World', String(FFileSystem.ReadAllText(TempFile, TEncoding.UTF8)));
end;

procedure TWindowsFileSystemTest.TestWriteAllLines_CreatesFile;
var
	TempFile: String;
	Lines: TStringList;
begin
	TempFile := FTempDir + 'wfstest_lines_' + IntToStr(GetTickCount) + '.tmp';
	FTempFiles.Add(TempFile);

	Lines := TStringList.Create;
	try
		Lines.Add('Line1');
		Lines.Add('Line2');
		FFileSystem.WriteAllLines(TempFile, Lines, TEncoding.UTF8);
		Assert.IsTrue(System.SysUtils.FileExists(TempFile));
	finally
		Lines.Free;
	end;
end;

procedure TWindowsFileSystemTest.TestOpenTextReader_ReturnsWorkingReader;
var
	TempFile: String;
	Reader: TStreamReader;
begin
	TempFile := CreateTempFile('Hello World');
	Reader := FFileSystem.OpenTextReader(TempFile, TEncoding.UTF8);
	try
		Assert.AreEqual('Hello World', Reader.ReadToEnd);
	finally
		Reader.Free;
	end;
end;

procedure TWindowsFileSystemTest.TestGetTmpFileName_ReturnsNonEmpty;
var
	TmpFile: string;
begin
	TmpFile := FFileSystem.GetTmpFileName;
	FTempFiles.Add(TmpFile);
	Assert.IsNotEmpty(TmpFile);
end;

procedure TWindowsFileSystemTest.TestGetTmpFileName_CreatesFile;
var
	TmpFile: string;
begin
	TmpFile := FFileSystem.GetTmpFileName;
	FTempFiles.Add(TmpFile);
	Assert.IsTrue(System.SysUtils.FileExists(TmpFile), 'GetTmpFileName should create a zero-byte file');
end;

procedure TWindowsFileSystemTest.TestGetTmpFileName_WithPrefix;
var
	TmpFile: string;
begin
	TmpFile := FFileSystem.GetTmpFileName('TST');
	FTempFiles.Add(TmpFile);
	{Windows API uses first 3 chars of prefix}
	Assert.StartsWith('TST', ExtractFileName(TmpFile));
end;

procedure TWindowsFileSystemTest.TestGetTmpFileName_ReturnsUniquePaths;
var
	TmpFile1, TmpFile2: string;
begin
	TmpFile1 := FFileSystem.GetTmpFileName;
	TmpFile2 := FFileSystem.GetTmpFileName;
	FTempFiles.Add(TmpFile1);
	FTempFiles.Add(TmpFile2);
	Assert.AreNotEqual(TmpFile1, TmpFile2, 'Each call should return a unique path');
end;

procedure TWindowsFileSystemTest.TestSetFileTime_SetsTimestamp;
var
	TempFile: String;
	NewTime, RetrievedTime: TFileTime;
	SystemTime: TSystemTime;
	Handle: THandle;
begin
	TempFile := CreateTempFile('test');

	{Set a specific timestamp}
	DateTimeToSystemTime(Now, SystemTime);
	SystemTimeToFileTime(SystemTime, NewTime);

	FFileSystem.SetFileTime(TempFile, NewTime);

	{Verify timestamp was set}
	Handle := FileOpen(TempFile, fmOpenRead);
	Assert.IsTrue(Handle <> THandle(-1), 'Failed to open test file');
	try
		Assert.IsTrue(Windows.GetFileTime(Handle, @RetrievedTime, nil, nil), 'Failed to get file time');
		Assert.IsTrue(CompareFileTime(NewTime, RetrievedTime) = 0, 'File creation time should match');
	finally
		FileClose(Handle);
	end;
end;

procedure TWindowsFileSystemTest.TestSetFileTime_NonExistentFile_NoError;
var
	NewTime: TFileTime;
	SystemTime: TSystemTime;
begin
	DateTimeToSystemTime(Now, SystemTime);
	SystemTimeToFileTime(SystemTime, NewTime);

	{Should not raise exception for non-existent file}
	FFileSystem.SetFileTime(FTempDir + 'nonexistent_file_12345.txt', NewTime);
	Assert.Pass('SetFileTime should handle non-existent files gracefully');
end;

procedure TWindowsFileSystemTest.TestImplementsIFileSystem;
var
	Intf: IFileSystem;
begin
	Intf := TWindowsFileSystem.Create;
	Assert.IsNotNull(Intf, 'Should implement IFileSystem');
end;

procedure TWindowsFileSystemTest.TestGetFileModTime_NonExistent_ReturnsZero;
begin
	Assert.AreEqual(Int64(0), FFileSystem.GetFileModTime(FTempDir + 'nonexistent_file_12345.txt'));
end;

procedure TWindowsFileSystemTest.TestGetFileModTime_ExistingFile_ReturnsNonZero;
var
	TempFile: String;
begin
	TempFile := CreateTempFile('test content');
	Assert.IsTrue(FFileSystem.GetFileModTime(TempFile) > 0, 'Existing file should have non-zero modification time');
end;

{ TOwningStreamReaderTest }

procedure TOwningStreamReaderTest.TestCreate_ReadsContent;
var
	Stream: TStringStream;
	Reader: TOwningStreamReader;
begin
	Stream := TStringStream.Create('Test Content', TEncoding.UTF8);
	Reader := TOwningStreamReader.Create(Stream, TEncoding.UTF8);
	try
		Assert.AreEqual('Test Content', Reader.ReadToEnd);
	finally
		Reader.Free;
	end;
end;

procedure TOwningStreamReaderTest.TestDestroy_FreesStream;
var
	Stream: TStringStream;
	Reader: TOwningStreamReader;
begin
	Stream := TStringStream.Create('Test', TEncoding.UTF8);
	Reader := TOwningStreamReader.Create(Stream, TEncoding.UTF8);
	Reader.Free;
	{If stream wasn't freed, accessing it would work - but we can't test this directly.
	 The test passes if no memory leak is detected by FastMM.}
	Assert.Pass('Stream should be freed with reader (check for memory leaks)');
end;

initialization

TDUnitX.RegisterTestFixture(TNullFileSystemTest);
TDUnitX.RegisterTestFixture(TMemoryFileSystemTest);
TDUnitX.RegisterTestFixture(TWindowsFileSystemTest);
TDUnitX.RegisterTestFixture(TOwningStreamReaderTest);

end.
