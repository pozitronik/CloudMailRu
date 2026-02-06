unit TimestampMetadataTest;

{Unit tests for TTimestampMetadata - per-directory timestamp file I/O.
	Tests read/write round-trip, entry CRUD, unicode filenames, edge cases.}

interface

uses
	DUnitX.TestFramework,
	System.SysUtils,
	System.Classes,
	Windows,
	TimestampMetadata,
	TimestampEntry,
	FileSystem;

type
	{Filesystem mock that throws on WriteAllLines/OpenTextReader for testing exception handlers}
	TThrowingFileSystem = class(TInterfacedObject, IFileSystem)
	private
		FInner: TMemoryFileSystem;
		FThrowOnWrite: Boolean;
		FThrowOnRead: Boolean;
	public
		constructor Create(Inner: TMemoryFileSystem; ThrowOnWrite: Boolean = False; ThrowOnRead: Boolean = False);
		function FileExists(const Path: WideString): Boolean;
		function GetFileSize(const Path: WideString): Int64;
		procedure CreateEmptyFile(const Path: WideString);
		procedure DeleteFile(const Path: WideString);
		function ReadFileHeader(const Path: WideString; ByteCount: Integer): TBytes;
		function ReadAllText(const Path: WideString; Encoding: TEncoding): WideString;
		function ReadAllLines(const Path: WideString; Encoding: TEncoding): TStringList;
		procedure WriteAllText(const Path: WideString; const Content: WideString; Encoding: TEncoding);
		procedure WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);
		function OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;
		function GetTmpFileName(const Prefix: WideString = ''): WideString;
		procedure SetFileTime(const Path: WideString; const FileTime: TFileTime);
		function FindFiles(const Pattern: WideString): TStringList;
		function GetFileModTime(const Path: WideString): Int64;
	end;

	[TestFixture]
	TTimestampMetadataTest = class
	private
		FFileSystem: TMemoryFileSystem;
		FFileSystemIntf: IFileSystem;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Read/Write round-trip}
		[Test]
		procedure TestReadWrite_RoundTrip_PreservesEntries;
		[Test]
		procedure TestRead_EmptyFile_ReturnsZeroEntries;
		[Test]
		procedure TestRead_MissingFile_ReturnsZeroEntries;
		[Test]
		procedure TestRead_MalformedLine_SkipsIt;
		[Test]
		procedure TestWrite_NoEntries_DeletesFile;

		{GetEntry / SetEntry}
		[Test]
		procedure TestGetEntry_ExistingKey_ReturnsEntry;
		[Test]
		procedure TestGetEntry_MissingKey_ReturnsEmpty;
		[Test]
		procedure TestSetEntry_EmptyEntry_DeletesKey;
		[Test]
		procedure TestSetEntry_Overwrite_UpdatesValue;

		{DeleteEntry}
		[Test]
		procedure TestDeleteEntry_ExistingKey_ReturnsTrue;
		[Test]
		procedure TestDeleteEntry_MissingKey_ReturnsFalse;

		{RenameEntry}
		[Test]
		procedure TestRenameEntry_ExistingKey_TransfersValue;
		[Test]
		procedure TestRenameEntry_MissingKey_ReturnsFalse;

		{HasEntry}
		[Test]
		procedure TestHasEntry_ExistingKey_ReturnsTrue;
		[Test]
		procedure TestHasEntry_MissingKey_ReturnsFalse;

		{Unicode filenames}
		[Test]
		procedure TestUnicodeFilename_RoundTrip;

		{Multiple entries}
		[Test]
		procedure TestMultipleEntries_ReadWrite;

		{Write with explicit TargetFileName}
		[Test]
		procedure TestWrite_WithTargetFileName_WritesToTarget;

		{Exception handling - Write exception path}
		[Test]
		procedure TestWrite_FileSystemThrows_ReturnsMinusOne;
	end;

implementation

procedure TTimestampMetadataTest.Setup;
begin
	FFileSystem := TMemoryFileSystem.Create;
	FFileSystemIntf := FFileSystem;
end;

procedure TTimestampMetadataTest.TearDown;
begin
	FFileSystemIntf := nil;
	FFileSystem := nil;
end;

procedure TTimestampMetadataTest.TestReadWrite_RoundTrip_PreservesEntries;
var
	Writer, Reader: TTimestampMetadata;
	Entry: TTimestampEntry;
begin
	Writer := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Entry.LocalMTime := 1704067200;
		Entry.CloudMTime := 1704153600;
		Writer.SetEntry('photo.jpg', Entry);
		Writer.Write;
	finally
		Writer.Free;
	end;

	Reader := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Reader.Read;
		Entry := Reader.GetEntry('photo.jpg');
		Assert.AreEqual(Int64(1704067200), Entry.LocalMTime);
		Assert.AreEqual(Int64(1704153600), Entry.CloudMTime);
	finally
		Reader.Free;
	end;
end;

procedure TTimestampMetadataTest.TestRead_EmptyFile_ReturnsZeroEntries;
var
	Metadata: TTimestampMetadata;
begin
	FFileSystem.SetFileContent('test.ts', '');
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Assert.AreEqual(0, Metadata.Read);
		Assert.IsFalse(Metadata.HasEntry('anything'));
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestRead_MissingFile_ReturnsZeroEntries;
var
	Metadata: TTimestampMetadata;
begin
	{TMemoryFileSystem.Create creates empty file, so no file = empty content}
	Metadata := TTimestampMetadata.Create('nonexistent.ts', FFileSystemIntf);
	try
		Assert.AreEqual(0, Metadata.Read);
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestRead_MalformedLine_SkipsIt;
var
	Metadata: TTimestampMetadata;
begin
	{Line with only 2 fields should be skipped}
	FFileSystem.SetFileContent('test.ts', 'badline' + #9 + '123' + sLineBreak +
		'good.txt' + #9 + '100' + #9 + '200' + sLineBreak);
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Metadata.Read;
		Assert.IsFalse(Metadata.HasEntry('badline'));
		Assert.IsTrue(Metadata.HasEntry('good.txt'));
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestWrite_NoEntries_DeletesFile;
var
	Metadata: TTimestampMetadata;
begin
	FFileSystem.SetFileContent('test.ts', 'some content');
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Metadata.Write;
		Assert.IsFalse(FFileSystem.FileExists('test.ts'), 'File should be deleted when no entries');
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestGetEntry_ExistingKey_ReturnsEntry;
var
	Metadata: TTimestampMetadata;
	Entry, Retrieved: TTimestampEntry;
begin
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Entry.LocalMTime := 999;
		Entry.CloudMTime := 888;
		Metadata.SetEntry('file.txt', Entry);

		Retrieved := Metadata.GetEntry('file.txt');
		Assert.AreEqual(Int64(999), Retrieved.LocalMTime);
		Assert.AreEqual(Int64(888), Retrieved.CloudMTime);
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestGetEntry_MissingKey_ReturnsEmpty;
var
	Metadata: TTimestampMetadata;
	Entry: TTimestampEntry;
begin
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Entry := Metadata.GetEntry('nonexistent.txt');
		Assert.IsTrue(Entry.IsEmpty);
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestSetEntry_EmptyEntry_DeletesKey;
var
	Metadata: TTimestampMetadata;
	Entry: TTimestampEntry;
begin
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Entry.LocalMTime := 123;
		Entry.CloudMTime := 456;
		Metadata.SetEntry('file.txt', Entry);
		Assert.IsTrue(Metadata.HasEntry('file.txt'));

		Metadata.SetEntry('file.txt', TTimestampEntry.Empty);
		Assert.IsFalse(Metadata.HasEntry('file.txt'));
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestSetEntry_Overwrite_UpdatesValue;
var
	Metadata: TTimestampMetadata;
	Entry: TTimestampEntry;
begin
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Entry.LocalMTime := 100;
		Entry.CloudMTime := 200;
		Metadata.SetEntry('file.txt', Entry);

		Entry.LocalMTime := 300;
		Entry.CloudMTime := 400;
		Metadata.SetEntry('file.txt', Entry);

		Entry := Metadata.GetEntry('file.txt');
		Assert.AreEqual(Int64(300), Entry.LocalMTime);
		Assert.AreEqual(Int64(400), Entry.CloudMTime);
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestDeleteEntry_ExistingKey_ReturnsTrue;
var
	Metadata: TTimestampMetadata;
	Entry: TTimestampEntry;
begin
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Entry.LocalMTime := 100;
		Entry.CloudMTime := 0;
		Metadata.SetEntry('file.txt', Entry);

		Assert.IsTrue(Metadata.DeleteEntry('file.txt'));
		Assert.IsFalse(Metadata.HasEntry('file.txt'));
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestDeleteEntry_MissingKey_ReturnsFalse;
var
	Metadata: TTimestampMetadata;
begin
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Assert.IsFalse(Metadata.DeleteEntry('nonexistent.txt'));
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestRenameEntry_ExistingKey_TransfersValue;
var
	Metadata: TTimestampMetadata;
	Entry: TTimestampEntry;
begin
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Entry.LocalMTime := 555;
		Entry.CloudMTime := 666;
		Metadata.SetEntry('old.txt', Entry);

		Assert.IsTrue(Metadata.RenameEntry('old.txt', 'new.txt'));
		Assert.IsFalse(Metadata.HasEntry('old.txt'));
		Entry := Metadata.GetEntry('new.txt');
		Assert.AreEqual(Int64(555), Entry.LocalMTime);
		Assert.AreEqual(Int64(666), Entry.CloudMTime);
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestRenameEntry_MissingKey_ReturnsFalse;
var
	Metadata: TTimestampMetadata;
begin
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Assert.IsFalse(Metadata.RenameEntry('nonexistent.txt', 'new.txt'));
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestHasEntry_ExistingKey_ReturnsTrue;
var
	Metadata: TTimestampMetadata;
	Entry: TTimestampEntry;
begin
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Entry.LocalMTime := 1;
		Entry.CloudMTime := 0;
		Metadata.SetEntry('file.txt', Entry);
		Assert.IsTrue(Metadata.HasEntry('file.txt'));
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestHasEntry_MissingKey_ReturnsFalse;
var
	Metadata: TTimestampMetadata;
begin
	Metadata := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Assert.IsFalse(Metadata.HasEntry('nonexistent.txt'));
	finally
		Metadata.Free;
	end;
end;

procedure TTimestampMetadataTest.TestUnicodeFilename_RoundTrip;
var
	Writer, Reader: TTimestampMetadata;
	Entry: TTimestampEntry;
	UnicodeName: WideString;
begin
	UnicodeName := #$0424#$043E#$0442#$043E + '.jpg'; {Фото.jpg}

	Writer := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Entry.LocalMTime := 1704067200;
		Entry.CloudMTime := 0;
		Writer.SetEntry(UnicodeName, Entry);
		Writer.Write;
	finally
		Writer.Free;
	end;

	Reader := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Reader.Read;
		Assert.IsTrue(Reader.HasEntry(UnicodeName));
		Entry := Reader.GetEntry(UnicodeName);
		Assert.AreEqual(Int64(1704067200), Entry.LocalMTime);
	finally
		Reader.Free;
	end;
end;

procedure TTimestampMetadataTest.TestMultipleEntries_ReadWrite;
var
	Writer, Reader: TTimestampMetadata;
	Entry1, Entry2, Read1, Read2: TTimestampEntry;
begin
	Writer := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Entry1.LocalMTime := 100;
		Entry1.CloudMTime := 200;
		Writer.SetEntry('file1.txt', Entry1);

		Entry2.LocalMTime := 300;
		Entry2.CloudMTime := 400;
		Writer.SetEntry('file2.txt', Entry2);
		Writer.Write;
	finally
		Writer.Free;
	end;

	Reader := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Reader.Read;
		Read1 := Reader.GetEntry('file1.txt');
		Read2 := Reader.GetEntry('file2.txt');

		Assert.AreEqual(Int64(100), Read1.LocalMTime);
		Assert.AreEqual(Int64(200), Read1.CloudMTime);
		Assert.AreEqual(Int64(300), Read2.LocalMTime);
		Assert.AreEqual(Int64(400), Read2.CloudMTime);
	finally
		Reader.Free;
	end;
end;

procedure TTimestampMetadataTest.TestWrite_WithTargetFileName_WritesToTarget;
var
	Writer, Reader: TTimestampMetadata;
	Entry: TTimestampEntry;
begin
	{Write with explicit TargetFileName exercises OutputPath := TargetFileName branch}
	Writer := TTimestampMetadata.Create('test.ts', FFileSystemIntf);
	try
		Entry.LocalMTime := 777;
		Entry.CloudMTime := 888;
		Writer.SetEntry('photo.jpg', Entry);
		Assert.AreEqual(0, Writer.Write('other.ts'));
	finally
		Writer.Free;
	end;

	{Verify data was written to the target file, not the default}
	Reader := TTimestampMetadata.Create('other.ts', FFileSystemIntf);
	try
		Reader.Read;
		Assert.IsTrue(Reader.HasEntry('photo.jpg'));
		Entry := Reader.GetEntry('photo.jpg');
		Assert.AreEqual(Int64(777), Entry.LocalMTime);
		Assert.AreEqual(Int64(888), Entry.CloudMTime);
	finally
		Reader.Free;
	end;
end;

procedure TTimestampMetadataTest.TestWrite_FileSystemThrows_ReturnsMinusOne;
var
	InnerFS: TMemoryFileSystem;
	ThrowingFS: IFileSystem;
	Metadata: TTimestampMetadata;
	Entry: TTimestampEntry;
begin
	{WriteAllLines throwing triggers except handler returning -1}
	InnerFS := TMemoryFileSystem.Create;
	ThrowingFS := TThrowingFileSystem.Create(InnerFS, True, False);

	Metadata := TTimestampMetadata.Create('test.ts', ThrowingFS);
	try
		Entry.LocalMTime := 100;
		Entry.CloudMTime := 200;
		Metadata.SetEntry('file.txt', Entry);
		Assert.AreEqual(-1, Metadata.Write, 'Write should return -1 when filesystem throws');
	finally
		Metadata.Free;
	end;
end;

{ TThrowingFileSystem }

constructor TThrowingFileSystem.Create(Inner: TMemoryFileSystem; ThrowOnWrite: Boolean; ThrowOnRead: Boolean);
begin
	inherited Create;
	FInner := Inner;
	FThrowOnWrite := ThrowOnWrite;
	FThrowOnRead := ThrowOnRead;
end;

function TThrowingFileSystem.FileExists(const Path: WideString): Boolean;
begin
	Result := FInner.FileExists(Path);
end;

function TThrowingFileSystem.GetFileSize(const Path: WideString): Int64;
begin
	Result := FInner.GetFileSize(Path);
end;

procedure TThrowingFileSystem.CreateEmptyFile(const Path: WideString);
begin
	FInner.CreateEmptyFile(Path);
end;

procedure TThrowingFileSystem.DeleteFile(const Path: WideString);
begin
	FInner.DeleteFile(Path);
end;

function TThrowingFileSystem.ReadFileHeader(const Path: WideString; ByteCount: Integer): TBytes;
begin
	Result := FInner.ReadFileHeader(Path, ByteCount);
end;

function TThrowingFileSystem.ReadAllText(const Path: WideString; Encoding: TEncoding): WideString;
begin
	Result := FInner.ReadAllText(Path, Encoding);
end;

function TThrowingFileSystem.ReadAllLines(const Path: WideString; Encoding: TEncoding): TStringList;
begin
	Result := FInner.ReadAllLines(Path, Encoding);
end;

procedure TThrowingFileSystem.WriteAllText(const Path: WideString; const Content: WideString; Encoding: TEncoding);
begin
	FInner.WriteAllText(Path, Content, Encoding);
end;

procedure TThrowingFileSystem.WriteAllLines(const Path: WideString; Lines: TStrings; Encoding: TEncoding);
begin
	if FThrowOnWrite then
		raise EWriteError.Create('Simulated write error');
	FInner.WriteAllLines(Path, Lines, Encoding);
end;

function TThrowingFileSystem.OpenTextReader(const Path: WideString; Encoding: TEncoding): TStreamReader;
begin
	if FThrowOnRead then
		raise EReadError.Create('Simulated read error');
	Result := FInner.OpenTextReader(Path, Encoding);
end;

function TThrowingFileSystem.GetTmpFileName(const Prefix: WideString): WideString;
begin
	Result := FInner.GetTmpFileName(Prefix);
end;

procedure TThrowingFileSystem.SetFileTime(const Path: WideString; const FileTime: TFileTime);
begin
	FInner.SetFileTime(Path, FileTime);
end;

function TThrowingFileSystem.FindFiles(const Pattern: WideString): TStringList;
begin
	Result := FInner.FindFiles(Pattern);
end;

function TThrowingFileSystem.GetFileModTime(const Path: WideString): Int64;
begin
	Result := FInner.GetFileModTime(Path);
end;

initialization
	TDUnitX.RegisterTestFixture(TTimestampMetadataTest);

end.
