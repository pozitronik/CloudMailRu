unit TimestampMetadataTest;

{Unit tests for TTimestampMetadata - per-directory timestamp file I/O.
	Tests read/write round-trip, entry CRUD, unicode filenames, edge cases.}

interface

uses
	DUnitX.TestFramework,
	TimestampMetadata,
	TimestampEntry,
	FileSystem;

type
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
	end;

implementation

uses
	SysUtils;

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

initialization
	TDUnitX.RegisterTestFixture(TTimestampMetadataTest);

end.
