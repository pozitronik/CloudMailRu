unit TimestampStoreAdapterTest;

{Tests for TTimestampStoreAdapter - verifies IRemoteMetadataStore implementation
	over TTimestampMetadata, particularly the tab-separated string serialization round-trip.}

interface

uses
	DUnitX.TestFramework,
	RemoteMetadataStore,
	TimestampMetadata,
	TimestampEntry,
	FileSystem;

type
	[TestFixture]
	TTimestampStoreAdapterTest = class
	private
		FStore: IRemoteMetadataStore;
		FFileSystem: IFileSystem;
		FTempFile: WideString;

		procedure CreateStoreWithContent(const Content: string);
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestRead_LoadsEntries;
		[Test]
		procedure TestWrite_PersistsEntries;
		[Test]
		procedure TestDeleteEntry_RemovesExistingKey;
		[Test]
		procedure TestDeleteEntry_NonExistentKey_ReturnsFalse;
		[Test]
		procedure TestRenameEntry_ExistingKey_ReturnsTrue;
		[Test]
		procedure TestRenameEntry_NonExistentKey_ReturnsFalse;
		[Test]
		procedure TestGetEntryAsString_ExistingKey_ReturnsTabSeparated;
		[Test]
		procedure TestGetEntryAsString_NonExistentKey_ReturnsEmpty;
		[Test]
		procedure TestSetEntryFromString_ParsesTabSeparated;
		[Test]
		procedure TestSetEntryFromString_EmptyValue_DeletesEntry;
		[Test]
		procedure TestGetFileName_ReturnsBackingFilePath;
		[Test]
		procedure TestRoundTrip_GetSetEntryAsString_PreservesTimestamps;
		[Test]
		procedure TestRoundTrip_ViaWriteRead_PreservesValues;
	end;

implementation

uses
	SysUtils,
	System.Classes,
	TimestampStoreAdapter;

procedure TTimestampStoreAdapterTest.Setup;
begin
	FFileSystem := TWindowsFileSystem.Create;
	FTempFile := FFileSystem.GetTmpFileName('tsm');
end;

procedure TTimestampStoreAdapterTest.TearDown;
begin
	FStore := nil; {Release adapter, which frees TTimestampMetadata}
	if FFileSystem.FileExists(FTempFile) then
		FFileSystem.DeleteFile(FTempFile);
	FFileSystem := nil;
end;

procedure TTimestampStoreAdapterTest.CreateStoreWithContent(const Content: string);
var
	Lines: TStringList;
begin
	if Content <> '' then
	begin
		Lines := TStringList.Create;
		try
			Lines.Text := Content;
			FFileSystem.WriteAllLines(FTempFile, Lines, TEncoding.UTF8);
		finally
			Lines.Free;
		end;
	end;
	FStore := TTimestampStoreAdapter.Create(
		TTimestampMetadata.Create(FTempFile, FFileSystem));
end;

procedure TTimestampStoreAdapterTest.TestRead_LoadsEntries;
begin
	CreateStoreWithContent('testfile.txt'#9'1700000000'#9'1700000100');
	FStore.Read;
	Assert.AreNotEqual('', string(FStore.GetEntryAsString('testfile.txt')));
end;

procedure TTimestampStoreAdapterTest.TestWrite_PersistsEntries;
var
	VerifyStore: IRemoteMetadataStore;
begin
	CreateStoreWithContent('');
	FStore.SetEntryFromString('newfile.txt', '1700000000'#9'1700000100');
	FStore.Write;

	VerifyStore := TTimestampStoreAdapter.Create(
		TTimestampMetadata.Create(FTempFile, FFileSystem));
	VerifyStore.Read;
	Assert.AreEqual('1700000000'#9'1700000100', string(VerifyStore.GetEntryAsString('newfile.txt')));
end;

procedure TTimestampStoreAdapterTest.TestDeleteEntry_RemovesExistingKey;
begin
	CreateStoreWithContent('file1.txt'#9'100'#9'200');
	FStore.Read;
	Assert.IsTrue(FStore.DeleteEntry('file1.txt'));
	Assert.AreEqual('', string(FStore.GetEntryAsString('file1.txt')));
end;

procedure TTimestampStoreAdapterTest.TestDeleteEntry_NonExistentKey_ReturnsFalse;
begin
	CreateStoreWithContent('');
	Assert.IsFalse(FStore.DeleteEntry('nonexistent.txt'));
end;

procedure TTimestampStoreAdapterTest.TestRenameEntry_ExistingKey_ReturnsTrue;
begin
	CreateStoreWithContent('old.txt'#9'100'#9'200');
	FStore.Read;
	Assert.IsTrue(FStore.RenameEntry('old.txt', 'new.txt'));
	Assert.AreEqual('100'#9'200', string(FStore.GetEntryAsString('new.txt')));
	Assert.AreEqual('', string(FStore.GetEntryAsString('old.txt')));
end;

procedure TTimestampStoreAdapterTest.TestRenameEntry_NonExistentKey_ReturnsFalse;
begin
	CreateStoreWithContent('');
	Assert.IsFalse(FStore.RenameEntry('nonexistent.txt', 'new.txt'));
end;

procedure TTimestampStoreAdapterTest.TestGetEntryAsString_ExistingKey_ReturnsTabSeparated;
begin
	CreateStoreWithContent('file.txt'#9'1700490201'#9'1700490300');
	FStore.Read;
	Assert.AreEqual('1700490201'#9'1700490300', string(FStore.GetEntryAsString('file.txt')));
end;

procedure TTimestampStoreAdapterTest.TestGetEntryAsString_NonExistentKey_ReturnsEmpty;
begin
	CreateStoreWithContent('');
	Assert.AreEqual('', string(FStore.GetEntryAsString('missing.txt')));
end;

procedure TTimestampStoreAdapterTest.TestSetEntryFromString_ParsesTabSeparated;
begin
	CreateStoreWithContent('');
	FStore.SetEntryFromString('file.txt', '1700490201'#9'1700490300');

	{Verify via GetEntryAsString round-trip}
	Assert.AreEqual('1700490201'#9'1700490300', string(FStore.GetEntryAsString('file.txt')));
end;

procedure TTimestampStoreAdapterTest.TestSetEntryFromString_EmptyValue_DeletesEntry;
begin
	CreateStoreWithContent('file.txt'#9'100'#9'200');
	FStore.Read;
	FStore.SetEntryFromString('file.txt', '');
	Assert.AreEqual('', string(FStore.GetEntryAsString('file.txt')));
end;

procedure TTimestampStoreAdapterTest.TestGetFileName_ReturnsBackingFilePath;
begin
	CreateStoreWithContent('');
	Assert.AreEqual(string(FTempFile), string(FStore.GetFileName));
end;

procedure TTimestampStoreAdapterTest.TestRoundTrip_GetSetEntryAsString_PreservesTimestamps;
var
	OriginalValue, RestoredValue: WideString;
begin
	CreateStoreWithContent('source.txt'#9'1700490201'#9'1700490300');
	FStore.Read;

	OriginalValue := FStore.GetEntryAsString('source.txt');
	FStore.SetEntryFromString('target.txt', OriginalValue);
	RestoredValue := FStore.GetEntryAsString('target.txt');

	Assert.AreEqual(string(OriginalValue), string(RestoredValue));
end;

procedure TTimestampStoreAdapterTest.TestRoundTrip_ViaWriteRead_PreservesValues;
var
	VerifyStore: IRemoteMetadataStore;
begin
	CreateStoreWithContent('');
	FStore.SetEntryFromString('file1.txt', '100'#9'200');
	FStore.SetEntryFromString('file2.txt', '300'#9'400');
	FStore.Write;

	FStore := nil; {Release first adapter}

	VerifyStore := TTimestampStoreAdapter.Create(
		TTimestampMetadata.Create(FTempFile, FFileSystem));
	VerifyStore.Read;
	Assert.AreEqual('100'#9'200', string(VerifyStore.GetEntryAsString('file1.txt')));
	Assert.AreEqual('300'#9'400', string(VerifyStore.GetEntryAsString('file2.txt')));
end;

end.
