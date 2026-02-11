unit DescriptionStoreAdapterTest;

{Tests for TDescriptionStoreAdapter - verifies IRemoteMetadataStore implementation
	over TDescription, particularly the string serialization round-trip.}

interface

uses
	DUnitX.TestFramework,
	RemoteMetadataStore,
	Description,
	FileSystem;

type
	[TestFixture]
	TDescriptionStoreAdapterTest = class
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
		procedure TestDeleteEntry_NonExistentKey_ReturnsTrue;
		[Test]
		procedure TestRenameEntry_ExistingKey_ReturnsTrue;
		[Test]
		procedure TestRenameEntry_NonExistentKey_ReturnsFalse;
		[Test]
		procedure TestGetEntryAsString_ExistingKey_ReturnsRawValue;
		[Test]
		procedure TestGetEntryAsString_NonExistentKey_ReturnsEmpty;
		[Test]
		procedure TestSetEntryFromString_CreatesEntry;
		[Test]
		procedure TestSetEntryFromString_EmptyValue_DeletesEntry;
		[Test]
		procedure TestGetFileName_ReturnsBackingFilePath;
		[Test]
		procedure TestRoundTrip_GetSetEntryAsString_PreservesValue;
	end;

implementation

uses
	SysUtils,
	System.Classes,
	DescriptionStoreAdapter;

procedure TDescriptionStoreAdapterTest.Setup;
begin
	FFileSystem := TWindowsFileSystem.Create;
	FTempFile := FFileSystem.GetTmpFileName('ion');
end;

procedure TDescriptionStoreAdapterTest.TearDown;
begin
	FStore := nil; {Release adapter, which frees TDescription}
	if FFileSystem.FileExists(FTempFile) then
		FFileSystem.DeleteFile(FTempFile);
	FFileSystem := nil;
end;

procedure TDescriptionStoreAdapterTest.CreateStoreWithContent(const Content: string);
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
	FStore := TDescriptionStoreAdapter.Create(
		TDescription.Create(FTempFile, FFileSystem, ENCODING_UTF8));
end;

procedure TDescriptionStoreAdapterTest.TestRead_LoadsEntries;
begin
	CreateStoreWithContent('testfile.txt A test description');
	FStore.Read;
	Assert.AreEqual('A test description', string(FStore.GetEntryAsString('testfile.txt')));
end;

procedure TDescriptionStoreAdapterTest.TestWrite_PersistsEntries;
var
	VerifyStore: IRemoteMetadataStore;
begin
	CreateStoreWithContent('');
	FStore.SetEntryFromString('newfile.txt', 'New description');
	FStore.Write;

	VerifyStore := TDescriptionStoreAdapter.Create(
		TDescription.Create(FTempFile, FFileSystem, ENCODING_UTF8));
	VerifyStore.Read;
	Assert.AreEqual('New description', string(VerifyStore.GetEntryAsString('newfile.txt')));
end;

procedure TDescriptionStoreAdapterTest.TestDeleteEntry_RemovesExistingKey;
begin
	CreateStoreWithContent('file1.txt Description one');
	FStore.Read;
	FStore.DeleteEntry('file1.txt');
	Assert.AreEqual('', string(FStore.GetEntryAsString('file1.txt')));
end;

procedure TDescriptionStoreAdapterTest.TestDeleteEntry_NonExistentKey_ReturnsTrue;
begin
	CreateStoreWithContent('');
	Assert.IsTrue(FStore.DeleteEntry('nonexistent.txt'));
end;

procedure TDescriptionStoreAdapterTest.TestRenameEntry_ExistingKey_ReturnsTrue;
begin
	CreateStoreWithContent('old.txt Some value');
	FStore.Read;
	Assert.IsTrue(FStore.RenameEntry('old.txt', 'new.txt'));
	Assert.AreEqual('Some value', string(FStore.GetEntryAsString('new.txt')));
	Assert.AreEqual('', string(FStore.GetEntryAsString('old.txt')));
end;

procedure TDescriptionStoreAdapterTest.TestRenameEntry_NonExistentKey_ReturnsFalse;
begin
	CreateStoreWithContent('');
	Assert.IsFalse(FStore.RenameEntry('nonexistent.txt', 'new.txt'));
end;

procedure TDescriptionStoreAdapterTest.TestGetEntryAsString_ExistingKey_ReturnsRawValue;
begin
	CreateStoreWithContent('file.txt Raw value with spaces');
	FStore.Read;
	Assert.AreEqual('Raw value with spaces', string(FStore.GetEntryAsString('file.txt')));
end;

procedure TDescriptionStoreAdapterTest.TestGetEntryAsString_NonExistentKey_ReturnsEmpty;
begin
	CreateStoreWithContent('');
	Assert.AreEqual('', string(FStore.GetEntryAsString('missing.txt')));
end;

procedure TDescriptionStoreAdapterTest.TestSetEntryFromString_CreatesEntry;
begin
	CreateStoreWithContent('');
	FStore.SetEntryFromString('file.txt', 'A value');
	Assert.AreEqual('A value', string(FStore.GetEntryAsString('file.txt')));
end;

procedure TDescriptionStoreAdapterTest.TestSetEntryFromString_EmptyValue_DeletesEntry;
begin
	CreateStoreWithContent('file.txt Existing value');
	FStore.Read;
	FStore.SetEntryFromString('file.txt', '');
	Assert.AreEqual('', string(FStore.GetEntryAsString('file.txt')));
end;

procedure TDescriptionStoreAdapterTest.TestGetFileName_ReturnsBackingFilePath;
begin
	CreateStoreWithContent('');
	Assert.AreEqual(string(FTempFile), string(FStore.GetFileName));
end;

procedure TDescriptionStoreAdapterTest.TestRoundTrip_GetSetEntryAsString_PreservesValue;
var
	OriginalValue, RestoredValue: WideString;
begin
	CreateStoreWithContent('source.txt Complex description with\nmultiline content');
	FStore.Read;

	OriginalValue := FStore.GetEntryAsString('source.txt');
	FStore.SetEntryFromString('target.txt', OriginalValue);
	RestoredValue := FStore.GetEntryAsString('target.txt');

	Assert.AreEqual(string(OriginalValue), string(RestoredValue));
end;

end.
