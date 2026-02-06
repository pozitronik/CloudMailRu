unit TimestampSyncIntegrationTest;

{Integration tests for timestamp synchronization against live cloud.mail.ru API.
	Tests the full chain: TTimestampSyncManager -> TCloudDescriptionOperationsAdapter
	-> TCloudMailRu -> cloud API. Verifies .cloud_timestamps file operations (create,
	read, update, delete) through real cloud round-trips.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig,
	TimestampEntry;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	[Category('Integration')]
	TTimestampSyncIntegrationTest = class(TIntegrationTestBase)
	private
		{Convert cloud API path (e.g., /Dir/File.bin) to WFX virtual path
			(e.g., \account@mail.ru\Dir\File.bin) for TRealPath parsing}
		function ToVirtualPath(const CloudPath: WideString): WideString;

		{Upload a .cloud_timestamps file to cloud with given entries.
			@param DirPath Remote directory path
			@param Entries Tab-separated lines: 'filename<TAB>local_mtime<TAB>cloud_mtime'}
		procedure UploadTimestampFile(const DirPath: WideString; const Entries: array of WideString);

		{Download and parse .cloud_timestamps file from cloud.
			@param DirPath Remote directory path
			@param FileName File name to look up in metadata
			@param Entry Returns the found entry
			@return True if entry exists in metadata}
		function DownloadAndCheckTimestampEntry(const DirPath, FileName: WideString;
			out Entry: TTimestampEntry): Boolean;
	public
		[Test]
		procedure TestOnFileUploaded_StoresTimestampInRemoteMetadata;

		[Test]
		procedure TestOnFileUploaded_MergesWithExistingMetadata;

		[Test]
		procedure TestOnFileDownloaded_ReturnsStoredLocalMTime;

		[Test]
		procedure TestOnFileDownloaded_NoMetadata_ReturnsZero;

		[Test]
		procedure TestOnFileDeleted_RemovesEntryFromRemoteMetadata;

		[Test]
		procedure TestOnFileDeleted_NoMetadata_DoesNothing;

		[Test]
		procedure TestOnFileRenamed_SameDir_RenamesEntry;

		[Test]
		procedure TestOnFileRenamed_CrossDir_TransfersEntry;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	System.IOUtils,
	CloudDescriptionOperationsAdapter,
	TimestampSyncManager,
	TimestampMetadata,
	FileSystem,
	PathHelper,
	RealPath,
	WFXTypes,
	SettingsConstants,
	TCHandler,
	TestDataGenerator;

const
	TEST_TIMESTAMP_FILENAME = '.cloud_timestamps';

type
	{Adapter wrapper that normalizes backslash paths to forward slashes.
		TimestampSyncManager builds paths with backslashes (WFX convention),
		but cloud API expects forward slashes. This bridge enables integration testing.}
	TPathNormalizingDescriptionOps = class(TInterfacedObject, ICloudDescriptionOps)
	private
		FInner: ICloudDescriptionOps;
		function Normalize(const Path: WideString): WideString;
	public
		constructor Create(Inner: ICloudDescriptionOps);
		function GetDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
		function PutDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
		function DeleteFile(const Path: WideString): Boolean;
	end;

	{Extends TWindowsFileSystem with configurable GetFileModTime for testing}
	TTestableWindowsFileSystem = class(TWindowsFileSystem)
	private
		FModTimeOverride: Int64;
		FHasOverride: Boolean;
	public
		procedure SetModTimeOverride(Value: Int64);
		function GetFileModTime(const Path: WideString): Int64; override;
	end;

{TPathNormalizingDescriptionOps}

constructor TPathNormalizingDescriptionOps.Create(Inner: ICloudDescriptionOps);
begin
	inherited Create;
	FInner := Inner;
end;

function TPathNormalizingDescriptionOps.Normalize(const Path: WideString): WideString;
begin
	Result := PathToUrl(Path, True, False);
end;

function TPathNormalizingDescriptionOps.GetDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
begin
	Result := FInner.GetDescriptionFile(Normalize(RemotePath), LocalCopy);
end;

function TPathNormalizingDescriptionOps.PutDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
begin
	Result := FInner.PutDescriptionFile(Normalize(RemotePath), LocalCopy);
end;

function TPathNormalizingDescriptionOps.DeleteFile(const Path: WideString): Boolean;
begin
	Result := FInner.DeleteFile(Normalize(Path));
end;

{TTestableWindowsFileSystem}

procedure TTestableWindowsFileSystem.SetModTimeOverride(Value: Int64);
begin
	FModTimeOverride := Value;
	FHasOverride := True;
end;

function TTestableWindowsFileSystem.GetFileModTime(const Path: WideString): Int64;
begin
	if FHasOverride then
		Result := FModTimeOverride
	else
		Result := inherited GetFileModTime(Path);
end;

{TTimestampSyncIntegrationTest}

function TTimestampSyncIntegrationTest.ToVirtualPath(const CloudPath: WideString): WideString;
begin
	{WFX virtual path: \account\cloud\path with backslashes}
	Result := '\' + FConfig.PrimaryEmail + StringReplace(CloudPath, '/', '\', [rfReplaceAll]);
end;

procedure TTimestampSyncIntegrationTest.UploadTimestampFile(const DirPath: WideString;
	const Entries: array of WideString);
var
	LocalTsPath: WideString;
	RemoteTsPath: WideString;
	FS: IFileSystem;
	Metadata: TTimestampMetadata;
	I, TabPos1, TabPos2: Integer;
	FileName: WideString;
	Entry: TTimestampEntry;
begin
	FS := TWindowsFileSystem.Create;
	LocalTsPath := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('ts', '.timestamps'));

	Metadata := TTimestampMetadata.Create(LocalTsPath, FS);
	try
		for I := 0 to High(Entries) do
		begin
			{Parse 'filename<TAB>local_mtime<TAB>cloud_mtime'}
			TabPos1 := Pos(#9, Entries[I]);
			TabPos2 := Pos(#9, Entries[I], TabPos1 + 1);
			FileName := Copy(Entries[I], 1, TabPos1 - 1);
			Entry.LocalMTime := StrToInt64(Copy(Entries[I], TabPos1 + 1, TabPos2 - TabPos1 - 1));
			Entry.CloudMTime := StrToInt64(Copy(Entries[I], TabPos2 + 1, MaxInt));
			Metadata.SetEntry(FileName, Entry);
		end;
		Metadata.Write();
	finally
		Metadata.Free;
	end;

	RemoteTsPath := DirPath + '/' + TEST_TIMESTAMP_FILENAME;
	try
		FPrimaryCloud.Uploader.Upload(LocalTsPath, RemoteTsPath);
		TrackForCleanup(RemoteTsPath);
	finally
		TFile.Delete(LocalTsPath);
	end;
end;

function TTimestampSyncIntegrationTest.DownloadAndCheckTimestampEntry(
	const DirPath, FileName: WideString; out Entry: TTimestampEntry): Boolean;
var
	RemoteTsPath: WideString;
	LocalTempPath: WideString;
	ResultHash: WideString;
	Metadata: TTimestampMetadata;
	FS: IFileSystem;
	DownloadResult: Integer;
begin
	Result := False;
	Entry := TTimestampEntry.Empty;
	FS := TWindowsFileSystem.Create;

	RemoteTsPath := DirPath + '/' + TEST_TIMESTAMP_FILENAME;
	LocalTempPath := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('tscheck', '.timestamps'));

	ResultHash := '';
	DownloadResult := FPrimaryCloud.Downloader.Download(RemoteTsPath, LocalTempPath, ResultHash, False);
	if DownloadResult <> FS_FILE_OK then
		Exit;

	try
		Metadata := TTimestampMetadata.Create(LocalTempPath, FS);
		try
			Metadata.Read;
			Entry := Metadata.GetEntry(FileName);
			Result := not Entry.IsEmpty;
		finally
			Metadata.Free;
		end;
	finally
		TFile.Delete(LocalTempPath);
	end;
end;

procedure TTimestampSyncIntegrationTest.TestOnFileUploaded_StoresTimestampInRemoteMetadata;
var
	DirPath, RemoteFilePath: WideString;
	LocalDir, LocalFilePath: WideString;
	CloudOps: ICloudDescriptionOps;
	TestFS: TTestableWindowsFileSystem;
	SyncManager: ITimestampSyncManager;
	Entry: TTimestampEntry;
begin
	{Create remote directory}
	DirPath := UniqueCloudPath('TsUpDir');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	RemoteFilePath := DirPath + '/' + 'UploadedFile.bin';

	{Create local file to reference}
	LocalDir := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFolderName('TsUp'));
	ForceDirectories(LocalDir);
	LocalFilePath := TPath.Combine(LocalDir, 'UploadedFile.bin');
	TFile.WriteAllText(LocalFilePath, 'content');

	try
		TestFS := TTestableWindowsFileSystem.Create;
		TestFS.SetModTimeOverride(1704067200);

		CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
		SyncManager := TTimestampSyncManager.Create(TEST_TIMESTAMP_FILENAME, TestFS, TimestampConflictUseStored);

		{Simulate upload completion - stores mtime in remote metadata}
		SyncManager.OnFileUploaded(
			TRealPath.GetRealPath(ToVirtualPath(RemoteFilePath)),
			LocalFilePath, CloudOps);
		TrackForCleanup(DirPath + '/' + TEST_TIMESTAMP_FILENAME);

		{Verify remote .cloud_timestamps has the entry}
		Assert.IsTrue(
			DownloadAndCheckTimestampEntry(DirPath, 'UploadedFile.bin', Entry),
			'Remote metadata should contain entry after upload');
		Assert.AreEqual(Int64(1704067200), Entry.LocalMTime,
			'LocalMTime should match overridden value');
		Assert.AreEqual(Int64(0), Entry.CloudMTime,
			'CloudMTime should be 0 for newly uploaded file');
	finally
		if TFile.Exists(LocalFilePath) then
			TFile.Delete(LocalFilePath);
		if TDirectory.Exists(LocalDir) then
			TDirectory.Delete(LocalDir, False);
	end;
end;

procedure TTimestampSyncIntegrationTest.TestOnFileUploaded_MergesWithExistingMetadata;
var
	DirPath, RemoteFilePath: WideString;
	LocalDir, LocalFilePath: WideString;
	CloudOps: ICloudDescriptionOps;
	TestFS: TTestableWindowsFileSystem;
	SyncManager: ITimestampSyncManager;
	EntryA, EntryB: TTimestampEntry;
begin
	{Create remote dir with existing .cloud_timestamps for FileA}
	DirPath := UniqueCloudPath('TsMergeDir');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	UploadTimestampFile(DirPath, ['FileA.bin'#9'1000000'#9'0']);

	RemoteFilePath := DirPath + '/' + 'FileB.bin';

	{Create local file for FileB}
	LocalDir := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFolderName('TsMerge'));
	ForceDirectories(LocalDir);
	LocalFilePath := TPath.Combine(LocalDir, 'FileB.bin');
	TFile.WriteAllText(LocalFilePath, 'content');

	try
		TestFS := TTestableWindowsFileSystem.Create;
		TestFS.SetModTimeOverride(2000000);

		CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
		SyncManager := TTimestampSyncManager.Create(TEST_TIMESTAMP_FILENAME, TestFS, TimestampConflictUseStored);

		{Upload FileB - should merge with existing metadata containing FileA}
		SyncManager.OnFileUploaded(
			TRealPath.GetRealPath(ToVirtualPath(RemoteFilePath)),
			LocalFilePath, CloudOps);

		{Verify both entries present}
		Assert.IsTrue(DownloadAndCheckTimestampEntry(DirPath, 'FileA.bin', EntryA),
			'Existing entry A should be preserved after merge');
		Assert.AreEqual(Int64(1000000), EntryA.LocalMTime,
			'FileA LocalMTime should be unchanged');

		Assert.IsTrue(DownloadAndCheckTimestampEntry(DirPath, 'FileB.bin', EntryB),
			'New entry B should be present after merge');
		Assert.AreEqual(Int64(2000000), EntryB.LocalMTime,
			'FileB LocalMTime should match uploaded value');
	finally
		if TFile.Exists(LocalFilePath) then
			TFile.Delete(LocalFilePath);
		if TDirectory.Exists(LocalDir) then
			TDirectory.Delete(LocalDir, False);
	end;
end;

procedure TTimestampSyncIntegrationTest.TestOnFileDownloaded_ReturnsStoredLocalMTime;
var
	DirPath: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: ITimestampSyncManager;
	StoredMTime: Int64;
begin
	{Create remote dir with .cloud_timestamps containing known entry}
	DirPath := UniqueCloudPath('TsDownDir');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	UploadTimestampFile(DirPath, ['TestFile.bin'#9'1704067200'#9'0']);

	CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
	SyncManager := TTimestampSyncManager.Create(TEST_TIMESTAMP_FILENAME, TWindowsFileSystem.Create,
		TimestampConflictUseStored);

	{Simulate download completion - should return stored LocalMTime}
	StoredMTime := SyncManager.OnFileDownloaded(
		TRealPath.GetRealPath(ToVirtualPath(DirPath + '/TestFile.bin')),
		TPath.Combine(TPath.GetTempPath, 'TestFile.bin'), 0, CloudOps);

	Assert.AreEqual(Int64(1704067200), StoredMTime,
		'Should return stored LocalMTime from remote metadata');
end;

procedure TTimestampSyncIntegrationTest.TestOnFileDownloaded_NoMetadata_ReturnsZero;
var
	DirPath: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: ITimestampSyncManager;
	StoredMTime: Int64;
begin
	{Create empty directory with no .cloud_timestamps}
	DirPath := UniqueCloudPath('TsDownNoMeta');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
	SyncManager := TTimestampSyncManager.Create(TEST_TIMESTAMP_FILENAME, TWindowsFileSystem.Create,
		TimestampConflictUseStored);

	{Should return 0 when no metadata file exists}
	StoredMTime := SyncManager.OnFileDownloaded(
		TRealPath.GetRealPath(ToVirtualPath(DirPath + '/SomeFile.bin')),
		TPath.Combine(TPath.GetTempPath, 'SomeFile.bin'), 0, CloudOps);

	Assert.AreEqual(Int64(0), StoredMTime,
		'Should return 0 when no metadata exists');
end;

procedure TTimestampSyncIntegrationTest.TestOnFileDeleted_RemovesEntryFromRemoteMetadata;
var
	DirPath, FilePath: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: ITimestampSyncManager;
	EntryA, EntryB: TTimestampEntry;
begin
	{Create directory with .cloud_timestamps for two files}
	DirPath := UniqueCloudPath('TsDelDir');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	UploadTimestampFile(DirPath, [
		'FileA.bin'#9'1000000'#9'0',
		'FileB.bin'#9'2000000'#9'0'
	]);

	FilePath := DirPath + '/' + 'FileA.bin';

	CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
	SyncManager := TTimestampSyncManager.Create(TEST_TIMESTAMP_FILENAME, TWindowsFileSystem.Create,
		TimestampConflictUseStored);

	{Delete FileA entry via sync manager}
	SyncManager.OnFileDeleted(TRealPath.GetRealPath(ToVirtualPath(FilePath)), CloudOps);

	{Verify: FileA entry removed, FileB remains}
	Assert.IsFalse(DownloadAndCheckTimestampEntry(DirPath, 'FileA.bin', EntryA),
		'FileA entry should be removed from metadata after delete');

	Assert.IsTrue(DownloadAndCheckTimestampEntry(DirPath, 'FileB.bin', EntryB),
		'FileB entry should still exist in metadata');
	Assert.AreEqual(Int64(2000000), EntryB.LocalMTime,
		'FileB LocalMTime should be preserved');
end;

procedure TTimestampSyncIntegrationTest.TestOnFileDeleted_NoMetadata_DoesNothing;
var
	DirPath, FilePath: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: ITimestampSyncManager;
	DummyEntry: TTimestampEntry;
begin
	{Create directory with no metadata file}
	DirPath := UniqueCloudPath('TsDelNoMeta');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	FilePath := DirPath + '/' + 'SomeFile.bin';

	CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
	SyncManager := TTimestampSyncManager.Create(TEST_TIMESTAMP_FILENAME, TWindowsFileSystem.Create,
		TimestampConflictUseStored);

	{Should not crash or create a metadata file}
	SyncManager.OnFileDeleted(TRealPath.GetRealPath(ToVirtualPath(FilePath)), CloudOps);

	{Verify no metadata was created}
	Assert.IsFalse(DownloadAndCheckTimestampEntry(DirPath, 'SomeFile.bin', DummyEntry),
		'No metadata should be created when none existed');
end;

procedure TTimestampSyncIntegrationTest.TestOnFileRenamed_SameDir_RenamesEntry;
var
	DirPath: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: ITimestampSyncManager;
	EntryOld, EntryNew: TTimestampEntry;
begin
	{Create directory with .cloud_timestamps containing entry for OldName}
	DirPath := UniqueCloudPath('TsRenDir');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	UploadTimestampFile(DirPath, ['OldName.bin'#9'1704067200'#9'500']);

	CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
	SyncManager := TTimestampSyncManager.Create(TEST_TIMESTAMP_FILENAME, TWindowsFileSystem.Create,
		TimestampConflictUseStored);

	{Rename OldName -> NewName within same directory}
	SyncManager.OnFileRenamed(
		TRealPath.GetRealPath(ToVirtualPath(DirPath + '/OldName.bin')),
		TRealPath.GetRealPath(ToVirtualPath(DirPath + '/NewName.bin')),
		CloudOps);

	{Verify: OldName gone, NewName has the timestamps}
	Assert.IsFalse(DownloadAndCheckTimestampEntry(DirPath, 'OldName.bin', EntryOld),
		'Old entry should be removed after rename');

	Assert.IsTrue(DownloadAndCheckTimestampEntry(DirPath, 'NewName.bin', EntryNew),
		'New entry should exist after rename');
	Assert.AreEqual(Int64(1704067200), EntryNew.LocalMTime,
		'LocalMTime should be transferred to new name');
	Assert.AreEqual(Int64(500), EntryNew.CloudMTime,
		'CloudMTime should be transferred to new name');
end;

procedure TTimestampSyncIntegrationTest.TestOnFileRenamed_CrossDir_TransfersEntry;
var
	DirA, DirB: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: ITimestampSyncManager;
	EntryA, EntryB: TTimestampEntry;
begin
	{Create two directories, .cloud_timestamps in DirA with entry for File}
	DirA := UniqueCloudPath('TsCrossA');
	FPrimaryCloud.FileOperations.CreateDirectory(DirA);
	TrackForCleanup(DirA);

	DirB := UniqueCloudPath('TsCrossB');
	FPrimaryCloud.FileOperations.CreateDirectory(DirB);
	TrackForCleanup(DirB);

	UploadTimestampFile(DirA, ['File.bin'#9'1704067200'#9'999']);

	CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
	SyncManager := TTimestampSyncManager.Create(TEST_TIMESTAMP_FILENAME, TWindowsFileSystem.Create,
		TimestampConflictUseStored);

	{Move File from DirA to DirB}
	SyncManager.OnFileRenamed(
		TRealPath.GetRealPath(ToVirtualPath(DirA + '/File.bin')),
		TRealPath.GetRealPath(ToVirtualPath(DirB + '/File.bin')),
		CloudOps);

	{DirB metadata should be tracked for cleanup}
	TrackForCleanup(DirB + '/' + TEST_TIMESTAMP_FILENAME);

	{Verify: DirA loses entry, DirB gains it}
	Assert.IsFalse(DownloadAndCheckTimestampEntry(DirA, 'File.bin', EntryA),
		'DirA should lose the entry after cross-dir rename');

	Assert.IsTrue(DownloadAndCheckTimestampEntry(DirB, 'File.bin', EntryB),
		'DirB should gain the entry after cross-dir rename');
	Assert.AreEqual(Int64(1704067200), EntryB.LocalMTime,
		'LocalMTime should be transferred across directories');
	Assert.AreEqual(Int64(999), EntryB.CloudMTime,
		'CloudMTime should be transferred across directories');
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TTimestampSyncIntegrationTest);

end.
