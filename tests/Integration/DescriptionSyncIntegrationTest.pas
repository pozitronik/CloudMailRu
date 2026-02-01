unit DescriptionSyncIntegrationTest;

{Integration tests for description synchronization against live cloud.mail.ru API.
	Tests the full chain: TDescriptionSyncManager -> TCloudDescriptionOperationsAdapter
	-> TCloudMailRu -> cloud API. Verifies .ion file operations (create, read, update,
	delete) through real cloud round-trips.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	TDescriptionSyncIntegrationTest = class(TIntegrationTestBase)
	private
		{Convert cloud API path (e.g., /Dir/File.bin) to WFX virtual path
			(e.g., \account@mail.ru\Dir\File.bin) for TRealPath parsing}
		function ToVirtualPath(const CloudPath: WideString): WideString;

		function UploadTestFile(SizeBytes: Integer; const NamePrefix: WideString): WideString;

		{Upload a .ion description file to cloud with given entries.
			@param DirPath Remote directory path
			@param Entries Pairs of filename=comment (e.g., 'FileA=CommentA')}
		procedure UploadIonFile(const DirPath: WideString; const Entries: array of WideString);

		{Download and parse .ion file from cloud.
			@param DirPath Remote directory path
			@param OutEntries Returns dictionary-like array of filename=comment pairs
			@return True if .ion file exists and was parsed}
		function DownloadAndCheckIonEntry(const DirPath, FileName: WideString; out Value: WideString): Boolean;
	public
		[Test]
		procedure TestOnFileDeleted_RemovesEntryFromRemoteIon;

		[Test]
		procedure TestOnFileDeleted_NoIonFile_DoesNothing;

		[Test]
		procedure TestOnFileRenamed_SameDir_RenamesEntry;

		[Test]
		procedure TestOnFileRenamed_CrossDir_TransfersEntry;

		[Test]
		procedure TestOnFileDownloaded_CopiesDescriptionToLocal;

		[Test]
		procedure TestOnFileUploaded_CopiesDescriptionToRemote;

		[Test]
		procedure TestOnFileUploaded_NoLocalIon_DoesNothing;

		[Test]
		procedure TestOnFileUploaded_MergesWithExistingRemoteIon;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	System.IOUtils,
	CloudDirItemList,
	CloudConstants,
	CloudDescriptionOperationsAdapter,
	CloudFileOperationsAdapter,
	DescriptionSyncManager,
	PathHelper,
	Description,
	FileSystem,
	TCHandler,
	RealPath,
	WFXTypes,
	TestDataGenerator;

const
	{Default description filename used by the plugin}
	TEST_ION_FILENAME = 'descript.ion';

type
	{Adapter wrapper that normalizes backslash paths to forward slashes.
		DescriptionSyncManager builds paths with backslashes (WFX convention),
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

{TDescriptionSyncIntegrationTest}

function TDescriptionSyncIntegrationTest.ToVirtualPath(const CloudPath: WideString): WideString;
begin
	{WFX virtual path: \account\cloud\path with backslashes}
	Result := '\' + FConfig.PrimaryEmail + StringReplace(CloudPath, '/', '\', [rfReplaceAll]);
end;

function TDescriptionSyncIntegrationTest.UploadTestFile(SizeBytes: Integer; const NamePrefix: WideString): WideString;
var
	LocalFile: WideString;
	TestData: TMemoryStream;
begin
	Result := UniqueCloudPath(NamePrefix) + '.bin';
	LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('temp', '.bin'));

	TestData := TTestDataGenerator.CreateSmallTestFile(SizeBytes);
	try
		TestData.SaveToFile(LocalFile);
	finally
		TestData.Free;
	end;

	try
		FPrimaryCloud.Uploader.Upload(LocalFile, Result);
	finally
		TFile.Delete(LocalFile);
	end;
end;

procedure TDescriptionSyncIntegrationTest.UploadIonFile(const DirPath: WideString; const Entries: array of WideString);
var
	LocalIonPath: WideString;
	RemoteIonPath: WideString;
	Desc: TDescription;
	FS: IFileSystem;
	I: Integer;
	SepPos: Integer;
	Key, Value: WideString;
begin
	FS := TWindowsFileSystem.Create;
	LocalIonPath := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('desc', '.ion'));

	Desc := TDescription.Create(LocalIonPath, FS);
	try
		for I := 0 to High(Entries) do
		begin
			SepPos := Pos('=', Entries[I]);
			Key := Copy(Entries[I], 1, SepPos - 1);
			Value := Copy(Entries[I], SepPos + 1, MaxInt);
			Desc.SetValue(Key, Value);
		end;
		Desc.Write();
	finally
		Desc.Free;
	end;

	RemoteIonPath := DirPath + '/' + TEST_ION_FILENAME;
	try
		FPrimaryCloud.Uploader.Upload(LocalIonPath, RemoteIonPath);
		TrackForCleanup(RemoteIonPath);
	finally
		TFile.Delete(LocalIonPath);
	end;
end;

function TDescriptionSyncIntegrationTest.DownloadAndCheckIonEntry(const DirPath, FileName: WideString; out Value: WideString): Boolean;
var
	RemoteIonPath: WideString;
	LocalTempPath: WideString;
	ResultHash: WideString;
	Desc: TDescription;
	FS: IFileSystem;
	DownloadResult: Integer;
begin
	Result := False;
	Value := '';
	FS := TWindowsFileSystem.Create;

	RemoteIonPath := DirPath + '/' + TEST_ION_FILENAME;
	LocalTempPath := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('ioncheck', '.ion'));

	ResultHash := '';
	DownloadResult := FPrimaryCloud.Downloader.Download(RemoteIonPath, LocalTempPath, ResultHash, False);
	if DownloadResult <> FS_FILE_OK then
		Exit;

	try
		Desc := TDescription.Create(LocalTempPath, FS);
		try
			Desc.Read;
			Value := Desc.GetValue(FileName);
			Result := Value <> '';
		finally
			Desc.Free;
		end;
	finally
		TFile.Delete(LocalTempPath);
	end;
end;

procedure TDescriptionSyncIntegrationTest.TestOnFileDeleted_RemovesEntryFromRemoteIon;
var
	DirPath, FilePath: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: IDescriptionSyncManager;
	ValueA, ValueB: WideString;
begin
	{Create a directory with two files and an .ion with entries for both}
	DirPath := UniqueCloudPath('DescDelDir');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	FilePath := DirPath + '/' + 'FileA.bin';
	UploadTestFile(512, 'temp');
	{We need actual files for the .ion to reference}
	UploadIonFile(DirPath, ['FileA.bin=CommentA', 'FileB.bin=CommentB']);

	{Setup sync manager and adapter}
	CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
	SyncManager := TDescriptionSyncManager.Create(TEST_ION_FILENAME, TWindowsFileSystem.Create, TNullTCHandler.Create);

	{Delete FileA entry via sync manager}
	SyncManager.OnFileDeleted(TRealPath.GetRealPath(ToVirtualPath(FilePath)), CloudOps);

	{Verify: FileA entry removed, FileB remains}
	Assert.IsFalse(DownloadAndCheckIonEntry(DirPath, 'FileA.bin', ValueA),
		'FileA entry should be removed from .ion after delete');

	Assert.IsTrue(DownloadAndCheckIonEntry(DirPath, 'FileB.bin', ValueB),
		'FileB entry should still exist in .ion');
	Assert.AreEqual(WideString('CommentB'), ValueB, 'FileB comment should be preserved');
end;

procedure TDescriptionSyncIntegrationTest.TestOnFileDeleted_NoIonFile_DoesNothing;
var
	DirPath, FilePath: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: IDescriptionSyncManager;
begin
	{Create a directory with no .ion file}
	DirPath := UniqueCloudPath('DescDelNoIon');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	FilePath := DirPath + '/' + 'SomeFile.bin';

	CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
	SyncManager := TDescriptionSyncManager.Create(TEST_ION_FILENAME, TWindowsFileSystem.Create, TNullTCHandler.Create);

	{Should not crash or create an .ion file}
	SyncManager.OnFileDeleted(TRealPath.GetRealPath(ToVirtualPath(FilePath)), CloudOps);

	{Verify no .ion was created}
	var Dummy: WideString;
	Assert.IsFalse(DownloadAndCheckIonEntry(DirPath, 'SomeFile.bin', Dummy),
		'No .ion should be created when none existed');
end;

procedure TDescriptionSyncIntegrationTest.TestOnFileRenamed_SameDir_RenamesEntry;
var
	DirPath: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: IDescriptionSyncManager;
	ValueOld, ValueNew: WideString;
begin
	{Create directory with .ion containing entry for OldName}
	DirPath := UniqueCloudPath('DescRenDir');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	UploadIonFile(DirPath, ['OldName.bin=TestComment']);

	CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
	SyncManager := TDescriptionSyncManager.Create(TEST_ION_FILENAME, TWindowsFileSystem.Create, TNullTCHandler.Create);

	{Rename OldName -> NewName within same directory}
	SyncManager.OnFileRenamed(
		TRealPath.GetRealPath(ToVirtualPath(DirPath + '/OldName.bin')),
		TRealPath.GetRealPath(ToVirtualPath(DirPath + '/NewName.bin')),
		CloudOps);

	{Verify: OldName gone, NewName has the comment}
	Assert.IsFalse(DownloadAndCheckIonEntry(DirPath, 'OldName.bin', ValueOld),
		'Old entry should be removed after rename');

	Assert.IsTrue(DownloadAndCheckIonEntry(DirPath, 'NewName.bin', ValueNew),
		'New entry should exist after rename');
	Assert.AreEqual(WideString('TestComment'), ValueNew, 'Comment should be transferred to new name');
end;

procedure TDescriptionSyncIntegrationTest.TestOnFileRenamed_CrossDir_TransfersEntry;
var
	DirA, DirB: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: IDescriptionSyncManager;
	ValueA, ValueB: WideString;
begin
	{Create two directories, .ion in DirA with entry for File}
	DirA := UniqueCloudPath('DescCrossA');
	FPrimaryCloud.FileOperations.CreateDirectory(DirA);
	TrackForCleanup(DirA);

	DirB := UniqueCloudPath('DescCrossB');
	FPrimaryCloud.FileOperations.CreateDirectory(DirB);
	TrackForCleanup(DirB);

	UploadIonFile(DirA, ['File.bin=CrossComment']);

	CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
	SyncManager := TDescriptionSyncManager.Create(TEST_ION_FILENAME, TWindowsFileSystem.Create, TNullTCHandler.Create);

	{Move File from DirA to DirB}
	SyncManager.OnFileRenamed(
		TRealPath.GetRealPath(ToVirtualPath(DirA + '/File.bin')),
		TRealPath.GetRealPath(ToVirtualPath(DirB + '/File.bin')),
		CloudOps);

	{DirB .ion should be tracked for cleanup too}
	TrackForCleanup(DirB + '/' + TEST_ION_FILENAME);

	{Verify: DirA loses entry, DirB gains it}
	Assert.IsFalse(DownloadAndCheckIonEntry(DirA, 'File.bin', ValueA),
		'DirA should lose the entry after cross-dir rename');

	Assert.IsTrue(DownloadAndCheckIonEntry(DirB, 'File.bin', ValueB),
		'DirB should gain the entry after cross-dir rename');
	Assert.AreEqual(WideString('CrossComment'), ValueB, 'Comment should be transferred across directories');
end;

procedure TDescriptionSyncIntegrationTest.TestOnFileDownloaded_CopiesDescriptionToLocal;
var
	DirPath, RemoteFilePath: WideString;
	LocalDir, LocalFilePath, LocalIonPath: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: IDescriptionSyncManager;
	LocalDesc: TDescription;
	FS: IFileSystem;
	LocalValue: WideString;
begin
	{Create remote dir with .ion containing entry}
	DirPath := UniqueCloudPath('DescDownDir');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	RemoteFilePath := DirPath + '/' + 'CloudFile.bin';
	UploadIonFile(DirPath, ['CloudFile.bin=CloudComment']);

	{Setup local directory structure}
	LocalDir := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFolderName('DescDown'));
	ForceDirectories(LocalDir);
	LocalFilePath := TPath.Combine(LocalDir, 'CloudFile.bin');
	LocalIonPath := TPath.Combine(LocalDir, TEST_ION_FILENAME);

	try
		FS := TWindowsFileSystem.Create;
		CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
		SyncManager := TDescriptionSyncManager.Create(TEST_ION_FILENAME, FS, TNullTCHandler.Create);

		{Simulate download completion}
		SyncManager.OnFileDownloaded(TRealPath.GetRealPath(ToVirtualPath(RemoteFilePath)), LocalFilePath, CloudOps);

		{Verify local .ion has the entry}
		Assert.IsTrue(TFile.Exists(LocalIonPath), 'Local .ion should be created after download');

		LocalDesc := TDescription.Create(LocalIonPath, FS);
		try
			LocalDesc.Read;
			LocalValue := LocalDesc.GetValue('CloudFile.bin');
			Assert.AreEqual(WideString('CloudComment'), LocalValue, 'Local .ion should contain the cloud comment');
		finally
			LocalDesc.Free;
		end;
	finally
		{Cleanup local files}
		if TFile.Exists(LocalIonPath) then
			TFile.Delete(LocalIonPath);
		if TFile.Exists(LocalFilePath) then
			TFile.Delete(LocalFilePath);
		if TDirectory.Exists(LocalDir) then
			TDirectory.Delete(LocalDir, False);
	end;
end;

procedure TDescriptionSyncIntegrationTest.TestOnFileUploaded_CopiesDescriptionToRemote;
var
	DirPath, RemoteFilePath: WideString;
	LocalDir, LocalFilePath, LocalIonPath: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: IDescriptionSyncManager;
	FS: IFileSystem;
	LocalDesc: TDescription;
	RemoteValue: WideString;
begin
	{Create remote directory}
	DirPath := UniqueCloudPath('DescUpDir');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	RemoteFilePath := DirPath + '/' + 'UploadedFile.bin';

	{Create local directory with .ion containing entry}
	LocalDir := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFolderName('DescUp'));
	ForceDirectories(LocalDir);
	LocalFilePath := TPath.Combine(LocalDir, 'UploadedFile.bin');
	LocalIonPath := TPath.Combine(LocalDir, TEST_ION_FILENAME);

	try
		FS := TWindowsFileSystem.Create;

		LocalDesc := TDescription.Create(LocalIonPath, FS);
		try
			LocalDesc.SetValue('UploadedFile.bin', 'LocalComment');
			LocalDesc.Write();
		finally
			LocalDesc.Free;
		end;

		CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
		SyncManager := TDescriptionSyncManager.Create(TEST_ION_FILENAME, FS, TNullTCHandler.Create);

		{Simulate upload completion}
		SyncManager.OnFileUploaded(TRealPath.GetRealPath(ToVirtualPath(RemoteFilePath)), LocalFilePath, CloudOps);
		TrackForCleanup(DirPath + '/' + TEST_ION_FILENAME);

		{Verify remote .ion has the entry}
		Assert.IsTrue(DownloadAndCheckIonEntry(DirPath, 'UploadedFile.bin', RemoteValue),
			'Remote .ion should contain the uploaded file entry');
		Assert.AreEqual(WideString('LocalComment'), RemoteValue, 'Remote comment should match local');
	finally
		if TFile.Exists(LocalIonPath) then
			TFile.Delete(LocalIonPath);
		if TFile.Exists(LocalFilePath) then
			TFile.Delete(LocalFilePath);
		if TDirectory.Exists(LocalDir) then
			TDirectory.Delete(LocalDir, False);
	end;
end;

procedure TDescriptionSyncIntegrationTest.TestOnFileUploaded_NoLocalIon_DoesNothing;
var
	DirPath, RemoteFilePath: WideString;
	LocalDir, LocalFilePath: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: IDescriptionSyncManager;
	Dummy: WideString;
begin
	{Create remote directory}
	DirPath := UniqueCloudPath('DescUpNoIon');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	RemoteFilePath := DirPath + '/' + 'NoIonFile.bin';

	{Create local dir with NO .ion file}
	LocalDir := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFolderName('DescUpNone'));
	ForceDirectories(LocalDir);
	LocalFilePath := TPath.Combine(LocalDir, 'NoIonFile.bin');

	try
		CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
		SyncManager := TDescriptionSyncManager.Create(TEST_ION_FILENAME, TWindowsFileSystem.Create, TNullTCHandler.Create);

		{Should not crash or create remote .ion}
		SyncManager.OnFileUploaded(TRealPath.GetRealPath(ToVirtualPath(RemoteFilePath)), LocalFilePath, CloudOps);

		{Verify no remote .ion was created}
		Assert.IsFalse(DownloadAndCheckIonEntry(DirPath, 'NoIonFile.bin', Dummy),
			'No remote .ion should be created when no local .ion exists');
	finally
		if TDirectory.Exists(LocalDir) then
			TDirectory.Delete(LocalDir, False);
	end;
end;

procedure TDescriptionSyncIntegrationTest.TestOnFileUploaded_MergesWithExistingRemoteIon;
var
	DirPath, RemoteFilePath: WideString;
	LocalDir, LocalFilePath, LocalIonPath: WideString;
	CloudOps: ICloudDescriptionOps;
	SyncManager: IDescriptionSyncManager;
	FS: IFileSystem;
	LocalDesc: TDescription;
	ValueA, ValueB: WideString;
begin
	{Create remote directory with existing .ion containing entry A}
	DirPath := UniqueCloudPath('DescMergeDir');
	FPrimaryCloud.FileOperations.CreateDirectory(DirPath);
	TrackForCleanup(DirPath);

	UploadIonFile(DirPath, ['FileA.bin=RemoteComment']);

	RemoteFilePath := DirPath + '/' + 'FileB.bin';

	{Create local .ion with entry B}
	LocalDir := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFolderName('DescMerge'));
	ForceDirectories(LocalDir);
	LocalFilePath := TPath.Combine(LocalDir, 'FileB.bin');
	LocalIonPath := TPath.Combine(LocalDir, TEST_ION_FILENAME);

	try
		FS := TWindowsFileSystem.Create;

		LocalDesc := TDescription.Create(LocalIonPath, FS);
		try
			LocalDesc.SetValue('FileB.bin', 'LocalComment');
			LocalDesc.Write();
		finally
			LocalDesc.Free;
		end;

		CloudOps := TPathNormalizingDescriptionOps.Create(TCloudDescriptionOperationsAdapter.Create(FPrimaryCloud));
		SyncManager := TDescriptionSyncManager.Create(TEST_ION_FILENAME, FS, TNullTCHandler.Create);

		{Upload FileB - should merge with existing remote .ion}
		SyncManager.OnFileUploaded(TRealPath.GetRealPath(ToVirtualPath(RemoteFilePath)), LocalFilePath, CloudOps);

		{Verify both entries present in remote .ion}
		Assert.IsTrue(DownloadAndCheckIonEntry(DirPath, 'FileA.bin', ValueA),
			'Existing remote entry A should be preserved after merge');
		Assert.AreEqual(WideString('RemoteComment'), ValueA, 'Remote comment A should be unchanged');

		Assert.IsTrue(DownloadAndCheckIonEntry(DirPath, 'FileB.bin', ValueB),
			'Uploaded entry B should be present after merge');
		Assert.AreEqual(WideString('LocalComment'), ValueB, 'Local comment B should be in remote');
	finally
		if TFile.Exists(LocalIonPath) then
			TFile.Delete(LocalIonPath);
		if TFile.Exists(LocalFilePath) then
			TFile.Delete(LocalFilePath);
		if TDirectory.Exists(LocalDir) then
			TDirectory.Delete(LocalDir, False);
	end;
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TDescriptionSyncIntegrationTest);

end.
