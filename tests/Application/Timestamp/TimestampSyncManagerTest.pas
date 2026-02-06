unit TimestampSyncManagerTest;

{Unit tests for TTimestampSyncManager - file timestamp synchronization.
	Uses real temp files since TimestampSyncManager downloads to temp paths via mock cloud.}

interface

uses
	System.SysUtils,
	System.Classes,
	DUnitX.TestFramework,
	RealPath,
	TimestampMetadata,
	TimestampEntry,
	FileSystem,
	CloudDescriptionOperationsAdapter,
	TimestampSyncManager,
	MockCloudDescriptionOps;

type
	{Extends TWindowsFileSystem with configurable GetFileModTime for testing}
	TTestableWindowsFileSystem = class(TWindowsFileSystem)
	private
		FModTimeOverride: Int64;
		FHasOverride: Boolean;
	public
		procedure SetModTimeOverride(Value: Int64);
		function GetFileModTime(const Path: WideString): Int64; override;
	end;

	[TestFixture]
	TTimestampSyncManagerTest = class
	private
		FManager: ITimestampSyncManager;
		FMockCloud: TMockCloudDescriptionOps;
		FMockCloudIntf: ICloudDescriptionOps;
		FFileSystem: TTestableWindowsFileSystem;
		FFileSystemIntf: IFileSystem;
		FTempDir: string;

		function CreatePath(const AccountPath: WideString): TRealPath;
		procedure CleanupMockTempFiles;
		function CreateTempDir: string;
		function CreateTempFile(const FileName, Content: string): string;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{OnFileUploaded tests}
		[Test]
		procedure TestOnFileUploaded_StoresLocalMTime;
		[Test]
		procedure TestOnFileUploaded_NoRemoteMetadata_CreatesNew;
		[Test]
		procedure TestOnFileUploaded_ExistingRemoteMetadata_AddsEntry;
		[Test]
		procedure TestOnFileUploaded_ZeroLocalMTime_DoesNothing;

		{OnFileDownloaded tests}
		[Test]
		procedure TestOnFileDownloaded_NoRemoteMetadata_ReturnsZero;
		[Test]
		procedure TestOnFileDownloaded_EntryExists_ReturnsStoredMTime;
		[Test]
		procedure TestOnFileDownloaded_EntryMissing_ReturnsZero;
		[Test]
		procedure TestOnFileDownloaded_ConflictUseStored_ReturnsStoredMTime;
		[Test]
		procedure TestOnFileDownloaded_ConflictUseServer_ReturnsZero;
		[Test]
		procedure TestOnFileDownloaded_NoConflict_ReturnsStoredMTime;
		[Test]
		procedure TestOnFileDownloaded_ZeroCloudMTime_AlwaysReturnsStored;

		{OnFileDeleted tests}
		[Test]
		procedure TestOnFileDeleted_NoMetadata_DoesNothing;
		[Test]
		procedure TestOnFileDeleted_RemovesEntry;
		[Test]
		procedure TestOnFileDeleted_UploadsUpdatedMetadata;

		{OnFileRenamed tests - same directory}
		[Test]
		procedure TestOnFileRenamed_SameDir_NoMetadata_DoesNothing;
		[Test]
		procedure TestOnFileRenamed_SameDir_RenamesEntry;
		[Test]
		procedure TestOnFileRenamed_SameDir_EntryNotFound_DoesNotUpdate;

		{OnFileRenamed tests - different directories}
		[Test]
		procedure TestOnFileRenamed_DifferentDir_TransfersEntry;
		[Test]
		procedure TestOnFileRenamed_DifferentDir_NoSourceMetadata_DoesNothing;

		{Temp file cleanup}
		[Test]
		procedure TestOnFileDeleted_CleansTempFile;
	end;

implementation

uses
	Windows,
	IOUtils,
	SettingsConstants;

const
	TEST_TIMESTAMP_FILE = '.cloud_timestamps';
	{Tab-separated: filename<TAB>local_mtime<TAB>cloud_mtime}
	METADATA_CONTENT = 'file1.txt'#9'1704067200'#9'0'#13#10 +
		'file2.txt'#9'1704153600'#9'1704200000'#13#10;

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

{TTimestampSyncManagerTest}

procedure TTimestampSyncManagerTest.Setup;
begin
	FMockCloud := TMockCloudDescriptionOps.Create;
	FMockCloudIntf := FMockCloud;
	FFileSystem := TTestableWindowsFileSystem.Create;
	FFileSystemIntf := FFileSystem;
	FManager := TTimestampSyncManager.Create(TEST_TIMESTAMP_FILE, FFileSystemIntf,
		TimestampConflictUseStored);
	FTempDir := CreateTempDir;
end;

procedure TTimestampSyncManagerTest.TearDown;
begin
	CleanupMockTempFiles;
	FManager := nil;
	FFileSystemIntf := nil;
	FFileSystem := nil;
	if (FTempDir <> '') and DirectoryExists(FTempDir) then
		TDirectory.Delete(FTempDir, True);
	FMockCloudIntf := nil;
	FMockCloud := nil;
end;

function TTimestampSyncManagerTest.CreatePath(const AccountPath: WideString): TRealPath;
begin
	Result := TRealPath.GetRealPath(AccountPath);
end;

procedure TTimestampSyncManagerTest.CleanupMockTempFiles;
var
	I: Integer;
	Op: TCloudOperation;
begin
	for I := 0 to FMockCloud.GetOperationCount - 1 do
	begin
		Op := FMockCloud.GetOperation(I);
		if (Op.LocalPath <> '') and System.SysUtils.FileExists(Op.LocalPath) then
			System.SysUtils.DeleteFile(Op.LocalPath);
	end;
end;

function TTimestampSyncManagerTest.CreateTempDir: string;
begin
	Result := TPath.Combine(TPath.GetTempPath, 'TsSyncTest_' + IntToStr(GetTickCount));
	ForceDirectories(Result);
end;

function TTimestampSyncManagerTest.CreateTempFile(const FileName, Content: string): string;
var
	Stream: TStreamWriter;
begin
	Result := TPath.Combine(FTempDir, FileName);
	Stream := TStreamWriter.Create(Result, False, TEncoding.UTF8);
	try
		Stream.Write(Content);
	finally
		Stream.Free;
	end;
end;

{OnFileUploaded tests}

procedure TTimestampSyncManagerTest.TestOnFileUploaded_StoresLocalMTime;
var
	Path: TRealPath;
	LocalFile: string;
begin
	{Arrange - local file exists, override mtime}
	LocalFile := CreateTempFile('photo.jpg', 'content');
	FFileSystem.SetModTimeOverride(1704067200);
	FMockCloud.SetDefaultResults(False, True, True);
	Path := CreatePath('\account\folder\photo.jpg');

	{Act}
	FManager.OnFileUploaded(Path, LocalFile, FMockCloud);

	{Assert - PUT was called to upload new metadata}
	Assert.IsTrue(FMockCloud.WasOperationPerformed('PUT', TEST_TIMESTAMP_FILE),
		'Should upload timestamp metadata');
end;

procedure TTimestampSyncManagerTest.TestOnFileUploaded_NoRemoteMetadata_CreatesNew;
var
	Path: TRealPath;
	LocalFile: string;
begin
	LocalFile := CreateTempFile('photo.jpg', 'content');
	FFileSystem.SetModTimeOverride(1704067200);
	FMockCloud.SetDefaultResults(False, True, True);
	Path := CreatePath('\account\folder\photo.jpg');

	FManager.OnFileUploaded(Path, LocalFile, FMockCloud);

	{GET (fail) + PUT}
	Assert.AreEqual(2, FMockCloud.GetOperationCount, 'Should have GET + PUT');
	Assert.AreEqual('GET', FMockCloud.GetOperation(0).Operation);
	Assert.AreEqual('PUT', FMockCloud.GetOperation(1).Operation);
end;

procedure TTimestampSyncManagerTest.TestOnFileUploaded_ExistingRemoteMetadata_AddsEntry;
var
	Path: TRealPath;
	LocalFile: string;
begin
	LocalFile := CreateTempFile('new.txt', 'content');
	FFileSystem.SetModTimeOverride(9999);
	FMockCloud.SetGetFileResponse('folder\' + TEST_TIMESTAMP_FILE, METADATA_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	Path := CreatePath('\account\folder\new.txt');

	FManager.OnFileUploaded(Path, LocalFile, FMockCloud);

	Assert.IsTrue(FMockCloud.WasOperationPerformed('GET', TEST_TIMESTAMP_FILE));
	Assert.IsTrue(FMockCloud.WasOperationPerformed('DELETE', TEST_TIMESTAMP_FILE));
	Assert.IsTrue(FMockCloud.WasOperationPerformed('PUT', TEST_TIMESTAMP_FILE));
end;

procedure TTimestampSyncManagerTest.TestOnFileUploaded_ZeroLocalMTime_DoesNothing;
var
	Path: TRealPath;
	LocalFile: string;
begin
	{Arrange - override mtime to 0}
	LocalFile := CreateTempFile('file.txt', 'content');
	FFileSystem.SetModTimeOverride(0);
	Path := CreatePath('\account\folder\file.txt');

	FManager.OnFileUploaded(Path, LocalFile, FMockCloud);

	Assert.AreEqual(0, FMockCloud.GetOperationCount, 'Should not perform any cloud operations');
end;

{OnFileDownloaded tests}

procedure TTimestampSyncManagerTest.TestOnFileDownloaded_NoRemoteMetadata_ReturnsZero;
var
	Path: TRealPath;
	StoredMTime: Int64;
begin
	FMockCloud.SetDefaultResults(False, True, True);
	Path := CreatePath('\account\folder\file.txt');

	StoredMTime := FManager.OnFileDownloaded(Path, TPath.Combine(FTempDir, 'file.txt'), 0, FMockCloud);

	Assert.AreEqual(Int64(0), StoredMTime);
end;

procedure TTimestampSyncManagerTest.TestOnFileDownloaded_EntryExists_ReturnsStoredMTime;
var
	Path: TRealPath;
	StoredMTime: Int64;
begin
	{file1.txt has LocalMTime=1704067200, CloudMTime=0}
	FMockCloud.SetGetFileResponse('folder\' + TEST_TIMESTAMP_FILE, METADATA_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	Path := CreatePath('\account\folder\file1.txt');

	StoredMTime := FManager.OnFileDownloaded(Path, TPath.Combine(FTempDir, 'file1.txt'), 0, FMockCloud);

	Assert.AreEqual(Int64(1704067200), StoredMTime);
end;

procedure TTimestampSyncManagerTest.TestOnFileDownloaded_EntryMissing_ReturnsZero;
var
	Path: TRealPath;
	StoredMTime: Int64;
begin
	FMockCloud.SetGetFileResponse('folder\' + TEST_TIMESTAMP_FILE, METADATA_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	Path := CreatePath('\account\folder\unknown.txt');

	StoredMTime := FManager.OnFileDownloaded(Path, TPath.Combine(FTempDir, 'unknown.txt'), 0, FMockCloud);

	Assert.AreEqual(Int64(0), StoredMTime);
end;

procedure TTimestampSyncManagerTest.TestOnFileDownloaded_ConflictUseStored_ReturnsStoredMTime;
var
	Path: TRealPath;
	StoredMTime: Int64;
begin
	{file2.txt: CloudMTime=1704200000, pass different value to trigger conflict}
	FMockCloud.SetGetFileResponse('folder\' + TEST_TIMESTAMP_FILE, METADATA_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	Path := CreatePath('\account\folder\file2.txt');

	{CloudMTime=9999 != stored 1704200000 = conflict, but UseStored mode}
	StoredMTime := FManager.OnFileDownloaded(Path, TPath.Combine(FTempDir, 'file2.txt'), 9999, FMockCloud);

	Assert.AreEqual(Int64(1704153600), StoredMTime);
end;

procedure TTimestampSyncManagerTest.TestOnFileDownloaded_ConflictUseServer_ReturnsZero;
var
	Path: TRealPath;
	StoredMTime: Int64;
	UseServerManager: ITimestampSyncManager;
begin
	UseServerManager := TTimestampSyncManager.Create(TEST_TIMESTAMP_FILE, FFileSystemIntf,
		TimestampConflictUseServer);
	FMockCloud.SetGetFileResponse('folder\' + TEST_TIMESTAMP_FILE, METADATA_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	Path := CreatePath('\account\folder\file2.txt');

	{CloudMTime=9999 != stored 1704200000 = conflict, UseServer mode}
	StoredMTime := UseServerManager.OnFileDownloaded(Path, TPath.Combine(FTempDir, 'file2.txt'), 9999, FMockCloud);

	Assert.AreEqual(Int64(0), StoredMTime);
end;

procedure TTimestampSyncManagerTest.TestOnFileDownloaded_NoConflict_ReturnsStoredMTime;
var
	Path: TRealPath;
	StoredMTime: Int64;
begin
	{file2.txt: CloudMTime matches stored (1704200000)}
	FMockCloud.SetGetFileResponse('folder\' + TEST_TIMESTAMP_FILE, METADATA_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	Path := CreatePath('\account\folder\file2.txt');

	StoredMTime := FManager.OnFileDownloaded(Path, TPath.Combine(FTempDir, 'file2.txt'), 1704200000, FMockCloud);

	Assert.AreEqual(Int64(1704153600), StoredMTime);
end;

procedure TTimestampSyncManagerTest.TestOnFileDownloaded_ZeroCloudMTime_AlwaysReturnsStored;
var
	Path: TRealPath;
	StoredMTime: Int64;
	UseServerManager: ITimestampSyncManager;
begin
	{Even with UseServer mode, stored CloudMTime=0 means unknown = no conflict}
	UseServerManager := TTimestampSyncManager.Create(TEST_TIMESTAMP_FILE, FFileSystemIntf,
		TimestampConflictUseServer);
	FMockCloud.SetGetFileResponse('folder\' + TEST_TIMESTAMP_FILE,
		'file.txt'#9'12345'#9'0'#13#10, True);
	FMockCloud.SetDefaultResults(True, True, True);
	Path := CreatePath('\account\folder\file.txt');

	StoredMTime := UseServerManager.OnFileDownloaded(Path, TPath.Combine(FTempDir, 'file.txt'), 9999, FMockCloud);

	Assert.AreEqual(Int64(12345), StoredMTime);
end;

{OnFileDeleted tests}

procedure TTimestampSyncManagerTest.TestOnFileDeleted_NoMetadata_DoesNothing;
var
	Path: TRealPath;
begin
	FMockCloud.SetDefaultResults(False, True, True);
	Path := CreatePath('\account\folder\deleted.txt');

	FManager.OnFileDeleted(Path, FMockCloud);

	Assert.AreEqual(1, FMockCloud.GetOperationCount, 'Should only attempt GET');
	Assert.AreEqual('GET', FMockCloud.GetOperation(0).Operation);
end;

procedure TTimestampSyncManagerTest.TestOnFileDeleted_RemovesEntry;
var
	Path: TRealPath;
begin
	FMockCloud.SetGetFileResponse('folder\' + TEST_TIMESTAMP_FILE, METADATA_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	Path := CreatePath('\account\folder\file2.txt');

	FManager.OnFileDeleted(Path, FMockCloud);

	Assert.IsTrue(FMockCloud.WasOperationPerformed('DELETE', TEST_TIMESTAMP_FILE),
		'Should delete old metadata file');
	Assert.IsTrue(FMockCloud.WasOperationPerformed('PUT', TEST_TIMESTAMP_FILE),
		'Should upload updated metadata file');
end;

procedure TTimestampSyncManagerTest.TestOnFileDeleted_UploadsUpdatedMetadata;
var
	Path: TRealPath;
begin
	FMockCloud.SetGetFileResponse('folder\' + TEST_TIMESTAMP_FILE, METADATA_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	Path := CreatePath('\account\folder\file1.txt');

	FManager.OnFileDeleted(Path, FMockCloud);

	Assert.IsTrue(FMockCloud.GetOperationCount >= 3, 'Should have at least 3 operations');
	Assert.AreEqual('GET', FMockCloud.GetOperation(0).Operation);
	Assert.AreEqual('DELETE', FMockCloud.GetOperation(1).Operation);
	Assert.AreEqual('PUT', FMockCloud.GetOperation(2).Operation);
end;

{OnFileRenamed - same directory}

procedure TTimestampSyncManagerTest.TestOnFileRenamed_SameDir_NoMetadata_DoesNothing;
var
	OldPath, NewPath: TRealPath;
begin
	FMockCloud.SetDefaultResults(False, True, True);
	OldPath := CreatePath('\account\folder\oldname.txt');
	NewPath := CreatePath('\account\folder\newname.txt');

	FManager.OnFileRenamed(OldPath, NewPath, FMockCloud);

	Assert.AreEqual(1, FMockCloud.GetOperationCount, 'Should only attempt GET');
end;

procedure TTimestampSyncManagerTest.TestOnFileRenamed_SameDir_RenamesEntry;
var
	OldPath, NewPath: TRealPath;
begin
	FMockCloud.SetGetFileResponse('folder\' + TEST_TIMESTAMP_FILE, METADATA_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	OldPath := CreatePath('\account\folder\file1.txt');
	NewPath := CreatePath('\account\folder\renamed.txt');

	FManager.OnFileRenamed(OldPath, NewPath, FMockCloud);

	Assert.IsTrue(FMockCloud.WasOperationPerformed('DELETE', TEST_TIMESTAMP_FILE),
		'Should delete old metadata');
	Assert.IsTrue(FMockCloud.WasOperationPerformed('PUT', TEST_TIMESTAMP_FILE),
		'Should upload renamed metadata');
end;

procedure TTimestampSyncManagerTest.TestOnFileRenamed_SameDir_EntryNotFound_DoesNotUpdate;
var
	OldPath, NewPath: TRealPath;
begin
	FMockCloud.SetGetFileResponse('folder\' + TEST_TIMESTAMP_FILE, METADATA_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	OldPath := CreatePath('\account\folder\nonexistent.txt');
	NewPath := CreatePath('\account\folder\newname.txt');

	FManager.OnFileRenamed(OldPath, NewPath, FMockCloud);

	Assert.AreEqual(1, FMockCloud.GetOperationCount, 'Should only attempt GET when item not found');
end;

{OnFileRenamed - different directories}

procedure TTimestampSyncManagerTest.TestOnFileRenamed_DifferentDir_TransfersEntry;
var
	OldPath, NewPath: TRealPath;
begin
	FMockCloud.SetGetFileResponse('folder1\' + TEST_TIMESTAMP_FILE, METADATA_CONTENT, True);
	FMockCloud.SetGetFileResponse('folder2\' + TEST_TIMESTAMP_FILE,
		'existing.txt'#9'111'#9'222'#13#10, True);
	FMockCloud.SetDefaultResults(True, True, True);
	OldPath := CreatePath('\account\folder1\file1.txt');
	NewPath := CreatePath('\account\folder2\file1.txt');

	FManager.OnFileRenamed(OldPath, NewPath, FMockCloud);

	Assert.IsTrue(FMockCloud.WasOperationPerformed('DELETE', 'folder1\' + TEST_TIMESTAMP_FILE),
		'Should delete old source metadata');
	Assert.IsTrue(FMockCloud.WasOperationPerformed('PUT', 'folder1\' + TEST_TIMESTAMP_FILE),
		'Should upload updated source metadata');
	Assert.IsTrue(FMockCloud.WasOperationPerformed('PUT', 'folder2\' + TEST_TIMESTAMP_FILE),
		'Should upload updated target metadata');
end;

procedure TTimestampSyncManagerTest.TestOnFileRenamed_DifferentDir_NoSourceMetadata_DoesNothing;
var
	OldPath, NewPath: TRealPath;
begin
	FMockCloud.SetDefaultResults(False, True, True);
	OldPath := CreatePath('\account\folder1\file.txt');
	NewPath := CreatePath('\account\folder2\file.txt');

	FManager.OnFileRenamed(OldPath, NewPath, FMockCloud);

	Assert.AreEqual(1, FMockCloud.GetOperationCount, 'Should only attempt GET source');
end;

{Temp file cleanup}

procedure TTimestampSyncManagerTest.TestOnFileDeleted_CleansTempFile;
var
	Path: TRealPath;
	TempFilePath: WideString;
begin
	FMockCloud.SetGetFileResponse('folder\' + TEST_TIMESTAMP_FILE, METADATA_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	Path := CreatePath('\account\folder\file2.txt');

	FManager.OnFileDeleted(Path, FMockCloud);

	TempFilePath := FMockCloud.GetOperation(0).LocalPath;
	Assert.IsFalse(System.SysUtils.FileExists(TempFilePath),
		'Temp file should be deleted after operation completes');
end;

initialization
	TDUnitX.RegisterTestFixture(TTimestampSyncManagerTest);

end.
