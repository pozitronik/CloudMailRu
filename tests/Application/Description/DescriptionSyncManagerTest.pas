unit DescriptionSyncManagerTest;

{Unit tests for TDescriptionSyncManager - file description synchronization.
 Uses real temp files since DescriptionSyncManager downloads to temp paths.}

interface

uses
	System.SysUtils,
	System.Classes,
	DUnitX.TestFramework,
	RealPath,
	Description,
	WindowsFileSystem,
	TCHandler,
	CloudDescriptionOperationsAdapter,
	DescriptionSyncManager,
	MockCloudDescriptionOps;

type
	[TestFixture]
	TDescriptionSyncManagerTest = class
	private
		FManager: IDescriptionSyncManager;
		FMockCloud: TMockCloudDescriptionOps;
		FMockCloudIntf: ICloudDescriptionOps; {Keeps FMockCloud alive via interface refcount}
		FFileSystem: IFileSystem;
		FTempDir: string;
		FTempFiles: TStringList;

		function CreatePath(const AccountPath: WideString): TRealPath;
		procedure CleanupTempFiles;
		function CreateTempDir: string;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{OnFileDeleted tests}
		[Test]
		procedure TestOnFileDeleted_NoDescriptionFile_DoesNothing;
		[Test]
		procedure TestOnFileDeleted_RemovesEntryFromDescription;
		[Test]
		procedure TestOnFileDeleted_UploadsUpdatedDescription;

		{OnFileRenamed - same directory tests}
		[Test]
		procedure TestOnFileRenamed_SameDir_NoDescriptionFile_DoesNothing;
		[Test]
		procedure TestOnFileRenamed_SameDir_RenamesEntryInDescription;
		[Test]
		procedure TestOnFileRenamed_SameDir_ItemNotFound_DoesNotUpdate;

		{OnFileRenamed - different directories tests}
		[Test]
		procedure TestOnFileRenamed_DifferentDir_TransfersEntry;
		[Test]
		procedure TestOnFileRenamed_DifferentDir_NoSourceDescription_DoesNothing;

		{OnFileDownloaded tests}
		[Test]
		procedure TestOnFileDownloaded_NoRemoteDescription_DoesNothing;
		[Test]
		procedure TestOnFileDownloaded_CopiesEntryToLocalDescription;

		{OnFileUploaded tests}
		[Test]
		procedure TestOnFileUploaded_NoLocalDescription_DoesNothing;
		[Test]
		procedure TestOnFileUploaded_CopiesEntryToRemoteDescription;
	end;

implementation

uses
	Windows,
	IOUtils,
	WindowsHelper;

const
	TEST_DESCRIPTION_FILE = 'descript.ion';
	{Description format: filename<space>comment}
	DESCRIPTION_CONTENT = 'file1.txt First file description'#13#10 +
	                      'file2.txt Second file description'#13#10 +
	                      'file3.txt Third file description'#13#10;

{TDescriptionSyncManagerTest}

procedure TDescriptionSyncManagerTest.Setup;
begin
	FMockCloud := TMockCloudDescriptionOps.Create;
	FMockCloudIntf := FMockCloud; {Keep interface reference to prevent premature destruction}
	FFileSystem := TWindowsFileSystem.Create;
	FManager := TDescriptionSyncManager.Create(TEST_DESCRIPTION_FILE, FFileSystem, TNullTCHandler.Create);
	FTempFiles := TStringList.Create;
	FTempDir := CreateTempDir;
end;

procedure TDescriptionSyncManagerTest.TearDown;
begin
	CleanupTempFiles;
	FTempFiles.Free;
	FManager := nil;
	FFileSystem := nil;
	{Remove temp directory}
	if (FTempDir <> '') and DirectoryExists(FTempDir) then
		TDirectory.Delete(FTempDir, True);
	{FMockCloud is freed via FMockCloudIntf interface reference counting}
	FMockCloudIntf := nil;
	FMockCloud := nil;
end;

function TDescriptionSyncManagerTest.CreateTempDir: string;
begin
	Result := TPath.Combine(TPath.GetTempPath, 'DescSyncTest_' + IntToStr(GetTickCount));
	ForceDirectories(Result);
end;

procedure TDescriptionSyncManagerTest.CleanupTempFiles;
var
	i: Integer;
	Op: TCloudOperation;
begin
	{Clean up any temp files created during test}
	for i := 0 to FMockCloud.GetOperationCount - 1 do
	begin
		Op := FMockCloud.GetOperation(i);
		if (Op.LocalPath <> '') and System.SysUtils.FileExists(Op.LocalPath) then
			System.SysUtils.DeleteFile(Op.LocalPath);
	end;
end;

function TDescriptionSyncManagerTest.CreatePath(const AccountPath: WideString): TRealPath;
begin
	Result := TRealPath.GetRealPath(AccountPath);
end;

{OnFileDeleted tests}

procedure TDescriptionSyncManagerTest.TestOnFileDeleted_NoDescriptionFile_DoesNothing;
var
	Path: TRealPath;
begin
	{Arrange - no description file on remote}
	FMockCloud.SetDefaultResults(False, True, True);
	Path := CreatePath('\account\folder\deleted.txt');

	{Act}
	FManager.OnFileDeleted(Path, FMockCloud);

	{Assert - only GET was attempted, no PUT or DELETE}
	Assert.AreEqual(1, FMockCloud.GetOperationCount, 'Should only attempt GET');
	Assert.AreEqual('GET', FMockCloud.GetOperation(0).Operation);
end;

procedure TDescriptionSyncManagerTest.TestOnFileDeleted_RemovesEntryFromDescription;
var
	Path: TRealPath;
begin
	{Arrange - remote has description with file2.txt entry}
	FMockCloud.SetGetFileResponse('folder\' + TEST_DESCRIPTION_FILE, DESCRIPTION_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	Path := CreatePath('\account\folder\file2.txt');

	{Act}
	FManager.OnFileDeleted(Path, FMockCloud);

	{Assert - entry was removed from description file}
	Assert.IsTrue(FMockCloud.WasOperationPerformed('DELETE', TEST_DESCRIPTION_FILE),
		'Should delete old description file');
	Assert.IsTrue(FMockCloud.WasOperationPerformed('PUT', TEST_DESCRIPTION_FILE),
		'Should upload new description file');
end;

procedure TDescriptionSyncManagerTest.TestOnFileDeleted_UploadsUpdatedDescription;
var
	Path: TRealPath;
begin
	{Arrange}
	FMockCloud.SetGetFileResponse('folder\' + TEST_DESCRIPTION_FILE, DESCRIPTION_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	Path := CreatePath('\account\folder\file1.txt');

	{Act}
	FManager.OnFileDeleted(Path, FMockCloud);

	{Assert - operations in correct order: GET, DELETE, PUT}
	Assert.IsTrue(FMockCloud.GetOperationCount >= 3, 'Should have at least 3 operations');
	Assert.AreEqual('GET', FMockCloud.GetOperation(0).Operation, 'First should be GET');
	Assert.AreEqual('DELETE', FMockCloud.GetOperation(1).Operation, 'Second should be DELETE');
	Assert.AreEqual('PUT', FMockCloud.GetOperation(2).Operation, 'Third should be PUT');
end;

{OnFileRenamed - same directory tests}

procedure TDescriptionSyncManagerTest.TestOnFileRenamed_SameDir_NoDescriptionFile_DoesNothing;
var
	OldPath, NewPath: TRealPath;
begin
	{Arrange - no description file}
	FMockCloud.SetDefaultResults(False, True, True);
	OldPath := CreatePath('\account\folder\oldname.txt');
	NewPath := CreatePath('\account\folder\newname.txt');

	{Act}
	FManager.OnFileRenamed(OldPath, NewPath, FMockCloud);

	{Assert - only GET attempted}
	Assert.AreEqual(1, FMockCloud.GetOperationCount, 'Should only attempt GET');
end;

procedure TDescriptionSyncManagerTest.TestOnFileRenamed_SameDir_RenamesEntryInDescription;
var
	OldPath, NewPath: TRealPath;
begin
	{Arrange - description exists with file1.txt}
	FMockCloud.SetGetFileResponse('folder\' + TEST_DESCRIPTION_FILE, DESCRIPTION_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	OldPath := CreatePath('\account\folder\file1.txt');
	NewPath := CreatePath('\account\folder\renamed.txt');

	{Act}
	FManager.OnFileRenamed(OldPath, NewPath, FMockCloud);

	{Assert - description was updated}
	Assert.IsTrue(FMockCloud.WasOperationPerformed('DELETE', TEST_DESCRIPTION_FILE),
		'Should delete old description');
	Assert.IsTrue(FMockCloud.WasOperationPerformed('PUT', TEST_DESCRIPTION_FILE),
		'Should upload renamed description');
end;

procedure TDescriptionSyncManagerTest.TestOnFileRenamed_SameDir_ItemNotFound_DoesNotUpdate;
var
	OldPath, NewPath: TRealPath;
begin
	{Arrange - description exists but without the renamed item}
	FMockCloud.SetGetFileResponse('folder\' + TEST_DESCRIPTION_FILE, DESCRIPTION_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	OldPath := CreatePath('\account\folder\nonexistent.txt');
	NewPath := CreatePath('\account\folder\newname.txt');

	{Act}
	FManager.OnFileRenamed(OldPath, NewPath, FMockCloud);

	{Assert - only GET, no update since item not found}
	Assert.AreEqual(1, FMockCloud.GetOperationCount, 'Should only attempt GET when item not in description');
end;

{OnFileRenamed - different directories tests}

procedure TDescriptionSyncManagerTest.TestOnFileRenamed_DifferentDir_TransfersEntry;
var
	OldPath, NewPath: TRealPath;
begin
	{Arrange - moving file1.txt from folder1 to folder2}
	FMockCloud.SetGetFileResponse('folder1\' + TEST_DESCRIPTION_FILE, DESCRIPTION_CONTENT, True);
	FMockCloud.SetGetFileResponse('folder2\' + TEST_DESCRIPTION_FILE,
		'existing.txt'#9'Existing description'#13#10, True);
	FMockCloud.SetDefaultResults(True, True, True);
	OldPath := CreatePath('\account\folder1\file1.txt');
	NewPath := CreatePath('\account\folder2\file1.txt');

	{Act}
	FManager.OnFileRenamed(OldPath, NewPath, FMockCloud);

	{Assert - both description files updated}
	Assert.IsTrue(FMockCloud.WasOperationPerformed('DELETE', 'folder1\' + TEST_DESCRIPTION_FILE),
		'Should delete old source description');
	Assert.IsTrue(FMockCloud.WasOperationPerformed('PUT', 'folder1\' + TEST_DESCRIPTION_FILE),
		'Should upload updated source description');
	Assert.IsTrue(FMockCloud.WasOperationPerformed('PUT', 'folder2\' + TEST_DESCRIPTION_FILE),
		'Should upload updated target description');
end;

procedure TDescriptionSyncManagerTest.TestOnFileRenamed_DifferentDir_NoSourceDescription_DoesNothing;
var
	OldPath, NewPath: TRealPath;
begin
	{Arrange - no source description}
	FMockCloud.SetDefaultResults(False, True, True);
	OldPath := CreatePath('\account\folder1\file.txt');
	NewPath := CreatePath('\account\folder2\file.txt');

	{Act}
	FManager.OnFileRenamed(OldPath, NewPath, FMockCloud);

	{Assert - only one GET attempted}
	Assert.AreEqual(1, FMockCloud.GetOperationCount, 'Should only attempt GET source');
end;

{OnFileDownloaded tests}

procedure TDescriptionSyncManagerTest.TestOnFileDownloaded_NoRemoteDescription_DoesNothing;
var
	RemotePath: TRealPath;
	LocalFilePath: string;
begin
	{Arrange - no remote description}
	FMockCloud.SetDefaultResults(False, True, True);
	RemotePath := CreatePath('\account\folder\file.txt');
	LocalFilePath := TPath.Combine(FTempDir, 'file.txt');

	{Act}
	FManager.OnFileDownloaded(RemotePath, LocalFilePath, FMockCloud);

	{Assert - only GET attempted, no local file operations}
	Assert.AreEqual(1, FMockCloud.GetOperationCount, 'Should only attempt GET');
end;

procedure TDescriptionSyncManagerTest.TestOnFileDownloaded_CopiesEntryToLocalDescription;
var
	RemotePath: TRealPath;
	LocalFilePath, LocalDescPath: string;
begin
	{Arrange - remote has description with file1.txt}
	FMockCloud.SetGetFileResponse('folder\' + TEST_DESCRIPTION_FILE, DESCRIPTION_CONTENT, True);
	FMockCloud.SetDefaultResults(True, True, True);
	RemotePath := CreatePath('\account\folder\file1.txt');
	LocalFilePath := TPath.Combine(FTempDir, 'file1.txt');
	LocalDescPath := TPath.Combine(FTempDir, TEST_DESCRIPTION_FILE);

	{Act}
	FManager.OnFileDownloaded(RemotePath, LocalFilePath, FMockCloud);

	{Assert - local description updated}
	Assert.IsTrue(FileExists(LocalDescPath), 'Local description should exist');
end;

{OnFileUploaded tests}

procedure TDescriptionSyncManagerTest.TestOnFileUploaded_NoLocalDescription_DoesNothing;
var
	RemotePath: TRealPath;
	LocalFilePath: string;
begin
	{Arrange - no local description, ensure directory exists}
	LocalFilePath := TPath.Combine(FTempDir, 'file.txt');
	{Don't create local description file}
	RemotePath := CreatePath('\account\folder\file.txt');

	{Act}
	FManager.OnFileUploaded(RemotePath, LocalFilePath, FMockCloud);

	{Assert - no cloud operations performed}
	Assert.AreEqual(0, FMockCloud.GetOperationCount, 'Should not perform any cloud operations');
end;

procedure TDescriptionSyncManagerTest.TestOnFileUploaded_CopiesEntryToRemoteDescription;
var
	RemotePath: TRealPath;
	LocalFilePath, LocalDescPath: string;
	DescFile: TStreamWriter;
begin
	{Arrange - local has description}
	LocalFilePath := TPath.Combine(FTempDir, 'file1.txt');
	LocalDescPath := TPath.Combine(FTempDir, TEST_DESCRIPTION_FILE);

	{Create local description file}
	DescFile := TStreamWriter.Create(LocalDescPath, False, TEncoding.UTF8);
	try
		DescFile.Write(DESCRIPTION_CONTENT);
	finally
		DescFile.Free;
	end;

	FMockCloud.SetGetFileResponse('folder\' + TEST_DESCRIPTION_FILE,
		'remote.txt'#9'Remote description'#13#10, True);
	FMockCloud.SetDefaultResults(True, True, True);
	RemotePath := CreatePath('\account\folder\file1.txt');

	{Act}
	FManager.OnFileUploaded(RemotePath, LocalFilePath, FMockCloud);

	{Assert - remote description updated}
	Assert.IsTrue(FMockCloud.WasOperationPerformed('GET', TEST_DESCRIPTION_FILE),
		'Should GET remote description');
	Assert.IsTrue(FMockCloud.WasOperationPerformed('DELETE', TEST_DESCRIPTION_FILE),
		'Should DELETE old remote description');
	Assert.IsTrue(FMockCloud.WasOperationPerformed('PUT', TEST_DESCRIPTION_FILE),
		'Should PUT updated remote description');
end;

initialization
	TDUnitX.RegisterTestFixture(TDescriptionSyncManagerTest);

end.
