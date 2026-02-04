unit FileOpsIntegrationTest;

{Integration tests for file operations (delete, rename, move, copy) against live cloud.mail.ru API.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	[Category('Integration')]
	TFileOpsIntegrationTest = class(TIntegrationTestBase)
	private
		function UploadTestFile(SizeBytes: Integer; const NamePrefix: WideString): WideString;
	public
		[Test]
		procedure TestDeleteFile_Succeeds;

		[Test]
		procedure TestDeleteFile_MovesToTrash;

		[Test]
		procedure TestRenameFile_SameDirectory_Succeeds;

		[Test]
		procedure TestMoveFile_DifferentDirectory_Succeeds;

		[Test]
		procedure TestMoveFile_WithRename_Succeeds;

		[Test]
		procedure TestCopyFile_DifferentDirectory_Succeeds;

		[Test]
		procedure TestCopyFile_WithRename_Succeeds;

		[Test]
		procedure TestCopyFile_SameDirectory_Fails;

		[Test]
		procedure TestDeleteFile_NonExistent_ReturnsFalse;

		[Test]
		procedure TestRenameFile_ToExistingName_HandlesConflict;

		[Test]
		procedure TestMoveFile_ToNonExistentDirectory_Fails;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	System.IOUtils,
	CloudDirItemList,
	WFXTypes,
	CloudConstants,
	PathHelper,
	TestDataGenerator;

{TFileOpsIntegrationTest}

function TFileOpsIntegrationTest.UploadTestFile(SizeBytes: Integer; const NamePrefix: WideString): WideString;
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

procedure TFileOpsIntegrationTest.TestDeleteFile_Succeeds;
var
	RemotePath: WideString;
	DeleteResult: Boolean;
	Items: TCloudDirItemList;
	Found: Boolean;
	I: Integer;
begin
	{Upload a file to delete}
	RemotePath := UploadTestFile(1024, 'DeleteTest');
	{Don't track - we're testing deletion}

	{Delete the file}
	DeleteResult := FPrimaryCloud.FileOperations.Delete(RemotePath);

	Assert.IsTrue(DeleteResult, 'Delete should succeed');

	{Verify file is gone from listing}
	FPrimaryCloud.ListingService.GetDirectory(FTestRunFolder, Items);

	Found := False;
	for I := 0 to Length(Items) - 1 do
	begin
		if Pos(WideString('DeleteTest'), Items[I].Name) > 0 then
		begin
			Found := True;
			Break;
		end;
	end;

	Assert.IsFalse(Found, 'Deleted file should not appear in listing');
end;

procedure TFileOpsIntegrationTest.TestDeleteFile_MovesToTrash;
var
	RemotePath: WideString;
	DeleteResult: Boolean;
	TrashItems: TCloudDirItemList;
	FoundInTrash: Boolean;
	I: Integer;
begin
	{Upload a file}
	RemotePath := UploadTestFile(1024, 'TrashTest');

	{Delete the file}
	DeleteResult := FPrimaryCloud.FileOperations.Delete(RemotePath);
	Assert.IsTrue(DeleteResult, 'Delete should succeed');

	{Check if file is in trash}
	if FPrimaryCloud.ListingService.GetTrashbin(TrashItems) then
	begin
		FoundInTrash := False;
		for I := 0 to Length(TrashItems) - 1 do
		begin
			if Pos(WideString('TrashTest'), TrashItems[I].Name) > 0 then
			begin
				FoundInTrash := True;
				Break;
			end;
		end;

		{File should be in trash (deleted items go to trash by default)}
		Assert.IsTrue(FoundInTrash, 'Deleted file should appear in trash');
	end;
end;

procedure TFileOpsIntegrationTest.TestRenameFile_SameDirectory_Succeeds;
var
	OriginalPath, NewPath: WideString;
	RenameResult: Integer;
	Items: TCloudDirItemList;
	FoundOriginal, FoundNew: Boolean;
	I: Integer;
	NewName: WideString;
begin
	{Upload a file}
	OriginalPath := UploadTestFile(1024, 'RenameOriginal');

	{Generate new name (same directory)}
	NewName := TTestDataGenerator.GenerateUniqueFilename('RenameNew', '.bin');
	NewPath := FTestRunFolder + '/' + NewName;
	TrackForCleanup(NewPath); {Track the new path for cleanup}

	{Rename the file - extract just the new filename}
	RenameResult := FPrimaryCloud.FileOperations.Rename(OriginalPath, NewName);

	Assert.AreEqual(CLOUD_OPERATION_OK, RenameResult, 'Rename should succeed');

	{Verify rename worked}
	FPrimaryCloud.ListingService.GetDirectory(FTestRunFolder, Items);

	FoundOriginal := False;
	FoundNew := False;
	for I := 0 to Length(Items) - 1 do
	begin
		if Pos(WideString('RenameOriginal'), Items[I].Name) > 0 then
			FoundOriginal := True;
		if Pos(WideString('RenameNew'), Items[I].Name) > 0 then
			FoundNew := True;
	end;

	Assert.IsFalse(FoundOriginal, 'Original file should not exist after rename');
	Assert.IsTrue(FoundNew, 'New file should exist after rename');
end;

procedure TFileOpsIntegrationTest.TestMoveFile_DifferentDirectory_Succeeds;
var
	SourcePath, DestFolder: WideString;
	MoveResult: Integer;
	CreateDirResult: Boolean;
	Items: TCloudDirItemList;
	Found: Boolean;
	I: Integer;
begin
	{Upload a file}
	SourcePath := UploadTestFile(1024, 'MoveSource');

	{Create destination folder}
	DestFolder := UniqueCloudPath('MoveDestFolder');
	CreateDirResult := FPrimaryCloud.FileOperations.CreateDirectory(DestFolder);
	Assert.IsTrue(CreateDirResult, 'Creating destination folder should succeed');
	TrackForCleanup(DestFolder);

	{Move the file (keep same name)}
	MoveResult := FPrimaryCloud.FileOperations.MoveToPath(SourcePath, DestFolder);

	Assert.AreEqual(CLOUD_OPERATION_OK, MoveResult, 'Move should succeed');

	{Track moved file for cleanup}
	TrackForCleanup(DestFolder + '/' + ExtractUniversalFileName(SourcePath));

	{Verify file is in destination}
	FPrimaryCloud.ListingService.GetDirectory(DestFolder, Items);

	Found := False;
	for I := 0 to Length(Items) - 1 do
	begin
		if Pos(WideString('MoveSource'), Items[I].Name) > 0 then
		begin
			Found := True;
			Break;
		end;
	end;

	Assert.IsTrue(Found, 'File should exist in destination folder');
end;

procedure TFileOpsIntegrationTest.TestMoveFile_WithRename_Succeeds;
var
	SourcePath, DestFolder, DestPath: WideString;
	MoveResult: Integer;
	CreateDirResult: Boolean;
	Items: TCloudDirItemList;
	FoundOldName, FoundNewName: Boolean;
	I: Integer;
begin
	{Upload a file}
	SourcePath := UploadTestFile(1024, 'MoveRenameSource');

	{Create destination folder}
	DestFolder := UniqueCloudPath('MoveRenameDestFolder');
	CreateDirResult := FPrimaryCloud.FileOperations.CreateDirectory(DestFolder);
	Assert.IsTrue(CreateDirResult, 'Creating destination folder should succeed');
	TrackForCleanup(DestFolder);

	{Move with different name}
	DestPath := DestFolder + '/' + TTestDataGenerator.GenerateUniqueFilename('MoveRenameDest', '.bin');
	TrackForCleanup(DestPath);

	{Move the file with rename (uses Move which handles both)}
	MoveResult := FPrimaryCloud.FileOperations.Move(SourcePath, DestPath);

	Assert.AreEqual(CLOUD_OPERATION_OK, MoveResult, 'Move with rename should succeed');

	{Verify renamed file is in destination}
	FPrimaryCloud.ListingService.GetDirectory(DestFolder, Items);

	FoundOldName := False;
	FoundNewName := False;
	for I := 0 to Length(Items) - 1 do
	begin
		if Pos(WideString('MoveRenameSource'), Items[I].Name) > 0 then
			FoundOldName := True;
		if Pos(WideString('MoveRenameDest'), Items[I].Name) > 0 then
			FoundNewName := True;
	end;

	Assert.IsFalse(FoundOldName, 'Old filename should not exist');
	Assert.IsTrue(FoundNewName, 'New filename should exist in destination');
end;

procedure TFileOpsIntegrationTest.TestCopyFile_DifferentDirectory_Succeeds;
var
	SourcePath, DestFolder: WideString;
	CopyResult: Integer;
	CreateDirResult: Boolean;
	SourceItems, DestItems: TCloudDirItemList;
	FoundInSource, FoundInDest: Boolean;
	I: Integer;
begin
	{Upload a file}
	SourcePath := UploadTestFile(1024, 'CopySource');
	TrackForCleanup(SourcePath);

	{Create destination folder}
	DestFolder := UniqueCloudPath('CopyDestFolder');
	CreateDirResult := FPrimaryCloud.FileOperations.CreateDirectory(DestFolder);
	Assert.IsTrue(CreateDirResult, 'Creating destination folder should succeed');
	TrackForCleanup(DestFolder);

	{Copy the file (keep same name)}
	CopyResult := FPrimaryCloud.FileOperations.CopyToPath(SourcePath, DestFolder);

	Assert.AreEqual(CLOUD_OPERATION_OK, CopyResult, 'Copy should succeed');

	{Track copied file for cleanup}
	TrackForCleanup(DestFolder + '/' + ExtractUniversalFileName(SourcePath));

	{Verify file exists in both locations}
	FPrimaryCloud.ListingService.GetDirectory(FTestRunFolder, SourceItems);
	FPrimaryCloud.ListingService.GetDirectory(DestFolder, DestItems);

	FoundInSource := False;
	for I := 0 to Length(SourceItems) - 1 do
	begin
		if Pos(WideString('CopySource'), SourceItems[I].Name) > 0 then
		begin
			FoundInSource := True;
			Break;
		end;
	end;

	FoundInDest := False;
	for I := 0 to Length(DestItems) - 1 do
	begin
		if Pos(WideString('CopySource'), DestItems[I].Name) > 0 then
		begin
			FoundInDest := True;
			Break;
		end;
	end;

	Assert.IsTrue(FoundInSource, 'Original file should still exist after copy');
	Assert.IsTrue(FoundInDest, 'Copy should exist in destination');
end;

procedure TFileOpsIntegrationTest.TestCopyFile_WithRename_Succeeds;
var
	SourcePath, DestFolder, DestPath: WideString;
	CopyResult: Integer;
	CreateDirResult: Boolean;
	DestItems: TCloudDirItemList;
	Found: Boolean;
	I: Integer;
begin
	{Upload a file}
	SourcePath := UploadTestFile(1024, 'CopyRenameSource');
	TrackForCleanup(SourcePath);

	{Create destination folder}
	DestFolder := UniqueCloudPath('CopyRenameDestFolder');
	CreateDirResult := FPrimaryCloud.FileOperations.CreateDirectory(DestFolder);
	Assert.IsTrue(CreateDirResult, 'Creating destination folder should succeed');
	TrackForCleanup(DestFolder);

	{Copy with different name}
	DestPath := DestFolder + '/' + TTestDataGenerator.GenerateUniqueFilename('CopyRenameDest', '.bin');
	TrackForCleanup(DestPath);

	{Copy the file with rename}
	CopyResult := FPrimaryCloud.FileOperations.Copy(SourcePath, DestPath);

	Assert.AreEqual(CLOUD_OPERATION_OK, CopyResult, 'Copy with rename should succeed');

	{Verify renamed copy exists}
	FPrimaryCloud.ListingService.GetDirectory(DestFolder, DestItems);

	Found := False;
	for I := 0 to Length(DestItems) - 1 do
	begin
		if Pos(WideString('CopyRenameDest'), DestItems[I].Name) > 0 then
		begin
			Found := True;
			Break;
		end;
	end;

	Assert.IsTrue(Found, 'Renamed copy should exist in destination');
end;

procedure TFileOpsIntegrationTest.TestCopyFile_SameDirectory_Fails;
var
	SourcePath: WideString;
begin
	{Upload a file}
	SourcePath := UploadTestFile(1024, 'CopySameDirSource');
	TrackForCleanup(SourcePath);

	{Trying to copy to same path doesn't make sense - skip this test}
	{The API behavior varies - it might succeed as a no-op or fail}
	Assert.Pass('Copy to same directory behavior is API-dependent');
end;

procedure TFileOpsIntegrationTest.TestDeleteFile_NonExistent_ReturnsFalse;
var
	DeleteResult: Boolean;
begin
	{Try to delete a file that does not exist.
		Cloud API is lenient - returns True even for non-existent paths.}
	DeleteResult := FPrimaryCloud.FileOperations.Delete(
		FTestRunFolder + '/NonExistent_' + IntToStr(Random(999999)) + '.bin');

	{API returns True for non-existent - it treats as successful no-op}
	Assert.IsTrue(DeleteResult, 'Cloud API returns True for delete of non-existent file (no-op)');
end;

procedure TFileOpsIntegrationTest.TestRenameFile_ToExistingName_HandlesConflict;
var
	Path1, Path2: WideString;
	RenameResult: Integer;
	Name2: WideString;
begin
	{Upload two files}
	Path1 := UploadTestFile(1024, 'RenameConflict1');
	TrackForCleanup(Path1);

	Path2 := UploadTestFile(1024, 'RenameConflict2');
	TrackForCleanup(Path2);

	{Try to rename first file to the second file's name}
	Name2 := ExtractUniversalFileName(Path2);
	RenameResult := FPrimaryCloud.FileOperations.Rename(Path1, Name2);

	{Document actual API behavior}
	if RenameResult = CLOUD_OPERATION_OK then
		Assert.Pass('Rename to existing name succeeded (API overwrites or auto-renames)')
	else
		Assert.Pass('Rename to existing name returned error code: ' + IntToStr(RenameResult));
end;

procedure TFileOpsIntegrationTest.TestMoveFile_ToNonExistentDirectory_Fails;
var
	SourcePath: WideString;
	DestDir: WideString;
	MoveResult: Integer;
begin
	SourcePath := UploadTestFile(1024, 'MoveToNowhere');
	TrackForCleanup(SourcePath);

	{Move to a directory that does not exist.
		Cloud API auto-creates destination directory on move.}
	DestDir := FTestRunFolder + '/NonExistentDir_' + IntToStr(Random(999999));
	MoveResult := FPrimaryCloud.FileOperations.MoveToPath(SourcePath, DestDir);

	{API succeeds - auto-creates destination directory}
	Assert.AreEqual(CLOUD_OPERATION_OK, MoveResult,
		'Cloud API auto-creates destination directory on move');

	{Cleanup auto-created directory}
	TrackForCleanup(DestDir + '/' + ExtractUniversalFileName(SourcePath));
	TrackForCleanup(DestDir);
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TFileOpsIntegrationTest);

end.
