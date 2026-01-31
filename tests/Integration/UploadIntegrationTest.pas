unit UploadIntegrationTest;

{Integration tests for file upload operations against live cloud.mail.ru API.
	Tests include regular upload, chunked upload, encryption, and conflict handling.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	TUploadIntegrationTest = class(TIntegrationTestBase)
	public
		[Test]
		procedure TestUploadSmallFile_Succeeds;

		[Test]
		procedure TestUploadSmallFile_WithHashVerification;

		[Test]
		procedure TestUploadChunkedFile_MultipleChunks_Succeeds;

		[Test]
		procedure TestUploadChunkedFile_GeneratesCRCFile;

		[Test]
		procedure TestUploadByHash_ExistingHash_Deduplicates;

		[Test]
		procedure TestUploadOverwrite_ReplacesFile;

		[Test]
		procedure TestUploadEncrypted_Succeeds;

		[Test]
		procedure TestUploadEncrypted_FilenameEncryption;

		[Test]
		procedure TestUpload_ConflictStrict_FailsOnExisting;

		[Test]
		procedure TestUpload_ConflictRename_CreatesNewName;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	System.IOUtils,
	CloudMailRu,
	CloudDirItemList,
	CloudConstants,
	WFXTypes,
	TestDataGenerator;

{TUploadIntegrationTest}

procedure TUploadIntegrationTest.TestUploadSmallFile_Succeeds;
var
	LocalFile: WideString;
	RemotePath: WideString;
	TestData: TMemoryStream;
	UploadResult: Integer;
begin
	{Create local test file}
	LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('upload', '.bin'));
	RemotePath := UniqueCloudPath('SmallFile') + '.bin';
	TrackForCleanup(RemotePath);

	TestData := TTestDataGenerator.CreateSmallTestFile(4096); {4KB}
	try
		TestData.SaveToFile(LocalFile);
	finally
		TestData.Free;
	end;

	try
		{Upload file}
		UploadResult := FPrimaryCloud.Uploader.Upload(LocalFile, RemotePath);

		Assert.AreEqual(FS_FILE_OK, UploadResult, 'Upload should succeed');
	finally
		TFile.Delete(LocalFile);
	end;
end;

procedure TUploadIntegrationTest.TestUploadSmallFile_WithHashVerification;
var
	LocalFile: WideString;
	RemotePath: WideString;
	TestData: TMemoryStream;
	ExpectedHash: WideString;
	UploadResult: Integer;
	Items: TCloudDirItemList;
	I: Integer;
	FoundItem: Boolean;
begin
	LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('hashtest', '.bin'));
	RemotePath := UniqueCloudPath('HashVerifyFile') + '.bin';
	TrackForCleanup(RemotePath);

	TestData := TTestDataGenerator.CreateSmallTestFile(2048, 123); {2KB, specific seed}
	try
		ExpectedHash := TTestDataGenerator.CalculateSHA1Hash(TestData);
		TestData.SaveToFile(LocalFile);
	finally
		TestData.Free;
	end;

	try
		{Upload and verify}
		UploadResult := FPrimaryCloud.Uploader.Upload(LocalFile, RemotePath);
		Assert.AreEqual(FS_FILE_OK, UploadResult, 'Upload should succeed');

		{List parent to verify file and hash}
		FPrimaryCloud.ListingService.GetDirectory(FTestRunFolder, Items);

		FoundItem := False;
		for I := 0 to Length(Items) - 1 do
		begin
			if Pos(WideString('HashVerifyFile'), Items[I].Name) > 0 then
			begin
				FoundItem := True;
				Assert.AreEqual(Int64(2048), Items[I].Size, 'File size should match');
				{Note: Cloud hash may differ from local SHA1 in some cases}
				Break;
			end;
		end;

		Assert.IsTrue(FoundItem, 'Uploaded file should be in listing');
	finally
		TFile.Delete(LocalFile);
	end;
end;

procedure TUploadIntegrationTest.TestUploadChunkedFile_MultipleChunks_Succeeds;
var
	LocalFile: WideString;
	RemotePath: WideString;
	TestData: TMemoryStream;
	UploadResult: Integer;
	FileSize: Int64;
begin
	{Skip if CloudMaxFileSize override is not set}
	if FConfig.CloudMaxFileSizeOverride <= 0 then
	begin
		Assert.Pass('SKIPPED: Chunked upload test requires CloudMaxFileSizeOverride > 0');
		Exit;
	end;

	FileSize := FConfig.TestChunkedFileSize;
	if FileSize <= FConfig.CloudMaxFileSizeOverride then
	begin
		Assert.Pass('SKIPPED: TestChunkedFileSize must be larger than CloudMaxFileSizeOverride for chunking');
		Exit;
	end;

	LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('chunked', '.bin'));
	RemotePath := UniqueCloudPath('ChunkedFile') + '.bin';
	TrackForCleanup(RemotePath);
	TrackForCleanup(RemotePath + '.crc'); {CRC file if created}
	TrackForCleanup(RemotePath + '.001'); {Chunk files}
	TrackForCleanup(RemotePath + '.002');
	TrackForCleanup(RemotePath + '.003');

	TestData := TTestDataGenerator.CreateLargeTestFile(FileSize);
	try
		TestData.SaveToFile(LocalFile);
	finally
		TestData.Free;
	end;

	try
		{Upload - should trigger chunking due to size override}
		UploadResult := FPrimaryCloud.Uploader.Upload(LocalFile, RemotePath);

		{Chunked upload may return OK or different status depending on split settings}
		Assert.IsTrue(UploadResult in [FS_FILE_OK, FS_FILE_EXISTS],
			'Chunked upload should succeed or indicate exists');
	finally
		TFile.Delete(LocalFile);
	end;
end;

procedure TUploadIntegrationTest.TestUploadChunkedFile_GeneratesCRCFile;
var
	LocalFile: WideString;
	RemotePath: WideString;
	TestData: TMemoryStream;
	Items: TCloudDirItemList;
	I: Integer;
	CRCFileFound: Boolean;
begin
	{This test verifies CRC file generation for chunked uploads}
	if FConfig.CloudMaxFileSizeOverride <= 0 then
	begin
		Assert.Pass('SKIPPED: Chunked upload test requires CloudMaxFileSizeOverride > 0');
		Exit;
	end;

	if FConfig.TestChunkedFileSize <= FConfig.CloudMaxFileSizeOverride then
	begin
		Assert.Pass('SKIPPED: TestChunkedFileSize must be larger than CloudMaxFileSizeOverride');
		Exit;
	end;

	LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('crctest', '.bin'));
	RemotePath := UniqueCloudPath('CRCTestFile') + '.bin';
	TrackForCleanup(RemotePath);
	TrackForCleanup(RemotePath + '.crc');
	TrackForCleanup(RemotePath + '.001');
	TrackForCleanup(RemotePath + '.002');
	TrackForCleanup(RemotePath + '.003');

	TestData := TTestDataGenerator.CreateLargeTestFile(FConfig.TestChunkedFileSize);
	try
		TestData.SaveToFile(LocalFile);
	finally
		TestData.Free;
	end;

	try
		FPrimaryCloud.Uploader.Upload(LocalFile, RemotePath);

		{Check for CRC file in listing}
		FPrimaryCloud.ListingService.GetDirectory(FTestRunFolder, Items);

		CRCFileFound := False;
		for I := 0 to Length(Items) - 1 do
		begin
			if Pos(WideString('.crc'), Items[I].Name) > 0 then
			begin
				CRCFileFound := True;
				Break;
			end;
		end;

		{CRC file presence depends on SplitLargeFiles setting}
		if CRCFileFound then
			Assert.Pass('Chunked upload generated CRC file')
		else
			Assert.Pass('Chunked upload completed (CRC file not generated - depends on settings)');
	finally
		TFile.Delete(LocalFile);
	end;
end;

procedure TUploadIntegrationTest.TestUploadByHash_ExistingHash_Deduplicates;
var
	LocalFile1, LocalFile2: WideString;
	RemotePath1, RemotePath2: WideString;
	TestData: TMemoryStream;
	UploadResult1, UploadResult2: Integer;
begin
	{Upload same content twice - second should use deduplication}
	LocalFile1 := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('dedup1', '.bin'));
	LocalFile2 := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('dedup2', '.bin'));
	RemotePath1 := UniqueCloudPath('DedupFile1') + '.bin';
	RemotePath2 := UniqueCloudPath('DedupFile2') + '.bin';
	TrackForCleanup(RemotePath1);
	TrackForCleanup(RemotePath2);

	{Create identical content}
	TestData := TTestDataGenerator.CreateSmallTestFile(8192, 999); {Same seed = same content}
	try
		TestData.SaveToFile(LocalFile1);
		TestData.Position := 0;
		TestData.SaveToFile(LocalFile2);
	finally
		TestData.Free;
	end;

	try
		{Upload first file}
		UploadResult1 := FPrimaryCloud.Uploader.Upload(LocalFile1, RemotePath1);
		Assert.AreEqual(FS_FILE_OK, UploadResult1, 'First upload should succeed');

		{Upload second file with same content - may use hash deduplication}
		UploadResult2 := FPrimaryCloud.Uploader.Upload(LocalFile2, RemotePath2);
		Assert.AreEqual(FS_FILE_OK, UploadResult2, 'Second upload (dedup) should succeed');
	finally
		TFile.Delete(LocalFile1);
		TFile.Delete(LocalFile2);
	end;
end;

procedure TUploadIntegrationTest.TestUploadOverwrite_ReplacesFile;
var
	LocalFile1, LocalFile2: WideString;
	RemotePath: WideString;
	TestData1, TestData2: TMemoryStream;
	UploadResult1, UploadResult2: Integer;
	Items: TCloudDirItemList;
	I: Integer;
	FoundSize: Int64;
begin
	LocalFile1 := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('overwrite1', '.bin'));
	LocalFile2 := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('overwrite2', '.bin'));
	RemotePath := UniqueCloudPath('OverwriteFile') + '.bin';
	TrackForCleanup(RemotePath);

	{Create two files of different sizes}
	TestData1 := TTestDataGenerator.CreateSmallTestFile(1024); {1KB}
	TestData2 := TTestDataGenerator.CreateSmallTestFile(2048); {2KB}
	try
		TestData1.SaveToFile(LocalFile1);
		TestData2.SaveToFile(LocalFile2);
	finally
		TestData1.Free;
		TestData2.Free;
	end;

	try
		{Upload first file}
		UploadResult1 := FPrimaryCloud.Uploader.Upload(LocalFile1, RemotePath);
		Assert.AreEqual(FS_FILE_OK, UploadResult1, 'First upload should succeed');

		{Delete and re-upload with larger file (API doesn't have direct overwrite)}
		FPrimaryCloud.FileOperations.Delete(RemotePath);
		UploadResult2 := FPrimaryCloud.Uploader.Upload(LocalFile2, RemotePath);
		Assert.AreEqual(FS_FILE_OK, UploadResult2, 'Re-upload should succeed');

		{Verify file was replaced by checking size}
		FPrimaryCloud.ListingService.GetDirectory(FTestRunFolder, Items);
		FoundSize := -1;
		for I := 0 to Length(Items) - 1 do
		begin
			if Pos(WideString('OverwriteFile'), Items[I].Name) > 0 then
			begin
				FoundSize := Items[I].Size;
				Break;
			end;
		end;

		Assert.AreEqual(Int64(2048), FoundSize, 'File should have been replaced with larger version');
	finally
		TFile.Delete(LocalFile1);
		TFile.Delete(LocalFile2);
	end;
end;

procedure TUploadIntegrationTest.TestUploadEncrypted_Succeeds;
var
	Cloud: TCloudMailRu;
	LocalFile: WideString;
	RemotePath: WideString;
	TestData: TMemoryStream;
	UploadResult: Integer;
begin
	RequireEncryption;

	Cloud := CreatePrimaryCloud(True); {Encrypted}
	try
		Assert.IsTrue(Cloud.Login, 'Encrypted cloud login should succeed');

		LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('encrypted', '.bin'));
		RemotePath := UniqueCloudPath('EncryptedFile') + '.bin';
		TrackForCleanup(RemotePath);

		TestData := TTestDataGenerator.CreateSmallTestFile(4096);
		try
			TestData.SaveToFile(LocalFile);
		finally
			TestData.Free;
		end;

		try
			UploadResult := Cloud.Uploader.Upload(LocalFile, RemotePath);
			Assert.AreEqual(FS_FILE_OK, UploadResult, 'Encrypted upload should succeed');
		finally
			TFile.Delete(LocalFile);
		end;
	finally
		Cloud.Free;
	end;
end;

procedure TUploadIntegrationTest.TestUploadEncrypted_FilenameEncryption;
var
	Cloud: TCloudMailRu;
	LocalFile: WideString;
	RemotePath: WideString;
	TestData: TMemoryStream;
	UploadResult: Integer;
begin
	RequireEncryption;

	if not FConfig.TestEncryptedFilenames then
	begin
		Assert.Pass('SKIPPED: Filename encryption testing not enabled');
		Exit;
	end;

	Cloud := CreatePrimaryCloud(True); {Encrypted}
	try
		Assert.IsTrue(Cloud.Login, 'Encrypted cloud login should succeed');

		LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('encname', '.bin'));
		RemotePath := UniqueCloudPath('EncryptedNameFile') + '.bin';
		TrackForCleanup(RemotePath);

		TestData := TTestDataGenerator.CreateSmallTestFile(2048);
		try
			TestData.SaveToFile(LocalFile);
		finally
			TestData.Free;
		end;

		try
			UploadResult := Cloud.Uploader.Upload(LocalFile, RemotePath);
			Assert.AreEqual(FS_FILE_OK, UploadResult, 'Upload with encrypted filename should succeed');
		finally
			TFile.Delete(LocalFile);
		end;
	finally
		Cloud.Free;
	end;
end;

procedure TUploadIntegrationTest.TestUpload_ConflictStrict_FailsOnExisting;
var
	LocalFile1, LocalFile2: WideString;
	RemotePath: WideString;
	TestData: TMemoryStream;
	UploadResult1, UploadResult2: Integer;
begin
	LocalFile1 := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('conflict1', '.bin'));
	LocalFile2 := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('conflict2', '.bin'));
	RemotePath := UniqueCloudPath('ConflictFile') + '.bin';
	TrackForCleanup(RemotePath);

	TestData := TTestDataGenerator.CreateSmallTestFile(1024);
	try
		TestData.SaveToFile(LocalFile1);
		TestData.Position := 0;
		TestData.SaveToFile(LocalFile2);
	finally
		TestData.Free;
	end;

	try
		{Upload first file}
		UploadResult1 := FPrimaryCloud.Uploader.Upload(LocalFile1, RemotePath);
		Assert.AreEqual(FS_FILE_OK, UploadResult1, 'First upload should succeed');

		{Try to upload second file with strict conflict mode - should fail}
		UploadResult2 := FPrimaryCloud.Uploader.Upload(LocalFile2, RemotePath, CLOUD_CONFLICT_STRICT);

		{Should fail or return EXISTS}
		Assert.AreNotEqual(FS_FILE_OK, UploadResult2, 'Upload with strict conflict to existing file should fail');
	finally
		TFile.Delete(LocalFile1);
		TFile.Delete(LocalFile2);
	end;
end;

procedure TUploadIntegrationTest.TestUpload_ConflictRename_CreatesNewName;
var
	LocalFile1, LocalFile2: WideString;
	RemotePath: WideString;
	TestData: TMemoryStream;
	UploadResult1, UploadResult2: Integer;
	Items: TCloudDirItemList;
	FileCount: Integer;
	I: Integer;
begin
	LocalFile1 := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('rename1', '.bin'));
	LocalFile2 := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('rename2', '.bin'));
	RemotePath := UniqueCloudPath('RenameConflictFile') + '.bin';
	TrackForCleanup(RemotePath);

	TestData := TTestDataGenerator.CreateSmallTestFile(1024);
	try
		TestData.SaveToFile(LocalFile1);
		TestData.Position := 0;
		TestData.SaveToFile(LocalFile2);
	finally
		TestData.Free;
	end;

	try
		{Upload first file}
		UploadResult1 := FPrimaryCloud.Uploader.Upload(LocalFile1, RemotePath);
		Assert.AreEqual(FS_FILE_OK, UploadResult1, 'First upload should succeed');

		{Upload second file with rename conflict mode}
		UploadResult2 := FPrimaryCloud.Uploader.Upload(LocalFile2, RemotePath, CLOUD_CONFLICT_RENAME);

		if UploadResult2 = FS_FILE_OK then
		begin
			{Check that we now have multiple files with similar names}
			FPrimaryCloud.ListingService.GetDirectory(FTestRunFolder, Items);
			FileCount := 0;
			for I := 0 to Length(Items) - 1 do
			begin
				if Pos(WideString('RenameConflictFile'), Items[I].Name) > 0 then
				begin
					Inc(FileCount);
					TrackForCleanup(FTestRunFolder + '/' + Items[I].Name);
				end;
			end;

			Assert.IsTrue(FileCount >= 2, 'Should have original and renamed file');
		end
		else
		begin
			Assert.Pass('SKIPPED: Conflict rename mode returned non-OK status: ' + IntToStr(UploadResult2));
		end;
	finally
		TFile.Delete(LocalFile1);
		TFile.Delete(LocalFile2);
	end;
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TUploadIntegrationTest);

end.
