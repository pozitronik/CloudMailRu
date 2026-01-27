unit DownloadIntegrationTest;

{Integration tests for file download operations against live cloud.mail.ru API.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	TDownloadIntegrationTest = class(TIntegrationTestBase)
	private
		function UploadTestFile(SizeBytes: Integer; const NamePrefix: WideString): WideString;
	public
		[Test]
		procedure TestDownloadFile_Succeeds;

		[Test]
		procedure TestDownloadFile_HashMatches;

		[Test]
		procedure TestDownloadFromPublicAccount_Succeeds;

		[Test]
		procedure TestDownloadEncrypted_Decrypts;

		[Test]
		procedure TestDownloadEncrypted_FilenameDecryption;

		[Test]
		procedure TestDownload_NonExistent_Fails;

		[Test]
		procedure TestGetStreamingUrl_ReturnsValidUrl;

		[Test]
		procedure TestStatusFile_ReturnsMetadata;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	System.IOUtils,
	CloudMailRu,
	CMRDirItem,
	CMRDirItemList,
	WFXTypes,
	TestDataGenerator;

{TDownloadIntegrationTest}

function TDownloadIntegrationTest.UploadTestFile(SizeBytes: Integer; const NamePrefix: WideString): WideString;
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

procedure TDownloadIntegrationTest.TestDownloadFile_Succeeds;
var
	RemotePath: WideString;
	LocalFile: WideString;
	ResultHash: WideString;
	DownloadResult: Integer;
begin
	{First upload a file to download}
	RemotePath := UploadTestFile(4096, 'DownloadTest');
	TrackForCleanup(RemotePath);

	LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('downloaded', '.bin'));

	try
		{Download the file}
		ResultHash := '';
		DownloadResult := FPrimaryCloud.Downloader.Download(RemotePath, LocalFile, ResultHash);

		Assert.AreEqual(FS_FILE_OK, DownloadResult, 'Download should succeed');
		Assert.IsTrue(TFile.Exists(LocalFile), 'Downloaded file should exist locally');
		Assert.AreEqual(Int64(4096), TFile.GetSize(LocalFile), 'Downloaded file size should match');
	finally
		if TFile.Exists(LocalFile) then
			TFile.Delete(LocalFile);
	end;
end;

procedure TDownloadIntegrationTest.TestDownloadFile_HashMatches;
var
	RemotePath: WideString;
	LocalFile: WideString;
	OriginalData, DownloadedData: TMemoryStream;
	OriginalHash, DownloadedHash, ResultHash: WideString;
	DownloadResult: Integer;
begin
	{Create file with known content}
	RemotePath := UniqueCloudPath('HashMatch') + '.bin';
	TrackForCleanup(RemotePath);

	LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('temp', '.bin'));

	OriginalData := TTestDataGenerator.CreateSmallTestFile(8192, 12345);
	try
		OriginalHash := TTestDataGenerator.CalculateSHA1Hash(OriginalData);
		OriginalData.SaveToFile(LocalFile);
	finally
		OriginalData.Free;
	end;

	try
		{Upload}
		FPrimaryCloud.Uploader.Upload(LocalFile, RemotePath);
		TFile.Delete(LocalFile);

		{Download to different file}
		LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('downloaded', '.bin'));
		ResultHash := '';
		DownloadResult := FPrimaryCloud.Downloader.Download(RemotePath, LocalFile, ResultHash);

		Assert.AreEqual(FS_FILE_OK, DownloadResult, 'Download should succeed');

		{Verify hash matches}
		DownloadedData := TMemoryStream.Create;
		try
			DownloadedData.LoadFromFile(LocalFile);
			DownloadedHash := TTestDataGenerator.CalculateSHA1Hash(DownloadedData);
		finally
			DownloadedData.Free;
		end;

		Assert.AreEqual(OriginalHash, DownloadedHash, 'Downloaded file hash should match original');
	finally
		if TFile.Exists(LocalFile) then
			TFile.Delete(LocalFile);
	end;
end;

procedure TDownloadIntegrationTest.TestDownloadFromPublicAccount_Succeeds;
var
	Cloud: TCloudMailRu;
	Items: TCMRDirItemList;
	LocalFile: WideString;
	RemotePath: WideString;
	ResultHash: WideString;
	DownloadResult: Integer;
begin
	RequirePublicUrl;

	Cloud := CreatePublicCloud;
	try
		Assert.IsTrue(Cloud.Login, 'Public cloud login should succeed');

		{List public folder to find a file}
		if not Cloud.ListingService.GetDirectory('/', Items) then
		begin
			Assert.Pass('SKIPPED: Could not list public folder - may be empty or inaccessible');
			Exit;
		end;

		if Length(Items) = 0 then
		begin
			Assert.Pass('SKIPPED: Public folder is empty - cannot test download');
			Exit;
		end;

		{Find first file (not directory)}
		RemotePath := '';
		for var I := 0 to Length(Items) - 1 do
		begin
			if not Items[I].IsDir then
			begin
				RemotePath := '/' + Items[I].Name;
				Break;
			end;
		end;

		if RemotePath = '' then
		begin
			Assert.Pass('SKIPPED: No files in public folder - only directories');
			Exit;
		end;

		LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('public', '.bin'));

		try
			ResultHash := '';
			DownloadResult := Cloud.Downloader.Download(RemotePath, LocalFile, ResultHash);
			Assert.AreEqual(FS_FILE_OK, DownloadResult, 'Download from public account should succeed');
		finally
			if TFile.Exists(LocalFile) then
				TFile.Delete(LocalFile);
		end;
	finally
		Cloud.Free;
	end;
end;

procedure TDownloadIntegrationTest.TestDownloadEncrypted_Decrypts;
var
	Cloud: TCloudMailRu;
	LocalUploadFile, LocalDownloadFile: WideString;
	RemotePath: WideString;
	OriginalData: TMemoryStream;
	OriginalHash, DownloadedHash, ResultHash: WideString;
	UploadResult, DownloadResult: Integer;
	DownloadedData: TMemoryStream;
begin
	RequireEncryption;

	Cloud := CreatePrimaryCloud(True, False);
	try
		Assert.IsTrue(Cloud.Login, 'Encrypted cloud login should succeed');

		RemotePath := UniqueCloudPath('EncryptedDownload') + '.bin';
		TrackForCleanup(RemotePath);

		LocalUploadFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('enc_upload', '.bin'));
		LocalDownloadFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('enc_download', '.bin'));

		{Create and upload encrypted file}
		OriginalData := TTestDataGenerator.CreateSmallTestFile(4096, 777);
		try
			OriginalHash := TTestDataGenerator.CalculateSHA1Hash(OriginalData);
			OriginalData.SaveToFile(LocalUploadFile);
		finally
			OriginalData.Free;
		end;

		try
			UploadResult := Cloud.Uploader.Upload(LocalUploadFile, RemotePath);
			Assert.AreEqual(FS_FILE_OK, UploadResult, 'Encrypted upload should succeed');

			{Download and verify decryption}
			ResultHash := '';
			DownloadResult := Cloud.Downloader.Download(RemotePath, LocalDownloadFile, ResultHash);
			Assert.AreEqual(FS_FILE_OK, DownloadResult, 'Encrypted download should succeed');

			DownloadedData := TMemoryStream.Create;
			try
				DownloadedData.LoadFromFile(LocalDownloadFile);
				DownloadedHash := TTestDataGenerator.CalculateSHA1Hash(DownloadedData);
			finally
				DownloadedData.Free;
			end;

			Assert.AreEqual(OriginalHash, DownloadedHash, 'Decrypted content should match original');
		finally
			if TFile.Exists(LocalUploadFile) then
				TFile.Delete(LocalUploadFile);
			if TFile.Exists(LocalDownloadFile) then
				TFile.Delete(LocalDownloadFile);
		end;
	finally
		Cloud.Free;
	end;
end;

procedure TDownloadIntegrationTest.TestDownloadEncrypted_FilenameDecryption;
begin
	RequireEncryption;

	if not FConfig.TestEncryptedFilenames then
	begin
		Assert.Pass('SKIPPED: Filename encryption testing not enabled');
		Exit;
	end;

	{Filename decryption is transparent - when listing with same encryption key,
		filenames appear decrypted. This test would need to verify the cloud
		actually stores encrypted names, which requires listing without cipher.}
	Assert.Pass('SKIPPED: Filename decryption verification requires comparing listings with/without cipher');
end;

procedure TDownloadIntegrationTest.TestDownload_NonExistent_Fails;
var
	LocalFile: WideString;
	ResultHash: WideString;
	DownloadResult: Integer;
begin
	LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('nonexistent', '.bin'));

	try
		ResultHash := '';
		DownloadResult := FPrimaryCloud.Downloader.Download(
			FTestRunFolder + '/NonExistentFile_' + IntToStr(Random(999999)) + '.bin',
			LocalFile,
			ResultHash);

		Assert.AreNotEqual(FS_FILE_OK, DownloadResult, 'Download of non-existent file should fail');
	finally
		if TFile.Exists(LocalFile) then
			TFile.Delete(LocalFile);
	end;
end;

procedure TDownloadIntegrationTest.TestGetStreamingUrl_ReturnsValidUrl;
var
	RemotePath: WideString;
	StreamingUrl: WideString;
begin
	{First upload a file}
	RemotePath := UploadTestFile(1024, 'StreamingTest');
	TrackForCleanup(RemotePath);

	{Get streaming URL}
	StreamingUrl := FPrimaryCloud.Downloader.GetSharedFileUrl(RemotePath);

	Assert.IsNotEmpty(String(StreamingUrl), 'Streaming URL should not be empty');
	Assert.IsTrue(Pos('http', LowerCase(StreamingUrl)) = 1, 'Streaming URL should start with http');
end;

procedure TDownloadIntegrationTest.TestStatusFile_ReturnsMetadata;
var
	RemotePath: WideString;
	Item: TCMRDirItem;
	StatusResult: Boolean;
begin
	{First upload a file with known size}
	RemotePath := UploadTestFile(2048, 'StatusTest');
	TrackForCleanup(RemotePath);

	{Get file status/metadata}
	StatusResult := FPrimaryCloud.ListingService.StatusFile(RemotePath, Item);

	Assert.IsTrue(StatusResult, 'Getting file status should succeed');
	Assert.AreEqual(Int64(2048), Item.Size, 'File size should match');
	Assert.IsFalse(Item.IsDir, 'Item should be a file, not directory');
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TDownloadIntegrationTest);

end.
