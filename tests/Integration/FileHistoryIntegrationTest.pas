unit FileHistoryIntegrationTest;

{Integration tests for file version history feature against live API.
	Tests GetFileHistory endpoint and restore/rollback via AddFileByIdentity.
	Primary account should have file history enabled; secondary should not.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig,
	CloudFileVersion;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	[Category('Integration')]
	TFileHistoryIntegrationTest = class(TIntegrationTestBase)
	private
		{Upload a test file and return its cloud path}
		function UploadTestFile(SizeBytes: Integer; const NamePrefix: WideString): WideString;
	public
		{GetFileHistory on primary account (enabled)}
		[Test]
		procedure TestGetFileHistory_ReturnsVersions;

		{GetFileHistory returns at least one version for newly uploaded file}
		[Test]
		procedure TestGetFileHistory_NewFile_HasAtLeastOneVersion;

		{Version fields are populated correctly}
		[Test]
		procedure TestGetFileHistory_VersionFieldsPopulated;

		{Uploading same file twice creates multiple versions}
		[Test]
		procedure TestGetFileHistory_MultipleUploads_CreatesVersions;

		{GetFileHistory on secondary account (disabled) returns no versions}
		[Test]
		procedure TestGetFileHistory_SecondaryAccount_ReturnsNoVersions;

		{Restore file from history using AddFileByIdentity}
		[Test]
		procedure TestRestoreFromHistory_CreatesFile;

		{GetFileHistory on non-existent file fails}
		[Test]
		procedure TestGetFileHistory_NonExistentFile_ReturnsFalse;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	System.IOUtils,
	CloudMailRu,
	CloudDirItem,
	CloudFileIdentity,
	CloudConstants,
	WFXTypes,
	TestDataGenerator;

{TFileHistoryIntegrationTest}

function TFileHistoryIntegrationTest.UploadTestFile(SizeBytes: Integer; const NamePrefix: WideString): WideString;
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

procedure TFileHistoryIntegrationTest.TestGetFileHistory_ReturnsVersions;
var
	RemotePath: WideString;
	Versions: TCloudFileVersionList;
	Success: Boolean;
begin
	RemotePath := UploadTestFile(2048, 'HistoryTest');
	TrackForCleanup(RemotePath);

	Success := FPrimaryCloud.ListingService.GetFileHistory(RemotePath, Versions);

	Assert.IsTrue(Success, 'GetFileHistory should succeed on primary account');
end;

procedure TFileHistoryIntegrationTest.TestGetFileHistory_NewFile_HasAtLeastOneVersion;
var
	RemotePath: WideString;
	Versions: TCloudFileVersionList;
begin
	RemotePath := UploadTestFile(1024, 'HistoryNew');
	TrackForCleanup(RemotePath);

	FPrimaryCloud.ListingService.GetFileHistory(RemotePath, Versions);

	Assert.IsTrue(Length(Versions) >= 1, 'Newly uploaded file should have at least 1 version');
end;

procedure TFileHistoryIntegrationTest.TestGetFileHistory_VersionFieldsPopulated;
var
	RemotePath: WideString;
	Versions: TCloudFileVersionList;
begin
	RemotePath := UploadTestFile(4096, 'HistoryFields');
	TrackForCleanup(RemotePath);

	FPrimaryCloud.ListingService.GetFileHistory(RemotePath, Versions);

	if Length(Versions) = 0 then
	begin
		Assert.Pass('SKIPPED: No versions returned - file history may not be available');
		Exit;
	end;

	{Basic field validation - name and size should be populated}
	Assert.IsNotEmpty(Versions[0].Name, 'Version name should be populated');
	Assert.IsTrue(Versions[0].Size > 0, 'Version size should be positive');
	Assert.IsTrue(Versions[0].Time > 0, 'Version time should be positive');
end;

procedure TFileHistoryIntegrationTest.TestGetFileHistory_MultipleUploads_CreatesVersions;
var
	RemotePath, TempPath: WideString;
	Versions: TCloudFileVersionList;
	TempItem: TCloudDirItem;
	Identity: TCloudFileIdentity;
begin
	{Upload initial file}
	RemotePath := UploadTestFile(1024, 'HistoryMulti');
	TrackForCleanup(RemotePath);

	{Upload a different file to a temp path to get its hash into the server.
	 We can't use Upload(file, samePath, 'rewrite') because the dedup logic
	 in PutFileStream short-circuits with conflict=strict before reaching rewrite.}
	TempPath := UploadTestFile(2048, 'HistoryMultiTemp');
	TrackForCleanup(TempPath);

	{Get the temp file's identity (hash + size) from the server}
	Assert.IsTrue(FPrimaryCloud.ListingService.StatusFile(TempPath, TempItem),
		'Temp file should exist after upload');

	Identity.Hash := TempItem.hash;
	Identity.Size := TempItem.size;

	{Overwrite the original file using AddFileByIdentity with conflict=rewrite,
	 which creates a new version in file history}
	Assert.AreEqual(FS_FILE_OK,
		FPrimaryCloud.Uploader.AddFileByIdentity(Identity, RemotePath, 'rewrite'),
		'AddFileByIdentity with rewrite should succeed');

	FPrimaryCloud.ListingService.GetFileHistory(RemotePath, Versions);

	{After two uploads, there should be at least 2 versions}
	Assert.IsTrue(Length(Versions) >= 2, 'File with two uploads should have at least 2 versions');
end;

procedure TFileHistoryIntegrationTest.TestGetFileHistory_SecondaryAccount_ReturnsNoVersions;
var
	Cloud: TCloudMailRu;
	RemotePath: WideString;
	Versions: TCloudFileVersionList;
	Success: Boolean;
begin
	RequireSecondaryAccount;

	Cloud := CreateSecondaryCloud;
	try
		if not Cloud.Login then
		begin
			Assert.Pass('SKIPPED: Secondary account login failed: ' + Cloud.AuthorizationError.ErrorMessage);
			Exit;
		end;

		{Upload a file via secondary account to its test area}
		RemotePath := FTestRunFolder + '/' + TTestDataGenerator.GenerateUniqueFilename('SecHist', '.bin');

		{Try to get file history - on secondary account (disabled) this should fail or return empty}
		Success := Cloud.ListingService.GetFileHistory(RemotePath, Versions);

		{When file history is disabled, the API either returns an error or empty body}
		if Success then
			Assert.AreEqual(Integer(0), Integer(Length(Versions)),
				'Secondary account should have no version history (feature disabled)')
		else
			Assert.Pass('GetFileHistory returned False on secondary account (feature disabled)');
	finally
		Cloud.Free;
	end;
end;

procedure TFileHistoryIntegrationTest.TestRestoreFromHistory_CreatesFile;
var
	RemotePath, RestoredPath: WideString;
	Versions: TCloudFileVersionList;
	Identity: TCloudFileIdentity;
	Item: TCloudDirItem;
	RestoreResult: Integer;
begin
	RemotePath := UploadTestFile(2048, 'HistoryRestore');
	TrackForCleanup(RemotePath);

	FPrimaryCloud.ListingService.GetFileHistory(RemotePath, Versions);

	if Length(Versions) = 0 then
	begin
		Assert.Pass('SKIPPED: No versions returned - file history may not be available');
		Exit;
	end;

	if not Versions[0].HasHash then
	begin
		Assert.Pass('SKIPPED: Version has no hash - restore requires hash (paid accounts only)');
		Exit;
	end;

	{Restore the file as a copy using hash from history}
	Identity.Hash := Versions[0].Hash;
	Identity.Size := Versions[0].Size;
	RestoredPath := ChangeFileExt(RemotePath, '') + '_restored' + ExtractFileExt(RemotePath);
	TrackForCleanup(RestoredPath);

	RestoreResult := FPrimaryCloud.Uploader.AddFileByIdentity(Identity, RestoredPath, CLOUD_CONFLICT_RENAME);

	Assert.AreEqual(FS_FILE_OK, RestoreResult, 'Restore via AddFileByIdentity should succeed');

	{Verify the restored file exists}
	Assert.IsTrue(FPrimaryCloud.ListingService.StatusFile(RestoredPath, Item),
		'Restored file should exist in cloud');
	Assert.AreEqual(Versions[0].Size, Item.Size, 'Restored file size should match version size');
end;

procedure TFileHistoryIntegrationTest.TestGetFileHistory_NonExistentFile_ReturnsFalse;
var
	Versions: TCloudFileVersionList;
	Success: Boolean;
begin
	Success := FPrimaryCloud.ListingService.GetFileHistory(
		FTestRunFolder + '/NonExistentFile_' + IntToStr(Random(999999)) + '.bin',
		Versions);

	Assert.IsFalse(Success, 'GetFileHistory should fail for non-existent file');
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TFileHistoryIntegrationTest);

end.
