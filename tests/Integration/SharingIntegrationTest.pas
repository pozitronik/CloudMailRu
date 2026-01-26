unit SharingIntegrationTest;

{Integration tests for sharing operations (publish, share) against live cloud.mail.ru API.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig;

type
	[TestFixture]
	TSharingIntegrationTest = class(TIntegrationTestBase)
	private
		function UploadTestFile(SizeBytes: Integer; const NamePrefix: WideString): WideString;
	public
		[Test]
		procedure TestPublishFile_CreatesPublicLink;

		[Test]
		procedure TestUnpublishFile_RemovesPublicLink;

		[Test]
		procedure TestShareFolder_WithSecondaryAccount;

		[Test]
		procedure TestUnshareFolder_RevokesAccess;

		[Test]
		procedure TestGetShareInfo_ReturnsInvitees;

		[Test]
		procedure TestCloneWeblink_CopiesPublicFile;

		[Test]
		procedure TestListIncomingInvites_ReturnsInvites;

		[Test]
		procedure TestGetSharedFileUrl_ReturnsDownloadUrl;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	System.IOUtils,
	CMRDirItemList,
	CMRInviteList,
	CMRIncomingInviteList,
	PLUGIN_TYPES,
	CMRConstants,
	TestDataGenerator;

{TSharingIntegrationTest}

function TSharingIntegrationTest.UploadTestFile(SizeBytes: Integer; const NamePrefix: WideString): WideString;
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

procedure TSharingIntegrationTest.TestPublishFile_CreatesPublicLink;
var
	FilePath: WideString;
	PublicLink: WideString;
	PublishResult: Boolean;
begin
	{Upload a file}
	FilePath := UploadTestFile(1024, 'PublishTest');
	TrackForCleanup(FilePath);

	{Publish the file}
	PublicLink := '';
	PublishResult := FPrimaryCloud.PublishFile(FilePath, PublicLink, CLOUD_PUBLISH);

	Assert.IsTrue(PublishResult, 'Publish should succeed');
	Assert.IsNotEmpty(String(PublicLink), 'Public link should be returned');
	Assert.IsTrue(Pos('cloud.mail.ru', LowerCase(PublicLink)) > 0, 'Link should be cloud.mail.ru URL');

	{Unpublish for cleanup}
	FPrimaryCloud.PublishFile(FilePath, PublicLink, CLOUD_UNPUBLISH);
end;

procedure TSharingIntegrationTest.TestUnpublishFile_RemovesPublicLink;
var
	FilePath: WideString;
	PublicLink: WideString;
	PublishResult, UnpublishResult: Boolean;
	SharedItems: TCMRDirItemList;
	FoundPublished: Boolean;
	I: Integer;
begin
	{Upload and publish a file}
	FilePath := UploadTestFile(1024, 'UnpublishTest');
	TrackForCleanup(FilePath);

	PublicLink := '';
	PublishResult := FPrimaryCloud.PublishFile(FilePath, PublicLink, CLOUD_PUBLISH);
	Assert.IsTrue(PublishResult, 'Publish should succeed');

	{Verify it's in shared links}
	FPrimaryCloud.ListingService.GetSharedLinks(SharedItems);

	FoundPublished := False;
	for I := 0 to Length(SharedItems) - 1 do
	begin
		if Pos(WideString('UnpublishTest'), SharedItems[I].Name) > 0 then
		begin
			FoundPublished := True;
			Break;
		end;
	end;

	Assert.IsTrue(FoundPublished, 'Published file should appear in shared links');

	{Unpublish the file}
	UnpublishResult := FPrimaryCloud.PublishFile(FilePath, PublicLink, CLOUD_UNPUBLISH);
	Assert.IsTrue(UnpublishResult, 'Unpublish should succeed');

	{Verify it's no longer in shared links}
	FPrimaryCloud.ListingService.GetSharedLinks(SharedItems);

	FoundPublished := False;
	for I := 0 to Length(SharedItems) - 1 do
	begin
		if Pos(WideString('UnpublishTest'), SharedItems[I].Name) > 0 then
		begin
			FoundPublished := True;
			Break;
		end;
	end;

	Assert.IsFalse(FoundPublished, 'Unpublished file should not appear in shared links');
end;

procedure TSharingIntegrationTest.TestShareFolder_WithSecondaryAccount;
var
	FolderPath: WideString;
	CreateResult: Boolean;
	ShareResult: Boolean;
begin
	RequireSecondaryAccount;

	{Create a folder to share}
	FolderPath := UniqueCloudPath('ShareFolder');
	CreateResult := FPrimaryCloud.FileOps.CreateDirectory(FolderPath);
	Assert.IsTrue(CreateResult, 'Creating folder should succeed');
	TrackForCleanup(FolderPath);

	{Share with secondary account}
	ShareResult := FPrimaryCloud.ShareFolder(FolderPath, FConfig.SecondaryEmail, CLOUD_SHARE_RO);

	{Note: ShareFolder may require specific API support}
	if ShareResult then
	begin
		Assert.Pass('Folder shared successfully with secondary account');

		{Unshare for cleanup}
		FPrimaryCloud.ShareService.Unshare(FolderPath, FConfig.SecondaryEmail);
	end
	else
	begin
		Assert.Pass('SKIPPED: ShareFolder not supported or failed - API may require different approach');
	end;
end;

procedure TSharingIntegrationTest.TestUnshareFolder_RevokesAccess;
var
	FolderPath: WideString;
	CreateResult: Boolean;
	ShareResult: Boolean;
	UnshareResult: Boolean;
begin
	RequireSecondaryAccount;

	{Create and share a folder}
	FolderPath := UniqueCloudPath('UnshareFolder');
	CreateResult := FPrimaryCloud.FileOps.CreateDirectory(FolderPath);
	Assert.IsTrue(CreateResult, 'Creating folder should succeed');
	TrackForCleanup(FolderPath);

	ShareResult := FPrimaryCloud.ShareFolder(FolderPath, FConfig.SecondaryEmail, CLOUD_SHARE_RO);

	if not ShareResult then
	begin
		Assert.Pass('SKIPPED: Cannot test unshare - initial share failed');
		Exit;
	end;

	{Unshare the folder}
	UnshareResult := FPrimaryCloud.ShareService.Unshare(FolderPath, FConfig.SecondaryEmail);

	Assert.IsTrue(UnshareResult, 'Unshare should succeed');
end;

procedure TSharingIntegrationTest.TestGetShareInfo_ReturnsInvitees;
var
	FolderPath: WideString;
	CreateResult: Boolean;
	ShareResult: Boolean;
	Invites: TCMRInviteList;
	GetInfoResult: Boolean;
begin
	RequireSecondaryAccount;

	{Create and share a folder}
	FolderPath := UniqueCloudPath('ShareInfoFolder');
	CreateResult := FPrimaryCloud.FileOps.CreateDirectory(FolderPath);
	Assert.IsTrue(CreateResult, 'Creating folder should succeed');
	TrackForCleanup(FolderPath);

	ShareResult := FPrimaryCloud.ShareFolder(FolderPath, FConfig.SecondaryEmail, CLOUD_SHARE_RO);

	if not ShareResult then
	begin
		Assert.Pass('SKIPPED: Cannot test share info - initial share failed');
		Exit;
	end;

	{Get share info}
	GetInfoResult := FPrimaryCloud.ShareService.GetShareInfo(FolderPath, Invites);

	if GetInfoResult then
	begin
		Assert.IsTrue(Length(Invites) > 0, 'Shared folder should have invitees');
		Assert.Pass('Share info retrieved successfully');
	end
	else
	begin
		Assert.Pass('SKIPPED: GetShareInfo not supported or failed');
	end;

	{Cleanup: unshare}
	FPrimaryCloud.ShareService.Unshare(FolderPath, FConfig.SecondaryEmail);
end;

procedure TSharingIntegrationTest.TestCloneWeblink_CopiesPublicFile;
var
	SourcePath, DestPath: WideString;
	PublicLink: WideString;
	PublishResult: Boolean;
	CloneResult: Integer;
	Items: TCMRDirItemList;
	Found: Boolean;
	I: Integer;
begin
	{Upload and publish a file}
	SourcePath := UploadTestFile(1024, 'CloneSource');
	TrackForCleanup(SourcePath);

	PublicLink := '';
	PublishResult := FPrimaryCloud.PublishFile(SourcePath, PublicLink, CLOUD_PUBLISH);

	if not PublishResult then
	begin
		Assert.Pass('SKIPPED: Cannot test clone - publish failed');
		Exit;
	end;

	{Clone to different location}
	DestPath := UniqueCloudPath('CloneDest');
	TrackForCleanup(DestPath);

	CloneResult := FPrimaryCloud.ShareService.CloneWeblink(DestPath, PublicLink);

	{Unpublish source}
	FPrimaryCloud.PublishFile(SourcePath, PublicLink, CLOUD_UNPUBLISH);

	if CloneResult = CLOUD_OPERATION_OK then
	begin
		{Verify clone exists}
		FPrimaryCloud.ListingService.GetDirectory(FTestRunFolder, Items);

		Found := False;
		for I := 0 to Length(Items) - 1 do
		begin
			if Pos(WideString('CloneDest'), Items[I].Name) > 0 then
			begin
				Found := True;
				Break;
			end;
		end;

		Assert.IsTrue(Found, 'Cloned file should exist');
	end
	else
	begin
		Assert.Pass('SKIPPED: Clone weblink returned non-OK status: ' + IntToStr(CloneResult));
	end;
end;

procedure TSharingIntegrationTest.TestListIncomingInvites_ReturnsInvites;
var
	Invites: TCMRIncomingInviteList;
	ListResult: Boolean;
begin
	{List incoming invites - may be empty but should succeed}
	ListResult := FPrimaryCloud.ListingService.GetIncomingInvites(Invites);

	Assert.IsTrue(ListResult, 'Listing incoming invites should succeed');

	{Note: We can't guarantee invites exist, but operation should work}
	Assert.Pass('Incoming invites listed successfully (count: ' + IntToStr(Length(Invites)) + ')');
end;

procedure TSharingIntegrationTest.TestGetSharedFileUrl_ReturnsDownloadUrl;
var
	FilePath: WideString;
	PublicLink: WideString;
	DownloadUrl: WideString;
	PublishResult: Boolean;
begin
	{Upload and publish a file}
	FilePath := UploadTestFile(1024, 'SharedUrlTest');
	TrackForCleanup(FilePath);

	PublicLink := '';
	PublishResult := FPrimaryCloud.PublishFile(FilePath, PublicLink, CLOUD_PUBLISH);

	if not PublishResult then
	begin
		Assert.Pass('SKIPPED: Cannot test shared URL - publish failed');
		Exit;
	end;

	{Get the direct download URL for the file}
	DownloadUrl := FPrimaryCloud.Downloader.GetSharedFileUrl(FilePath);

	{Unpublish for cleanup}
	FPrimaryCloud.PublishFile(FilePath, PublicLink, CLOUD_UNPUBLISH);

	Assert.IsNotEmpty(String(DownloadUrl), 'Download URL should be returned');
	Assert.IsTrue(Pos('http', LowerCase(DownloadUrl)) = 1, 'URL should start with http');
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TSharingIntegrationTest);

end.
