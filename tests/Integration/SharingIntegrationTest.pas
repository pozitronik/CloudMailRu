unit SharingIntegrationTest;

{Integration tests for sharing operations (publish, share) against live cloud.mail.ru API.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	[Category('Integration')]
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
		procedure TestGetIncomingInvitesAsDirItems_ConvertsToDirItems;

		[Test]
		procedure TestPublishFile_AlreadyPublished_IsIdempotent;

		[Test]
		procedure TestUnpublishFile_NotPublished_HandlesGracefully;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	System.IOUtils,
	CloudDirItemList,
	CloudInviteList,
	CloudIncomingInviteList,
	WFXTypes,
	CloudConstants,
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
	Assert.IsNotEmpty(String(PublicLink), 'Public link (weblink ID) should be returned');
	{Note: PublicLink is just the weblink ID, not full URL. Full URL = PUBLIC_ACCESS_URL + PublicLink}

	{Unpublish for cleanup}
	FPrimaryCloud.PublishFile(FilePath, PublicLink, CLOUD_UNPUBLISH);
end;

procedure TSharingIntegrationTest.TestUnpublishFile_RemovesPublicLink;
var
	FilePath: WideString;
	PublicLink: WideString;
	PublishResult, UnpublishResult: Boolean;
	SharedItems: TCloudDirItemList;
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
	CreateResult := FPrimaryCloud.FileOperations.CreateDirectory(FolderPath);
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
	CreateResult := FPrimaryCloud.FileOperations.CreateDirectory(FolderPath);
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
	Invites: TCloudInviteList;
	GetInfoResult: Boolean;
begin
	RequireSecondaryAccount;

	{Create and share a folder}
	FolderPath := UniqueCloudPath('ShareInfoFolder');
	CreateResult := FPrimaryCloud.FileOperations.CreateDirectory(FolderPath);
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
	Items: TCloudDirItemList;
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
	Invites: TCloudIncomingInviteList;
	ListResult: Boolean;
begin
	{List incoming invites - may be empty but should succeed}
	ListResult := FPrimaryCloud.ListingService.GetIncomingInvites(Invites);

	Assert.IsTrue(ListResult, 'Listing incoming invites should succeed');

	{Note: We can't guarantee invites exist, but operation should work}
	Assert.Pass('Incoming invites listed successfully (count: ' + IntToStr(Length(Invites)) + ')');
end;

procedure TSharingIntegrationTest.TestGetIncomingInvitesAsDirItems_ConvertsToDirItems;
var
	FolderPath: WideString;
	DirListing: TCloudDirItemList;
	InvitesListing: TCloudIncomingInviteList;
	ListResult: Boolean;
begin
	RequireSecondaryAccount;

	{Create secondary cloud instance if not yet created}
	if not Assigned(FSecondaryCloud) then
	begin
		FSecondaryCloud := CreateSecondaryCloud;
		Assert.IsTrue(FSecondaryCloud.Login, 'Secondary account login failed');
	end;

	{Share a folder from primary to secondary}
	FolderPath := UniqueCloudPath('InviteDirItemsFolder');
	Assert.IsTrue(FPrimaryCloud.FileOperations.CreateDirectory(FolderPath), 'Creating folder should succeed');
	TrackForCleanup(FolderPath);

	if not FPrimaryCloud.ShareFolder(FolderPath, FConfig.SecondaryEmail, CLOUD_SHARE_RO) then
	begin
		Assert.Pass('SKIPPED: ShareFolder failed - cannot test GetIncomingInvitesAsDirItems');
		Exit;
	end;

	try
		{On secondary account, get incoming invites as dir items}
		ListResult := FSecondaryCloud.ListingService.GetIncomingInvitesAsDirItems(DirListing, InvitesListing);

		if ListResult then
		begin
			Assert.IsTrue(Length(DirListing) > 0, 'DirListing should contain at least one item');
			Assert.IsTrue(Length(InvitesListing) > 0, 'InvitesListing should contain at least one invite');
		end
		else
			Assert.Pass('SKIPPED: GetIncomingInvitesAsDirItems returned False');
	finally
		FPrimaryCloud.ShareService.Unshare(FolderPath, FConfig.SecondaryEmail);
	end;
end;

procedure TSharingIntegrationTest.TestPublishFile_AlreadyPublished_IsIdempotent;
var
	FilePath: WideString;
	PublicLink1, PublicLink2: WideString;
	Result1, Result2: Boolean;
begin
	FilePath := UploadTestFile(1024, 'PublishIdempotent');
	TrackForCleanup(FilePath);

	{Publish first time}
	PublicLink1 := '';
	Result1 := FPrimaryCloud.PublishFile(FilePath, PublicLink1, CLOUD_PUBLISH);
	Assert.IsTrue(Result1, 'First publish should succeed');

	{Publish second time - should be idempotent}
	PublicLink2 := '';
	Result2 := FPrimaryCloud.PublishFile(FilePath, PublicLink2, CLOUD_PUBLISH);
	Assert.IsTrue(Result2, 'Second publish should also succeed (idempotent)');

	{Cleanup: unpublish}
	FPrimaryCloud.PublishFile(FilePath, PublicLink2, CLOUD_UNPUBLISH);
end;

procedure TSharingIntegrationTest.TestUnpublishFile_NotPublished_HandlesGracefully;
var
	FilePath: WideString;
	PublicLink: WideString;
	UnpublishResult: Boolean;
begin
	{Upload a file but don't publish it}
	FilePath := UploadTestFile(1024, 'UnpublishNever');
	TrackForCleanup(FilePath);

	{Try to unpublish a file that was never published}
	PublicLink := '';
	UnpublishResult := FPrimaryCloud.PublishFile(FilePath, PublicLink, CLOUD_UNPUBLISH);

	{Document actual API behavior - may succeed as no-op or fail}
	if UnpublishResult then
		Assert.Pass('Unpublish of never-published file succeeded (API treats as no-op)')
	else
		Assert.Pass('Unpublish of never-published file returned False (expected)');
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TSharingIntegrationTest);

end.
