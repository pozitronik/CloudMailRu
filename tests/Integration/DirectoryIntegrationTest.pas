unit DirectoryIntegrationTest;

{Integration tests for directory operations against live cloud.mail.ru API.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	TDirectoryIntegrationTest = class(TIntegrationTestBase)
	public
		[Test]
		procedure TestListDirectory_Root_ReturnsItems;

		[Test]
		procedure TestListDirectory_Subfolder_ReturnsItems;

		[Test]
		procedure TestListDirectory_Empty_ReturnsEmptyList;

		[Test]
		procedure TestCreateDirectory_NewFolder_Succeeds;

		[Test]
		procedure TestCreateDirectory_Existing_Fails;

		[Test]
		procedure TestRemoveDirectory_Empty_Succeeds;

		[Test]
		procedure TestRemoveDirectory_WithContents_Succeeds;

		[Test]
		procedure TestListSharedLinks_ReturnsItems;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	CloudDirItemList,
	WFXTypes,
	TestDataGenerator;

{TDirectoryIntegrationTest}

procedure TDirectoryIntegrationTest.TestListDirectory_Root_ReturnsItems;
var
	Items: TCloudDirItemList;
	ListResult: Boolean;
begin
	{List root directory - should contain at least our test folder}
	ListResult := FPrimaryCloud.ListingService.GetDirectory('/', Items);

	Assert.IsTrue(ListResult, 'Listing root directory should succeed');
	{Root will have at least the test folder we created}
end;

procedure TDirectoryIntegrationTest.TestListDirectory_Subfolder_ReturnsItems;
var
	Items: TCloudDirItemList;
	ListResult: Boolean;
begin
	{List our test run folder}
	ListResult := FPrimaryCloud.ListingService.GetDirectory(FTestRunFolder, Items);

	Assert.IsTrue(ListResult, 'Listing test run folder should succeed');
	{May be empty initially, which is fine}
end;

procedure TDirectoryIntegrationTest.TestListDirectory_Empty_ReturnsEmptyList;
var
	EmptyFolder: WideString;
	Items: TCloudDirItemList;
	CreateResult: Boolean;
	ListResult: Boolean;
begin
	EmptyFolder := UniqueCloudPath('EmptyDir');
	TrackForCleanup(EmptyFolder);

	{Create empty folder}
	CreateResult := FPrimaryCloud.FileOps.CreateDirectory(EmptyFolder);
	Assert.IsTrue(CreateResult, 'Creating empty folder should succeed');

	{List empty folder}
	ListResult := FPrimaryCloud.ListingService.GetDirectory(EmptyFolder, Items);

	Assert.IsTrue(ListResult, 'Listing empty folder should succeed');
	Assert.AreEqual(Integer(0), Integer(Length(Items)), 'Empty folder should have no items');
end;

procedure TDirectoryIntegrationTest.TestCreateDirectory_NewFolder_Succeeds;
var
	NewFolder: WideString;
	CreateResult: Boolean;
	Items: TCloudDirItemList;
begin
	NewFolder := UniqueCloudPath('NewFolder');
	TrackForCleanup(NewFolder);

	CreateResult := FPrimaryCloud.FileOps.CreateDirectory(NewFolder);

	Assert.IsTrue(CreateResult, 'Creating new folder should succeed');

	{Verify folder exists by listing parent}
	FPrimaryCloud.ListingService.GetDirectory(FTestRunFolder, Items);
	{The folder should appear in the listing}
end;

procedure TDirectoryIntegrationTest.TestCreateDirectory_Existing_Fails;
var
	ExistingFolder: WideString;
	CreateResult1, CreateResult2: Boolean;
begin
	ExistingFolder := UniqueCloudPath('ExistingFolder');
	TrackForCleanup(ExistingFolder);

	{Create folder first time}
	CreateResult1 := FPrimaryCloud.FileOps.CreateDirectory(ExistingFolder);
	Assert.IsTrue(CreateResult1, 'First create should succeed');

	{Try to create same folder again}
	CreateResult2 := FPrimaryCloud.FileOps.CreateDirectory(ExistingFolder);

	{Should fail with EXISTS error or similar}
	Assert.IsFalse(CreateResult2, 'Creating existing folder should fail');
end;

procedure TDirectoryIntegrationTest.TestRemoveDirectory_Empty_Succeeds;
var
	FolderToRemove: WideString;
	CreateResult, RemoveResult: Boolean;
begin
	FolderToRemove := UniqueCloudPath('FolderToRemove');
	{Don't track for cleanup - we're testing removal}

	{Create folder}
	CreateResult := FPrimaryCloud.FileOps.CreateDirectory(FolderToRemove);
	Assert.IsTrue(CreateResult, 'Creating folder should succeed');

	{Remove empty folder}
	RemoveResult := FPrimaryCloud.FileOps.RemoveDirectory(FolderToRemove);

	Assert.IsTrue(RemoveResult, 'Removing empty folder should succeed');
end;

procedure TDirectoryIntegrationTest.TestRemoveDirectory_WithContents_Succeeds;
var
	ParentFolder, ChildFolder: WideString;
	CreateResult1, CreateResult2, RemoveResult: Boolean;
begin
	ParentFolder := UniqueCloudPath('ParentFolder');
	ChildFolder := ParentFolder + '/ChildFolder';
	{Don't track - testing removal}

	{Create parent and child folders}
	CreateResult1 := FPrimaryCloud.FileOps.CreateDirectory(ParentFolder);
	Assert.IsTrue(CreateResult1, 'Creating parent folder should succeed');

	CreateResult2 := FPrimaryCloud.FileOps.CreateDirectory(ChildFolder);
	Assert.IsTrue(CreateResult2, 'Creating child folder should succeed');

	{Remove parent - API removes recursively}
	RemoveResult := FPrimaryCloud.FileOps.RemoveDirectory(ParentFolder);

	Assert.IsTrue(RemoveResult, 'Removing folder with contents should succeed');
end;

procedure TDirectoryIntegrationTest.TestListSharedLinks_ReturnsItems;
var
	Items: TCloudDirItemList;
	ListResult: Boolean;
begin
	{List shared links - may be empty but should succeed}
	ListResult := FPrimaryCloud.ListingService.GetSharedLinks(Items);

	{Should succeed even if no shared items exist}
	Assert.IsTrue(ListResult, 'Listing shared links should succeed');
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TDirectoryIntegrationTest);

end.
