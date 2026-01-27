unit ListingProviderTest;

{Unit tests for TListingProvider - virtual directory listing routing.
 Tests verify that the correct cloud API is called based on path type.}

interface

uses
	System.SysUtils,
	DUnitX.TestFramework,
	ListingProvider,
	CloudMailRu,
	CloudSettings,
	MockCloudHTTP,
	MockHTTPManager,
	AuthStrategy,
	WindowsFileSystem,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	RealPath,
	CloudDirItem,
	CloudDirItemList,
	CloudIncomingInviteList,
	CloudConstants,
	SettingsConstants;

type
	{Testable CloudMailRu for listing tests}
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
	end;

	[TestFixture]
	TListingProviderTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TTestableCloudMailRu;
		FProvider: IListingProvider;

		function CreateCloud: TTestableCloudMailRu;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Path type detection tests - verify TRealPath flags work correctly with provider}
		[Test]
		procedure TestTrashPath_HasTrashDirFlag;
		[Test]
		procedure TestSharedPath_HasSharedDirFlag;
		[Test]
		procedure TestInvitesPath_HasInvitesDirFlag;
		[Test]
		procedure TestNormalPath_HasNoVirtualFlags;

		{Provider instantiation tests}
		[Test]
		procedure TestCreate_ReturnsValidInstance;
		[Test]
		procedure TestCreate_ImplementsInterface;

		{FetchListing tests - verify correct cloud API is called}
		[Test]
		procedure TestFetchListing_TrashPath_CallsTrashbinAPI;
		[Test]
		procedure TestFetchListing_SharedPath_CallsSharedLinksAPI;
		[Test]
		procedure TestFetchListing_InvitesPath_CallsIncomingLinksAPI;
		[Test]
		procedure TestFetchListing_NormalPath_CallsDirListingAPI;
		[Test]
		procedure TestFetchListing_TrashPath_ReturnsSuccess;
		[Test]
		procedure TestFetchListing_SharedPath_ReturnsSuccess;
		[Test]
		procedure TestFetchListing_InvitesPath_ReturnsSuccess;
		[Test]
		procedure TestFetchListing_NormalPath_ReturnsSuccess;
		[Test]
		procedure TestFetchListing_APIFailure_ReturnsFalse;
		[Test]
		procedure TestFetchListing_PopulatesListing;
	end;

implementation

const
	{Sample JSON responses for different listing types}
	JSON_TRASHBIN_LISTING =
		'{"email":"test@mail.ru","body":{"list":[' +
		'{"name":"deleted.txt","size":100,"kind":"file","mtime":1234567890}' +
		']},"status":200}';

	JSON_SHARED_LINKS_LISTING =
		'{"email":"test@mail.ru","body":{"list":[' +
		'{"name":"shared.txt","size":200,"kind":"file","weblink":"abc123"}' +
		']},"status":200}';

	JSON_INCOMING_INVITES_LISTING =
		'{"email":"test@mail.ru","body":{"list":[' +
		'{"name":"invite_folder","tree":"shared_tree","access":"read_only","invite_token":"token123",' +
		'"owner":{"email":"owner@mail.ru","name":"Owner"}}' +
		']},"status":200}';

	JSON_DIR_LISTING =
		'{"email":"test@mail.ru","body":{"list":[' +
		'{"name":"file.txt","size":300,"kind":"file","mtime":1234567890},' +
		'{"name":"subfolder","kind":"folder","mtime":1234567890}' +
		']},"status":200}';

	JSON_EMPTY_LISTING =
		'{"email":"test@mail.ru","body":{"list":[]},"status":200}';

	JSON_FAILURE =
		'{"email":"test@mail.ru","body":{"home":{"error":"not_exists"}},"status":400}';

{TTestableCloudMailRu}

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

{TListingProviderTest}

procedure TListingProviderTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
	FProvider := TListingProvider.Create;
end;

procedure TListingProviderTest.TearDown;
begin
	FProvider := nil;
	FCloud.Free;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TListingProviderTest.CreateCloud: TTestableCloudMailRu;
var
	Settings: TCloudSettings;
begin
	Settings := Default(TCloudSettings);
	Result := TTestableCloudMailRu.Create(
		Settings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create);
	Result.SetUnitedParams('api=2&access_token=test_token');
end;

{Path type detection tests}

procedure TListingProviderTest.TestTrashPath_HasTrashDirFlag;
var
	Path: TRealPath;
begin
	Path.FromPath('\account' + TrashPostfix + '\somefile');
	Assert.IsTrue(Path.trashDir, 'Trash path should have trashDir flag');
	Assert.IsFalse(Path.sharedDir, 'Trash path should not have sharedDir flag');
	Assert.IsFalse(Path.invitesDir, 'Trash path should not have invitesDir flag');
end;

procedure TListingProviderTest.TestSharedPath_HasSharedDirFlag;
var
	Path: TRealPath;
begin
	Path.FromPath('\account' + SharedPostfix + '\somefile');
	Assert.IsFalse(Path.trashDir, 'Shared path should not have trashDir flag');
	Assert.IsTrue(Path.sharedDir, 'Shared path should have sharedDir flag');
	Assert.IsFalse(Path.invitesDir, 'Shared path should not have invitesDir flag');
end;

procedure TListingProviderTest.TestInvitesPath_HasInvitesDirFlag;
var
	Path: TRealPath;
begin
	Path.FromPath('\account' + InvitesPostfix + '\somefile');
	Assert.IsFalse(Path.trashDir, 'Invites path should not have trashDir flag');
	Assert.IsFalse(Path.sharedDir, 'Invites path should not have sharedDir flag');
	Assert.IsTrue(Path.invitesDir, 'Invites path should have invitesDir flag');
end;

procedure TListingProviderTest.TestNormalPath_HasNoVirtualFlags;
var
	Path: TRealPath;
begin
	Path.FromPath('\account\normalfolder\file.txt');
	Assert.IsFalse(Path.trashDir, 'Normal path should not have trashDir flag');
	Assert.IsFalse(Path.sharedDir, 'Normal path should not have sharedDir flag');
	Assert.IsFalse(Path.invitesDir, 'Normal path should not have invitesDir flag');
end;

{Provider instantiation tests}

procedure TListingProviderTest.TestCreate_ReturnsValidInstance;
begin
	Assert.IsNotNull(FProvider, 'Provider should not be nil');
end;

procedure TListingProviderTest.TestCreate_ImplementsInterface;
var
	Intf: IListingProvider;
begin
	Assert.IsTrue(Supports(TListingProvider.Create, IListingProvider, Intf),
		'TListingProvider should implement IListingProvider');
end;

{FetchListing tests}

procedure TListingProviderTest.TestFetchListing_TrashPath_CallsTrashbinAPI;
var
	Path: TRealPath;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_TRASHBIN, True, JSON_TRASHBIN_LISTING);
	Path.FromPath('\account' + TrashPostfix);

	FProvider.FetchListing(FCloud, Path, DirListing, InviteListing);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_TRASHBIN), 'Should call trashbin API');
end;

procedure TListingProviderTest.TestFetchListing_SharedPath_CallsSharedLinksAPI;
var
	Path: TRealPath;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARED_LINKS, True, JSON_SHARED_LINKS_LISTING);
	Path.FromPath('\account' + SharedPostfix);

	FProvider.FetchListing(FCloud, Path, DirListing, InviteListing);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FOLDER_SHARED_LINKS), 'Should call shared links API');
end;

procedure TListingProviderTest.TestFetchListing_InvitesPath_CallsIncomingLinksAPI;
var
	Path: TRealPath;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARED_INCOMING, True, JSON_INCOMING_INVITES_LISTING);
	Path.FromPath('\account' + InvitesPostfix);

	FProvider.FetchListing(FCloud, Path, DirListing, InviteListing);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FOLDER_SHARED_INCOMING), 'Should call incoming links API');
end;

procedure TListingProviderTest.TestFetchListing_NormalPath_CallsDirListingAPI;
var
	Path: TRealPath;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING);
	Path.FromPath('\account\myfolder');

	FProvider.FetchListing(FCloud, Path, DirListing, InviteListing);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FOLDER), 'Should call folder API');
end;

procedure TListingProviderTest.TestFetchListing_TrashPath_ReturnsSuccess;
var
	Path: TRealPath;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_TRASHBIN, True, JSON_TRASHBIN_LISTING);
	Path.FromPath('\account' + TrashPostfix);

	Success := FProvider.FetchListing(FCloud, Path, DirListing, InviteListing);

	Assert.IsTrue(Success, 'FetchListing should return True on success');
end;

procedure TListingProviderTest.TestFetchListing_SharedPath_ReturnsSuccess;
var
	Path: TRealPath;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARED_LINKS, True, JSON_SHARED_LINKS_LISTING);
	Path.FromPath('\account' + SharedPostfix);

	Success := FProvider.FetchListing(FCloud, Path, DirListing, InviteListing);

	Assert.IsTrue(Success, 'FetchListing should return True on success');
end;

procedure TListingProviderTest.TestFetchListing_InvitesPath_ReturnsSuccess;
var
	Path: TRealPath;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARED_INCOMING, True, JSON_INCOMING_INVITES_LISTING);
	Path.FromPath('\account' + InvitesPostfix);

	Success := FProvider.FetchListing(FCloud, Path, DirListing, InviteListing);

	Assert.IsTrue(Success, 'FetchListing should return True on success');
end;

procedure TListingProviderTest.TestFetchListing_NormalPath_ReturnsSuccess;
var
	Path: TRealPath;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING);
	Path.FromPath('\account\myfolder');

	Success := FProvider.FetchListing(FCloud, Path, DirListing, InviteListing);

	Assert.IsTrue(Success, 'FetchListing should return True on success');
end;

procedure TListingProviderTest.TestFetchListing_APIFailure_ReturnsFalse;
var
	Path: TRealPath;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, False, ''); {HTTP failure}
	Path.FromPath('\account\myfolder');

	Success := FProvider.FetchListing(FCloud, Path, DirListing, InviteListing);

	Assert.IsFalse(Success, 'FetchListing should return False on HTTP failure');
end;

procedure TListingProviderTest.TestFetchListing_PopulatesListing;
var
	Path: TRealPath;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING);
	Path.FromPath('\account\myfolder');

	FProvider.FetchListing(FCloud, Path, DirListing, InviteListing);

	Assert.AreEqual(2, Integer(Length(DirListing)), 'Should populate listing with 2 items');
	Assert.AreEqual('file.txt', DirListing[0].name);
	Assert.AreEqual('subfolder', DirListing[1].name);
end;

initialization
	TDUnitX.RegisterTestFixture(TListingProviderTest);

end.
