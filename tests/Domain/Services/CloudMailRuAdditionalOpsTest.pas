unit CloudMailRuAdditionalOpsTest;

{Tests for TCloudMailRu additional operations:
 - Incoming links listing
 - Trashbin operations (restore, empty)
 - Folder sharing (ShareFolder)
 - Folder mounting (MountFolder, UnmountFolder)
 Note: GetUserSpace is private and not directly testable.
 Note: UnshareFolder method doesn't exist (API_FOLDER_UNSHARE not implemented).}

interface

uses
	CloudMailRu,
	CloudSettings,
	CMRDirItemList,
	CMRIncomingInviteList,
	CMRConstants,
	PLUGIN_TYPES,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	IAuthStrategyInterface,
	WindowsFileSystem,
	CloudHTTP,
	HTTPManager,
	MockCloudHTTP,
	MockHTTPManager,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework;

type
	{Testable subclass that exposes protected members}
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
	end;

	[TestFixture]
	TCloudMailRuAdditionalOpsTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TTestableCloudMailRu;
		FSettings: TCloudSettings;

		function CreateCloud(PublicAccount: Boolean = False): TTestableCloudMailRu;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{GetIncomingLinksListing tests}
		[Test]
		procedure TestGetIncomingLinksListing_Success;
		[Test]
		procedure TestGetIncomingLinksListing_Empty;
		[Test]
		procedure TestGetIncomingLinksListing_Failure;
		[Test]
		procedure TestGetIncomingLinksListing_PublicAccount_ReturnsFalse;

		{TrashbinRestore tests}
		[Test]
		procedure TestTrashbinRestore_Success;
		[Test]
		procedure TestTrashbinRestore_Failure;
		[Test]
		procedure TestTrashbinRestore_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestTrashbinRestore_ConstructsCorrectPostData;

		{TrashbinEmpty tests}
		[Test]
		procedure TestTrashbinEmpty_Success;
		[Test]
		procedure TestTrashbinEmpty_Failure;
		[Test]
		procedure TestTrashbinEmpty_PublicAccount_ReturnsFalse;

		{GetUserSpace tests - REMOVED: GetUserSpace is private, not accessible for direct testing.
		 It's called internally by LogUserSpaceInfo() which only logs, doesn't return values.
		 Consider making GetUserSpace protected if testing is needed.}

		{ShareFolder tests}
		[Test]
		procedure TestShareFolder_Success;
		[Test]
		procedure TestShareFolder_Failure;
		[Test]
		procedure TestShareFolder_ReadOnlyAccess;
		[Test]
		procedure TestShareFolder_ReadWriteAccess;

		{UnshareFolder tests - REMOVED: UnshareFolder method does not exist in TCloudMailRu.
		 The API endpoint API_FOLDER_UNSHARE exists but is not implemented.
		 Sharing is removed when all users are removed via ShareFolder or when folder is unmounted.}

		{MountFolder tests}
		[Test]
		procedure TestMountFolder_Success;
		[Test]
		procedure TestMountFolder_Failure;
		[Test]
		procedure TestMountFolder_PublicAccount_ReturnsFalse;

		{UnmountFolder tests}
		[Test]
		procedure TestUnmountFolder_Success;
		[Test]
		procedure TestUnmountFolder_WithCloneCopy;
		[Test]
		procedure TestUnmountFolder_WithoutCloneCopy;
	end;

implementation

const
	JSON_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';
	JSON_FAILURE = '{"email":"test@mail.ru","body":{"home":{"error":"invalid"}},"status":400}';

	JSON_INCOMING_LINKS_SUCCESS =
		'{"email":"test@mail.ru","body":{"list":[' +
		'{"name":"SharedFolder","size":0,"tree":"abc123","owner":{"email":"other@mail.ru","name":"Other User"}}' +
		']},"status":200}';

	JSON_INCOMING_LINKS_EMPTY =
		'{"email":"test@mail.ru","body":{"list":[]},"status":200}';

{TTestableCloudMailRu}

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

{TCloudMailRuAdditionalOpsTest}

procedure TCloudMailRuAdditionalOpsTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
end;

procedure TCloudMailRuAdditionalOpsTest.TearDown;
begin
	FCloud.Free;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCloudMailRuAdditionalOpsTest.CreateCloud(PublicAccount: Boolean): TTestableCloudMailRu;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := PublicAccount;

	Result := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create);

	Result.SetUnitedParams('api=2&access_token=test_token');
end;

{GetIncomingLinksListing tests}

procedure TCloudMailRuAdditionalOpsTest.TestGetIncomingLinksListing_Success;
var
	IncomingListing: TCMRIncomingInviteList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARED_INCOMING, True, JSON_INCOMING_LINKS_SUCCESS);

	var Success := FCloud.ListingService.GetIncomingInvites(IncomingListing);

	Assert.IsTrue(Success, 'GetIncomingLinksListing should return True on success');
	Assert.AreEqual(Integer(1), Integer(Length(IncomingListing)), 'Should have 1 incoming link');
end;

procedure TCloudMailRuAdditionalOpsTest.TestGetIncomingLinksListing_Empty;
var
	IncomingListing: TCMRIncomingInviteList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARED_INCOMING, True, JSON_INCOMING_LINKS_EMPTY);

	var Success := FCloud.ListingService.GetIncomingInvites(IncomingListing);

	Assert.IsTrue(Success, 'GetIncomingLinksListing should return True for empty list');
	Assert.AreEqual(Integer(0), Integer(Length(IncomingListing)), 'Should have 0 items');
end;

procedure TCloudMailRuAdditionalOpsTest.TestGetIncomingLinksListing_Failure;
var
	IncomingListing: TCMRIncomingInviteList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetDefaultResponse(False, '', FS_FILE_READERROR);

	var Success := FCloud.ListingService.GetIncomingInvites(IncomingListing);

	Assert.IsFalse(Success, 'GetIncomingLinksListing should return False on failure');
end;

procedure TCloudMailRuAdditionalOpsTest.TestGetIncomingLinksListing_PublicAccount_ReturnsFalse;
var
	IncomingListing: TCMRIncomingInviteList;
begin
	FCloud := CreateCloud(True);
	FMockHTTP.SetResponse(API_FOLDER_SHARED_INCOMING, True, JSON_INCOMING_LINKS_SUCCESS);

	var Success := FCloud.ListingService.GetIncomingInvites(IncomingListing);

	Assert.IsFalse(Success, 'GetIncomingLinksListing should return False for public accounts');
end;

{TrashbinRestore tests}

procedure TCloudMailRuAdditionalOpsTest.TestTrashbinRestore_Success;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_TRASHBIN_RESTORE, True, JSON_SUCCESS);

	var Success := FCloud.ListingService.TrashbinRestore('/deleted/file.txt', 12345);

	Assert.IsTrue(Success, 'TrashbinRestore should return True on success');
end;

procedure TCloudMailRuAdditionalOpsTest.TestTrashbinRestore_Failure;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_TRASHBIN_RESTORE, True, JSON_FAILURE);

	var Success := FCloud.ListingService.TrashbinRestore('/deleted/file.txt', 12345);

	Assert.IsFalse(Success, 'TrashbinRestore should return False on failure');
end;

procedure TCloudMailRuAdditionalOpsTest.TestTrashbinRestore_PublicAccount_ReturnsFalse;
begin
	FCloud := CreateCloud(True);
	FMockHTTP.SetResponse(API_TRASHBIN_RESTORE, True, JSON_SUCCESS);

	var Success := FCloud.ListingService.TrashbinRestore('/deleted/file.txt', 12345);

	Assert.IsFalse(Success, 'TrashbinRestore should return False for public accounts');
end;

procedure TCloudMailRuAdditionalOpsTest.TestTrashbinRestore_ConstructsCorrectPostData;
var
	PostedData: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_TRASHBIN_RESTORE, True, JSON_SUCCESS);

	FCloud.ListingService.TrashbinRestore('/deleted/file.txt', 12345);

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.IsTrue(Pos(String('path='), String(PostedData)) > 0, 'Should contain path parameter');
	Assert.IsTrue(Pos(String('restore_revision='), String(PostedData)) > 0, 'Should contain restore_revision parameter');
	Assert.IsTrue(Pos(String('conflict='), String(PostedData)) > 0, 'Should contain conflict parameter');
end;

{TrashbinEmpty tests}

procedure TCloudMailRuAdditionalOpsTest.TestTrashbinEmpty_Success;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_TRASHBIN_EMPTY, True, JSON_SUCCESS);

	var Success := FCloud.ListingService.TrashbinEmpty;

	Assert.IsTrue(Success, 'TrashbinEmpty should return True on success');
end;

procedure TCloudMailRuAdditionalOpsTest.TestTrashbinEmpty_Failure;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_TRASHBIN_EMPTY, True, JSON_FAILURE);

	var Success := FCloud.ListingService.TrashbinEmpty;

	Assert.IsFalse(Success, 'TrashbinEmpty should return False on failure');
end;

procedure TCloudMailRuAdditionalOpsTest.TestTrashbinEmpty_PublicAccount_ReturnsFalse;
begin
	FCloud := CreateCloud(True);
	FMockHTTP.SetResponse(API_TRASHBIN_EMPTY, True, JSON_SUCCESS);

	var Success := FCloud.ListingService.TrashbinEmpty;

	Assert.IsFalse(Success, 'TrashbinEmpty should return False for public accounts');
end;

{ShareFolder tests}

procedure TCloudMailRuAdditionalOpsTest.TestShareFolder_Success;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARE, True, JSON_SUCCESS);

	var Success := FCloud.ShareFolder('/MyFolder', 'user@mail.ru', CLOUD_SHARE_RO);

	Assert.IsTrue(Success, 'ShareFolder should return True on success');
end;

procedure TCloudMailRuAdditionalOpsTest.TestShareFolder_Failure;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARE, True, JSON_FAILURE);

	var Success := FCloud.ShareFolder('/MyFolder', 'user@mail.ru', CLOUD_SHARE_RO);

	Assert.IsFalse(Success, 'ShareFolder should return False on failure');
end;

procedure TCloudMailRuAdditionalOpsTest.TestShareFolder_ReadOnlyAccess;
var
	PostedData: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARE, True, JSON_SUCCESS);

	FCloud.ShareFolder('/MyFolder', 'user@mail.ru', CLOUD_SHARE_RO);

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.IsTrue(Pos(String(CLOUD_SHARE_ACCESS_READ_ONLY), String(PostedData)) > 0,
		'Should use read-only access string');
end;

procedure TCloudMailRuAdditionalOpsTest.TestShareFolder_ReadWriteAccess;
var
	PostedData: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARE, True, JSON_SUCCESS);

	FCloud.ShareFolder('/MyFolder', 'user@mail.ru', CLOUD_SHARE_RW);

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.IsTrue(Pos(String(CLOUD_SHARE_ACCESS_READ_WRITE), String(PostedData)) > 0,
		'Should use read-write access string');
end;

{MountFolder tests}

procedure TCloudMailRuAdditionalOpsTest.TestMountFolder_Success;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_MOUNT, True, JSON_SUCCESS);

	var Success := FCloud.ShareService.Mount('/SharedFolder', 'invite_token_123');

	Assert.IsTrue(Success, 'MountFolder should return True on success');
end;

procedure TCloudMailRuAdditionalOpsTest.TestMountFolder_Failure;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_MOUNT, True, JSON_FAILURE);

	var Success := FCloud.ShareService.Mount('/SharedFolder', 'invite_token_123');

	Assert.IsFalse(Success, 'MountFolder should return False on failure');
end;

procedure TCloudMailRuAdditionalOpsTest.TestMountFolder_PublicAccount_ReturnsFalse;
begin
	FCloud := CreateCloud(True);
	FMockHTTP.SetResponse(API_FOLDER_MOUNT, True, JSON_SUCCESS);

	var Success := FCloud.ShareService.Mount('/SharedFolder', 'invite_token_123');

	Assert.IsFalse(Success, 'MountFolder should return False for public accounts');
end;

{UnmountFolder tests}

procedure TCloudMailRuAdditionalOpsTest.TestUnmountFolder_Success;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_UNMOUNT, True, JSON_SUCCESS);

	var Success := FCloud.ShareService.Unmount('/SharedFolder', False);

	Assert.IsTrue(Success, 'UnmountFolder should return True on success');
end;

procedure TCloudMailRuAdditionalOpsTest.TestUnmountFolder_WithCloneCopy;
var
	PostedData: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_UNMOUNT, True, JSON_SUCCESS);

	FCloud.ShareService.Unmount('/SharedFolder', True);

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.IsTrue(Pos(String('clone_copy=true'), String(PostedData)) > 0,
		'Should set clone_copy=true');
end;

procedure TCloudMailRuAdditionalOpsTest.TestUnmountFolder_WithoutCloneCopy;
var
	PostedData: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_UNMOUNT, True, JSON_SUCCESS);

	FCloud.ShareService.Unmount('/SharedFolder', False);

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.IsTrue(Pos(String('clone_copy=false'), String(PostedData)) > 0,
		'Should set clone_copy=false');
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuAdditionalOpsTest);

end.
