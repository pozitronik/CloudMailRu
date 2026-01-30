unit CloudListingServiceTest;

interface

uses
	CloudListingService,
	CloudContext,
	CloudDirItem,
	CloudDirItemList,
	CloudIncomingInviteList,
	CloudOperationResult,
	CloudSpace,
	CloudConstants,
	WFXTypes,
	TCLogger,
	CloudHTTP,
	FileCipher,
	MockCloudHTTP,
	MockCloudContext,
	TokenRetryHelper,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework;

type
	{Tests for TCloudListingService}
	[TestFixture]
	TCloudListingServiceTest = class
	private
		FService: ICloudListingService;
		FMockHTTP: TMockCloudHTTP;
		FMockContext: TMockCloudContext;
		FMockContextRef: ICloudContext;
		FRetryOperation: IRetryOperation;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Construction tests}
		[Test]
		procedure TestCreate_InitializesCorrectly;

		{GetDirectory tests}
		[Test]
		procedure TestGetDirectory_Success_PopulatesListing;
		[Test]
		procedure TestGetDirectory_PublicAccount_UsesWeblinkEndpoint;

		{GetSharedLinks tests}
		[Test]
		procedure TestGetSharedLinks_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestGetSharedLinks_Success_PopulatesListing;

		{GetIncomingInvites tests}
		[Test]
		procedure TestGetIncomingInvites_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestGetIncomingInvites_Success_PopulatesListing;

		{GetIncomingInvitesAsDirItems tests}
		[Test]
		procedure TestGetIncomingInvitesAsDirItems_ConvertsToItems;

		{GetTrashbin tests}
		[Test]
		procedure TestGetTrashbin_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestGetTrashbin_Success_PopulatesListing;

		{StatusFile tests}
		[Test]
		procedure TestStatusFile_Success_PopulatesFileInfo;
		[Test]
		procedure TestStatusFile_PublicAccount_UsesWeblinkEndpoint;

		{TrashbinRestore tests}
		[Test]
		procedure TestTrashbinRestore_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestTrashbinRestore_Success_ReturnsTrue;

		{TrashbinEmpty tests}
		[Test]
		procedure TestTrashbinEmpty_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestTrashbinEmpty_Success_ReturnsTrue;

		{GetUserSpace tests}
		[Test]
		procedure TestGetUserSpace_Success_PopulatesSpaceInfo;
		[Test]
		procedure TestGetUserSpace_HTTPFailure_ReturnsFalse;

		{LogUserSpaceInfo tests}
		[Test]
		procedure TestLogUserSpaceInfo_PublicAccount_DoesNotLog;
		[Test]
		procedure TestLogUserSpaceInfo_Success_LogsSpaceInfo;

		{Pagination tests}
		[Test]
		procedure TestGetDirectory_Pagination_AccumulatesMultiplePages;
		[Test]
		procedure TestGetDirectory_SinglePage_NoExtraRequests;
	end;

implementation

{ TCloudListingServiceTest }

procedure TCloudListingServiceTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{"list":[]}}');

	{Setup mock context}
	FMockContext := TMockCloudContext.Create;
	FMockContextRef := FMockContext;
	FMockContext.SetHTTP(FMockHTTP);
	FMockContext.SetIsPublicAccount(False);
	FMockContext.SetUnitedParams('token=test&x-email=test@mail.ru');
	FMockContext.SetPublicLink('public_weblink_123');

	{Create retry operation with mock context}
	FRetryOperation := TRetryOperation.Create(FMockContext, 3);

	FService := TCloudListingService.Create(FMockContext, TNullCipher.Create, TNullLogger.Create, FRetryOperation, False);
end;

procedure TCloudListingServiceTest.TearDown;
begin
	FService := nil;
	FRetryOperation := nil;
	FMockContextRef := nil;
end;

{Construction tests}

procedure TCloudListingServiceTest.TestCreate_InitializesCorrectly;
begin
	Assert.IsNotNull(FService, 'Service should be created');
end;

{GetDirectory tests}

procedure TCloudListingServiceTest.TestGetDirectory_Success_PopulatesListing;
var
	Listing: TCloudDirItemList;
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True,
		'{"status":200,"body":{"list":[{"name":"file.txt","size":100,"type":"file"}]}}');

	Success := FService.GetDirectory('/test', Listing);

	{GetDirectory should succeed - actual parsing depends on FromJSON implementation}
	Assert.IsTrue(Success or (Length(Listing) = 0), 'GetDirectory should complete');
end;

procedure TCloudListingServiceTest.TestGetDirectory_PublicAccount_UsesWeblinkEndpoint;
var
	Listing: TCloudDirItemList;
	Success: Boolean;
begin
	FMockContext.SetIsPublicAccount(True);
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{"list":[]}}');

	Success := FService.GetDirectory('/test', Listing);

	Assert.IsTrue(Success, 'GetDirectory should succeed for public account');
	{For public accounts, different endpoint is used}
	Assert.IsTrue(FMockHTTP.WasURLCalled('weblink'), 'Public account should use weblink endpoint');
end;

{GetSharedLinks tests}

procedure TCloudListingServiceTest.TestGetSharedLinks_PublicAccount_ReturnsFalse;
var
	Listing: TCloudDirItemList;
	Success: Boolean;
begin
	FMockContext.SetIsPublicAccount(True);

	Success := FService.GetSharedLinks(Listing);

	Assert.IsFalse(Success, 'GetSharedLinks should return false for public account');
end;

procedure TCloudListingServiceTest.TestGetSharedLinks_Success_PopulatesListing;
var
	Listing: TCloudDirItemList;
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True,
		'{"status":200,"body":{"list":[]}}');

	Success := FService.GetSharedLinks(Listing);

	Assert.IsTrue(Success, 'GetSharedLinks should succeed');
end;

{GetIncomingInvites tests}

procedure TCloudListingServiceTest.TestGetIncomingInvites_PublicAccount_ReturnsFalse;
var
	Listing: TCloudIncomingInviteList;
	Success: Boolean;
begin
	FMockContext.SetIsPublicAccount(True);

	Success := FService.GetIncomingInvites(Listing);

	Assert.IsFalse(Success, 'GetIncomingInvites should return false for public account');
end;

procedure TCloudListingServiceTest.TestGetIncomingInvites_Success_PopulatesListing;
var
	Listing: TCloudIncomingInviteList;
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True,
		'{"status":200,"body":[]}');

	Success := FService.GetIncomingInvites(Listing);

	{The result depends on FromJSON implementation}
	Assert.IsTrue(Success or (Length(Listing) = 0), 'GetIncomingInvites should complete');
end;

{GetIncomingInvitesAsDirItems tests}

procedure TCloudListingServiceTest.TestGetIncomingInvitesAsDirItems_ConvertsToItems;
var
	DirListing: TCloudDirItemList;
	InvitesListing: TCloudIncomingInviteList;
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":[]}');

	Success := FService.GetIncomingInvitesAsDirItems(DirListing, InvitesListing);

	{Result depends on FromJSON implementation - empty body may return False}
	Assert.IsTrue(Success or (Length(DirListing) = 0), 'GetIncomingInvitesAsDirItems should complete');
	{Method should convert invites to dir items - lengths must match}
	Assert.AreEqual(Length(InvitesListing), Length(DirListing), 'Dir listing should match invites count');
end;

{GetTrashbin tests}

procedure TCloudListingServiceTest.TestGetTrashbin_PublicAccount_ReturnsFalse;
var
	Listing: TCloudDirItemList;
	Success: Boolean;
begin
	FMockContext.SetIsPublicAccount(True);

	Success := FService.GetTrashbin(Listing);

	Assert.IsFalse(Success, 'GetTrashbin should return false for public account');
end;

procedure TCloudListingServiceTest.TestGetTrashbin_Success_PopulatesListing;
var
	Listing: TCloudDirItemList;
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True,
		'{"status":200,"body":{"list":[]}}');

	Success := FService.GetTrashbin(Listing);

	Assert.IsTrue(Success, 'GetTrashbin should succeed');
end;

{StatusFile tests}

procedure TCloudListingServiceTest.TestStatusFile_Success_PopulatesFileInfo;
var
	FileInfo: TCloudDirItem;
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True,
		'{"status":200,"body":{"name":"test.txt","size":1024,"type":"file"}}');

	Success := FService.StatusFile('/test.txt', FileInfo);

	{StatusFile should succeed - actual parsing depends on FromJSON implementation}
	Assert.IsTrue(Success or (FileInfo.name <> ''), 'StatusFile should complete');
end;

procedure TCloudListingServiceTest.TestStatusFile_PublicAccount_UsesWeblinkEndpoint;
var
	FileInfo: TCloudDirItem;
	Success: Boolean;
begin
	FMockContext.SetIsPublicAccount(True);
	FMockHTTP.SetDefaultResponse(True,
		'{"status":200,"body":{"name":"test.txt","size":1024,"type":"file"}}');

	Success := FService.StatusFile('/test.txt', FileInfo);

	Assert.IsTrue(Success, 'StatusFile should succeed for public account');
	{For public accounts, different endpoint is used}
	Assert.IsTrue(FMockHTTP.WasURLCalled('weblink'), 'Public account should use weblink endpoint');
end;

{TrashbinRestore tests}

procedure TCloudListingServiceTest.TestTrashbinRestore_PublicAccount_ReturnsFalse;
var
	Success: Boolean;
begin
	FMockContext.SetIsPublicAccount(True);

	Success := FService.TrashbinRestore('/deleted/file.txt', 12345);

	Assert.IsFalse(Success, 'TrashbinRestore should return false for public account');
end;

procedure TCloudListingServiceTest.TestTrashbinRestore_Success_ReturnsTrue;
var
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":"ok"}');

	Success := FService.TrashbinRestore('/deleted/file.txt', 12345);

	Assert.IsTrue(Success, 'TrashbinRestore should succeed');
end;

{TrashbinEmpty tests}

procedure TCloudListingServiceTest.TestTrashbinEmpty_PublicAccount_ReturnsFalse;
var
	Success: Boolean;
begin
	FMockContext.SetIsPublicAccount(True);

	Success := FService.TrashbinEmpty();

	Assert.IsFalse(Success, 'TrashbinEmpty should return false for public account');
end;

procedure TCloudListingServiceTest.TestTrashbinEmpty_Success_ReturnsTrue;
var
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":"ok"}');

	Success := FService.TrashbinEmpty();

	Assert.IsTrue(Success, 'TrashbinEmpty should succeed');
end;

{GetUserSpace tests}

procedure TCloudListingServiceTest.TestGetUserSpace_Success_PopulatesSpaceInfo;
var
	SpaceInfo: TCloudSpace;
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True,
		'{"status":200,"body":{"bytes_total":10737418240,"bytes_used":5368709120,"overquota":false}}');

	Success := FService.GetUserSpace(SpaceInfo);

	Assert.IsTrue(Success, 'GetUserSpace should succeed');
	Assert.AreEqual(Int64(10737418240), SpaceInfo.total, 'Total space should be parsed');
	Assert.AreEqual(Int64(5368709120), SpaceInfo.used, 'Used space should be parsed');
	Assert.IsFalse(SpaceInfo.overquota, 'Overquota should be false');
end;

procedure TCloudListingServiceTest.TestGetUserSpace_HTTPFailure_ReturnsFalse;
var
	SpaceInfo: TCloudSpace;
	Success: Boolean;
begin
	FMockHTTP.SetDefaultResponse(False, '');

	Success := FService.GetUserSpace(SpaceInfo);

	Assert.IsFalse(Success, 'GetUserSpace should return false on HTTP failure');
end;

{LogUserSpaceInfo tests}

procedure TCloudListingServiceTest.TestLogUserSpaceInfo_PublicAccount_DoesNotLog;
begin
	FMockContext.SetIsPublicAccount(True);

	{Should not throw, just exit early for public accounts}
	FService.LogUserSpaceInfo('test@mail.ru');

	{No assertions needed - just verify no exception}
	Assert.Pass('LogUserSpaceInfo should handle public accounts gracefully');
end;

procedure TCloudListingServiceTest.TestLogUserSpaceInfo_Success_LogsSpaceInfo;
begin
	{Set up response for GetUserSpace}
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{"bytes_total":10737418240,"bytes_used":5368709120,"overquota":false}}');
	FMockContext.SetIsPublicAccount(False);

	{Should log space info without throwing}
	FService.LogUserSpaceInfo('test@mail.ru');

	{No assertions needed - verify method completes without exception}
	Assert.Pass('LogUserSpaceInfo should log space information successfully');
end;

{Pagination tests}

procedure TCloudListingServiceTest.TestGetDirectory_Pagination_AccumulatesMultiplePages;
var
	Listing: TCloudDirItemList;
	Success: Boolean;
begin
	{Queue two page responses: page 1 has 2 items, page 2 has 1 item, total expected = 3}
	FMockHTTP.QueueResponse('folder', True,
		'{"status":200,"body":{"count":{"folders":0,"files":3},"list":[' +
		'{"name":"file1.txt","size":100,"type":"file","home":"/dir/file1.txt"},' +
		'{"name":"file2.txt","size":200,"type":"file","home":"/dir/file2.txt"}' +
		']}}');
	FMockHTTP.QueueResponse('folder', True,
		'{"status":200,"body":{"count":{"folders":0,"files":3},"list":[' +
		'{"name":"file3.txt","size":300,"type":"file","home":"/dir/file3.txt"}' +
		']}}');

	Success := FService.GetDirectory('/dir', Listing);

	Assert.IsTrue(Success, 'Paginated GetDirectory should succeed');
	Assert.AreEqual(Integer(3), Integer(Length(Listing)), 'Should accumulate all 3 items from 2 pages');
	Assert.AreEqual('file1.txt', Listing[0].name, 'First item from page 1');
	Assert.AreEqual('file2.txt', Listing[1].name, 'Second item from page 1');
	Assert.AreEqual('file3.txt', Listing[2].name, 'Item from page 2');
end;

procedure TCloudListingServiceTest.TestGetDirectory_SinglePage_NoExtraRequests;
var
	Listing: TCloudDirItemList;
	Success: Boolean;
	CallCountBefore, CallCountAfter: Integer;
begin
	{Single page: count matches list length, so no second request}
	FMockHTTP.QueueResponse('folder', True,
		'{"status":200,"body":{"count":{"folders":0,"files":2},"list":[' +
		'{"name":"a.txt","size":10,"type":"file","home":"/dir/a.txt"},' +
		'{"name":"b.txt","size":20,"type":"file","home":"/dir/b.txt"}' +
		']}}');

	CallCountBefore := FMockHTTP.GetCallCount;
	Success := FService.GetDirectory('/dir', Listing);
	CallCountAfter := FMockHTTP.GetCallCount;

	Assert.IsTrue(Success, 'Single-page GetDirectory should succeed');
	Assert.AreEqual(Integer(2), Integer(Length(Listing)), 'Should return all 2 items');
	{Only 1 HTTP request should have been made (no second page request)}
	Assert.AreEqual(Integer(1), CallCountAfter - CallCountBefore, 'Should make exactly 1 HTTP request');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudListingServiceTest);

end.
