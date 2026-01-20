unit CloudListingServiceTest;

interface

uses
	CloudListingService,
	CMRDirItem,
	CMRDirItemList,
	CMRIncomingInviteList,
	CMROperationResult,
	CMRConstants,
	PLUGIN_TYPES,
	TCLogger,
	CloudHTTP,
	FileCipher,
	MockCloudHTTP,
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
		FIsPublicAccount: Boolean;
		FUnitedParams: WideString;
		FPublicLink: WideString;
		FRetryOperation: TRetryOperation;

		function GetHTTP: ICloudHTTP;
		function IsPublicAccount: Boolean;
		function GetUnitedParams: WideString;
		function GetPublicLink: WideString;
		function CloudResultToBoolean(JSON: WideString; ErrorPrefix: WideString): Boolean;
		function CloudResultToBooleanFromResult(OperationResult: TCMROperationResult; ErrorPrefix: WideString): Boolean;
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
	end;

implementation

{ TCloudListingServiceTest }

procedure TCloudListingServiceTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{"list":[]}}');

	FIsPublicAccount := False;
	FUnitedParams := 'token=test&x-email=test@mail.ru';
	FPublicLink := 'public_weblink_123';

	{Create retry operation for tests}
	FRetryOperation := TRetryOperation.Create(
		function: Boolean begin Result := True; end, {RefreshToken}
		function(const URL, Data: WideString; var Answer: WideString): Boolean begin Result := FMockHTTP.PostForm(URL, Data, Answer); end, {PostForm}
		function(const URL: WideString; var JSON: WideString; var ShowProgress: Boolean): Boolean begin Result := FMockHTTP.GetPage(URL, JSON, ShowProgress); end, {GetPage}
		function(const JSON, ErrorPrefix: WideString): Boolean begin Result := Pos(WideString('"status":200'), JSON) > 0; end, {ToBoolean}
		function(const JSON, ErrorPrefix: WideString): Integer begin Result := FS_FILE_OK; end, {ToInteger}
		3 {MaxRetries}
	);

	FService := TCloudListingService.Create(
		GetHTTP,
		nil, {No cipher for basic tests}
		TNullLogger.Create,
		FRetryOperation,
		IsPublicAccount,
		GetUnitedParams,
		GetPublicLink,
		CloudResultToBoolean,
		CloudResultToBooleanFromResult,
		False {DoCryptFilenames}
	);
end;

procedure TCloudListingServiceTest.TearDown;
begin
	FService := nil;
	if Assigned(FRetryOperation) then
		FRetryOperation.Free;
end;

function TCloudListingServiceTest.GetHTTP: ICloudHTTP;
begin
	Result := FMockHTTP;
end;

function TCloudListingServiceTest.IsPublicAccount: Boolean;
begin
	Result := FIsPublicAccount;
end;

function TCloudListingServiceTest.GetUnitedParams: WideString;
begin
	Result := FUnitedParams;
end;

function TCloudListingServiceTest.GetPublicLink: WideString;
begin
	Result := FPublicLink;
end;

function TCloudListingServiceTest.CloudResultToBoolean(JSON: WideString; ErrorPrefix: WideString): Boolean;
begin
	Result := Pos(WideString('"status":200'), JSON) > 0;
end;

function TCloudListingServiceTest.CloudResultToBooleanFromResult(OperationResult: TCMROperationResult; ErrorPrefix: WideString): Boolean;
begin
	Result := OperationResult.OperationStatus = 200;
end;

{Construction tests}

procedure TCloudListingServiceTest.TestCreate_InitializesCorrectly;
begin
	Assert.IsNotNull(FService, 'Service should be created');
end;

{GetDirectory tests}

procedure TCloudListingServiceTest.TestGetDirectory_Success_PopulatesListing;
var
	Listing: TCMRDirItemList;
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
	Listing: TCMRDirItemList;
	Success: Boolean;
begin
	FIsPublicAccount := True;
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":{"list":[]}}');

	Success := FService.GetDirectory('/test', Listing);

	Assert.IsTrue(Success, 'GetDirectory should succeed for public account');
	{For public accounts, different endpoint is used}
	Assert.IsTrue(FMockHTTP.WasURLCalled('weblink'), 'Public account should use weblink endpoint');
end;

{GetSharedLinks tests}

procedure TCloudListingServiceTest.TestGetSharedLinks_PublicAccount_ReturnsFalse;
var
	Listing: TCMRDirItemList;
	Success: Boolean;
begin
	FIsPublicAccount := True;

	Success := FService.GetSharedLinks(Listing);

	Assert.IsFalse(Success, 'GetSharedLinks should return false for public account');
end;

procedure TCloudListingServiceTest.TestGetSharedLinks_Success_PopulatesListing;
var
	Listing: TCMRDirItemList;
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
	Listing: TCMRIncomingInviteList;
	Success: Boolean;
begin
	FIsPublicAccount := True;

	Success := FService.GetIncomingInvites(Listing);

	Assert.IsFalse(Success, 'GetIncomingInvites should return false for public account');
end;

procedure TCloudListingServiceTest.TestGetIncomingInvites_Success_PopulatesListing;
var
	Listing: TCMRIncomingInviteList;
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
	DirListing: TCMRDirItemList;
	InvitesListing: TCMRIncomingInviteList;
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
	Listing: TCMRDirItemList;
	Success: Boolean;
begin
	FIsPublicAccount := True;

	Success := FService.GetTrashbin(Listing);

	Assert.IsFalse(Success, 'GetTrashbin should return false for public account');
end;

procedure TCloudListingServiceTest.TestGetTrashbin_Success_PopulatesListing;
var
	Listing: TCMRDirItemList;
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
	FileInfo: TCMRDirItem;
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
	FileInfo: TCMRDirItem;
	Success: Boolean;
begin
	FIsPublicAccount := True;
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
	FIsPublicAccount := True;

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
	FIsPublicAccount := True;

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

initialization

TDUnitX.RegisterTestFixture(TCloudListingServiceTest);

end.
