unit CloudMailRuDirListingTest;

{Tests for TCloudMailRu directory listing methods.
 Uses mock HTTP to verify request formation and response handling.}

interface

uses
	CloudMailRu,
	CloudSettings,
	CloudDirItemList,
	CloudDirItem,
	CloudIncomingInviteList,
	CloudConstants,
	FileCipher,
	WFXTypes,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	AuthStrategy,
	WindowsFileSystem,
	CloudHTTP,
	HTTPManager,
	MockCloudHTTP,
	MockHTTPManager,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework,
	OpenSSLProvider,
	AccountCredentialsProvider;

type
	{Testable subclass that exposes protected members for testing}
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
		procedure SetPublicLink(const Value: WideString);
	end;

	[TestFixture]
	TCloudMailRuDirListingTest = class
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

		{GetDirListing - success scenarios}
		[Test]
		procedure TestGetDirListing_Success_ReturnsTrueAndPopulatesList;
		[Test]
		procedure TestGetDirListing_EmptyDirectory_ReturnsEmptyList;
		[Test]
		procedure TestGetDirListing_WithSubdirectory_ParsesFolderCorrectly;
		[Test]
		procedure TestGetDirListing_WithFile_ParsesFileCorrectly;

		{GetDirListing - failure scenarios}
		[Test]
		procedure TestGetDirListing_HTTPFailure_ReturnsFalse;
		[Test]
		procedure TestGetDirListing_PathNotFound_ReturnsFalse;
		[Test]
		procedure TestGetDirListing_InvalidJSON_ReturnsFalse;
		[Test]
		procedure TestGetDirListing_APIError_ReturnsFalse;

		{GetDirListing - URL construction}
		[Test]
		procedure TestGetDirListing_ConstructsCorrectURL;
		[Test]
		procedure TestGetDirListing_EncodesPathInURL;

		{GetDirListing - public account}
		[Test]
		procedure TestGetDirListing_PublicAccount_UsesWeblinkURL;
		[Test]
		procedure TestGetDirListing_PublicAccount_Success;

		{GetTrashbinListing tests}
		[Test]
		procedure TestGetTrashbinListing_Success;
		[Test]
		procedure TestGetTrashbinListing_Empty;
		[Test]
		procedure TestGetTrashbinListing_Failure;

		{GetSharedLinksListing tests}
		[Test]
		procedure TestGetSharedLinksListing_Success;
		[Test]
		procedure TestGetSharedLinksListing_Failure;
	end;

implementation

uses
	LanguageStrings;

const
	{Sample API responses for testing}
	JSON_DIR_LISTING_SUCCESS =
		'{"email":"test@mail.ru","body":{"count":{"folders":1,"files":1},' +
		'"name":"TestDir","kind":"folder","type":"folder","home":"/TestDir",' +
		'"list":[' +
		'{"name":"subdir","size":0,"kind":"folder","type":"folder","home":"/TestDir/subdir"},' +
		'{"name":"file.txt","size":1024,"hash":"ABC123","mtime":1700000000,"kind":"file","type":"file","home":"/TestDir/file.txt","virus_scan":"pass"}' +
		']},"status":200}';

	JSON_DIR_LISTING_EMPTY =
		'{"email":"test@mail.ru","body":{"count":{"folders":0,"files":0},' +
		'"name":"EmptyDir","kind":"folder","type":"folder","home":"/EmptyDir",' +
		'"list":[]},"status":200}';

	JSON_DIR_NOT_FOUND =
		'{"email":"test@mail.ru","body":{"home":"/NonExistent"},"status":404,"error":"not_exists"}';

	JSON_API_ERROR =
		'{"email":"test@mail.ru","body":{},"status":400,"error":"invalid"}';

	JSON_TRASHBIN_SUCCESS =
		'{"email":"test@mail.ru","body":{"list":[' +
		'{"name":"deleted.txt","size":512,"hash":"DEF456","mtime":1700000000,"kind":"file","type":"file",' +
		'"home":"/deleted.txt","deleted_at":"1700000000","deleted_from":"/original/"}' +
		']},"status":200}';

	JSON_SHARED_LINKS_SUCCESS =
		'{"email":"test@mail.ru","body":{"list":[' +
		'{"name":"shared.txt","size":256,"hash":"GHI789","mtime":1700000000,"kind":"file","type":"file",' +
		'"home":"/shared.txt","weblink":"abcdef123456"}' +
		']},"status":200}';

{TTestableCloudMailRu}

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

procedure TTestableCloudMailRu.SetPublicLink(const Value: WideString);
begin
	FPublicLink := Value;
end;

{TCloudMailRuDirListingTest}

procedure TCloudMailRuDirListingTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
end;

procedure TCloudMailRuDirListingTest.TearDown;
begin
	FCloud.Free;
	{Interfaces are reference-counted, will be released automatically}
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCloudMailRuDirListingTest.CreateCloud(PublicAccount: Boolean): TTestableCloudMailRu;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := PublicAccount;

	Result := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TestThreadID(),
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		TNullCipher.Create,
		TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);

	{Set up API parameters as if logged in}
	Result.SetUnitedParams('api=2&access_token=test_token');
end;

{GetDirListing - success scenarios}

procedure TCloudMailRuDirListingTest.TestGetDirListing_Success_ReturnsTrueAndPopulatesList;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING_SUCCESS);

	Success := FCloud.ListingService.GetDirectory('/TestDir', DirListing);

	Assert.IsTrue(Success, 'GetDirListing should return True on success');
	Assert.AreEqual(Integer(2), Integer(Length(DirListing)), 'Should have 2 items (1 folder + 1 file)');
end;

procedure TCloudMailRuDirListingTest.TestGetDirListing_EmptyDirectory_ReturnsEmptyList;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING_EMPTY);

	Success := FCloud.ListingService.GetDirectory('/EmptyDir', DirListing);

	Assert.IsTrue(Success, 'GetDirListing should return True for empty directory');
	Assert.AreEqual(Integer(0), Integer(Length(DirListing)), 'Should have 0 items');
end;

procedure TCloudMailRuDirListingTest.TestGetDirListing_WithSubdirectory_ParsesFolderCorrectly;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
	FolderItem: TCloudDirItem;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING_SUCCESS);

	Success := FCloud.ListingService.GetDirectory('/TestDir', DirListing);

	Assert.IsTrue(Success);
	Assert.IsTrue(Length(DirListing) >= 1, 'Should have at least 1 item');

	{Find folder item}
	FolderItem := Default(TCloudDirItem);
	for var Item in DirListing do
		if Item.kind = TYPE_DIR then
		begin
			FolderItem := Item;
			Break;
		end;

	Assert.AreEqual(String('subdir'), String(FolderItem.name), 'Folder name should be "subdir"');
	Assert.AreEqual(String(TYPE_DIR), String(FolderItem.kind), 'Kind should be folder');
	Assert.AreEqual(String('/TestDir/subdir'), String(FolderItem.home), 'Home path should match');
end;

procedure TCloudMailRuDirListingTest.TestGetDirListing_WithFile_ParsesFileCorrectly;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
	FileItem: TCloudDirItem;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING_SUCCESS);

	Success := FCloud.ListingService.GetDirectory('/TestDir', DirListing);

	Assert.IsTrue(Success);

	{Find file item}
	FileItem := Default(TCloudDirItem);
	for var Item in DirListing do
		if Item.kind = TYPE_FILE then
		begin
			FileItem := Item;
			Break;
		end;

	Assert.AreEqual(String('file.txt'), String(FileItem.name), 'File name should be "file.txt"');
	Assert.AreEqual(String(TYPE_FILE), String(FileItem.kind), 'Kind should be file');
	Assert.AreEqual(Int64(1024), FileItem.size, 'File size should be 1024');
	Assert.AreEqual(String('ABC123'), String(FileItem.hash), 'Hash should match');
	Assert.AreEqual(String('pass'), String(FileItem.virus_scan), 'Virus scan should be pass');
end;

{GetDirListing - failure scenarios}

procedure TCloudMailRuDirListingTest.TestGetDirListing_HTTPFailure_ReturnsFalse;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	{Default mock response is failure}
	FMockHTTP.SetDefaultResponse(False, '', FS_FILE_READERROR);

	Success := FCloud.ListingService.GetDirectory('/SomeDir', DirListing);

	Assert.IsFalse(Success, 'GetDirListing should return False on HTTP failure');
	Assert.AreEqual(Integer(0), Integer(Length(DirListing)), 'DirListing should be empty');
end;

procedure TCloudMailRuDirListingTest.TestGetDirListing_PathNotFound_ReturnsFalse;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_NOT_FOUND);

	Success := FCloud.ListingService.GetDirectory('/NonExistent', DirListing);

	Assert.IsFalse(Success, 'GetDirListing should return False for non-existent path');
end;

procedure TCloudMailRuDirListingTest.TestGetDirListing_InvalidJSON_ReturnsFalse;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, 'invalid json {{{');

	Success := FCloud.ListingService.GetDirectory('/SomeDir', DirListing);

	Assert.IsFalse(Success, 'GetDirListing should return False for invalid JSON');
end;

procedure TCloudMailRuDirListingTest.TestGetDirListing_APIError_ReturnsFalse;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_API_ERROR);

	Success := FCloud.ListingService.GetDirectory('/SomeDir', DirListing);

	Assert.IsFalse(Success, 'GetDirListing should return False for API error');
end;

{GetDirListing - URL construction}

procedure TCloudMailRuDirListingTest.TestGetDirListing_ConstructsCorrectURL;
var
	DirListing: TCloudDirItemList;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING_SUCCESS);

	FCloud.ListingService.GetDirectory('/TestDir', DirListing);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FOLDER), 'Should call folder API endpoint');
	Assert.IsTrue(FMockHTTP.WasURLCalled('home='), 'URL should contain home parameter');
end;

procedure TCloudMailRuDirListingTest.TestGetDirListing_EncodesPathInURL;
var
	DirListing: TCloudDirItemList;
	LastCall: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING_SUCCESS);

	FCloud.ListingService.GetDirectory('/Path With Spaces', DirListing);

	LastCall := FMockHTTP.GetLastCall;
	{Path should be URL-encoded}
	Assert.IsTrue(Pos(String('%20'), String(LastCall)) > 0, 'Spaces in path should be URL-encoded');
end;

{GetDirListing - public account}

procedure TCloudMailRuDirListingTest.TestGetDirListing_PublicAccount_UsesWeblinkURL;
var
	DirListing: TCloudDirItemList;
	LastCall: WideString;
begin
	FCloud := CreateCloud(True); {Public account}
	FCloud.SetPublicLink('publiclink123');
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING_SUCCESS);

	FCloud.ListingService.GetDirectory('/TestDir', DirListing);

	LastCall := FMockHTTP.GetLastCall;
	Assert.IsTrue(Pos(String('weblink='), String(LastCall)) > 0, 'Public account should use weblink parameter');
end;

procedure TCloudMailRuDirListingTest.TestGetDirListing_PublicAccount_Success;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
begin
	FCloud := CreateCloud(True);
	FCloud.SetPublicLink('publiclink123');
	FMockHTTP.SetResponse(API_FOLDER, True, JSON_DIR_LISTING_SUCCESS);

	Success := FCloud.ListingService.GetDirectory('/TestDir', DirListing);

	Assert.IsTrue(Success, 'Public account GetDirListing should succeed');
	Assert.AreEqual(Integer(2), Integer(Length(DirListing)), 'Should parse items correctly');
end;

{GetTrashbinListing tests}

procedure TCloudMailRuDirListingTest.TestGetTrashbinListing_Success;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_TRASHBIN, True, JSON_TRASHBIN_SUCCESS);

	Success := FCloud.ListingService.GetTrashbin(DirListing);

	Assert.IsTrue(Success, 'GetTrashbinListing should return True');
	Assert.AreEqual(Integer(1), Integer(Length(DirListing)), 'Should have 1 deleted item');
	Assert.AreEqual(String('deleted.txt'), String(DirListing[0].name), 'Item name should match');
end;

procedure TCloudMailRuDirListingTest.TestGetTrashbinListing_Empty;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_TRASHBIN, True,
		'{"email":"test@mail.ru","body":{"list":[]},"status":200}');

	Success := FCloud.ListingService.GetTrashbin(DirListing);

	Assert.IsTrue(Success, 'GetTrashbinListing should return True for empty trash');
	Assert.AreEqual(Integer(0), Integer(Length(DirListing)), 'Should have 0 items');
end;

procedure TCloudMailRuDirListingTest.TestGetTrashbinListing_Failure;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetDefaultResponse(False, '', FS_FILE_READERROR);

	Success := FCloud.ListingService.GetTrashbin(DirListing);

	Assert.IsFalse(Success, 'GetTrashbinListing should return False on HTTP failure');
end;

{GetSharedLinksListing tests}

procedure TCloudMailRuDirListingTest.TestGetSharedLinksListing_Success;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_FOLDER_SHARED_LINKS, True, JSON_SHARED_LINKS_SUCCESS);

	Success := FCloud.ListingService.GetSharedLinks(DirListing);

	Assert.IsTrue(Success, 'GetSharedLinksListing should return True');
	Assert.AreEqual(Integer(1), Integer(Length(DirListing)), 'Should have 1 shared item');
	Assert.AreEqual(String('shared.txt'), String(DirListing[0].name), 'Item name should match');
	Assert.AreEqual(String('abcdef123456'), String(DirListing[0].weblink), 'Weblink should match');
end;

procedure TCloudMailRuDirListingTest.TestGetSharedLinksListing_Failure;
var
	DirListing: TCloudDirItemList;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetDefaultResponse(False, '', FS_FILE_READERROR);

	Success := FCloud.ListingService.GetSharedLinks(DirListing);

	Assert.IsFalse(Success, 'GetSharedLinksListing should return False on HTTP failure');
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuDirListingTest);

end.
