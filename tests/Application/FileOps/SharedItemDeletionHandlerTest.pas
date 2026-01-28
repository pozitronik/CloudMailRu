unit SharedItemDeletionHandlerTest;

{Unit tests for TSharedItemDeletionHandler.
 Tests shared item deletion logic using mock cloud infrastructure.}

interface

uses
	DUnitX.TestFramework,
	CloudDirItem,
	CloudConstants,
	CloudMailRu,
	CloudSettings,
	FileCipher,
	AuthStrategy,
	WindowsFileSystem,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	CloudHTTP,
	HTTPManager,
	MockCloudHTTP,
	MockHTTPManager,
	SharedItemDeletionHandler,
	OpenSSLProvider,
	TestHelper;

type
	[TestFixture]
	TSharedItemDeletionHandlerTest = class
	private
		FHandler: ISharedItemDeletionHandler;
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TCloudMailRu;

		function CreatePublishedItem(const HomePath, Weblink: WideString): TCloudDirItem;
		function CreateUnpublishedItem(const HomePath: WideString): TCloudDirItem;
		function CreateCloud: TCloudMailRu;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Published item tests}
		[Test]
		procedure TestExecute_PublishedItem_CallsUnpublish;
		[Test]
		procedure TestExecute_PublishedItem_ReturnsTrue;

		{Unpublished item tests}
		[Test]
		procedure TestExecute_UnpublishedItem_SkipsUnpublish;
		[Test]
		procedure TestExecute_UnpublishedItem_ReturnsTrue;

		{Collaborator tests}
		[Test]
		procedure TestExecute_WithCollaborators_UnsharesEach;
	end;

implementation

uses
	SysUtils;

const
	{Sample API responses}
	JSON_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';
	JSON_SHARE_INFO_EMPTY = '{"email":"test@mail.ru","body":{"invited":[]},"status":200}';
	JSON_SHARE_INFO_WITH_COLLABORATORS = '{"email":"test@mail.ru","body":{"invited":[{"email":"user1@mail.ru"},{"email":"user2@mail.ru"}]},"status":200}';

function TSharedItemDeletionHandlerTest.CreatePublishedItem(const HomePath, Weblink: WideString): TCloudDirItem;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.home := HomePath;
	Result.weblink := Weblink;
	Result.type_ := TYPE_DIR;
end;

function TSharedItemDeletionHandlerTest.CreateUnpublishedItem(const HomePath: WideString): TCloudDirItem;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.home := HomePath;
	Result.weblink := '';
	Result.type_ := TYPE_DIR;
end;

function TSharedItemDeletionHandlerTest.CreateCloud: TCloudMailRu;
var
	Settings: TCloudSettings;
begin
	Settings := Default(TCloudSettings);

	Result := TCloudMailRu.Create(
		Settings,
		FMockHTTPManager,
		TestThreadID(),
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		TNullCipher.Create,
		TNullOpenSSLProvider.Create);
end;

procedure TSharedItemDeletionHandlerTest.Setup;
begin
	FHandler := TSharedItemDeletionHandler.Create;
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
	FCloud := nil;
end;

procedure TSharedItemDeletionHandlerTest.TearDown;
begin
	FCloud.Free;
	FHandler := nil;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

{Published item tests}

procedure TSharedItemDeletionHandlerTest.TestExecute_PublishedItem_CallsUnpublish;
var
	Item: TCloudDirItem;
begin
	FCloud := CreateCloud;
	FMockHTTP.QueueResponse('', True, JSON_SHARE_INFO_EMPTY);
	FMockHTTP.QueueResponse('', True, JSON_SUCCESS);
	Item := CreatePublishedItem('/shared/folder', 'abc123');

	FHandler.Execute(FCloud, Item);

	Assert.IsTrue(FMockHTTP.GetCallCount >= 2, 'Should call GetShareInfo and PublishFile');
end;

procedure TSharedItemDeletionHandlerTest.TestExecute_PublishedItem_ReturnsTrue;
var
	Item: TCloudDirItem;
	Res: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.QueueResponse('', True, JSON_SHARE_INFO_EMPTY);
	FMockHTTP.QueueResponse('', True, JSON_SUCCESS);
	Item := CreatePublishedItem('/shared/folder', 'weblink123');

	Res := FHandler.Execute(FCloud, Item);

	Assert.IsTrue(Res, 'Should return True on success');
end;

{Unpublished item tests}

procedure TSharedItemDeletionHandlerTest.TestExecute_UnpublishedItem_SkipsUnpublish;
var
	Item: TCloudDirItem;
	InitialCallCount: Integer;
begin
	FCloud := CreateCloud;
	FMockHTTP.QueueResponse('', True, JSON_SHARE_INFO_EMPTY);
	Item := CreateUnpublishedItem('/shared/folder');
	InitialCallCount := FMockHTTP.GetCallCount;

	FHandler.Execute(FCloud, Item);

	{Should only call GetShareInfo, not PublishFile since item is not published}
	Assert.AreEqual(InitialCallCount + 1, FMockHTTP.GetCallCount, 'Should only call GetShareInfo');
end;

procedure TSharedItemDeletionHandlerTest.TestExecute_UnpublishedItem_ReturnsTrue;
var
	Item: TCloudDirItem;
	Res: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.QueueResponse('', True, JSON_SHARE_INFO_EMPTY);
	Item := CreateUnpublishedItem('/shared/folder');

	Res := FHandler.Execute(FCloud, Item);

	Assert.IsTrue(Res, 'Should return True even for unpublished items');
end;

{Collaborator tests}

procedure TSharedItemDeletionHandlerTest.TestExecute_WithCollaborators_UnsharesEach;
var
	Item: TCloudDirItem;
begin
	FCloud := CreateCloud;
	FMockHTTP.QueueResponse('', True, JSON_SHARE_INFO_WITH_COLLABORATORS);
	FMockHTTP.QueueResponse('', True, JSON_SUCCESS); {unshare user1}
	FMockHTTP.QueueResponse('', True, JSON_SUCCESS); {unshare user2}
	FMockHTTP.QueueResponse('', True, JSON_SUCCESS); {unpublish}
	Item := CreatePublishedItem('/shared/folder', 'weblink456');

	FHandler.Execute(FCloud, Item);

	{Should call: GetShareInfo + 2 shareFolder (unshare) + 1 publishFile (unpublish)}
	Assert.IsTrue(FMockHTTP.GetCallCount >= 4, 'Should unshare each collaborator');
end;

initialization
	TDUnitX.RegisterTestFixture(TSharedItemDeletionHandlerTest);

end.
