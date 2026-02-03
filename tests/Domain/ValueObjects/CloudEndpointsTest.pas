unit CloudEndpointsTest;

interface

uses
	CloudEndpoints,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCloudEndpointsTest = class
	public
		[Test]
		procedure TestCreateDefaults_ApiBase;
		[Test]
		procedure TestCreateDefaults_OAuthUrl;
		[Test]
		procedure TestCreateDefaults_DispatcherUrl;
		[Test]
		procedure TestCreateDefaults_ThumbnailUrl;
		[Test]
		procedure TestCreateDefaults_PublicUrl;
		[Test]
		procedure TestCreateDefaults_DownloadUrlEmpty;
		[Test]
		procedure TestCreateDefaults_UploadUrlEmpty;
		[Test]
		procedure TestApiCsrf;
		[Test]
		procedure TestApiFile;
		[Test]
		procedure TestApiFileMove;
		[Test]
		procedure TestApiFilePublish;
		[Test]
		procedure TestApiFileUnpublish;
		[Test]
		procedure TestApiFileRename;
		[Test]
		procedure TestApiFileAdd;
		[Test]
		procedure TestApiFileRemove;
		[Test]
		procedure TestApiFileCopy;
		[Test]
		procedure TestApiFolder;
		[Test]
		procedure TestApiFolderAdd;
		[Test]
		procedure TestApiFolderSharedInfo;
		[Test]
		procedure TestApiFolderInvites;
		[Test]
		procedure TestApiFolderShare;
		[Test]
		procedure TestApiFolderUnshare;
		[Test]
		procedure TestApiFolderMount;
		[Test]
		procedure TestApiFolderUnmount;
		[Test]
		procedure TestApiFolderSharedLinks;
		[Test]
		procedure TestApiFolderSharedIncoming;
		[Test]
		procedure TestApiTrashbin;
		[Test]
		procedure TestApiTrashbinRestore;
		[Test]
		procedure TestApiTrashbinEmpty;
		[Test]
		procedure TestApiDispatcher;
		[Test]
		procedure TestApiUserSpace;
		[Test]
		procedure TestApiClone;
		[Test]
		procedure TestApiInviteReject;
		[Test]
		procedure TestDefaultEndpointsMatchConstants;
		[Test]
		procedure TestCustomApiBase;
		[Test]
		procedure TestCustomApiBase_AllPathsDerived;
	end;

implementation

uses
	CloudConstants;

procedure TCloudEndpointsTest.TestCreateDefaults_ApiBase;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual('https://cloud.mail.ru/api/v2', Endpoints.ApiBase);
end;

procedure TCloudEndpointsTest.TestCreateDefaults_OAuthUrl;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(OAUTH_TOKEN_URL, Endpoints.OAuthUrl);
end;

procedure TCloudEndpointsTest.TestCreateDefaults_DispatcherUrl;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(OAUTH_DISPATCHER_URL, Endpoints.DispatcherUrl);
end;

procedure TCloudEndpointsTest.TestCreateDefaults_ThumbnailUrl;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(THUMB_CLOUD_URL, Endpoints.ThumbnailUrl);
end;

procedure TCloudEndpointsTest.TestCreateDefaults_PublicUrl;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(PUBLIC_ACCESS_URL, Endpoints.PublicUrl);
end;

procedure TCloudEndpointsTest.TestCreateDefaults_DownloadUrlEmpty;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual('', string(Endpoints.DownloadUrl));
end;

procedure TCloudEndpointsTest.TestCreateDefaults_UploadUrlEmpty;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual('', string(Endpoints.UploadUrl));
end;

procedure TCloudEndpointsTest.TestApiCsrf;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_CSRF, Endpoints.ApiCsrf);
end;

procedure TCloudEndpointsTest.TestApiFile;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FILE, Endpoints.ApiFile);
end;

procedure TCloudEndpointsTest.TestApiFileMove;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FILE_MOVE, Endpoints.ApiFileMove);
end;

procedure TCloudEndpointsTest.TestApiFilePublish;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FILE_PUBLISH, Endpoints.ApiFilePublish);
end;

procedure TCloudEndpointsTest.TestApiFileUnpublish;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FILE_UNPUBLISH, Endpoints.ApiFileUnpublish);
end;

procedure TCloudEndpointsTest.TestApiFileRename;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FILE_RENAME, Endpoints.ApiFileRename);
end;

procedure TCloudEndpointsTest.TestApiFileAdd;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FILE_ADD, Endpoints.ApiFileAdd);
end;

procedure TCloudEndpointsTest.TestApiFileRemove;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FILE_REMOVE, Endpoints.ApiFileRemove);
end;

procedure TCloudEndpointsTest.TestApiFileCopy;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FILE_COPY, Endpoints.ApiFileCopy);
end;

procedure TCloudEndpointsTest.TestApiFolder;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FOLDER, Endpoints.ApiFolder);
end;

procedure TCloudEndpointsTest.TestApiFolderAdd;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FOLDER_ADD, Endpoints.ApiFolderAdd);
end;

procedure TCloudEndpointsTest.TestApiFolderSharedInfo;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FOLDER_SHARED_INFO, Endpoints.ApiFolderSharedInfo);
end;

procedure TCloudEndpointsTest.TestApiFolderInvites;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FOLDER_INVITES, Endpoints.ApiFolderInvites);
end;

procedure TCloudEndpointsTest.TestApiFolderShare;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FOLDER_SHARE, Endpoints.ApiFolderShare);
end;

procedure TCloudEndpointsTest.TestApiFolderUnshare;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FOLDER_UNSHARE, Endpoints.ApiFolderUnshare);
end;

procedure TCloudEndpointsTest.TestApiFolderMount;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FOLDER_MOUNT, Endpoints.ApiFolderMount);
end;

procedure TCloudEndpointsTest.TestApiFolderUnmount;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FOLDER_UNMOUNT, Endpoints.ApiFolderUnmount);
end;

procedure TCloudEndpointsTest.TestApiFolderSharedLinks;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FOLDER_SHARED_LINKS, Endpoints.ApiFolderSharedLinks);
end;

procedure TCloudEndpointsTest.TestApiFolderSharedIncoming;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_FOLDER_SHARED_INCOMING, Endpoints.ApiFolderSharedIncoming);
end;

procedure TCloudEndpointsTest.TestApiTrashbin;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_TRASHBIN, Endpoints.ApiTrashbin);
end;

procedure TCloudEndpointsTest.TestApiTrashbinRestore;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_TRASHBIN_RESTORE, Endpoints.ApiTrashbinRestore);
end;

procedure TCloudEndpointsTest.TestApiTrashbinEmpty;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_TRASHBIN_EMPTY, Endpoints.ApiTrashbinEmpty);
end;

procedure TCloudEndpointsTest.TestApiDispatcher;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_DISPATCHER, Endpoints.ApiDispatcher);
end;

procedure TCloudEndpointsTest.TestApiUserSpace;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_USER_SPACE, Endpoints.ApiUserSpace);
end;

procedure TCloudEndpointsTest.TestApiClone;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_CLONE, Endpoints.ApiClone);
end;

procedure TCloudEndpointsTest.TestApiInviteReject;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_INVITE_REJECT, Endpoints.ApiInviteReject);
end;

{Verify that default computed endpoints match the hardcoded constants}
procedure TCloudEndpointsTest.TestDefaultEndpointsMatchConstants;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(API_CSRF, Endpoints.ApiCsrf);
	Assert.AreEqual(API_FILE, Endpoints.ApiFile);
	Assert.AreEqual(API_FILE_MOVE, Endpoints.ApiFileMove);
	Assert.AreEqual(API_FILE_PUBLISH, Endpoints.ApiFilePublish);
	Assert.AreEqual(API_FILE_UNPUBLISH, Endpoints.ApiFileUnpublish);
	Assert.AreEqual(API_FILE_RENAME, Endpoints.ApiFileRename);
	Assert.AreEqual(API_FILE_ADD, Endpoints.ApiFileAdd);
	Assert.AreEqual(API_FILE_REMOVE, Endpoints.ApiFileRemove);
	Assert.AreEqual(API_FILE_COPY, Endpoints.ApiFileCopy);
	Assert.AreEqual(API_FOLDER, Endpoints.ApiFolder);
	Assert.AreEqual(API_FOLDER_ADD, Endpoints.ApiFolderAdd);
	Assert.AreEqual(API_FOLDER_SHARED_INFO, Endpoints.ApiFolderSharedInfo);
	Assert.AreEqual(API_FOLDER_INVITES, Endpoints.ApiFolderInvites);
	Assert.AreEqual(API_FOLDER_SHARE, Endpoints.ApiFolderShare);
	Assert.AreEqual(API_FOLDER_UNSHARE, Endpoints.ApiFolderUnshare);
	Assert.AreEqual(API_FOLDER_MOUNT, Endpoints.ApiFolderMount);
	Assert.AreEqual(API_FOLDER_UNMOUNT, Endpoints.ApiFolderUnmount);
	Assert.AreEqual(API_FOLDER_SHARED_LINKS, Endpoints.ApiFolderSharedLinks);
	Assert.AreEqual(API_FOLDER_SHARED_INCOMING, Endpoints.ApiFolderSharedIncoming);
	Assert.AreEqual(API_TRASHBIN, Endpoints.ApiTrashbin);
	Assert.AreEqual(API_TRASHBIN_RESTORE, Endpoints.ApiTrashbinRestore);
	Assert.AreEqual(API_TRASHBIN_EMPTY, Endpoints.ApiTrashbinEmpty);
	Assert.AreEqual(API_DISPATCHER, Endpoints.ApiDispatcher);
	Assert.AreEqual(API_USER_SPACE, Endpoints.ApiUserSpace);
	Assert.AreEqual(API_CLONE, Endpoints.ApiClone);
	Assert.AreEqual(API_INVITE_REJECT, Endpoints.ApiInviteReject);
end;

{Verify all computed paths derive from custom ApiBase}
procedure TCloudEndpointsTest.TestCustomApiBase;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Endpoints.ApiBase := 'http://localhost:8080/api/v2';
	Assert.AreEqual('http://localhost:8080/api/v2/tokens/csrf', Endpoints.ApiCsrf);
	Assert.AreEqual('http://localhost:8080/api/v2/file', Endpoints.ApiFile);
	Assert.AreEqual('http://localhost:8080/api/v2/file/add', Endpoints.ApiFileAdd);
end;

procedure TCloudEndpointsTest.TestCustomApiBase_AllPathsDerived;
var
	Endpoints: TCloudEndpoints;
	CustomBase: WideString;
begin
	CustomBase := 'https://my.server.com/cloud/api/v2';
	Endpoints := TCloudEndpoints.CreateDefaults;
	Endpoints.ApiBase := CustomBase;

	{Every computed path must start with the custom base}
	Assert.StartsWith(CustomBase, Endpoints.ApiCsrf);
	Assert.StartsWith(CustomBase, Endpoints.ApiFile);
	Assert.StartsWith(CustomBase, Endpoints.ApiFileMove);
	Assert.StartsWith(CustomBase, Endpoints.ApiFilePublish);
	Assert.StartsWith(CustomBase, Endpoints.ApiFileUnpublish);
	Assert.StartsWith(CustomBase, Endpoints.ApiFileRename);
	Assert.StartsWith(CustomBase, Endpoints.ApiFileAdd);
	Assert.StartsWith(CustomBase, Endpoints.ApiFileRemove);
	Assert.StartsWith(CustomBase, Endpoints.ApiFileCopy);
	Assert.StartsWith(CustomBase, Endpoints.ApiFolder);
	Assert.StartsWith(CustomBase, Endpoints.ApiFolderAdd);
	Assert.StartsWith(CustomBase, Endpoints.ApiFolderSharedInfo);
	Assert.StartsWith(CustomBase, Endpoints.ApiFolderInvites);
	Assert.StartsWith(CustomBase, Endpoints.ApiFolderShare);
	Assert.StartsWith(CustomBase, Endpoints.ApiFolderUnshare);
	Assert.StartsWith(CustomBase, Endpoints.ApiFolderMount);
	Assert.StartsWith(CustomBase, Endpoints.ApiFolderUnmount);
	Assert.StartsWith(CustomBase, Endpoints.ApiFolderSharedLinks);
	Assert.StartsWith(CustomBase, Endpoints.ApiFolderSharedIncoming);
	Assert.StartsWith(CustomBase, Endpoints.ApiTrashbin);
	Assert.StartsWith(CustomBase, Endpoints.ApiTrashbinRestore);
	Assert.StartsWith(CustomBase, Endpoints.ApiTrashbinEmpty);
	Assert.StartsWith(CustomBase, Endpoints.ApiDispatcher);
	Assert.StartsWith(CustomBase, Endpoints.ApiUserSpace);
	Assert.StartsWith(CustomBase, Endpoints.ApiClone);
	Assert.StartsWith(CustomBase, Endpoints.ApiInviteReject);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudEndpointsTest);

end.
