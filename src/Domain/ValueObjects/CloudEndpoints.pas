unit CloudEndpoints;

{Holds all resolved endpoint base URLs for a cloud service instance.
	Provides computed accessors for individual API paths derived from the base API URL.
	Allows configuring endpoints for self-hosted API-compatible servers.}

interface

type
	TCloudEndpoints = record
		{Configurable base URLs}
		ApiBase: WideString;
		OAuthUrl: WideString;
		DispatcherUrl: WideString;
		ThumbnailUrl: WideString;
		PublicUrl: WideString;
		DownloadUrl: WideString; {Empty = use dispatcher}
		UploadUrl: WideString; {Empty = use dispatcher}

		{Computed API v2 endpoints -- all derived from ApiBase}
		function ApiCsrf: WideString;
		function ApiFile: WideString;
		function ApiFileMove: WideString;
		function ApiFilePublish: WideString;
		function ApiFileUnpublish: WideString;
		function ApiFileRename: WideString;
		function ApiFileAdd: WideString;
		function ApiFileRemove: WideString;
		function ApiFileCopy: WideString;
		function ApiFolder: WideString;
		function ApiFolderAdd: WideString;
		function ApiFolderSharedInfo: WideString;
		function ApiFolderShare: WideString;
		function ApiFolderUnshare: WideString;
		function ApiFolderMount: WideString;
		function ApiFolderUnmount: WideString;
		function ApiFolderSharedLinks: WideString;
		function ApiFolderSharedIncoming: WideString;
		function ApiTrashbin: WideString;
		function ApiTrashbinRestore: WideString;
		function ApiTrashbinEmpty: WideString;
		function ApiDispatcher: WideString;
		function ApiUserSpace: WideString;
		function ApiClone: WideString;
		function ApiInviteReject: WideString;
		function ApiFileHistory: WideString;

		class function CreateDefaults: TCloudEndpoints; static;
	end;

implementation

uses
	CloudConstants;

{TCloudEndpoints}

function TCloudEndpoints.ApiCsrf: WideString;
begin
	Result := ApiBase + '/tokens/csrf';
end;

function TCloudEndpoints.ApiFile: WideString;
begin
	Result := ApiBase + '/file';
end;

function TCloudEndpoints.ApiFileMove: WideString;
begin
	Result := ApiBase + '/file/move';
end;

function TCloudEndpoints.ApiFilePublish: WideString;
begin
	Result := ApiBase + '/file/publish';
end;

function TCloudEndpoints.ApiFileUnpublish: WideString;
begin
	Result := ApiBase + '/file/unpublish';
end;

function TCloudEndpoints.ApiFileRename: WideString;
begin
	Result := ApiBase + '/file/rename';
end;

function TCloudEndpoints.ApiFileAdd: WideString;
begin
	Result := ApiBase + '/file/add';
end;

function TCloudEndpoints.ApiFileRemove: WideString;
begin
	Result := ApiBase + '/file/remove';
end;

function TCloudEndpoints.ApiFileCopy: WideString;
begin
	Result := ApiBase + '/file/copy';
end;

function TCloudEndpoints.ApiFolder: WideString;
begin
	Result := ApiBase + '/folder?sort={%22type%22%3A%22name%22%2C%22order%22%3A%22asc%22}';
end;

function TCloudEndpoints.ApiFolderAdd: WideString;
begin
	Result := ApiBase + '/folder/add';
end;

function TCloudEndpoints.ApiFolderSharedInfo: WideString;
begin
	Result := ApiBase + '/folder/shared/info';
end;

function TCloudEndpoints.ApiFolderShare: WideString;
begin
	Result := ApiBase + '/folder/share';
end;

function TCloudEndpoints.ApiFolderUnshare: WideString;
begin
	Result := ApiBase + '/folder/unshare';
end;

function TCloudEndpoints.ApiFolderMount: WideString;
begin
	Result := ApiBase + '/folder/mount';
end;

function TCloudEndpoints.ApiFolderUnmount: WideString;
begin
	Result := ApiBase + '/folder/unmount';
end;

function TCloudEndpoints.ApiFolderSharedLinks: WideString;
begin
	Result := ApiBase + '/folder/shared/links';
end;

function TCloudEndpoints.ApiFolderSharedIncoming: WideString;
begin
	Result := ApiBase + '/folder/shared/incoming';
end;

function TCloudEndpoints.ApiTrashbin: WideString;
begin
	Result := ApiBase + '/trashbin';
end;

function TCloudEndpoints.ApiTrashbinRestore: WideString;
begin
	Result := ApiBase + '/trashbin/restore';
end;

function TCloudEndpoints.ApiTrashbinEmpty: WideString;
begin
	Result := ApiBase + '/trashbin/empty';
end;

function TCloudEndpoints.ApiDispatcher: WideString;
begin
	Result := ApiBase + '/dispatcher/';
end;

function TCloudEndpoints.ApiUserSpace: WideString;
begin
	Result := ApiBase + '/user/space';
end;

function TCloudEndpoints.ApiClone: WideString;
begin
	Result := ApiBase + '/clone';
end;

function TCloudEndpoints.ApiInviteReject: WideString;
begin
	Result := ApiBase + '/folder/invites/reject';
end;

function TCloudEndpoints.ApiFileHistory: WideString;
begin
	Result := ApiBase + '/file/history';
end;

class function TCloudEndpoints.CreateDefaults: TCloudEndpoints;
begin
	Result := Default(TCloudEndpoints);
	Result.ApiBase := 'https://cloud.mail.ru/api/v2';
	Result.OAuthUrl := OAUTH_TOKEN_URL;
	Result.DispatcherUrl := OAUTH_DISPATCHER_URL;
	Result.ThumbnailUrl := THUMB_CLOUD_URL;
	Result.PublicUrl := PUBLIC_ACCESS_URL;
	Result.DownloadUrl := '';
	Result.UploadUrl := '';
end;

end.
