unit CloudShareService;

{Service for sharing and publishing operations on Cloud.Mail.ru}

interface

uses
	CloudContext,
	CloudDirItem,
	CloudInviteList,
	CloudInviteListJsonAdapter,
	CloudConstants,
	CloudHTTP,
	CloudShardManager,
	JSONHelper,
	LanguageStrings,
	PathHelper,
	WFXTypes,
	StringHelper,
	Logger,
	TokenRetryHelper,
	DCPbase64,
	System.SysUtils;

type
	{Interface for share service operations}
	ICloudShareService = interface
		['{B8C9D0E1-F2A3-4B5C-6D7E-8F9A0B1C2D3E}']
		{Publish or unpublish a file/folder, returns public link when publishing}
		function Publish(Path: WideString; var PublicLink: WideString): Boolean;
		function Unpublish(Path: WideString; PublicLink: WideString): Boolean;
		{Get sharing information for a path}
		function GetShareInfo(Path: WideString; var InviteListing: TCloudInviteList): Boolean;
		{Share a folder with another user by email}
		function Share(Path, Email: WideString; Access: Integer): Boolean;
		{Unshare a folder from a user}
		function Unshare(Path, Email: WideString): Boolean;
		{Mount a shared folder to user's cloud}
		function Mount(Home, InviteToken: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		{Unmount a shared folder, optionally keeping a copy}
		function Unmount(Home: WideString; CloneCopy: Boolean): Boolean;
		{Reject a share invitation}
		function RejectInvite(InviteToken: WideString): Boolean;
		{Get streaming URL for a published file}
		function GetPublishedFileStreamUrl(FileIdentity: TCloudDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; Publish: Boolean = CLOUD_PUBLISH): Boolean;
		{Clone a public weblink to user's cloud}
		function CloneWeblink(Path, Link: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Integer;
	end;

	{Implementation of share service}
	TCloudShareService = class(TInterfacedObject, ICloudShareService)
	private
		FContext: ICloudContext;
		FLogger: ILogger;
		FRetryOperation: IRetryOperation;
		FShardManager: ICloudShardManager;
	public
		constructor Create(Context: ICloudContext; Logger: ILogger; RetryOperation: IRetryOperation; ShardManager: ICloudShardManager);

		{ICloudShareService implementation}
		function Publish(Path: WideString; var PublicLink: WideString): Boolean;
		function Unpublish(Path: WideString; PublicLink: WideString): Boolean;
		function GetShareInfo(Path: WideString; var InviteListing: TCloudInviteList): Boolean;
		function Share(Path, Email: WideString; Access: Integer): Boolean;
		function Unshare(Path, Email: WideString): Boolean;
		function Mount(Home, InviteToken: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function Unmount(Home: WideString; CloneCopy: Boolean): Boolean;
		function RejectInvite(InviteToken: WideString): Boolean;
		function GetPublishedFileStreamUrl(FileIdentity: TCloudDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; Publish: Boolean = CLOUD_PUBLISH): Boolean;
		function CloneWeblink(Path, Link: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Integer;
	end;

implementation

{TCloudShareService}

constructor TCloudShareService.Create(Context: ICloudContext; Logger: ILogger; RetryOperation: IRetryOperation; ShardManager: ICloudShardManager);
begin
	inherited Create;
	FContext := Context;
	FLogger := Logger;
	FRetryOperation := RetryOperation;
	FShardManager := ShardManager;
end;

function TCloudShareService.Publish(Path: WideString; var PublicLink: WideString): Boolean;
var
	CallResult: TAPICallResult;
	ExtractedLink: WideString;
begin
	Result := False;
	if FContext.IsPublicAccount then
		Exit;

	ExtractedLink := '';

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := FContext.GetHTTP.PostForm(FContext.GetEndpoints.ApiFilePublish + '?' + FContext.GetUnitedParams, Format('home=/%s&conflict', [PathToUrl(Path)]), JSON, 'application/x-www-form-urlencoded', true, False);

			if Success then
				Success := FContext.CloudResultToBoolean(JSON, PREFIX_ERR_FILE_PUBLISH);

			if Success then
				Success := JSONHelper.GetPublicLink(JSON, ExtractedLink);

			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		PublicLink := ExtractedLink;
end;

function TCloudShareService.Unpublish(Path: WideString; PublicLink: WideString): Boolean;
var
	CallResult: TAPICallResult;
	CurrentLink: WideString;
begin
	Result := False;
	if FContext.IsPublicAccount then
		Exit;

	CurrentLink := PublicLink;

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := FContext.GetHTTP.PostForm(FContext.GetEndpoints.ApiFileUnpublish + '?' + FContext.GetUnitedParams, Format('weblink=%s&conflict', [CurrentLink]), JSON, 'application/x-www-form-urlencoded', true, False);

			if Success then
				Success := FContext.CloudResultToBoolean(JSON, PREFIX_ERR_FILE_PUBLISH);

			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
end;

function TCloudShareService.GetShareInfo(Path: WideString; var InviteListing: TCloudInviteList): Boolean;
var
	CallResult: TAPICallResult;
	LocalListing: TCloudInviteList;
begin
	SetLength(LocalListing, 0);

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Progress: Boolean;
			Success: Boolean;
		begin
			Progress := False;
			Success := FContext.GetHTTP.GetPage(Format('%s?home=%s&%s', [FContext.GetEndpoints.ApiFolderSharedInfo, PathToUrl(Path), FContext.GetUnitedParams]), JSON, Progress);
			if Success then
				Success := TCloudInviteListJsonAdapter.Parse(JSON, LocalListing);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		InviteListing := LocalListing;
end;

function TCloudShareService.Share(Path, Email: WideString; Access: Integer): Boolean;
var
	CallResult: TAPICallResult;
	AccessString: WideString;
begin
	if not(Access in [CLOUD_SHARE_RW, CLOUD_SHARE_RO]) then
		Exit(False);

	if Access = CLOUD_SHARE_RW then
		AccessString := CLOUD_SHARE_ACCESS_READ_WRITE
	else
		AccessString := CLOUD_SHARE_ACCESS_READ_ONLY;

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := FContext.GetHTTP.PostForm(FContext.GetEndpoints.ApiFolderShare + '?' + FContext.GetUnitedParams, Format('home=/%s&invite={"email":"%s","access":"%s"}', [PathToUrl(Path), Email, AccessString]), JSON);

			if Success then
				Success := FContext.CloudResultToBoolean(JSON, PREFIX_ERR_INVITE_MEMBER);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
end;

function TCloudShareService.Unshare(Path, Email: WideString): Boolean;
var
	CallResult: TAPICallResult;
begin
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := FContext.GetHTTP.PostForm(FContext.GetEndpoints.ApiFolderUnshare + '?' + FContext.GetUnitedParams, Format('home=/%s&invite={"email":"%s"}', [PathToUrl(Path), Email]), JSON);

			if Success then
				Success := FContext.CloudResultToBoolean(JSON, PREFIX_ERR_INVITE_MEMBER);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
end;

function TCloudShareService.Mount(Home, InviteToken: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
begin
	Result := False;
	if FContext.IsPublicAccount then
		Exit;

	Result := FRetryOperation.PostFormBoolean(FContext.GetEndpoints.ApiFolderMount, Format('home=%s&invite_token=%s&conflict=%s', [UrlEncode(Home), InviteToken, ConflictMode]), PREFIX_ERR_FOLDER_MOUNT);
end;

function TCloudShareService.Unmount(Home: WideString; CloneCopy: Boolean): Boolean;
var
	CopyStr: WideString;
begin
	Result := False;
	if FContext.IsPublicAccount then
		Exit;

	if CloneCopy then
		CopyStr := 'true'
	else
		CopyStr := 'false';

	Result := FRetryOperation.PostFormBoolean(FContext.GetEndpoints.ApiFolderUnmount, Format('home=%s&clone_copy=%s', [UrlEncode(Home), CopyStr]), PREFIX_ERR_FOLDER_UNMOUNT);
end;

function TCloudShareService.RejectInvite(InviteToken: WideString): Boolean;
begin
	Result := False;
	if FContext.IsPublicAccount then
		Exit;

	Result := FRetryOperation.PostFormBoolean(FContext.GetEndpoints.ApiInviteReject, Format('invite_token=%s', [InviteToken]), PREFIX_ERR_INVITE_REJECT);
end;

function TCloudShareService.GetPublishedFileStreamUrl(FileIdentity: TCloudDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; Publish: Boolean = CLOUD_PUBLISH): Boolean;
var
	ShardUrl: WideString;
	LocalWeblink: WideString;
begin
	Result := False;
	LocalWeblink := FileIdentity.weblink;

	{Publish file first if no weblink exists and publishing is requested}
	if (EmptyWideStr = LocalWeblink) then
	begin
		if (not Publish) or (not Self.Publish(FileIdentity.Home, LocalWeblink)) then
			Exit;
	end;

	{Get the shard URL for video streaming}
	if not FShardManager.ResolveShard(ShardUrl, ShardType) then
		Exit;

	{Build streaming URL with base64-encoded weblink}
	StreamUrl := Format('%s0p/%s.m3u8?double_encode=1', [ShardUrl, DCPbase64.Base64EncodeStr(String(RawByteString(LocalWeblink)))]);
	Result := true;
end;

function TCloudShareService.CloneWeblink(Path, Link, ConflictMode: WideString): Integer;
var
	CallResult: TAPICallResult;
begin
	if FContext.IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Progress: Boolean;
			ResultCode: Integer;
		begin
			Progress := true;
			if FContext.GetHTTP.GetPage(Format('%s?folder=/%s&weblink=%s&conflict=%s&%s', [FContext.GetEndpoints.ApiClone, PathToUrl(Path), Link, ConflictMode, FContext.GetUnitedParams]), JSON, Progress) then
			begin
				ResultCode := FContext.CloudResultToFsResult(JSON, PREFIX_ERR_FILE_PUBLISH);
				if (ResultCode <> FS_FILE_OK) and not(Progress) then
					ResultCode := FS_FILE_USERABORT; {user cancelled}
			end
			else
				ResultCode := FS_FILE_WRITEERROR;
			Result := TAPICallResult.FromInteger(ResultCode, JSON);
		end);

	Result := CallResult.ResultCode;
end;

end.
