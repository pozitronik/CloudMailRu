unit CloudShareService;

{Service for sharing and publishing operations on Cloud.Mail.ru}

interface

uses
	CMRDirItem,
	CMRInviteList,
	CMRInviteListJsonAdapter,
	CMRConstants,
	CloudHTTP,
	CloudShardManager,
	JSONHelper,
	ParsingHelper,
	LANGUAGE_STRINGS,
	PathHelper,
	WFXTypes,
	StringHelper,
	TCLogger,
	TokenRetryHelper,
	DCPbase64,
	System.SysUtils;

type
	{Callback types for dynamic state access}
	TIsPublicAccountFunc = reference to function: Boolean;
	TGetUnitedParamsFunc = reference to function: WideString;
	TCloudResultToBooleanFunc = reference to function(JSON: WideString; ErrorPrefix: WideString): Boolean;
	TCloudResultToFsResultFunc = reference to function(JSON: WideString; ErrorPrefix: WideString): Integer;

	{Interface for share service operations}
	ICloudShareService = interface
		['{B8C9D0E1-F2A3-4B5C-6D7E-8F9A0B1C2D3E}']
		{Publish or unpublish a file/folder, returns public link when publishing}
		function Publish(Path: WideString; var PublicLink: WideString): Boolean;
		function Unpublish(Path: WideString; PublicLink: WideString): Boolean;
		{Get sharing information for a path}
		function GetShareInfo(Path: WideString; var InviteListing: TCMRInviteList): Boolean;
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
		function GetPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; Publish: Boolean = CLOUD_PUBLISH): Boolean;
		{Clone a public weblink to user's cloud}
		function CloneWeblink(Path, Link: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Integer;
	end;

	{Implementation of share service}
	TCloudShareService = class(TInterfacedObject, ICloudShareService)
	private
		FHTTP: ICloudHTTP;
		FLogger: ILogger;
		FRetryOperation: TRetryOperation;
		FIsPublicAccount: TIsPublicAccountFunc;
		FGetUnitedParams: TGetUnitedParamsFunc;
		FCloudResultToBoolean: TCloudResultToBooleanFunc;
		FCloudResultToFsResult: TCloudResultToFsResultFunc;
		FShardManager: ICloudShardManager;
	public
		constructor Create(HTTP: ICloudHTTP; Logger: ILogger; RetryOperation: TRetryOperation; IsPublicAccount: TIsPublicAccountFunc; GetUnitedParams: TGetUnitedParamsFunc; CloudResultToBoolean: TCloudResultToBooleanFunc; CloudResultToFsResult: TCloudResultToFsResultFunc; ShardManager: ICloudShardManager);

		{ICloudShareService implementation}
		function Publish(Path: WideString; var PublicLink: WideString): Boolean;
		function Unpublish(Path: WideString; PublicLink: WideString): Boolean;
		function GetShareInfo(Path: WideString; var InviteListing: TCMRInviteList): Boolean;
		function Share(Path, Email: WideString; Access: Integer): Boolean;
		function Unshare(Path, Email: WideString): Boolean;
		function Mount(Home, InviteToken: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function Unmount(Home: WideString; CloneCopy: Boolean): Boolean;
		function RejectInvite(InviteToken: WideString): Boolean;
		function GetPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; Publish: Boolean = CLOUD_PUBLISH): Boolean;
		function CloneWeblink(Path, Link: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Integer;
	end;

implementation

{TCloudShareService}

constructor TCloudShareService.Create(HTTP: ICloudHTTP; Logger: ILogger; RetryOperation: TRetryOperation; IsPublicAccount: TIsPublicAccountFunc; GetUnitedParams: TGetUnitedParamsFunc; CloudResultToBoolean: TCloudResultToBooleanFunc; CloudResultToFsResult: TCloudResultToFsResultFunc; ShardManager: ICloudShardManager);
begin
	inherited Create;
	FHTTP := HTTP;
	FLogger := Logger;
	FRetryOperation := RetryOperation;
	FIsPublicAccount := IsPublicAccount;
	FGetUnitedParams := GetUnitedParams;
	FCloudResultToBoolean := CloudResultToBoolean;
	FCloudResultToFsResult := CloudResultToFsResult;
	FShardManager := ShardManager;
end;

function TCloudShareService.Publish(Path: WideString; var PublicLink: WideString): Boolean;
var
	CallResult: TAPICallResult;
	ExtractedLink: WideString;
	UnitedParams: WideString;
begin
	Result := False;
	if FIsPublicAccount() then
		Exit;

	UnitedParams := FGetUnitedParams();
	ExtractedLink := '';

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := FHTTP.PostForm(API_FILE_PUBLISH + '?' + UnitedParams, Format('home=/%s&conflict', [PathToUrl(Path)]), JSON, 'application/x-www-form-urlencoded', true, False);

			if Success then
				Success := FCloudResultToBoolean(JSON, PREFIX_ERR_FILE_PUBLISH);

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
	UnitedParams: WideString;
	CurrentLink: WideString;
begin
	Result := False;
	if FIsPublicAccount() then
		Exit;

	UnitedParams := FGetUnitedParams();
	CurrentLink := PublicLink;

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := FHTTP.PostForm(API_FILE_UNPUBLISH + '?' + UnitedParams, Format('weblink=%s&conflict', [CurrentLink]), JSON, 'application/x-www-form-urlencoded', true, False);

			if Success then
				Success := FCloudResultToBoolean(JSON, PREFIX_ERR_FILE_PUBLISH);

			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
end;

function TCloudShareService.GetShareInfo(Path: WideString; var InviteListing: TCMRInviteList): Boolean;
var
	CallResult: TAPICallResult;
	LocalListing: TCMRInviteList;
	UnitedParams: WideString;
begin
	UnitedParams := FGetUnitedParams();
	SetLength(LocalListing, 0);

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Progress: Boolean;
			Success: Boolean;
		begin
			Progress := False;
			Success := FHTTP.GetPage(Format('%s?home=%s&%s', [API_FOLDER_SHARED_INFO, PathToUrl(Path), UnitedParams]), JSON, Progress);
			if Success then
				Success := TCMRInviteListJsonAdapter.Parse(JSON, LocalListing);
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
	UnitedParams: WideString;
begin
	if not(Access in [CLOUD_SHARE_RW, CLOUD_SHARE_RO]) then
		Exit(False);

	UnitedParams := FGetUnitedParams();

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
			Success := FHTTP.PostForm(API_FOLDER_SHARE + '?' + UnitedParams, Format('home=/%s&invite={"email":"%s","access":"%s"}', [PathToUrl(Path), Email, AccessString]), JSON);

			if Success then
				Success := FCloudResultToBoolean(JSON, PREFIX_ERR_INVITE_MEMBER);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
end;

function TCloudShareService.Unshare(Path, Email: WideString): Boolean;
var
	CallResult: TAPICallResult;
	UnitedParams: WideString;
begin
	UnitedParams := FGetUnitedParams();

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := FHTTP.PostForm(API_FOLDER_UNSHARE + '?' + UnitedParams, Format('home=/%s&invite={"email":"%s"}', [PathToUrl(Path), Email]), JSON);

			if Success then
				Success := FCloudResultToBoolean(JSON, PREFIX_ERR_INVITE_MEMBER);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
end;

function TCloudShareService.Mount(Home, InviteToken: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
begin
	Result := False;
	if FIsPublicAccount() then
		Exit;

	Result := FRetryOperation.PostFormBoolean(API_FOLDER_MOUNT + '?' + FGetUnitedParams(), Format('home=%s&invite_token=%s&conflict=%s', [UrlEncode(Home), InviteToken, ConflictMode]), PREFIX_ERR_FOLDER_MOUNT);
end;

function TCloudShareService.Unmount(Home: WideString; CloneCopy: Boolean): Boolean;
var
	CopyStr: WideString;
begin
	Result := False;
	if FIsPublicAccount() then
		Exit;

	if CloneCopy then
		CopyStr := 'true'
	else
		CopyStr := 'false';

	Result := FRetryOperation.PostFormBoolean(API_FOLDER_UNMOUNT + '?' + FGetUnitedParams(), Format('home=%s&clone_copy=%s', [UrlEncode(Home), CopyStr]), PREFIX_ERR_FOLDER_UNMOUNT);
end;

function TCloudShareService.RejectInvite(InviteToken: WideString): Boolean;
begin
	Result := False;
	if FIsPublicAccount() then
		Exit;

	Result := FRetryOperation.PostFormBoolean(API_INVITE_REJECT + '?' + FGetUnitedParams(), Format('invite_token=%s', [InviteToken]), PREFIX_ERR_INVITE_REJECT);
end;

function TCloudShareService.GetPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; Publish: Boolean = CLOUD_PUBLISH): Boolean;
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
	UnitedParams: WideString;
begin
	if FIsPublicAccount() then
		Exit(FS_FILE_NOTSUPPORTED);

	UnitedParams := FGetUnitedParams();

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Progress: Boolean;
			ResultCode: Integer;
		begin
			Progress := true;
			if FHTTP.GetPage(Format('%s?folder=/%s&weblink=%s&conflict=%s&%s', [API_CLONE, PathToUrl(Path), Link, ConflictMode, UnitedParams]), JSON, Progress) then
			begin
				ResultCode := FCloudResultToFsResult(JSON, PREFIX_ERR_FILE_PUBLISH);
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
