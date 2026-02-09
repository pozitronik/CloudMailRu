unit CloudListingService;

{Service for directory listing operations on Cloud.Mail.ru}

interface

uses
	CloudContext,
	CloudDirItem,
	CloudDirItemList,
	CloudDirItemJsonAdapter,
	CloudDirItemListJsonAdapter,
	CloudIncomingInviteList,
	CloudIncomingInviteListJsonAdapter,
	CloudOperationResult,
	CloudOperationResultJsonAdapter,
	CloudSpace,
	CloudSpaceJsonAdapter,
	CloudFileVersion,
	CloudFileVersionJsonAdapter,
	CloudConstants,
	CloudHTTP,
	Cipher,
	PathHelper,
	StringHelper, {FormatSize}
	LanguageStrings,
	WFXTypes,
	Logger,
	TokenRetryHelper,
	System.SysUtils;

type
	{Interface for listing service operations}
	ICloudListingService = interface
		['{2A923C8D-EB0B-4E3B-84DC-E372CD6C9AE5}']
		{Get directory listing for a path}
		function GetDirectory(Path: WideString; var Listing: TCloudDirItemList; ShowProgress: Boolean = False): Boolean;
		{Get shared links listing}
		function GetSharedLinks(var Listing: TCloudDirItemList; ShowProgress: Boolean = False): Boolean;
		{Get incoming invites listing (returns invite data)}
		function GetIncomingInvites(var Listing: TCloudIncomingInviteList; ShowProgress: Boolean = False): Boolean;
		{Get incoming invites as directory items with invite data}
		function GetIncomingInvitesAsDirItems(var DirListing: TCloudDirItemList; var InvitesListing: TCloudIncomingInviteList; ShowProgress: Boolean = False): Boolean;
		{Get trashbin listing}
		function GetTrashbin(var Listing: TCloudDirItemList; ShowProgress: Boolean = False): Boolean;
		{Get file/folder status information}
		function StatusFile(Path: WideString; var FileInfo: TCloudDirItem): Boolean;
		{Restore item from trashbin}
		function TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		{Empty trashbin}
		function TrashbinEmpty(): Boolean;
		{Get user storage space information}
		function GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
		{Log user space information to logger}
		procedure LogUserSpaceInfo(Email: WideString);
		{Get file version history}
		function GetFileHistory(Path: WideString; var Versions: TCloudFileVersionList): Boolean;
	end;

	{Callback type for parsing JSON response into a result}
	TListingParser<T> = reference to function(const JSON: WideString; var Output: T): Boolean;

	{Implementation of listing service}
	TCloudListingService = class(TInterfacedObject, ICloudListingService)
	private
		FContext: ICloudContext;
		FCipher: ICipher;
		FLogger: ILogger;
		FRetryOperation: IRetryOperation;

		{Generic helper for virtual folder listings (shared/invites/trash).
		 Handles public account check, progress, retry, and result copying.}
		function GetVirtualListing<T>(const Endpoint: WideString; const ProgressName: WideString; const ErrorPrefix: WideString; Parser:TListingParser<T>; var Listing: T; ShowProgress: Boolean): Boolean;
	public
		constructor Create(Context: ICloudContext; Cipher: ICipher; Logger: ILogger; RetryOperation: IRetryOperation);

		{ICloudListingService implementation}
		function GetDirectory(Path: WideString; var Listing: TCloudDirItemList; ShowProgress: Boolean = False): Boolean;
		function GetSharedLinks(var Listing: TCloudDirItemList; ShowProgress: Boolean = False): Boolean;
		function GetIncomingInvites(var Listing: TCloudIncomingInviteList; ShowProgress: Boolean = False): Boolean;
		function GetIncomingInvitesAsDirItems(var DirListing: TCloudDirItemList; var InvitesListing: TCloudIncomingInviteList; ShowProgress: Boolean = False): Boolean;
		function GetTrashbin(var Listing: TCloudDirItemList; ShowProgress: Boolean = False): Boolean;
		function StatusFile(Path: WideString; var FileInfo: TCloudDirItem): Boolean;
		function TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function TrashbinEmpty(): Boolean;
		function GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
		procedure LogUserSpaceInfo(Email: WideString);
		function GetFileHistory(Path: WideString; var Versions: TCloudFileVersionList): Boolean;
	end;

implementation

{TCloudListingService}

constructor TCloudListingService.Create(Context: ICloudContext; Cipher: ICipher; Logger: ILogger; RetryOperation: IRetryOperation);
begin
	inherited Create;
	FContext := Context;
	FCipher := Cipher;
	FLogger := Logger;
	FRetryOperation := RetryOperation;
end;

function TCloudListingService.GetDirectory(Path: WideString; var Listing: TCloudDirItemList; ShowProgress: Boolean = False): Boolean;
var
	CallResult: TAPICallResult;
	PageListing: TCloudDirItemList;
	Offset, ExpectedCount, PageExpectedCount: Integer;
begin
	SetLength(Listing, 0);

	{Public accounts use different API endpoint}
	if FContext.IsPublicAccount then
	begin
		var
			JSON: WideString;
		var
			OperationResult: TCloudOperationResult;
		Result := FContext.GetHTTP.GetPage(Format('%s&offset=0&limit=%d&weblink=%s%s&%s', [FContext.GetEndpoints.ApiFolder, API_FOLDER_LIMIT, IncludeSlash(FContext.GetPublicLink), PathToUrl(Path, False), FContext.GetUnitedParams]), JSON, ShowProgress);
		if Result then
		begin
			TCloudOperationResultJsonAdapter.Parse(JSON, OperationResult);
			Result := FContext.CloudResultToBoolean(OperationResult, PREFIX_ERR_DIR_LISTING);
			if Result then
			begin
				Result := TCloudDirItemListJsonAdapter.Parse(JSON, Listing);
			end else if OperationResult.OperationResult = CLOUD_ERROR_NOT_EXISTS then
				FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_ERR_PATH_NOT_EXISTS, Path]);
		end;
		Exit;
	end;

	FContext.GetHTTP.SetProgressNames(DIR_LISTING, Path);

	{Pagination loop: API silently caps responses at ~8000 items per request}
	Offset := 0;
	ExpectedCount := 0;

	repeat
		SetLength(PageListing, 0);
		PageExpectedCount := 0;

		CallResult := FRetryOperation.Execute(
			function: TAPICallResult
			var
				JSON: WideString;
				OperationResult: TCloudOperationResult;
				Success: Boolean;
			begin
				Success := FContext.GetHTTP.GetPage(Format('%s&offset=%d&limit=%d&home=%s&%s', [FContext.GetEndpoints.ApiFolder, Offset, API_FOLDER_LIMIT, PathToUrl(Path), FContext.GetUnitedParams]), JSON, ShowProgress);
				if Success then
				begin
					TCloudOperationResultJsonAdapter.Parse(JSON, OperationResult);
					Success := FContext.CloudResultToBoolean(OperationResult, PREFIX_ERR_DIR_LISTING);
					if Success then
					begin
						Success := TCloudDirItemListJsonAdapter.Parse(JSON, PageListing, PageExpectedCount);
					end else if OperationResult.OperationResult = CLOUD_ERROR_NOT_EXISTS then
						FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_ERR_PATH_NOT_EXISTS, Path]);
				end;
				Result := TAPICallResult.FromBoolean(Success, JSON);
			end);

		if not CallResult.Success then
		begin
			{First page failed -- return failure; subsequent page failed -- return what we have}
			Result := Length(Listing) > 0;
			Exit;
		end;

		{Use expected count from the first page (all pages report the same total)}
		if Offset = 0 then
			ExpectedCount := PageExpectedCount;

		Listing.Append(PageListing);
		Offset := Length(Listing);

	{Stop when: all items received, or empty page (safety), or no count info (single-page fallback)}
	until (Length(PageListing) = 0) or (ExpectedCount = 0) or (Length(Listing) >= ExpectedCount);

	Result := True;
end;

function TCloudListingService.GetVirtualListing<T>(const Endpoint: WideString; const ProgressName: WideString; 	const ErrorPrefix: WideString; Parser: TListingParser<T>; var Listing: T; ShowProgress: Boolean): Boolean;
var
	CallResult: TAPICallResult;
	LocalListing: T;
begin
	Result := False;
	Listing := Default(T);

	if FContext.IsPublicAccount then
		Exit;

	if ShowProgress then
		FContext.GetHTTP.SetProgressNames(ProgressName, UNKNOWN_ITEM);

	LocalListing := Default(T);
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := FContext.GetHTTP.GetPage(Format('%s?%s', [Endpoint, FContext.GetUnitedParams]), JSON, ShowProgress);
			if Success then
				Success := FContext.CloudResultToBoolean(JSON, ErrorPrefix) and Parser(JSON, LocalListing);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		Listing := LocalListing;
end;

function TCloudListingService.GetSharedLinks(var Listing: TCloudDirItemList; ShowProgress: Boolean = False): Boolean;
begin
	Result := GetVirtualListing<TCloudDirItemList>(
		FContext.GetEndpoints.ApiFolderSharedLinks,
		SHARED_LINKS_LISTING,
		PREFIX_ERR_SHARED_LINKS_LISTING,
		function(const JSON: WideString; var Output: TCloudDirItemList): Boolean
		begin
			Result := TCloudDirItemListJsonAdapter.Parse(JSON, Output);
		end,
		Listing,
		ShowProgress);
end;

function TCloudListingService.GetIncomingInvites(var Listing: TCloudIncomingInviteList; ShowProgress: Boolean = False): Boolean;
begin
	Result := GetVirtualListing<TCloudIncomingInviteList>(
		FContext.GetEndpoints.ApiFolderSharedIncoming,
		INCOMING_LINKS_LISTING,
		PREFIX_ERR_INCOMING_REQUESTS_LISTING,
		function(const JSON: WideString; var Output: TCloudIncomingInviteList): Boolean
		begin
			Result := TCloudIncomingInviteListJsonAdapter.Parse(JSON, Output);
		end,
		Listing,
		ShowProgress);
end;

function TCloudListingService.GetIncomingInvitesAsDirItems(var DirListing: TCloudDirItemList; var InvitesListing: TCloudIncomingInviteList; ShowProgress: Boolean = False): Boolean;
var
	i: Integer;
begin
	SetLength(DirListing, 0);
	Result := GetIncomingInvites(InvitesListing, ShowProgress);
	if Result then
	begin
		SetLength(DirListing, Length(InvitesListing));
		for i := 0 to Length(InvitesListing) - 1 do
		begin
			DirListing[i].name := InvitesListing[i].name;
			DirListing[i].size := InvitesListing[i].size;
			DirListing[i].tree := InvitesListing[i].tree;
		end;
	end;
end;

function TCloudListingService.GetTrashbin(var Listing: TCloudDirItemList; ShowProgress: Boolean = False): Boolean;
begin
	Result := GetVirtualListing<TCloudDirItemList>(
		FContext.GetEndpoints.ApiTrashbin,
		TRASH_LISTING,
		PREFIX_ERR_TRASH_LISTING,
		function(const JSON: WideString; var Output: TCloudDirItemList): Boolean
		begin
			Result := TCloudDirItemListJsonAdapter.Parse(JSON, Output);
		end,
		Listing,
		ShowProgress);
end;

function TCloudListingService.StatusFile(Path: WideString; var FileInfo: TCloudDirItem): Boolean;
var
	CallResult: TAPICallResult;
	LocalInfo: TCloudDirItem;
begin
	{Public accounts use different API endpoint}
	if FContext.IsPublicAccount then
	begin
		var
			JSON: WideString;
		var
			Progress: Boolean := False;
		Result := FContext.GetHTTP.GetPage(Format('%s?weblink=%s%s&%s', [FContext.GetEndpoints.ApiFile, IncludeSlash(FContext.GetPublicLink), PathToUrl(Path), FContext.GetUnitedParams]), JSON, Progress);
		if Result then
			Result := FContext.CloudResultToBoolean(JSON, PREFIX_ERR_FILE_STATUS) and TCloudDirItemJsonAdapter.Parse(JSON, FileInfo);
		Exit;
	end;

	LocalInfo := default (TCloudDirItem);
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Progress: Boolean;
			Success: Boolean;
		begin
			Progress := False;
			Success := FContext.GetHTTP.GetPage(Format('%s?home=%s&%s', [FContext.GetEndpoints.ApiFile, PathToUrl(Path), FContext.GetUnitedParams]), JSON, Progress);
			if Success then
				Success := FContext.CloudResultToBoolean(JSON, PREFIX_ERR_FILE_STATUS) and TCloudDirItemJsonAdapter.Parse(JSON, LocalInfo);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		FileInfo := LocalInfo;
end;

function TCloudListingService.TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString): Boolean;
begin
	Result := False;
	if FContext.IsPublicAccount then
		Exit;
	Result := FRetryOperation.PostFormBoolean(FContext.GetEndpoints.ApiTrashbinRestore, Format('path=%s&restore_revision=%d&conflict=%s', [PathToUrl(Path), RestoreRevision, ConflictMode]), PREFIX_ERR_FILE_RESTORE);
end;

function TCloudListingService.TrashbinEmpty(): Boolean;
begin
	Result := False;
	if FContext.IsPublicAccount then
		Exit;
	Result := FRetryOperation.PostFormBoolean(FContext.GetEndpoints.ApiTrashbinEmpty, EmptyWideStr, PREFIX_ERR_TRASH_CLEAN);
end;

function TCloudListingService.GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
var
	CallResult: TAPICallResult;
	LocalSpace: TCloudSpace;
begin
	LocalSpace := default(TCloudSpace);
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Progress: Boolean;
			Success: Boolean;
		begin
			Progress := False;
			Success := FContext.GetHTTP.GetPage(Format('%s?%s', [FContext.GetEndpoints.ApiUserSpace, FContext.GetUnitedParams]), JSON, Progress);
			if Success then
				Success := FContext.CloudResultToBoolean(JSON, PREFIX_ERR_GET_USER_SPACE) and TCloudSpaceJsonAdapter.Parse(JSON, LocalSpace);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		SpaceInfo := LocalSpace;
end;

function TCloudListingService.GetFileHistory(Path: WideString; var Versions: TCloudFileVersionList): Boolean;
var
	CallResult: TAPICallResult;
	LocalVersions: TCloudFileVersionList;
begin
	Result := False;
	SetLength(Versions, 0);

	if FContext.IsPublicAccount then
		Exit;

	SetLength(LocalVersions, 0);
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Progress: Boolean;
			Success: Boolean;
		begin
			Progress := False;
			Success := FContext.GetHTTP.GetPage(Format('%s?home=%s&%s', [FContext.GetEndpoints.ApiFileHistory, PathToUrl(Path), FContext.GetUnitedParams]), JSON, Progress);
			if Success then
				Success := FContext.CloudResultToBoolean(JSON, PREFIX_ERR_FILE_STATUS) and TCloudFileVersionJsonAdapter.Parse(JSON, LocalVersions);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		Versions := LocalVersions;
end;

procedure TCloudListingService.LogUserSpaceInfo(Email: WideString);
var
	US: TCloudSpace;
	QuotaInfo: WideString;
begin
	if FContext.IsPublicAccount then
		Exit;
	if GetUserSpace(US) then
	begin
		if US.overquota then
			QuotaInfo := WARN_QUOTA_EXHAUSTED
		else
			QuotaInfo := EmptyWideStr;
		FLogger.Log(LOG_LEVEL_FILE_OPERATION, MSGTYPE_DETAILS, USER_SPACE_INFO, [FormatSize(US.total), FormatSize(US.used), FormatSize(US.total - US.used), QuotaInfo]);
	end else begin
		FLogger.Log(LOG_LEVEL_DEBUG, MSGTYPE_IMPORTANTERROR, ERR_GET_USER_SPACE, [Email]);
	end;
end;

end.
