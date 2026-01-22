unit CloudListingService;

{Service for directory listing operations on Cloud.Mail.ru}

interface

uses
	CMRDirItem,
	CMRDirItemList,
	CMRDirItemJsonAdapter,
	CMRDirItemListJsonAdapter,
	CMRIncomingInviteList,
	CMRIncomingInviteListJsonAdapter,
	CMROperationResult,
	CMROperationResultJsonAdapter,
	CMRSpace,
	CMRSpaceJsonAdapter,
	CMRConstants,
	CloudHTTP,
	FileCipher,
	PathHelper,
	StringHelper, {FormatSize}
	LANGUAGE_STRINGS,
	PLUGIN_TYPES,
	TCLogger,
	TokenRetryHelper,
	System.SysUtils;

type
	{Callback types for dynamic state access}
	TIsPublicAccountFunc = reference to function: Boolean;
	TGetUnitedParamsFunc = reference to function: WideString;
	TGetPublicLinkFunc = reference to function: WideString;
	TCloudResultToBooleanFunc = reference to function(JSON: WideString; ErrorPrefix: WideString): Boolean;
	TCloudResultToBooleanFromResultFunc = reference to function(OperationResult: TCMROperationResult; ErrorPrefix: WideString): Boolean;

	{Interface for listing service operations}
	ICloudListingService = interface
		['{2A923C8D-EB0B-4E3B-84DC-E372CD6C9AE5}']
		{Get directory listing for a path}
		function GetDirectory(Path: WideString; var Listing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		{Get shared links listing}
		function GetSharedLinks(var Listing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		{Get incoming invites listing (returns invite data)}
		function GetIncomingInvites(var Listing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean;
		{Get incoming invites as directory items with invite data}
		function GetIncomingInvitesAsDirItems(var DirListing: TCMRDirItemList; var InvitesListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean;
		{Get trashbin listing}
		function GetTrashbin(var Listing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		{Get file/folder status information}
		function StatusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
		{Restore item from trashbin}
		function TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		{Empty trashbin}
		function TrashbinEmpty(): Boolean;
		{Get user storage space information}
		function GetUserSpace(var SpaceInfo: TCMRSpace): Boolean;
		{Log user space information to logger}
		procedure LogUserSpaceInfo(Email: WideString);
	end;

	{Implementation of listing service}
	TCloudListingService = class(TInterfacedObject, ICloudListingService)
	private
		FHTTP: ICloudHTTP;
		FCipher: ICipher;
		FLogger: ILogger;
		FRetryOperation: TRetryOperation;
		FIsPublicAccount: TIsPublicAccountFunc;
		FGetUnitedParams: TGetUnitedParamsFunc;
		FGetPublicLink: TGetPublicLinkFunc;
		FCloudResultToBoolean: TCloudResultToBooleanFunc;
		FCloudResultToBooleanFromResult: TCloudResultToBooleanFromResultFunc;
		FDoCryptFilenames: Boolean;
	public
		constructor Create(HTTP: ICloudHTTP; Cipher: ICipher; Logger: ILogger; RetryOperation: TRetryOperation; IsPublicAccount: TIsPublicAccountFunc; GetUnitedParams: TGetUnitedParamsFunc; GetPublicLink: TGetPublicLinkFunc; CloudResultToBoolean: TCloudResultToBooleanFunc; CloudResultToBooleanFromResult: TCloudResultToBooleanFromResultFunc; DoCryptFilenames: Boolean);

		{ICloudListingService implementation}
		function GetDirectory(Path: WideString; var Listing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function GetSharedLinks(var Listing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function GetIncomingInvites(var Listing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean;
		function GetIncomingInvitesAsDirItems(var DirListing: TCMRDirItemList; var InvitesListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean;
		function GetTrashbin(var Listing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function StatusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
		function TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function TrashbinEmpty(): Boolean;
		function GetUserSpace(var SpaceInfo: TCMRSpace): Boolean;
		procedure LogUserSpaceInfo(Email: WideString);
	end;

implementation

{TCloudListingService}

constructor TCloudListingService.Create(HTTP: ICloudHTTP; Cipher: ICipher; Logger: ILogger; RetryOperation: TRetryOperation; IsPublicAccount: TIsPublicAccountFunc; GetUnitedParams: TGetUnitedParamsFunc; GetPublicLink: TGetPublicLinkFunc; CloudResultToBoolean: TCloudResultToBooleanFunc; CloudResultToBooleanFromResult: TCloudResultToBooleanFromResultFunc; DoCryptFilenames: Boolean);
begin
	inherited Create;
	FHTTP := HTTP;
	FCipher := Cipher;
	FLogger := Logger;
	FRetryOperation := RetryOperation;
	FIsPublicAccount := IsPublicAccount;
	FGetUnitedParams := GetUnitedParams;
	FGetPublicLink := GetPublicLink;
	FCloudResultToBoolean := CloudResultToBoolean;
	FCloudResultToBooleanFromResult := CloudResultToBooleanFromResult;
	FDoCryptFilenames := DoCryptFilenames;
end;

function TCloudListingService.GetDirectory(Path: WideString; var Listing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
var
	CallResult: TAPICallResult;
	LocalListing: TCMRDirItemList;
	UnitedParams: WideString;
begin
	SetLength(Listing, 0);
	UnitedParams := FGetUnitedParams();

	{Public accounts use different API endpoint}
	if FIsPublicAccount() then
	begin
		var
			JSON: WideString;
		var
			OperationResult: TCMROperationResult;
		Result := FHTTP.GetPage(Format('%s&weblink=%s%s&%s', [API_FOLDER, IncludeSlash(FGetPublicLink()), PathToUrl(Path, False), UnitedParams]), JSON, ShowProgress);
		if Result then
		begin
			TCMROperationResultJsonAdapter.Parse(JSON, OperationResult);
			Result := FCloudResultToBooleanFromResult(OperationResult, PREFIX_ERR_DIR_LISTING);
			if Result then
			begin
				Result := TCMRDirItemListJsonAdapter.Parse(JSON, Listing);
				if Result and FDoCryptFilenames and Assigned(FCipher) then
					FCipher.DecryptDirListing(Listing);
			end else if OperationResult.OperationResult = CLOUD_ERROR_NOT_EXISTS then
				FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_ERR_PATH_NOT_EXISTS, Path]);
		end;
		Exit;
	end;

	FHTTP.SetProgressNames(DIR_LISTING, Path);
	SetLength(LocalListing, 0);

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			OperationResult: TCMROperationResult;
			Success: Boolean;
		begin
			Success := FHTTP.GetPage(Format('%s&home=%s&%s', [API_FOLDER, PathToUrl(Path), UnitedParams]), JSON, ShowProgress);
			if Success then
			begin
				TCMROperationResultJsonAdapter.Parse(JSON, OperationResult);
				Success := FCloudResultToBooleanFromResult(OperationResult, PREFIX_ERR_DIR_LISTING);
				if Success then
				begin
					Success := TCMRDirItemListJsonAdapter.Parse(JSON, LocalListing);
					if Success and FDoCryptFilenames and Assigned(FCipher) then
						FCipher.DecryptDirListing(LocalListing);
				end else if OperationResult.OperationResult = CLOUD_ERROR_NOT_EXISTS then
					FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_ERR_PATH_NOT_EXISTS, Path]);
			end;
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		Listing := LocalListing;
end;

function TCloudListingService.GetSharedLinks(var Listing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
var
	CallResult: TAPICallResult;
	LocalListing: TCMRDirItemList;
	UnitedParams: WideString;
begin
	Result := False;
	SetLength(Listing, 0);

	if FIsPublicAccount() then
		Exit;

	UnitedParams := FGetUnitedParams();

	if ShowProgress then
		FHTTP.SetProgressNames(SHARED_LINKS_LISTING, UNKNOWN_ITEM);

	SetLength(LocalListing, 0);
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := FHTTP.GetPage(Format('%s?%s', [API_FOLDER_SHARED_LINKS, UnitedParams]), JSON, ShowProgress);
			if Success then
				Success := FCloudResultToBoolean(JSON, PREFIX_ERR_SHARED_LINKS_LISTING) and TCMRDirItemListJsonAdapter.Parse(JSON, LocalListing);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		Listing := LocalListing;
end;

function TCloudListingService.GetIncomingInvites(var Listing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean;
var
	CallResult: TAPICallResult;
	LocalListing: TCMRIncomingInviteList;
	UnitedParams: WideString;
begin
	Result := False;
	SetLength(Listing, 0);

	if FIsPublicAccount() then
		Exit;

	UnitedParams := FGetUnitedParams();

	if ShowProgress then
		FHTTP.SetProgressNames(INCOMING_LINKS_LISTING, UNKNOWN_ITEM);

	SetLength(LocalListing, 0);
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := FHTTP.GetPage(Format('%s?%s', [API_FOLDER_SHARED_INCOMING, UnitedParams]), JSON, ShowProgress);
			if Success then
				Success := FCloudResultToBoolean(JSON, PREFIX_ERR_INCOMING_REQUESTS_LISTING) and TCMRIncomingInviteListJsonAdapter.Parse(JSON, LocalListing);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		Listing := LocalListing;
end;

function TCloudListingService.GetIncomingInvitesAsDirItems(var DirListing: TCMRDirItemList; var InvitesListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean;
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

function TCloudListingService.GetTrashbin(var Listing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
var
	CallResult: TAPICallResult;
	LocalListing: TCMRDirItemList;
	UnitedParams: WideString;
begin
	Result := False;
	SetLength(Listing, 0);

	if FIsPublicAccount() then
		Exit;

	UnitedParams := FGetUnitedParams();

	if ShowProgress then
		FHTTP.SetProgressNames(TRASH_LISTING, UNKNOWN_ITEM);

	SetLength(LocalListing, 0);
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := FHTTP.GetPage(Format('%s?%s', [API_TRASHBIN, UnitedParams]), JSON, ShowProgress);
			if Success then
				Success := FCloudResultToBoolean(JSON, PREFIX_ERR_TRASH_LISTING) and TCMRDirItemListJsonAdapter.Parse(JSON, LocalListing);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		Listing := LocalListing;
end;

function TCloudListingService.StatusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
var
	CallResult: TAPICallResult;
	LocalInfo: TCMRDirItem;
	UnitedParams: WideString;
begin
	UnitedParams := FGetUnitedParams();

	{Public accounts use different API endpoint}
	if FIsPublicAccount() then
	begin
		var
			JSON: WideString;
		var
			Progress: Boolean := False;
		Result := FHTTP.GetPage(Format('%s?weblink=%s%s&%s', [API_FILE, IncludeSlash(FGetPublicLink()), PathToUrl(Path), UnitedParams]), JSON, Progress);
		if Result then
			Result := FCloudResultToBoolean(JSON, PREFIX_ERR_FILE_STATUS) and TCMRDirItemJsonAdapter.Parse(JSON, FileInfo);
		Exit;
	end;

	LocalInfo := default (TCMRDirItem);
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Progress: Boolean;
			Success: Boolean;
		begin
			Progress := False;
			Success := FHTTP.GetPage(Format('%s?home=%s&%s', [API_FILE, PathToUrl(Path), UnitedParams]), JSON, Progress);
			if Success then
				Success := FCloudResultToBoolean(JSON, PREFIX_ERR_FILE_STATUS) and TCMRDirItemJsonAdapter.Parse(JSON, LocalInfo);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		FileInfo := LocalInfo;
end;

function TCloudListingService.TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString): Boolean;
begin
	Result := False;
	if FIsPublicAccount() then
		Exit;
	Result := FRetryOperation.PostFormBoolean(API_TRASHBIN_RESTORE + '?' + FGetUnitedParams(), Format('path=%s&restore_revision=%d&conflict=%s', [PathToUrl(Path), RestoreRevision, ConflictMode]), PREFIX_ERR_FILE_RESTORE);
end;

function TCloudListingService.TrashbinEmpty(): Boolean;
begin
	Result := False;
	if FIsPublicAccount() then
		Exit;
	Result := FRetryOperation.PostFormBoolean(API_TRASHBIN_EMPTY + '?' + FGetUnitedParams(), EmptyWideStr, PREFIX_ERR_TRASH_CLEAN);
end;

function TCloudListingService.GetUserSpace(var SpaceInfo: TCMRSpace): Boolean;
var
	CallResult: TAPICallResult;
	LocalSpace: TCMRSpace;
begin
	LocalSpace := default(TCMRSpace);
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Progress: Boolean;
			Success: Boolean;
		begin
			Progress := False;
			Success := FHTTP.GetPage(Format('%s?%s', [API_USER_SPACE, FGetUnitedParams()]), JSON, Progress);
			if Success then
				Success := FCloudResultToBoolean(JSON, PREFIX_ERR_GET_USER_SPACE) and TCMRSpaceJsonAdapter.Parse(JSON, LocalSpace);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		SpaceInfo := LocalSpace;
end;

procedure TCloudListingService.LogUserSpaceInfo(Email: WideString);
var
	US: TCMRSpace;
	QuotaInfo: WideString;
begin
	if FIsPublicAccount() then
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
