unit PathListingHandler;

{Path directory listing handler.
 Lists cloud directory contents for non-root paths.}

interface

uses
	Windows,
	IPathListingHandlerInterface,
	IListingProviderInterface,
	IListingPathValidatorInterface,
	ConnectionManager,
	PLUGIN_TYPES,
	RealPath,
	CMRDirItemList,
	CMRIncomingInviteList,
	CMRConstants,
	WindowsHelper;

type
	TPathListingHandler = class(TInterfacedObject, IPathListingHandler)
	private
		FConnectionManager: TConnectionManager;
		FListingProvider: IListingProvider;
		FListingPathValidator: IListingPathValidator;

		{Handles empty listing result}
		procedure HandleEmptyListing(var Result: TPathListingResult);

		{Handles non-empty listing result}
		procedure HandleNonEmptyListing(var Result: TPathListingResult);
	public
		constructor Create(
			ConnectionManager: TConnectionManager;
			ListingProvider: IListingProvider;
			ListingPathValidator: IListingPathValidator
		);
		function Execute(const Path: WideString): TPathListingResult;
	end;

implementation

uses
	CloudMailRu;

constructor TPathListingHandler.Create(
	ConnectionManager: TConnectionManager;
	ListingProvider: IListingProvider;
	ListingPathValidator: IListingPathValidator
);
begin
	inherited Create;
	FConnectionManager := ConnectionManager;
	FListingProvider := ListingProvider;
	FListingPathValidator := ListingPathValidator;
end;

procedure TPathListingHandler.HandleEmptyListing(var Result: TPathListingResult);
begin
	Result.FindData := GetFindDataEmptyDir();
	Result.Handle := FIND_NO_MORE_FILES;
	Result.ErrorCode := ERROR_NO_MORE_FILES;
end;

procedure TPathListingHandler.HandleNonEmptyListing(var Result: TPathListingResult);
begin
	Result.FindData := Result.Listing[0].ToFindData(Result.RealPath.sharedDir);
	Result.FileCounter := 1;
	if Result.RealPath.sharedDir then
		Result.Handle := FIND_SHARED_LINKS
	else
		Result.Handle := FIND_OK;
end;

function TPathListingHandler.Execute(const Path: WideString): TPathListingResult;
var
	getResult: Integer;
	CurrentCloud: TCloudMailRu;
	ValidationResult: TListingValidationResult;
begin
	Result.ErrorCode := 0;
	Result.FileCounter := 0;
	Result.Handle := INVALID_HANDLE_VALUE;
	Result.RealPath.FromPath(Path);

	{Get cloud connection}
	CurrentCloud := FConnectionManager.Get(Result.RealPath.account, getResult);

	if getResult <> CLOUD_OPERATION_OK then
	begin
		Result.ErrorCode := ERROR_ACCESS_DENIED;
		Exit;
	end;

	if not Assigned(CurrentCloud) then
	begin
		Result.ErrorCode := ERROR_PATH_NOT_FOUND;
		Exit;
	end;

	{Fetch listing from cloud}
	if not FListingProvider.FetchListing(CurrentCloud, Result.RealPath, Result.Listing, Result.IncomingInvites) then
		Result.ErrorCode := ERROR_PATH_NOT_FOUND;

	{Validate path can be listed}
	ValidationResult := FListingPathValidator.ValidatePath(
		Result.RealPath.isVirtual,
		Result.RealPath.isInAccountsList,
		CurrentCloud.IsPublicAccount,
		Result.RealPath.Path,
		Result.Listing
	);

	if not ValidationResult.IsValid then
	begin
		Result.ErrorCode := ValidationResult.ErrorCode;
		Exit;
	end;

	{Return listing result}
	if Length(Result.Listing) = 0 then
		HandleEmptyListing(Result)
	else
		HandleNonEmptyListing(Result);
end;

end.
