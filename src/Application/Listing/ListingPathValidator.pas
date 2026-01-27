unit ListingPathValidator;

{Interface and implementation for validating paths before directory listing.
	Checks virtual path constraints (can't list inside virtual directory objects)
	and verifies the path resolves to a directory.}

interface

uses
	Windows,
	CloudDirItem,
	CloudDirItemList;

type
	{Result of path validation for listing}
	TListingValidationResult = record
		IsValid: Boolean;
		ErrorCode: Cardinal; {0 = OK, ERROR_ACCESS_DENIED, ERROR_PATH_NOT_FOUND}
	end;

	IListingPathValidator = interface
		['{8F3B2C91-D5E7-4A6F-B8C2-3E1D9F7A4B5C}']

		{Validates that a path can be listed.
			Checks virtual path constraints and verifies path is a directory.
			@param IsVirtual True if path is in virtual directory (.trash/.shared/.invites)
			@param IsInAccountsList True if path is at account level in virtual dir
			@param IsPublicAccount True if account is public
			@param PathToFind Path component to find in listing
			@param Listing Current directory listing to search
			@return Validation result with IsValid and ErrorCode}
		function ValidatePath(IsVirtual, IsInAccountsList, IsPublicAccount: Boolean; const PathToFind: WideString; const Listing: TCloudDirItemList): TListingValidationResult;
	end;

	TListingPathValidator = class(TInterfacedObject, IListingPathValidator)
	public
		function ValidatePath(IsVirtual, IsInAccountsList, IsPublicAccount: Boolean; const PathToFind: WideString; const Listing: TCloudDirItemList): TListingValidationResult;
	end;

implementation

uses
	PathHelper;

function TListingPathValidator.ValidatePath(IsVirtual, IsInAccountsList, IsPublicAccount: Boolean; const PathToFind: WideString; const Listing: TCloudDirItemList): TListingValidationResult;
var
	CurrentItem: TCloudDirItem;
begin
	Result.IsValid := True;
	Result.ErrorCode := 0;

	{Virtual paths (inside .trash/.shared/.invites) can only be listed at account level}
	if IsVirtual and not IsInAccountsList then
	begin
		Result.IsValid := False;
		Result.ErrorCode := ERROR_ACCESS_DENIED;
		Exit;
	end;

	{Find the item matching the path - different lookup for public vs private accounts}
	if IsPublicAccount then
		CurrentItem := Listing.FindByName(ExtractUniversalFileName(PathToFind))
	else
		CurrentItem := Listing.FindByHomePath(PathToFind);

	{Path must resolve to a directory (or be empty/not found which means listing empty dir)}
	if not(CurrentItem.isNone or CurrentItem.isDir) then
	begin
		Result.IsValid := False;
		Result.ErrorCode := ERROR_PATH_NOT_FOUND;
	end;
end;

end.
