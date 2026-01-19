unit ListingPathValidator;

{Validates paths before directory listing.
 Checks virtual path constraints (can't list inside virtual directory objects)
 and verifies the path resolves to a directory.}

interface

uses
	Windows,
	CMRDirItem,
	CMRDirItemList,
	IListingPathValidatorInterface;

type
	TListingPathValidator = class(TInterfacedObject, IListingPathValidator)
	public
		function ValidatePath(IsVirtual, IsInAccountsList, IsPublicAccount: Boolean;
			const PathToFind: WideString; const Listing: TCMRDirItemList): TListingValidationResult;
	end;

implementation

uses
	PathHelper;

function TListingPathValidator.ValidatePath(IsVirtual, IsInAccountsList, IsPublicAccount: Boolean;
	const PathToFind: WideString; const Listing: TCMRDirItemList): TListingValidationResult;
var
	CurrentItem: TCMRDirItem;
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
