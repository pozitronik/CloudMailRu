unit IListingPathValidatorInterface;

{Interface for validating paths before directory listing.
 Checks virtual path constraints and verifies path resolves to a directory.}

interface

uses
	Windows,
	CMRDirItemList;

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
		function ValidatePath(IsVirtual, IsInAccountsList, IsPublicAccount: Boolean;
			const PathToFind: WideString; const Listing: TCMRDirItemList): TListingValidationResult;
	end;

implementation

end.
