unit ListingResultApplier;

{Interface and implementation for applying listing results to FsFindFirst output.
	Unifies common SetLastError and field assignment pattern.}

interface

uses
	Windows;

type
	{Common result data shared by all listing operations}
	TListingResultBase = record
		FileCounter: Integer;
		FindData: tWIN32FINDDATAW;
		ErrorCode: DWORD;
		Handle: THandle;
	end;

	IListingResultApplier = interface
		['{326874D7-E5BA-420B-B09E-5A898FEC3FE0}']

		{Applies common listing result fields to output variables.
			@param Base Common result fields from listing handler
			@param OutFindData FindData output parameter to set
			@param OutFileCounter FileCounter state to update
			@return Handle value for FsFindFirst result}
		function Apply(const Base: TListingResultBase; var OutFindData: tWIN32FINDDATAW; var OutFileCounter: Integer): THandle;
	end;

	TListingResultApplier = class(TInterfacedObject, IListingResultApplier)
	public
		function Apply(const Base: TListingResultBase; var OutFindData: tWIN32FINDDATAW; var OutFileCounter: Integer): THandle;
	end;

implementation

function TListingResultApplier.Apply(const Base: TListingResultBase; var OutFindData: tWIN32FINDDATAW; var OutFileCounter: Integer): THandle;
begin
	OutFileCounter := Base.FileCounter;
	OutFindData := Base.FindData;

	if Base.ErrorCode <> 0 then
		SetLastError(Base.ErrorCode);

	Result := Base.Handle;
end;

end.
