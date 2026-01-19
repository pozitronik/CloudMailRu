unit ListingResultApplier;

{Applies listing results to FsFindFirst output.
 Unifies common SetLastError and field assignment pattern.}

interface

uses
	Windows,
	IListingResultApplierInterface;

type
	TListingResultApplier = class(TInterfacedObject, IListingResultApplier)
	public
		function Apply(const Base: TListingResultBase;
			var OutFindData: tWIN32FINDDATAW;
			var OutFileCounter: Integer): THandle;
	end;

implementation

function TListingResultApplier.Apply(const Base: TListingResultBase;
	var OutFindData: tWIN32FINDDATAW;
	var OutFileCounter: Integer): THandle;
begin
	OutFileCounter := Base.FileCounter;
	OutFindData := Base.FindData;

	if Base.ErrorCode <> 0 then
		SetLastError(Base.ErrorCode);

	Result := Base.Handle;
end;

end.
