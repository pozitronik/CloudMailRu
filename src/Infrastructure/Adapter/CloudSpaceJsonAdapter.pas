unit CloudSpaceJsonAdapter;

{Adapter to parse JSON into TCloudSpace records.
	Separates JSON parsing (infrastructure concern) from the domain object.}

interface

uses
	CloudSpace;

type
	TCloudSpaceJsonAdapter = class
	public
		class function Parse(const JSON: WideString; out Space: TCloudSpace): Boolean; static;
	end;

implementation

uses
	CloudConstants,
	SafeJSON;

class function TCloudSpaceJsonAdapter.Parse(const JSON: WideString; out Space: TCloudSpace): Boolean;
var
	Root, Body: TSafeJSON;
begin
	Space := Default(TCloudSpace);
	Result := False;

	Root := TSafeJSON.Parse(JSON);
	try
		if Root.IsNull then
			Exit;

		Body := Root.Get(NAME_BODY);
		if Body.IsNull then
			Exit;

		Space.overquota := Body.Get(NAME_OVERQUOTA).AsBool;
		Space.total := Body.Get(NAME_TOTAL).AsInt64;
		Space.used := Body.Get(NAME_USED).AsInt64;

		Result := True;
	finally
		Root.Free;
	end;
end;

end.
