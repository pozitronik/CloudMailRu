unit CMRSpaceJsonAdapter;

{Adapter to parse JSON into TCMRSpace records.
	Separates JSON parsing (infrastructure concern) from the domain object.}

interface

uses
	CMRSpace;

type
	TCMRSpaceJsonAdapter = class
	public
		class function Parse(const JSON: WideString; out Space: TCMRSpace): Boolean; static;
	end;

implementation

uses
	SysUtils,
	CMRConstants,
	JSONHelper,
	JSON;

class function TCMRSpaceJsonAdapter.Parse(const JSON: WideString; out Space: TCMRSpace): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
begin
	{Initialize with safe defaults}
	Space.overquota := False;
	Space.total := 0;
	Space.used := 0;

	Result := False;
	JSONVal := nil;
	try
		try
			if not init(JSON, JSONVal) then
				Exit;
			ParserObj := JSONVal.Values[NAME_BODY] as TJSONObject;

			assignFromName(NAME_OVERQUOTA, ParserObj, Space.overquota);
			assignFromName(NAME_TOTAL, ParserObj, Space.total);
			assignFromName(NAME_USED, ParserObj, Space.used);

			Result := True;
		except
			Exit;
		end;
	finally
		JSONVal.Free;
	end;
end;

end.
