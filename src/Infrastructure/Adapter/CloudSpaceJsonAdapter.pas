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
	SysUtils,
	CloudConstants,
	JSONHelper,
	JSON;

class function TCloudSpaceJsonAdapter.Parse(const JSON: WideString; out Space: TCloudSpace): Boolean;
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
