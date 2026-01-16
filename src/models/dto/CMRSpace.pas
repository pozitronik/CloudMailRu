unit CMRSpace;

interface

uses
	CMRConstants,
	JSONHelper,
	JSON;

type

	TCMRSpace = record
		overquota: Boolean;
		total: int64;
		used: int64;

		function fromJSON(JSON: WideString): Boolean;
	End;

implementation

{TCloudMailRuSpaceInfo}

function TCMRSpace.fromJSON(JSON: WideString): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		ParserObj := JSONVal.Values[NAME_BODY] as TJSONObject;
		with self do
		begin
			assignFromName(NAME_OVERQUOTA, ParserObj, overquota);
			assignFromName(NAME_TOTAL, ParserObj, total);
			assignFromName(NAME_USED, ParserObj, used);
		end;
	except
		Exit;
	end;
	result := true;
	JSONVal.free;
end;

end.
