unit CloudMailRuSpaceInfo;

interface

uses
	CMRConstants,
	JSONHelper,
	JSON;

type

	TCloudMailRuSpaceInfo = record
		overquota: Boolean;
		total: int64;
		used: int64;
	End;

function getUserSpace(JSON: WideString; var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean;

implementation

function getUserSpace(JSON: WideString; var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		ParserObj := JSONVal.Values[NAME_BODY] as TJSONObject;
		with CloudMailRuSpaceInfo do
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
