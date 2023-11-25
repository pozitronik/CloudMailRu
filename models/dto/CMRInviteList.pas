unit CMRInviteList;

interface

uses
	System.Generics.Collections,
	SysUtils,
	CMRInvite,
	CMRConstants,
	JSONHelper,
	JSON;

type

	TCMRInviteList = TArray<TCMRInvite>;

	TCMRInviteListHelper = record helper for TCMRInviteList
		function FromJSON(JSON: WideString): Boolean;
	end;

implementation

{TCMRInviteListHelper}

function TCMRInviteListHelper.FromJSON(JSON: WideString): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	result := False;
	SetLength(self, 0);
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		A := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITED] as TJSONArray;
		if not Assigned(A) then
			Exit(true); //no invites
		SetLength(self, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with self[J] do
			begin
				assignFromName(NAME_EMAIL, ParserObj, email);
				assignFromName(NAME_STATUS, ParserObj, status);
				assignFromName(NAME_ACCESS, ParserObj, access);
				assignFromName(NAME_NAME, ParserObj, name);
			end;
		end;
	except
		on E: {EJSON}Exception do
		begin
			Exit;
		end;
	end;
	result := true;
	JSONVal.free;
end;

end.
