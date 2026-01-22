unit CMRInviteListJsonAdapter;

{Adapter to parse JSON into TCMRInviteList arrays.
	Separates JSON parsing (infrastructure concern) from the domain object.}

interface

uses
	CMRInviteList;

type
	TCMRInviteListJsonAdapter = class
	public
		class function Parse(const JSON: WideString; var List: TCMRInviteList): Boolean; static;
	end;

implementation

uses
	System.Generics.Collections,
	SysUtils,
	CMRInvite,
	CMRConstants,
	JSONHelper,
	JSON;

class function TCMRInviteListJsonAdapter.Parse(const JSON: WideString; var List: TCMRInviteList): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
	J: Integer;
	A: TJSONArray;
begin
	Result := False;
	SetLength(List, 0);
	JSONVal := nil;
	try
		try
			if not init(JSON, JSONVal) then
				Exit;
			A := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITED] as TJSONArray;
			if not Assigned(A) then
				Exit(True); {no invites}
			SetLength(List, A.Count);
			for J := 0 to A.Count - 1 do
			begin
				ParserObj := A.Items[J] as TJSONObject;
				assignFromName(NAME_EMAIL, ParserObj, List[J].email);
				assignFromName(NAME_STATUS, ParserObj, List[J].status);
				assignFromName(NAME_ACCESS, ParserObj, List[J].access);
				assignFromName(NAME_NAME, ParserObj, List[J].name);
			end;
			Result := True;
		except
			Exit;
		end;
	finally
		JSONVal.Free;
	end;
end;

end.
