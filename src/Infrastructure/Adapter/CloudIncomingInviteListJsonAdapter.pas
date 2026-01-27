unit CloudIncomingInviteListJsonAdapter;

{Adapter to parse JSON into TCloudIncomingInviteList arrays.
	Separates JSON parsing (infrastructure concern) from the domain object.}

interface

uses
	CloudIncomingInviteList;

type
	TCloudIncomingInviteListJsonAdapter = class
	public
		class function Parse(const JSON: WideString; var List: TCloudIncomingInviteList): Boolean; static;
	end;

implementation

uses
	System.Generics.Collections,
	SysUtils,
	CloudIncomingInvite,
	CloudConstants,
	JSONHelper,
	JSON;

class function TCloudIncomingInviteListJsonAdapter.Parse(const JSON: WideString; var List: TCloudIncomingInviteList): Boolean;
var
	JSONVal: TJSONObject;
	OwnerObj: TJSONObject;
	ParserObj: TJSONObject;
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
			A := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_LIST] as TJSONArray;
			if not Assigned(A) then
				Exit; {no invites}
			SetLength(List, A.Count);
			for J := 0 to A.Count - 1 do
			begin
				ParserObj := A.Items[J] as TJSONObject;

				if Assigned(ParserObj.Values[NAME_OWNER]) then
				begin
					OwnerObj := ParserObj.Values[NAME_OWNER] as TJSONObject;
					if Assigned(OwnerObj.Values[NAME_EMAIL]) then
						List[J].owner.email := OwnerObj.Values[NAME_EMAIL].Value;
					if Assigned(OwnerObj.Values[NAME_NAME]) then
						List[J].owner.name := OwnerObj.Values[NAME_NAME].Value;
				end;

				assignFromName(NAME_TREE, ParserObj, List[J].tree);
				assignFromName(NAME_ACCESS, ParserObj, List[J].access);
				assignFromName(NAME_NAME, ParserObj, List[J].name);
				assignFromName(NAME_HOME, ParserObj, List[J].home);
				assignFromName(NAME_SIZE, ParserObj, List[J].size);
				assignFromName(NAME_INVITE_TOKEN, ParserObj, List[J].invite_token);
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
