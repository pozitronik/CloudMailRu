unit CMRIncomingInviteList;

interface

uses
	System.Generics.Collections,
	SysUtils,
	CMRIncomingInvite,
	CMRConstants,
	JSONHelper,
	JSON;

type
	TCMRIncomingInviteList = TArray<TCMRIncomingInvite>;

	TCMRIncomingInviteListHelper = record helper for TCMRIncomingInviteList
		function FromJSON(JSON: WideString): Boolean;
		function FindByName(ItemName: WideString): TCMRIncomingInvite;
	end;

implementation

{TCMRIncomingInviteListHelper}

function TCMRIncomingInviteListHelper.FindByName(ItemName: WideString): TCMRIncomingInvite;
var
	CurrentItem: TCMRIncomingInvite;
begin
	for CurrentItem in self do
		if CurrentItem.name = ItemName then
			exit(CurrentItem);
	exit(CurrentItem.None)
end;

function TCMRIncomingInviteListHelper.FromJSON(JSON: WideString): Boolean;
var
	JSONVal: TJSONObject;
	OwnerObj: TJSONObject;
	ParserObj: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	result := False;

	SetLength(self, 0);
	try
		if (not init(JSON, JSONVal)) then
			exit;
		A := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_LIST] as TJSONArray;
		if not Assigned(A) then
			exit; //no invites
		SetLength(self, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with self[J] do
			begin
				if Assigned(ParserObj.Values[NAME_OWNER]) then
				begin
					OwnerObj := ParserObj.Values[NAME_OWNER] as TJSONObject;
					if Assigned(OwnerObj.Values[NAME_EMAIL]) then
						owner.email := OwnerObj.Values[NAME_EMAIL].Value;
					if Assigned(OwnerObj.Values[NAME_NAME]) then
						owner.name := OwnerObj.Values[NAME_NAME].Value;
				end;

				assignFromName(NAME_TREE, ParserObj, tree);
				assignFromName(NAME_ACCESS, ParserObj, access);
				assignFromName(NAME_NAME, ParserObj, name);
				assignFromName(NAME_HOME, ParserObj, home);
				assignFromName(NAME_SIZE, ParserObj, size);
				assignFromName(NAME_INVITE_TOKEN, ParserObj, invite_token);
			end;
		end;
	except
		on E: {EJSON}Exception do
		begin
			exit;
		end;
	end;
	result := true;
	JSONVal.free;
end;

end.
