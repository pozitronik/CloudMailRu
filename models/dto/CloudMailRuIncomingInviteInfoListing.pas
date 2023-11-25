unit CloudMailRuIncomingInviteInfoListing;

interface

uses
	System.Generics.Collections,
	SysUtils,
	RealPath,
	CMRIncomingInvite,
	CMRConstants,
	JSONHelper,
	JSON;

type
	TCloudMailRuIncomingInviteInfoListing = TArray<TCMRIncomingInvite>;

function FindByName(InviteListing: TCloudMailRuIncomingInviteInfoListing; ItemName: WideString): TCMRIncomingInvite;
function getIncomingInviteListing(JSON: WideString; var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean;

implementation

function FindByName(InviteListing: TCloudMailRuIncomingInviteInfoListing; ItemName: WideString): TCMRIncomingInvite;
var
	CurrentItem: TCMRIncomingInvite;
begin
	for CurrentItem in InviteListing do
		if CurrentItem.name = ItemName then
			exit(CurrentItem);
end;

function getIncomingInviteListing(JSON: WideString; var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean;
var
	JSONVal: TJSONObject;
	OwnerObj: TJSONObject;
	ParserObj: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	result := False;

	SetLength(IncomingInviteListing, 0);
	try
		if (not init(JSON, JSONVal)) then
			exit;
		A := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_LIST] as TJSONArray;
		if not Assigned(A) then
			exit; //no invites
		SetLength(IncomingInviteListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with IncomingInviteListing[J] do
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
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			exit;
		end;
	end;
	result := true;
	JSONVal.free;
end;

end.
