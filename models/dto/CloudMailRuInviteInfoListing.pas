unit CloudMailRuInviteInfoListing;

interface

uses
	System.Generics.Collections,
	SysUtils,
	CloudMailRuInviteInfo,
	CMRConstants,
	JSONHelper,
	JSON;

type

	TCloudMailRuInviteInfoListing = TArray<TCloudMailRuInviteInfo>;
function getInviteListing(JSON: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;

implementation

function getInviteListing(JSON: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	result := False;
	SetLength(InviteListing, 0);
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		A := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITED] as TJSONArray;
		if not Assigned(A) then
			Exit(true); //no invites
		SetLength(InviteListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with InviteListing[J] do
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
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			Exit;
		end;
	end;
	result := true;
	JSONVal.free;
end;

end.
