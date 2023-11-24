unit CloudMailRuOAuthInfo;

interface

uses
	SysUtils,
	CMRConstants,
	JSONHelper,
	JSON;

type
	TCloudMailRuOAuthInfo = Record
		error: WideString;
		error_code: integer;
		error_description: WideString;
		expires_in: integer;
		refresh_token: WideString;
		access_token: WideString;
	end;

function getOAuthTokenInfo(JSON: WideString; var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean;

implementation

function getOAuthTokenInfo(JSON: WideString; var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean;
var
	JSONVal: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		with CloudMailRuOAuthInfo do
		begin
			assignFromName(NAME_ERROR, JSONVal, error);
			assignFromName(NAME_ERROR_CODE, JSONVal, error_code);
			assignFromName(NAME_ERROR_DESCRIPTION, JSONVal, error_description);
			assignFromName(NAME_EXPIRES_IN, JSONVal, expires_in);
			assignFromName(NAME_REFRESH_TOKEN, JSONVal, refresh_token);
			assignFromName(NAME_ACCESS_TOKEN, JSONVal, access_token);
		end;
	except
		on E: {EJSON}Exception do
		begin
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			CloudMailRuOAuthInfo.error_code := CLOUD_ERROR_UNKNOWN;
			CloudMailRuOAuthInfo.error := 'Answer parsing';
			CloudMailRuOAuthInfo.error_description := 'JSON parsing error: at ' + JSON;
			Exit;
		end;
	end;
	result := true;
	JSONVal.free;
end;

end.
