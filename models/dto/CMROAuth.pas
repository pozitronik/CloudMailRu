unit CMROAuth;

interface

uses
	SysUtils,
	CMRConstants,
	LANGUAGE_STRINGS,
	JSONHelper,
	JSON;

type
	TCMROAuth = Record
		error: WideString;
		error_code: integer;
		error_description: WideString;
		expires_in: integer;
		refresh_token: WideString;
		access_token: WideString;
		function fromJSON(JSON: WideString): Boolean;
	end;

implementation

{TCloudMailRuOAuthInfo}

function TCMROAuth.fromJSON(JSON: WideString): Boolean;
var
	JSONVal: TJSONObject;
begin
	{ Initialize with safe defaults }
	self.error := EmptyWideStr;
	self.error_code := 0;
	self.error_description := EmptyWideStr;
	self.expires_in := 0;
	self.refresh_token := EmptyWideStr;
	self.access_token := EmptyWideStr;

	result := False;
	JSONVal := nil;
	try
		try
			if (not init(JSON, JSONVal)) then
				Exit;
			with self do
			begin
				assignFromName(NAME_ERROR, JSONVal, error);
				assignFromName(NAME_ERROR_CODE, JSONVal, error_code);
				assignFromName(NAME_ERROR_DESCRIPTION, JSONVal, error_description);
				assignFromName(NAME_EXPIRES_IN, JSONVal, expires_in);
				assignFromName(NAME_REFRESH_TOKEN, JSONVal, refresh_token);
				assignFromName(NAME_ACCESS_TOKEN, JSONVal, access_token);
			end;
			result := true;
		except
			on E: Exception do
			begin
				self.error_code := CLOUD_ERROR_UNKNOWN;
				self.error := ERR_PARSING_ANSWER;
				self.error_description := Format(ERR_JSON_PARSING, [JSON]);
			end;
		end;
	finally
		JSONVal.Free;
	end;
end;

end.
