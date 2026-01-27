unit CloudOAuthJsonAdapter;

{Adapter to parse JSON into TCloudOAuth records.
	Separates JSON parsing (infrastructure concern) from the domain object.}

interface

uses
	CloudOAuth;

type
	TCloudOAuthJsonAdapter = class
	public
		class function Parse(const JSON: WideString; out OAuth: TCloudOAuth): Boolean; static;
	end;

implementation

uses
	SysUtils,
	CloudConstants,
	LanguageStrings,
	JSONHelper,
	JSON;

class function TCloudOAuthJsonAdapter.Parse(const JSON: WideString; out OAuth: TCloudOAuth): Boolean;
var
	JSONVal: TJSONObject;
begin
	{Initialize with safe defaults}
	OAuth.error := EmptyWideStr;
	OAuth.error_code := 0;
	OAuth.error_description := EmptyWideStr;
	OAuth.expires_in := 0;
	OAuth.refresh_token := EmptyWideStr;
	OAuth.access_token := EmptyWideStr;

	Result := False;
	JSONVal := nil;
	try
		try
			if not init(JSON, JSONVal) then
				Exit;

			assignFromName(NAME_ERROR, JSONVal, OAuth.error);
			assignFromName(NAME_ERROR_CODE, JSONVal, OAuth.error_code);
			assignFromName(NAME_ERROR_DESCRIPTION, JSONVal, OAuth.error_description);
			assignFromName(NAME_EXPIRES_IN, JSONVal, OAuth.expires_in);
			assignFromName(NAME_REFRESH_TOKEN, JSONVal, OAuth.refresh_token);
			assignFromName(NAME_ACCESS_TOKEN, JSONVal, OAuth.access_token);

			Result := True;
		except
			on E: Exception do
			begin
				OAuth.error_code := CLOUD_ERROR_UNKNOWN;
				OAuth.error := ERR_PARSING_ANSWER;
				OAuth.error_description := Format(ERR_JSON_PARSING, [JSON]);
			end;
		end;
	finally
		JSONVal.Free;
	end;
end;

end.
