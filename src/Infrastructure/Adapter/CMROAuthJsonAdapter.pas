unit CMROAuthJsonAdapter;

{Adapter to parse JSON into TCMROAuth records.
	Separates JSON parsing (infrastructure concern) from the domain object.}

interface

uses
	CMROAuth;

type
	TCMROAuthJsonAdapter = class
	public
		class function Parse(const JSON: WideString; out OAuth: TCMROAuth): Boolean; static;
	end;

implementation

uses
	SysUtils,
	CMRConstants,
	LANGUAGE_STRINGS,
	JSONHelper,
	JSON;

class function TCMROAuthJsonAdapter.Parse(const JSON: WideString; out OAuth: TCMROAuth): Boolean;
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
