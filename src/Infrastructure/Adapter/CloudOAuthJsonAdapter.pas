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
	SafeJSON;

class function TCloudOAuthJsonAdapter.Parse(const JSON: WideString; out OAuth: TCloudOAuth): Boolean;
var
	Root: TSafeJSON;
begin
	OAuth := Default(TCloudOAuth);
	Result := False;

	Root := TSafeJSON.Parse(JSON);
	try
		if Root.IsNull then
		begin
			OAuth.error_code := CLOUD_ERROR_UNKNOWN;
			OAuth.error := ERR_PARSING_ANSWER;
			OAuth.error_description := Format(ERR_JSON_PARSING, [JSON]);
			Exit;
		end;

		OAuth.error := Root.Get(NAME_ERROR).AsString;
		OAuth.error_code := Root.Get(NAME_ERROR_CODE).AsInt;
		OAuth.error_description := Root.Get(NAME_ERROR_DESCRIPTION).AsString;
		OAuth.expires_in := Root.Get(NAME_EXPIRES_IN).AsInt;
		OAuth.refresh_token := Root.Get(NAME_REFRESH_TOKEN).AsString;
		OAuth.access_token := Root.Get(NAME_ACCESS_TOKEN).AsString;

		Result := True;
	finally
		Root.Free;
	end;
end;

end.
