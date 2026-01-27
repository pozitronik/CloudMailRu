unit CloudTwostepJsonAdapter;

{Adapter to parse JSON into TCloudTwostep records.
	Separates JSON parsing (infrastructure concern) from the domain object.}

interface

uses
	CloudTwostep;

type
	TCloudTwostepJsonAdapter = class
	public
		class function Parse(const JSON: WideString; out Twostep: TCloudTwostep): Boolean; static;
	end;

implementation

uses
	SysUtils,
	CloudConstants,
	JSONHelper,
	JSON;

class function TCloudTwostepJsonAdapter.Parse(const JSON: WideString; out Twostep: TCloudTwostep): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
begin
	{Initialize with safe defaults}
	Twostep.form_name := '';
	Twostep.auth_host := '';
	Twostep.secstep_phone := '';
	Twostep.secstep_page := '';
	Twostep.secstep_code_fail := '';
	Twostep.secstep_resend_fail := '';
	Twostep.secstep_resend_success := '';
	Twostep.secstep_timeout := 0;
	Twostep.secstep_login := '';
	Twostep.secstep_disposable_fail := '';
	Twostep.secstep_smsapi_error := '';
	Twostep.secstep_captcha := '';
	Twostep.totp_enabled := '';
	Twostep.locale := '';
	Twostep.client := '';
	Twostep.csrf := '';
	Twostep.device := '';

	Result := False;
	JSONVal := nil;
	try
		try
			if not init(JSON, JSONVal) then
				Exit;
			ParserObj := JSONVal as TJSONObject;

			assignFromName(NAME_FORM_NAME, ParserObj, Twostep.form_name);
			assignFromName(NAME_AUTH_HOST, ParserObj, Twostep.auth_host);
			assignFromName(NAME_SECSTEP_PHONE, ParserObj, Twostep.secstep_phone);
			assignFromName(NAME_SECSTEP_PAGE, ParserObj, Twostep.secstep_page);
			assignFromName(NAME_SECSTEP_CODE_FAIL, ParserObj, Twostep.secstep_code_fail);
			assignFromName(NAME_SECSTEP_RESEND_FAIL, ParserObj, Twostep.secstep_resend_fail);
			assignFromName(NAME_SECSTEP_RESEND_SUCCESS, ParserObj, Twostep.secstep_resend_success);
			if Assigned(ParserObj.Values[NAME_SECSTEP_TIMEOUT]) then
			begin
				if ParserObj.Values[NAME_SECSTEP_TIMEOUT].Value <> '' then
					Twostep.secstep_timeout := ParserObj.Values[NAME_SECSTEP_TIMEOUT].Value.ToInt64
				else
					Twostep.secstep_timeout := AUTH_APP_USED;
			end;
			assignFromName(NAME_SECSTEP_LOGIN, ParserObj, Twostep.secstep_login);
			assignFromName(NAME_SECSTEP_DISPOSABLE_FAIL, ParserObj, Twostep.secstep_disposable_fail);
			assignFromName(NAME_SECSTEP_SMSAPI_ERROR, ParserObj, Twostep.secstep_smsapi_error);
			assignFromName(NAME_SECSTEP_CAPTCHA, ParserObj, Twostep.secstep_captcha);
			assignFromName(NAME_TOTP_ENABLED, ParserObj, Twostep.totp_enabled);
			assignFromName(NAME_LOCALE, ParserObj, Twostep.locale);
			assignFromName(NAME_CLIENT, ParserObj, Twostep.client);
			assignFromName(NAME_CSRF, ParserObj, Twostep.csrf);
			assignFromName(NAME_DEVICE, ParserObj, Twostep.device);

			Result := True;
		except
			Exit;
		end;
	finally
		JSONVal.Free;
	end;
end;

end.
