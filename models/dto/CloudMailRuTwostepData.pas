unit CloudMailRuTwostepData;

interface

uses
	SysUtils,
	CMRConstants,
	JSONHelper,
	JSON;

type

	TCloudMailRuTwostepData = record
		form_name: WideString;
		auth_host: WideString;
		secstep_phone: WideString;
		secstep_page: WideString;
		secstep_code_fail: WideString;
		secstep_resend_fail: WideString;
		secstep_resend_success: WideString;
		secstep_timeout: int64;
		secstep_login: WideString;
		secstep_disposable_fail: WideString;
		secstep_smsapi_error: WideString;
		secstep_captcha: WideString;
		totp_enabled: WideString;
		locale: WideString;
		client: WideString;
		csrf: WideString;
		device: WideString;
		{some items skipped}
	end;

function getTwostepData(JSON: WideString; var TwostepData: TCloudMailRuTwostepData): Boolean;

implementation

function getTwostepData(JSON: WideString; var TwostepData: TCloudMailRuTwostepData): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		ParserObj := JSONVal as TJSONObject; //не менять
		with TwostepData do
		begin
			assignFromName(NAME_FORM_NAME, ParserObj, form_name);
			assignFromName(NAME_AUTH_HOST, ParserObj, auth_host);
			assignFromName(NAME_SECSTEP_PHONE, ParserObj, secstep_phone);
			assignFromName(NAME_SECSTEP_PAGE, ParserObj, secstep_page);
			assignFromName(NAME_SECSTEP_CODE_FAIL, ParserObj, secstep_code_fail);
			assignFromName(NAME_SECSTEP_RESEND_FAIL, ParserObj, secstep_resend_fail);
			assignFromName(NAME_SECSTEP_RESEND_SUCCESS, ParserObj, secstep_resend_success);
			if Assigned(ParserObj.Values[NAME_SECSTEP_TIMEOUT]) then
			begin
				if ParserObj.Values[NAME_SECSTEP_TIMEOUT].Value <> '' then
					secstep_timeout := ParserObj.Values[NAME_SECSTEP_TIMEOUT].Value.ToInt64
				else
					secstep_timeout := AUTH_APP_USED;
			end;
			assignFromName(NAME_SECSTEP_LOGIN, ParserObj, secstep_login);
			assignFromName(NAME_SECSTEP_DISPOSABLE_FAIL, ParserObj, secstep_disposable_fail);
			assignFromName(NAME_SECSTEP_SMSAPI_ERROR, ParserObj, secstep_smsapi_error);
			assignFromName(NAME_SECSTEP_CAPTCHA, ParserObj, secstep_captcha);
			assignFromName(NAME_TOTP_ENABLED, ParserObj, totp_enabled);
			assignFromName(NAME_LOCALE, ParserObj, locale);
			assignFromName(NAME_CLIENT, ParserObj, client);
			assignFromName(NAME_CSRF, ParserObj, csrf);
			assignFromName(NAME_DEVICE, ParserObj, device);
		end;
	except
		Exit;
	end;
	result := true;
	JSONVal.free;
end;

end.
