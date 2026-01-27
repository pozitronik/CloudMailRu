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
	CloudConstants,
	SafeJSON;

class function TCloudTwostepJsonAdapter.Parse(const JSON: WideString; out Twostep: TCloudTwostep): Boolean;
var
	Root, TimeoutNode: TSafeJSON;
	TimeoutStr: WideString;
begin
	Twostep := Default(TCloudTwostep);
	Result := False;

	Root := TSafeJSON.Parse(JSON);
	try
		if Root.IsNull then
			Exit;

		Twostep.form_name := Root.Get(NAME_FORM_NAME).AsString;
		Twostep.auth_host := Root.Get(NAME_AUTH_HOST).AsString;
		Twostep.secstep_phone := Root.Get(NAME_SECSTEP_PHONE).AsString;
		Twostep.secstep_page := Root.Get(NAME_SECSTEP_PAGE).AsString;
		Twostep.secstep_code_fail := Root.Get(NAME_SECSTEP_CODE_FAIL).AsString;
		Twostep.secstep_resend_fail := Root.Get(NAME_SECSTEP_RESEND_FAIL).AsString;
		Twostep.secstep_resend_success := Root.Get(NAME_SECSTEP_RESEND_SUCCESS).AsString;

		{Special handling: empty timeout string means auth app is used}
		TimeoutNode := Root.Get(NAME_SECSTEP_TIMEOUT);
		if not TimeoutNode.IsNull then
		begin
			TimeoutStr := TimeoutNode.AsString;
			if TimeoutStr = '' then
				Twostep.secstep_timeout := AUTH_APP_USED
			else
				Twostep.secstep_timeout := TimeoutNode.AsInt64;
		end;

		Twostep.secstep_login := Root.Get(NAME_SECSTEP_LOGIN).AsString;
		Twostep.secstep_disposable_fail := Root.Get(NAME_SECSTEP_DISPOSABLE_FAIL).AsString;
		Twostep.secstep_smsapi_error := Root.Get(NAME_SECSTEP_SMSAPI_ERROR).AsString;
		Twostep.secstep_captcha := Root.Get(NAME_SECSTEP_CAPTCHA).AsString;
		Twostep.totp_enabled := Root.Get(NAME_TOTP_ENABLED).AsString;
		Twostep.locale := Root.Get(NAME_LOCALE).AsString;
		Twostep.client := Root.Get(NAME_CLIENT).AsString;
		Twostep.csrf := Root.Get(NAME_CSRF).AsString;
		Twostep.device := Root.Get(NAME_DEVICE).AsString;

		Result := True;
	finally
		Root.Free;
	end;
end;

end.
