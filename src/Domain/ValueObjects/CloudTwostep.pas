unit CloudTwostep;

interface

type
	TCloudTwostep = record
		form_name: WideString;
		auth_host: WideString;
		secstep_phone: WideString;
		secstep_page: WideString;
		secstep_code_fail: WideString;
		secstep_resend_fail: WideString;
		secstep_resend_success: WideString;
		secstep_timeout: Int64;
		secstep_login: WideString;
		secstep_disposable_fail: WideString;
		secstep_smsapi_error: WideString;
		secstep_captcha: WideString;
		totp_enabled: WideString;
		locale: WideString;
		client: WideString;
		csrf: WideString;
		device: WideString;
	end;

implementation

end.
