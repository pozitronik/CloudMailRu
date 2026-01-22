unit CMRTwostepTest;

interface

uses
	CMRTwostep,
	CMRTwostepJsonAdapter,
	CMRConstants,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCMRTwostepTest = class
	public
		[Test]
		procedure TestFromJSONValid;
		[Test]
		procedure TestFromJSONAllFields;
		[Test]
		procedure TestFromJSONTimeoutWithValue;
		[Test]
		procedure TestFromJSONTimeoutEmpty;
		[Test]
		procedure TestFromJSONTimeoutMissing;
		[Test]
		procedure TestFromJSONPartialData;
		[Test]
		procedure TestFromJSONInvalidJSON;
		[Test]
		procedure TestFromJSONEmptyString;
	end;

implementation

const
	{ Full twostep response }
	JSON_TWOSTEP_FULL = '{' +
		'"form_name":"secstep",' +
		'"auth_host":"auth.mail.ru",' +
		'"secstep_phone":"+7 (999) ***-**-99",' +
		'"secstep_page":"sms",' +
		'"secstep_code_fail":"Invalid code",' +
		'"secstep_resend_fail":"Cannot resend",' +
		'"secstep_resend_success":"Code sent",' +
		'"secstep_timeout":"120",' +
		'"secstep_login":"user@mail.ru",' +
		'"secstep_disposable_fail":"Disposable fail",' +
		'"secstep_smsapi_error":"SMS API error",' +
		'"secstep_captcha":"captcha_url",' +
		'"totp_enabled":"true",' +
		'"locale":"ru_RU",' +
		'"client":"web",' +
		'"csrf":"csrf_token_here",' +
		'"device":"desktop"' +
		'}';

	{ Twostep with empty timeout (indicates auth app used) }
	JSON_TWOSTEP_AUTH_APP = '{' +
		'"form_name":"secstep",' +
		'"secstep_timeout":"",' +
		'"secstep_login":"user@mail.ru"' +
		'}';

	{ Twostep without timeout field }
	JSON_TWOSTEP_NO_TIMEOUT = '{' +
		'"form_name":"secstep",' +
		'"secstep_login":"user@mail.ru"' +
		'}';

	{ Partial twostep data }
	JSON_TWOSTEP_PARTIAL = '{' +
		'"csrf":"token123",' +
		'"locale":"en_US"' +
		'}';

procedure TCMRTwostepTest.TestFromJSONValid;
var
	Twostep: TCMRTwostep;
begin
	Assert.IsTrue(TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_FULL, Twostep));
end;

procedure TCMRTwostepTest.TestFromJSONAllFields;
var
	Twostep: TCMRTwostep;
begin
	TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_FULL, Twostep);

	Assert.AreEqual('secstep', Twostep.form_name);
	Assert.AreEqual('auth.mail.ru', Twostep.auth_host);
	Assert.AreEqual('+7 (999) ***-**-99', Twostep.secstep_phone);
	Assert.AreEqual('sms', Twostep.secstep_page);
	Assert.AreEqual('Invalid code', Twostep.secstep_code_fail);
	Assert.AreEqual('Cannot resend', Twostep.secstep_resend_fail);
	Assert.AreEqual('Code sent', Twostep.secstep_resend_success);
	Assert.AreEqual(Int64(120), Twostep.secstep_timeout);
	Assert.AreEqual('user@mail.ru', Twostep.secstep_login);
	Assert.AreEqual('Disposable fail', Twostep.secstep_disposable_fail);
	Assert.AreEqual('SMS API error', Twostep.secstep_smsapi_error);
	Assert.AreEqual('captcha_url', Twostep.secstep_captcha);
	Assert.AreEqual('true', Twostep.totp_enabled);
	Assert.AreEqual('ru_RU', Twostep.locale);
	Assert.AreEqual('web', Twostep.client);
	Assert.AreEqual('csrf_token_here', Twostep.csrf);
	Assert.AreEqual('desktop', Twostep.device);
end;

procedure TCMRTwostepTest.TestFromJSONTimeoutWithValue;
var
	Twostep: TCMRTwostep;
begin
	TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_FULL, Twostep);

	{ Timeout should be parsed as integer }
	Assert.AreEqual(Int64(120), Twostep.secstep_timeout);
end;

procedure TCMRTwostepTest.TestFromJSONTimeoutEmpty;
var
	Twostep: TCMRTwostep;
begin
	{ Empty timeout indicates authenticator app is used }
	TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_AUTH_APP, Twostep);

	Assert.AreEqual(Int64(AUTH_APP_USED), Twostep.secstep_timeout);
end;

procedure TCMRTwostepTest.TestFromJSONTimeoutMissing;
var
	Twostep: TCMRTwostep;
begin
	{ When timeout field is missing, it should remain at default (0) }
	TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_NO_TIMEOUT, Twostep);

	{ Default value when field is missing }
	Assert.AreEqual(Int64(0), Twostep.secstep_timeout);
end;

procedure TCMRTwostepTest.TestFromJSONPartialData;
var
	Twostep: TCMRTwostep;
begin
	Assert.IsTrue(TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_PARTIAL, Twostep));

	Assert.AreEqual('token123', Twostep.csrf);
	Assert.AreEqual('en_US', Twostep.locale);
end;

procedure TCMRTwostepTest.TestFromJSONInvalidJSON;
var
	Twostep: TCMRTwostep;
begin
	Assert.IsFalse(TCMRTwostepJsonAdapter.Parse('not valid json', Twostep));
end;

procedure TCMRTwostepTest.TestFromJSONEmptyString;
var
	Twostep: TCMRTwostep;
begin
	Assert.IsFalse(TCMRTwostepJsonAdapter.Parse('', Twostep));
end;

initialization

TDUnitX.RegisterTestFixture(TCMRTwostepTest);

end.
