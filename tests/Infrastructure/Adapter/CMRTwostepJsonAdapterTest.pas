unit CMRTwostepJsonAdapterTest;

interface

uses
	CMRTwostep,
	CMRTwostepJsonAdapter,
	CMRConstants,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCMRTwostepJsonAdapterTest = class
	private
		const
			{Full two-step data}
			JSON_TWOSTEP_FULL = '{"form_name":"auth_form","auth_host":"https://auth.mail.ru",' +
				'"secstep_phone":"+7***1234","secstep_page":"sms",' +
				'"secstep_code_fail":"","secstep_resend_fail":"0",' +
				'"secstep_resend_success":"1","secstep_timeout":60,' +
				'"secstep_login":"test@mail.ru","secstep_disposable_fail":"",' +
				'"secstep_smsapi_error":"","secstep_captcha":"",' +
				'"totp_enabled":"0","locale":"ru_RU",' +
				'"client":"cloud","csrf":"token123","device":"browser"}';

			{Auth app used (empty timeout)}
			JSON_TWOSTEP_AUTH_APP = '{"form_name":"auth_form","secstep_timeout":""}';

			{Partial data}
			JSON_TWOSTEP_PARTIAL = '{"form_name":"auth_form","csrf":"abc123"}';

			{Invalid JSON}
			JSON_INVALID = 'not valid json';

			{Empty string}
			JSON_EMPTY = '';
	public
		[Test]
		procedure TestParse_Full_ReturnsTrue;
		[Test]
		procedure TestParse_Full_ParsesFormName;
		[Test]
		procedure TestParse_Full_ParsesAuthHost;
		[Test]
		procedure TestParse_Full_ParsesSecstepPhone;
		[Test]
		procedure TestParse_Full_ParsesTimeout;
		[Test]
		procedure TestParse_Full_ParsesCsrf;
		[Test]
		procedure TestParse_AuthApp_SetsAuthAppUsed;
		[Test]
		procedure TestParse_Partial_ReturnsTrue;
		[Test]
		procedure TestParse_InvalidJSON_ReturnsFalse;
		[Test]
		procedure TestParse_EmptyString_ReturnsFalse;
	end;

implementation

procedure TCMRTwostepJsonAdapterTest.TestParse_Full_ReturnsTrue;
var
	Twostep: TCMRTwostep;
begin
	Assert.IsTrue(TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_FULL, Twostep));
end;

procedure TCMRTwostepJsonAdapterTest.TestParse_Full_ParsesFormName;
var
	Twostep: TCMRTwostep;
begin
	TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_FULL, Twostep);
	Assert.AreEqual('auth_form', Twostep.form_name);
end;

procedure TCMRTwostepJsonAdapterTest.TestParse_Full_ParsesAuthHost;
var
	Twostep: TCMRTwostep;
begin
	TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_FULL, Twostep);
	Assert.AreEqual('https://auth.mail.ru', Twostep.auth_host);
end;

procedure TCMRTwostepJsonAdapterTest.TestParse_Full_ParsesSecstepPhone;
var
	Twostep: TCMRTwostep;
begin
	TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_FULL, Twostep);
	Assert.AreEqual('+7***1234', Twostep.secstep_phone);
end;

procedure TCMRTwostepJsonAdapterTest.TestParse_Full_ParsesTimeout;
var
	Twostep: TCMRTwostep;
begin
	TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_FULL, Twostep);
	Assert.AreEqual(Int64(60), Twostep.secstep_timeout);
end;

procedure TCMRTwostepJsonAdapterTest.TestParse_Full_ParsesCsrf;
var
	Twostep: TCMRTwostep;
begin
	TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_FULL, Twostep);
	Assert.AreEqual('token123', Twostep.csrf);
end;

procedure TCMRTwostepJsonAdapterTest.TestParse_AuthApp_SetsAuthAppUsed;
var
	Twostep: TCMRTwostep;
begin
	TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_AUTH_APP, Twostep);
	Assert.AreEqual(Int64(AUTH_APP_USED), Twostep.secstep_timeout);
end;

procedure TCMRTwostepJsonAdapterTest.TestParse_Partial_ReturnsTrue;
var
	Twostep: TCMRTwostep;
begin
	Assert.IsTrue(TCMRTwostepJsonAdapter.Parse(JSON_TWOSTEP_PARTIAL, Twostep));
	Assert.AreEqual('auth_form', Twostep.form_name);
	Assert.AreEqual('abc123', Twostep.csrf);
end;

procedure TCMRTwostepJsonAdapterTest.TestParse_InvalidJSON_ReturnsFalse;
var
	Twostep: TCMRTwostep;
begin
	Assert.IsFalse(TCMRTwostepJsonAdapter.Parse(JSON_INVALID, Twostep));
end;

procedure TCMRTwostepJsonAdapterTest.TestParse_EmptyString_ReturnsFalse;
var
	Twostep: TCMRTwostep;
begin
	Assert.IsFalse(TCMRTwostepJsonAdapter.Parse(JSON_EMPTY, Twostep));
end;

initialization

TDUnitX.RegisterTestFixture(TCMRTwostepJsonAdapterTest);

end.
