unit VKIDLoginFormTest;

{Tests for VKIDLogin unit -- constants and static utilities.
	The form itself (TVKIDLoginForm) requires WebView2 runtime and cannot
	be fully tested in a headless environment. We test what can be verified
	without COM/WebView2 dependencies: public constants and IsWebView2Available
	returning a boolean without crashing.}

interface

uses
	DUnitX.TestFramework,
	Winapi.Messages,
	VKIDLogin;

type

	[TestFixture]
	TVKIDLoginConstantsTest = class
	public
		[Test]
		procedure TestLoginURL_IsHTTPS;
		[Test]
		procedure TestLoginURL_ContainsAccountMailRu;
		[Test]
		procedure TestLoginURL_HasServiceCloudParam;

		[Test]
		procedure TestJsFetchCSRF_ContainsXHR;
		[Test]
		procedure TestJsFetchCSRF_CallsCSRFEndpoint;
		[Test]
		procedure TestJsFetchCSRF_ParsesBodyToken;
		[Test]
		procedure TestJsFetchCSRF_IsIIFE;
		[Test]
		procedure TestJsFetchCSRF_HasErrorHandling;

		[Test]
		procedure TestWMFinalizeLogin_IsInUserMessageRange;
	end;

	[TestFixture]
	TVKIDLoginAvailabilityTest = class
	public
		{IsWebView2Available should return a boolean without crashing,
			regardless of whether WebView2 is actually installed.}
		[Test]
		procedure TestIsWebView2Available_DoesNotRaise;
		[Test]
		procedure TestIsWebView2Available_ReturnsBooleanType;
	end;

implementation

uses
	SysUtils;

{TVKIDLoginConstantsTest}

procedure TVKIDLoginConstantsTest.TestLoginURL_IsHTTPS;
begin
	Assert.StartsWith('https://', VKID_LOGIN_URL);
end;

procedure TVKIDLoginConstantsTest.TestLoginURL_ContainsAccountMailRu;
begin
	Assert.Contains(VKID_LOGIN_URL, 'account.mail.ru');
end;

procedure TVKIDLoginConstantsTest.TestLoginURL_HasServiceCloudParam;
begin
	Assert.Contains(VKID_LOGIN_URL, 'service=cloud');
end;

procedure TVKIDLoginConstantsTest.TestJsFetchCSRF_ContainsXHR;
begin
	Assert.Contains(JS_FETCH_CSRF, 'XMLHttpRequest');
end;

procedure TVKIDLoginConstantsTest.TestJsFetchCSRF_CallsCSRFEndpoint;
begin
	Assert.Contains(JS_FETCH_CSRF, '/api/v2/tokens/csrf');
end;

procedure TVKIDLoginConstantsTest.TestJsFetchCSRF_ParsesBodyToken;
begin
	// JS must extract body.token from the API response
	Assert.Contains(JS_FETCH_CSRF, 'data.body.token');
end;

procedure TVKIDLoginConstantsTest.TestJsFetchCSRF_IsIIFE;
begin
	// Must be an immediately-invoked function expression (IIFE) to return a value
	Assert.StartsWith('(function()', JS_FETCH_CSRF);
	Assert.EndsWith('()', JS_FETCH_CSRF);
end;

procedure TVKIDLoginConstantsTest.TestJsFetchCSRF_HasErrorHandling;
begin
	// Must have try/catch to avoid script errors breaking the flow
	Assert.Contains(JS_FETCH_CSRF, 'try');
	Assert.Contains(JS_FETCH_CSRF, 'catch');
end;

procedure TVKIDLoginConstantsTest.TestWMFinalizeLogin_IsInUserMessageRange;
begin
	// WM_FINALIZE_LOGIN must be >= WM_USER to avoid colliding with system messages
	Assert.IsTrue(WM_FINALIZE_LOGIN >= WM_USER,
		'Custom message ID should be in WM_USER range');
	Assert.AreEqual(Integer(WM_USER + 1), Integer(WM_FINALIZE_LOGIN));
end;

{TVKIDLoginAvailabilityTest}

procedure TVKIDLoginAvailabilityTest.TestIsWebView2Available_DoesNotRaise;
begin
	// Should not raise, regardless of WebView2 installation state
	TVKIDLoginForm.IsWebView2Available;
	Assert.Pass;
end;

procedure TVKIDLoginAvailabilityTest.TestIsWebView2Available_ReturnsBooleanType;
var
	Available: Boolean;
begin
	Available := TVKIDLoginForm.IsWebView2Available;
	// Result is either True or False, no exception
	Assert.IsTrue((Available = True) or (Available = False));
end;

initialization

TDUnitX.RegisterTestFixture(TVKIDLoginConstantsTest);
TDUnitX.RegisterTestFixture(TVKIDLoginAvailabilityTest);

end.
