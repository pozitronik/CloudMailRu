unit CMROAuthTest;

interface

uses
	CMROAuth,
	CMROAuthJsonAdapter,
	CMRConstants,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCMROAuthTest = class
	public
		[Test]
		procedure TestFromJSONValidSuccess;
		[Test]
		procedure TestFromJSONValidWithError;
		[Test]
		procedure TestFromJSONAccessToken;
		[Test]
		procedure TestFromJSONRefreshToken;
		[Test]
		procedure TestFromJSONExpiresIn;
		[Test]
		procedure TestFromJSONInvalidJSON;
		[Test]
		procedure TestFromJSONEmptyString;
		[Test]
		procedure TestFromJSONPartialData;
	end;

implementation

const
	{ Successful OAuth response }
	JSON_OAUTH_SUCCESS = '{"access_token":"abc123token","refresh_token":"refresh456","expires_in":3600}';

	{ OAuth error response }
	JSON_OAUTH_ERROR = '{"error":"invalid_grant","error_code":401,"error_description":"The provided authorization grant is invalid"}';

	{ Partial response - only access token }
	JSON_OAUTH_PARTIAL = '{"access_token":"partial_token"}';

procedure TCMROAuthTest.TestFromJSONValidSuccess;
var
	OAuth: TCMROAuth;
begin
	Assert.IsTrue(TCMROAuthJsonAdapter.Parse(JSON_OAUTH_SUCCESS, OAuth));
end;

procedure TCMROAuthTest.TestFromJSONValidWithError;
var
	OAuth: TCMROAuth;
begin
	{ Error response is still valid JSON that parses successfully }
	Assert.IsTrue(TCMROAuthJsonAdapter.Parse(JSON_OAUTH_ERROR, OAuth));

	Assert.AreEqual('invalid_grant', OAuth.error);
	Assert.AreEqual(401, OAuth.error_code);
	Assert.AreEqual('The provided authorization grant is invalid', OAuth.error_description);
end;

procedure TCMROAuthTest.TestFromJSONAccessToken;
var
	OAuth: TCMROAuth;
begin
	TCMROAuthJsonAdapter.Parse(JSON_OAUTH_SUCCESS, OAuth);

	Assert.AreEqual('abc123token', OAuth.access_token);
end;

procedure TCMROAuthTest.TestFromJSONRefreshToken;
var
	OAuth: TCMROAuth;
begin
	TCMROAuthJsonAdapter.Parse(JSON_OAUTH_SUCCESS, OAuth);

	Assert.AreEqual('refresh456', OAuth.refresh_token);
end;

procedure TCMROAuthTest.TestFromJSONExpiresIn;
var
	OAuth: TCMROAuth;
begin
	TCMROAuthJsonAdapter.Parse(JSON_OAUTH_SUCCESS, OAuth);

	Assert.AreEqual(3600, OAuth.expires_in);
end;

procedure TCMROAuthTest.TestFromJSONInvalidJSON;
var
	OAuth: TCMROAuth;
begin
	{ Invalid JSON should return false with initialized fields }
	Assert.IsFalse(TCMROAuthJsonAdapter.Parse('not valid json', OAuth));

	{ Fields are initialized to safe defaults }
	Assert.AreEqual(0, OAuth.error_code);
	Assert.AreEqual(0, OAuth.expires_in);
end;

procedure TCMROAuthTest.TestFromJSONEmptyString;
var
	OAuth: TCMROAuth;
begin
	Assert.IsFalse(TCMROAuthJsonAdapter.Parse('', OAuth));
end;

procedure TCMROAuthTest.TestFromJSONPartialData;
var
	OAuth: TCMROAuth;
begin
	{ Partial data should still parse successfully }
	Assert.IsTrue(TCMROAuthJsonAdapter.Parse(JSON_OAUTH_PARTIAL, OAuth));

	Assert.AreEqual('partial_token', OAuth.access_token);
	{ Missing fields are initialized to safe defaults }
	Assert.AreEqual(0, OAuth.expires_in);
	Assert.AreEqual(0, OAuth.error_code);
end;

initialization

TDUnitX.RegisterTestFixture(TCMROAuthTest);

end.
