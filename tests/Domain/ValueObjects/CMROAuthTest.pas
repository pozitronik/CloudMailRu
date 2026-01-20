unit CMROAuthTest;

interface

uses
	CMROAuth,
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
	Assert.IsTrue(OAuth.fromJSON(JSON_OAUTH_SUCCESS));
end;

procedure TCMROAuthTest.TestFromJSONValidWithError;
var
	OAuth: TCMROAuth;
begin
	{ Error response is still valid JSON that parses successfully }
	Assert.IsTrue(OAuth.fromJSON(JSON_OAUTH_ERROR));

	Assert.AreEqual('invalid_grant', OAuth.error);
	Assert.AreEqual(401, OAuth.error_code);
	Assert.AreEqual('The provided authorization grant is invalid', OAuth.error_description);
end;

procedure TCMROAuthTest.TestFromJSONAccessToken;
var
	OAuth: TCMROAuth;
begin
	OAuth.fromJSON(JSON_OAUTH_SUCCESS);

	Assert.AreEqual('abc123token', OAuth.access_token);
end;

procedure TCMROAuthTest.TestFromJSONRefreshToken;
var
	OAuth: TCMROAuth;
begin
	OAuth.fromJSON(JSON_OAUTH_SUCCESS);

	Assert.AreEqual('refresh456', OAuth.refresh_token);
end;

procedure TCMROAuthTest.TestFromJSONExpiresIn;
var
	OAuth: TCMROAuth;
begin
	OAuth.fromJSON(JSON_OAUTH_SUCCESS);

	Assert.AreEqual(3600, OAuth.expires_in);
end;

procedure TCMROAuthTest.TestFromJSONInvalidJSON;
var
	OAuth: TCMROAuth;
begin
	{ Invalid JSON should return false with initialized fields }
	Assert.IsFalse(OAuth.fromJSON('not valid json'));

	{ Fields are initialized to safe defaults }
	Assert.AreEqual(0, OAuth.error_code);
	Assert.AreEqual(0, OAuth.expires_in);
end;

procedure TCMROAuthTest.TestFromJSONEmptyString;
var
	OAuth: TCMROAuth;
begin
	Assert.IsFalse(OAuth.fromJSON(''));
end;

procedure TCMROAuthTest.TestFromJSONPartialData;
var
	OAuth: TCMROAuth;
begin
	{ Partial data should still parse successfully }
	Assert.IsTrue(OAuth.fromJSON(JSON_OAUTH_PARTIAL));

	Assert.AreEqual('partial_token', OAuth.access_token);
	{ Missing fields are initialized to safe defaults }
	Assert.AreEqual(0, OAuth.expires_in);
	Assert.AreEqual(0, OAuth.error_code);
end;

initialization

TDUnitX.RegisterTestFixture(TCMROAuthTest);

end.
