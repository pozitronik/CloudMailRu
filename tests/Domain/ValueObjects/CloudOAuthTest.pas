unit CloudOAuthTest;

interface

uses
	CloudOAuth,
	CloudOAuthJsonAdapter,
	CloudConstants,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCloudOAuthTest = class
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

procedure TCloudOAuthTest.TestFromJSONValidSuccess;
var
	OAuth: TCloudOAuth;
begin
	Assert.IsTrue(TCloudOAuthJsonAdapter.Parse(JSON_OAUTH_SUCCESS, OAuth));
end;

procedure TCloudOAuthTest.TestFromJSONValidWithError;
var
	OAuth: TCloudOAuth;
begin
	{ Error response is still valid JSON that parses successfully }
	Assert.IsTrue(TCloudOAuthJsonAdapter.Parse(JSON_OAUTH_ERROR, OAuth));

	Assert.AreEqual('invalid_grant', OAuth.error);
	Assert.AreEqual(401, OAuth.error_code);
	Assert.AreEqual('The provided authorization grant is invalid', OAuth.error_description);
end;

procedure TCloudOAuthTest.TestFromJSONAccessToken;
var
	OAuth: TCloudOAuth;
begin
	TCloudOAuthJsonAdapter.Parse(JSON_OAUTH_SUCCESS, OAuth);

	Assert.AreEqual('abc123token', OAuth.access_token);
end;

procedure TCloudOAuthTest.TestFromJSONRefreshToken;
var
	OAuth: TCloudOAuth;
begin
	TCloudOAuthJsonAdapter.Parse(JSON_OAUTH_SUCCESS, OAuth);

	Assert.AreEqual('refresh456', OAuth.refresh_token);
end;

procedure TCloudOAuthTest.TestFromJSONExpiresIn;
var
	OAuth: TCloudOAuth;
begin
	TCloudOAuthJsonAdapter.Parse(JSON_OAUTH_SUCCESS, OAuth);

	Assert.AreEqual(3600, OAuth.expires_in);
end;

procedure TCloudOAuthTest.TestFromJSONInvalidJSON;
var
	OAuth: TCloudOAuth;
begin
	{ Invalid JSON should return false with initialized fields }
	Assert.IsFalse(TCloudOAuthJsonAdapter.Parse('not valid json', OAuth));

	{ Fields are initialized to safe defaults }
	Assert.AreEqual(0, OAuth.error_code);
	Assert.AreEqual(0, OAuth.expires_in);
end;

procedure TCloudOAuthTest.TestFromJSONEmptyString;
var
	OAuth: TCloudOAuth;
begin
	Assert.IsFalse(TCloudOAuthJsonAdapter.Parse('', OAuth));
end;

procedure TCloudOAuthTest.TestFromJSONPartialData;
var
	OAuth: TCloudOAuth;
begin
	{ Partial data should still parse successfully }
	Assert.IsTrue(TCloudOAuthJsonAdapter.Parse(JSON_OAUTH_PARTIAL, OAuth));

	Assert.AreEqual('partial_token', OAuth.access_token);
	{ Missing fields are initialized to safe defaults }
	Assert.AreEqual(0, OAuth.expires_in);
	Assert.AreEqual(0, OAuth.error_code);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudOAuthTest);

end.
