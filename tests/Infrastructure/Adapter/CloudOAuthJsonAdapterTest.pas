unit CloudOAuthJsonAdapterTest;

interface

uses
	CloudOAuth,
	CloudOAuthJsonAdapter,
	CloudConstants,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCloudOAuthJsonAdapterTest = class
	private
		const
			{Successful OAuth response}
			JSON_SUCCESS = '{"access_token":"test_access_token","refresh_token":"test_refresh_token","expires_in":7200}';

			{Error response}
			JSON_ERROR = '{"error":"invalid_request","error_code":400,"error_description":"Bad request"}';

			{Partial response - only some fields}
			JSON_PARTIAL = '{"access_token":"partial_token","expires_in":1800}';

			{Invalid JSON}
			JSON_INVALID = 'not valid json';

			{Empty string}
			JSON_EMPTY = '';
	public
		[Test]
		procedure TestParse_Success_ReturnsTrue;
		[Test]
		procedure TestParse_Success_ParsesAccessToken;
		[Test]
		procedure TestParse_Success_ParsesRefreshToken;
		[Test]
		procedure TestParse_Success_ParsesExpiresIn;
		[Test]
		procedure TestParse_Error_ReturnsTrue;
		[Test]
		procedure TestParse_Error_ParsesAllErrorFields;
		[Test]
		procedure TestParse_Partial_ReturnsTrue;
		[Test]
		procedure TestParse_Partial_InitializesMissingFields;
		[Test]
		procedure TestParse_InvalidJSON_ReturnsFalse;
		[Test]
		procedure TestParse_EmptyString_ReturnsFalse;
		[Test]
		procedure TestParse_InvalidJSON_SetsErrorFields;
	end;

implementation

procedure TCloudOAuthJsonAdapterTest.TestParse_Success_ReturnsTrue;
var
	OAuth: TCloudOAuth;
begin
	Assert.IsTrue(TCloudOAuthJsonAdapter.Parse(JSON_SUCCESS, OAuth));
end;

procedure TCloudOAuthJsonAdapterTest.TestParse_Success_ParsesAccessToken;
var
	OAuth: TCloudOAuth;
begin
	TCloudOAuthJsonAdapter.Parse(JSON_SUCCESS, OAuth);
	Assert.AreEqual('test_access_token', OAuth.access_token);
end;

procedure TCloudOAuthJsonAdapterTest.TestParse_Success_ParsesRefreshToken;
var
	OAuth: TCloudOAuth;
begin
	TCloudOAuthJsonAdapter.Parse(JSON_SUCCESS, OAuth);
	Assert.AreEqual('test_refresh_token', OAuth.refresh_token);
end;

procedure TCloudOAuthJsonAdapterTest.TestParse_Success_ParsesExpiresIn;
var
	OAuth: TCloudOAuth;
begin
	TCloudOAuthJsonAdapter.Parse(JSON_SUCCESS, OAuth);
	Assert.AreEqual(7200, OAuth.expires_in);
end;

procedure TCloudOAuthJsonAdapterTest.TestParse_Error_ReturnsTrue;
var
	OAuth: TCloudOAuth;
begin
	{Error response is still valid JSON}
	Assert.IsTrue(TCloudOAuthJsonAdapter.Parse(JSON_ERROR, OAuth));
end;

procedure TCloudOAuthJsonAdapterTest.TestParse_Error_ParsesAllErrorFields;
var
	OAuth: TCloudOAuth;
begin
	TCloudOAuthJsonAdapter.Parse(JSON_ERROR, OAuth);

	Assert.AreEqual('invalid_request', OAuth.error);
	Assert.AreEqual(400, OAuth.error_code);
	Assert.AreEqual('Bad request', OAuth.error_description);
end;

procedure TCloudOAuthJsonAdapterTest.TestParse_Partial_ReturnsTrue;
var
	OAuth: TCloudOAuth;
begin
	Assert.IsTrue(TCloudOAuthJsonAdapter.Parse(JSON_PARTIAL, OAuth));
end;

procedure TCloudOAuthJsonAdapterTest.TestParse_Partial_InitializesMissingFields;
var
	OAuth: TCloudOAuth;
begin
	TCloudOAuthJsonAdapter.Parse(JSON_PARTIAL, OAuth);

	Assert.AreEqual('partial_token', OAuth.access_token);
	Assert.AreEqual(1800, OAuth.expires_in);
	{Missing fields are initialized to safe defaults}
	Assert.AreEqual('', OAuth.refresh_token);
	Assert.AreEqual('', OAuth.error);
	Assert.AreEqual(0, OAuth.error_code);
end;

procedure TCloudOAuthJsonAdapterTest.TestParse_InvalidJSON_ReturnsFalse;
var
	OAuth: TCloudOAuth;
begin
	Assert.IsFalse(TCloudOAuthJsonAdapter.Parse(JSON_INVALID, OAuth));
end;

procedure TCloudOAuthJsonAdapterTest.TestParse_EmptyString_ReturnsFalse;
var
	OAuth: TCloudOAuth;
begin
	Assert.IsFalse(TCloudOAuthJsonAdapter.Parse(JSON_EMPTY, OAuth));
end;

procedure TCloudOAuthJsonAdapterTest.TestParse_InvalidJSON_SetsErrorFields;
var
	OAuth: TCloudOAuth;
begin
	TCloudOAuthJsonAdapter.Parse(JSON_INVALID, OAuth);

	{On parse error, fields are initialized to safe defaults}
	Assert.AreEqual(0, OAuth.error_code);
	Assert.AreEqual(0, OAuth.expires_in);
	Assert.AreEqual('', OAuth.access_token);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudOAuthJsonAdapterTest);

end.
