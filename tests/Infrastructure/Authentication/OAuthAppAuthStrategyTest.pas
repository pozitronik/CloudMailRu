unit OAuthAppAuthStrategyTest;

interface

uses
	AuthStrategy,
	OAuthAppAuthStrategy,
	CloudHTTP,
	MockCloudHTTP,
	DUnitX.TestFramework;

type

	[TestFixture]
	TOAuthAppAuthStrategyTest = class
	private
		FStrategy: IAuthStrategy;
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPIntf: ICloudHTTP; {Keep reference alive}
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestGetName_ReturnsOAuthAppPassword;

		[Test]
		procedure TestImplementsInterface;

		{HTTP response tests}
		[Test]
		procedure TestAuthenticate_PostFails_ReturnsFailure;
		[Test]
		procedure TestAuthenticate_InvalidJSON_ReturnsFailure;
		[Test]
		procedure TestAuthenticate_ErrorInResponse_ReturnsFailure;
		[Test]
		procedure TestAuthenticate_ValidOAuthResponse_ReturnsSuccess;
		[Test]
		procedure TestAuthenticate_ValidOAuthResponse_SetsAccessToken;
		[Test]
		procedure TestAuthenticate_PostsToCorrectURL;
		[Test]
		procedure TestAuthenticate_PostsCorrectCredentials;

		{Factory tests}
		[Test]
		procedure TestDefaultFactory_CreatesOAuthStrategy;
	end;

implementation

uses
	SysUtils,
	CloudConstants,
	Logger;

{TOAuthAppAuthStrategyTest}

procedure TOAuthAppAuthStrategyTest.Setup;
begin
	FStrategy := TOAuthAppAuthStrategy.Create;
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPIntf := FMockHTTP; {Keep interface reference alive}
end;

procedure TOAuthAppAuthStrategyTest.TearDown;
begin
	FStrategy := nil;
	FMockHTTPIntf := nil;
	FMockHTTP := nil;
end;

procedure TOAuthAppAuthStrategyTest.TestGetName_ReturnsOAuthAppPassword;
begin
	Assert.AreEqual('OAuth App Password', FStrategy.GetName);
end;

procedure TOAuthAppAuthStrategyTest.TestImplementsInterface;
begin
	{FStrategy is assigned in Setup as IAuthStrategy, proving interface support}
	Assert.IsNotNull(FStrategy);
end;

procedure TOAuthAppAuthStrategyTest.TestAuthenticate_PostFails_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	AuthResult: TAuthResult;
begin
	{When HTTP post fails, authentication should fail}
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');
	FMockHTTP.SetDefaultResponse(False, ''); {All POST calls fail}

	AuthResult := FStrategy.Authenticate(Credentials, FMockHTTPIntf, TNullLogger.Create);

	Assert.IsFalse(AuthResult.Success);
	Assert.Contains(AuthResult.ErrorMessage, 'HTTP error');
end;

procedure TOAuthAppAuthStrategyTest.TestAuthenticate_InvalidJSON_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	AuthResult: TAuthResult;
begin
	{When response is not valid JSON, authentication should fail}
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');
	FMockHTTP.SetResponse(OAUTH_TOKEN_URL, True, 'not valid json');

	AuthResult := FStrategy.Authenticate(Credentials, FMockHTTPIntf, TNullLogger.Create);

	Assert.IsFalse(AuthResult.Success);
	Assert.Contains(AuthResult.ErrorMessage, 'JSON parse error');
end;

procedure TOAuthAppAuthStrategyTest.TestAuthenticate_ErrorInResponse_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	AuthResult: TAuthResult;
	ErrorJSON: WideString;
begin
	{When OAuth returns an error code, authentication should fail}
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');
	ErrorJSON := '{"error":"invalid_grant","error_description":"Invalid password","error_code":1}';
	FMockHTTP.SetResponse(OAUTH_TOKEN_URL, True, ErrorJSON);

	AuthResult := FStrategy.Authenticate(Credentials, FMockHTTPIntf, TNullLogger.Create);

	Assert.IsFalse(AuthResult.Success);
	Assert.Contains(AuthResult.ErrorMessage, 'invalid_grant');
end;

procedure TOAuthAppAuthStrategyTest.TestAuthenticate_ValidOAuthResponse_ReturnsSuccess;
var
	Credentials: TAuthCredentials;
	AuthResult: TAuthResult;
	ValidJSON: WideString;
begin
	{When OAuth returns valid token, authentication should succeed}
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');
	ValidJSON := '{"access_token":"test_access_token","refresh_token":"test_refresh_token","expires_in":3600,"error_code":0}';
	FMockHTTP.SetResponse(OAUTH_TOKEN_URL, True, ValidJSON);

	AuthResult := FStrategy.Authenticate(Credentials, FMockHTTPIntf, TNullLogger.Create);

	Assert.IsTrue(AuthResult.Success);
end;

procedure TOAuthAppAuthStrategyTest.TestAuthenticate_ValidOAuthResponse_SetsAccessToken;
var
	Credentials: TAuthCredentials;
	AuthResult: TAuthResult;
	ValidJSON: WideString;
begin
	{When OAuth returns valid token, the token should be set in result}
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');
	ValidJSON := '{"access_token":"my_access_token_12345","refresh_token":"my_refresh","expires_in":7200,"error_code":0}';
	FMockHTTP.SetResponse(OAUTH_TOKEN_URL, True, ValidJSON);

	AuthResult := FStrategy.Authenticate(Credentials, FMockHTTPIntf, TNullLogger.Create);

	Assert.IsTrue(AuthResult.Success);
	Assert.AreEqual('my_access_token_12345', AuthResult.OAuthToken.access_token);
	Assert.AreEqual('my_refresh', AuthResult.OAuthToken.refresh_token);
	Assert.AreEqual(7200, AuthResult.OAuthToken.expires_in);
end;

procedure TOAuthAppAuthStrategyTest.TestAuthenticate_PostsToCorrectURL;
var
	Credentials: TAuthCredentials;
	ValidJSON: WideString;
begin
	{Verify that authentication posts to the OAuth token URL}
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');
	ValidJSON := '{"access_token":"token","refresh_token":"refresh","expires_in":3600,"error_code":0}';
	FMockHTTP.SetResponse(OAUTH_TOKEN_URL, True, ValidJSON);

	FStrategy.Authenticate(Credentials, FMockHTTPIntf, TNullLogger.Create);

	Assert.IsTrue(FMockHTTP.WasURLCalled(OAUTH_TOKEN_URL));
end;

procedure TOAuthAppAuthStrategyTest.TestAuthenticate_PostsCorrectCredentials;
var
	Credentials: TAuthCredentials;
	ValidJSON: WideString;
	PostedData: WideString;
begin
	{Verify that authentication posts correct credentials in the request body}
	Credentials := TAuthCredentials.Create('myuser@inbox.ru', 'mypass123', 'myuser', 'inbox.ru');
	ValidJSON := '{"access_token":"token","refresh_token":"refresh","expires_in":3600,"error_code":0}';
	FMockHTTP.SetResponse(OAUTH_TOKEN_URL, True, ValidJSON);

	FStrategy.Authenticate(Credentials, FMockHTTPIntf, TNullLogger.Create);

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.Contains(PostedData, 'grant_type=password');
	Assert.Contains(PostedData, 'username=myuser@inbox.ru');
	{Password is URL-encoded in the request}
	Assert.Contains(PostedData, 'password=mypass123');
end;

procedure TOAuthAppAuthStrategyTest.TestDefaultFactory_CreatesOAuthStrategy;
var
	Factory: IAuthStrategyFactory;
	Strategy: IAuthStrategy;
begin
	Factory := TDefaultAuthStrategyFactory.Create;
	Strategy := Factory.CreateDefaultStrategy;

	Assert.IsNotNull(Strategy);
	Assert.AreEqual('OAuth App Password', Strategy.GetName);
end;

initialization

TDUnitX.RegisterTestFixture(TOAuthAppAuthStrategyTest);

end.
