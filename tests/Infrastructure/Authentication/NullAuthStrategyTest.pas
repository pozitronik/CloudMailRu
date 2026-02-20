unit NullAuthStrategyTest;

interface

uses
	AuthStrategy,
	Logger,
	DUnitX.TestFramework;

type

	[TestFixture]
	TNullAuthStrategyTest = class
	private
		FStrategy: IAuthStrategy;
	public
		[Setup]
		procedure Setup;

		[Test]
		procedure TestGetName_ReturnsNull;

		[Test]
		procedure TestAuthenticate_ReturnsFalse;

		[Test]
		procedure TestAuthenticate_ReturnsErrorMessage;

		[Test]
		procedure TestAuthenticate_TokensAreEmpty;
	end;

	[TestFixture]
	TAuthResultTest = class
	public
		[Test]
		procedure TestCreateFailure_SetsSuccessFalse;

		[Test]
		procedure TestCreateFailure_SetsErrorMessage;

		[Test]
		procedure TestCreateOAuthSuccess_SetsTokenFromOAuth;

		[Test]
		procedure TestCreateOAuthSuccess_FormatsUnitedParams;

		[Test]
		procedure TestCreateCookieSuccess_SetsSuccessTrue;
		[Test]
		procedure TestCreateCookieSuccess_SetsCookieBasedTrue;
		[Test]
		procedure TestCreateCookieSuccess_SetsAuthTokenFromCSRF;
		[Test]
		procedure TestCreateCookieSuccess_SetsOAuthAccessTokenFromCSRF;
		[Test]
		procedure TestCreateCookieSuccess_FormatsUnitedParamsWithToken;
		[Test]
		procedure TestCreateOAuthSuccess_CookieBasedIsFalse;
		[Test]
		procedure TestCreateFailure_CookieBasedIsFalse;
	end;

	[TestFixture]
	TAuthCredentialsTest = class
	public
		[Test]
		procedure TestCreate_SetsAllFields;
	end;

implementation

uses
	CloudOAuth;

{TNullAuthStrategyTest}

procedure TNullAuthStrategyTest.Setup;
begin
	FStrategy := TNullAuthStrategy.Create;
end;

procedure TNullAuthStrategyTest.TestGetName_ReturnsNull;
begin
	Assert.AreEqual('Null', FStrategy.GetName);
end;

procedure TNullAuthStrategyTest.TestAuthenticate_ReturnsFalse;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');

	Result := FStrategy.Authenticate(Credentials, nil, TNullLogger.Create);

	Assert.IsFalse(Result.Success);
end;

procedure TNullAuthStrategyTest.TestAuthenticate_ReturnsErrorMessage;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');

	Result := FStrategy.Authenticate(Credentials, nil, TNullLogger.Create);

	Assert.IsNotEmpty(Result.ErrorMessage);
end;

procedure TNullAuthStrategyTest.TestAuthenticate_TokensAreEmpty;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');

	Result := FStrategy.Authenticate(Credentials, nil, TNullLogger.Create);

	Assert.IsEmpty(Result.AuthToken);
	Assert.IsEmpty(Result.UnitedParams);
end;

{TAuthResultTest}

procedure TAuthResultTest.TestCreateFailure_SetsSuccessFalse;
var
	Result: TAuthResult;
begin
	Result := TAuthResult.CreateFailure('Some error');

	Assert.IsFalse(Result.Success);
end;

procedure TAuthResultTest.TestCreateFailure_SetsErrorMessage;
var
	Result: TAuthResult;
begin
	Result := TAuthResult.CreateFailure('Authentication failed');

	Assert.AreEqual('Authentication failed', Result.ErrorMessage);
end;

procedure TAuthResultTest.TestCreateOAuthSuccess_SetsTokenFromOAuth;
var
	OAuth: TCloudOAuth;
	Result: TAuthResult;
begin
	OAuth := Default(TCloudOAuth);
	OAuth.access_token := 'oauth_access_token';

	Result := TAuthResult.CreateOAuthSuccess(OAuth);

	Assert.IsTrue(Result.Success);
	Assert.AreEqual('oauth_access_token', Result.AuthToken);
	Assert.AreEqual('oauth_access_token', Result.OAuthToken.access_token);
end;

procedure TAuthResultTest.TestCreateOAuthSuccess_FormatsUnitedParams;
var
	OAuth: TCloudOAuth;
	Result: TAuthResult;
begin
	OAuth := Default(TCloudOAuth);
	OAuth.access_token := 'token123';

	Result := TAuthResult.CreateOAuthSuccess(OAuth);

	Assert.AreEqual('access_token=token123', Result.UnitedParams);
end;

procedure TAuthResultTest.TestCreateCookieSuccess_SetsSuccessTrue;
var
	Result: TAuthResult;
begin
	Result := TAuthResult.CreateCookieSuccess('csrf_token_123');

	Assert.IsTrue(Result.Success);
end;

procedure TAuthResultTest.TestCreateCookieSuccess_SetsCookieBasedTrue;
var
	Result: TAuthResult;
begin
	Result := TAuthResult.CreateCookieSuccess('csrf_token_123');

	Assert.IsTrue(Result.CookieBased);
end;

procedure TAuthResultTest.TestCreateCookieSuccess_SetsAuthTokenFromCSRF;
var
	Result: TAuthResult;
begin
	Result := TAuthResult.CreateCookieSuccess('my_csrf_token');

	Assert.AreEqual('my_csrf_token', Result.AuthToken);
end;

procedure TAuthResultTest.TestCreateCookieSuccess_SetsOAuthAccessTokenFromCSRF;
var
	Result: TAuthResult;
begin
	{In cookie mode, OAuthToken.access_token mirrors the CSRF token}
	Result := TAuthResult.CreateCookieSuccess('csrf_abc');

	Assert.AreEqual('csrf_abc', Result.OAuthToken.access_token);
end;

procedure TAuthResultTest.TestCreateCookieSuccess_FormatsUnitedParamsWithToken;
var
	Result: TAuthResult;
begin
	{Cookie mode uses 'token=' prefix instead of 'access_token='}
	Result := TAuthResult.CreateCookieSuccess('csrf_token_xyz');

	Assert.AreEqual('token=csrf_token_xyz', Result.UnitedParams);
end;

procedure TAuthResultTest.TestCreateOAuthSuccess_CookieBasedIsFalse;
var
	OAuth: TCloudOAuth;
	Result: TAuthResult;
begin
	{OAuth success should not set CookieBased flag}
	OAuth := Default(TCloudOAuth);
	OAuth.access_token := 'token';

	Result := TAuthResult.CreateOAuthSuccess(OAuth);

	Assert.IsFalse(Result.CookieBased);
end;

procedure TAuthResultTest.TestCreateFailure_CookieBasedIsFalse;
var
	Result: TAuthResult;
begin
	{Failure should not set CookieBased flag}
	Result := TAuthResult.CreateFailure('error');

	Assert.IsFalse(Result.CookieBased);
end;

{TAuthCredentialsTest}

procedure TAuthCredentialsTest.TestCreate_SetsAllFields;
var
	Creds: TAuthCredentials;
begin
	Creds := TAuthCredentials.Create('user@domain.com', 'secret', 'user', 'domain.com');

	Assert.AreEqual('user@domain.com', Creds.Email);
	Assert.AreEqual('secret', Creds.Password);
	Assert.AreEqual('user', Creds.User);
	Assert.AreEqual('domain.com', Creds.Domain);
end;

initialization

TDUnitX.RegisterTestFixture(TNullAuthStrategyTest);
TDUnitX.RegisterTestFixture(TAuthResultTest);
TDUnitX.RegisterTestFixture(TAuthCredentialsTest);

end.
