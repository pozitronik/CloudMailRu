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
