unit NullAuthStrategyTest;

interface

uses
	AuthStrategy,
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
		procedure TestCreateSuccess_SetsSuccessTrue;

		[Test]
		procedure TestCreateSuccess_SetsTokenAndParams;

		[Test]
		procedure TestCreateFailure_SetsSuccessFalse;

		[Test]
		procedure TestCreateFailure_SetsErrorMessage;

		[Test]
		procedure TestCreateOAuthSuccess_SetsTokenFromOAuth;

		[Test]
		procedure TestCreateOAuthSuccess_FormatsUnitedParams;

		[Test]
		procedure TestCreateSharedSuccess_SetsShard;
	end;

	[TestFixture]
	TAuthCredentialsTest = class
	public
		[Test]
		procedure TestCreate_SetsAllFields;

		[Test]
		procedure TestCreatePublic_SetsPublicUrl;

		[Test]
		procedure TestCreatePublic_LeavesCredentialsEmpty;
	end;

implementation

uses
	CMROAuth;

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

	Result := FStrategy.Authenticate(Credentials, nil, nil);

	Assert.IsFalse(Result.Success);
end;

procedure TNullAuthStrategyTest.TestAuthenticate_ReturnsErrorMessage;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');

	Result := FStrategy.Authenticate(Credentials, nil, nil);

	Assert.IsNotEmpty(Result.ErrorMessage);
end;

procedure TNullAuthStrategyTest.TestAuthenticate_TokensAreEmpty;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');

	Result := FStrategy.Authenticate(Credentials, nil, nil);

	Assert.IsEmpty(Result.AuthToken);
	Assert.IsEmpty(Result.UnitedParams);
end;

{TAuthResultTest}

procedure TAuthResultTest.TestCreateSuccess_SetsSuccessTrue;
var
	Result: TAuthResult;
begin
	Result := TAuthResult.CreateSuccess('token123', 'param=value');

	Assert.IsTrue(Result.Success);
end;

procedure TAuthResultTest.TestCreateSuccess_SetsTokenAndParams;
var
	Result: TAuthResult;
begin
	Result := TAuthResult.CreateSuccess('mytoken', 'access_token=mytoken');

	Assert.AreEqual('mytoken', Result.AuthToken);
	Assert.AreEqual('access_token=mytoken', Result.UnitedParams);
end;

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
	OAuth: TCMROAuth;
	Result: TAuthResult;
begin
	OAuth := Default(TCMROAuth);
	OAuth.access_token := 'oauth_access_token';

	Result := TAuthResult.CreateOAuthSuccess(OAuth);

	Assert.IsTrue(Result.Success);
	Assert.AreEqual('oauth_access_token', Result.AuthToken);
	Assert.AreEqual('oauth_access_token', Result.OAuthToken.access_token);
end;

procedure TAuthResultTest.TestCreateOAuthSuccess_FormatsUnitedParams;
var
	OAuth: TCMROAuth;
	Result: TAuthResult;
begin
	OAuth := Default(TCMROAuth);
	OAuth.access_token := 'token123';

	Result := TAuthResult.CreateOAuthSuccess(OAuth);

	Assert.AreEqual('access_token=token123', Result.UnitedParams);
end;

procedure TAuthResultTest.TestCreateSharedSuccess_SetsShard;
var
	Result: TAuthResult;
begin
	Result := TAuthResult.CreateSharedSuccess('https://shard.url/', 'ABC123');

	Assert.IsTrue(Result.Success);
	Assert.AreEqual('https://shard.url/', Result.PublicShard);
	Assert.AreEqual('ABC123', Result.PublicLink);
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

procedure TAuthCredentialsTest.TestCreatePublic_SetsPublicUrl;
var
	Creds: TAuthCredentials;
begin
	Creds := TAuthCredentials.CreatePublic('https://cloud.mail.ru/public/ABC123');

	Assert.AreEqual('https://cloud.mail.ru/public/ABC123', Creds.PublicUrl);
end;

procedure TAuthCredentialsTest.TestCreatePublic_LeavesCredentialsEmpty;
var
	Creds: TAuthCredentials;
begin
	Creds := TAuthCredentials.CreatePublic('https://cloud.mail.ru/public/ABC123');

	Assert.IsEmpty(Creds.Email);
	Assert.IsEmpty(Creds.Password);
	Assert.IsEmpty(Creds.User);
	Assert.IsEmpty(Creds.Domain);
end;

initialization

TDUnitX.RegisterTestFixture(TNullAuthStrategyTest);
TDUnitX.RegisterTestFixture(TAuthResultTest);
TDUnitX.RegisterTestFixture(TAuthCredentialsTest);

end.
