unit DeprecatedStrategiesTest;

{Tests for deprecated authentication strategies.
 These tests verify that the deprecated strategies compile, implement the interface,
 and handle missing dependencies gracefully. They don't test actual authentication
 since the strategies no longer work after VK ID migration.}

interface

uses
	WebAuthStrategy,
	TwoStepAuthStrategy,
	OldOAuthStrategy,
	IAuthStrategyInterface,
	PasswordUIProvider,
	TCLogger,
	DUnitX.TestFramework;

type
	{TWebAuthStrategy Tests}
	[TestFixture]
	TWebAuthStrategyTest = class
	private
		FStrategy: TWebAuthStrategy;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestGetName;
		[Test]
		procedure TestImplementsInterface;
		[Test]
		procedure TestAuthenticate_EmptyCredentials_ReturnsFailure;
		[Test]
		procedure TestAuthenticate_NoHTTP_ReturnsFailure;
		[Test]
		procedure TestAuthenticate_MissingUserDomain_ReturnsFailure;
	end;

	{TTwoStepAuthStrategy Tests}
	[TestFixture]
	TTwoStepAuthStrategyTest = class
	private
		FStrategy: TTwoStepAuthStrategy;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestGetName;
		[Test]
		procedure TestImplementsInterface;
		[Test]
		procedure TestAuthenticate_EmptyCredentials_ReturnsFailure;
		[Test]
		procedure TestAuthenticate_NoHTTP_ReturnsFailure;
		[Test]
		procedure TestAuthenticate_NoPasswordUI_ReturnsFailure;
	end;

	{TOldOAuthStrategy Tests}
	[TestFixture]
	TOldOAuthStrategyTest = class
	private
		FStrategy: TOldOAuthStrategy;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestGetName;
		[Test]
		procedure TestImplementsInterface;
		[Test]
		procedure TestAuthenticate_EmptyCredentials_ReturnsFailure;
		[Test]
		procedure TestAuthenticate_NoHTTP_ReturnsFailure;
	end;

implementation

uses
	TestHelper;

{ TWebAuthStrategyTest }

procedure TWebAuthStrategyTest.Setup;
begin
	FStrategy := TWebAuthStrategy.Create;
end;

procedure TWebAuthStrategyTest.TearDown;
begin
	FStrategy.Free;
end;

procedure TWebAuthStrategyTest.TestGetName;
begin
	Assert.AreEqual('Web Form (Deprecated)', FStrategy.GetName);
end;

procedure TWebAuthStrategyTest.TestImplementsInterface;
var
	Strategy: IAuthStrategy;
begin
	Strategy := TWebAuthStrategy.Create;
	Assert.IsNotNull(Strategy, 'Strategy should implement IAuthStrategy');
end;

procedure TWebAuthStrategyTest.TestAuthenticate_EmptyCredentials_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	Credentials := Default(TAuthCredentials);
	Result := FStrategy.Authenticate(Credentials, nil, TNullLogger.Create);
	Assert.IsFalse(Result.Success);
	Assert.AreEqual('Email and password are required', Result.ErrorMessage);
end;

procedure TWebAuthStrategyTest.TestAuthenticate_NoHTTP_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password123', 'test', 'mail.ru');
	Result := FStrategy.Authenticate(Credentials, nil, TNullLogger.Create);
	Assert.IsFalse(Result.Success);
	Assert.AreEqual('HTTP client is required', Result.ErrorMessage);
end;

procedure TWebAuthStrategyTest.TestAuthenticate_MissingUserDomain_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{Create credentials without User/Domain parsed}
	Credentials := Default(TAuthCredentials);
	Credentials.Email := 'test@mail.ru';
	Credentials.Password := 'password123';
	{User and Domain are empty}

	Result := FStrategy.Authenticate(Credentials, nil, TNullLogger.Create);
	Assert.IsFalse(Result.Success);
	Assert.AreEqual('User and domain must be parsed from email', Result.ErrorMessage);
end;

{ TTwoStepAuthStrategyTest }

procedure TTwoStepAuthStrategyTest.Setup;
begin
	FStrategy := TTwoStepAuthStrategy.Create(TNullPasswordUIProvider.Create);
end;

procedure TTwoStepAuthStrategyTest.TearDown;
begin
	FStrategy.Free;
end;

procedure TTwoStepAuthStrategyTest.TestGetName;
begin
	Assert.AreEqual('Two-Step Web (Deprecated)', FStrategy.GetName);
end;

procedure TTwoStepAuthStrategyTest.TestImplementsInterface;
var
	Strategy: IAuthStrategy;
begin
	Strategy := TTwoStepAuthStrategy.Create(TNullPasswordUIProvider.Create);
	Assert.IsNotNull(Strategy, 'Strategy should implement IAuthStrategy');
end;

procedure TTwoStepAuthStrategyTest.TestAuthenticate_EmptyCredentials_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	Credentials := Default(TAuthCredentials);
	Result := FStrategy.Authenticate(Credentials, nil, TNullLogger.Create);
	Assert.IsFalse(Result.Success);
	Assert.AreEqual('Email and password are required', Result.ErrorMessage);
end;

procedure TTwoStepAuthStrategyTest.TestAuthenticate_NoHTTP_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password123', 'test', 'mail.ru');
	Result := FStrategy.Authenticate(Credentials, nil, TNullLogger.Create);
	Assert.IsFalse(Result.Success);
	Assert.AreEqual('HTTP client is required', Result.ErrorMessage);
end;

procedure TTwoStepAuthStrategyTest.TestAuthenticate_NoPasswordUI_ReturnsFailure;
var
	Strategy: TTwoStepAuthStrategy;
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{Create strategy without PasswordUI}
	Strategy := TTwoStepAuthStrategy.Create(nil);
	try
		Credentials := TAuthCredentials.Create('test@mail.ru', 'password123', 'test', 'mail.ru');
		Result := Strategy.Authenticate(Credentials, nil, TNullLogger.Create);
		Assert.IsFalse(Result.Success);
		Assert.AreEqual('Password UI provider is required for two-step auth', Result.ErrorMessage);
	finally
		Strategy.Free;
	end;
end;

{ TOldOAuthStrategyTest }

procedure TOldOAuthStrategyTest.Setup;
begin
	FStrategy := TOldOAuthStrategy.Create;
end;

procedure TOldOAuthStrategyTest.TearDown;
begin
	FStrategy.Free;
end;

procedure TOldOAuthStrategyTest.TestGetName;
begin
	Assert.AreEqual('Old OAuth (Deprecated)', FStrategy.GetName);
end;

procedure TOldOAuthStrategyTest.TestImplementsInterface;
var
	Strategy: IAuthStrategy;
begin
	Strategy := TOldOAuthStrategy.Create;
	Assert.IsNotNull(Strategy, 'Strategy should implement IAuthStrategy');
end;

procedure TOldOAuthStrategyTest.TestAuthenticate_EmptyCredentials_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	Credentials := Default(TAuthCredentials);
	Result := FStrategy.Authenticate(Credentials, nil, TNullLogger.Create);
	Assert.IsFalse(Result.Success);
	Assert.AreEqual('Email and password are required', Result.ErrorMessage);
end;

procedure TOldOAuthStrategyTest.TestAuthenticate_NoHTTP_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password123', 'test', 'mail.ru');
	Result := FStrategy.Authenticate(Credentials, nil, TNullLogger.Create);
	Assert.IsFalse(Result.Success);
	Assert.AreEqual('HTTP client is required', Result.ErrorMessage);
end;

initialization

TDUnitX.RegisterTestFixture(TWebAuthStrategyTest);
TDUnitX.RegisterTestFixture(TTwoStepAuthStrategyTest);
TDUnitX.RegisterTestFixture(TOldOAuthStrategyTest);

end.
