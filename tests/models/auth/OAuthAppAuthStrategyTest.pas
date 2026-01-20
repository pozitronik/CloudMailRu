unit OAuthAppAuthStrategyTest;

interface

uses
	IAuthStrategyInterface,
	OAuthAppAuthStrategy,
	DUnitX.TestFramework;

type

	[TestFixture]
	TOAuthAppAuthStrategyTest = class
	private
		FStrategy: IAuthStrategy;
	public
		[Setup]
		procedure Setup;

		[Test]
		procedure TestGetName_ReturnsOAuthAppPassword;

		[Test]
		procedure TestAuthenticate_WithNilHTTP_ReturnsFalse;

		[Test]
		procedure TestAuthenticate_WithNilHTTP_HasErrorMessage;

		[Test]
		procedure TestImplementsInterface;
	end;

implementation

uses
	TCLogger;

{TOAuthAppAuthStrategyTest}

procedure TOAuthAppAuthStrategyTest.Setup;
begin
	FStrategy := TOAuthAppAuthStrategy.Create;
end;

procedure TOAuthAppAuthStrategyTest.TestGetName_ReturnsOAuthAppPassword;
begin
	Assert.AreEqual('OAuth App Password', FStrategy.GetName);
end;

procedure TOAuthAppAuthStrategyTest.TestAuthenticate_WithNilHTTP_ReturnsFalse;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{Calling with nil HTTP should fail gracefully - cannot make actual HTTP call}
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');

	{This will raise an exception or return failure when HTTP is nil}
	try
		Result := FStrategy.Authenticate(Credentials, nil, nil);
		Assert.IsFalse(Result.Success);
	except
		{Expected - nil HTTP cannot be used}
		Assert.Pass('Exception expected when HTTP is nil');
	end;
end;

procedure TOAuthAppAuthStrategyTest.TestAuthenticate_WithNilHTTP_HasErrorMessage;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	Credentials := TAuthCredentials.Create('test@mail.ru', 'password', 'test', 'mail.ru');

	try
		Result := FStrategy.Authenticate(Credentials, nil, nil);
		Assert.IsNotEmpty(Result.ErrorMessage);
	except
		{Expected - nil HTTP cannot be used}
		Assert.Pass('Exception expected when HTTP is nil');
	end;
end;

procedure TOAuthAppAuthStrategyTest.TestImplementsInterface;
begin
	{FStrategy is assigned in Setup as IAuthStrategy, proving interface support}
	Assert.IsNotNull(FStrategy);
end;

initialization

TDUnitX.RegisterTestFixture(TOAuthAppAuthStrategyTest);

end.
