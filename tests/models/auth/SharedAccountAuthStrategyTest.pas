unit SharedAccountAuthStrategyTest;

{Tests for TSharedAccountAuthStrategy - public/shared weblink authentication.
 This strategy doesn't use credentials, it fetches the public URL page
 and extracts the shard information needed to access shared content.}

interface

uses
	SharedAccountAuthStrategy,
	IAuthStrategyInterface,
	ILoggerInterface,
	DUnitX.TestFramework;

type
	[TestFixture]
	TSharedAccountAuthStrategyTest = class
	private
		FStrategy: TSharedAccountAuthStrategy;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ Basic tests }
		[Test]
		procedure TestGetName;
		[Test]
		procedure TestImplementsInterface;

		{ Authentication with missing credentials }
		[Test]
		procedure TestAuthenticate_EmptyPublicUrl_ReturnsFailure;

		{ TAuthCredentials.CreatePublic helper }
		[Test]
		procedure TestCreatePublicCredentials;
	end;

implementation

uses
	TestHelper;

{ TSharedAccountAuthStrategyTest }

procedure TSharedAccountAuthStrategyTest.Setup;
begin
	FStrategy := TSharedAccountAuthStrategy.Create;
end;

procedure TSharedAccountAuthStrategyTest.TearDown;
begin
	FStrategy.Free;
end;

procedure TSharedAccountAuthStrategyTest.TestGetName;
begin
	Assert.AreEqual('Shared Account', FStrategy.GetName);
end;

procedure TSharedAccountAuthStrategyTest.TestImplementsInterface;
var
	Strategy: IAuthStrategy;
begin
	Strategy := TSharedAccountAuthStrategy.Create;
	Assert.IsNotNull(Strategy, 'Strategy should implement IAuthStrategy');
end;

procedure TSharedAccountAuthStrategyTest.TestAuthenticate_EmptyPublicUrl_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{ Empty public URL should fail }
	Credentials := TAuthCredentials.CreatePublic('');

	Result := FStrategy.Authenticate(Credentials, nil, TNullLogger.Create);

	Assert.IsFalse(Result.Success, 'Empty public URL should fail authentication');
	Assert.AreEqual('Public URL is required for shared account authentication', Result.ErrorMessage);
end;

procedure TSharedAccountAuthStrategyTest.TestCreatePublicCredentials;
var
	Credentials: TAuthCredentials;
begin
	Credentials := TAuthCredentials.CreatePublic('https://cloud.mail.ru/public/ABC123');

	Assert.AreEqual('', Credentials.Email, 'Public credentials should have empty email');
	Assert.AreEqual('', Credentials.Password, 'Public credentials should have empty password');
	Assert.AreEqual('https://cloud.mail.ru/public/ABC123', Credentials.PublicUrl);
end;

initialization

TDUnitX.RegisterTestFixture(TSharedAccountAuthStrategyTest);

end.
