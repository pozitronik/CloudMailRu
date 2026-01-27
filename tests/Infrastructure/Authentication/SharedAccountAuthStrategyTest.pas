unit SharedAccountAuthStrategyTest;

{Tests for TSharedAccountAuthStrategy - public/shared weblink authentication.
 This strategy doesn't use credentials, it fetches the public URL page
 and extracts the shard information needed to access shared content.}

interface

uses
	SharedAccountAuthStrategy,
	AuthStrategy,
	CloudHTTP,
	TCLogger,
	MockCloudHTTP,
	DUnitX.TestFramework;

type
	{Mock logger that tracks calls}
	TMockAuthLogger = class(TInterfacedObject, ILogger)
	public
		LogCalls: Integer;
		LastLogMessage: WideString;
		constructor Create;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
	end;

	[TestFixture]
	TSharedAccountAuthStrategyTest = class
	private
		FStrategy: TSharedAccountAuthStrategy;
		FMockHTTP: TMockCloudHTTP;
		FMockLoggerRef: ILogger; {Keeps object alive via ref counting}
		FMockLogger: TMockAuthLogger; {Access to mock-specific properties}
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

		{ Successful authentication }
		[Test]
		procedure TestAuthenticate_ValidPage_ReturnsSuccess;
		[Test]
		procedure TestAuthenticate_ValidPage_ExtractsPublicShard;
		[Test]
		procedure TestAuthenticate_ValidPage_ExtractsPublicLink;

		{ HTTP failure }
		[Test]
		procedure TestAuthenticate_HttpFails_ReturnsFailure;

		{ Shard extraction failure }
		[Test]
		procedure TestAuthenticate_NoShardInPage_ReturnsFailure;
		[Test]
		procedure TestAuthenticate_NoShardInPage_LogsError;

		{ URL parsing tests }
		[Test]
		procedure TestAuthenticate_UrlWithTrailingSlash_RemovesSlash;
		[Test]
		procedure TestAuthenticate_UrlWithoutPrefix_ExtractsCorrectly;

		{ Logging tests }
		[Test]
		procedure TestAuthenticate_LogsUrlBeforeFetch;
	end;

implementation

uses
	SysUtils,
	TestHelper;

const
	{Sample page content with valid shard info}
	VALID_PAGE_CONTENT = '<!DOCTYPE html><html><script>var conf = {"weblink_get":{"url":"https://cloclo123.cloud.mail.ru/weblink/get/"}};</script></html>';
	{Page content without shard info}
	INVALID_PAGE_CONTENT = '<!DOCTYPE html><html><body>No shard here</body></html>';

{ TMockAuthLogger }

constructor TMockAuthLogger.Create;
begin
	inherited Create;
	LogCalls := 0;
	LastLogMessage := '';
end;

procedure TMockAuthLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	Inc(LogCalls);
	LastLogMessage := LogString;
end;

procedure TMockAuthLogger.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
var
	i: Integer;
	TempStr: String;
	ArgStr: String;
begin
	Inc(LogCalls);
	TempStr := String(LogString);

	{Simple %s substitution for testing purposes}
	for i := 0 to High(Args) do
	begin
		case Args[i].VType of
			vtWideString: ArgStr := String(WideString(Args[i].VWideString));
			vtString: ArgStr := String(Args[i].VString^);
			vtAnsiString: ArgStr := String(AnsiString(Args[i].VAnsiString));
			vtUnicodeString: ArgStr := String(UnicodeString(Args[i].VUnicodeString));
			vtInteger: ArgStr := IntToStr(Args[i].VInteger);
		else
			ArgStr := '?';
		end;
		TempStr := StringReplace(TempStr, '%s', ArgStr, []);
	end;

	LastLogMessage := TempStr;
end;

{ TSharedAccountAuthStrategyTest }

procedure TSharedAccountAuthStrategyTest.Setup;
begin
	FStrategy := TSharedAccountAuthStrategy.Create;
	FMockHTTP := TMockCloudHTTP.Create;
	FMockLogger := TMockAuthLogger.Create;
	FMockLoggerRef := FMockLogger; {Keep reference alive}
end;

procedure TSharedAccountAuthStrategyTest.TearDown;
begin
	FStrategy.Free;
	FMockHTTP := nil;
	FMockLoggerRef := nil; {Release interface reference}
	FMockLogger := nil;
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
	{Empty public URL should fail}
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

{ Successful authentication }

procedure TSharedAccountAuthStrategyTest.TestAuthenticate_ValidPage_ReturnsSuccess;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{Configure mock to return valid page with shard info}
	FMockHTTP.SetResponse('', True, VALID_PAGE_CONTENT);
	Credentials := TAuthCredentials.CreatePublic('https://cloud.mail.ru/public/ABC123');

	Result := FStrategy.Authenticate(Credentials, FMockHTTP, FMockLogger);

	Assert.IsTrue(Result.Success, 'Valid page should authenticate successfully');
end;

procedure TSharedAccountAuthStrategyTest.TestAuthenticate_ValidPage_ExtractsPublicShard;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{Configure mock to return valid page with shard info}
	FMockHTTP.SetResponse('', True, VALID_PAGE_CONTENT);
	Credentials := TAuthCredentials.CreatePublic('https://cloud.mail.ru/public/ABC123');

	Result := FStrategy.Authenticate(Credentials, FMockHTTP, FMockLogger);

	Assert.AreEqual('https://cloclo123.cloud.mail.ru/weblink/get/', Result.PublicShard, 'Should extract public shard from page');
end;

procedure TSharedAccountAuthStrategyTest.TestAuthenticate_ValidPage_ExtractsPublicLink;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{Configure mock to return valid page with shard info}
	FMockHTTP.SetResponse('', True, VALID_PAGE_CONTENT);
	Credentials := TAuthCredentials.CreatePublic('https://cloud.mail.ru/public/ABC123');

	Result := FStrategy.Authenticate(Credentials, FMockHTTP, FMockLogger);

	Assert.AreEqual('ABC123', Result.PublicLink, 'Should extract public link from URL');
end;

{ HTTP failure }

procedure TSharedAccountAuthStrategyTest.TestAuthenticate_HttpFails_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{Configure mock to fail HTTP request}
	FMockHTTP.SetResponse('', False, '');
	Credentials := TAuthCredentials.CreatePublic('https://cloud.mail.ru/public/ABC123');

	Result := FStrategy.Authenticate(Credentials, FMockHTTP, FMockLogger);

	Assert.IsFalse(Result.Success, 'HTTP failure should fail authentication');
	Assert.AreEqual('Failed to fetch public URL page', Result.ErrorMessage);
end;

{ Shard extraction failure }

procedure TSharedAccountAuthStrategyTest.TestAuthenticate_NoShardInPage_ReturnsFailure;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{Configure mock to return page without shard info}
	FMockHTTP.SetResponse('', True, INVALID_PAGE_CONTENT);
	Credentials := TAuthCredentials.CreatePublic('https://cloud.mail.ru/public/ABC123');

	Result := FStrategy.Authenticate(Credentials, FMockHTTP, FMockLogger);

	Assert.IsFalse(Result.Success, 'Page without shard should fail authentication');
	Assert.AreEqual('Failed to extract public shard from page', Result.ErrorMessage);
end;

procedure TSharedAccountAuthStrategyTest.TestAuthenticate_NoShardInPage_LogsError;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{Configure mock to return page without shard info}
	FMockHTTP.SetResponse('', True, INVALID_PAGE_CONTENT);
	Credentials := TAuthCredentials.CreatePublic('https://cloud.mail.ru/public/ABC123');

	Result := FStrategy.Authenticate(Credentials, FMockHTTP, FMockLogger);

	{Should log error when shard extraction fails}
	Assert.IsTrue(FMockLogger.LogCalls >= 2, 'Should log URL and error'); {URL log + error log}
end;

{ URL parsing tests }

procedure TSharedAccountAuthStrategyTest.TestAuthenticate_UrlWithTrailingSlash_RemovesSlash;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{Configure mock to return valid page}
	FMockHTTP.SetResponse('', True, VALID_PAGE_CONTENT);
	Credentials := TAuthCredentials.CreatePublic('https://cloud.mail.ru/public/XYZ789/');

	Result := FStrategy.Authenticate(Credentials, FMockHTTP, FMockLogger);

	Assert.AreEqual('XYZ789', Result.PublicLink, 'Should remove trailing slash from public link');
end;

procedure TSharedAccountAuthStrategyTest.TestAuthenticate_UrlWithoutPrefix_ExtractsCorrectly;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{Configure mock to return valid page}
	FMockHTTP.SetResponse('', True, VALID_PAGE_CONTENT);
	{URL already without the cloud.mail.ru prefix - just the link path}
	Credentials := TAuthCredentials.CreatePublic('https://cloud.mail.ru/public/DEEP/PATH/LINK');

	Result := FStrategy.Authenticate(Credentials, FMockHTTP, FMockLogger);

	Assert.AreEqual('DEEP/PATH/LINK', Result.PublicLink, 'Should handle deep path links');
end;

{ Logging tests }

procedure TSharedAccountAuthStrategyTest.TestAuthenticate_LogsUrlBeforeFetch;
var
	Credentials: TAuthCredentials;
	Result: TAuthResult;
begin
	{Configure mock to return valid page}
	FMockHTTP.SetResponse('', True, VALID_PAGE_CONTENT);
	Credentials := TAuthCredentials.CreatePublic('https://cloud.mail.ru/public/TESTLINK');

	Result := FStrategy.Authenticate(Credentials, FMockHTTP, FMockLogger);

	{Should log the URL before fetching}
	Assert.IsTrue(FMockLogger.LogCalls > 0, 'Should log the URL');
	Assert.IsTrue(Pos('TESTLINK', String(FMockLogger.LastLogMessage)) > 0, 'Log should contain the URL');
end;

initialization

TDUnitX.RegisterTestFixture(TSharedAccountAuthStrategyTest);

end.
