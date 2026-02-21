unit VKIDAuthStrategyTest;

{Tests for TVKIDAuthStrategy.
	Covers the cookie persistence path of Authenticate (cookie restore + save-back).
	The WebView2 login form path is NOT tested here because it requires a GUI
	environment with WebView2 runtime. Tests that would fall through to the login
	form are intentionally excluded to avoid blocking MessageBox calls.}

interface

uses
	SysUtils, Classes,
	IdCookieManager,
	AuthStrategy,
	CloudHTTP,
	Logger,
	VKIDAuthStrategy,
	MockCloudHTTP,
	MockLogger,
	DUnitX.TestFramework;

type

	[TestFixture]
	TVKIDAuthStrategyTest = class
	private
		FStrategy: TVKIDAuthStrategy;
		FHTTP: TMockCloudHTTP;
		FHTTPIntf: ICloudHTTP; {prevents premature free via ref counting}
		FLogger: TMockLogger;
		FLoggerIntf: ILogger; {prevents premature free via ref counting}
		FCookieManager: TIdCookieManager;
		FTempDir: WideString;
		FCookieFilePath: WideString;

		{Creates a cookie file with valid content for session restore testing}
		procedure CreateCookieFile(const CSRFToken: WideString);

		{Builds a CSRF API response that getBodyToken can parse}
		function BuildCSRFResponse(const Token: WideString): WideString;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestGetName_ReturnsExpectedValue;

		{Cookie restore success: file exists, CSRF refresh succeeds}
		[Test]
		procedure TestAuthenticate_CookieRestore_ReturnsSuccess;
		[Test]
		procedure TestAuthenticate_CookieRestore_ResultIsCookieBased;
		[Test]
		procedure TestAuthenticate_CookieRestore_ContainsRefreshedCSRF;
		[Test]
		procedure TestAuthenticate_CookieRestore_UnitedParamsContainsToken;
		[Test]
		procedure TestAuthenticate_CookieRestore_SavesUpdatedCookies;
		[Test]
		procedure TestAuthenticate_CookieRestore_LogsRestoreMessage;
	end;

implementation

uses
	Winapi.Windows,
	IOUtils, FileSystem, CookiePersistence;

{TVKIDAuthStrategyTest}

procedure TVKIDAuthStrategyTest.Setup;
begin
	FStrategy := TVKIDAuthStrategy.Create;
	FHTTP := TMockCloudHTTP.Create;
	FHTTPIntf := FHTTP;
	FLogger := TMockLogger.Create;
	FLoggerIntf := FLogger;
	FCookieManager := TIdCookieManager.Create(nil);

	{Provide a real cookie manager to MockCloudHTTP}
	FHTTP.SetExternalAuthCookie(FCookieManager);

	{Create temp directory for cookie file I/O}
	FTempDir := IncludeTrailingPathDelimiter(TPath.GetTempPath) + 'CloudMailRuTest_VKID_' + IntToStr(GetCurrentThreadId);
	ForceDirectories(FTempDir);
	FCookieFilePath := IncludeTrailingPathDelimiter(FTempDir) + 'test.cookies';
end;

procedure TVKIDAuthStrategyTest.TearDown;
begin
	FStrategy.Free;
	{MockCloudHTTP does not own FCookieManager (SetExternalAuthCookie)}
	FCookieManager.Free;
	FHTTPIntf := nil;
	FLoggerIntf := nil;

	{Clean up temp files}
	if DirectoryExists(FTempDir) then
		TDirectory.Delete(FTempDir, True);
end;

procedure TVKIDAuthStrategyTest.CreateCookieFile(const CSRFToken: WideString);
var
	Lines: TStringList;
begin
	Lines := TStringList.Create;
	try
		Lines.Add('__csrf__'#9 + CSRFToken);
		Lines.Add('sid'#9'session_value'#9'.mail.ru'#9'/'#9'1'#9'0');
		Lines.Add('t'#9'tracking_value'#9'.cloud.mail.ru'#9'/'#9'0'#9'0');
		Lines.SaveToFile(FCookieFilePath, TEncoding.UTF8);
	finally
		Lines.Free;
	end;
end;

function TVKIDAuthStrategyTest.BuildCSRFResponse(const Token: WideString): WideString;
begin
	// getBodyToken parses: body.token from JSON
	Result := '{"body":{"token":"' + Token + '"},"status":200}';
end;

procedure TVKIDAuthStrategyTest.TestGetName_ReturnsExpectedValue;
begin
	Assert.AreEqual('VK ID Browser Login', FStrategy.GetName);
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieRestore_ReturnsSuccess;
var
	Credentials: TAuthCredentials;
	AuthResult: TAuthResult;
begin
	CreateCookieFile('original_csrf');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('refreshed_csrf'));

	Credentials := TAuthCredentials.Create('test@mail.ru', '', 'test', 'mail.ru',
		'', FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf');

	AuthResult := FStrategy.Authenticate(Credentials, FHTTP, FLogger);

	Assert.IsTrue(AuthResult.Success, 'Cookie restore should succeed');
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieRestore_ResultIsCookieBased;
var
	Credentials: TAuthCredentials;
	AuthResult: TAuthResult;
begin
	CreateCookieFile('csrf_token');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('fresh_csrf'));

	Credentials := TAuthCredentials.Create('test@mail.ru', '', 'test', 'mail.ru',
		'', FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf');

	AuthResult := FStrategy.Authenticate(Credentials, FHTTP, FLogger);

	Assert.IsTrue(AuthResult.CookieBased, 'Result should be cookie-based');
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieRestore_ContainsRefreshedCSRF;
var
	Credentials: TAuthCredentials;
	AuthResult: TAuthResult;
begin
	CreateCookieFile('old_csrf_will_be_replaced');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('new_csrf_from_server'));

	Credentials := TAuthCredentials.Create('test@mail.ru', '', 'test', 'mail.ru',
		'', FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf');

	AuthResult := FStrategy.Authenticate(Credentials, FHTTP, FLogger);

	// Should use the refreshed CSRF, not the one from the cookie file
	Assert.AreEqual('new_csrf_from_server', AuthResult.AuthToken);
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieRestore_UnitedParamsContainsToken;
var
	Credentials: TAuthCredentials;
	AuthResult: TAuthResult;
begin
	CreateCookieFile('csrf_tok');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('api_csrf_token'));

	Credentials := TAuthCredentials.Create('test@mail.ru', '', 'test', 'mail.ru',
		'', FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf');

	AuthResult := FStrategy.Authenticate(Credentials, FHTTP, FLogger);

	Assert.AreEqual('token=api_csrf_token', AuthResult.UnitedParams);
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieRestore_SavesUpdatedCookies;
var
	Credentials: TAuthCredentials;
	Persistence: TCookiePersistence;
	SavedCSRF: WideString;
	LoadManager: TIdCookieManager;
begin
	CreateCookieFile('old_csrf');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('updated_csrf'));

	Credentials := TAuthCredentials.Create('test@mail.ru', '', 'test', 'mail.ru',
		'', FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf');

	FStrategy.Authenticate(Credentials, FHTTP, FLogger);

	// Read the cookie file back and verify the CSRF was updated
	LoadManager := TIdCookieManager.Create(nil);
	try
		Persistence := TCookiePersistence.Create(FCookieFilePath, TWindowsFileSystem.Create);
		try
			Assert.IsTrue(Persistence.Load(LoadManager, SavedCSRF), 'Should be able to reload saved cookies');
			Assert.AreEqual('updated_csrf', SavedCSRF, 'Saved CSRF should be the refreshed one');
		finally
			Persistence.Free;
		end;
	finally
		LoadManager.Free;
	end;
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieRestore_LogsRestoreMessage;
var
	Credentials: TAuthCredentials;
begin
	CreateCookieFile('csrf');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('fresh'));

	Credentials := TAuthCredentials.Create('test@mail.ru', '', 'test', 'mail.ru',
		'', FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf');

	FStrategy.Authenticate(Credentials, FHTTP, FLogger);

	Assert.IsTrue(FLogger.LogCalled, 'Logger should have been called');
	Assert.Contains(FLogger.LastMessage, 'Restored session from cookies');
end;

initialization

TDUnitX.RegisterTestFixture(TVKIDAuthStrategyTest);

end.
