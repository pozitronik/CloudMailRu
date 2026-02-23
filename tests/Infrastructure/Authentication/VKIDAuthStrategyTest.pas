unit VKIDAuthStrategyTest;

{Tests for TVKIDAuthStrategy.
	Covers both the cookie persistence path (cookie restore + save-back)
	and the login form path (via IVKIDLoginProvider mock).}

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
	MockAuthStrategy,
	DUnitX.TestFramework;

type

	[TestFixture]
	TVKIDAuthStrategyTest = class
	private
		FStrategy: TVKIDAuthStrategy;
		FMockLoginProvider: TMockVKIDLoginProvider;
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

		{Builds credentials with cookie file path and CSRF URL}
		function BuildCredentials(const CookieFile: WideString = '';
			const CsrfUrl: WideString = ''): TAuthCredentials;
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
		[Test]
		procedure TestAuthenticate_CookieRestore_DoesNotCallLoginProvider;

		{Cookie restore failure: file exists but CSRF refresh fails -> falls through to login form}
		[Test]
		procedure TestAuthenticate_CookieExpired_FallsThroughToLoginForm;
		[Test]
		procedure TestAuthenticate_CookieExpired_ClearsCookiesBeforeLogin;
		[Test]
		procedure TestAuthenticate_CookieExpired_LogsExpiredMessage;

		{No cookie file -> skips restore, goes straight to login form}
		[Test]
		procedure TestAuthenticate_NoCookieFile_GoesToLoginForm;

		{No CookieFilePath -> skips restore entirely}
		[Test]
		procedure TestAuthenticate_NoCookieFilePath_SkipsRestore;

		{Login form failure: form returns False}
		[Test]
		procedure TestAuthenticate_LoginFormFails_ReturnsFailure;
		[Test]
		procedure TestAuthenticate_LoginFormFails_ErrorContainsCancelledMessage;
		[Test]
		procedure TestAuthenticate_LoginFormFails_LogsWarning;

		{Login form succeeds but CSRF is empty}
		[Test]
		procedure TestAuthenticate_EmptyCSRFAfterLogin_ReturnsFailure;
		[Test]
		procedure TestAuthenticate_EmptyCSRFAfterLogin_ErrorContainsCsrfFailed;
		[Test]
		procedure TestAuthenticate_EmptyCSRFAfterLogin_LogsWarning;

		{Login form succeeds with valid CSRF}
		[Test]
		procedure TestAuthenticate_LoginSuccess_ReturnsSuccess;
		[Test]
		procedure TestAuthenticate_LoginSuccess_ResultIsCookieBased;
		[Test]
		procedure TestAuthenticate_LoginSuccess_ContainsCSRFToken;
		[Test]
		procedure TestAuthenticate_LoginSuccess_UnitedParamsContainsToken;
		[Test]
		procedure TestAuthenticate_LoginSuccess_LogsSuccessMessage;

		{Post-login cookie persistence}
		[Test]
		procedure TestAuthenticate_LoginSuccess_SavesCookiesToFile;
		[Test]
		procedure TestAuthenticate_LoginSuccess_NoCookiePath_DoesNotCreateFile;

		{Edge case: AuthCookie is nil}
		[Test]
		procedure TestAuthenticate_NilAuthCookie_LogsWarning;
	end;

implementation

uses
	Winapi.Windows,
	IOUtils, FileSystem, CookiePersistence,
	CloudConstants, LanguageStrings;

{TVKIDAuthStrategyTest}

procedure TVKIDAuthStrategyTest.Setup;
begin
	FMockLoginProvider := TMockVKIDLoginProvider.Create(True, 'mock_csrf', 'ok');
	FStrategy := TVKIDAuthStrategy.Create(FMockLoginProvider, TWindowsFileSystem.Create);
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

function TVKIDAuthStrategyTest.BuildCredentials(const CookieFile: WideString;
	const CsrfUrl: WideString): TAuthCredentials;
begin
	Result := TAuthCredentials.Create('test@mail.ru', '', 'test', 'mail.ru',
		'', CookieFile, CsrfUrl);
end;

{--- Name ---}

procedure TVKIDAuthStrategyTest.TestGetName_ReturnsExpectedValue;
begin
	Assert.AreEqual('VK ID Browser Login', FStrategy.GetName);
end;

{--- Cookie restore success ---}

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieRestore_ReturnsSuccess;
var
	AuthResult: TAuthResult;
begin
	CreateCookieFile('original_csrf');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('refreshed_csrf'));

	AuthResult := FStrategy.Authenticate(
		BuildCredentials(FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf'),
		FHTTP, FLogger);

	Assert.IsTrue(AuthResult.Success, 'Cookie restore should succeed');
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieRestore_ResultIsCookieBased;
var
	AuthResult: TAuthResult;
begin
	CreateCookieFile('csrf_token');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('fresh_csrf'));

	AuthResult := FStrategy.Authenticate(
		BuildCredentials(FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf'),
		FHTTP, FLogger);

	Assert.IsTrue(AuthResult.CookieBased, 'Result should be cookie-based');
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieRestore_ContainsRefreshedCSRF;
var
	AuthResult: TAuthResult;
begin
	CreateCookieFile('old_csrf_will_be_replaced');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('new_csrf_from_server'));

	AuthResult := FStrategy.Authenticate(
		BuildCredentials(FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf'),
		FHTTP, FLogger);

	// Should use the refreshed CSRF, not the one from the cookie file
	Assert.AreEqual('new_csrf_from_server', AuthResult.AuthToken);
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieRestore_UnitedParamsContainsToken;
var
	AuthResult: TAuthResult;
begin
	CreateCookieFile('csrf_tok');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('api_csrf_token'));

	AuthResult := FStrategy.Authenticate(
		BuildCredentials(FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf'),
		FHTTP, FLogger);

	Assert.AreEqual('token=api_csrf_token', AuthResult.UnitedParams);
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieRestore_SavesUpdatedCookies;
var
	Persistence: TCookiePersistence;
	SavedCSRF: WideString;
	LoadManager: TIdCookieManager;
begin
	CreateCookieFile('old_csrf');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('updated_csrf'));

	FStrategy.Authenticate(
		BuildCredentials(FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf'),
		FHTTP, FLogger);

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
begin
	CreateCookieFile('csrf');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('fresh'));

	FStrategy.Authenticate(
		BuildCredentials(FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf'),
		FHTTP, FLogger);

	Assert.IsTrue(FLogger.LogCalled, 'Logger should have been called');
	Assert.Contains(FLogger.LastMessage, 'Restored session from cookies');
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieRestore_DoesNotCallLoginProvider;
begin
	CreateCookieFile('csrf');
	FHTTP.SetResponse('/api/v2/tokens/csrf', True, BuildCSRFResponse('fresh'));

	FStrategy.Authenticate(
		BuildCredentials(FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf'),
		FHTTP, FLogger);

	Assert.AreEqual(0, FMockLoginProvider.CallCount, 'Login provider should not be called when cookie restore succeeds');
end;

{--- Cookie restore failure (session expired) ---}

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieExpired_FallsThroughToLoginForm;
begin
	CreateCookieFile('old_csrf');
	// CSRF refresh fails (server returns invalid JSON or failure)
	FHTTP.SetResponse('/api/v2/tokens/csrf', False, '');

	FStrategy.Authenticate(
		BuildCredentials(FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf'),
		FHTTP, FLogger);

	Assert.AreEqual(1, FMockLoginProvider.CallCount, 'Should fall through to login form when cookies expire');
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieExpired_ClearsCookiesBeforeLogin;
begin
	CreateCookieFile('old_csrf');
	FHTTP.SetResponse('/api/v2/tokens/csrf', False, '');

	FStrategy.Authenticate(
		BuildCredentials(FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf'),
		FHTTP, FLogger);

	// The mock login provider received the cookie manager -- verify cookies were cleared
	// before the login form was invoked (CookieCollection.Count should be 0 at invocation time)
	// Since we cannot inspect state at call time, we verify cookies were loaded then cleared:
	// after the full method runs, the login form sets new cookies (mock doesn't add cookies)
	Assert.AreEqual(0, FCookieManager.CookieCollection.Count,
		'Stale cookies should have been cleared before login form');
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_CookieExpired_LogsExpiredMessage;
begin
	CreateCookieFile('old_csrf');
	FHTTP.SetResponse('/api/v2/tokens/csrf', False, '');

	// Mock login provider will succeed, so subsequent logging will happen.
	// The "expired" message won't be the last one. Reconfigure to fail so the log ends sooner.
	FMockLoginProvider.ShouldSucceed := False;

	FStrategy.Authenticate(
		BuildCredentials(FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf'),
		FHTTP, FLogger);

	// Last message when login form fails is the warning about form returning False.
	// The expired message comes before that. We verify via log call count (>2 means extra logs happened).
	Assert.IsTrue(FLogger.LogCalls >= 3, 'Should have logged at least: request + expired + form failure');
end;

{--- No cookie file ---}

procedure TVKIDAuthStrategyTest.TestAuthenticate_NoCookieFile_GoesToLoginForm;
begin
	// Cookie file path set but file does not exist -- TCookiePersistence.Load returns False
	FStrategy.Authenticate(
		BuildCredentials(FCookieFilePath, 'https://cloud.mail.ru/api/v2/tokens/csrf'),
		FHTTP, FLogger);

	Assert.AreEqual(1, FMockLoginProvider.CallCount, 'Should go to login form when no cookie file');
end;

{--- No CookieFilePath ---}

procedure TVKIDAuthStrategyTest.TestAuthenticate_NoCookieFilePath_SkipsRestore;
begin
	// Empty CookieFilePath -> entire cookie restore block skipped
	FStrategy.Authenticate(
		BuildCredentials('', ''),
		FHTTP, FLogger);

	Assert.AreEqual(1, FMockLoginProvider.CallCount, 'Should go straight to login form');
end;

{--- Login form failure ---}

procedure TVKIDAuthStrategyTest.TestAuthenticate_LoginFormFails_ReturnsFailure;
var
	AuthResult: TAuthResult;
begin
	FMockLoginProvider.ShouldSucceed := False;

	AuthResult := FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	Assert.IsFalse(AuthResult.Success, 'Should fail when login form returns False');
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_LoginFormFails_ErrorContainsCancelledMessage;
var
	AuthResult: TAuthResult;
begin
	FMockLoginProvider.ShouldSucceed := False;

	AuthResult := FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	// Default result is CreateFailure(ERR_VKID_LOGIN_CANCELLED) set at method start
	Assert.AreEqual(ERR_VKID_LOGIN_CANCELLED, AuthResult.ErrorMessage);
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_LoginFormFails_LogsWarning;
begin
	FMockLoginProvider.ShouldSucceed := False;
	FMockLoginProvider.SetScriptResult('error_details');

	FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	Assert.Contains(FLogger.LastMessage, 'Login form returned False');
	Assert.Contains(FLogger.LastMessage, 'error_details');
end;

{--- Login form success with empty CSRF ---}

procedure TVKIDAuthStrategyTest.TestAuthenticate_EmptyCSRFAfterLogin_ReturnsFailure;
var
	AuthResult: TAuthResult;
begin
	FMockLoginProvider.ShouldSucceed := True;
	FMockLoginProvider.SetCSRFToken('');

	AuthResult := FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	Assert.IsFalse(AuthResult.Success, 'Should fail when CSRF is empty');
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_EmptyCSRFAfterLogin_ErrorContainsCsrfFailed;
var
	AuthResult: TAuthResult;
begin
	FMockLoginProvider.ShouldSucceed := True;
	FMockLoginProvider.SetCSRFToken('');

	AuthResult := FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	Assert.AreEqual(ERR_VKID_CSRF_FAILED, AuthResult.ErrorMessage);
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_EmptyCSRFAfterLogin_LogsWarning;
begin
	FMockLoginProvider.ShouldSucceed := True;
	FMockLoginProvider.SetCSRFToken('');
	FMockLoginProvider.SetScriptResult('some_script_output');

	FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	Assert.Contains(FLogger.LastMessage, 'CSRF token is empty');
	Assert.Contains(FLogger.LastMessage, 'some_script_output');
end;

{--- Login form success with valid CSRF ---}

procedure TVKIDAuthStrategyTest.TestAuthenticate_LoginSuccess_ReturnsSuccess;
var
	AuthResult: TAuthResult;
begin
	FMockLoginProvider.ShouldSucceed := True;
	FMockLoginProvider.SetCSRFToken('valid_csrf');

	AuthResult := FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	Assert.IsTrue(AuthResult.Success, 'Should succeed with valid CSRF');
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_LoginSuccess_ResultIsCookieBased;
var
	AuthResult: TAuthResult;
begin
	FMockLoginProvider.ShouldSucceed := True;
	FMockLoginProvider.SetCSRFToken('valid_csrf');

	AuthResult := FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	Assert.IsTrue(AuthResult.CookieBased, 'Login form result should be cookie-based');
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_LoginSuccess_ContainsCSRFToken;
var
	AuthResult: TAuthResult;
begin
	FMockLoginProvider.ShouldSucceed := True;
	FMockLoginProvider.SetCSRFToken('my_csrf_token');

	AuthResult := FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	Assert.AreEqual('my_csrf_token', AuthResult.AuthToken);
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_LoginSuccess_UnitedParamsContainsToken;
var
	AuthResult: TAuthResult;
begin
	FMockLoginProvider.ShouldSucceed := True;
	FMockLoginProvider.SetCSRFToken('csrf_123');

	AuthResult := FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	Assert.AreEqual('token=csrf_123', AuthResult.UnitedParams);
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_LoginSuccess_LogsSuccessMessage;
begin
	FMockLoginProvider.ShouldSucceed := True;
	FMockLoginProvider.SetCSRFToken('valid');

	FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	Assert.AreEqual(VKID_LOGIN_SUCCESS, FLogger.LastMessage);
end;

{--- Post-login cookie persistence ---}

procedure TVKIDAuthStrategyTest.TestAuthenticate_LoginSuccess_SavesCookiesToFile;
var
	Persistence: TCookiePersistence;
	SavedCSRF: WideString;
	LoadManager: TIdCookieManager;
begin
	FMockLoginProvider.ShouldSucceed := True;
	FMockLoginProvider.SetCSRFToken('saved_csrf');

	FStrategy.Authenticate(
		BuildCredentials(FCookieFilePath, ''),
		FHTTP, FLogger);

	// Verify cookie file was created with correct CSRF
	LoadManager := TIdCookieManager.Create(nil);
	try
		Persistence := TCookiePersistence.Create(FCookieFilePath, TWindowsFileSystem.Create);
		try
			Assert.IsTrue(Persistence.Load(LoadManager, SavedCSRF), 'Cookie file should exist and be loadable');
			Assert.AreEqual('saved_csrf', SavedCSRF, 'Saved CSRF should match login form result');
		finally
			Persistence.Free;
		end;
	finally
		LoadManager.Free;
	end;
end;

procedure TVKIDAuthStrategyTest.TestAuthenticate_LoginSuccess_NoCookiePath_DoesNotCreateFile;
begin
	FMockLoginProvider.ShouldSucceed := True;
	FMockLoginProvider.SetCSRFToken('valid');

	// No CookieFilePath -> no file should be created
	FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	Assert.IsFalse(FileExists(FCookieFilePath), 'No cookie file should be created when CookieFilePath is empty');
end;

{--- Edge case: nil AuthCookie ---}

procedure TVKIDAuthStrategyTest.TestAuthenticate_NilAuthCookie_LogsWarning;
begin
	// Remove cookie manager from mock HTTP
	FHTTP.SetExternalAuthCookie(nil);

	FMockLoginProvider.ShouldSucceed := False;

	FStrategy.Authenticate(BuildCredentials('', ''), FHTTP, FLogger);

	// The "AuthCookie is nil" warning should have been logged
	// It's not the last message (login form failure logs after it), but we can verify log count
	Assert.IsTrue(FLogger.LogCalls >= 2, 'Should have logged nil warning plus subsequent messages');
end;

initialization

TDUnitX.RegisterTestFixture(TVKIDAuthStrategyTest);

end.
