unit VKIDAuthStrategy;

{VK ID authentication strategy using embedded WebView2 browser.
	Shows a login form where the user authenticates via VK ID on mail.ru,
	then extracts cookies and obtains a CSRF token for API access.
	Unlike OAuth, this mode uses cookies for session auth and CSRF
	token for API calls, downloads, and uploads.
	Supports optional cookie persistence: if CookieFilePath is set,
	saved cookies are tried first to skip the WebView2 login.}

interface

uses
	AuthStrategy,
	CloudHTTP,
	Logger;

type
	TVKIDAuthStrategy = class(TInterfacedObject, IAuthStrategy)
	public
		function Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
		function GetName: WideString;
	end;

implementation

uses
	SysUtils, Classes, Math,
	Winapi.Windows,
	CloudConstants,
	WFXTypes,
	LanguageStrings,
	FileSystem,
	CookiePersistence,
	JSONHelper,
	VKIDLogin;

{TVKIDAuthStrategy}

function TVKIDAuthStrategy.Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
var
	CSRFToken: WideString;
	FreshCSRFToken: WideString;
	ScriptResult: WideString;
	CookieCount: Integer;
	I: Integer;
	Persistence: TCookiePersistence;
	JSON: WideString;
	ShowProgress: Boolean;
begin
	Result := TAuthResult.CreateFailure(ERR_VKID_LOGIN_CANCELLED);

	Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, REQUESTING_VKID_LOGIN, [Credentials.Email]);

	if HTTP.AuthCookie = nil then
		Logger.Log(LOG_LEVEL_WARNING, msgtype_details, 'VK ID: AuthCookie is nil!', []);

	{Try to restore session from saved cookies}
	if Credentials.CookieFilePath <> '' then
	begin
		Persistence := TCookiePersistence.Create(Credentials.CookieFilePath, TWindowsFileSystem.Create);
		try
			if Persistence.Load(HTTP.AuthCookie, CSRFToken) then
			begin
				// Validate session by refreshing CSRF token via API call
				ShowProgress := False;
				if HTTP.GetPage(Credentials.CsrfUrl, JSON, ShowProgress) and getBodyToken(JSON, FreshCSRFToken) then
				begin
					Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, 'VK ID: Restored session from cookies, CSRF refreshed', []);
					// Save updated cookies (server may have rotated them)
					Persistence.Save(HTTP.AuthCookie, FreshCSRFToken);
					Result := TAuthResult.CreateCookieSuccess(FreshCSRFToken);
					Exit;
				end;
				// Session expired - clear stale cookies before showing WebView2
				HTTP.AuthCookie.CookieCollection.Clear;
				Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, 'VK ID: Saved cookies expired, showing login form', []);
			end;
		finally
			Persistence.Free;
		end;
	end;

	{Show WebView2 login form: user logs in, browser navigates to cloud.mail.ru,
		CSRF token is fetched via JavaScript (which has access to all browser cookies
		including SDC), and cookies are injected into Indy's cookie manager.}
	if not TVKIDLoginForm.Execute(0, HTTP.AuthCookie, CSRFToken, ScriptResult) then
	begin
		Logger.Log(LOG_LEVEL_WARNING, msgtype_details, 'VK ID: Login form returned False (script: %s)', [ScriptResult]);
		Exit;
	end;

	CookieCount := HTTP.AuthCookie.CookieCollection.Count;
	Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, 'VK ID: Cookies=%d, CSRF length=%d, script=%s', [CookieCount, Length(CSRFToken), ScriptResult]);
	for I := 0 to Min(CookieCount - 1, 9) do
		Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, 'VK ID: Cookie[%d] name=%s domain=%s path=%s', [
			I,
			HTTP.AuthCookie.CookieCollection.Cookies[I].CookieName,
			HTTP.AuthCookie.CookieCollection.Cookies[I].Domain,
			HTTP.AuthCookie.CookieCollection.Cookies[I].Path
		]);

	if CSRFToken = '' then
	begin
		Logger.Log(LOG_LEVEL_WARNING, msgtype_details, 'VK ID: CSRF token is empty, script result: %s', [ScriptResult]);
		Result := TAuthResult.CreateFailure(ERR_VKID_CSRF_FAILED);
		Exit;
	end;

	{Save cookies for future session reuse}
	if Credentials.CookieFilePath <> '' then
	begin
		Persistence := TCookiePersistence.Create(Credentials.CookieFilePath, TWindowsFileSystem.Create);
		try
			Persistence.Save(HTTP.AuthCookie, CSRFToken);
			Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, 'VK ID: Cookies saved to %s', [Credentials.CookieFilePath]);
		finally
			Persistence.Free;
		end;
	end;

	Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, VKID_LOGIN_SUCCESS);
	Result := TAuthResult.CreateCookieSuccess(CSRFToken);
end;

function TVKIDAuthStrategy.GetName: WideString;
begin
	Result := 'VK ID Browser Login';
end;

end.
