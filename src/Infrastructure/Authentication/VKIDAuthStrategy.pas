unit VKIDAuthStrategy;

{VK ID authentication strategy using embedded WebView2 browser.
	Shows a login form where the user authenticates via VK ID on mail.ru,
	then extracts cookies and obtains a CSRF token for API access.
	Unlike OAuth, this mode uses cookies for session auth and CSRF
	token for API calls, downloads, and uploads.}

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
	SysUtils, Math,
	Winapi.Windows,
	CloudConstants,
	WFXTypes,
	LanguageStrings,
	VKIDLogin;

{TVKIDAuthStrategy}

function TVKIDAuthStrategy.Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
var
	CSRFToken: WideString;
	ScriptResult: WideString;
	CookieCount: Integer;
	I: Integer;
begin
	Result := TAuthResult.CreateFailure(ERR_VKID_LOGIN_CANCELLED);

	Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, REQUESTING_VKID_LOGIN, [Credentials.Email]);

	if HTTP.AuthCookie = nil then
		Logger.Log(LOG_LEVEL_WARNING, msgtype_details, 'VK ID: AuthCookie is nil!', []);

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

	Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, VKID_LOGIN_SUCCESS);
	Result := TAuthResult.CreateCookieSuccess(CSRFToken);
end;

function TVKIDAuthStrategy.GetName: WideString;
begin
	Result := 'VK ID Browser Login';
end;

end.
