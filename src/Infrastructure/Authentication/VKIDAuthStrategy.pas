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
	Winapi.Windows,
	IdCookieManager,
	AuthStrategy,
	CloudHTTP,
	FileSystem,
	Logger;

type
	{Abstraction for the VK ID login form, enabling testability
		without WebView2 runtime or GUI.}
	IVKIDLoginProvider = interface
		['{F3A7E1D4-8B2C-4F6E-A9D1-5C3B7E0F2A84}']
		function Execute(ParentWindowHandle: HWND; CookieManager: TIdCookieManager;
			var CSRFToken: WideString; var ScriptResult: WideString): Boolean;
	end;

	{Default login provider that delegates to TVKIDLoginForm.Execute.}
	TVKIDLoginProvider = class(TInterfacedObject, IVKIDLoginProvider)
	public
		function Execute(ParentWindowHandle: HWND; CookieManager: TIdCookieManager;
			var CSRFToken: WideString; var ScriptResult: WideString): Boolean;
	end;

	TVKIDAuthStrategy = class(TInterfacedObject, IAuthStrategy)
	private
		FLoginProvider: IVKIDLoginProvider;
		FFileSystem: IFileSystem;
	public
		constructor Create(LoginProvider: IVKIDLoginProvider; FileSystem: IFileSystem);
		function Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
		function GetName: WideString;
	end;

implementation

uses
	SysUtils, Classes, Math,
	CloudConstants,
	WFXTypes,
	LanguageStrings,
	CookiePersistence,
	JSONHelper,
	VKIDLogin;

{TVKIDLoginProvider}

function TVKIDLoginProvider.Execute(ParentWindowHandle: HWND; CookieManager: TIdCookieManager;
	var CSRFToken: WideString; var ScriptResult: WideString): Boolean;
begin
	Result := TVKIDLoginForm.Execute(ParentWindowHandle, CookieManager, CSRFToken, ScriptResult);
end;

{TVKIDAuthStrategy}

constructor TVKIDAuthStrategy.Create(LoginProvider: IVKIDLoginProvider; FileSystem: IFileSystem);
begin
	inherited Create;
	FLoginProvider := LoginProvider;
	FFileSystem := FileSystem;
end;

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
		Logger.Log(LOG_LEVEL_WARNING, msgtype_details, VKID_AUTH_COOKIE_NIL, []);

	{Try to restore session from saved cookies}
	if Credentials.CookieFilePath <> '' then
	begin
		Persistence := TCookiePersistence.Create(Credentials.CookieFilePath, FFileSystem);
		try
			if Persistence.Load(HTTP.AuthCookie, CSRFToken) then
			begin
				// Validate session by refreshing CSRF token via API call
				ShowProgress := False;
				if HTTP.GetPage(Credentials.CsrfUrl, JSON, ShowProgress) and getBodyToken(JSON, FreshCSRFToken) then
				begin
					Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, VKID_SESSION_RESTORED, []);
					// Save updated cookies (server may have rotated them)
					Persistence.Save(HTTP.AuthCookie, FreshCSRFToken);
					Result := TAuthResult.CreateCookieSuccess(FreshCSRFToken);
					Exit;
				end;
				// Session expired - clear stale cookies before showing WebView2
				HTTP.AuthCookie.CookieCollection.Clear;
				Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, VKID_COOKIES_EXPIRED, []);
			end;
		finally
			Persistence.Free;
		end;
	end;

	{Show WebView2 login form: user logs in, browser navigates to cloud.mail.ru,
		CSRF token is fetched via JavaScript (which has access to all browser cookies
		including SDC), and cookies are injected into Indy's cookie manager.}
	if not FLoginProvider.Execute(0, HTTP.AuthCookie, CSRFToken, ScriptResult) then
	begin
		Logger.Log(LOG_LEVEL_WARNING, msgtype_details, VKID_LOGIN_FORM_FAILED, [ScriptResult]);
		Exit;
	end;

	CookieCount := HTTP.AuthCookie.CookieCollection.Count;
	Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, VKID_LOGIN_DETAILS, [CookieCount, Length(CSRFToken), ScriptResult]);
	for I := 0 to Min(CookieCount - 1, 9) do
		Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, VKID_COOKIE_DETAIL, [
			I,
			HTTP.AuthCookie.CookieCollection.Cookies[I].CookieName,
			HTTP.AuthCookie.CookieCollection.Cookies[I].Domain,
			HTTP.AuthCookie.CookieCollection.Cookies[I].Path
		]);

	if CSRFToken = '' then
	begin
		Logger.Log(LOG_LEVEL_WARNING, msgtype_details, VKID_CSRF_EMPTY, [ScriptResult]);
		Result := TAuthResult.CreateFailure(ERR_VKID_CSRF_FAILED);
		Exit;
	end;

	{Save cookies for future session reuse}
	if Credentials.CookieFilePath <> '' then
	begin
		Persistence := TCookiePersistence.Create(Credentials.CookieFilePath, FFileSystem);
		try
			Persistence.Save(HTTP.AuthCookie, CSRFToken);
			Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, VKID_COOKIES_SAVED, [Credentials.CookieFilePath]);
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
