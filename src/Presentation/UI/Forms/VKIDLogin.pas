unit VKIDLogin;

{WebView2-based login form for VK ID authentication.
	Opens an embedded browser navigating to the mail.ru login page,
	which redirects through VK ID. When the user completes login and
	is redirected back to cloud.mail.ru, the CSRF token is fetched
	via JavaScript (which has access to all browser cookies including SDC),
	then cookies are extracted and injected into Indy's cookie manager.}

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Classes,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Edge,
	Winapi.WebView2,
	Winapi.ActiveX,
	IdCookieManager,
	IdURI;

const
	{Posted from NavigationCompleted to run cookie/CSRF extraction outside
		the WebView2 callback chain -- ExecuteScript callbacks are not dispatched
		while a NavigationCompleted handler is still on the stack.}
	WM_FINALIZE_LOGIN = WM_USER + 1;

type
	TVKIDLoginForm = class(TForm)
		EdgeBrowser: TEdgeBrowser;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure EdgeBrowserNavigationCompleted(Sender: TCustomEdgeBrowser;
			IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
	private
		FLoginSucceeded: Boolean;
		FLoginPageShown: Boolean;
		FNavigatingToCloud: Boolean;
		FScriptExecuted: Boolean;
		FCookieManager: TIdCookieManager;
		FCSRFToken: WideString;
		FScriptRawResult: WideString;
		procedure WMFinalizeLogin(var Message: TMessage); message WM_FINALIZE_LOGIN;
		function FetchCSRFToken: WideString;
		function ExtractCookies: Boolean;
	public
		{Shows the VK ID login form modally.
			Injects cookies into the provided manager and fetches a CSRF token via JS.
			@param ParentWindowHandle TC parent window for centering
			@param CookieManager Indy cookie manager to receive auth cookies
			@param CSRFToken Receives the CSRF token obtained from the browser
			@param ScriptResult Raw ExecuteScript result for diagnostics
			@return True if cookies were successfully extracted}
		class function Execute(ParentWindowHandle: HWND; CookieManager: TIdCookieManager;
			var CSRFToken: WideString; var ScriptResult: WideString): Boolean;
		{Checks whether WebView2 runtime is available on this system.}
		class function IsWebView2Available: Boolean;
	end;

const
	{Mail.ru login URL that triggers VK ID redirect chain}
	VKID_LOGIN_URL = 'https://account.mail.ru/login?service=cloud';

	{JavaScript that fetches the CSRF token via synchronous XHR.
		Runs in the cloud.mail.ru page context where all cookies are available.
		CSRF response format: body is an object with a token field, e.g.
		  "body": "token": "AG_...", "status": 200 }
	JS_FETCH_CSRF =
		'(function() {' +
		'  try {' +
		'    var xhr = new XMLHttpRequest();' +
		'    xhr.open("GET", "/api/v2/tokens/csrf", false);' +
		'    xhr.send();' +
		'    var data = JSON.parse(xhr.responseText);' +
		'    if (data && data.body && data.body.token) return data.body.token;' +
		'    return "";' +
		'  } catch(e) { return ""; }' +
		'})()';

implementation

uses
	LanguageStrings;

{$R *.dfm}

{Helper classes for async WebView2 callbacks}
type
	TGetCookiesCallback = reference to procedure(errorCode: HResult; const cookieList: ICoreWebView2CookieList);

	TGetCookiesCompletedHandler = class(TInterfacedObject, ICoreWebView2GetCookiesCompletedHandler)
	private
		FCallback: TGetCookiesCallback;
	public
		constructor Create(ACallback: TGetCookiesCallback);
		function Invoke(errorCode: HResult; const cookieList: ICoreWebView2CookieList): HResult; stdcall;
	end;

	TExecuteScriptCallback = reference to procedure(errorCode: HResult; resultJson: PWideChar);

	TExecuteScriptCompletedHandler = class(TInterfacedObject, ICoreWebView2ExecuteScriptCompletedHandler)
	private
		FCallback: TExecuteScriptCallback;
	public
		constructor Create(ACallback: TExecuteScriptCallback);
		function Invoke(errorCode: HResult; resultObjectAsJson: PWideChar): HResult; stdcall;
	end;

constructor TGetCookiesCompletedHandler.Create(ACallback: TGetCookiesCallback);
begin
	inherited Create;
	FCallback := ACallback;
end;

function TGetCookiesCompletedHandler.Invoke(errorCode: HResult; const cookieList: ICoreWebView2CookieList): HResult; stdcall;
begin
	FCallback(errorCode, cookieList);
	Result := S_OK;
end;

constructor TExecuteScriptCompletedHandler.Create(ACallback: TExecuteScriptCallback);
begin
	inherited Create;
	FCallback := ACallback;
end;

function TExecuteScriptCompletedHandler.Invoke(errorCode: HResult; resultObjectAsJson: PWideChar): HResult; stdcall;
begin
	FCallback(errorCode, resultObjectAsJson);
	Result := S_OK;
end;

{TVKIDLoginForm}

class function TVKIDLoginForm.IsWebView2Available: Boolean;
type
	TGetVersionFunc = function(browserExecutableFolder: LPCWSTR; versionInfo: PPWideChar): HResult; stdcall;
var
	Lib: HMODULE;
	GetVersion: TGetVersionFunc;
	VersionInfo: PWideChar;
begin
	{Official Microsoft detection: call GetAvailableCoreWebView2BrowserVersionString
		from WebView2Loader.dll. Registry checks miss Windows 11 built-in WebView2.}
	Result := False;
	Lib := LoadLibrary('WebView2Loader.dll');
	if Lib = 0 then
		Exit;
	try
		@GetVersion := GetProcAddress(Lib, 'GetAvailableCoreWebView2BrowserVersionString');
		if @GetVersion = nil then
			Exit;
		VersionInfo := nil;
		if Succeeded(GetVersion(nil, @VersionInfo)) and (VersionInfo <> nil) and (VersionInfo^ <> #0) then
			Result := True;
		if VersionInfo <> nil then
			CoTaskMemFree(VersionInfo);
	finally
		FreeLibrary(Lib);
	end;
end;

class function TVKIDLoginForm.Execute(ParentWindowHandle: HWND; CookieManager: TIdCookieManager;
	var CSRFToken: WideString; var ScriptResult: WideString): Boolean;
var
	Form: TVKIDLoginForm;
begin
	CSRFToken := '';
	ScriptResult := '';
	if not IsWebView2Available then
	begin
		MessageBox(ParentWindowHandle, PWideChar(ERR_WEBVIEW2_NOT_AVAILABLE), PWideChar(DFM_VKID_LOGIN_TITLE), MB_OK or MB_ICONERROR);
		Exit(False);
	end;

	Form := TVKIDLoginForm.Create(nil);
	try
		Form.ParentWindow := ParentWindowHandle;
		Form.FCookieManager := CookieManager;
		Form.Caption := DFM_VKID_LOGIN_TITLE;
		Form.ShowModal;
		CSRFToken := Form.FCSRFToken;
		ScriptResult := Form.FScriptRawResult;
		Result := Form.FLoginSucceeded;
	finally
		Form.Free;
	end;
end;

procedure TVKIDLoginForm.FormCreate(Sender: TObject);
var
	UserDataFolder: WideString;
begin
	FLoginSucceeded := False;
	FLoginPageShown := False;
	FNavigatingToCloud := False;
	FScriptExecuted := False;
	FCSRFToken := '';
	FScriptRawResult := '';
	{Use plugin config directory for WebView2 user data to avoid permission issues}
	UserDataFolder := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')) + 'CloudMailRu\WebView2';
	EdgeBrowser.UserDataFolder := UserDataFolder;
	EdgeBrowser.Navigate(VKID_LOGIN_URL);
end;

procedure TVKIDLoginForm.FormDestroy(Sender: TObject);
begin
	{EdgeBrowser cleanup is handled by VCL component ownership}
end;

procedure TVKIDLoginForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	{Fallback: if auto-detection didn't trigger, try extracting cookies now.
		The user may have logged in successfully but the redirect URL didn't match
		our detection logic. The CSRF token call in the strategy will validate
		whether cookies are actually valid.}
	if not FLoginSucceeded then
		FLoginSucceeded := ExtractCookies;
end;

procedure TVKIDLoginForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_ESCAPE then
		Close;
end;

procedure TVKIDLoginForm.EdgeBrowserNavigationCompleted(Sender: TCustomEdgeBrowser;
	IsSuccess: Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS);
var
	CurrentURL: String;
begin
	CurrentURL := LowerCase(EdgeBrowser.LocationURL);

	{Track all navigation events for diagnostics}
	FScriptRawResult := FScriptRawResult + ' | nav=' + CurrentURL
		+ ' ok=' + BoolToStr(IsSuccess, True)
		+ ' p1=' + BoolToStr(FLoginPageShown, True)
		+ ' p2=' + BoolToStr(FNavigatingToCloud, True);

	if not IsSuccess then
		Exit;

	{Phase 3: cloud.mail.ru loaded -- defer cookie/CSRF extraction via PostMessage.
		We cannot call ExecuteScript with a blocking wait here because WebView2
		does not dispatch script completion callbacks while NavigationCompleted
		is still on the call stack. PostMessage ensures we run outside the callback.}
	if FNavigatingToCloud then
	begin
		if not FScriptExecuted and (Pos('cloud.mail.ru', CurrentURL) > 0) then
		begin
			FScriptExecuted := True;
			PostMessage(Handle, WM_FINALIZE_LOGIN, 0, 0);
		end;
		Exit;
	end;

	{Phase 1: wait until the login page has actually appeared before detecting success.
		Without this, intermediate redirects (sdc.mail.ru, r.mail.ru, etc.) during
		the initial page load would trigger premature detection.
		account.mail.ru redirects via 302 to id.vk.ru, so we accept both.}
	if not FLoginPageShown then
	begin
		if (Pos('account.mail.ru', CurrentURL) > 0) or (Pos('id.vk.ru', CurrentURL) > 0) then
			FLoginPageShown := True;
		Exit;
	end;

	{Phase 2: detect successful login -- navigated away from auth pages to any mail.ru service.
		After login, user lands on e.mail.ru (inbox). Now navigate to cloud.mail.ru
		so that SDC cookies are set (they require JavaScript execution in the browser).}
	if (Pos('.mail.ru', CurrentURL) > 0)
		and (Pos('account.mail.ru', CurrentURL) = 0)
		and (Pos('auth.mail.ru', CurrentURL) = 0) then
	begin
		FNavigatingToCloud := True;
		EdgeBrowser.Navigate('https://cloud.mail.ru/');
	end;
end;

procedure TVKIDLoginForm.WMFinalizeLogin(var Message: TMessage);
begin
	ExtractCookies;
	FCSRFToken := FetchCSRFToken;
	FLoginSucceeded := True;
	ModalResult := mrOk;
end;

function TVKIDLoginForm.FetchCSRFToken: WideString;
var
	WebView: ICoreWebView2;
	Handler: ICoreWebView2ExecuteScriptCompletedHandler;
	Event: THandle;
	Msg: TMsg;
	WaitResult: DWORD;
	Deadline: Int64;
	Remaining: Int64;
	ScriptJson: WideString;
	HR: HResult;
begin
	Result := '';

	if EdgeBrowser.DefaultInterface = nil then
		Exit;

	WebView := EdgeBrowser.DefaultInterface;

	Event := CreateEvent(nil, True, False, nil);
	ScriptJson := '';

	Handler := TExecuteScriptCompletedHandler.Create(
		procedure(errorCode: HResult; resultJson: PWideChar)
		begin
			FScriptRawResult := FScriptRawResult + ' | HRESULT=' + IntToStr(errorCode);
			if resultJson <> nil then
			begin
				ScriptJson := resultJson;
				FScriptRawResult := FScriptRawResult + ' JSON=' + ScriptJson;
			end;
			SetEvent(Event);
		end
	);

	HR := WebView.ExecuteScript(PWideChar(WideString(JS_FETCH_CSRF)), Handler);
	if not Succeeded(HR) then
	begin
		FScriptRawResult := FScriptRawResult + ' | ExecuteScript failed HR=' + IntToStr(HR);
		CloseHandle(Event);
		Exit;
	end;

	{Pump messages while waiting -- same pattern as ExtractCookies.
		Use Int64 arithmetic to avoid unsigned overflow with $Q+ in debug builds.}
	Deadline := Int64(GetTickCount) + 10000;
	repeat
		Remaining := Deadline - Int64(GetTickCount);
		if Remaining <= 0 then
			Break;
		WaitResult := MsgWaitForMultipleObjects(1, Event, False, Cardinal(Remaining), QS_ALLINPUT);
		if WaitResult = WAIT_OBJECT_0 then
			Break;
		if WaitResult = WAIT_OBJECT_0 + 1 then
		begin
			while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
			begin
				TranslateMessage(Msg);
				DispatchMessage(Msg);
			end;
		end
		else
			Break;
	until False;
	CloseHandle(Event);

	{ExecuteScript returns JSON: a string "abc" becomes '"abc"'.
		Strip surrounding quotes to get the raw CSRF token value.}
	if (Length(ScriptJson) > 2) and (ScriptJson[1] = '"') then
		Result := Copy(ScriptJson, 2, Length(ScriptJson) - 2);
end;

function TVKIDLoginForm.ExtractCookies: Boolean;
var
	WebView: ICoreWebView2;
	WebView2: ICoreWebView2_2;
	CookieManager: ICoreWebView2CookieManager;
	Handler: ICoreWebView2GetCookiesCompletedHandler;
	CookieList: ICoreWebView2CookieList;
	Cookie: ICoreWebView2Cookie;
	CookieCount: Cardinal;
	I: Cardinal;
	CookieName, CookieValue, CookieDomain, CookiePath: PWideChar;
	ServerCookieURL: TIdURI;
	Event: THandle;
	Msg: TMsg;
	WaitResult: DWORD;
	Deadline: Int64;
	Remaining: Int64;
	CountBefore: Integer;
begin
	Result := False;

	if EdgeBrowser.DefaultInterface = nil then
		Exit;

	WebView := EdgeBrowser.DefaultInterface;
	if not Succeeded(WebView.QueryInterface(ICoreWebView2_2, WebView2)) then
		Exit;

	CookieManager := nil;
	WebView2.Get_CookieManager(CookieManager);
	if CookieManager = nil then
		Exit;

	{Use a manual-reset event to synchronize the async cookie retrieval}
	Event := CreateEvent(nil, True, False, nil);
	CookieList := nil;

	Handler := TGetCookiesCompletedHandler.Create(
		procedure(errorCode: HResult; const cookieListResult: ICoreWebView2CookieList)
		begin
			if Succeeded(errorCode) then
				CookieList := cookieListResult;
			SetEvent(Event);
		end
	);

	CookieManager.GetCookies('https://cloud.mail.ru', Handler);

	{Pump messages while waiting: WebView2 callbacks need the message loop running.
		Plain WaitForSingleObject deadlocks because it blocks the thread that must
		dispatch the callback.
		Use Int64 arithmetic to avoid unsigned overflow with $Q+ in debug builds.}
	Deadline := Int64(GetTickCount) + 10000;
	repeat
		Remaining := Deadline - Int64(GetTickCount);
		if Remaining <= 0 then
			Break;
		WaitResult := MsgWaitForMultipleObjects(1, Event, False, Cardinal(Remaining), QS_ALLINPUT);
		if WaitResult = WAIT_OBJECT_0 then
			Break;
		if WaitResult = WAIT_OBJECT_0 + 1 then
		begin
			while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
			begin
				TranslateMessage(Msg);
				DispatchMessage(Msg);
			end;
		end
		else
			Break;
	until False;
	CloseHandle(Event);

	if CookieList = nil then
		Exit;

	CountBefore := FCookieManager.CookieCollection.Count;

	CookieList.Get_Count(CookieCount);
	for I := 0 to CookieCount - 1 do
	begin
		CookieList.GetValueAtIndex(I, Cookie);
		if Cookie = nil then
			Continue;

		Cookie.Get_Name(CookieName);
		Cookie.Get_Value(CookieValue);
		Cookie.Get_Domain(CookieDomain);
		Cookie.Get_Path(CookiePath);

		{Use AddServerCookie to register cookies through Indy's standard pipeline.
			CookieCollection.Add bypasses domain matching setup, so cookies
			added that way may not be sent with subsequent requests.}
		ServerCookieURL := TIdURI.Create('https://cloud.mail.ru/');
		try
			FCookieManager.AddServerCookie(
				WideString(CookieName) + '=' + WideString(CookieValue)
					+ '; Domain=' + WideString(CookieDomain)
					+ '; Path=' + WideString(CookiePath)
					+ '; Secure',
				ServerCookieURL);
		finally
			ServerCookieURL.Free;
		end;

		CoTaskMemFree(CookieName);
		CoTaskMemFree(CookieValue);
		CoTaskMemFree(CookieDomain);
		CoTaskMemFree(CookiePath);
	end;

	Result := FCookieManager.CookieCollection.Count > CountBefore;
end;

end.
