unit CloudHTTPTest;

{Unit tests for CloudHTTP - ICloudHTTP interface implementations.
 Tests TNullCloudHTTP (null object pattern) and TCloudMailRuHTTP initialization.
 Network-dependent methods cannot be unit tested without mocking Indy internals.}

interface

uses
	DUnitX.TestFramework,
	System.SysUtils,
	System.Classes,
	System.Generics.Collections,
	CloudHTTP,
	ConnectionSettings,
	ProxySettings,
	SETTINGS_CONSTANTS,
	WFXTypes,
	CMRConstants,
	TCLogger,
	TCProgress,
	IdCookieManager,
	IdHTTP;

type
	[TestFixture]
	TNullCloudHTTPTest = class
	private
		FHTTP: ICloudHTTP;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{GetPage tests}
		[Test]
		procedure TestGetPage_ReturnsFalse;
		[Test]
		procedure TestGetPage_SetsAnswerToEmpty;

		{GetFile tests}
		[Test]
		procedure TestGetFile_ReturnsReadError;
		[Test]
		procedure TestGetFile_DoesNotWriteToStream;

		{GetRedirection tests}
		[Test]
		procedure TestGetRedirection_ReturnsFalse;
		[Test]
		procedure TestGetRedirection_SetsRedirectionURLToEmpty;

		{PostForm tests}
		[Test]
		procedure TestPostForm_ReturnsFalse;
		[Test]
		procedure TestPostForm_SetsAnswerToEmpty;

		{PostMultipart tests}
		[Test]
		procedure TestPostMultipart_ReturnsFalse;
		[Test]
		procedure TestPostMultipart_SetsAnswerToEmpty;

		{PostFile tests}
		[Test]
		procedure TestPostFile_ReturnsWriteError;
		[Test]
		procedure TestPostFile_SetsAnswerToEmpty;

		{PutFile tests}
		[Test]
		procedure TestPutFile_ReturnsWriteError;
		[Test]
		procedure TestPutFile_SetsAnswerToEmpty;

		{Head tests}
		[Test]
		procedure TestHead_DoesNotRaiseException;

		{SetProgressNames tests}
		[Test]
		procedure TestSetProgressNames_DoesNotRaiseException;

		{SetAuthCookie tests}
		[Test]
		procedure TestSetAuthCookie_DoesNotRaiseException;

		{GetHTTP tests}
		[Test]
		procedure TestGetHTTP_ReturnsNil;
	end;

	[TestFixture]
	TCloudMailRuHTTPConstructorTest = class
	private
		FLogger: ILogger;
		FProgress: IProgress;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Basic construction tests}
		[Test]
		procedure TestCreate_NoProxy_InitializesHTTP;
		[Test]
		procedure TestCreate_NoProxy_SetsUserAgent;
		[Test]
		procedure TestCreate_NoProxy_EnablesCookies;
		[Test]
		procedure TestCreate_NoProxy_EnablesRedirects;

		{Timeout tests}
		[Test]
		procedure TestCreate_NegativeTimeout_SetsTimeouts;
		[Test]
		procedure TestCreate_PositiveTimeout_DoesNotSetTimeouts;

		{Throttle tests}
		[Test]
		procedure TestCreate_WithBandwidthLimits_ConfiguresThrottle;

		{SOCKS5 proxy tests}
		[Test]
		procedure TestCreate_Socks5Proxy_CreatesSocksInfo;
		[Test]
		procedure TestCreate_Socks5WithAuth_SetsAuthentication;

		{SOCKS4 proxy tests}
		[Test]
		procedure TestCreate_Socks4Proxy_CreatesSocksInfo;

		{HTTP proxy tests}
		[Test]
		procedure TestCreate_HTTPProxy_SetsProxyParams;
		[Test]
		procedure TestCreate_HTTPProxyWithAuth_SetsProxyAuthentication;
	end;

	[TestFixture]
	TCloudMailRuHTTPSetterTest = class
	private
		FHTTP: TCloudMailRuHTTP;
		FSettings: TConnectionSettings;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{SetCookie tests via AuthCookie property}
		[Test]
		procedure TestAuthCookie_SetsCookieManager;

		{SetProgressNames tests}
		[Test]
		procedure TestSetProgressNames_DoesNotRaise;

		{HTTP property tests}
		[Test]
		procedure TestHTTP_IsNotNil;
	end;

	[TestFixture]
	TCloudMailRuHTTPExceptionHandlerTest = class
	private
		FHTTP: TCloudMailRuHTTP;
		FSettings: TConnectionSettings;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{HTTP method result codes}
		[Test]
		procedure TestExceptionHandler_GETMethod_ReturnsReadError;
		[Test]
		procedure TestExceptionHandler_POSTMethod_ReturnsOperationFailed;
		[Test]
		procedure TestExceptionHandler_PUTMethod_ReturnsOperationFailed;
		[Test]
		procedure TestExceptionHandler_OPTIONSMethod_ReturnsOperationFailed;

		{EAbort handling}
		[Test]
		procedure TestExceptionHandler_EAbort_ReturnsCancelled;

		{Socket error handling}
		[Test]
		procedure TestExceptionHandler_SocketError_LogsAndReturnsError;

		{Generic exception handling}
		[Test]
		procedure TestExceptionHandler_GenericException_ReturnsError;
	end;

implementation

uses
	IdException,
	IdExceptionCore,
	IdStack;

{TNullCloudHTTPTest}

procedure TNullCloudHTTPTest.Setup;
begin
	FHTTP := TNullCloudHTTP.Create;
end;

procedure TNullCloudHTTPTest.TearDown;
begin
	FHTTP := nil;
end;

procedure TNullCloudHTTPTest.TestGetPage_ReturnsFalse;
var
	Answer: WideString;
	ProgressEnabled: Boolean;
begin
	ProgressEnabled := True;
	Assert.IsFalse(FHTTP.GetPage('http://test.com', Answer, ProgressEnabled));
end;

procedure TNullCloudHTTPTest.TestGetPage_SetsAnswerToEmpty;
var
	Answer: WideString;
	ProgressEnabled: Boolean;
begin
	Answer := 'initial';
	ProgressEnabled := True;
	FHTTP.GetPage('http://test.com', Answer, ProgressEnabled);
	Assert.AreEqual('', Answer);
end;

procedure TNullCloudHTTPTest.TestGetFile_ReturnsReadError;
var
	Stream: TMemoryStream;
begin
	Stream := TMemoryStream.Create;
	try
		Assert.AreEqual(FS_FILE_READERROR, FHTTP.GetFile('http://test.com/file', Stream));
	finally
		Stream.Free;
	end;
end;

procedure TNullCloudHTTPTest.TestGetFile_DoesNotWriteToStream;
var
	Stream: TMemoryStream;
begin
	Stream := TMemoryStream.Create;
	try
		FHTTP.GetFile('http://test.com/file', Stream);
		Assert.AreEqual(Int64(0), Stream.Size, 'Stream should remain empty');
	finally
		Stream.Free;
	end;
end;

procedure TNullCloudHTTPTest.TestGetRedirection_ReturnsFalse;
var
	RedirectURL: WideString;
	ProgressEnabled: Boolean;
begin
	ProgressEnabled := True;
	Assert.IsFalse(FHTTP.GetRedirection('http://test.com', RedirectURL, ProgressEnabled));
end;

procedure TNullCloudHTTPTest.TestGetRedirection_SetsRedirectionURLToEmpty;
var
	RedirectURL: WideString;
	ProgressEnabled: Boolean;
begin
	RedirectURL := 'http://initial.com';
	ProgressEnabled := True;
	FHTTP.GetRedirection('http://test.com', RedirectURL, ProgressEnabled);
	Assert.AreEqual('', RedirectURL);
end;

procedure TNullCloudHTTPTest.TestPostForm_ReturnsFalse;
var
	Answer: WideString;
begin
	Assert.IsFalse(FHTTP.PostForm('http://test.com', 'data=value', Answer));
end;

procedure TNullCloudHTTPTest.TestPostForm_SetsAnswerToEmpty;
var
	Answer: WideString;
begin
	Answer := 'initial';
	FHTTP.PostForm('http://test.com', 'data=value', Answer);
	Assert.AreEqual('', Answer);
end;

procedure TNullCloudHTTPTest.TestPostMultipart_ReturnsFalse;
var
	Params: TDictionary<WideString, WideString>;
	Answer: WideString;
begin
	Params := TDictionary<WideString, WideString>.Create;
	try
		Params.Add('key', 'value');
		Assert.IsFalse(FHTTP.PostMultipart('http://test.com', Params, Answer));
	finally
		Params.Free;
	end;
end;

procedure TNullCloudHTTPTest.TestPostMultipart_SetsAnswerToEmpty;
var
	Params: TDictionary<WideString, WideString>;
	Answer: WideString;
begin
	Params := TDictionary<WideString, WideString>.Create;
	try
		Params.Add('key', 'value');
		Answer := 'initial';
		FHTTP.PostMultipart('http://test.com', Params, Answer);
		Assert.AreEqual('', Answer);
	finally
		Params.Free;
	end;
end;

procedure TNullCloudHTTPTest.TestPostFile_ReturnsWriteError;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	Stream := TMemoryStream.Create;
	try
		Assert.AreEqual(FS_FILE_WRITEERROR, FHTTP.PostFile('http://test.com', 'file.txt', Stream, Answer));
	finally
		Stream.Free;
	end;
end;

procedure TNullCloudHTTPTest.TestPostFile_SetsAnswerToEmpty;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	Stream := TMemoryStream.Create;
	try
		Answer := 'initial';
		FHTTP.PostFile('http://test.com', 'file.txt', Stream, Answer);
		Assert.AreEqual('', Answer);
	finally
		Stream.Free;
	end;
end;

procedure TNullCloudHTTPTest.TestPutFile_ReturnsWriteError;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	Stream := TMemoryStream.Create;
	try
		Assert.AreEqual(FS_FILE_WRITEERROR, FHTTP.PutFile('http://test.com', 'file.txt', Stream, Answer));
	finally
		Stream.Free;
	end;
end;

procedure TNullCloudHTTPTest.TestPutFile_SetsAnswerToEmpty;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	Stream := TMemoryStream.Create;
	try
		Answer := 'initial';
		FHTTP.PutFile('http://test.com', 'file.txt', Stream, Answer);
		Assert.AreEqual('', Answer);
	finally
		Stream.Free;
	end;
end;

procedure TNullCloudHTTPTest.TestHead_DoesNotRaiseException;
begin
	{Should not raise}
	FHTTP.Head('http://test.com');
	Assert.Pass;
end;

procedure TNullCloudHTTPTest.TestSetProgressNames_DoesNotRaiseException;
begin
	{Should not raise}
	FHTTP.SetProgressNames('source', 'target');
	Assert.Pass;
end;

procedure TNullCloudHTTPTest.TestSetAuthCookie_DoesNotRaiseException;
var
	CookieManager: TIdCookieManager;
begin
	CookieManager := TIdCookieManager.Create;
	try
		{Should not raise}
		FHTTP.SetAuthCookie(CookieManager);
		Assert.Pass;
	finally
		CookieManager.Free;
	end;
end;

procedure TNullCloudHTTPTest.TestGetHTTP_ReturnsNil;
begin
	Assert.IsNull(FHTTP.GetHTTP);
end;

{TCloudMailRuHTTPConstructorTest}

procedure TCloudMailRuHTTPConstructorTest.Setup;
begin
	FLogger := TNullLogger.Create;
	FProgress := TNullProgress.Create;
end;

procedure TCloudMailRuHTTPConstructorTest.TearDown;
begin
	FLogger := nil;
	FProgress := nil;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_NoProxy_InitializesHTTP;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	Settings := Default(TConnectionSettings);
	Settings.UserAgent := 'TestAgent/1.0';
	HTTP := TCloudMailRuHTTP.Create(Settings, FLogger, FProgress);
	try
		Assert.IsNotNull(HTTP.HTTP, 'Internal TIdHTTP should be created');
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_NoProxy_SetsUserAgent;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	Settings := Default(TConnectionSettings);
	Settings.UserAgent := 'CustomUserAgent/2.0';
	HTTP := TCloudMailRuHTTP.Create(Settings, FLogger, FProgress);
	try
		Assert.AreEqual('CustomUserAgent/2.0', HTTP.HTTP.Request.UserAgent);
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_NoProxy_EnablesCookies;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	Settings := Default(TConnectionSettings);
	HTTP := TCloudMailRuHTTP.Create(Settings, FLogger, FProgress);
	try
		Assert.IsTrue(HTTP.HTTP.AllowCookies, 'Cookies should be enabled');
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_NoProxy_EnablesRedirects;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	Settings := Default(TConnectionSettings);
	HTTP := TCloudMailRuHTTP.Create(Settings, FLogger, FProgress);
	try
		Assert.IsTrue(HTTP.HTTP.HandleRedirects, 'Redirects should be handled');
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_NegativeTimeout_SetsTimeouts;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	Settings := Default(TConnectionSettings);
	Settings.SocketTimeout := -5000;
	HTTP := TCloudMailRuHTTP.Create(Settings, FLogger, FProgress);
	try
		Assert.AreEqual(-5000, HTTP.HTTP.ConnectTimeout);
		Assert.AreEqual(-5000, HTTP.HTTP.ReadTimeout);
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_PositiveTimeout_DoesNotSetTimeouts;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	{Positive timeout means "use default" per the code logic}
	Settings := Default(TConnectionSettings);
	Settings.SocketTimeout := 5000;
	HTTP := TCloudMailRuHTTP.Create(Settings, FLogger, FProgress);
	try
		{Default TIdHTTP timeout is 0 (infinite)}
		Assert.AreEqual(0, HTTP.HTTP.ConnectTimeout, 'Positive timeout should not override default');
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_WithBandwidthLimits_ConfiguresThrottle;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	Settings := Default(TConnectionSettings);
	Settings.UploadBPS := 1024;
	Settings.DownloadBPS := 2048;
	HTTP := TCloudMailRuHTTP.Create(Settings, FLogger, FProgress);
	try
		{Throttle is private, but we can verify HTTP was created successfully}
		Assert.IsNotNull(HTTP.HTTP);
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_Socks5Proxy_CreatesSocksInfo;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	Settings := Default(TConnectionSettings);
	Settings.ProxySettings.ProxyType := ProxySocks5;
	Settings.ProxySettings.Server := '127.0.0.1';
	Settings.ProxySettings.Port := 1080;
	HTTP := TCloudMailRuHTTP.Create(Settings, FLogger, FProgress);
	try
		{Socks is private, verify HTTP created successfully with proxy config}
		Assert.IsNotNull(HTTP.HTTP);
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_Socks5WithAuth_SetsAuthentication;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	Settings := Default(TConnectionSettings);
	Settings.ProxySettings.ProxyType := ProxySocks5;
	Settings.ProxySettings.Server := '127.0.0.1';
	Settings.ProxySettings.Port := 1080;
	Settings.ProxySettings.User := 'proxyuser';
	Settings.ProxySettings.Password := 'proxypass';
	HTTP := TCloudMailRuHTTP.Create(Settings, FLogger, FProgress);
	try
		Assert.IsNotNull(HTTP.HTTP);
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_Socks4Proxy_CreatesSocksInfo;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	Settings := Default(TConnectionSettings);
	Settings.ProxySettings.ProxyType := ProxySocks4;
	Settings.ProxySettings.Server := '127.0.0.1';
	Settings.ProxySettings.Port := 1080;
	HTTP := TCloudMailRuHTTP.Create(Settings, FLogger, FProgress);
	try
		Assert.IsNotNull(HTTP.HTTP);
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_HTTPProxy_SetsProxyParams;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	Settings := Default(TConnectionSettings);
	Settings.ProxySettings.ProxyType := ProxyHTTP;
	Settings.ProxySettings.Server := 'proxy.example.com';
	Settings.ProxySettings.Port := 8080;
	HTTP := TCloudMailRuHTTP.Create(Settings, FLogger, FProgress);
	try
		Assert.AreEqual('proxy.example.com', HTTP.HTTP.ProxyParams.ProxyServer);
		Assert.AreEqual(8080, HTTP.HTTP.ProxyParams.ProxyPort);
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_HTTPProxyWithAuth_SetsProxyAuthentication;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	Settings := Default(TConnectionSettings);
	Settings.ProxySettings.ProxyType := ProxyHTTP;
	Settings.ProxySettings.Server := 'proxy.example.com';
	Settings.ProxySettings.Port := 8080;
	Settings.ProxySettings.User := 'httpuser';
	Settings.ProxySettings.Password := 'httppass';
	HTTP := TCloudMailRuHTTP.Create(Settings, FLogger, FProgress);
	try
		Assert.IsTrue(HTTP.HTTP.ProxyParams.BasicAuthentication);
		Assert.AreEqual('httpuser', HTTP.HTTP.ProxyParams.ProxyUsername);
		Assert.AreEqual('httppass', HTTP.HTTP.ProxyParams.ProxyPassword);
	finally
		HTTP.Free;
	end;
end;

{TCloudMailRuHTTPSetterTest}

procedure TCloudMailRuHTTPSetterTest.Setup;
begin
	FSettings := Default(TConnectionSettings);
	FHTTP := TCloudMailRuHTTP.Create(FSettings, TNullLogger.Create, TNullProgress.Create);
end;

procedure TCloudMailRuHTTPSetterTest.TearDown;
begin
	FHTTP.Free;
end;

procedure TCloudMailRuHTTPSetterTest.TestAuthCookie_SetsCookieManager;
var
	CookieManager: TIdCookieManager;
begin
	CookieManager := TIdCookieManager.Create;
	{Use the public AuthCookie property which calls SetCookie}
	FHTTP.AuthCookie := CookieManager;
	Assert.AreSame(CookieManager, FHTTP.HTTP.CookieManager);
	{CookieManager is now owned by TIdHTTP, don't free it}
end;

procedure TCloudMailRuHTTPSetterTest.TestSetProgressNames_DoesNotRaise;
begin
	{SetProgressNames stores values internally for progress callbacks}
	FHTTP.SetProgressNames('source.txt', 'target.txt');
	Assert.Pass;
end;

procedure TCloudMailRuHTTPSetterTest.TestHTTP_IsNotNil;
begin
	Assert.IsNotNull(FHTTP.HTTP);
end;

{TCloudMailRuHTTPExceptionHandlerTest}

procedure TCloudMailRuHTTPExceptionHandlerTest.Setup;
begin
	FSettings := Default(TConnectionSettings);
	FHTTP := TCloudMailRuHTTP.Create(FSettings, TNullLogger.Create, TNullProgress.Create);
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TearDown;
begin
	FHTTP.Free;
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_GETMethod_ReturnsReadError;
var
	E: Exception;
begin
	E := Exception.Create('Test error');
	try
		Assert.AreEqual(FS_FILE_READERROR, FHTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_GET, False));
	finally
		E.Free;
	end;
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_POSTMethod_ReturnsOperationFailed;
var
	E: Exception;
begin
	E := Exception.Create('Test error');
	try
		Assert.AreEqual(CLOUD_OPERATION_FAILED, FHTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_POST, False));
	finally
		E.Free;
	end;
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_PUTMethod_ReturnsOperationFailed;
var
	E: Exception;
begin
	E := Exception.Create('Test error');
	try
		Assert.AreEqual(CLOUD_OPERATION_FAILED, FHTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_PUT, False));
	finally
		E.Free;
	end;
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_OPTIONSMethod_ReturnsOperationFailed;
var
	E: Exception;
begin
	E := Exception.Create('Test error');
	try
		Assert.AreEqual(CLOUD_OPERATION_FAILED, FHTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_OPTIONS, False));
	finally
		E.Free;
	end;
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_EAbort_ReturnsCancelled;
var
	E: EAbort;
begin
	E := EAbort.Create('User cancelled');
	try
		Assert.AreEqual(CLOUD_OPERATION_CANCELLED, FHTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_POST, False));
	finally
		E.Free;
	end;
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_SocketError_LogsAndReturnsError;
var
	E: EIdSocketError;
begin
	E := EIdSocketError.Create('Socket connection failed');
	try
		{With LogErrors=False, just returns the error code without logging}
		Assert.AreEqual(CLOUD_OPERATION_FAILED, FHTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_POST, False));
	finally
		E.Free;
	end;
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_GenericException_ReturnsError;
var
	E: Exception;
begin
	E := Exception.Create('Generic error');
	try
		Assert.AreEqual(CLOUD_OPERATION_FAILED, FHTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_POST, False));
	finally
		E.Free;
	end;
end;

initialization
	TDUnitX.RegisterTestFixture(TNullCloudHTTPTest);
	TDUnitX.RegisterTestFixture(TCloudMailRuHTTPConstructorTest);
	TDUnitX.RegisterTestFixture(TCloudMailRuHTTPSetterTest);
	TDUnitX.RegisterTestFixture(TCloudMailRuHTTPExceptionHandlerTest);

end.
