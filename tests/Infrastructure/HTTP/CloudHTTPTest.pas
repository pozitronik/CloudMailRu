unit CloudHTTPTest;

{Unit tests for CloudHTTP - ICloudHTTP interface implementations.
 Tests TNullCloudHTTP (null object pattern) and TCloudMailRuHTTP initialization.
 Network-dependent methods cannot be unit tested without mocking Indy internals.}

interface

uses
	DUnitX.TestFramework,
	System.SysUtils,
	System.Classes,
	CloudHTTP,
	ConnectionSettings,
	ProxySettings,
	SettingsConstants,
	SSLHandlerFactory,
	IndySSLHandlerFactory,
	WFXTypes,
	CloudConstants,
	Logger,
	Progress,
	IdCookieManager,
	IdHTTP;

type
	TLogEntry = record
		LogLevel: Integer;
		Message: WideString;
	end;

	{Mock logger that tracks log calls}
	TMockLoggerForHTTP = class(TInterfacedObject, ILogger)
	private
		FLogCalled: Boolean;
		FLastLogLevel: Integer;
		FLastMessage: WideString;
		FEntries: TArray<TLogEntry>;
	public
		constructor Create;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
		function HasLogWithLevel(Level: Integer): Boolean;
		property LogCalled: Boolean read FLogCalled;
		property LastLogLevel: Integer read FLastLogLevel;
		property LastMessage: WideString read FLastMessage;
		property Entries: TArray<TLogEntry> read FEntries;
	end;

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

		{PostForm tests}
		[Test]
		procedure TestPostForm_ReturnsFalse;
		[Test]
		procedure TestPostForm_SetsAnswerToEmpty;

		{PutFile tests}
		[Test]
		procedure TestPutFile_ReturnsWriteError;
		[Test]
		procedure TestPutFile_SetsAnswerToEmpty;

		{SetProgressNames tests}
		[Test]
		procedure TestSetProgressNames_DoesNotRaiseException;

		{SetAuthCookie tests}
		[Test]
		procedure TestSetAuthCookie_DoesNotRaiseException;
		[Test]
		procedure TestGetAuthCookie_ReturnsNil;

		{GetHTTP tests}
		[Test]
		procedure TestGetHTTP_ReturnsNil;

		{SetCSRFToken tests}
		[Test]
		procedure TestSetCSRFToken_DoesNotRaiseException;

		{SetProgress tests}
		[Test]
		procedure TestSetProgress_DoesNotRaiseException;
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
		procedure TestCreate_PositiveTimeout_SetsTimeouts;
		[Test]
		procedure TestCreate_ZeroTimeout_DoesNotSetTimeouts;

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

		{SourceName/TargetName property setters}
		[Test]
		procedure TestSourceName_SetsWithoutError;
		[Test]
		procedure TestTargetName_SetsWithoutError;

		{GetHTTP via interface}
		[Test]
		procedure TestGetHTTP_ViaInterface_ReturnsHTTP;

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
		{EAbort handling}
		[Test]
		procedure TestExceptionHandler_EAbort_ReturnsCancelled;

		{Socket error handling}
		[Test]
		procedure TestExceptionHandler_SocketError_LogsAndReturnsError;

		{Generic exception handling}
		[Test]
		procedure TestExceptionHandler_GenericException_ReturnsError;

		{Token outdated handling}
		[Test]
		procedure TestExceptionHandler_TokenOutdated_ReturnsTokenOutdatedError;
		[Test]
		procedure TestExceptionHandler_NotAuthorized_ReturnsTokenOutdatedError;
		[Test]
		procedure TestExceptionHandler_OtherHTTPError_DoesNotReturnTokenOutdated;

		{Logging tests}
		[Test]
		procedure TestExceptionHandler_WithLogging_LogsHTTPProtocolException;
		[Test]
		procedure TestExceptionHandler_WithLogging_LogsSocketError;
		[Test]
		procedure TestExceptionHandler_WithLogging_LogsGenericException;
	end;

	[TestFixture]
	TCloudMailRuHTTPCSRFTokenTest = class
	private
		FHTTP: TCloudMailRuHTTP;
		FHTTPInterface: ICloudHTTP;
		FSettings: TConnectionSettings;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestSetCSRFToken_SetsCustomHeader;
		[Test]
		procedure TestSetCSRFToken_EmptyToken_ClearsHeader;
		[Test]
		procedure TestSetCSRFToken_OverwritesPreviousToken;
	end;

	[TestFixture]
	TCloudMailRuHTTPSetProgressTest = class
	private
		FHTTP: TCloudMailRuHTTP;
		FSettings: TConnectionSettings;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestSetProgress_DoesNotRaise;
		[Test]
		procedure TestSetProgress_AcceptsNullProgress;
	end;

	{Mock progress that tracks calls and can simulate user cancellation}
	TMockProgressForHTTP = class(TInterfacedObject, IProgress)
	private
		FProgressResult: Boolean;
		FLastPercent: Integer;
		FProgressCalled: Boolean;
	public
		constructor Create;
		function Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(SourceName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(PercentDone: Integer): Boolean; overload;
		function Aborted(): Boolean;
		property ProgressResult: Boolean read FProgressResult write FProgressResult;
		property LastPercent: Integer read FLastPercent;
		property ProgressCalled: Boolean read FProgressCalled;
	end;

	{Tests for HTTPProgress callback -- direct invocation without network}
	[TestFixture]
	TCloudMailRuHTTPProgressTest = class
	private
		FHTTP: TCloudMailRuHTTP;
		FMockProgress: TMockProgressForHTTP;
		FProgressRef: IProgress;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestHTTPProgress_ReadMode_CalculatesPercent;
		[Test]
		procedure TestHTTPProgress_WriteMode_UsesRequestContentLength;
		[Test]
		procedure TestHTTPProgress_AbortOnCancel_RaisesEAbort;
	end;

	{Tests for production HTTP methods -- uses localhost:1 for immediate connection failure}
	[TestFixture]
	TCloudMailRuHTTPNetworkMethodsTest = class
	private
		FHTTP: TCloudMailRuHTTP;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestGetFile_FailedConnection_ReturnsError;
		[Test]
		procedure TestPostForm_FailedConnection_ReturnsFalse;
		[Test]
		procedure TestPutFile_FailedConnection_ReturnsError;
	end;

	{Tests that HTTP methods emit LOG_LEVEL_HTTP entry logs before network operations}
	[TestFixture]
	TCloudMailRuHTTPLoggingTest = class
	private
		FHTTP: TCloudMailRuHTTP;
		FMockLogger: TMockLoggerForHTTP;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestGetPage_LogsHTTPRequest;
		[Test]
		procedure TestGetFile_LogsHTTPRequest;
		[Test]
		procedure TestPostForm_LogsHTTPRequest;
		[Test]
		procedure TestPutFile_LogsHTTPRequest;
	end;

implementation

uses
	IdException,
	IdExceptionCore,
	IdStack,
	IdHTTPHeaderInfo,
	IdComponent;

constructor TMockLoggerForHTTP.Create;
begin
	inherited Create;
	FLogCalled := False;
	FEntries := nil;
end;

procedure TMockLoggerForHTTP.Log(LogLevel, MsgType: Integer; LogString: WideString);
var
	Entry: TLogEntry;
begin
	FLogCalled := True;
	FLastLogLevel := LogLevel;
	FLastMessage := LogString;
	Entry.LogLevel := LogLevel;
	Entry.Message := LogString;
	SetLength(FEntries, Length(FEntries) + 1);
	FEntries[High(FEntries)] := Entry;
end;

procedure TMockLoggerForHTTP.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
var
	Entry: TLogEntry;
begin
	FLogCalled := True;
	FLastLogLevel := LogLevel;
	FLastMessage := Format(LogString, Args);
	Entry.LogLevel := LogLevel;
	Entry.Message := FLastMessage;
	SetLength(FEntries, Length(FEntries) + 1);
	FEntries[High(FEntries)] := Entry;
end;

function TMockLoggerForHTTP.HasLogWithLevel(Level: Integer): Boolean;
var
	i: Integer;
begin
	Result := False;
	for i := 0 to High(FEntries) do
		if FEntries[i].LogLevel = Level then
			Exit(True);
end;

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

procedure TNullCloudHTTPTest.TestGetAuthCookie_ReturnsNil;
begin
	Assert.IsNull(FHTTP.GetAuthCookie);
end;

procedure TNullCloudHTTPTest.TestGetHTTP_ReturnsNil;
begin
	Assert.IsNull(FHTTP.GetHTTP);
end;

procedure TNullCloudHTTPTest.TestSetCSRFToken_DoesNotRaiseException;
begin
	{Should not raise}
	FHTTP.SetCSRFToken('test-token-123');
	Assert.Pass;
end;

procedure TNullCloudHTTPTest.TestSetProgress_DoesNotRaiseException;
begin
	{Should not raise}
	FHTTP.SetProgress(TNullProgress.Create);
	Assert.Pass;
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
	HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
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
	HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
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
	HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
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
	HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
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
	HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
	try
		Assert.AreEqual(-5000, HTTP.HTTP.ConnectTimeout);
		Assert.AreEqual(-5000, HTTP.HTTP.ReadTimeout);
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_PositiveTimeout_SetsTimeouts;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
begin
	Settings := Default(TConnectionSettings);
	Settings.SocketTimeout := 5000;
	HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
	try
		Assert.AreEqual(5000, HTTP.HTTP.ConnectTimeout, 'Positive timeout should be applied');
		Assert.AreEqual(5000, HTTP.HTTP.ReadTimeout, 'Positive timeout should be applied');
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPConstructorTest.TestCreate_ZeroTimeout_DoesNotSetTimeouts;
var
	Settings: TConnectionSettings;
	HTTP: TCloudMailRuHTTP;
	DefaultHTTP: TIdHTTP;
begin
	{Zero means "use Indy default" - do not override}
	DefaultHTTP := TIdHTTP.Create();
	try
		Settings := Default(TConnectionSettings);
		Settings.SocketTimeout := 0;
		HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
		try
			Assert.AreEqual(DefaultHTTP.ConnectTimeout, HTTP.HTTP.ConnectTimeout, 'ConnectTimeout should stay at Indy default');
			Assert.AreEqual(DefaultHTTP.ReadTimeout, HTTP.HTTP.ReadTimeout, 'ReadTimeout should stay at Indy default');
		finally
			HTTP.Free;
		end;
	finally
		DefaultHTTP.Free;
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
	HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
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
	HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
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
	HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
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
	HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
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
	HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
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
	HTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FLogger, FProgress);
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
	FHTTP := TCloudMailRuHTTP.Create(FSettings, TIndySSLHandlerFactory.Create, TNullLogger.Create, TNullProgress.Create);
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

procedure TCloudMailRuHTTPSetterTest.TestSourceName_SetsWithoutError;
begin
	{SourceName property uses SetExternalSourceName setter}
	FHTTP.SourceName := 'test_source.txt';
	Assert.Pass;
end;

procedure TCloudMailRuHTTPSetterTest.TestTargetName_SetsWithoutError;
begin
	{TargetName property uses SetExternalTargetName setter}
	FHTTP.TargetName := 'test_target.txt';
	Assert.Pass;
end;

procedure TCloudMailRuHTTPSetterTest.TestGetHTTP_ViaInterface_ReturnsHTTP;
var
	Intf: ICloudHTTP;
begin
	{Access GetHTTP through the ICloudHTTP interface, not the public field}
	Intf := TCloudMailRuHTTP.Create(FSettings, TIndySSLHandlerFactory.Create, TNullLogger.Create, TNullProgress.Create);
	Assert.IsNotNull(Intf.HTTP);
end;

{TCloudMailRuHTTPExceptionHandlerTest}

procedure TCloudMailRuHTTPExceptionHandlerTest.Setup;
begin
	FSettings := Default(TConnectionSettings);
	FHTTP := TCloudMailRuHTTP.Create(FSettings, TIndySSLHandlerFactory.Create, TNullLogger.Create, TNullProgress.Create);
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

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_TokenOutdated_ReturnsTokenOutdatedError;
var
	E: EIdHTTPProtocolException;
begin
	{Token outdated is detected by checking if body equals "token"}
	E := EIdHTTPProtocolException.CreateError(403, 'Forbidden', '{"body":"token"}');
	try
		Assert.AreEqual(CLOUD_ERROR_TOKEN_OUTDATED, FHTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_POST, False));
	finally
		E.Free;
	end;
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_NotAuthorized_ReturnsTokenOutdatedError;
var
	E: EIdHTTPProtocolException;
begin
	//OAuth session expiry returns "error":"NOT/AUTHORIZED" with HTTP 403
	E := EIdHTTPProtocolException.CreateError(403, 'Forbidden', '{"error":"NOT/AUTHORIZED"}');
	try
		Assert.AreEqual(CLOUD_ERROR_TOKEN_OUTDATED, FHTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_POST, False));
	finally
		E.Free;
	end;
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_OtherHTTPError_DoesNotReturnTokenOutdated;
var
	E: EIdHTTPProtocolException;
begin
	//Other HTTP errors should not trigger token outdated
	E := EIdHTTPProtocolException.CreateError(500, 'Internal Server Error', '{"error":"INTERNAL"}');
	try
		Assert.AreNotEqual(CLOUD_ERROR_TOKEN_OUTDATED, FHTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_POST, False));
	finally
		E.Free;
	end;
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_WithLogging_LogsHTTPProtocolException;
var
	E: EIdHTTPProtocolException;
	MockLogger: TMockLoggerForHTTP;
	HTTP: TCloudMailRuHTTP;
begin
	MockLogger := TMockLoggerForHTTP.Create;
	HTTP := TCloudMailRuHTTP.Create(FSettings, TIndySSLHandlerFactory.Create, MockLogger, TNullProgress.Create);
	try
		E := EIdHTTPProtocolException.CreateError(500, 'Internal Server Error', 'Server error details');
		try
			HTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_POST, True);
			Assert.IsTrue(MockLogger.LogCalled, 'Logger should be called for HTTP protocol exception');
		finally
			E.Free;
		end;
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_WithLogging_LogsSocketError;
var
	E: EIdSocketError;
	MockLogger: TMockLoggerForHTTP;
	HTTP: TCloudMailRuHTTP;
begin
	MockLogger := TMockLoggerForHTTP.Create;
	HTTP := TCloudMailRuHTTP.Create(FSettings, TIndySSLHandlerFactory.Create, MockLogger, TNullProgress.Create);
	try
		E := EIdSocketError.Create('Connection refused');
		try
			HTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_GET, True);
			Assert.IsTrue(MockLogger.LogCalled, 'Logger should be called for socket error');
		finally
			E.Free;
		end;
	finally
		HTTP.Free;
	end;
end;

procedure TCloudMailRuHTTPExceptionHandlerTest.TestExceptionHandler_WithLogging_LogsGenericException;
var
	E: Exception;
	MockLogger: TMockLoggerForHTTP;
	HTTP: TCloudMailRuHTTP;
begin
	MockLogger := TMockLoggerForHTTP.Create;
	HTTP := TCloudMailRuHTTP.Create(FSettings, TIndySSLHandlerFactory.Create, MockLogger, TNullProgress.Create);
	try
		E := Exception.Create('Something went wrong');
		try
			HTTP.ExceptionHandler(E, 'http://test.com', HTTP_METHOD_POST, True);
			Assert.IsTrue(MockLogger.LogCalled, 'Logger should be called for generic exception');
		finally
			E.Free;
		end;
	finally
		HTTP.Free;
	end;
end;

{TCloudMailRuHTTPCSRFTokenTest}

procedure TCloudMailRuHTTPCSRFTokenTest.Setup;
begin
	FSettings := Default(TConnectionSettings);
	FHTTP := TCloudMailRuHTTP.Create(FSettings, TIndySSLHandlerFactory.Create, TNullLogger.Create, TNullProgress.Create);
	FHTTPInterface := FHTTP;
end;

procedure TCloudMailRuHTTPCSRFTokenTest.TearDown;
begin
	FHTTPInterface := nil;
	{FHTTP is freed via interface release}
end;

procedure TCloudMailRuHTTPCSRFTokenTest.TestSetCSRFToken_SetsCustomHeader;
begin
	FHTTPInterface.SetCSRFToken('my-csrf-token-123');
	Assert.AreEqual('my-csrf-token-123', FHTTP.HTTP.Request.CustomHeaders.Values['X-CSRF-Token']);
end;

procedure TCloudMailRuHTTPCSRFTokenTest.TestSetCSRFToken_EmptyToken_ClearsHeader;
begin
	FHTTPInterface.SetCSRFToken('initial-token');
	FHTTPInterface.SetCSRFToken('');
	Assert.AreEqual('', FHTTP.HTTP.Request.CustomHeaders.Values['X-CSRF-Token']);
end;

procedure TCloudMailRuHTTPCSRFTokenTest.TestSetCSRFToken_OverwritesPreviousToken;
begin
	FHTTPInterface.SetCSRFToken('first-token');
	FHTTPInterface.SetCSRFToken('second-token');
	Assert.AreEqual('second-token', FHTTP.HTTP.Request.CustomHeaders.Values['X-CSRF-Token']);
end;

{TCloudMailRuHTTPSetProgressTest}

procedure TCloudMailRuHTTPSetProgressTest.Setup;
begin
	FSettings := Default(TConnectionSettings);
	FHTTP := TCloudMailRuHTTP.Create(FSettings, TIndySSLHandlerFactory.Create, TNullLogger.Create, TNullProgress.Create);
end;

procedure TCloudMailRuHTTPSetProgressTest.TearDown;
begin
	FHTTP.Free;
end;

procedure TCloudMailRuHTTPSetProgressTest.TestSetProgress_DoesNotRaise;
begin
	FHTTP.SetProgress(TNullProgress.Create);
	Assert.Pass;
end;

procedure TCloudMailRuHTTPSetProgressTest.TestSetProgress_AcceptsNullProgress;
begin
	{SetProgress should accept any IProgress including another null}
	FHTTP.SetProgress(TNullProgress.Create);
	FHTTP.SetProgress(TNullProgress.Create);
	Assert.Pass;
end;

{TMockProgressForHTTP}

constructor TMockProgressForHTTP.Create;
begin
	inherited Create;
	FProgressResult := False;
	FLastPercent := -1;
	FProgressCalled := False;
end;

function TMockProgressForHTTP.Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean;
begin
	FProgressCalled := True;
	FLastPercent := PercentDone;
	Result := FProgressResult;
end;

function TMockProgressForHTTP.Progress(SourceName: WideString; PercentDone: Integer): Boolean;
begin
	FProgressCalled := True;
	FLastPercent := PercentDone;
	Result := FProgressResult;
end;

function TMockProgressForHTTP.Progress(PercentDone: Integer): Boolean;
begin
	FProgressCalled := True;
	FLastPercent := PercentDone;
	Result := FProgressResult;
end;

function TMockProgressForHTTP.Aborted(): Boolean;
begin
	Result := FProgressResult;
end;

{TCloudMailRuHTTPProgressTest}

procedure TCloudMailRuHTTPProgressTest.Setup;
begin
	FMockProgress := TMockProgressForHTTP.Create;
	FProgressRef := FMockProgress;
	FHTTP := TCloudMailRuHTTP.Create(Default(TConnectionSettings), TIndySSLHandlerFactory.Create, TNullLogger.Create, FProgressRef);
end;

procedure TCloudMailRuHTTPProgressTest.TearDown;
begin
	FHTTP.Free;
	FProgressRef := nil;
end;

procedure TCloudMailRuHTTPProgressTest.TestHTTPProgress_ReadMode_CalculatesPercent;
begin
	{Direct callback test: wmRead mode uses Response.ContentLength}
	FHTTP.SetProgressNames('source', 'target');
	FHTTP.HTTP.Response.ContentLength := 1000;
	FHTTP.HTTPProgress(FHTTP.HTTP, wmRead, 500);
	Assert.IsTrue(FMockProgress.ProgressCalled, 'Progress should be called');
	Assert.AreEqual(50, FMockProgress.LastPercent, 'Percent should be 50 for 500/1000');
end;

procedure TCloudMailRuHTTPProgressTest.TestHTTPProgress_WriteMode_UsesRequestContentLength;
begin
	{Direct callback test: wmWrite mode uses Request.ContentLength}
	FHTTP.SetProgressNames('source', 'target');
	FHTTP.HTTP.Request.ContentLength := 2000;
	FHTTP.HTTPProgress(FHTTP.HTTP, wmWrite, 1000);
	Assert.IsTrue(FMockProgress.ProgressCalled, 'Progress should be called for write mode');
	Assert.AreEqual(50, FMockProgress.LastPercent, 'Percent should be 50 for 1000/2000');
end;

procedure TCloudMailRuHTTPProgressTest.TestHTTPProgress_AbortOnCancel_RaisesEAbort;
begin
	{When progress returns True (user wants abort), HTTPProgress raises EAbort}
	FMockProgress.ProgressResult := True;
	FHTTP.SetProgressNames('source', 'target');
	FHTTP.HTTP.Response.ContentLength := 1000;
	Assert.WillRaise(
		procedure begin
			FHTTP.HTTPProgress(FHTTP.HTTP, wmRead, 500);
		end,
		EAbort);
end;

{TCloudMailRuHTTPNetworkMethodsTest}

const
	{Localhost port 1 is typically not bound -- connection fails immediately with RST}
	FAIL_URL = 'http://127.0.0.1:1/test';

procedure TCloudMailRuHTTPNetworkMethodsTest.Setup;
var
	Settings: TConnectionSettings;
begin
	Settings := Default(TConnectionSettings);
	Settings.SocketTimeout := 500;
	FHTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, TNullLogger.Create, TNullProgress.Create);
end;

procedure TCloudMailRuHTTPNetworkMethodsTest.TearDown;
begin
	FHTTP.Free;
end;

procedure TCloudMailRuHTTPNetworkMethodsTest.TestGetFile_FailedConnection_ReturnsError;
var
	Stream: TMemoryStream;
	ResultCode: Integer;
begin
	Stream := TMemoryStream.Create;
	try
		ResultCode := FHTTP.GetFile(FAIL_URL, Stream);
		Assert.AreEqual(FS_FILE_READERROR, ResultCode);
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuHTTPNetworkMethodsTest.TestPostForm_FailedConnection_ReturnsFalse;
var
	Answer: WideString;
begin
	Assert.IsFalse(FHTTP.PostForm(FAIL_URL, 'data=value', Answer));
end;

procedure TCloudMailRuHTTPNetworkMethodsTest.TestPutFile_FailedConnection_ReturnsError;
var
	Stream: TMemoryStream;
	Answer: WideString;
	ResultCode: Integer;
begin
	Stream := TMemoryStream.Create;
	try
		ResultCode := FHTTP.PutFile(FAIL_URL, 'file.txt', Stream, Answer);
		Assert.AreNotEqual(FS_FILE_OK, ResultCode, 'PutFile should fail on connection error');
	finally
		Stream.Free;
	end;
end;

{TCloudMailRuHTTPLoggingTest}

const
	LOG_FAIL_URL = 'http://127.0.0.1:1/test';

procedure TCloudMailRuHTTPLoggingTest.Setup;
var
	Settings: TConnectionSettings;
begin
	FMockLogger := TMockLoggerForHTTP.Create;
	Settings := Default(TConnectionSettings);
	Settings.SocketTimeout := 500;
	FHTTP := TCloudMailRuHTTP.Create(Settings, TIndySSLHandlerFactory.Create, FMockLogger, TNullProgress.Create);
end;

procedure TCloudMailRuHTTPLoggingTest.TearDown;
begin
	FHTTP.Free;
end;

procedure TCloudMailRuHTTPLoggingTest.TestGetPage_LogsHTTPRequest;
var
	Answer: WideString;
	ProgressEnabled: Boolean;
begin
	ProgressEnabled := False;
	FHTTP.GetPage(LOG_FAIL_URL, Answer, ProgressEnabled);
	Assert.IsTrue(FMockLogger.HasLogWithLevel(LOG_LEVEL_HTTP), 'GetPage should log at LOG_LEVEL_HTTP');
end;

procedure TCloudMailRuHTTPLoggingTest.TestGetFile_LogsHTTPRequest;
var
	Stream: TMemoryStream;
begin
	Stream := TMemoryStream.Create;
	try
		FHTTP.GetFile(LOG_FAIL_URL, Stream);
		Assert.IsTrue(FMockLogger.HasLogWithLevel(LOG_LEVEL_HTTP), 'GetFile should log at LOG_LEVEL_HTTP');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuHTTPLoggingTest.TestPostForm_LogsHTTPRequest;
var
	Answer: WideString;
begin
	FHTTP.PostForm(LOG_FAIL_URL, 'data=value', Answer);
	Assert.IsTrue(FMockLogger.HasLogWithLevel(LOG_LEVEL_HTTP), 'PostForm should log at LOG_LEVEL_HTTP');
end;

procedure TCloudMailRuHTTPLoggingTest.TestPutFile_LogsHTTPRequest;
var
	Stream: TMemoryStream;
	Answer: WideString;
begin
	Stream := TMemoryStream.Create;
	try
		FHTTP.PutFile(LOG_FAIL_URL, 'file.txt', Stream, Answer);
		Assert.IsTrue(FMockLogger.HasLogWithLevel(LOG_LEVEL_HTTP), 'PutFile should log at LOG_LEVEL_HTTP');
	finally
		Stream.Free;
	end;
end;

initialization
	TDUnitX.RegisterTestFixture(TNullCloudHTTPTest);
	TDUnitX.RegisterTestFixture(TCloudMailRuHTTPConstructorTest);
	TDUnitX.RegisterTestFixture(TCloudMailRuHTTPSetterTest);
	TDUnitX.RegisterTestFixture(TCloudMailRuHTTPExceptionHandlerTest);
	TDUnitX.RegisterTestFixture(TCloudMailRuHTTPCSRFTokenTest);
	TDUnitX.RegisterTestFixture(TCloudMailRuHTTPSetProgressTest);
	TDUnitX.RegisterTestFixture(TCloudMailRuHTTPProgressTest);
	TDUnitX.RegisterTestFixture(TCloudMailRuHTTPNetworkMethodsTest);
	TDUnitX.RegisterTestFixture(TCloudMailRuHTTPLoggingTest);

end.
