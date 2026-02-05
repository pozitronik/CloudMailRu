unit IndySecSSLHandlerFactoryTest;

interface

uses
	System.SysUtils,
	IndySecSSLHandlerFactory,
	SSLHandlerFactory,
	IdSSL,
	IdSecOpenSSL,
	IdSecOpenSSLOptions,
	IdSocks,
	ProxySettings,
	SettingsConstants,
	DUnitX.TestFramework;

type
	{Tests for TIndySecSSLHandlerFactory - IndySecOpenSSL backend with TLS 1.3 support}
	[TestFixture]
	TIndySecSSLHandlerFactoryTest = class
	private
		FFactory: ISSLHandlerFactory;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Construction}
		[Test]
		procedure TestCreate_ImplementsInterface;

		{CreateHandler}
		[Test]
		procedure TestCreateHandler_ReturnsNonNil;
		[Test]
		procedure TestCreateHandler_ReturnsCorrectType;
		[Test]
		procedure TestCreateHandler_SetsTLS12AndTLS13;
		[Test]
		procedure TestCreateHandler_SetsClientMode;
		[Test]
		procedure TestCreateHandler_MultipleCallsReturnDistinctInstances;

		{GetBackendName}
		[Test]
		procedure TestGetBackendName_ReturnsExpectedString;
		[Test]
		procedure TestGetBackendName_IsNotEmpty;

		{GetLibCryptoHandle - depends on OpenSSL availability at runtime}
		[Test]
		procedure TestGetLibCryptoHandle_DoesNotRaise;

		{ConfigureSocksProxy - non-SOCKS types are ignored}
		[Test]
		procedure TestConfigureSocksProxy_ProxyNone_DoesNotSetTransparentProxy;
		[Test]
		procedure TestConfigureSocksProxy_ProxyHTTP_DoesNotSetTransparentProxy;

		{ConfigureSocksProxy - SOCKS5}
		[Test]
		procedure TestConfigureSocksProxy_Socks5_SetsTransparentProxy;
		[Test]
		procedure TestConfigureSocksProxy_Socks5_SetsHostAndPort;
		[Test]
		procedure TestConfigureSocksProxy_Socks5_SetsVersion;
		[Test]
		procedure TestConfigureSocksProxy_Socks5_WithCredentials_SetsAuth;
		[Test]
		procedure TestConfigureSocksProxy_Socks5_NoCredentials_SetsNoAuth;

		{ConfigureSocksProxy - SOCKS4}
		[Test]
		procedure TestConfigureSocksProxy_Socks4_SetsVersion;

		{ConfigureSocksProxy - reusability}
		[Test]
		procedure TestConfigureSocksProxy_CalledTwice_UpdatesSettings;
	end;

implementation

{TIndySecSSLHandlerFactoryTest}

procedure TIndySecSSLHandlerFactoryTest.Setup;
begin
	FFactory := TIndySecSSLHandlerFactory.Create;
end;

procedure TIndySecSSLHandlerFactoryTest.TearDown;
begin
	FFactory := nil;
end;

{Construction}

procedure TIndySecSSLHandlerFactoryTest.TestCreate_ImplementsInterface;
begin
	Assert.IsNotNull(FFactory);
end;

{CreateHandler}

procedure TIndySecSSLHandlerFactoryTest.TestCreateHandler_ReturnsNonNil;
var
	Handler: TIdSSLIOHandlerSocketBase;
begin
	Handler := FFactory.CreateHandler;
	try
		Assert.IsNotNull(Handler);
	finally
		Handler.Free;
	end;
end;

procedure TIndySecSSLHandlerFactoryTest.TestCreateHandler_ReturnsCorrectType;
var
	Handler: TIdSSLIOHandlerSocketBase;
begin
	Handler := FFactory.CreateHandler;
	try
		Assert.IsTrue(Handler is TIdSecIOHandlerSocketOpenSSL,
			'Handler should be TIdSecIOHandlerSocketOpenSSL');
	finally
		Handler.Free;
	end;
end;

procedure TIndySecSSLHandlerFactoryTest.TestCreateHandler_SetsTLS12AndTLS13;
var
	Handler: TIdSecIOHandlerSocketOpenSSL;
begin
	Handler := FFactory.CreateHandler as TIdSecIOHandlerSocketOpenSSL;
	try
		Assert.IsTrue(sslvTLSv1_2 in Handler.SSLOptions.SSLVersions,
			'SSLVersions should include TLS 1.2');
		Assert.IsTrue(sslvTLSv1_3 in Handler.SSLOptions.SSLVersions,
			'SSLVersions should include TLS 1.3');
	finally
		Handler.Free;
	end;
end;

procedure TIndySecSSLHandlerFactoryTest.TestCreateHandler_SetsClientMode;
var
	Handler: TIdSecIOHandlerSocketOpenSSL;
begin
	Handler := FFactory.CreateHandler as TIdSecIOHandlerSocketOpenSSL;
	try
		Assert.AreEqual(Ord(sslmClient), Ord(Handler.SSLOptions.Mode),
			'SSL mode should be sslmClient');
	finally
		Handler.Free;
	end;
end;

procedure TIndySecSSLHandlerFactoryTest.TestCreateHandler_MultipleCallsReturnDistinctInstances;
var
	Handler1, Handler2: TIdSSLIOHandlerSocketBase;
begin
	Handler1 := FFactory.CreateHandler;
	Handler2 := FFactory.CreateHandler;
	try
		Assert.AreNotEqual(NativeInt(Handler1), NativeInt(Handler2),
			'Each call should return a new instance');
	finally
		Handler2.Free;
		Handler1.Free;
	end;
end;

{GetBackendName}

procedure TIndySecSSLHandlerFactoryTest.TestGetBackendName_ReturnsExpectedString;
begin
	Assert.AreEqual('IndySec SSL (OpenSSL 1.1.x/3.x)', FFactory.GetBackendName);
end;

procedure TIndySecSSLHandlerFactoryTest.TestGetBackendName_IsNotEmpty;
begin
	Assert.IsNotEmpty(FFactory.GetBackendName);
end;

{GetLibCryptoHandle}

procedure TIndySecSSLHandlerFactoryTest.TestGetLibCryptoHandle_DoesNotRaise;
var
	Handle: THandle;
begin
	{In test environment OpenSSL may not be loaded, but the call should not raise}
	Handle := FFactory.GetLibCryptoHandle;
	Assert.Pass('GetLibCryptoHandle returned ' + IntToStr(Handle) + ' without exception');
end;

{ConfigureSocksProxy - non-SOCKS}

procedure TIndySecSSLHandlerFactoryTest.TestConfigureSocksProxy_ProxyNone_DoesNotSetTransparentProxy;
var
	Handler: TIdSSLIOHandlerSocketBase;
	Proxy: TProxySettings;
begin
	Handler := FFactory.CreateHandler;
	try
		Proxy := Default(TProxySettings);
		Proxy.ProxyType := ProxyNone;
		Proxy.Server := 'proxy.test';
		Proxy.Port := 1080;

		FFactory.ConfigureSocksProxy(Handler, Proxy);

		{Indy auto-creates a default TIdSocksInfo as TransparentProxy, so type check is useless.
			The factory sets Enabled := True when it configures SOCKS; verify it stayed disabled.}
		Assert.IsFalse((Handler.TransparentProxy as TIdSocksInfo).Enabled,
			'SOCKS proxy should not be enabled for ProxyNone');
	finally
		Handler.Free;
	end;
end;

procedure TIndySecSSLHandlerFactoryTest.TestConfigureSocksProxy_ProxyHTTP_DoesNotSetTransparentProxy;
var
	Handler: TIdSSLIOHandlerSocketBase;
	Proxy: TProxySettings;
begin
	Handler := FFactory.CreateHandler;
	try
		Proxy := Default(TProxySettings);
		Proxy.ProxyType := ProxyHTTP;
		Proxy.Server := 'proxy.test';
		Proxy.Port := 8080;

		FFactory.ConfigureSocksProxy(Handler, Proxy);

		Assert.IsFalse((Handler.TransparentProxy as TIdSocksInfo).Enabled,
			'SOCKS proxy should not be enabled for ProxyHTTP');
	finally
		Handler.Free;
	end;
end;

{ConfigureSocksProxy - SOCKS5}

procedure TIndySecSSLHandlerFactoryTest.TestConfigureSocksProxy_Socks5_SetsTransparentProxy;
var
	Handler: TIdSSLIOHandlerSocketBase;
	Proxy: TProxySettings;
begin
	Handler := FFactory.CreateHandler;
	try
		Proxy := Default(TProxySettings);
		Proxy.ProxyType := ProxySocks5;
		Proxy.Server := 'socks.test';
		Proxy.Port := 1080;

		FFactory.ConfigureSocksProxy(Handler, Proxy);

		Assert.IsNotNull(Handler.TransparentProxy,
			'TransparentProxy should be set for SOCKS5');
	finally
		Handler.Free;
	end;
end;

procedure TIndySecSSLHandlerFactoryTest.TestConfigureSocksProxy_Socks5_SetsHostAndPort;
var
	Handler: TIdSSLIOHandlerSocketBase;
	Proxy: TProxySettings;
	SocksInfo: TIdSocksInfo;
begin
	Handler := FFactory.CreateHandler;
	try
		Proxy := Default(TProxySettings);
		Proxy.ProxyType := ProxySocks5;
		Proxy.Server := 'socks5.host';
		Proxy.Port := 9090;

		FFactory.ConfigureSocksProxy(Handler, Proxy);

		SocksInfo := Handler.TransparentProxy as TIdSocksInfo;
		Assert.AreEqual('socks5.host', SocksInfo.Host);
		Assert.AreEqual(9090, SocksInfo.Port);
	finally
		Handler.Free;
	end;
end;

procedure TIndySecSSLHandlerFactoryTest.TestConfigureSocksProxy_Socks5_SetsVersion;
var
	Handler: TIdSSLIOHandlerSocketBase;
	Proxy: TProxySettings;
	SocksInfo: TIdSocksInfo;
begin
	Handler := FFactory.CreateHandler;
	try
		Proxy := Default(TProxySettings);
		Proxy.ProxyType := ProxySocks5;
		Proxy.Server := 'socks.test';
		Proxy.Port := 1080;

		FFactory.ConfigureSocksProxy(Handler, Proxy);

		SocksInfo := Handler.TransparentProxy as TIdSocksInfo;
		Assert.AreEqual(Ord(svSocks5), Ord(SocksInfo.Version),
			'Version should be svSocks5');
	finally
		Handler.Free;
	end;
end;

procedure TIndySecSSLHandlerFactoryTest.TestConfigureSocksProxy_Socks5_WithCredentials_SetsAuth;
var
	Handler: TIdSSLIOHandlerSocketBase;
	Proxy: TProxySettings;
	SocksInfo: TIdSocksInfo;
begin
	Handler := FFactory.CreateHandler;
	try
		Proxy := Default(TProxySettings);
		Proxy.ProxyType := ProxySocks5;
		Proxy.Server := 'socks.test';
		Proxy.Port := 1080;
		Proxy.User := 'testuser';
		Proxy.Password := 'testpass';

		FFactory.ConfigureSocksProxy(Handler, Proxy);

		SocksInfo := Handler.TransparentProxy as TIdSocksInfo;
		Assert.AreEqual(Ord(saUsernamePassword), Ord(SocksInfo.Authentication),
			'Authentication should be saUsernamePassword');
		Assert.AreEqual('testuser', SocksInfo.Username);
		Assert.AreEqual('testpass', SocksInfo.Password);
	finally
		Handler.Free;
	end;
end;

procedure TIndySecSSLHandlerFactoryTest.TestConfigureSocksProxy_Socks5_NoCredentials_SetsNoAuth;
var
	Handler: TIdSSLIOHandlerSocketBase;
	Proxy: TProxySettings;
	SocksInfo: TIdSocksInfo;
begin
	Handler := FFactory.CreateHandler;
	try
		Proxy := Default(TProxySettings);
		Proxy.ProxyType := ProxySocks5;
		Proxy.Server := 'socks.test';
		Proxy.Port := 1080;
		Proxy.User := '';

		FFactory.ConfigureSocksProxy(Handler, Proxy);

		SocksInfo := Handler.TransparentProxy as TIdSocksInfo;
		Assert.AreEqual(Ord(saNoAuthentication), Ord(SocksInfo.Authentication),
			'Authentication should be saNoAuthentication when User is empty');
	finally
		Handler.Free;
	end;
end;

{ConfigureSocksProxy - SOCKS4}

procedure TIndySecSSLHandlerFactoryTest.TestConfigureSocksProxy_Socks4_SetsVersion;
var
	Handler: TIdSSLIOHandlerSocketBase;
	Proxy: TProxySettings;
	SocksInfo: TIdSocksInfo;
begin
	Handler := FFactory.CreateHandler;
	try
		Proxy := Default(TProxySettings);
		Proxy.ProxyType := ProxySocks4;
		Proxy.Server := 'socks4.test';
		Proxy.Port := 1080;

		FFactory.ConfigureSocksProxy(Handler, Proxy);

		SocksInfo := Handler.TransparentProxy as TIdSocksInfo;
		Assert.AreEqual(Ord(svSocks4), Ord(SocksInfo.Version),
			'Version should be svSocks4');
	finally
		Handler.Free;
	end;
end;

{ConfigureSocksProxy - reusability}

procedure TIndySecSSLHandlerFactoryTest.TestConfigureSocksProxy_CalledTwice_UpdatesSettings;
var
	Handler1, Handler2: TIdSSLIOHandlerSocketBase;
	Proxy1, Proxy2: TProxySettings;
	SocksInfo: TIdSocksInfo;
begin
	Handler1 := FFactory.CreateHandler;
	Handler2 := FFactory.CreateHandler;
	try
		Proxy1 := Default(TProxySettings);
		Proxy1.ProxyType := ProxySocks5;
		Proxy1.Server := 'first.host';
		Proxy1.Port := 1080;

		FFactory.ConfigureSocksProxy(Handler1, Proxy1);

		Proxy2 := Default(TProxySettings);
		Proxy2.ProxyType := ProxySocks4;
		Proxy2.Server := 'second.host';
		Proxy2.Port := 2080;

		FFactory.ConfigureSocksProxy(Handler2, Proxy2);

		{The factory reuses a single FSocksInfo object, so the second call
			updates the shared instance. Verify the latest state is applied.}
		SocksInfo := Handler2.TransparentProxy as TIdSocksInfo;
		Assert.AreEqual('second.host', SocksInfo.Host);
		Assert.AreEqual(2080, SocksInfo.Port);
		Assert.AreEqual(Ord(svSocks4), Ord(SocksInfo.Version));
	finally
		Handler2.Free;
		Handler1.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TIndySecSSLHandlerFactoryTest);

end.
