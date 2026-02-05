unit IndySecSSLHandlerFactory;

{IndySecOpenSSL handler factory implementation.
	Supports OpenSSL 1.1.x and 3.x via TIdSecIOHandlerSocketOpenSSL.
	This backend provides TLS 1.3 support and modern OpenSSL compatibility.}

interface

uses
	SSLHandlerFactory,
	IdSSL,
	IdSocks,
	IdSecOpenSSL,
	IdSecOpenSSLOptions,
	ProxySettings,
	SettingsConstants;

type
	{Factory implementation using IndySecOpenSSL (IdSecOpenSSL unit).
		Creates TIdSecIOHandlerSocketOpenSSL instances with modern TLS support.}
	TIndySecSSLHandlerFactory = class(TInterfacedObject, ISSLHandlerFactory)
	private
		FSocksInfo: TIdSocksInfo; {Reusable SOCKS info object}
	public
		constructor Create;
		destructor Destroy; override;

		{ISSLHandlerFactory}
		function CreateHandler: TIdSSLIOHandlerSocketBase;
		procedure ConfigureSocksProxy(Handler: TIdSSLIOHandlerSocketBase; const Proxy: TProxySettings);
		function GetLibCryptoHandle: THandle;
		function GetBackendName: WideString;
	end;

implementation

uses
	IdSecOpenSSLAPI;

{TIndySecSSLHandlerFactory}

constructor TIndySecSSLHandlerFactory.Create;
begin
	inherited Create;
	FSocksInfo := TIdSocksInfo.Create;
end;

destructor TIndySecSSLHandlerFactory.Destroy;
begin
	FSocksInfo.Free;
	inherited;
end;

function TIndySecSSLHandlerFactory.CreateHandler: TIdSSLIOHandlerSocketBase;
var
	Handler: TIdSecIOHandlerSocketOpenSSL;
begin
	Handler := TIdSecIOHandlerSocketOpenSSL.Create;
	{Default to TLS 1.2 and 1.3 for modern security}
	Handler.SSLOptions.SSLVersions := [sslvTLSv1_2, sslvTLSv1_3];
	Handler.SSLOptions.Mode := sslmClient;
	Result := Handler;
end;

procedure TIndySecSSLHandlerFactory.ConfigureSocksProxy(Handler: TIdSSLIOHandlerSocketBase; const Proxy: TProxySettings);
begin
	if not (Proxy.ProxyType in SocksProxyTypes) then
		Exit;

	FSocksInfo.Host := Proxy.Server;
	FSocksInfo.Port := Proxy.Port;

	if Proxy.User <> '' then
	begin
		FSocksInfo.Authentication := saUsernamePassword;
		FSocksInfo.Username := Proxy.User;
		FSocksInfo.Password := Proxy.Password;
	end
	else
		FSocksInfo.Authentication := saNoAuthentication;

	case Proxy.ProxyType of
		ProxySocks5:
			FSocksInfo.Version := svSocks5;
		ProxySocks4:
			FSocksInfo.Version := svSocks4;
	end;

	FSocksInfo.Enabled := True;
	Handler.TransparentProxy := FSocksInfo;
end;

function TIndySecSSLHandlerFactory.GetLibCryptoHandle: THandle;
var
	OpenSSLDDL: IOpenSSLDLL;
begin
	OpenSSLDDL := GetIOpenSSLDDL;
	if Assigned(OpenSSLDDL) and OpenSSLDDL.IsLoaded then
		Result := OpenSSLDDL.GetLibCryptoHandle
	else
		Result := 0;
end;

function TIndySecSSLHandlerFactory.GetBackendName: WideString;
begin
	Result := 'IndySec SSL (OpenSSL 1.1.x/3.x)';
end;

end.
