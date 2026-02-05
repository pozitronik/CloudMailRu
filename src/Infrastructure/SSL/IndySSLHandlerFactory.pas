unit IndySSLHandlerFactory;

{Indy SSL handler factory implementation using standard Indy OpenSSL.
	Supports OpenSSL 1.0.x and 1.1.x via TIdSSLIOHandlerSocketOpenSSL.}

interface

uses
	SSLHandlerFactory,
	IdSSL,
	IdSocks,
	IdSSLOpenSSL,
	ProxySettings,
	SettingsConstants;

type
	{Factory implementation using standard Indy SSL (IdSSLOpenSSL unit).
		Creates TIdSSLIOHandlerSocketOpenSSL instances.}
	TIndySSLHandlerFactory = class(TInterfacedObject, ISSLHandlerFactory)
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
	IdSSLOpenSSLHeaders;

{TIndySSLHandlerFactory}

constructor TIndySSLHandlerFactory.Create;
begin
	inherited Create;
	FSocksInfo := TIdSocksInfo.Create;
end;

destructor TIndySSLHandlerFactory.Destroy;
begin
	FSocksInfo.Free;
	inherited;
end;

function TIndySSLHandlerFactory.CreateHandler: TIdSSLIOHandlerSocketBase;
var
	Handler: TIdSSLIOHandlerSocketOpenSSL;
begin
	Handler := TIdSSLIOHandlerSocketOpenSSL.Create;
	Handler.SSLOptions.SSLVersions := [sslvSSLv23];
	Result := Handler;
end;

procedure TIndySSLHandlerFactory.ConfigureSocksProxy(Handler: TIdSSLIOHandlerSocketBase; const Proxy: TProxySettings);
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

function TIndySSLHandlerFactory.GetLibCryptoHandle: THandle;
begin
	Result := GetCryptLibHandle;
end;

function TIndySSLHandlerFactory.GetBackendName: WideString;
begin
	Result := 'Indy SSL (OpenSSL 1.x)';
end;

end.
