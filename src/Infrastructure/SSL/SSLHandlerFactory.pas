unit SSLHandlerFactory;

{SSL handler factory interface for abstracting SSL/TLS implementation.
	Enables switching between different SSL backends (Indy SSL, IndySecOpenSSL)
	without changing HTTP layer code.

	Both Indy's TIdSSLIOHandlerSocketOpenSSL and IndySecOpenSSL's
	TIdSecIOHandlerSocketOpenSSL inherit from TIdSSLIOHandlerSocketBase,
	allowing polymorphic usage through this factory.}

interface

uses
	IdSSL,
	IdSocks,
	ProxySettings;

type
	{SSL backend implementation type}
	TSSLBackend = (
		sslbAuto,    {Auto-detect best available backend}
		sslbIndy,    {Original Indy SSL (OpenSSL 1.x)}
		sslbIndySec  {IndySecOpenSSL (OpenSSL 1.x/3.x)}
	);

	{Factory interface for creating and configuring SSL handlers.
		Implementations wrap specific SSL libraries while exposing common interface.}
	ISSLHandlerFactory = interface
		['{FB459A2A-B089-42AF-B438-01EAB5767668}']

		{Create new SSL handler instance.
			@return SSL handler compatible with TIdHTTP.IOHandler}
		function CreateHandler: TIdSSLIOHandlerSocketBase;

		{Configure SOCKS proxy on SSL handler.
			HTTP proxy is configured on TIdHTTP directly, not through SSL handler.
			@param Handler SSL handler to configure
			@param Proxy SOCKS proxy settings (ignored if ProxyType is not SOCKS)}
		procedure ConfigureSocksProxy(Handler: TIdSSLIOHandlerSocketBase; const Proxy: TProxySettings);

		{Get OpenSSL crypto library handle for hash/cipher operations.
			Used by OpenSSLProvider to access EVP functions.
			@return Library handle, or 0 if not loaded}
		function GetLibCryptoHandle: THandle;

		{Get human-readable backend name for logging.
			@return Backend identifier string}
		function GetBackendName: WideString;
	end;

implementation

end.
