unit IHTTPManagerInterface;

{Abstraction for HTTP connection management, enabling testability without real HTTP connections.}

interface

uses
	ICloudHTTPInterface,
	ConnectionSettings;

type

	{Interface for per-thread HTTP connection pooling}
	IHTTPManager = interface
		['{D33816F9-B695-44DB-AB97-38171EC81C7B}']
		{Returns HTTP connection for the specified thread, creating one if needed}
		function Get(ThreadId: Cardinal): ICloudHTTP;

		{Returns the connection settings used by this manager}
		function GetConnectionSettings: TConnectionSettings;

		{Sets the proxy password for all connections}
		procedure SetProxyPassword(Password: WideString);

		{Property accessors for convenience}
		property ConnectionSettings: TConnectionSettings read GetConnectionSettings;
		property ProxyPassword: WideString write SetProxyPassword;
	end;

	{Null implementation that returns nil/default values.
	 Useful for testing scenarios where no HTTP connections are expected.}
	TNullHTTPManager = class(TInterfacedObject, IHTTPManager)
	private
		FConnectionSettings: TConnectionSettings;
	public
		function Get(ThreadId: Cardinal): ICloudHTTP;
		function GetConnectionSettings: TConnectionSettings;
		procedure SetProxyPassword(Password: WideString);
	end;

implementation

{TNullHTTPManager}

function TNullHTTPManager.Get(ThreadId: Cardinal): ICloudHTTP;
begin
	Result := nil;
end;

function TNullHTTPManager.GetConnectionSettings: TConnectionSettings;
begin
	Result := FConnectionSettings;
end;

procedure TNullHTTPManager.SetProxyPassword(Password: WideString);
begin
	FConnectionSettings.ProxySettings.Password := Password;
end;

end.
