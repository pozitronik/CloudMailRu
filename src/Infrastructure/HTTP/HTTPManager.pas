unit HTTPManager;

{Abstraction for HTTP connection management, enabling testability without real HTTP connections.}

interface

uses
	CloudHTTP,
	CMRConstants,
	PLUGIN_TYPES,
	ConnectionSettings,
	TCLogger,
	TCProgress,
	System.Generics.Collections,
	SysUtils,
	IdCookieManager;

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

	THTTPManager = class(TInterfacedObject, IHTTPManager)
	private
		FConnectionSettings: TConnectionSettings;
		FLogger: ILogger;
		FProgress: IProgress;

		Connections: TDictionary<Cardinal, ICloudHTTP>; //<ThreadId, HTTP>

	public
		{Параметры, с которыми будут отдаваться подключения: создаём с ними экземпляр класса, а дальше он сам рулит}
		constructor Create(Settings: TConnectionSettings; Logger: ILogger; Progress: IProgress);
		destructor Destroy; override;
		function Get(ThreadId: Cardinal): ICloudHTTP;
		function GetConnectionSettings: TConnectionSettings;
		procedure SetProxyPassword(Password: WideString);

		property ConnectionSettings: TConnectionSettings read GetConnectionSettings;
		property ProxyPassword: WideString write SetProxyPassword;
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

{THTTPManager}

constructor THTTPManager.Create(Settings: TConnectionSettings; Logger: ILogger; Progress: IProgress);
begin
	self.FConnectionSettings := Settings;
	self.FProgress := Progress;
	self.FLogger := Logger;
	Connections := TDictionary<Cardinal, ICloudHTTP>.Create;
end;

destructor THTTPManager.Destroy;
begin
	{Interfaces are reference-counted - just clear the dictionary}
	Connections.Clear;
	FreeAndNil(Connections);
	inherited;
end;

function THTTPManager.Get(ThreadId: Cardinal): ICloudHTTP;
begin
	if not Connections.TryGetValue(ThreadId, Result) then
	begin
		Result := TCloudMailRuHTTP.Create(FConnectionSettings, FLogger, FProgress);
		Connections.AddOrSetValue(ThreadId, Result);
	end;
end;

function THTTPManager.GetConnectionSettings: TConnectionSettings;
begin
	Result := FConnectionSettings;
end;

procedure THTTPManager.SetProxyPassword(Password: WideString);
begin
	FConnectionSettings.ProxySettings.Password := Password;
end;

end.
