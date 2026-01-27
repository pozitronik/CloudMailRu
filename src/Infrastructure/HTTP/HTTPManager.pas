unit HTTPManager;

{Abstraction for HTTP connection management, enabling testability without real HTTP connections.}

interface

uses
	CloudHTTP,
	CMRConstants,
	WFXTypes,
	ConnectionSettings,
	TCLogger,
	TCProgress,
	System.Generics.Collections,
	SysUtils,
	IdCookieManager;

type

	{Factory interface for creating HTTP connections.
		Enables dependency injection and testing of HTTPManager without real connections.}
	ICloudHTTPFactory = interface
		['{E4A7C9D2-8B3F-4E1A-9D5C-7F2B8A6E4C1D}']
		function CreateHTTP(Settings: TConnectionSettings; Logger: ILogger; Progress: IProgress): ICloudHTTP;
	end;

	{Default factory implementation that creates real TCloudMailRuHTTP instances.}
	TCloudHTTPFactory = class(TInterfacedObject, ICloudHTTPFactory)
	public
		function CreateHTTP(Settings: TConnectionSettings; Logger: ILogger; Progress: IProgress): ICloudHTTP;
	end;

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
		FHTTPFactory: ICloudHTTPFactory;

		Connections: TDictionary<Cardinal, ICloudHTTP>; //<ThreadId, HTTP>

	public
		{Creates manager with injected dependencies for connection pooling}
		constructor Create(Settings: TConnectionSettings; Logger: ILogger; Progress: IProgress; HTTPFactory: ICloudHTTPFactory);
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

{TCloudHTTPFactory}

function TCloudHTTPFactory.CreateHTTP(Settings: TConnectionSettings; Logger: ILogger; Progress: IProgress): ICloudHTTP;
begin
	Result := TCloudMailRuHTTP.Create(Settings, Logger, Progress);
end;

{THTTPManager}

constructor THTTPManager.Create(Settings: TConnectionSettings; Logger: ILogger; Progress: IProgress; HTTPFactory: ICloudHTTPFactory);
begin
	self.FConnectionSettings := Settings;
	self.FProgress := Progress;
	self.FLogger := Logger;
	self.FHTTPFactory := HTTPFactory;
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
		Result := FHTTPFactory.CreateHTTP(FConnectionSettings, FLogger, FProgress);
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
