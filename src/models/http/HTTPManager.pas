unit HTTPManager;

interface

uses
	CloudMailRuHTTP,
	ICloudHTTPInterface,
	CMRConstants,
	PLUGIN_TYPES,
	ConnectionSettings,
	TCLogger,
	TCProgress,
	IHTTPManagerInterface,
	System.Generics.Collections,
	SysUtils,
	IdCookieManager;

type

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
