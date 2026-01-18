unit HTTPManager;

interface

uses
	CloudMailRuHTTP,
	CMRConstants,
	PLUGIN_TYPES,
	ConnectionSettings,
	ILoggerInterface,
	IProgressInterface,
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

		Connections: TDictionary<Cardinal, TCloudMailRuHTTP>; //<ThreadId, HTTP>

	public
		{Параметры, с которыми будут отдаваться подключения: создаём с ними экземпляр класса, а дальше он сам рулит}
		constructor Create(Settings: TConnectionSettings; Logger: ILogger; Progress: IProgress);
		destructor Destroy; override;
		function Get(ThreadId: Cardinal): TCloudMailRuHTTP;
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
	Connections := TDictionary<Cardinal, TCloudMailRuHTTP>.Create;
end;

destructor THTTPManager.Destroy;
var
	Item: TPair<Cardinal, TCloudMailRuHTTP>;
begin
	for Item in Connections do
		Item.Value.Destroy;

	FreeAndNil(Connections);
	inherited;
end;

function THTTPManager.Get(ThreadId: Cardinal): TCloudMailRuHTTP;
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
