unit HTTPManager;

interface

uses
	CloudMailRuHTTP,
	CMRConstants,
	PLUGIN_TYPES,
	ConnectionSettings,
	ILoggerInterface,
	IProgressInterface,
	System.Generics.Collections,
	SysUtils,
	IdCookieManager;

type

	THTTPManager = class
	private
		FConnectionSettings: TConnectionSettings;
		FLogger: ILogger;
		FProgress: IProgress;

		Connections: TDictionary<Cardinal, TCloudMailRuHTTP>; //<ThreadId, HTTP>

	public
		{Параметры, с которыми будут отдаваться подключения: создаём с ними экземпляр класса, а дальше он сам рулит}
		constructor Create(Settings: TConnectionSettings; Logger: ILogger; Progress: IProgress);
		destructor Destroy; override;
		function get(ThreadId: Cardinal): TCloudMailRuHTTP;

		property ConnectionSettings: TConnectionSettings read FConnectionSettings;
		property ProxyPassword: WideString write FConnectionSettings.ProxySettings.Password;
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

function THTTPManager.get(ThreadId: Cardinal): TCloudMailRuHTTP;
begin
	if not Connections.TryGetValue(ThreadId, result) then
	begin
		result := TCloudMailRuHTTP.Create(FConnectionSettings, FLogger, FProgress);
		Connections.AddOrSetValue(ThreadId, result);
	end;
end;

end.
