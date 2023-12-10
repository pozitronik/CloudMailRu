unit HTTPManager;

interface

uses
	CloudMailRuHTTP,
	CMRConstants,
	PLUGIN_TYPES,
	Settings,
	ConnectionSettings,
	TCLogger,
	TCProgress,
	System.Generics.Collections,
	SysUtils,
	IdCookieManager;

type

	THTTPManager = class
	private
		FConnectionSettings: TConnectionSettings;
		FLogger: TTCLogger;
		FProgress: TTCProgress;

		Connections: TDictionary<Cardinal, TCloudMailRuHTTP>; //<ThreadId, HTTP>

	public
		{Параметры, с которыми будут отдаваться подключения: создаём с ними экземпляр класса, а дальше он сам рулит}
		constructor Create(Settings: TConnectionSettings; Progress: TTCProgress = nil; Logger: TTCLogger = nil);
		destructor Destroy; override;
		function get(ThreadId: Cardinal): TCloudMailRuHTTP;

		property ConnectionSettings: TConnectionSettings read FConnectionSettings;
		property ProxyPassword: WideString write FConnectionSettings.ProxySettings.Password;
	end;

implementation

{THTTPManager}

constructor THTTPManager.Create(Settings: TConnectionSettings; Progress: TTCProgress = nil; Logger: TTCLogger = nil);
begin
	self.FConnectionSettings := Settings;
	self.FProgress := Progress;
	if not Assigned(self.FProgress) then
		self.FProgress := TTCProgress.Create;

	self.FLogger := Logger;
	if not Assigned(self.FLogger) then
		self.FLogger := TTCLogger.Create;
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
		result := TCloudMailRuHTTP.Create(FConnectionSettings, FProgress, FLogger);
		Connections.AddOrSetValue(ThreadId, result);
	end;
end;

end.
