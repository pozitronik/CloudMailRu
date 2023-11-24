unit HTTPManager;

interface

uses
	CloudMailRuHTTP,
	CMRConstants,
	PLUGIN_TYPES,
	Settings,
	TCLogger,
	System.Generics.Collections,
	SysUtils,
	IdCookieManager;

type

	THTTPManager = class
	private
		ConnectionSettings: TConnectionSettings;
		ExternalProgressProc: TProgressHandler;
		Logger: TTCLogger;

		Connections: TDictionary<Cardinal, TCloudMailRuHTTP>; //<ThreadId, HTTP>

	public
		{Параметры, с которыми будут отдаваться подключения: создаём с ними экземпляр класса, а дальше он сам рулит}
		constructor Create(Settings: TConnectionSettings; ExternalProgressProc: TProgressHandler = nil; Logger: TTCLogger = nil);
		destructor Destroy; override;
		function get(ThreadId: Cardinal): TCloudMailRuHTTP;
	end;

implementation

{THTTPManager}

constructor THTTPManager.Create(Settings: TConnectionSettings; ExternalProgressProc: TProgressHandler = nil; Logger: TTCLogger = nil);
begin
	self.ConnectionSettings := Settings;
	self.ExternalProgressProc := ExternalProgressProc;
	self.Logger := Logger;
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
		result := TCloudMailRuHTTP.Create(ConnectionSettings, ExternalProgressProc, Logger);
		Connections.AddOrSetValue(ThreadId, result);
	end;
end;

end.
