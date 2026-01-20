unit MockHTTPManager;

{Mock HTTP manager for testing - allows injecting mock HTTP connections.}

interface

uses
	ICloudHTTPInterface,
	HTTPManager,
	ConnectionSettings,
	System.Generics.Collections;

type
	{Mock HTTP manager that returns pre-configured ICloudHTTP instances}
	TMockHTTPManager = class(TInterfacedObject, IHTTPManager)
	private
		FMockHTTP: ICloudHTTP;
		FConnectionSettings: TConnectionSettings;
	public
		constructor Create(MockHTTP: ICloudHTTP);

		{IHTTPManager implementation}
		function Get(ThreadId: Cardinal): ICloudHTTP;
		function GetConnectionSettings: TConnectionSettings;
		procedure SetProxyPassword(Password: WideString);

		{Additional methods for testing}
		procedure SetMockHTTP(MockHTTP: ICloudHTTP);
		procedure SetConnectionSettings(Settings: TConnectionSettings);
	end;

implementation

{TMockHTTPManager}

constructor TMockHTTPManager.Create(MockHTTP: ICloudHTTP);
begin
	inherited Create;
	FMockHTTP := MockHTTP;
	FConnectionSettings := Default(TConnectionSettings);
end;

function TMockHTTPManager.Get(ThreadId: Cardinal): ICloudHTTP;
begin
	Result := FMockHTTP;
end;

function TMockHTTPManager.GetConnectionSettings: TConnectionSettings;
begin
	Result := FConnectionSettings;
end;

procedure TMockHTTPManager.SetProxyPassword(Password: WideString);
begin
	FConnectionSettings.ProxySettings.Password := Password;
end;

procedure TMockHTTPManager.SetMockHTTP(MockHTTP: ICloudHTTP);
begin
	FMockHTTP := MockHTTP;
end;

procedure TMockHTTPManager.SetConnectionSettings(Settings: TConnectionSettings);
begin
	FConnectionSettings := Settings;
end;

end.
