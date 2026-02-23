unit ServerConfigFetcher;

{Fetches server endpoint configuration via HTTP GET to the server root URL.
	Used by the Servers tab Self-Configure button to auto-populate
	endpoint fields from a remote server's self-description endpoint.}

interface

uses
	CloudEndpoints;

type
	IServerConfigFetcher = interface
		['{7399F029-36D5-455C-832F-F46DA4504CBC}']
		{Fetches endpoint configuration from the server root URL.
			On success, returns True and populates Endpoints.
			On failure, returns False and sets ErrorMsg.}
		function Fetch(const ServerUrl: WideString; var Endpoints: TCloudEndpoints; var ErrorMsg: WideString): Boolean;
	end;

	{Production implementation -- HTTP GET to server root, fallback to URL inference}
	TServerConfigFetcher = class(TInterfacedObject, IServerConfigFetcher)
	public
		function Fetch(const ServerUrl: WideString; var Endpoints: TCloudEndpoints; var ErrorMsg: WideString): Boolean;
	end;

	{Null implementation for testing -- always fails}
	TNullServerConfigFetcher = class(TInterfacedObject, IServerConfigFetcher)
	public
		function Fetch(const ServerUrl: WideString; var Endpoints: TCloudEndpoints; var ErrorMsg: WideString): Boolean;
	end;

implementation

uses
	SysUtils,
	StrUtils,
	Classes,
	IdHTTP,
	IdSSLOpenSSL,
	CloudEndpointsJsonAdapter,
	ServerProfileManager;

{TServerConfigFetcher}

function TServerConfigFetcher.Fetch(const ServerUrl: WideString; var Endpoints: TCloudEndpoints; var ErrorMsg: WideString): Boolean;
var
	BaseUrl: WideString;
	HTTP: TIdHTTP;
	SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
	ResponseStream: TStringStream;
begin
	Result := True;
	ErrorMsg := '';

	if ServerUrl = '' then
	begin
		ErrorMsg := 'Server URL is required';
		Result := False;
		Exit;
	end;

	BaseUrl := ServerUrl;
	if Pos(string('://'), string(BaseUrl)) = 0 then
		BaseUrl := 'http://' + BaseUrl;
	if (Length(BaseUrl) > 0) and (BaseUrl[Length(BaseUrl)] = '/') then
		Delete(BaseUrl, Length(BaseUrl), 1);

	{Start with inferred endpoints as baseline}
	Endpoints := TServerProfileManager.InferEndpointsFromServerUrl(BaseUrl);

	{HTTP GET to server root for endpoint configuration}
	HTTP := TIdHTTP.Create(nil);
	try
		HTTP.ConnectTimeout := 5000;
		HTTP.ReadTimeout := 5000;

		if StartsText('https://', string(BaseUrl)) then
		begin
			SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(HTTP);
			HTTP.IOHandler := SSLHandler;
		end;

		ResponseStream := TStringStream.Create('', TEncoding.UTF8);
		try
			try
				HTTP.Get(string(BaseUrl) + '/', ResponseStream);
				{Merge server-provided endpoints over inferred baseline}
				TCloudEndpointsJsonAdapter.Merge(ResponseStream.DataString, Endpoints);
			except
				on E: Exception do
					begin
						ErrorMsg := E.Message;
						Result := False;
					end;
			end;
		finally
			ResponseStream.Free;
		end;
	finally
		HTTP.Free;
	end;
end;

{TNullServerConfigFetcher}

function TNullServerConfigFetcher.Fetch(const ServerUrl: WideString; var Endpoints: TCloudEndpoints; var ErrorMsg: WideString): Boolean;
begin
	Result := False;
	ErrorMsg := 'Self-configure not available';
end;

end.
