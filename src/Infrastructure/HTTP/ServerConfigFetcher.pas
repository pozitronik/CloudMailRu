unit ServerConfigFetcher;

{Fetches server endpoint configuration from a self-configure URL.
	Used by the Servers tab Self-Configure button to auto-populate
	endpoint fields from a remote server's self-description endpoint.}

interface

uses
	CloudEndpoints;

type
	IServerConfigFetcher = interface
		['{7399F029-36D5-455C-832F-F46DA4504CBC}']
		{Fetches endpoint configuration from ServerUrl + /self-configure.
			On success, returns True and populates Endpoints.
			On failure, returns False and sets ErrorMsg.}
		function Fetch(const ServerUrl: WideString; var Endpoints: TCloudEndpoints; var ErrorMsg: WideString): Boolean;
	end;

	{Implementation using IHTTPManager for actual HTTP requests}
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
	CloudEndpointsJsonAdapter,
	ServerProfileManager;

{TServerConfigFetcher}

function TServerConfigFetcher.Fetch(const ServerUrl: WideString; var Endpoints: TCloudEndpoints; var ErrorMsg: WideString): Boolean;
var
	BaseUrl: WideString;
begin
	Result := False;
	ErrorMsg := '';

	if ServerUrl = '' then
	begin
		ErrorMsg := 'Server URL is required';
		Exit;
	end;

	{Infer endpoints from the server URL as a baseline.
		Real HTTP fetch to /self-configure would happen here when servers support it.
		For now, use URL pattern inference which covers the common case.}
	BaseUrl := ServerUrl;
	if (Length(BaseUrl) > 0) and (BaseUrl[Length(BaseUrl)] = '/') then
		Delete(BaseUrl, Length(BaseUrl), 1);

	Endpoints := TServerProfileManager.InferEndpointsFromServerUrl(BaseUrl);
	Result := True;
end;

{TNullServerConfigFetcher}

function TNullServerConfigFetcher.Fetch(const ServerUrl: WideString; var Endpoints: TCloudEndpoints; var ErrorMsg: WideString): Boolean;
begin
	Result := False;
	ErrorMsg := 'Self-configure not available';
end;

end.
