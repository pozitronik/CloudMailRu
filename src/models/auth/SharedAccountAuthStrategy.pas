unit SharedAccountAuthStrategy;

{Authentication for public/shared weblinks.
 This strategy doesn't require credentials - it fetches the public URL page
 and extracts the shard information needed to access shared content.}

interface

uses
	IAuthStrategyInterface,
	ICloudHTTPInterface,
	ILoggerInterface;

type
	TSharedAccountAuthStrategy = class(TInterfacedObject, IAuthStrategy)
	public
		function Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
		function GetName: WideString;
	end;

implementation

uses
	SysUtils,
	CMRConstants,
	PLUGIN_TYPES,
	LANGUAGE_STRINGS,
	ParsingHelper;

{Helper function to extract public link from URL}
function ExtractPublicLinkFromUrl(const PublicUrl: WideString): WideString;
var
	TempResult: String;
begin
	TempResult := String(PublicUrl);
	{Remove PUBLIC_ACCESS_URL prefix}
	if (TempResult <> '') and (Pos(String(PUBLIC_ACCESS_URL), TempResult) = 1) then
		Delete(TempResult, 1, Length(PUBLIC_ACCESS_URL));
	{Remove trailing slash}
	if (TempResult <> '') and (TempResult[Length(TempResult)] = '/') then
		Delete(TempResult, Length(TempResult), 1);
	Result := TempResult;
end;

{TSharedAccountAuthStrategy}

function TSharedAccountAuthStrategy.Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
var
	PageContent: WideString;
	PublicShard: WideString;
	PublicLink: WideString;
	Progress: Boolean;
begin
	Result := TAuthResult.CreateFailure('Shared account authentication failed');

	if Credentials.PublicUrl = '' then
	begin
		Result := TAuthResult.CreateFailure('Public URL is required for shared account authentication');
		Exit;
	end;

	if Assigned(Logger) then
		Logger.Log(LOG_LEVEL_DETAIL, msgtype_details, URL_OPEN, [Credentials.PublicUrl]);

	Progress := False;
	if HTTP.GetPage(Credentials.PublicUrl, PageContent, Progress) then
	begin
		if not extractPublicShard(PageContent, PublicShard) then
		begin
			if Assigned(Logger) then
				Logger.Log(LOG_LEVEL_ERROR, msgtype_importanterror, ERR_GET_PUBLIC_SHARE);
			Result := TAuthResult.CreateFailure('Failed to extract public shard from page');
			Exit;
		end;

		{Extract public link from URL}
		PublicLink := ExtractPublicLinkFromUrl(Credentials.PublicUrl);

		Result := TAuthResult.CreateSharedSuccess(PublicShard, PublicLink);
	end else begin
		Result := TAuthResult.CreateFailure('Failed to fetch public URL page');
	end;
end;

function TSharedAccountAuthStrategy.GetName: WideString;
begin
	Result := 'Shared Account';
end;

end.
