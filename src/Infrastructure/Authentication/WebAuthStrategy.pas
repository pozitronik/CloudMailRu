unit WebAuthStrategy;

{DEPRECATED: Web form authentication - no longer works after VK ID migration.
	This strategy is preserved for historical reference only.
	Use TOAuthAppAuthStrategy for production authentication.}

interface

uses
	AuthStrategy,
	CloudHTTP,
	TCLogger;

type
	TWebAuthStrategy = class(TInterfacedObject, IAuthStrategy)
	public
		function Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
		function GetName: WideString;
	end;

implementation

uses
	SysUtils,
	DateUtils,
	CMRConstants,
	WFXTypes,
	LANGUAGE_STRINGS,
	ParsingHelper,
	StringHelper,
	HTTPManager;

{TWebAuthStrategy}

function TWebAuthStrategy.Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
var
	PostAnswer: WideString;
	TokenPageContent: WideString;
	AuthToken: WideString;
	x_page_id, build: WideString;
	UnitedParams: WideString;
	Progress: Boolean;
begin
	Result := TAuthResult.CreateFailure('Web authentication failed');

	if (Credentials.Email = '') or (Credentials.Password = '') then
	begin
		Result := TAuthResult.CreateFailure('Email and password are required');
		Exit;
	end;

	if (Credentials.User = '') or (Credentials.Domain = '') then
	begin
		Result := TAuthResult.CreateFailure('User and domain must be parsed from email');
		Exit;
	end;

	if Assigned(Logger) then
		Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, REQUESTING_AUTH_TOKEN, [Credentials.Email]);

	{Step 1: Post login form}
	if not HTTP.PostForm(LOGIN_URL, Format('page=https://cloud.mail.ru/?new_auth_form=1&Domain=%s&Login=%s&Password=%s&FailPage=', [Credentials.Domain, Credentials.User, UrlEncode(Credentials.Password)]), PostAnswer) then
	begin
		if Assigned(Logger) then
			Logger.Log(LOG_LEVEL_ERROR, msgtype_importanterror, ERR_GET_AUTH_TOKEN, [Credentials.Email]);
		Result := TAuthResult.CreateFailure('Failed to post login form');
		Exit;
	end;

	if Assigned(Logger) then
		Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, PARSING_TOKEN_DATA);

	{Step 2: Get token page to extract CSRF token and API params}
	Progress := False;
	if not HTTP.GetPage(TOKEN_HOME_URL, TokenPageContent, Progress) then
	begin
		Result := TAuthResult.CreateFailure('Failed to fetch token page');
		Exit;
	end;

	{Step 3: Extract token and parameters from page}
	if not(extractTokenFromText(TokenPageContent, AuthToken) and extract_x_page_id_FromText(TokenPageContent, x_page_id) and extract_build_FromText(TokenPageContent, build)) then
	begin
		if Assigned(Logger) then
			Logger.Log(LOG_LEVEL_ERROR, msgtype_importanterror, ERR_PARSING_AUTH_TOKEN, [Credentials.Email]);
		Result := TAuthResult.CreateFailure('Failed to parse authentication token');
		Exit;
	end;

	{Build united params string for API calls}
	UnitedParams := Format('api=2&build=%s&x-page-id=%s&email=%s@%s&x-email=%s@%s&_=%d810', [build, x_page_id, Credentials.User, Credentials.Domain, Credentials.User, Credentials.Domain, DateTimeToUnix(Now)]);

	Result := TAuthResult.CreateSuccess(AuthToken, UnitedParams);
end;

function TWebAuthStrategy.GetName: WideString;
begin
	Result := 'Web Form (Deprecated)';
end;

end.
