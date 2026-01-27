unit OAuthAppAuthStrategy;

{OAuth authentication using app password - the recommended method.
	This strategy authenticates using the OAuth password grant with an app-specific password.
	It is the only currently working authentication method after VK ID migration.}

interface

uses
	AuthStrategy,
	CloudOAuth,
	CloudHTTP,
	TCLogger;

type
	TOAuthAppAuthStrategy = class(TInterfacedObject, IAuthStrategy)
	public
		function Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
		function GetName: WideString;
	end;

	{Default factory that creates TOAuthAppAuthStrategy.
		Use this factory for production code to inject auth strategy via DI.}
	TDefaultAuthStrategyFactory = class(TInterfacedObject, IAuthStrategyFactory)
	public
		function CreateDefaultStrategy: IAuthStrategy;
	end;

implementation

uses
	SysUtils,
	Winapi.Windows,
	CloudConstants,
	CloudOAuthJsonAdapter,
	WFXTypes,
	LanguageStrings,
	StringHelper;

{TOAuthAppAuthStrategy}

function TOAuthAppAuthStrategy.Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
var
	OAuthToken: TCloudOAuth;
	PostAnswer: WideString;
begin
	Result := TAuthResult.CreateFailure('OAuth authentication failed');

	Logger.Log(LOG_LEVEL_DEBUG, msgtype_details, REQUESTING_OAUTH_TOKEN, [Credentials.Email]);

	{OAuth password grant request}
	if HTTP.PostForm(OAUTH_TOKEN_URL, Format('client_id=%s&grant_type=password&username=%s@%s&password=%s', [OAUTH_CLIENT_ID, Credentials.User, Credentials.Domain, UrlEncode(Credentials.Password)]), PostAnswer) then
	begin
		if not TCloudOAuthJsonAdapter.Parse(PostAnswer, OAuthToken) then
		begin
			Result := TAuthResult.CreateFailure(Format(PREFIX_ERR_OAUTH, ['JSON parse error', 'Invalid response format']));
			Exit;
		end;

		if OAuthToken.error_code <> NOERROR then
		begin
			Result := TAuthResult.CreateFailure(Format(PREFIX_ERR_OAUTH, [OAuthToken.error, OAuthToken.error_description]));
			Exit;
		end;

		{Success - build auth result}
		Result := TAuthResult.CreateOAuthSuccess(OAuthToken);
	end else begin
		Result := TAuthResult.CreateFailure(Format(PREFIX_ERR_OAUTH, ['HTTP error', 'Failed to connect to OAuth server']));
	end;
end;

function TOAuthAppAuthStrategy.GetName: WideString;
begin
	Result := 'OAuth App Password';
end;

{TDefaultAuthStrategyFactory}

function TDefaultAuthStrategyFactory.CreateDefaultStrategy: IAuthStrategy;
begin
	Result := TOAuthAppAuthStrategy.Create;
end;

end.
