unit OldOAuthStrategy;

{DEPRECATED: Old OAuth method - incomplete implementation.
	This strategy is preserved for historical reference only.
	Use TOAuthAppAuthStrategy for production authentication.}

interface

uses
	AuthStrategy,
	CloudHTTP,
	TCLogger;

type
	TOldOAuthStrategy = class(TInterfacedObject, IAuthStrategy)
	public
		function Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
		function GetName: WideString;
	end;

implementation

uses
	SysUtils,
	CloudConstants,
	CloudOAuth,
	CloudOAuthJsonAdapter,
	WFXTypes,
	LanguageStrings;

{Helper function to get OAuth token - extracted from CloudMailRu.pas}
function GetOAuthToken(HTTP: ICloudHTTP; const Email, Password: WideString; out OAuthToken: TCloudOAuth): Boolean;
var
	Answer: WideString;
	PostData: WideString;
begin
	OAuthToken := Default (TCloudOAuth);

	PostData := Format('client_id=%s&grant_type=password&username=%s&password=%s', [OAUTH_CLIENT_ID, Email, Password]);

	Result := HTTP.PostForm(OAUTH_TOKEN_URL, PostData, Answer);
	if Result then
		Result := TCloudOAuthJsonAdapter.Parse(Answer, OAuthToken);
end;

{TOldOAuthStrategy}

function TOldOAuthStrategy.Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
var
	OAuthToken: TCloudOAuth;
begin
	Result := TAuthResult.CreateFailure('Old OAuth authentication failed');

	if (Credentials.Email = '') or (Credentials.Password = '') then
	begin
		Result := TAuthResult.CreateFailure('Email and password are required');
		Exit;
	end;

	{Attempt OAuth token request - note: this implementation is incomplete}
	if GetOAuthToken(HTTP, Credentials.Email, Credentials.Password, OAuthToken) then
	begin
		{Old OAuth didn't properly set up UnitedParams, making it incomplete}
		Result := TAuthResult.CreateOAuthSuccess(OAuthToken);
	end else begin
		Logger.Log(LOG_LEVEL_ERROR, msgtype_importanterror, PREFIX_ERR_OAUTH, [OAuthToken.error, OAuthToken.error_description]);
		Result := TAuthResult.CreateFailure(Format('%s: %s', [OAuthToken.error, OAuthToken.error_description]));
	end;
end;

function TOldOAuthStrategy.GetName: WideString;
begin
	Result := 'Old OAuth (Deprecated)';
end;

end.
