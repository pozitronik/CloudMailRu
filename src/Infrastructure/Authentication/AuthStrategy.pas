unit AuthStrategy;

{Authentication strategy interface for cloud services.
	Implements Strategy pattern to decouple authentication logic from cloud operations.}

interface

uses
	CloudOAuth,
	CloudHTTP,
	Logger;

type
	{Result of authentication attempt}
	TAuthResult = record
		Success: Boolean;
		AuthToken: WideString; {CSRF token or OAuth access_token}
		OAuthToken: TCloudOAuth; {Full OAuth response structure}
		UnitedParams: WideString; {Formatted API request parameters string}
		ErrorMessage: WideString; {Error details if authentication failed}
		class function CreateFailure(const Error: WideString): TAuthResult; static;
		class function CreateOAuthSuccess(const OAuth: TCloudOAuth): TAuthResult; static;
	end;

	{Authentication credentials container}
	TAuthCredentials = record
		Email: WideString;
		Password: WideString;
		User: WideString; {Username part of email (before @)}
		Domain: WideString; {Domain part of email (after @)}
		class function Create(const AEmail, APassword, AUser, ADomain: WideString): TAuthCredentials; static;
	end;

	{Strategy interface for cloud authentication.
		Each implementation handles a specific authentication method.}
	IAuthStrategy = interface
		['{A5774E58-9B80-4B49-BCE4-D874248F3DA2}']
		{Performs authentication and returns result.
			@param Credentials User credentials for authentication
			@param HTTP HTTP connection for making requests
			@param Logger Logger for diagnostic output
			@return TAuthResult with success/failure and tokens}
		function Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
		{Returns human-readable strategy name for logging.}
		function GetName: WideString;
	end;

	{Null implementation for testing - always returns failure.
		Use in unit tests where authentication behavior needs to be controlled.}
	TNullAuthStrategy = class(TInterfacedObject, IAuthStrategy)
	public
		function Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
		function GetName: WideString;
	end;

	{Factory interface for creating authentication strategies.
		Enables dependency injection and testability of components that need auth strategies.}
	IAuthStrategyFactory = interface
		['{B8E4F6A3-7D2C-4E5B-9A1F-3C6D8E0B4F2A}']
		{Creates the default authentication strategy for regular cloud accounts.}
		function CreateDefaultStrategy: IAuthStrategy;
	end;

	{Null factory for testing - always returns TNullAuthStrategy.}
	TNullAuthStrategyFactory = class(TInterfacedObject, IAuthStrategyFactory)
	public
		function CreateDefaultStrategy: IAuthStrategy;
	end;

implementation

uses
	SysUtils;

{TAuthResult}

class function TAuthResult.CreateFailure(const Error: WideString): TAuthResult;
begin
	Result := Default (TAuthResult);
	Result.Success := False;
	Result.ErrorMessage := Error;
end;

class function TAuthResult.CreateOAuthSuccess(const OAuth: TCloudOAuth): TAuthResult;
begin
	Result := Default (TAuthResult);
	Result.Success := True;
	Result.OAuthToken := OAuth;
	Result.AuthToken := OAuth.access_token;
	Result.UnitedParams := Format('access_token=%s', [OAuth.access_token]);
end;

{TAuthCredentials}

class function TAuthCredentials.Create(const AEmail, APassword, AUser, ADomain: WideString): TAuthCredentials;
begin
	Result := Default (TAuthCredentials);
	Result.Email := AEmail;
	Result.Password := APassword;
	Result.User := AUser;
	Result.Domain := ADomain;
end;

{TNullAuthStrategy}

function TNullAuthStrategy.Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
begin
	Result := TAuthResult.CreateFailure('Null authentication strategy - always fails');
end;

function TNullAuthStrategy.GetName: WideString;
begin
	Result := 'Null';
end;

{TNullAuthStrategyFactory}

function TNullAuthStrategyFactory.CreateDefaultStrategy: IAuthStrategy;
begin
	Result := TNullAuthStrategy.Create;
end;

end.
