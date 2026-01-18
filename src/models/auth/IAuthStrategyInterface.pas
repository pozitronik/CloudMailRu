unit IAuthStrategyInterface;

{Authentication strategy interface for cloud services.
 Implements Strategy pattern to decouple authentication logic from cloud operations.}

interface

uses
	CMROAuth,
	ICloudHTTPInterface,
	ILoggerInterface;

type
	{Result of authentication attempt}
	TAuthResult = record
		Success: Boolean;
		AuthToken: WideString;      {CSRF token or OAuth access_token}
		OAuthToken: TCMROAuth;      {Full OAuth response structure}
		UnitedParams: WideString;   {Formatted API request parameters string}
		ErrorMessage: WideString;   {Error details if authentication failed}
		PublicShard: WideString;    {Public shard URL for shared account auth}
		PublicLink: WideString;     {Extracted public link for shared accounts}
		class function CreateSuccess(const Token, Params: WideString): TAuthResult; static;
		class function CreateFailure(const Error: WideString): TAuthResult; static;
		class function CreateOAuthSuccess(const OAuth: TCMROAuth): TAuthResult; static;
		class function CreateSharedSuccess(const Shard, Link: WideString): TAuthResult; static;
	end;

	{Authentication credentials container}
	TAuthCredentials = record
		Email: WideString;
		Password: WideString;
		User: WideString;     {Username part of email (before @)}
		Domain: WideString;   {Domain part of email (after @)}
		PublicUrl: WideString; {For shared account authentication}
		class function Create(const AEmail, APassword, AUser, ADomain: WideString): TAuthCredentials; overload; static;
		class function CreatePublic(const APublicUrl: WideString): TAuthCredentials; overload; static;
	end;

	{Strategy interface for cloud authentication.
	 Each implementation handles a specific authentication method.}
	IAuthStrategy = interface
		['{8A7F2C3E-5B1D-4E9A-B6C8-D2F4A3E1B0C7}']
		{Performs authentication and returns result.
		 @param Credentials User credentials for authentication
		 @param HTTP HTTP connection for making requests
		 @param Logger Logger for diagnostic output
		 @return TAuthResult with success/failure and tokens}
		function Authenticate(const Credentials: TAuthCredentials; 	HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
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

implementation

uses
	SysUtils;

{TAuthResult}

class function TAuthResult.CreateSuccess(const Token, Params: WideString): TAuthResult;
begin
	Result := Default(TAuthResult);
	Result.Success := True;
	Result.AuthToken := Token;
	Result.UnitedParams := Params;
end;

class function TAuthResult.CreateFailure(const Error: WideString): TAuthResult;
begin
	Result := Default(TAuthResult);
	Result.Success := False;
	Result.ErrorMessage := Error;
end;

class function TAuthResult.CreateOAuthSuccess(const OAuth: TCMROAuth): TAuthResult;
begin
	Result := Default(TAuthResult);
	Result.Success := True;
	Result.OAuthToken := OAuth;
	Result.AuthToken := OAuth.access_token;
	Result.UnitedParams := Format('access_token=%s', [OAuth.access_token]);
end;

class function TAuthResult.CreateSharedSuccess(const Shard, Link: WideString): TAuthResult;
begin
	Result := Default(TAuthResult);
	Result.Success := True;
	Result.PublicShard := Shard;
	Result.PublicLink := Link;
end;

{TAuthCredentials}

class function TAuthCredentials.Create(const AEmail, APassword, AUser, ADomain: WideString): TAuthCredentials;
begin
	Result := Default(TAuthCredentials);
	Result.Email := AEmail;
	Result.Password := APassword;
	Result.User := AUser;
	Result.Domain := ADomain;
end;

class function TAuthCredentials.CreatePublic(const APublicUrl: WideString): TAuthCredentials;
begin
	Result := Default(TAuthCredentials);
	Result.PublicUrl := APublicUrl;
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

end.
