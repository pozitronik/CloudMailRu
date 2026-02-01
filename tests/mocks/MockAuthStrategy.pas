unit MockAuthStrategy;

{Configurable mock authentication strategy for testing TCloudMailRu login flows.
 Allows controlling authentication success/failure and returned tokens.}

interface

uses
	AuthStrategy,
	CloudHTTP,
	TCLogger,
	CloudOAuth,
	System.SysUtils;

type
	{Configurable mock authentication strategy}
	TMockAuthStrategy = class(TInterfacedObject, IAuthStrategy)
	private
		FShouldSucceed: Boolean;
		FAccessToken: WideString;
		FRefreshToken: WideString;
		FExpiresIn: Integer;
		FErrorMessage: WideString;
		FUnitedParams: WideString;
		FAuthenticateCalled: Boolean;
		FLastCredentials: TAuthCredentials;
		FCallCount: Integer;
	public
		{Create a mock that succeeds with specified tokens}
		constructor CreateSuccess(const AccessToken: WideString;
			const RefreshToken: WideString = ''; const ExpiresIn: Integer = 3600);

		{Create a mock that fails with specified error}
		constructor CreateFailure(const ErrorMessage: WideString = 'Mock authentication failed');

		{Create a mock that succeeds with full OAuth configuration}
		constructor CreateOAuthSuccess(const AccessToken, RefreshToken: WideString; ExpiresIn: Integer);

		{IAuthStrategy implementation}
		function Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP;
			Logger: ILogger): TAuthResult;
		function GetName: WideString;

		{Inspection - verify authentication was called}
		property AuthenticateCalled: Boolean read FAuthenticateCalled;
		property LastCredentials: TAuthCredentials read FLastCredentials;
		property CallCount: Integer read FCallCount;

		{Configuration - can change behavior after creation}
		procedure SetSucceed(Value: Boolean);
		procedure SetAccessToken(const Value: WideString);
		procedure SetErrorMessage(const Value: WideString);
		procedure SetUnitedParams(const Value: WideString);
		procedure Reset;
	end;

	{Sequence mock - returns different results on successive calls}
	TMockAuthStrategySequence = class(TInterfacedObject, IAuthStrategy)
	private
		FResults: array of TAuthResult;
		FCurrentIndex: Integer;
		FCallCount: Integer;
	public
		constructor Create;

		{Add results to the sequence}
		procedure AddSuccess(const AccessToken: WideString);
		procedure AddFailure(const ErrorMessage: WideString);
		procedure AddOAuthSuccess(const AccessToken, RefreshToken: WideString; ExpiresIn: Integer);

		{IAuthStrategy implementation}
		function Authenticate(const Credentials: TAuthCredentials; HTTP: ICloudHTTP;
			Logger: ILogger): TAuthResult;
		function GetName: WideString;

		property CallCount: Integer read FCallCount;
		procedure Reset;
	end;

implementation

{TMockAuthStrategy}

constructor TMockAuthStrategy.CreateSuccess(const AccessToken: WideString;
	const RefreshToken: WideString; const ExpiresIn: Integer);
begin
	inherited Create;
	FShouldSucceed := True;
	FAccessToken := AccessToken;
	FRefreshToken := RefreshToken;
	FAuthenticateCalled := False;
	FCallCount := 0;
end;

constructor TMockAuthStrategy.CreateFailure(const ErrorMessage: WideString);
begin
	inherited Create;
	FShouldSucceed := False;
	FErrorMessage := ErrorMessage;
	FAuthenticateCalled := False;
	FCallCount := 0;
end;

constructor TMockAuthStrategy.CreateOAuthSuccess(const AccessToken, RefreshToken: WideString;
	ExpiresIn: Integer);
begin
	inherited Create;
	FShouldSucceed := True;
	FAccessToken := AccessToken;
	FRefreshToken := RefreshToken;
	FExpiresIn := ExpiresIn;
	FAuthenticateCalled := False;
	FCallCount := 0;
end;

function TMockAuthStrategy.Authenticate(const Credentials: TAuthCredentials;
	HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
var
	OAuth: TCloudOAuth;
begin
	FAuthenticateCalled := True;
	FLastCredentials := Credentials;
	Inc(FCallCount);

	if not FShouldSucceed then
		Exit(TAuthResult.CreateFailure(FErrorMessage));

	{Return OAuth result}
	OAuth := Default(TCloudOAuth);
	OAuth.access_token := FAccessToken;
	OAuth.refresh_token := FRefreshToken;
	if FExpiresIn > 0 then
		OAuth.expires_in := FExpiresIn
	else
		OAuth.expires_in := 3600;

	Result := TAuthResult.CreateOAuthSuccess(OAuth);

	{Override UnitedParams if custom value was set}
	if FUnitedParams <> '' then
		Result.UnitedParams := FUnitedParams;
end;

function TMockAuthStrategy.GetName: WideString;
begin
	Result := 'Mock';
end;

procedure TMockAuthStrategy.SetSucceed(Value: Boolean);
begin
	FShouldSucceed := Value;
end;

procedure TMockAuthStrategy.SetAccessToken(const Value: WideString);
begin
	FAccessToken := Value;
end;

procedure TMockAuthStrategy.SetErrorMessage(const Value: WideString);
begin
	FErrorMessage := Value;
end;

procedure TMockAuthStrategy.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

procedure TMockAuthStrategy.Reset;
begin
	FAuthenticateCalled := False;
	FCallCount := 0;
end;

{TMockAuthStrategySequence}

constructor TMockAuthStrategySequence.Create;
begin
	inherited Create;
	SetLength(FResults, 0);
	FCurrentIndex := 0;
	FCallCount := 0;
end;

procedure TMockAuthStrategySequence.AddSuccess(const AccessToken: WideString);
var
	OAuth: TCloudOAuth;
begin
	SetLength(FResults, Length(FResults) + 1);
	OAuth := Default(TCloudOAuth);
	OAuth.access_token := AccessToken;
	FResults[High(FResults)] := TAuthResult.CreateOAuthSuccess(OAuth);
end;

procedure TMockAuthStrategySequence.AddFailure(const ErrorMessage: WideString);
begin
	SetLength(FResults, Length(FResults) + 1);
	FResults[High(FResults)] := TAuthResult.CreateFailure(ErrorMessage);
end;

procedure TMockAuthStrategySequence.AddOAuthSuccess(const AccessToken, RefreshToken: WideString;
	ExpiresIn: Integer);
var
	OAuth: TCloudOAuth;
begin
	SetLength(FResults, Length(FResults) + 1);
	OAuth := Default(TCloudOAuth);
	OAuth.access_token := AccessToken;
	OAuth.refresh_token := RefreshToken;
	OAuth.expires_in := ExpiresIn;
	FResults[High(FResults)] := TAuthResult.CreateOAuthSuccess(OAuth);
end;

function TMockAuthStrategySequence.Authenticate(const Credentials: TAuthCredentials;
	HTTP: ICloudHTTP; Logger: ILogger): TAuthResult;
begin
	Inc(FCallCount);

	if Length(FResults) = 0 then
		Exit(TAuthResult.CreateFailure('No results configured'));

	if FCurrentIndex >= Length(FResults) then
		{Return last result if sequence exhausted}
		Result := FResults[High(FResults)]
	else
	begin
		Result := FResults[FCurrentIndex];
		Inc(FCurrentIndex);
	end;
end;

function TMockAuthStrategySequence.GetName: WideString;
begin
	Result := 'MockSequence';
end;

procedure TMockAuthStrategySequence.Reset;
begin
	FCurrentIndex := 0;
	FCallCount := 0;
end;

end.
