unit TestableCloudMailRu;

{Shared testable TCloudMailRu subclass for unit tests.
	Exposes protected fields needed for test setup. Tests requiring
	additional exposed methods should subclass this further.}

interface

uses
	CloudMailRu;

type
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
		procedure SetPublicLink(const Value: WideString);
		procedure SetAuthorized;
		procedure SetCookieBasedAuth(Value: Boolean);
		procedure SetAuthToken(const Value: WideString);
		procedure SetOAuthAccessToken(const Value: WideString);
		function GetAuthToken: WideString;
		function GetOAuthAccessTokenDirect: WideString;
		function GetUnitedParamsDirect: WideString;
		function GetCookieBasedAuth: Boolean;
	end;

implementation

uses
	CloudAuthorizationState;

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

procedure TTestableCloudMailRu.SetPublicLink(const Value: WideString);
begin
	FPublicLink := Value;
end;

procedure TTestableCloudMailRu.SetAuthorized;
begin
	SetAuthorizationState(asAuthorized);
end;

procedure TTestableCloudMailRu.SetCookieBasedAuth(Value: Boolean);
begin
	FCookieBasedAuth := Value;
end;

procedure TTestableCloudMailRu.SetAuthToken(const Value: WideString);
begin
	FAuthToken := Value;
end;

procedure TTestableCloudMailRu.SetOAuthAccessToken(const Value: WideString);
begin
	FOAuthToken.access_token := Value;
end;

function TTestableCloudMailRu.GetAuthToken: WideString;
begin
	Result := FAuthToken;
end;

function TTestableCloudMailRu.GetOAuthAccessTokenDirect: WideString;
begin
	Result := FOAuthToken.access_token;
end;

function TTestableCloudMailRu.GetUnitedParamsDirect: WideString;
begin
	Result := FUnitedParams;
end;

function TTestableCloudMailRu.GetCookieBasedAuth: Boolean;
begin
	Result := FCookieBasedAuth;
end;

end.
