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

end.
