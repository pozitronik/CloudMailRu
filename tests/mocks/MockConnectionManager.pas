unit MockConnectionManager;

{Mock implementation of IConnectionManager for testing.
 Allows tests to configure cloud instances to return.}

interface

uses
	IConnectionManagerInterface,
	CloudMailRu,
	CMRConstants,
	System.Generics.Collections;

type
	TMockConnectionManager = class(TInterfacedObject, IConnectionManager)
	private
		FCloudInstances: TDictionary<WideString, TCloudMailRu>;
		FGetCallCount: Integer;
		FFreeCallCount: Integer;
	public
		constructor Create;
		destructor Destroy; override;

		{Configure cloud instance for account}
		procedure SetCloud(const AccountName: WideString; Cloud: TCloudMailRu);

		{IConnectionManager implementation}
		function Get(ConnectionName: WideString; var OperationResult: Integer): TCloudMailRu;
		procedure Free(ConnectionName: WideString);

		{Test inspection}
		property GetCallCount: Integer read FGetCallCount;
		property FreeCallCount: Integer read FFreeCallCount;
	end;

implementation

constructor TMockConnectionManager.Create;
begin
	inherited Create;
	FCloudInstances := TDictionary<WideString, TCloudMailRu>.Create;
	FGetCallCount := 0;
	FFreeCallCount := 0;
end;

destructor TMockConnectionManager.Destroy;
begin
	FCloudInstances.Free;
	inherited;
end;

procedure TMockConnectionManager.SetCloud(const AccountName: WideString; Cloud: TCloudMailRu);
begin
	FCloudInstances.AddOrSetValue(AccountName, Cloud);
end;

function TMockConnectionManager.Get(ConnectionName: WideString; var OperationResult: Integer): TCloudMailRu;
begin
	Inc(FGetCallCount);
	OperationResult := CLOUD_OPERATION_OK;

	if not FCloudInstances.TryGetValue(ConnectionName, Result) then
		Result := nil;
end;

procedure TMockConnectionManager.Free(ConnectionName: WideString);
begin
	Inc(FFreeCallCount);
	FCloudInstances.Remove(ConnectionName);
end;

end.
