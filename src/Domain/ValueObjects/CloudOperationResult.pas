unit CloudOperationResult;

interface

uses
	CloudConstants;

type
	TCloudOperationResult = record
		OperationStatus: Integer; {HTTP Code}
		OperationResult: Integer; {error code}

		function ToBoolean: Boolean;
	end;

implementation

function TCloudOperationResult.ToBoolean: Boolean;
begin
	Result := self.OperationResult = CLOUD_OPERATION_OK
end;

end.
