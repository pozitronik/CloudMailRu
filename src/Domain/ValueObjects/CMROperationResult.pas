unit CMROperationResult;

interface

uses
	CMRConstants;

type
	TCMROperationResult = record
		OperationStatus: Integer; {HTTP Code}
		OperationResult: Integer; {error code}

		function ToBoolean: Boolean;
	end;

implementation

function TCMROperationResult.ToBoolean: Boolean;
begin
	Result := self.OperationResult = CLOUD_OPERATION_OK
end;

end.
