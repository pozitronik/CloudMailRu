unit CloudMailRuOperationResult;

interface

type
	TCloudMailRuOperationResult = record
		OperationStatus: integer; //HTTP Code
		OperationResult: integer; //error code (mostly)
	end;

implementation

end.
