unit IOperationStatusContextBuilderInterface;

{Interface for building operation status context.
 Encapsulates context construction for FsStatusInfo lifecycle handling.}

interface

uses
	RealPath,
	IOperationLifecycleInterface;

type
	IOperationStatusContextBuilder = interface
		['{48CE30A2-FE40-4BE4-AFA5-B137818F802E}']

		{Builds operation context from path and operation type.
		 @param Path Parsed real path
		 @param Operation Operation type constant
		 @return Fully populated TOperationContext}
		function BuildContext(const Path: TRealPath; Operation: Integer): TOperationContext;
	end;

implementation

end.
