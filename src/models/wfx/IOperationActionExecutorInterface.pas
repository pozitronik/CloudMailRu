unit IOperationActionExecutorInterface;

{Interface for executing operation lifecycle actions.
 Separates action execution from determination (IOperationLifecycleHandler).
 Actions are executed based on flags in TOperationActions set.}

interface

uses
	IOperationLifecycleInterface,
	RealPath;

type
	{Executes operation lifecycle actions determined by IOperationLifecycleHandler.
	 Each action in the set triggers specific side effects (thread state changes,
	 logging, description loading, etc.).}
	IOperationActionExecutor = interface
		['{8A3D1F92-E4B7-4C6A-9D5E-2F8B7A1C3E4D}']
		procedure Execute(Actions: TOperationActions; const RealPath: TRealPath; Operation: Integer);
	end;

implementation

end.
