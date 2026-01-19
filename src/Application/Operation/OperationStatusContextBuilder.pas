unit OperationStatusContextBuilder;

{Builds operation status context for FsStatusInfo.
 Extracts context construction logic including public account detection.}

interface

uses
	OperationLifecycleHandler,
	IPluginSettingsManagerInterface,
	IConnectionManagerInterface,
	RealPath;

type
	IOperationStatusContextBuilder = interface
		['{48CE30A2-FE40-4BE4-AFA5-B137818F802E}']

		{Builds operation context from path and operation type.
		 @param Path Parsed real path
		 @param Operation Operation type constant
		 @return Fully populated TOperationContext}
		function BuildContext(const Path: TRealPath; Operation: Integer): TOperationContext;
	end;

	TOperationStatusContextBuilder = class(TInterfacedObject, IOperationStatusContextBuilder)
	private
		FSettingsManager: IPluginSettingsManager;
		FConnectionManager: IConnectionManager;
	public
		constructor Create(
			SettingsManager: IPluginSettingsManager;
			ConnectionManager: IConnectionManager);

		function BuildContext(const Path: TRealPath; Operation: Integer): TOperationContext;
	end;

implementation

constructor TOperationStatusContextBuilder.Create(
	SettingsManager: IPluginSettingsManager;
	ConnectionManager: IConnectionManager);
begin
	inherited Create;
	FSettingsManager := SettingsManager;
	FConnectionManager := ConnectionManager;
end;

function TOperationStatusContextBuilder.BuildContext(const Path: TRealPath;
	Operation: Integer): TOperationContext;
var
	getResult: Integer;
begin
	Result.Operation := Operation;
	Result.IsInAccount := Path.IsInAccount();
	Result.DescriptionsEnabled := FSettingsManager.GetSettings.DescriptionEnabled;
	Result.LogUserSpaceEnabled := FSettingsManager.GetSettings.LogUserSpace;

	{Public account check only needed when in account context}
	if Result.IsInAccount then
		Result.IsPublicAccount := FConnectionManager.Get(Path.account, getResult).IsPublicAccount
	else
		Result.IsPublicAccount := False;
end;

end.
