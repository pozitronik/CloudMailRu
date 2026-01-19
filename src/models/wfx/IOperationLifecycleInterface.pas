unit IOperationLifecycleInterface;

interface

type
	{ Discrete actions that can be performed during operation lifecycle }
	TOperationAction = (
		oaResetRetryDownload,
		oaResetRetryUpload,
		oaResetRetryRenMov,
		oaSetSkipListDelete,
		oaClearSkipListDelete,
		oaSetSkipListRenMov,
		oaClearSkipListRenMov,
		oaSetCanAbortRenMov,
		oaClearCanAbortRenMov,
		oaCreateSkippedPath,
		oaClearSkippedPath,
		oaIncrementBackgroundJobs,
		oaDecrementBackgroundJobs,
		oaSetBackgroundThreadStatus,
		oaRemoveBackgroundThread,
		oaLogUserSpaceInfo,
		oaLoadDescriptions,
		oaWarnPublicAccountCopy
	);

	TOperationActions = set of TOperationAction;

	{ Context for determining what actions to perform }
	TOperationContext = record
		Operation: Integer;
		IsPublicAccount: Boolean;
		IsInAccount: Boolean;
		DescriptionsEnabled: Boolean;
		LogUserSpaceEnabled: Boolean;
	end;

	{ Determines what actions to perform for operation START/END events.
	  Separates action determination from execution. }
	IOperationLifecycleHandler = interface
		['{1F5B2387-0B3E-44F3-B84B-7C5EA6A686DF}']
		function GetStartActions(const Context: TOperationContext): TOperationActions;
		function GetEndActions(const Context: TOperationContext): TOperationActions;
	end;

implementation

end.
