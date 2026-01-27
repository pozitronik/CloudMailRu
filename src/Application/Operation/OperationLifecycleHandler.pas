unit OperationLifecycleHandler;

{Determines what actions to perform for operation START/END events.
	Maps TC operation codes to discrete actions.
	Separates action determination from execution (IOperationActionExecutor).}

interface

uses
	WFXTypes;

type
	{Discrete actions that can be performed during operation lifecycle}
	TOperationAction = (oaResetRetryDownload, oaResetRetryUpload, oaResetRetryRenMov, oaSetSkipListDelete, oaClearSkipListDelete, oaSetSkipListRenMov, oaClearSkipListRenMov, oaSetCanAbortRenMov, oaClearCanAbortRenMov, oaCreateSkippedPath, oaClearSkippedPath, oaIncrementBackgroundJobs, oaDecrementBackgroundJobs, oaSetBackgroundThreadStatus, oaRemoveBackgroundThread, oaLogUserSpaceInfo, oaLoadDescriptions, oaWarnPublicAccountCopy);

	TOperationActions = set of TOperationAction;

	{Context for determining what actions to perform}
	TOperationContext = record
		Operation: Integer;
		IsPublicAccount: Boolean;
		IsInAccount: Boolean;
		DescriptionsEnabled: Boolean;
		LogUserSpaceEnabled: Boolean;
	end;

	{Determines what actions to perform for operation START/END events.
		Separates action determination from execution.}
	IOperationLifecycleHandler = interface
		['{1F5B2387-0B3E-44F3-B84B-7C5EA6A686DF}']
		function GetStartActions(const Context: TOperationContext): TOperationActions;
		function GetEndActions(const Context: TOperationContext): TOperationActions;
	end;

	{Determines what actions to perform for operation START/END events.
		Maps TC operation codes to discrete actions.}
	TOperationLifecycleHandler = class(TInterfacedObject, IOperationLifecycleHandler)
	public
		function GetStartActions(const Context: TOperationContext): TOperationActions;
		function GetEndActions(const Context: TOperationContext): TOperationActions;
	end;

implementation

function TOperationLifecycleHandler.GetStartActions(const Context: TOperationContext): TOperationActions;
begin
	Result := [];

	case Context.Operation of
		FS_STATUS_OP_LIST:
			if Context.DescriptionsEnabled and Context.IsInAccount then
				Include(Result, oaLoadDescriptions);

		FS_STATUS_OP_GET_SINGLE, FS_STATUS_OP_GET_MULTI:
			Include(Result, oaResetRetryDownload);

		FS_STATUS_OP_PUT_SINGLE, FS_STATUS_OP_PUT_MULTI:
			Include(Result, oaResetRetryUpload);

		FS_STATUS_OP_RENMOV_MULTI:
			begin
				if Context.IsPublicAccount then
					Include(Result, oaWarnPublicAccountCopy);
				Result := Result + [oaResetRetryRenMov, oaSetCanAbortRenMov, oaCreateSkippedPath];
			end;

		FS_STATUS_OP_DELETE:
			Include(Result, oaSetSkipListDelete);

		FS_STATUS_OP_GET_MULTI_THREAD:
			Result := [oaResetRetryDownload, oaIncrementBackgroundJobs, oaSetBackgroundThreadStatus];

		FS_STATUS_OP_PUT_MULTI_THREAD:
			Result := [oaResetRetryUpload, oaIncrementBackgroundJobs, oaSetBackgroundThreadStatus];
	end;
end;

function TOperationLifecycleHandler.GetEndActions(const Context: TOperationContext): TOperationActions;

	procedure AddLogUserSpaceIfEnabled;
	begin
		if Context.IsInAccount and Context.LogUserSpaceEnabled then
			Include(Result, oaLogUserSpaceInfo);
	end;

begin
	Result := [];

	case Context.Operation of
		FS_STATUS_OP_PUT_SINGLE, FS_STATUS_OP_PUT_MULTI, FS_STATUS_OP_RENMOV_SINGLE, FS_STATUS_OP_SYNC_GET, FS_STATUS_OP_SYNC_PUT, FS_STATUS_OP_SYNC_DELETE:
			AddLogUserSpaceIfEnabled;

		FS_STATUS_OP_RENMOV_MULTI:
			begin
				Result := [oaClearSkipListRenMov, oaClearCanAbortRenMov, oaClearSkippedPath];
				AddLogUserSpaceIfEnabled;
			end;

		FS_STATUS_OP_DELETE:
			begin
				Include(Result, oaClearSkipListDelete);
				AddLogUserSpaceIfEnabled;
			end;

		FS_STATUS_OP_GET_MULTI_THREAD:
			begin
				Result := [oaDecrementBackgroundJobs, oaRemoveBackgroundThread];
				AddLogUserSpaceIfEnabled;
			end;

		FS_STATUS_OP_PUT_MULTI_THREAD:
			begin
				Result := [oaDecrementBackgroundJobs, oaRemoveBackgroundThread];
				AddLogUserSpaceIfEnabled;
			end;
	end;
end;

end.
