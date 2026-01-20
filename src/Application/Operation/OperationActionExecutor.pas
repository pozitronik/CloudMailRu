unit OperationActionExecutor;

{Executes operation lifecycle actions determined by IOperationLifecycleHandler.
	Centralizes all side effects triggered by operation START/END events:
	retry counter management, skip list flags, background job tracking,
	description loading, and user space logging.}

interface

uses
	OperationLifecycleHandler,
	ThreadStateManager,
	PluginSettingsManager,
	TCLogger,
	ConnectionManager,
	Description,
	RealPath,
	CMRConstants,
	LANGUAGE_STRINGS,
	PLUGIN_TYPES;

type
	{Executes operation lifecycle actions determined by IOperationLifecycleHandler.
		Each action in the set triggers specific side effects (thread state changes,
		logging, description loading, etc.).}
	IOperationActionExecutor = interface
		['{8A3D1F92-E4B7-4C6A-9D5E-2F8B7A1C3E4D}']
		procedure Execute(Actions: TOperationActions; const RealPath: TRealPath; Operation: Integer);
	end;

	TOperationActionExecutor = class(TInterfacedObject, IOperationActionExecutor)
	private
		FThreadState: IThreadStateManager;
		FConnectionManager: IConnectionManager;
		FSettingsManager: IPluginSettingsManager;
		FDescriptions: TDescription;
		FLogger: ILogger;

		{Thread state actions - retry counters}
		procedure ExecuteRetryActions(Actions: TOperationActions);

		{Thread state actions - skip list flags}
		procedure ExecuteSkipListActions(Actions: TOperationActions);

		{Thread state actions - abort control}
		procedure ExecuteAbortControlActions(Actions: TOperationActions);

		{Thread state actions - skipped path management}
		procedure ExecuteSkippedPathActions(Actions: TOperationActions);

		{Thread state actions - background tracking}
		procedure ExecuteBackgroundActions(Actions: TOperationActions; const RealPath: TRealPath; Operation: Integer);

		{Cloud operations - user space logging}
		procedure ExecuteUserSpaceLogging(Actions: TOperationActions; const RealPath: TRealPath);

		{Cloud operations - description loading}
		procedure ExecuteDescriptionLoading(Actions: TOperationActions; const RealPath: TRealPath);

		{Warning actions}
		procedure ExecuteWarningActions(Actions: TOperationActions);
	public
		constructor Create(ThreadState: IThreadStateManager; AConnectionManager: IConnectionManager; SettingsManager: IPluginSettingsManager; Descriptions: TDescription; Logger: ILogger);

		procedure Execute(Actions: TOperationActions; const RealPath: TRealPath; Operation: Integer);
	end;

implementation

uses
	SysUtils;

constructor TOperationActionExecutor.Create(ThreadState: IThreadStateManager; AConnectionManager: IConnectionManager; SettingsManager: IPluginSettingsManager; Descriptions: TDescription; Logger: ILogger);
begin
	inherited Create;
	FThreadState := ThreadState;
	FConnectionManager := AConnectionManager;
	FSettingsManager := SettingsManager;
	FDescriptions := Descriptions;
	FLogger := Logger;
end;

procedure TOperationActionExecutor.ExecuteRetryActions(Actions: TOperationActions);
begin
	if oaResetRetryDownload in Actions then
		FThreadState.ResetRetryCountDownload;
	if oaResetRetryUpload in Actions then
		FThreadState.ResetRetryCountUpload;
	if oaResetRetryRenMov in Actions then
		FThreadState.ResetRetryCountRenMov;
end;

procedure TOperationActionExecutor.ExecuteSkipListActions(Actions: TOperationActions);
begin
	if oaSetSkipListDelete in Actions then
		FThreadState.SetSkipListDelete(True);
	if oaClearSkipListDelete in Actions then
		FThreadState.SetSkipListDelete(False);
	if oaSetSkipListRenMov in Actions then
		FThreadState.SetSkipListRenMov(True);
	if oaClearSkipListRenMov in Actions then
		FThreadState.SetSkipListRenMov(False);
end;

procedure TOperationActionExecutor.ExecuteAbortControlActions(Actions: TOperationActions);
begin
	if oaSetCanAbortRenMov in Actions then
		FThreadState.SetCanAbortRenMov(True);
	if oaClearCanAbortRenMov in Actions then
		FThreadState.SetCanAbortRenMov(False);
end;

procedure TOperationActionExecutor.ExecuteSkippedPathActions(Actions: TOperationActions);
begin
	if oaCreateSkippedPath in Actions then
		FThreadState.CreateRemoveDirSkippedPath;
	if oaClearSkippedPath in Actions then
		FThreadState.ClearRemoveDirSkippedPath;
end;

procedure TOperationActionExecutor.ExecuteBackgroundActions(Actions: TOperationActions; const RealPath: TRealPath; Operation: Integer);
begin
	if oaIncrementBackgroundJobs in Actions then
		FThreadState.IncrementBackgroundJobs(RealPath.account);
	if oaDecrementBackgroundJobs in Actions then
		FThreadState.DecrementBackgroundJobs(RealPath.account);

	if oaSetBackgroundThreadStatus in Actions then
		FThreadState.SetBackgroundThreadStatus(Operation);
	if oaRemoveBackgroundThread in Actions then
		FThreadState.RemoveBackgroundThread;
end;

procedure TOperationActionExecutor.ExecuteUserSpaceLogging(Actions: TOperationActions; const RealPath: TRealPath);
var
	getResult: Integer;
begin
	if oaLogUserSpaceInfo in Actions then
		FConnectionManager.Get(RealPath.account, getResult).logUserSpaceInfo;
end;

procedure TOperationActionExecutor.ExecuteDescriptionLoading(Actions: TOperationActions; const RealPath: TRealPath);
var
	getResult: Integer;
begin
	if oaLoadDescriptions in Actions then
	begin
		if FConnectionManager.Get(RealPath.account, getResult).getDescriptionFile(IncludeTrailingBackslash(RealPath.Path) + FSettingsManager.GetSettings.DescriptionFileName, FDescriptions.ionFilename) then
			FDescriptions.Read
		else
			FDescriptions.Clear;
	end;
end;

procedure TOperationActionExecutor.ExecuteWarningActions(Actions: TOperationActions);
begin
	if oaWarnPublicAccountCopy in Actions then
	begin
		FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, ERR_DIRECT_COPY_SUPPORT);
		FThreadState.SetSkipListRenMov(True);
	end;
end;

procedure TOperationActionExecutor.Execute(Actions: TOperationActions; const RealPath: TRealPath; Operation: Integer);
begin
	ExecuteRetryActions(Actions);
	ExecuteSkipListActions(Actions);
	ExecuteAbortControlActions(Actions);
	ExecuteSkippedPathActions(Actions);
	ExecuteBackgroundActions(Actions, RealPath, Operation);
	ExecuteUserSpaceLogging(Actions, RealPath);
	ExecuteDescriptionLoading(Actions, RealPath);
	ExecuteWarningActions(Actions);
end;

end.
