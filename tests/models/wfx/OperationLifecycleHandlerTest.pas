unit OperationLifecycleHandlerTest;

interface

uses
	SysUtils,
	PLUGIN_TYPES,
	IOperationLifecycleInterface, OperationLifecycleHandler,
	DUnitX.TestFramework;

type
	[TestFixture]
	TOperationLifecycleHandlerTest = class
	private
		FHandler: IOperationLifecycleHandler;
		function CreateContext(Operation: Integer): TOperationContext;
		function CreateContextInAccount(Operation: Integer): TOperationContext;
	public
		[Setup]
		procedure Setup;

		[TearDown]
		procedure TearDown;

		{ START - Download operations }
		[Test]
		procedure TestStart_GetSingle_ResetsRetryDownload;
		[Test]
		procedure TestStart_GetMulti_ResetsRetryDownload;
		[Test]
		procedure TestStart_GetMultiThread_ResetsRetryAndStartsBackground;

		{ START - Upload operations }
		[Test]
		procedure TestStart_PutSingle_ResetsRetryUpload;
		[Test]
		procedure TestStart_PutMulti_ResetsRetryUpload;
		[Test]
		procedure TestStart_PutMultiThread_ResetsRetryAndStartsBackground;

		{ START - RenMov operations }
		[Test]
		procedure TestStart_RenMovSingle_NoActions;
		[Test]
		procedure TestStart_RenMovMulti_SetsFlags;
		[Test]
		procedure TestStart_RenMovMulti_PublicAccount_WarnsAndSetsFlags;

		{ START - Delete operation }
		[Test]
		procedure TestStart_Delete_SetsSkipFlag;

		{ START - List operation }
		[Test]
		procedure TestStart_List_DescriptionsEnabled_LoadsDescriptions;
		[Test]
		procedure TestStart_List_DescriptionsDisabled_NoActions;
		[Test]
		procedure TestStart_List_NotInAccount_NoActions;

		{ START - No-op operations }
		[Test]
		procedure TestStart_Attrib_NoActions;
		[Test]
		procedure TestStart_MkDir_NoActions;
		[Test]
		procedure TestStart_Exec_NoActions;
		[Test]
		procedure TestStart_CalcSize_NoActions;
		[Test]
		procedure TestStart_Search_NoActions;

		{ END - Operations with LogUserSpace }
		[Test]
		procedure TestEnd_PutSingle_LogsUserSpace;
		[Test]
		procedure TestEnd_PutMulti_LogsUserSpace;
		[Test]
		procedure TestEnd_RenMovSingle_LogsUserSpace;
		[Test]
		procedure TestEnd_SyncGet_LogsUserSpace;
		[Test]
		procedure TestEnd_SyncPut_LogsUserSpace;
		[Test]
		procedure TestEnd_SyncDelete_LogsUserSpace;
		[Test]
		procedure TestEnd_LogUserSpaceDisabled_NoLog;
		[Test]
		procedure TestEnd_NotInAccount_NoLog;

		{ END - RenMov cleanup }
		[Test]
		procedure TestEnd_RenMovMulti_ClearsFlags;

		{ END - Delete cleanup }
		[Test]
		procedure TestEnd_Delete_ClearsSkipFlag;

		{ END - Background thread cleanup }
		[Test]
		procedure TestEnd_GetMultiThread_CleansUpBackground;
		[Test]
		procedure TestEnd_PutMultiThread_CleansUpBackground;

		{ END - No-op operations }
		[Test]
		procedure TestEnd_GetSingle_NoActions;
		[Test]
		procedure TestEnd_GetMulti_NoActions;
		[Test]
		procedure TestEnd_List_NoActions;
	end;

implementation

procedure TOperationLifecycleHandlerTest.Setup;
begin
	FHandler := TOperationLifecycleHandler.Create;
end;

procedure TOperationLifecycleHandlerTest.TearDown;
begin
	FHandler := nil;
end;

function TOperationLifecycleHandlerTest.CreateContext(Operation: Integer): TOperationContext;
begin
	Result.Operation := Operation;
	Result.IsPublicAccount := False;
	Result.IsInAccount := False;
	Result.DescriptionsEnabled := False;
	Result.LogUserSpaceEnabled := False;
end;

function TOperationLifecycleHandlerTest.CreateContextInAccount(Operation: Integer): TOperationContext;
begin
	Result := CreateContext(Operation);
	Result.IsInAccount := True;
	Result.LogUserSpaceEnabled := True;
end;

{ START - Download operations }

procedure TOperationLifecycleHandlerTest.TestStart_GetSingle_ResetsRetryDownload;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetStartActions(CreateContext(FS_STATUS_OP_GET_SINGLE));

	Assert.IsTrue(oaResetRetryDownload in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestStart_GetMulti_ResetsRetryDownload;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetStartActions(CreateContext(FS_STATUS_OP_GET_MULTI));

	Assert.IsTrue(oaResetRetryDownload in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestStart_GetMultiThread_ResetsRetryAndStartsBackground;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetStartActions(CreateContext(FS_STATUS_OP_GET_MULTI_THREAD));

	Assert.IsTrue(oaResetRetryDownload in Actions);
	Assert.IsTrue(oaIncrementBackgroundJobs in Actions);
	Assert.IsTrue(oaSetBackgroundThreadStatus in Actions);
end;

{ START - Upload operations }

procedure TOperationLifecycleHandlerTest.TestStart_PutSingle_ResetsRetryUpload;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetStartActions(CreateContext(FS_STATUS_OP_PUT_SINGLE));

	Assert.IsTrue(oaResetRetryUpload in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestStart_PutMulti_ResetsRetryUpload;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetStartActions(CreateContext(FS_STATUS_OP_PUT_MULTI));

	Assert.IsTrue(oaResetRetryUpload in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestStart_PutMultiThread_ResetsRetryAndStartsBackground;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetStartActions(CreateContext(FS_STATUS_OP_PUT_MULTI_THREAD));

	Assert.IsTrue(oaResetRetryUpload in Actions);
	Assert.IsTrue(oaIncrementBackgroundJobs in Actions);
	Assert.IsTrue(oaSetBackgroundThreadStatus in Actions);
end;

{ START - RenMov operations }

procedure TOperationLifecycleHandlerTest.TestStart_RenMovSingle_NoActions;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetStartActions(CreateContext(FS_STATUS_OP_RENMOV_SINGLE));

	Assert.IsTrue(Actions = []);
end;

procedure TOperationLifecycleHandlerTest.TestStart_RenMovMulti_SetsFlags;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetStartActions(CreateContext(FS_STATUS_OP_RENMOV_MULTI));

	Assert.IsTrue(oaResetRetryRenMov in Actions);
	Assert.IsTrue(oaSetCanAbortRenMov in Actions);
	Assert.IsTrue(oaCreateSkippedPath in Actions);
	Assert.IsFalse(oaWarnPublicAccountCopy in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestStart_RenMovMulti_PublicAccount_WarnsAndSetsFlags;
var
	Context: TOperationContext;
	Actions: TOperationActions;
begin
	Context := CreateContext(FS_STATUS_OP_RENMOV_MULTI);
	Context.IsPublicAccount := True;

	Actions := FHandler.GetStartActions(Context);

	Assert.IsTrue(oaWarnPublicAccountCopy in Actions);
	Assert.IsTrue(oaResetRetryRenMov in Actions);
	Assert.IsTrue(oaSetCanAbortRenMov in Actions);
	Assert.IsTrue(oaCreateSkippedPath in Actions);
end;

{ START - Delete operation }

procedure TOperationLifecycleHandlerTest.TestStart_Delete_SetsSkipFlag;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetStartActions(CreateContext(FS_STATUS_OP_DELETE));

	Assert.IsTrue(oaSetSkipListDelete in Actions);
end;

{ START - List operation }

procedure TOperationLifecycleHandlerTest.TestStart_List_DescriptionsEnabled_LoadsDescriptions;
var
	Context: TOperationContext;
	Actions: TOperationActions;
begin
	Context := CreateContext(FS_STATUS_OP_LIST);
	Context.DescriptionsEnabled := True;
	Context.IsInAccount := True;

	Actions := FHandler.GetStartActions(Context);

	Assert.IsTrue(oaLoadDescriptions in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestStart_List_DescriptionsDisabled_NoActions;
var
	Context: TOperationContext;
	Actions: TOperationActions;
begin
	Context := CreateContext(FS_STATUS_OP_LIST);
	Context.DescriptionsEnabled := False;
	Context.IsInAccount := True;

	Actions := FHandler.GetStartActions(Context);

	Assert.IsTrue(Actions = []);
end;

procedure TOperationLifecycleHandlerTest.TestStart_List_NotInAccount_NoActions;
var
	Context: TOperationContext;
	Actions: TOperationActions;
begin
	Context := CreateContext(FS_STATUS_OP_LIST);
	Context.DescriptionsEnabled := True;
	Context.IsInAccount := False;

	Actions := FHandler.GetStartActions(Context);

	Assert.IsTrue(Actions = []);
end;

{ START - No-op operations }

procedure TOperationLifecycleHandlerTest.TestStart_Attrib_NoActions;
begin
	Assert.IsTrue(FHandler.GetStartActions(CreateContext(FS_STATUS_OP_ATTRIB)) = []);
end;

procedure TOperationLifecycleHandlerTest.TestStart_MkDir_NoActions;
begin
	Assert.IsTrue(FHandler.GetStartActions(CreateContext(FS_STATUS_OP_MKDIR)) = []);
end;

procedure TOperationLifecycleHandlerTest.TestStart_Exec_NoActions;
begin
	Assert.IsTrue(FHandler.GetStartActions(CreateContext(FS_STATUS_OP_EXEC)) = []);
end;

procedure TOperationLifecycleHandlerTest.TestStart_CalcSize_NoActions;
begin
	Assert.IsTrue(FHandler.GetStartActions(CreateContext(FS_STATUS_OP_CALCSIZE)) = []);
end;

procedure TOperationLifecycleHandlerTest.TestStart_Search_NoActions;
begin
	Assert.IsTrue(FHandler.GetStartActions(CreateContext(FS_STATUS_OP_SEARCH)) = []);
end;

{ END - Operations with LogUserSpace }

procedure TOperationLifecycleHandlerTest.TestEnd_PutSingle_LogsUserSpace;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetEndActions(CreateContextInAccount(FS_STATUS_OP_PUT_SINGLE));

	Assert.IsTrue(oaLogUserSpaceInfo in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestEnd_PutMulti_LogsUserSpace;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetEndActions(CreateContextInAccount(FS_STATUS_OP_PUT_MULTI));

	Assert.IsTrue(oaLogUserSpaceInfo in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestEnd_RenMovSingle_LogsUserSpace;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetEndActions(CreateContextInAccount(FS_STATUS_OP_RENMOV_SINGLE));

	Assert.IsTrue(oaLogUserSpaceInfo in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestEnd_SyncGet_LogsUserSpace;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetEndActions(CreateContextInAccount(FS_STATUS_OP_SYNC_GET));

	Assert.IsTrue(oaLogUserSpaceInfo in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestEnd_SyncPut_LogsUserSpace;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetEndActions(CreateContextInAccount(FS_STATUS_OP_SYNC_PUT));

	Assert.IsTrue(oaLogUserSpaceInfo in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestEnd_SyncDelete_LogsUserSpace;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetEndActions(CreateContextInAccount(FS_STATUS_OP_SYNC_DELETE));

	Assert.IsTrue(oaLogUserSpaceInfo in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestEnd_LogUserSpaceDisabled_NoLog;
var
	Context: TOperationContext;
	Actions: TOperationActions;
begin
	Context := CreateContext(FS_STATUS_OP_PUT_SINGLE);
	Context.IsInAccount := True;
	Context.LogUserSpaceEnabled := False;

	Actions := FHandler.GetEndActions(Context);

	Assert.IsFalse(oaLogUserSpaceInfo in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestEnd_NotInAccount_NoLog;
var
	Context: TOperationContext;
	Actions: TOperationActions;
begin
	Context := CreateContext(FS_STATUS_OP_PUT_SINGLE);
	Context.IsInAccount := False;
	Context.LogUserSpaceEnabled := True;

	Actions := FHandler.GetEndActions(Context);

	Assert.IsFalse(oaLogUserSpaceInfo in Actions);
end;

{ END - RenMov cleanup }

procedure TOperationLifecycleHandlerTest.TestEnd_RenMovMulti_ClearsFlags;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetEndActions(CreateContextInAccount(FS_STATUS_OP_RENMOV_MULTI));

	Assert.IsTrue(oaClearSkipListRenMov in Actions);
	Assert.IsTrue(oaClearCanAbortRenMov in Actions);
	Assert.IsTrue(oaClearSkippedPath in Actions);
	Assert.IsTrue(oaLogUserSpaceInfo in Actions);
end;

{ END - Delete cleanup }

procedure TOperationLifecycleHandlerTest.TestEnd_Delete_ClearsSkipFlag;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetEndActions(CreateContextInAccount(FS_STATUS_OP_DELETE));

	Assert.IsTrue(oaClearSkipListDelete in Actions);
	Assert.IsTrue(oaLogUserSpaceInfo in Actions);
end;

{ END - Background thread cleanup }

procedure TOperationLifecycleHandlerTest.TestEnd_GetMultiThread_CleansUpBackground;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetEndActions(CreateContextInAccount(FS_STATUS_OP_GET_MULTI_THREAD));

	Assert.IsTrue(oaDecrementBackgroundJobs in Actions);
	Assert.IsTrue(oaRemoveBackgroundThread in Actions);
	Assert.IsTrue(oaLogUserSpaceInfo in Actions);
end;

procedure TOperationLifecycleHandlerTest.TestEnd_PutMultiThread_CleansUpBackground;
var
	Actions: TOperationActions;
begin
	Actions := FHandler.GetEndActions(CreateContextInAccount(FS_STATUS_OP_PUT_MULTI_THREAD));

	Assert.IsTrue(oaDecrementBackgroundJobs in Actions);
	Assert.IsTrue(oaRemoveBackgroundThread in Actions);
	Assert.IsTrue(oaLogUserSpaceInfo in Actions);
end;

{ END - No-op operations }

procedure TOperationLifecycleHandlerTest.TestEnd_GetSingle_NoActions;
begin
	Assert.IsTrue(FHandler.GetEndActions(CreateContext(FS_STATUS_OP_GET_SINGLE)) = []);
end;

procedure TOperationLifecycleHandlerTest.TestEnd_GetMulti_NoActions;
begin
	Assert.IsTrue(FHandler.GetEndActions(CreateContext(FS_STATUS_OP_GET_MULTI)) = []);
end;

procedure TOperationLifecycleHandlerTest.TestEnd_List_NoActions;
begin
	Assert.IsTrue(FHandler.GetEndActions(CreateContext(FS_STATUS_OP_LIST)) = []);
end;

initialization
	TDUnitX.RegisterTestFixture(TOperationLifecycleHandlerTest);

end.
