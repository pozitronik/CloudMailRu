unit MoveOperationContextTrackerTest;

{Unit tests for TMoveOperationContextTracker.
 Tests move operation context tracking between FsMkDir and FsRemoveDir.}

interface

uses
	Classes,
	DUnitX.TestFramework,
	ThreadStateManager,
	MoveOperationContextTracker,
	RealPath;

type
	{Minimal mock for IThreadStateManager - only implements GetFsStatusInfo}
	TMockThreadStateForMove = class(TInterfacedObject, IThreadStateManager)
	private
		FFsStatusInfo: Integer;
	public
		constructor Create;

		procedure SetFsStatusInfoValue(Value: Integer);

		{IThreadStateManager - only GetFsStatusInfo is used by tracker}
		function GetFsStatusInfo: Integer;

		{Unused methods - minimal stub implementation}
		function GetSkipListDelete: Boolean;
		procedure SetSkipListDelete(Value: Boolean);
		function GetSkipListRenMov: Boolean;
		procedure SetSkipListRenMov(Value: Boolean);
		function GetCanAbortRenMov: Boolean;
		procedure SetCanAbortRenMov(Value: Boolean);
		function GetListingAborted: Boolean;
		procedure SetListingAborted(Value: Boolean);
		function GetRetryCountDownload: Integer;
		procedure SetRetryCountDownload(Value: Integer);
		procedure IncrementRetryCountDownload;
		procedure ResetRetryCountDownload;
		function GetRetryCountUpload: Integer;
		procedure SetRetryCountUpload(Value: Integer);
		procedure IncrementRetryCountUpload;
		procedure ResetRetryCountUpload;
		function GetRetryCountRenMov: Integer;
		procedure SetRetryCountRenMov(Value: Integer);
		procedure IncrementRetryCountRenMov;
		procedure ResetRetryCountRenMov;
		procedure SetFsStatusInfo(Value: Integer);
		procedure RemoveFsStatusInfo;
		function GetBackgroundThreadStatus: Integer;
		procedure SetBackgroundThreadStatus(Value: Integer);
		procedure RemoveBackgroundThread;
		function HasRemoveDirSkippedPath: Boolean;
		function IsPathSkipped(const Path: WideString): Boolean;
		procedure AddSkippedPath(const Path: WideString);
		procedure RemoveSkippedPath(const Path: WideString);
		procedure CreateRemoveDirSkippedPath;
		procedure ClearRemoveDirSkippedPath;
		function GetRemoveDirSkippedPath: TStringList;
		function GetBackgroundJobsCount(const Account: WideString): Integer;
		procedure IncrementBackgroundJobs(const Account: WideString);
		procedure DecrementBackgroundJobs(const Account: WideString);
		function HasActiveBackgroundJobs(const Account: WideString): Boolean;
		function HasFsStatusInfo: Boolean;
		function HasBackgroundThread: Boolean;
	end;

	[TestFixture]
	TMoveOperationContextTrackerTest = class
	private
		FMockThreadState: TMockThreadStateForMove;
		FTracker: IMoveOperationContextTracker;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{IsMoveOperation tests}
		[Test]
		procedure TestIsMoveOperation_WhenRenMovMulti_ReturnsTrue;
		[Test]
		procedure TestIsMoveOperation_WhenOtherOperation_ReturnsFalse;
		[Test]
		procedure TestIsMoveOperation_WhenNoOperation_ReturnsFalse;

		{TrackMoveTarget tests}
		[Test]
		procedure TestTrackMoveTarget_StoresPath;
		[Test]
		procedure TestTrackMoveTarget_OverwritesPreviousPath;

		{GetMoveTarget tests}
		[Test]
		procedure TestGetMoveTarget_WhenTracked_ReturnsPath;
		[Test]
		procedure TestGetMoveTarget_WhenNotTracked_ReturnsDefault;

		{ClearMoveTarget tests}
		[Test]
		procedure TestClearMoveTarget_ClearsTrackedPath;
		[Test]
		procedure TestClearMoveTarget_WhenNotTracked_DoesNotFail;

		{Integration scenario tests}
		[Test]
		procedure TestScenario_MoveOperation_TrackAndRetrieve;
		[Test]
		procedure TestScenario_DeleteOperation_NoTracking;
	end;

implementation

uses
	SysUtils,
	PLUGIN_TYPES;

{TMockThreadStateForMove}

constructor TMockThreadStateForMove.Create;
begin
	inherited Create;
	FFsStatusInfo := 0;
end;

procedure TMockThreadStateForMove.SetFsStatusInfoValue(Value: Integer);
begin
	FFsStatusInfo := Value;
end;

function TMockThreadStateForMove.GetFsStatusInfo: Integer;
begin
	Result := FFsStatusInfo;
end;

{Stub implementations - not used by tracker}
function TMockThreadStateForMove.GetSkipListDelete: Boolean; begin Result := False; end;
procedure TMockThreadStateForMove.SetSkipListDelete(Value: Boolean); begin end;
function TMockThreadStateForMove.GetSkipListRenMov: Boolean; begin Result := False; end;
procedure TMockThreadStateForMove.SetSkipListRenMov(Value: Boolean); begin end;
function TMockThreadStateForMove.GetCanAbortRenMov: Boolean; begin Result := False; end;
procedure TMockThreadStateForMove.SetCanAbortRenMov(Value: Boolean); begin end;
function TMockThreadStateForMove.GetListingAborted: Boolean; begin Result := False; end;
procedure TMockThreadStateForMove.SetListingAborted(Value: Boolean); begin end;
function TMockThreadStateForMove.GetRetryCountDownload: Integer; begin Result := 0; end;
procedure TMockThreadStateForMove.SetRetryCountDownload(Value: Integer); begin end;
procedure TMockThreadStateForMove.IncrementRetryCountDownload; begin end;
procedure TMockThreadStateForMove.ResetRetryCountDownload; begin end;
function TMockThreadStateForMove.GetRetryCountUpload: Integer; begin Result := 0; end;
procedure TMockThreadStateForMove.SetRetryCountUpload(Value: Integer); begin end;
procedure TMockThreadStateForMove.IncrementRetryCountUpload; begin end;
procedure TMockThreadStateForMove.ResetRetryCountUpload; begin end;
function TMockThreadStateForMove.GetRetryCountRenMov: Integer; begin Result := 0; end;
procedure TMockThreadStateForMove.SetRetryCountRenMov(Value: Integer); begin end;
procedure TMockThreadStateForMove.IncrementRetryCountRenMov; begin end;
procedure TMockThreadStateForMove.ResetRetryCountRenMov; begin end;
procedure TMockThreadStateForMove.SetFsStatusInfo(Value: Integer); begin FFsStatusInfo := Value; end;
procedure TMockThreadStateForMove.RemoveFsStatusInfo; begin FFsStatusInfo := 0; end;
function TMockThreadStateForMove.GetBackgroundThreadStatus: Integer; begin Result := 0; end;
procedure TMockThreadStateForMove.SetBackgroundThreadStatus(Value: Integer); begin end;
procedure TMockThreadStateForMove.RemoveBackgroundThread; begin end;
function TMockThreadStateForMove.HasRemoveDirSkippedPath: Boolean; begin Result := False; end;
function TMockThreadStateForMove.IsPathSkipped(const Path: WideString): Boolean; begin Result := False; end;
procedure TMockThreadStateForMove.AddSkippedPath(const Path: WideString); begin end;
procedure TMockThreadStateForMove.RemoveSkippedPath(const Path: WideString); begin end;
procedure TMockThreadStateForMove.CreateRemoveDirSkippedPath; begin end;
procedure TMockThreadStateForMove.ClearRemoveDirSkippedPath; begin end;
function TMockThreadStateForMove.GetRemoveDirSkippedPath: TStringList; begin Result := nil; end;
function TMockThreadStateForMove.GetBackgroundJobsCount(const Account: WideString): Integer; begin Result := 0; end;
procedure TMockThreadStateForMove.IncrementBackgroundJobs(const Account: WideString); begin end;
procedure TMockThreadStateForMove.DecrementBackgroundJobs(const Account: WideString); begin end;
function TMockThreadStateForMove.HasActiveBackgroundJobs(const Account: WideString): Boolean; begin Result := False; end;
function TMockThreadStateForMove.HasFsStatusInfo: Boolean; begin Result := FFsStatusInfo <> 0; end;
function TMockThreadStateForMove.HasBackgroundThread: Boolean; begin Result := False; end;

{TMoveOperationContextTrackerTest}

procedure TMoveOperationContextTrackerTest.Setup;
begin
	FMockThreadState := TMockThreadStateForMove.Create;
	FTracker := TMoveOperationContextTracker.Create(FMockThreadState);
end;

procedure TMoveOperationContextTrackerTest.TearDown;
begin
	FTracker := nil;
	{FMockThreadState freed by reference counting}
end;

{IsMoveOperation tests}

procedure TMoveOperationContextTrackerTest.TestIsMoveOperation_WhenRenMovMulti_ReturnsTrue;
begin
	FMockThreadState.SetFsStatusInfoValue(FS_STATUS_OP_RENMOV_MULTI);

	Assert.IsTrue(FTracker.IsMoveOperation);
end;

procedure TMoveOperationContextTrackerTest.TestIsMoveOperation_WhenOtherOperation_ReturnsFalse;
begin
	FMockThreadState.SetFsStatusInfoValue(FS_STATUS_OP_DELETE);

	Assert.IsFalse(FTracker.IsMoveOperation);
end;

procedure TMoveOperationContextTrackerTest.TestIsMoveOperation_WhenNoOperation_ReturnsFalse;
begin
	FMockThreadState.SetFsStatusInfoValue(0);

	Assert.IsFalse(FTracker.IsMoveOperation);
end;

{TrackMoveTarget tests}

procedure TMoveOperationContextTrackerTest.TestTrackMoveTarget_StoresPath;
var
	TargetPath, RetrievedPath: TRealPath;
begin
	TargetPath.FromPath('\account\destination\folder');

	FTracker.TrackMoveTarget(TargetPath);
	RetrievedPath := FTracker.GetMoveTarget;

	Assert.AreEqual('account', RetrievedPath.account);
	Assert.AreEqual('destination\folder', RetrievedPath.Path);
end;

procedure TMoveOperationContextTrackerTest.TestTrackMoveTarget_OverwritesPreviousPath;
var
	FirstPath, SecondPath, RetrievedPath: TRealPath;
begin
	FirstPath.FromPath('\account\first');
	SecondPath.FromPath('\account\second');

	FTracker.TrackMoveTarget(FirstPath);
	FTracker.TrackMoveTarget(SecondPath);
	RetrievedPath := FTracker.GetMoveTarget;

	Assert.AreEqual('second', RetrievedPath.Path);
end;

{GetMoveTarget tests}

procedure TMoveOperationContextTrackerTest.TestGetMoveTarget_WhenTracked_ReturnsPath;
var
	TargetPath, RetrievedPath: TRealPath;
begin
	TargetPath.FromPath('\account\folder');
	FTracker.TrackMoveTarget(TargetPath);

	RetrievedPath := FTracker.GetMoveTarget;

	Assert.AreEqual('folder', RetrievedPath.Path);
end;

procedure TMoveOperationContextTrackerTest.TestGetMoveTarget_WhenNotTracked_ReturnsDefault;
var
	RetrievedPath: TRealPath;
begin
	RetrievedPath := FTracker.GetMoveTarget;

	Assert.AreEqual('', RetrievedPath.account);
	Assert.AreEqual('', RetrievedPath.Path);
end;

{ClearMoveTarget tests}

procedure TMoveOperationContextTrackerTest.TestClearMoveTarget_ClearsTrackedPath;
var
	TargetPath, RetrievedPath: TRealPath;
begin
	TargetPath.FromPath('\account\folder');
	FTracker.TrackMoveTarget(TargetPath);

	FTracker.ClearMoveTarget;
	RetrievedPath := FTracker.GetMoveTarget;

	Assert.AreEqual('', RetrievedPath.account);
end;

procedure TMoveOperationContextTrackerTest.TestClearMoveTarget_WhenNotTracked_DoesNotFail;
begin
	{Should not raise exception}
	FTracker.ClearMoveTarget;

	Assert.Pass;
end;

{Integration scenario tests}

procedure TMoveOperationContextTrackerTest.TestScenario_MoveOperation_TrackAndRetrieve;
var
	DestinationPath, SourcePath, RetrievedTarget: TRealPath;
begin
	{Simulate move operation: FsMkDir creates destination during RENMOV_MULTI}
	FMockThreadState.SetFsStatusInfoValue(FS_STATUS_OP_RENMOV_MULTI);
	DestinationPath.FromPath('\account\new_location\folder');

	{FsMkDir tracks the target}
	if FTracker.IsMoveOperation then
		FTracker.TrackMoveTarget(DestinationPath);

	{FsRemoveDir retrieves the target for description sync}
	SourcePath.FromPath('\account\old_location\folder');
	if FTracker.IsMoveOperation then
	begin
		RetrievedTarget := FTracker.GetMoveTarget;
		{Would call OnFileRenamed(SourcePath, RetrievedTarget)}
		Assert.AreEqual('new_location\folder', RetrievedTarget.Path);
	end
	else
		Assert.Fail('Should be in move operation');
end;

procedure TMoveOperationContextTrackerTest.TestScenario_DeleteOperation_NoTracking;
var
	SourcePath, RetrievedTarget: TRealPath;
begin
	{Simulate delete operation - not RENMOV_MULTI}
	FMockThreadState.SetFsStatusInfoValue(FS_STATUS_OP_DELETE);
	SourcePath.FromPath('\account\folder_to_delete');

	{FsRemoveDir checks if it's a move}
	Assert.IsFalse(FTracker.IsMoveOperation);

	{GetMoveTarget returns empty since nothing was tracked}
	RetrievedTarget := FTracker.GetMoveTarget;
	Assert.AreEqual('', RetrievedTarget.account);
end;

initialization
	TDUnitX.RegisterTestFixture(TMoveOperationContextTrackerTest);

end.
