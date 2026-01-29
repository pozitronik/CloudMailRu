unit DirectoryDeletionPreCheckTest;

{Unit tests for TDirectoryDeletionPreCheck.
 Tests pre-deletion validation logic for FsRemoveDir.}

interface

uses
	Classes,
	DUnitX.TestFramework,
	ThreadStateManager,
	DirectoryDeletionPreCheck;

type
	{Minimal mock for IThreadStateManager - only implements methods used by pre-check}
	TMockThreadStateForDeletion = class(TInterfacedObject, IThreadStateManager)
	private
		FPathSkipped: Boolean;
		FListingAborted: Boolean;
		FAbortResetCalled: Boolean;
		FLastSkipCheckPath: WideString;
	public
		constructor Create;

		{Test setup helpers}
		procedure ConfigurePathSkipped(Value: Boolean);
		procedure ConfigureListingAborted(Value: Boolean);
		function WasAbortResetCalled: Boolean;
		function GetLastSkipCheckPath: WideString;

		{IThreadStateManager - methods used by pre-check}
		function IsPathSkipped(const Path: WideString): Boolean;
		function GetListingAborted: Boolean;
		procedure SetListingAborted(Value: Boolean);

		{Unused methods - stub implementation}
		function GetSkipListDelete: Boolean;
		procedure SetSkipListDelete(Value: Boolean);
		function GetSkipListRenMov: Boolean;
		procedure SetSkipListRenMov(Value: Boolean);
		function GetCanAbortRenMov: Boolean;
		procedure SetCanAbortRenMov(Value: Boolean);
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
		function GetFsStatusInfo: Integer;
		procedure SetFsStatusInfo(Value: Integer);
		procedure RemoveFsStatusInfo;
		function GetBackgroundThreadStatus: Integer;
		procedure SetBackgroundThreadStatus(Value: Integer);
		procedure RemoveBackgroundThread;
		function HasRemoveDirSkippedPath: Boolean;
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
		function HasAnyActiveOperations: Boolean;
	end;

	[TestFixture]
	TDirectoryDeletionPreCheckTest = class
	private
		FMockThreadState: TMockThreadStateForDeletion;
		FPreCheck: IDirectoryDeletionPreCheck;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ShouldProceed tests - normal flow}
		[Test]
		procedure TestShouldProceed_WhenNoSkipConditions_ReturnsTrue;

		{ShouldProceed tests - path skip list}
		[Test]
		procedure TestShouldProceed_WhenPathSkipped_ReturnsFalse;
		[Test]
		procedure TestShouldProceed_ChecksCorrectPath;

		{ShouldProceed tests - listing abort}
		[Test]
		procedure TestShouldProceed_WhenListingAborted_ReturnsFalse;
		[Test]
		procedure TestShouldProceed_WhenListingAborted_ResetsFlag;
		[Test]
		procedure TestShouldProceed_WhenNotAborted_DoesNotResetFlag;

		{ShouldProceed tests - priority}
		[Test]
		procedure TestShouldProceed_PathSkipCheckedBeforeAbort;
	end;

implementation

uses
	SysUtils;

{TMockThreadStateForDeletion}

constructor TMockThreadStateForDeletion.Create;
begin
	inherited Create;
	FPathSkipped := False;
	FListingAborted := False;
	FAbortResetCalled := False;
	FLastSkipCheckPath := '';
end;

procedure TMockThreadStateForDeletion.ConfigurePathSkipped(Value: Boolean);
begin
	FPathSkipped := Value;
end;

procedure TMockThreadStateForDeletion.ConfigureListingAborted(Value: Boolean);
begin
	FListingAborted := Value;
end;

procedure TMockThreadStateForDeletion.SetListingAborted(Value: Boolean);
begin
	FListingAborted := Value;
	if not Value then
		FAbortResetCalled := True;
end;

function TMockThreadStateForDeletion.WasAbortResetCalled: Boolean;
begin
	Result := FAbortResetCalled;
end;

function TMockThreadStateForDeletion.GetLastSkipCheckPath: WideString;
begin
	Result := FLastSkipCheckPath;
end;

function TMockThreadStateForDeletion.IsPathSkipped(const Path: WideString): Boolean;
begin
	FLastSkipCheckPath := Path;
	Result := FPathSkipped;
end;

function TMockThreadStateForDeletion.GetListingAborted: Boolean;
begin
	Result := FListingAborted;
end;

{Stub implementations}
function TMockThreadStateForDeletion.GetSkipListDelete: Boolean; begin Result := False; end;
procedure TMockThreadStateForDeletion.SetSkipListDelete(Value: Boolean); begin end;
function TMockThreadStateForDeletion.GetSkipListRenMov: Boolean; begin Result := False; end;
procedure TMockThreadStateForDeletion.SetSkipListRenMov(Value: Boolean); begin end;
function TMockThreadStateForDeletion.GetCanAbortRenMov: Boolean; begin Result := False; end;
procedure TMockThreadStateForDeletion.SetCanAbortRenMov(Value: Boolean); begin end;
function TMockThreadStateForDeletion.GetRetryCountDownload: Integer; begin Result := 0; end;
procedure TMockThreadStateForDeletion.SetRetryCountDownload(Value: Integer); begin end;
procedure TMockThreadStateForDeletion.IncrementRetryCountDownload; begin end;
procedure TMockThreadStateForDeletion.ResetRetryCountDownload; begin end;
function TMockThreadStateForDeletion.GetRetryCountUpload: Integer; begin Result := 0; end;
procedure TMockThreadStateForDeletion.SetRetryCountUpload(Value: Integer); begin end;
procedure TMockThreadStateForDeletion.IncrementRetryCountUpload; begin end;
procedure TMockThreadStateForDeletion.ResetRetryCountUpload; begin end;
function TMockThreadStateForDeletion.GetRetryCountRenMov: Integer; begin Result := 0; end;
procedure TMockThreadStateForDeletion.SetRetryCountRenMov(Value: Integer); begin end;
procedure TMockThreadStateForDeletion.IncrementRetryCountRenMov; begin end;
procedure TMockThreadStateForDeletion.ResetRetryCountRenMov; begin end;
function TMockThreadStateForDeletion.GetFsStatusInfo: Integer; begin Result := 0; end;
procedure TMockThreadStateForDeletion.SetFsStatusInfo(Value: Integer); begin end;
procedure TMockThreadStateForDeletion.RemoveFsStatusInfo; begin end;
function TMockThreadStateForDeletion.GetBackgroundThreadStatus: Integer; begin Result := 0; end;
procedure TMockThreadStateForDeletion.SetBackgroundThreadStatus(Value: Integer); begin end;
procedure TMockThreadStateForDeletion.RemoveBackgroundThread; begin end;
function TMockThreadStateForDeletion.HasRemoveDirSkippedPath: Boolean; begin Result := False; end;
procedure TMockThreadStateForDeletion.AddSkippedPath(const Path: WideString); begin end;
procedure TMockThreadStateForDeletion.RemoveSkippedPath(const Path: WideString); begin end;
procedure TMockThreadStateForDeletion.CreateRemoveDirSkippedPath; begin end;
procedure TMockThreadStateForDeletion.ClearRemoveDirSkippedPath; begin end;
function TMockThreadStateForDeletion.GetRemoveDirSkippedPath: TStringList; begin Result := nil; end;
function TMockThreadStateForDeletion.GetBackgroundJobsCount(const Account: WideString): Integer; begin Result := 0; end;
procedure TMockThreadStateForDeletion.IncrementBackgroundJobs(const Account: WideString); begin end;
procedure TMockThreadStateForDeletion.DecrementBackgroundJobs(const Account: WideString); begin end;
function TMockThreadStateForDeletion.HasActiveBackgroundJobs(const Account: WideString): Boolean; begin Result := False; end;
function TMockThreadStateForDeletion.HasFsStatusInfo: Boolean; begin Result := False; end;
function TMockThreadStateForDeletion.HasBackgroundThread: Boolean; begin Result := False; end;
function TMockThreadStateForDeletion.HasAnyActiveOperations: Boolean; begin Result := False; end;

{TDirectoryDeletionPreCheckTest}

procedure TDirectoryDeletionPreCheckTest.Setup;
begin
	FMockThreadState := TMockThreadStateForDeletion.Create;
	FPreCheck := TDirectoryDeletionPreCheck.Create(FMockThreadState);
end;

procedure TDirectoryDeletionPreCheckTest.TearDown;
begin
	FPreCheck := nil;
	{FMockThreadState freed by reference counting}
end;

{ShouldProceed tests - normal flow}

procedure TDirectoryDeletionPreCheckTest.TestShouldProceed_WhenNoSkipConditions_ReturnsTrue;
begin
	FMockThreadState.ConfigurePathSkipped(False);
	FMockThreadState.ConfigureListingAborted(False);

	Assert.IsTrue(FPreCheck.ShouldProceed('\account\folder'));
end;

{ShouldProceed tests - path skip list}

procedure TDirectoryDeletionPreCheckTest.TestShouldProceed_WhenPathSkipped_ReturnsFalse;
begin
	FMockThreadState.ConfigurePathSkipped(True);

	Assert.IsFalse(FPreCheck.ShouldProceed('\account\skipped_folder'));
end;

procedure TDirectoryDeletionPreCheckTest.TestShouldProceed_ChecksCorrectPath;
var
	TestPath: WideString;
begin
	TestPath := '\account\specific\path\to\check';
	FMockThreadState.ConfigurePathSkipped(False);

	FPreCheck.ShouldProceed(TestPath);

	Assert.AreEqual(TestPath, FMockThreadState.GetLastSkipCheckPath);
end;

{ShouldProceed tests - listing abort}

procedure TDirectoryDeletionPreCheckTest.TestShouldProceed_WhenListingAborted_ReturnsFalse;
begin
	FMockThreadState.ConfigurePathSkipped(False);
	FMockThreadState.ConfigureListingAborted(True);

	Assert.IsFalse(FPreCheck.ShouldProceed('\account\folder'));
end;

procedure TDirectoryDeletionPreCheckTest.TestShouldProceed_WhenListingAborted_ResetsFlag;
begin
	FMockThreadState.ConfigurePathSkipped(False);
	FMockThreadState.ConfigureListingAborted(True);

	FPreCheck.ShouldProceed('\account\folder');

	Assert.IsTrue(FMockThreadState.WasAbortResetCalled);
end;

procedure TDirectoryDeletionPreCheckTest.TestShouldProceed_WhenNotAborted_DoesNotResetFlag;
begin
	FMockThreadState.ConfigurePathSkipped(False);
	FMockThreadState.ConfigureListingAborted(False);

	FPreCheck.ShouldProceed('\account\folder');

	Assert.IsFalse(FMockThreadState.WasAbortResetCalled);
end;

{ShouldProceed tests - priority}

procedure TDirectoryDeletionPreCheckTest.TestShouldProceed_PathSkipCheckedBeforeAbort;
begin
	{When path is skipped, abort flag should NOT be reset because
	 path skip is checked first and exits early}
	FMockThreadState.ConfigurePathSkipped(True);
	FMockThreadState.ConfigureListingAborted(True);

	FPreCheck.ShouldProceed('\account\folder');

	{Abort reset should NOT be called because we exited early on path skip}
	Assert.IsFalse(FMockThreadState.WasAbortResetCalled);
end;

initialization
	TDUnitX.RegisterTestFixture(TDirectoryDeletionPreCheckTest);

end.
