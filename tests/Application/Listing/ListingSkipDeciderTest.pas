unit ListingSkipDeciderTest;

{Unit tests for TListingSkipDecider - listing skip decision logic.
 Tests verify skip-list flag handling and user abort detection.}

interface

uses
	Classes,
	DUnitX.TestFramework,
	ThreadStateManager,
	TCProgress,
	ListingSkipDecider;

type
	{Mock thread state manager}
	TMockThreadState = class(TInterfacedObject, IThreadStateManager)
	private
		FSkipListDelete: Boolean;
		FSkipListRenMov: Boolean;
		FCanAbortRenMov: Boolean;
		FListingAborted: Boolean;
	public
		ListingAbortedSetCount: Integer;

		constructor Create;

		{Setters for test setup}
		procedure SetSkipListDeleteValue(Value: Boolean);
		procedure SetSkipListRenMovValue(Value: Boolean);
		procedure SetCanAbortRenMovValue(Value: Boolean);

		{IThreadStateManager implementation}
		function GetSkipListDelete: Boolean;
		procedure SetSkipListDelete(Value: Boolean);
		function GetSkipListRenMov: Boolean;
		procedure SetSkipListRenMov(Value: Boolean);
		function GetCanAbortRenMov: Boolean;
		procedure SetCanAbortRenMov(Value: Boolean);
		function GetListingAborted: Boolean;
		procedure SetListingAborted(Value: Boolean);

		{Unused methods - minimal implementation}
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

	{Mock progress}
	TMockProgress = class(TInterfacedObject, IProgress)
	private
		FAbortOnProgress: Boolean;
	public
		ProgressCalls: Integer;
		LastPath: WideString;

		constructor Create;
		procedure SetAbortOnProgress(Value: Boolean);

		function Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(SourceName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(PercentDone: Integer): Boolean; overload;
		function Aborted: Boolean;
	end;

	[TestFixture]
	TListingSkipDeciderTest = class
	private
		FDecider: IListingSkipDecider;
		FThreadState: TMockThreadState;
		FProgress: TMockProgress;

		procedure CreateDecider;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{No skip conditions}
		[Test]
		procedure TestShouldSkip_NoFlags_ReturnsFalse;

		{Skip list delete tests}
		[Test]
		procedure TestShouldSkip_SkipListDelete_ReturnsTrue;

		{Skip list renmov tests}
		[Test]
		procedure TestShouldSkip_SkipListRenMov_ReturnsTrue;

		{User abort tests}
		[Test]
		procedure TestShouldSkip_UserAbort_ReturnsTrue;
		[Test]
		procedure TestShouldSkip_UserAbort_SetsListingAbortedFlag;
		[Test]
		procedure TestShouldSkip_CanAbortFalse_DoesNotCheckProgress;
		[Test]
		procedure TestShouldSkip_UserAbort_WasAbortedIsTrue;

		{Combined conditions}
		[Test]
		procedure TestShouldSkip_MultipleFlags_ReturnsTrue;
		[Test]
		procedure TestShouldSkip_NoAbort_WasAbortedIsFalse;

		{Progress callback}
		[Test]
		procedure TestShouldSkip_PassesPathToProgress;
	end;

implementation

{TMockThreadState}

constructor TMockThreadState.Create;
begin
	inherited Create;
	FSkipListDelete := False;
	FSkipListRenMov := False;
	FCanAbortRenMov := False;
	FListingAborted := False;
	ListingAbortedSetCount := 0;
end;

procedure TMockThreadState.SetSkipListDeleteValue(Value: Boolean);
begin
	FSkipListDelete := Value;
end;

procedure TMockThreadState.SetSkipListRenMovValue(Value: Boolean);
begin
	FSkipListRenMov := Value;
end;

procedure TMockThreadState.SetCanAbortRenMovValue(Value: Boolean);
begin
	FCanAbortRenMov := Value;
end;

function TMockThreadState.GetSkipListDelete: Boolean;
begin
	Result := FSkipListDelete;
end;

procedure TMockThreadState.SetSkipListDelete(Value: Boolean);
begin
	FSkipListDelete := Value;
end;

function TMockThreadState.GetSkipListRenMov: Boolean;
begin
	Result := FSkipListRenMov;
end;

procedure TMockThreadState.SetSkipListRenMov(Value: Boolean);
begin
	FSkipListRenMov := Value;
end;

function TMockThreadState.GetCanAbortRenMov: Boolean;
begin
	Result := FCanAbortRenMov;
end;

procedure TMockThreadState.SetCanAbortRenMov(Value: Boolean);
begin
	FCanAbortRenMov := Value;
end;

function TMockThreadState.GetListingAborted: Boolean;
begin
	Result := FListingAborted;
end;

procedure TMockThreadState.SetListingAborted(Value: Boolean);
begin
	FListingAborted := Value;
	Inc(ListingAbortedSetCount);
end;

{Unused methods - minimal implementation}
function TMockThreadState.GetRetryCountDownload: Integer; begin Result := 0; end;
procedure TMockThreadState.SetRetryCountDownload(Value: Integer); begin end;
procedure TMockThreadState.IncrementRetryCountDownload; begin end;
procedure TMockThreadState.ResetRetryCountDownload; begin end;
function TMockThreadState.GetRetryCountUpload: Integer; begin Result := 0; end;
procedure TMockThreadState.SetRetryCountUpload(Value: Integer); begin end;
procedure TMockThreadState.IncrementRetryCountUpload; begin end;
procedure TMockThreadState.ResetRetryCountUpload; begin end;
function TMockThreadState.GetRetryCountRenMov: Integer; begin Result := 0; end;
procedure TMockThreadState.SetRetryCountRenMov(Value: Integer); begin end;
procedure TMockThreadState.IncrementRetryCountRenMov; begin end;
procedure TMockThreadState.ResetRetryCountRenMov; begin end;
function TMockThreadState.GetFsStatusInfo: Integer; begin Result := 0; end;
procedure TMockThreadState.SetFsStatusInfo(Value: Integer); begin end;
procedure TMockThreadState.RemoveFsStatusInfo; begin end;
function TMockThreadState.GetBackgroundThreadStatus: Integer; begin Result := 0; end;
procedure TMockThreadState.SetBackgroundThreadStatus(Value: Integer); begin end;
procedure TMockThreadState.RemoveBackgroundThread; begin end;
function TMockThreadState.HasRemoveDirSkippedPath: Boolean; begin Result := False; end;
function TMockThreadState.IsPathSkipped(const Path: WideString): Boolean; begin Result := False; end;
procedure TMockThreadState.AddSkippedPath(const Path: WideString); begin end;
procedure TMockThreadState.RemoveSkippedPath(const Path: WideString); begin end;
procedure TMockThreadState.CreateRemoveDirSkippedPath; begin end;
procedure TMockThreadState.ClearRemoveDirSkippedPath; begin end;
function TMockThreadState.GetBackgroundJobsCount(const Account: WideString): Integer; begin Result := 0; end;
procedure TMockThreadState.IncrementBackgroundJobs(const Account: WideString); begin end;
procedure TMockThreadState.DecrementBackgroundJobs(const Account: WideString); begin end;
function TMockThreadState.HasActiveBackgroundJobs(const Account: WideString): Boolean; begin Result := False; end;
function TMockThreadState.GetRemoveDirSkippedPath: TStringList; begin Result := nil; end;
function TMockThreadState.HasFsStatusInfo: Boolean; begin Result := False; end;
function TMockThreadState.HasBackgroundThread: Boolean; begin Result := False; end;

{TMockProgress}

constructor TMockProgress.Create;
begin
	inherited Create;
	FAbortOnProgress := False;
	ProgressCalls := 0;
	LastPath := '';
end;

procedure TMockProgress.SetAbortOnProgress(Value: Boolean);
begin
	FAbortOnProgress := Value;
end;

function TMockProgress.Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean;
begin
	Inc(ProgressCalls);
	LastPath := SourceName;
	Result := FAbortOnProgress;
end;

function TMockProgress.Progress(SourceName: WideString; PercentDone: Integer): Boolean;
begin
	Inc(ProgressCalls);
	LastPath := SourceName;
	Result := FAbortOnProgress;
end;

function TMockProgress.Progress(PercentDone: Integer): Boolean;
begin
	Inc(ProgressCalls);
	Result := FAbortOnProgress;
end;

function TMockProgress.Aborted: Boolean;
begin
	Result := FAbortOnProgress;
end;

{TListingSkipDeciderTest}

procedure TListingSkipDeciderTest.Setup;
begin
	FThreadState := TMockThreadState.Create;
	FProgress := TMockProgress.Create;
end;

procedure TListingSkipDeciderTest.TearDown;
begin
	FDecider := nil;
	FThreadState := nil;
	FProgress := nil;
end;

procedure TListingSkipDeciderTest.CreateDecider;
begin
	FDecider := TListingSkipDecider.Create(FThreadState, FProgress);
end;

{No skip conditions}

procedure TListingSkipDeciderTest.TestShouldSkip_NoFlags_ReturnsFalse;
var
	Result: TListingSkipResult;
begin
	CreateDecider;

	Result := FDecider.ShouldSkipListing('\account\folder');

	Assert.IsFalse(Result.ShouldSkip, 'Should not skip when no flags set');
end;

{Skip list delete tests}

procedure TListingSkipDeciderTest.TestShouldSkip_SkipListDelete_ReturnsTrue;
var
	Result: TListingSkipResult;
begin
	FThreadState.SetSkipListDeleteValue(True);
	CreateDecider;

	Result := FDecider.ShouldSkipListing('\account\folder');

	Assert.IsTrue(Result.ShouldSkip, 'Should skip when SkipListDelete is set');
end;

{Skip list renmov tests}

procedure TListingSkipDeciderTest.TestShouldSkip_SkipListRenMov_ReturnsTrue;
var
	Result: TListingSkipResult;
begin
	FThreadState.SetSkipListRenMovValue(True);
	CreateDecider;

	Result := FDecider.ShouldSkipListing('\account\folder');

	Assert.IsTrue(Result.ShouldSkip, 'Should skip when SkipListRenMov is set');
end;

{User abort tests}

procedure TListingSkipDeciderTest.TestShouldSkip_UserAbort_ReturnsTrue;
var
	Result: TListingSkipResult;
begin
	FThreadState.SetCanAbortRenMovValue(True);
	FProgress.SetAbortOnProgress(True);
	CreateDecider;

	Result := FDecider.ShouldSkipListing('\account\folder');

	Assert.IsTrue(Result.ShouldSkip, 'Should skip when user aborts');
end;

procedure TListingSkipDeciderTest.TestShouldSkip_UserAbort_SetsListingAbortedFlag;
begin
	FThreadState.SetCanAbortRenMovValue(True);
	FProgress.SetAbortOnProgress(True);
	CreateDecider;

	FDecider.ShouldSkipListing('\account\folder');

	Assert.AreEqual(1, FThreadState.ListingAbortedSetCount, 'Should set ListingAborted flag');
	Assert.IsTrue(FThreadState.GetListingAborted, 'ListingAborted should be True');
end;

procedure TListingSkipDeciderTest.TestShouldSkip_CanAbortFalse_DoesNotCheckProgress;
begin
	FThreadState.SetCanAbortRenMovValue(False);
	FProgress.SetAbortOnProgress(True); {Would abort if called}
	CreateDecider;

	FDecider.ShouldSkipListing('\account\folder');

	Assert.AreEqual(0, FProgress.ProgressCalls, 'Should not call Progress when CanAbort is false');
end;

procedure TListingSkipDeciderTest.TestShouldSkip_UserAbort_WasAbortedIsTrue;
var
	Result: TListingSkipResult;
begin
	FThreadState.SetCanAbortRenMovValue(True);
	FProgress.SetAbortOnProgress(True);
	CreateDecider;

	Result := FDecider.ShouldSkipListing('\account\folder');

	Assert.IsTrue(Result.WasAborted, 'WasAborted should be True when user aborts');
end;

{Combined conditions}

procedure TListingSkipDeciderTest.TestShouldSkip_MultipleFlags_ReturnsTrue;
var
	Result: TListingSkipResult;
begin
	FThreadState.SetSkipListDeleteValue(True);
	FThreadState.SetSkipListRenMovValue(True);
	CreateDecider;

	Result := FDecider.ShouldSkipListing('\account\folder');

	Assert.IsTrue(Result.ShouldSkip, 'Should skip when multiple flags set');
end;

procedure TListingSkipDeciderTest.TestShouldSkip_NoAbort_WasAbortedIsFalse;
var
	Result: TListingSkipResult;
begin
	FThreadState.SetSkipListDeleteValue(True); {Will skip, but not due to abort}
	CreateDecider;

	Result := FDecider.ShouldSkipListing('\account\folder');

	Assert.IsFalse(Result.WasAborted, 'WasAborted should be False when skip is not due to abort');
end;

{Progress callback}

procedure TListingSkipDeciderTest.TestShouldSkip_PassesPathToProgress;
begin
	FThreadState.SetCanAbortRenMovValue(True);
	CreateDecider;

	FDecider.ShouldSkipListing('\myaccount\myfolder');

	Assert.AreEqual('\myaccount\myfolder', FProgress.LastPath, 'Should pass path to Progress');
end;

initialization
	TDUnitX.RegisterTestFixture(TListingSkipDeciderTest);

end.
