unit ThreadStateManagerTest;

interface

uses
	Windows, Classes, SysUtils,
	ThreadStateManager,
	DUnitX.TestFramework;

type
	[TestFixture]
	TThreadStateManagerTest = class
	private
		FManager: IThreadStateManager;
	public
		[Setup]
		procedure Setup;

		[TearDown]
		procedure TearDown;

		{ Boolean flag tests }
		[Test]
		procedure TestSkipListDelete_DefaultsFalse;
		[Test]
		procedure TestSkipListDelete_SetAndGet;
		[Test]
		procedure TestSkipListRenMov_DefaultsFalse;
		[Test]
		procedure TestSkipListRenMov_SetAndGet;
		[Test]
		procedure TestCanAbortRenMov_DefaultsFalse;
		[Test]
		procedure TestCanAbortRenMov_SetAndGet;
		[Test]
		procedure TestListingAborted_DefaultsFalse;
		[Test]
		procedure TestListingAborted_SetAndGet;

		{ Retry counter tests }
		[Test]
		procedure TestRetryCountDownload_DefaultsZero;
		[Test]
		procedure TestRetryCountDownload_SetAndGet;
		[Test]
		procedure TestRetryCountDownload_Increment;
		[Test]
		procedure TestRetryCountDownload_Reset;
		[Test]
		procedure TestRetryCountUpload_DefaultsZero;
		[Test]
		procedure TestRetryCountUpload_Increment;
		[Test]
		procedure TestRetryCountUpload_Reset;
		[Test]
		procedure TestRetryCountRenMov_DefaultsZero;
		[Test]
		procedure TestRetryCountRenMov_Increment;
		[Test]
		procedure TestRetryCountRenMov_Reset;

		{ Operation context tests }
		[Test]
		procedure TestFsStatusInfo_DefaultsZero;
		[Test]
		procedure TestFsStatusInfo_SetAndGet;
		[Test]
		procedure TestFsStatusInfo_Remove;
		[Test]
		procedure TestFsStatusInfo_HasFsStatusInfo;

		{ Background thread tests }
		[Test]
		procedure TestBackgroundThread_DefaultsZero;
		[Test]
		procedure TestBackgroundThread_SetAndGet;
		[Test]
		procedure TestBackgroundThread_Remove;
		[Test]
		procedure TestBackgroundThread_HasBackgroundThread;

		{ Path blacklist tests }
		[Test]
		procedure TestRemoveDirSkippedPath_InitiallyNil;
		[Test]
		procedure TestRemoveDirSkippedPath_CreateAndHas;
		[Test]
		procedure TestRemoveDirSkippedPath_AddPath;
		[Test]
		procedure TestRemoveDirSkippedPath_IsPathSkipped;
		[Test]
		procedure TestRemoveDirSkippedPath_RemovePath;
		[Test]
		procedure TestRemoveDirSkippedPath_Clear;

		{ Background jobs (account-keyed) tests }
		[Test]
		procedure TestBackgroundJobs_DefaultsZero;
		[Test]
		procedure TestBackgroundJobs_Increment;
		[Test]
		procedure TestBackgroundJobs_Decrement;
		[Test]
		procedure TestBackgroundJobs_HasActiveJobs;
		[Test]
		procedure TestBackgroundJobs_AccountIsolation;

		{ HasAnyActiveOperations tests }
		[Test]
		procedure TestHasAnyActiveOperations_FalseWhenEmpty;
		[Test]
		procedure TestHasAnyActiveOperations_TrueWithBackgroundThread;
		[Test]
		procedure TestHasAnyActiveOperations_TrueWithActiveBackgroundJobs;
		[Test]
		procedure TestHasAnyActiveOperations_FalseWithZeroJobCount;

		{ Edge cases }
		[Test]
		procedure TestIsPathSkipped_NoSkippedPathList_ReturnsFalse;
		[Test]
		procedure TestDecrementBackgroundJobs_UnknownAccount_SetsNegative;
		[Test]
		procedure TestCreateRemoveDirSkippedPath_DoubleCreate_NoLeak;
	end;

implementation

procedure TThreadStateManagerTest.Setup;
begin
	FManager := TThreadStateManager.Create;
end;

procedure TThreadStateManagerTest.TearDown;
begin
	FManager := nil;
end;

{ Boolean flag tests }

procedure TThreadStateManagerTest.TestSkipListDelete_DefaultsFalse;
begin
	Assert.IsFalse(FManager.GetSkipListDelete);
end;

procedure TThreadStateManagerTest.TestSkipListDelete_SetAndGet;
begin
	FManager.SetSkipListDelete(True);
	Assert.IsTrue(FManager.GetSkipListDelete);

	FManager.SetSkipListDelete(False);
	Assert.IsFalse(FManager.GetSkipListDelete);
end;

procedure TThreadStateManagerTest.TestSkipListRenMov_DefaultsFalse;
begin
	Assert.IsFalse(FManager.GetSkipListRenMov);
end;

procedure TThreadStateManagerTest.TestSkipListRenMov_SetAndGet;
begin
	FManager.SetSkipListRenMov(True);
	Assert.IsTrue(FManager.GetSkipListRenMov);
end;

procedure TThreadStateManagerTest.TestCanAbortRenMov_DefaultsFalse;
begin
	Assert.IsFalse(FManager.GetCanAbortRenMov);
end;

procedure TThreadStateManagerTest.TestCanAbortRenMov_SetAndGet;
begin
	FManager.SetCanAbortRenMov(True);
	Assert.IsTrue(FManager.GetCanAbortRenMov);
end;

procedure TThreadStateManagerTest.TestListingAborted_DefaultsFalse;
begin
	Assert.IsFalse(FManager.GetListingAborted);
end;

procedure TThreadStateManagerTest.TestListingAborted_SetAndGet;
begin
	FManager.SetListingAborted(True);
	Assert.IsTrue(FManager.GetListingAborted);

	FManager.SetListingAborted(False);
	Assert.IsFalse(FManager.GetListingAborted);
end;

{ Retry counter tests }

procedure TThreadStateManagerTest.TestRetryCountDownload_DefaultsZero;
begin
	Assert.AreEqual(0, FManager.GetRetryCountDownload);
end;

procedure TThreadStateManagerTest.TestRetryCountDownload_SetAndGet;
begin
	FManager.SetRetryCountDownload(5);
	Assert.AreEqual(5, FManager.GetRetryCountDownload);
end;

procedure TThreadStateManagerTest.TestRetryCountDownload_Increment;
begin
	Assert.AreEqual(0, FManager.GetRetryCountDownload);

	FManager.IncrementRetryCountDownload;
	Assert.AreEqual(1, FManager.GetRetryCountDownload);

	FManager.IncrementRetryCountDownload;
	Assert.AreEqual(2, FManager.GetRetryCountDownload);

	FManager.IncrementRetryCountDownload;
	Assert.AreEqual(3, FManager.GetRetryCountDownload);
end;

procedure TThreadStateManagerTest.TestRetryCountDownload_Reset;
begin
	FManager.SetRetryCountDownload(5);
	Assert.AreEqual(5, FManager.GetRetryCountDownload);

	FManager.ResetRetryCountDownload;
	Assert.AreEqual(0, FManager.GetRetryCountDownload);
end;

procedure TThreadStateManagerTest.TestRetryCountUpload_DefaultsZero;
begin
	Assert.AreEqual(0, FManager.GetRetryCountUpload);
end;

procedure TThreadStateManagerTest.TestRetryCountUpload_Increment;
begin
	FManager.IncrementRetryCountUpload;
	FManager.IncrementRetryCountUpload;
	Assert.AreEqual(2, FManager.GetRetryCountUpload);
end;

procedure TThreadStateManagerTest.TestRetryCountUpload_Reset;
begin
	FManager.SetRetryCountUpload(3);
	FManager.ResetRetryCountUpload;
	Assert.AreEqual(0, FManager.GetRetryCountUpload);
end;

procedure TThreadStateManagerTest.TestRetryCountRenMov_DefaultsZero;
begin
	Assert.AreEqual(0, FManager.GetRetryCountRenMov);
end;

procedure TThreadStateManagerTest.TestRetryCountRenMov_Increment;
begin
	FManager.IncrementRetryCountRenMov;
	Assert.AreEqual(1, FManager.GetRetryCountRenMov);
end;

procedure TThreadStateManagerTest.TestRetryCountRenMov_Reset;
begin
	FManager.SetRetryCountRenMov(7);
	FManager.ResetRetryCountRenMov;
	Assert.AreEqual(0, FManager.GetRetryCountRenMov);
end;

{ Operation context tests }

procedure TThreadStateManagerTest.TestFsStatusInfo_DefaultsZero;
begin
	Assert.AreEqual(0, FManager.GetFsStatusInfo);
end;

procedure TThreadStateManagerTest.TestFsStatusInfo_SetAndGet;
begin
	FManager.SetFsStatusInfo(42);
	Assert.AreEqual(42, FManager.GetFsStatusInfo);
end;

procedure TThreadStateManagerTest.TestFsStatusInfo_Remove;
begin
	FManager.SetFsStatusInfo(42);
	Assert.IsTrue(FManager.HasFsStatusInfo);

	FManager.RemoveFsStatusInfo;
	Assert.IsFalse(FManager.HasFsStatusInfo);
	Assert.AreEqual(0, FManager.GetFsStatusInfo);
end;

procedure TThreadStateManagerTest.TestFsStatusInfo_HasFsStatusInfo;
begin
	Assert.IsFalse(FManager.HasFsStatusInfo);

	FManager.SetFsStatusInfo(1);
	Assert.IsTrue(FManager.HasFsStatusInfo);
end;

{ Background thread tests }

procedure TThreadStateManagerTest.TestBackgroundThread_DefaultsZero;
begin
	Assert.AreEqual(0, FManager.GetBackgroundThreadStatus);
end;

procedure TThreadStateManagerTest.TestBackgroundThread_SetAndGet;
begin
	FManager.SetBackgroundThreadStatus(100);
	Assert.AreEqual(100, FManager.GetBackgroundThreadStatus);
end;

procedure TThreadStateManagerTest.TestBackgroundThread_Remove;
begin
	FManager.SetBackgroundThreadStatus(100);
	Assert.IsTrue(FManager.HasBackgroundThread);

	FManager.RemoveBackgroundThread;
	Assert.IsFalse(FManager.HasBackgroundThread);
end;

procedure TThreadStateManagerTest.TestBackgroundThread_HasBackgroundThread;
begin
	Assert.IsFalse(FManager.HasBackgroundThread);

	FManager.SetBackgroundThreadStatus(1);
	Assert.IsTrue(FManager.HasBackgroundThread);
end;

{ Path blacklist tests }

procedure TThreadStateManagerTest.TestRemoveDirSkippedPath_InitiallyNil;
begin
	Assert.IsNull(FManager.GetRemoveDirSkippedPath);
	Assert.IsFalse(FManager.HasRemoveDirSkippedPath);
end;

procedure TThreadStateManagerTest.TestRemoveDirSkippedPath_CreateAndHas;
begin
	FManager.CreateRemoveDirSkippedPath;
	Assert.IsTrue(FManager.HasRemoveDirSkippedPath);
	Assert.IsNotNull(FManager.GetRemoveDirSkippedPath);
end;

procedure TThreadStateManagerTest.TestRemoveDirSkippedPath_AddPath;
var
	SkippedPath: TStringList;
begin
	FManager.CreateRemoveDirSkippedPath;
	FManager.AddSkippedPath('\account\folder1');
	FManager.AddSkippedPath('\account\folder2');

	SkippedPath := FManager.GetRemoveDirSkippedPath;
	Assert.AreEqual(2, SkippedPath.Count);
	Assert.AreEqual('\account\folder1', SkippedPath[0]);
	Assert.AreEqual('\account\folder2', SkippedPath[1]);
end;

procedure TThreadStateManagerTest.TestRemoveDirSkippedPath_IsPathSkipped;
begin
	FManager.CreateRemoveDirSkippedPath;
	FManager.AddSkippedPath('\account\folder1');

	Assert.IsTrue(FManager.IsPathSkipped('\account\folder1'));
	Assert.IsFalse(FManager.IsPathSkipped('\account\folder2'));
end;

procedure TThreadStateManagerTest.TestRemoveDirSkippedPath_RemovePath;
begin
	FManager.CreateRemoveDirSkippedPath;
	FManager.AddSkippedPath('\account\folder1');
	FManager.AddSkippedPath('\account\folder2');

	FManager.RemoveSkippedPath('\account\folder1');

	Assert.IsFalse(FManager.IsPathSkipped('\account\folder1'));
	Assert.IsTrue(FManager.IsPathSkipped('\account\folder2'));
	Assert.AreEqual(1, FManager.GetRemoveDirSkippedPath.Count);
end;

procedure TThreadStateManagerTest.TestRemoveDirSkippedPath_Clear;
begin
	FManager.CreateRemoveDirSkippedPath;
	FManager.AddSkippedPath('\account\folder1');
	Assert.IsTrue(FManager.HasRemoveDirSkippedPath);

	FManager.ClearRemoveDirSkippedPath;
	Assert.IsFalse(FManager.HasRemoveDirSkippedPath);
end;

{ Background jobs (account-keyed) tests }

procedure TThreadStateManagerTest.TestBackgroundJobs_DefaultsZero;
begin
	Assert.AreEqual(0, FManager.GetBackgroundJobsCount('test@mail.ru'));
end;

procedure TThreadStateManagerTest.TestBackgroundJobs_Increment;
begin
	FManager.IncrementBackgroundJobs('test@mail.ru');
	Assert.AreEqual(1, FManager.GetBackgroundJobsCount('test@mail.ru'));

	FManager.IncrementBackgroundJobs('test@mail.ru');
	Assert.AreEqual(2, FManager.GetBackgroundJobsCount('test@mail.ru'));
end;

procedure TThreadStateManagerTest.TestBackgroundJobs_Decrement;
begin
	FManager.IncrementBackgroundJobs('test@mail.ru');
	FManager.IncrementBackgroundJobs('test@mail.ru');
	FManager.DecrementBackgroundJobs('test@mail.ru');
	Assert.AreEqual(1, FManager.GetBackgroundJobsCount('test@mail.ru'));
end;

procedure TThreadStateManagerTest.TestBackgroundJobs_HasActiveJobs;
begin
	Assert.IsFalse(FManager.HasActiveBackgroundJobs('test@mail.ru'));

	FManager.IncrementBackgroundJobs('test@mail.ru');
	Assert.IsTrue(FManager.HasActiveBackgroundJobs('test@mail.ru'));

	FManager.DecrementBackgroundJobs('test@mail.ru');
	Assert.IsFalse(FManager.HasActiveBackgroundJobs('test@mail.ru'));
end;

procedure TThreadStateManagerTest.TestBackgroundJobs_AccountIsolation;
begin
	{ Different accounts should have isolated job counters }
	FManager.IncrementBackgroundJobs('account1@mail.ru');
	FManager.IncrementBackgroundJobs('account1@mail.ru');
	FManager.IncrementBackgroundJobs('account2@mail.ru');

	Assert.AreEqual(2, FManager.GetBackgroundJobsCount('account1@mail.ru'));
	Assert.AreEqual(1, FManager.GetBackgroundJobsCount('account2@mail.ru'));
	Assert.AreEqual(0, FManager.GetBackgroundJobsCount('account3@mail.ru'));
end;

{ HasAnyActiveOperations tests }

procedure TThreadStateManagerTest.TestHasAnyActiveOperations_FalseWhenEmpty;
begin
	{ Fresh manager has no active operations }
	Assert.IsFalse(FManager.HasAnyActiveOperations);
end;

procedure TThreadStateManagerTest.TestHasAnyActiveOperations_TrueWithBackgroundThread;
begin
	Assert.IsFalse(FManager.HasAnyActiveOperations);

	{ Adding a background thread makes operations active }
	FManager.SetBackgroundThreadStatus(1);
	Assert.IsTrue(FManager.HasAnyActiveOperations);

	{ Removing it clears active state }
	FManager.RemoveBackgroundThread;
	Assert.IsFalse(FManager.HasAnyActiveOperations);
end;

procedure TThreadStateManagerTest.TestHasAnyActiveOperations_TrueWithActiveBackgroundJobs;
begin
	Assert.IsFalse(FManager.HasAnyActiveOperations);

	{ Adding background jobs makes operations active }
	FManager.IncrementBackgroundJobs('test@mail.ru');
	Assert.IsTrue(FManager.HasAnyActiveOperations);

	{ Decrementing to zero clears active state }
	FManager.DecrementBackgroundJobs('test@mail.ru');
	Assert.IsFalse(FManager.HasAnyActiveOperations);
end;

procedure TThreadStateManagerTest.TestHasAnyActiveOperations_FalseWithZeroJobCount;
begin
	{ An account entry with zero count should not be considered active }
	FManager.IncrementBackgroundJobs('test@mail.ru');
	FManager.DecrementBackgroundJobs('test@mail.ru');
	{ Now there's an entry in the dictionary with value 0 }
	Assert.IsFalse(FManager.HasAnyActiveOperations);
end;

{ Edge cases }

procedure TThreadStateManagerTest.TestIsPathSkipped_NoSkippedPathList_ReturnsFalse;
begin
	{ IsPathSkipped should return False when no skipped path list was created }
	Assert.IsFalse(FManager.IsPathSkipped('\some\path'));
end;

procedure TThreadStateManagerTest.TestDecrementBackgroundJobs_UnknownAccount_SetsNegative;
begin
	{ Decrementing on unknown account starts from 0, resulting in -1 }
	FManager.DecrementBackgroundJobs('unknown@mail.ru');
	Assert.AreEqual(-1, FManager.GetBackgroundJobsCount('unknown@mail.ru'));
end;

procedure TThreadStateManagerTest.TestCreateRemoveDirSkippedPath_DoubleCreate_NoLeak;
begin
	{Calling CreateRemoveDirSkippedPath twice must not leak the first TStringList.
		We verify the second call replaces cleanly and the list is functional.}
	FManager.CreateRemoveDirSkippedPath;
	FManager.AddSkippedPath('\account\folder1');
	Assert.AreEqual(1, FManager.GetRemoveDirSkippedPath.Count);

	{Second create should free the old list and start fresh}
	FManager.CreateRemoveDirSkippedPath;
	Assert.AreEqual(0, FManager.GetRemoveDirSkippedPath.Count, 'Double-create should produce a fresh empty list');
	Assert.IsTrue(FManager.HasRemoveDirSkippedPath);

	{New list should be fully functional}
	FManager.AddSkippedPath('\account\folder2');
	Assert.IsTrue(FManager.IsPathSkipped('\account\folder2'));
	Assert.IsFalse(FManager.IsPathSkipped('\account\folder1'), 'Old entries must not survive double-create');
end;

initialization
	TDUnitX.RegisterTestFixture(TThreadStateManagerTest);

end.
