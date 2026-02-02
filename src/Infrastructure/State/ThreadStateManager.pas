unit ThreadStateManager;

{Per-thread state management for WFX plugin.
	Encapsulates 11 TDictionary instances with TCriticalSection synchronization.
	All methods implicitly use GetCurrentThreadID() for thread-keyed operations,
	except background jobs which are keyed by account name.}

interface

uses
	Windows, SysUtils, Classes, Generics.Collections, SyncObjs;

type
	{Interface for managing per-thread state in WFX plugin.
		All methods implicitly use GetCurrentThreadID() for thread-keyed operations,
		except background jobs which are keyed by account name.}
	IThreadStateManager = interface
		['{30E7A013-7529-4A45-B122-99A218FCCE6C}']

		{Check if any background operations are active (threads or jobs).
			Used to determine if it's safe to perform cleanup during DLL unload.}
		function HasAnyActiveOperations: Boolean;

		{Skip listing flags - prevent directory enumeration during bulk operations}
		function GetSkipListDelete: Boolean;
		procedure SetSkipListDelete(Value: Boolean);
		function GetSkipListRenMov: Boolean;
		procedure SetSkipListRenMov(Value: Boolean);

		{Abort control - enable cancel dialog during long operations (issue #113)}
		function GetCanAbortRenMov: Boolean;
		procedure SetCanAbortRenMov(Value: Boolean);
		function GetListingAborted: Boolean;
		procedure SetListingAborted(Value: Boolean);

		{Retry counters - track retry attempts per operation type}
		function GetRetryCountDownload: Int32;
		procedure SetRetryCountDownload(Value: Int32);
		procedure IncrementRetryCountDownload;
		procedure ResetRetryCountDownload;

		function GetRetryCountUpload: Int32;
		procedure SetRetryCountUpload(Value: Int32);
		procedure IncrementRetryCountUpload;
		procedure ResetRetryCountUpload;

		function GetRetryCountRenMov: Int32;
		procedure SetRetryCountRenMov(Value: Int32);
		procedure IncrementRetryCountRenMov;
		procedure ResetRetryCountRenMov;

		{Operation context - track current filesystem operation for context-aware decisions}
		function GetFsStatusInfo: Int32;
		procedure SetFsStatusInfo(Value: Int32);
		procedure RemoveFsStatusInfo;
		function HasFsStatusInfo: Boolean;

		{Background thread tracking - track operation status per thread}
		function GetBackgroundThreadStatus: Int32;
		procedure SetBackgroundThreadStatus(Value: Int32);
		procedure RemoveBackgroundThread;
		function HasBackgroundThread: Boolean;

		{Path blacklist - paths to skip during move operations (issue #168).
			Ownership: TStringList created internally, caller should not free it.}
		function GetRemoveDirSkippedPath: TStringList;
		procedure CreateRemoveDirSkippedPath;
		procedure ClearRemoveDirSkippedPath;
		function HasRemoveDirSkippedPath: Boolean;
		function IsPathSkipped(const Path: WideString): Boolean;
		procedure AddSkippedPath(const Path: WideString);
		procedure RemoveSkippedPath(const Path: WideString);

		{Background jobs - track active jobs per account (keyed by account name, not thread ID).
			Used to prevent connection pool cleanup while operations are in progress.}
		function GetBackgroundJobsCount(const Account: WideString): Int32;
		procedure IncrementBackgroundJobs(const Account: WideString);
		procedure DecrementBackgroundJobs(const Account: WideString);
		function HasActiveBackgroundJobs(const Account: WideString): Boolean;
	end;

	{Centralized manager for per-thread state in WFX plugin.
		Encapsulates 11 TDictionary instances previously scattered in TWFXApplication.
		Thread-safe through a single TCriticalSection guarding all dictionary access.}
	TThreadStateManager = class(TInterfacedObject, IThreadStateManager)
	private
		FLock: TCriticalSection;
		FSkipListDelete: TDictionary<DWORD, Boolean>;
		FSkipListRenMov: TDictionary<DWORD, Boolean>;
		FCanAbortRenMov: TDictionary<DWORD, Boolean>;
		FListingAborted: TDictionary<DWORD, Boolean>;
		FRetryCountDownload: TDictionary<DWORD, Int32>;
		FRetryCountUpload: TDictionary<DWORD, Int32>;
		FRetryCountRenMov: TDictionary<DWORD, Int32>;
		FBackgroundJobs: TDictionary<WideString, Int32>;
		FBackgroundThreads: TDictionary<DWORD, Int32>;
		FFsStatusInfo: TDictionary<DWORD, Int32>;
		FRemoveDirSkippedPath: TDictionary<DWORD, TStringList>;
	public
		constructor Create;
		destructor Destroy; override;

		{Check if any background operations are active}
		function HasAnyActiveOperations: Boolean;

		{Skip listing flags}
		function GetSkipListDelete: Boolean;
		procedure SetSkipListDelete(Value: Boolean);
		function GetSkipListRenMov: Boolean;
		procedure SetSkipListRenMov(Value: Boolean);

		{Abort control}
		function GetCanAbortRenMov: Boolean;
		procedure SetCanAbortRenMov(Value: Boolean);
		function GetListingAborted: Boolean;
		procedure SetListingAborted(Value: Boolean);

		{Retry counters}
		function GetRetryCountDownload: Int32;
		procedure SetRetryCountDownload(Value: Int32);
		procedure IncrementRetryCountDownload;
		procedure ResetRetryCountDownload;

		function GetRetryCountUpload: Int32;
		procedure SetRetryCountUpload(Value: Int32);
		procedure IncrementRetryCountUpload;
		procedure ResetRetryCountUpload;

		function GetRetryCountRenMov: Int32;
		procedure SetRetryCountRenMov(Value: Int32);
		procedure IncrementRetryCountRenMov;
		procedure ResetRetryCountRenMov;

		{Operation context}
		function GetFsStatusInfo: Int32;
		procedure SetFsStatusInfo(Value: Int32);
		procedure RemoveFsStatusInfo;
		function HasFsStatusInfo: Boolean;

		{Background thread tracking}
		function GetBackgroundThreadStatus: Int32;
		procedure SetBackgroundThreadStatus(Value: Int32);
		procedure RemoveBackgroundThread;
		function HasBackgroundThread: Boolean;

		{Path blacklist}
		function GetRemoveDirSkippedPath: TStringList;
		procedure CreateRemoveDirSkippedPath;
		procedure ClearRemoveDirSkippedPath;
		function HasRemoveDirSkippedPath: Boolean;
		function IsPathSkipped(const Path: WideString): Boolean;
		procedure AddSkippedPath(const Path: WideString);
		procedure RemoveSkippedPath(const Path: WideString);

		{Background jobs (account-keyed)}
		function GetBackgroundJobsCount(const Account: WideString): Int32;
		procedure IncrementBackgroundJobs(const Account: WideString);
		procedure DecrementBackgroundJobs(const Account: WideString);
		function HasActiveBackgroundJobs(const Account: WideString): Boolean;
	end;

implementation

constructor TThreadStateManager.Create;
begin
	inherited Create;
	FLock := TCriticalSection.Create;
	FSkipListDelete := TDictionary<DWORD, Boolean>.Create;
	FSkipListRenMov := TDictionary<DWORD, Boolean>.Create;
	FCanAbortRenMov := TDictionary<DWORD, Boolean>.Create;
	FListingAborted := TDictionary<DWORD, Boolean>.Create;
	FRetryCountDownload := TDictionary<DWORD, Int32>.Create;
	FRetryCountUpload := TDictionary<DWORD, Int32>.Create;
	FRetryCountRenMov := TDictionary<DWORD, Int32>.Create;
	FBackgroundJobs := TDictionary<WideString, Int32>.Create;
	FBackgroundThreads := TDictionary<DWORD, Int32>.Create;
	FFsStatusInfo := TDictionary<DWORD, Int32>.Create;
	FRemoveDirSkippedPath := TDictionary<DWORD, TStringList>.Create;
end;

destructor TThreadStateManager.Destroy;
var
	Pair: TPair<DWORD, TStringList>;
begin
	{Free TStringList values before dictionary - may remain if operations were interrupted}
	for Pair in FRemoveDirSkippedPath do
		Pair.Value.Free;

	FreeAndNil(FRemoveDirSkippedPath);
	FreeAndNil(FFsStatusInfo);
	FreeAndNil(FBackgroundThreads);
	FreeAndNil(FBackgroundJobs);
	FreeAndNil(FRetryCountRenMov);
	FreeAndNil(FRetryCountUpload);
	FreeAndNil(FRetryCountDownload);
	FreeAndNil(FListingAborted);
	FreeAndNil(FCanAbortRenMov);
	FreeAndNil(FSkipListRenMov);
	FreeAndNil(FSkipListDelete);
	FreeAndNil(FLock);
	inherited;
end;

function TThreadStateManager.HasAnyActiveOperations: Boolean;
var
	JobCount: Int32;
begin
	FLock.Enter;
	try
		{Check if any background threads are tracked}
		if FBackgroundThreads.Count > 0 then
			Exit(True);
		{Check if any background jobs are active (count > 0 for any account)}
		for JobCount in FBackgroundJobs.Values do
			if JobCount > 0 then
				Exit(True);
		Result := False;
	finally
		FLock.Leave;
	end;
end;

{Skip listing flags}

function TThreadStateManager.GetSkipListDelete: Boolean;
begin
	FLock.Enter;
	try
		if not FSkipListDelete.TryGetValue(GetCurrentThreadID(), Result) then
			Result := False;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.SetSkipListDelete(Value: Boolean);
begin
	FLock.Enter;
	try
		FSkipListDelete.AddOrSetValue(GetCurrentThreadID(), Value);
	finally
		FLock.Leave;
	end;
end;

function TThreadStateManager.GetSkipListRenMov: Boolean;
begin
	FLock.Enter;
	try
		if not FSkipListRenMov.TryGetValue(GetCurrentThreadID(), Result) then
			Result := False;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.SetSkipListRenMov(Value: Boolean);
begin
	FLock.Enter;
	try
		FSkipListRenMov.AddOrSetValue(GetCurrentThreadID(), Value);
	finally
		FLock.Leave;
	end;
end;

{Abort control}

function TThreadStateManager.GetCanAbortRenMov: Boolean;
begin
	FLock.Enter;
	try
		if not FCanAbortRenMov.TryGetValue(GetCurrentThreadID(), Result) then
			Result := False;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.SetCanAbortRenMov(Value: Boolean);
begin
	FLock.Enter;
	try
		FCanAbortRenMov.AddOrSetValue(GetCurrentThreadID(), Value);
	finally
		FLock.Leave;
	end;
end;

function TThreadStateManager.GetListingAborted: Boolean;
begin
	FLock.Enter;
	try
		if not FListingAborted.TryGetValue(GetCurrentThreadID(), Result) then
			Result := False;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.SetListingAborted(Value: Boolean);
begin
	FLock.Enter;
	try
		FListingAborted.AddOrSetValue(GetCurrentThreadID(), Value);
	finally
		FLock.Leave;
	end;
end;

{Retry counters}

function TThreadStateManager.GetRetryCountDownload: Int32;
begin
	FLock.Enter;
	try
		if not FRetryCountDownload.TryGetValue(GetCurrentThreadID(), Result) then
			Result := 0;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.SetRetryCountDownload(Value: Int32);
begin
	FLock.Enter;
	try
		FRetryCountDownload.AddOrSetValue(GetCurrentThreadID(), Value);
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.IncrementRetryCountDownload;
var
	Current: Int32;
begin
	FLock.Enter;
	try
		if not FRetryCountDownload.TryGetValue(GetCurrentThreadID(), Current) then
			Current := 0;
		FRetryCountDownload.AddOrSetValue(GetCurrentThreadID(), Current + 1);
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.ResetRetryCountDownload;
begin
	FLock.Enter;
	try
		FRetryCountDownload.AddOrSetValue(GetCurrentThreadID(), 0);
	finally
		FLock.Leave;
	end;
end;

function TThreadStateManager.GetRetryCountUpload: Int32;
begin
	FLock.Enter;
	try
		if not FRetryCountUpload.TryGetValue(GetCurrentThreadID(), Result) then
			Result := 0;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.SetRetryCountUpload(Value: Int32);
begin
	FLock.Enter;
	try
		FRetryCountUpload.AddOrSetValue(GetCurrentThreadID(), Value);
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.IncrementRetryCountUpload;
var
	Current: Int32;
begin
	FLock.Enter;
	try
		if not FRetryCountUpload.TryGetValue(GetCurrentThreadID(), Current) then
			Current := 0;
		FRetryCountUpload.AddOrSetValue(GetCurrentThreadID(), Current + 1);
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.ResetRetryCountUpload;
begin
	FLock.Enter;
	try
		FRetryCountUpload.AddOrSetValue(GetCurrentThreadID(), 0);
	finally
		FLock.Leave;
	end;
end;

function TThreadStateManager.GetRetryCountRenMov: Int32;
begin
	FLock.Enter;
	try
		if not FRetryCountRenMov.TryGetValue(GetCurrentThreadID(), Result) then
			Result := 0;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.SetRetryCountRenMov(Value: Int32);
begin
	FLock.Enter;
	try
		FRetryCountRenMov.AddOrSetValue(GetCurrentThreadID(), Value);
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.IncrementRetryCountRenMov;
var
	Current: Int32;
begin
	FLock.Enter;
	try
		if not FRetryCountRenMov.TryGetValue(GetCurrentThreadID(), Current) then
			Current := 0;
		FRetryCountRenMov.AddOrSetValue(GetCurrentThreadID(), Current + 1);
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.ResetRetryCountRenMov;
begin
	FLock.Enter;
	try
		FRetryCountRenMov.AddOrSetValue(GetCurrentThreadID(), 0);
	finally
		FLock.Leave;
	end;
end;

{Operation context}

function TThreadStateManager.GetFsStatusInfo: Int32;
begin
	FLock.Enter;
	try
		if not FFsStatusInfo.TryGetValue(GetCurrentThreadID(), Result) then
			Result := 0;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.SetFsStatusInfo(Value: Int32);
begin
	FLock.Enter;
	try
		FFsStatusInfo.AddOrSetValue(GetCurrentThreadID(), Value);
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.RemoveFsStatusInfo;
begin
	FLock.Enter;
	try
		FFsStatusInfo.Remove(GetCurrentThreadID());
	finally
		FLock.Leave;
	end;
end;

function TThreadStateManager.HasFsStatusInfo: Boolean;
begin
	FLock.Enter;
	try
		Result := FFsStatusInfo.ContainsKey(GetCurrentThreadID());
	finally
		FLock.Leave;
	end;
end;

{Background thread tracking}

function TThreadStateManager.GetBackgroundThreadStatus: Int32;
begin
	FLock.Enter;
	try
		if not FBackgroundThreads.TryGetValue(GetCurrentThreadID(), Result) then
			Result := 0;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.SetBackgroundThreadStatus(Value: Int32);
begin
	FLock.Enter;
	try
		FBackgroundThreads.AddOrSetValue(GetCurrentThreadID(), Value);
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.RemoveBackgroundThread;
begin
	FLock.Enter;
	try
		FBackgroundThreads.Remove(GetCurrentThreadID());
	finally
		FLock.Leave;
	end;
end;

function TThreadStateManager.HasBackgroundThread: Boolean;
begin
	FLock.Enter;
	try
		Result := FBackgroundThreads.ContainsKey(GetCurrentThreadID());
	finally
		FLock.Leave;
	end;
end;

{Path blacklist}

function TThreadStateManager.GetRemoveDirSkippedPath: TStringList;
begin
	FLock.Enter;
	try
		if not FRemoveDirSkippedPath.TryGetValue(GetCurrentThreadID(), Result) then
			Result := nil;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.CreateRemoveDirSkippedPath;
begin
	FLock.Enter;
	try
		FRemoveDirSkippedPath.AddOrSetValue(GetCurrentThreadID(), TStringList.Create());
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.ClearRemoveDirSkippedPath;
var
	SkippedPath: TStringList;
begin
	FLock.Enter;
	try
		if FRemoveDirSkippedPath.TryGetValue(GetCurrentThreadID(), SkippedPath) then
		begin
			SkippedPath.Free;
			FRemoveDirSkippedPath.Remove(GetCurrentThreadID());
		end;
	finally
		FLock.Leave;
	end;
end;

function TThreadStateManager.HasRemoveDirSkippedPath: Boolean;
begin
	FLock.Enter;
	try
		Result := FRemoveDirSkippedPath.ContainsKey(GetCurrentThreadID());
	finally
		FLock.Leave;
	end;
end;

function TThreadStateManager.IsPathSkipped(const Path: WideString): Boolean;
var
	SkippedPath: TStringList;
begin
	FLock.Enter;
	try
		if FRemoveDirSkippedPath.TryGetValue(GetCurrentThreadID(), SkippedPath) then
			Result := SkippedPath.Text.Contains(Path)
		else
			Result := False;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.AddSkippedPath(const Path: WideString);
var
	SkippedPath: TStringList;
begin
	FLock.Enter;
	try
		if FRemoveDirSkippedPath.TryGetValue(GetCurrentThreadID(), SkippedPath) then
			SkippedPath.Add(Path);
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.RemoveSkippedPath(const Path: WideString);
var
	SkippedPath: TStringList;
	Index: Integer;
begin
	FLock.Enter;
	try
		if FRemoveDirSkippedPath.TryGetValue(GetCurrentThreadID(), SkippedPath) then
		begin
			Index := SkippedPath.IndexOf(Path);
			if Index >= 0 then
				SkippedPath.Delete(Index);
		end;
	finally
		FLock.Leave;
	end;
end;

{Background jobs (account-keyed)}

function TThreadStateManager.GetBackgroundJobsCount(const Account: WideString): Int32;
begin
	FLock.Enter;
	try
		if not FBackgroundJobs.TryGetValue(Account, Result) then
			Result := 0;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.IncrementBackgroundJobs(const Account: WideString);
var
	Current: Int32;
begin
	FLock.Enter;
	try
		if not FBackgroundJobs.TryGetValue(Account, Current) then
			Current := 0;
		FBackgroundJobs.AddOrSetValue(Account, Current + 1);
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.DecrementBackgroundJobs(const Account: WideString);
var
	Current: Int32;
begin
	FLock.Enter;
	try
		if not FBackgroundJobs.TryGetValue(Account, Current) then
			Current := 0;
		FBackgroundJobs.AddOrSetValue(Account, Current - 1);
	finally
		FLock.Leave;
	end;
end;

function TThreadStateManager.HasActiveBackgroundJobs(const Account: WideString): Boolean;
var
	Current: Int32;
begin
	FLock.Enter;
	try
		if not FBackgroundJobs.TryGetValue(Account, Current) then
			Current := 0;
		Result := Current > 0;
	finally
		FLock.Leave;
	end;
end;

end.
