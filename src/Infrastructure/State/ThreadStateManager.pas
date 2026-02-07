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
		procedure IncrementRetryCountDownload;
		procedure ResetRetryCountDownload;

		function GetRetryCountUpload: Int32;
		procedure IncrementRetryCountUpload;
		procedure ResetRetryCountUpload;

		function GetRetryCountRenMov: Int32;
		procedure IncrementRetryCountRenMov;
		procedure ResetRetryCountRenMov;

		{Operation context - track current filesystem operation for context-aware decisions}
		function GetFsStatusInfo: Int32;
		procedure SetFsStatusInfo(Value: Int32);
		procedure RemoveFsStatusInfo;

		{Background thread tracking - track operation status per thread}
		procedure SetBackgroundThreadStatus(Value: Int32);
		procedure RemoveBackgroundThread;

		{Path blacklist - paths to skip during move operations (issue #168).
			Ownership: TStringList created internally, caller should not free it.}
		procedure CreateRemoveDirSkippedPath;
		procedure ClearRemoveDirSkippedPath;
		function HasRemoveDirSkippedPath: Boolean;
		function IsPathSkipped(const Path: WideString): Boolean;
		procedure AddSkippedPath(const Path: WideString);
		procedure RemoveSkippedPath(const Path: WideString);

		{Background jobs - track active jobs per account (keyed by account name, not thread ID).
			Used to prevent connection pool cleanup while operations are in progress.}
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

		{Private helpers to reduce boilerplate - all use GetCurrentThreadID as key}
		function GetThreadBoolean(Dict: TDictionary<DWORD, Boolean>): Boolean;
		procedure SetThreadBoolean(Dict: TDictionary<DWORD, Boolean>; Value: Boolean);
		function GetThreadInt32(Dict: TDictionary<DWORD, Int32>): Int32;
		procedure SetThreadInt32(Dict: TDictionary<DWORD, Int32>; Value: Int32);
		procedure IncrementThreadInt32(Dict: TDictionary<DWORD, Int32>);
		procedure ResetThreadInt32(Dict: TDictionary<DWORD, Int32>);
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

{Private helpers - consolidate thread-keyed dictionary operations}

function TThreadStateManager.GetThreadBoolean(Dict: TDictionary<DWORD, Boolean>): Boolean;
begin
	FLock.Enter;
	try
		if not Dict.TryGetValue(GetCurrentThreadID(), Result) then
			Result := False;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.SetThreadBoolean(Dict: TDictionary<DWORD, Boolean>; Value: Boolean);
begin
	FLock.Enter;
	try
		Dict.AddOrSetValue(GetCurrentThreadID(), Value);
	finally
		FLock.Leave;
	end;
end;

function TThreadStateManager.GetThreadInt32(Dict: TDictionary<DWORD, Int32>): Int32;
begin
	FLock.Enter;
	try
		if not Dict.TryGetValue(GetCurrentThreadID(), Result) then
			Result := 0;
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.SetThreadInt32(Dict: TDictionary<DWORD, Int32>; Value: Int32);
begin
	FLock.Enter;
	try
		Dict.AddOrSetValue(GetCurrentThreadID(), Value);
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.IncrementThreadInt32(Dict: TDictionary<DWORD, Int32>);
var
	Current: Int32;
begin
	FLock.Enter;
	try
		if not Dict.TryGetValue(GetCurrentThreadID(), Current) then
			Current := 0;
		Dict.AddOrSetValue(GetCurrentThreadID(), Current + 1);
	finally
		FLock.Leave;
	end;
end;

procedure TThreadStateManager.ResetThreadInt32(Dict: TDictionary<DWORD, Int32>);
begin
	FLock.Enter;
	try
		Dict.AddOrSetValue(GetCurrentThreadID(), 0);
	finally
		FLock.Leave;
	end;
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
	Result := GetThreadBoolean(FSkipListDelete);
end;

procedure TThreadStateManager.SetSkipListDelete(Value: Boolean);
begin
	SetThreadBoolean(FSkipListDelete, Value);
end;

function TThreadStateManager.GetSkipListRenMov: Boolean;
begin
	Result := GetThreadBoolean(FSkipListRenMov);
end;

procedure TThreadStateManager.SetSkipListRenMov(Value: Boolean);
begin
	SetThreadBoolean(FSkipListRenMov, Value);
end;

{Abort control}

function TThreadStateManager.GetCanAbortRenMov: Boolean;
begin
	Result := GetThreadBoolean(FCanAbortRenMov);
end;

procedure TThreadStateManager.SetCanAbortRenMov(Value: Boolean);
begin
	SetThreadBoolean(FCanAbortRenMov, Value);
end;

function TThreadStateManager.GetListingAborted: Boolean;
begin
	Result := GetThreadBoolean(FListingAborted);
end;

procedure TThreadStateManager.SetListingAborted(Value: Boolean);
begin
	SetThreadBoolean(FListingAborted, Value);
end;

{Retry counters}

function TThreadStateManager.GetRetryCountDownload: Int32;
begin
	Result := GetThreadInt32(FRetryCountDownload);
end;

procedure TThreadStateManager.SetRetryCountDownload(Value: Int32);
begin
	SetThreadInt32(FRetryCountDownload, Value);
end;

procedure TThreadStateManager.IncrementRetryCountDownload;
begin
	IncrementThreadInt32(FRetryCountDownload);
end;

procedure TThreadStateManager.ResetRetryCountDownload;
begin
	ResetThreadInt32(FRetryCountDownload);
end;

function TThreadStateManager.GetRetryCountUpload: Int32;
begin
	Result := GetThreadInt32(FRetryCountUpload);
end;

procedure TThreadStateManager.SetRetryCountUpload(Value: Int32);
begin
	SetThreadInt32(FRetryCountUpload, Value);
end;

procedure TThreadStateManager.IncrementRetryCountUpload;
begin
	IncrementThreadInt32(FRetryCountUpload);
end;

procedure TThreadStateManager.ResetRetryCountUpload;
begin
	ResetThreadInt32(FRetryCountUpload);
end;

function TThreadStateManager.GetRetryCountRenMov: Int32;
begin
	Result := GetThreadInt32(FRetryCountRenMov);
end;

procedure TThreadStateManager.SetRetryCountRenMov(Value: Int32);
begin
	SetThreadInt32(FRetryCountRenMov, Value);
end;

procedure TThreadStateManager.IncrementRetryCountRenMov;
begin
	IncrementThreadInt32(FRetryCountRenMov);
end;

procedure TThreadStateManager.ResetRetryCountRenMov;
begin
	ResetThreadInt32(FRetryCountRenMov);
end;

{Operation context}

function TThreadStateManager.GetFsStatusInfo: Int32;
begin
	Result := GetThreadInt32(FFsStatusInfo);
end;

procedure TThreadStateManager.SetFsStatusInfo(Value: Int32);
begin
	SetThreadInt32(FFsStatusInfo, Value);
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
	Result := GetThreadInt32(FBackgroundThreads);
end;

procedure TThreadStateManager.SetBackgroundThreadStatus(Value: Int32);
begin
	SetThreadInt32(FBackgroundThreads, Value);
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
var
	Existing: TStringList;
begin
	FLock.Enter;
	try
		if FRemoveDirSkippedPath.TryGetValue(GetCurrentThreadID(), Existing) then
			Existing.Free;
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
