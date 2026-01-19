unit ThreadStateManager;

interface

uses
	Windows, SysUtils, Classes, Generics.Collections,
	IThreadStateManagerInterface;

type
	{ Centralized manager for per-thread state in WFX plugin.
	  Encapsulates 11 TDictionary instances previously scattered in TMailRuCloudWFX.
	  Thread-safe through TDictionary keyed by GetCurrentThreadID(). }
	TThreadStateManager = class(TInterfacedObject, IThreadStateManager)
	private
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

		{ Skip listing flags }
		function GetSkipListDelete: Boolean;
		procedure SetSkipListDelete(Value: Boolean);
		function GetSkipListRenMov: Boolean;
		procedure SetSkipListRenMov(Value: Boolean);

		{ Abort control }
		function GetCanAbortRenMov: Boolean;
		procedure SetCanAbortRenMov(Value: Boolean);
		function GetListingAborted: Boolean;
		procedure SetListingAborted(Value: Boolean);

		{ Retry counters }
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

		{ Operation context }
		function GetFsStatusInfo: Int32;
		procedure SetFsStatusInfo(Value: Int32);
		procedure RemoveFsStatusInfo;
		function HasFsStatusInfo: Boolean;

		{ Background thread tracking }
		function GetBackgroundThreadStatus: Int32;
		procedure SetBackgroundThreadStatus(Value: Int32);
		procedure RemoveBackgroundThread;
		function HasBackgroundThread: Boolean;

		{ Path blacklist }
		function GetRemoveDirSkippedPath: TStringList;
		procedure CreateRemoveDirSkippedPath;
		procedure ClearRemoveDirSkippedPath;
		function HasRemoveDirSkippedPath: Boolean;
		function IsPathSkipped(const Path: WideString): Boolean;
		procedure AddSkippedPath(const Path: WideString);
		procedure RemoveSkippedPath(const Path: WideString);

		{ Background jobs (account-keyed) }
		function GetBackgroundJobsCount(const Account: WideString): Int32;
		procedure IncrementBackgroundJobs(const Account: WideString);
		procedure DecrementBackgroundJobs(const Account: WideString);
		function HasActiveBackgroundJobs(const Account: WideString): Boolean;
	end;

implementation

constructor TThreadStateManager.Create;
begin
	inherited Create;
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
	{ Free TStringList values before dictionary - may remain if operations were interrupted }
	if Assigned(FRemoveDirSkippedPath) then
		for Pair in FRemoveDirSkippedPath do
			if Assigned(Pair.Value) then
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
	inherited;
end;

{ Skip listing flags }

function TThreadStateManager.GetSkipListDelete: Boolean;
begin
	if not FSkipListDelete.TryGetValue(GetCurrentThreadID(), Result) then
		Result := False;
end;

procedure TThreadStateManager.SetSkipListDelete(Value: Boolean);
begin
	FSkipListDelete.AddOrSetValue(GetCurrentThreadID(), Value);
end;

function TThreadStateManager.GetSkipListRenMov: Boolean;
begin
	if not FSkipListRenMov.TryGetValue(GetCurrentThreadID(), Result) then
		Result := False;
end;

procedure TThreadStateManager.SetSkipListRenMov(Value: Boolean);
begin
	FSkipListRenMov.AddOrSetValue(GetCurrentThreadID(), Value);
end;

{ Abort control }

function TThreadStateManager.GetCanAbortRenMov: Boolean;
begin
	if not FCanAbortRenMov.TryGetValue(GetCurrentThreadID(), Result) then
		Result := False;
end;

procedure TThreadStateManager.SetCanAbortRenMov(Value: Boolean);
begin
	FCanAbortRenMov.AddOrSetValue(GetCurrentThreadID(), Value);
end;

function TThreadStateManager.GetListingAborted: Boolean;
begin
	if not FListingAborted.TryGetValue(GetCurrentThreadID(), Result) then
		Result := False;
end;

procedure TThreadStateManager.SetListingAborted(Value: Boolean);
begin
	FListingAborted.AddOrSetValue(GetCurrentThreadID(), Value);
end;

{ Retry counters }

function TThreadStateManager.GetRetryCountDownload: Int32;
begin
	if not FRetryCountDownload.TryGetValue(GetCurrentThreadID(), Result) then
		Result := 0;
end;

procedure TThreadStateManager.SetRetryCountDownload(Value: Int32);
begin
	FRetryCountDownload.AddOrSetValue(GetCurrentThreadID(), Value);
end;

procedure TThreadStateManager.IncrementRetryCountDownload;
begin
	SetRetryCountDownload(GetRetryCountDownload + 1);
end;

procedure TThreadStateManager.ResetRetryCountDownload;
begin
	SetRetryCountDownload(0);
end;

function TThreadStateManager.GetRetryCountUpload: Int32;
begin
	if not FRetryCountUpload.TryGetValue(GetCurrentThreadID(), Result) then
		Result := 0;
end;

procedure TThreadStateManager.SetRetryCountUpload(Value: Int32);
begin
	FRetryCountUpload.AddOrSetValue(GetCurrentThreadID(), Value);
end;

procedure TThreadStateManager.IncrementRetryCountUpload;
begin
	SetRetryCountUpload(GetRetryCountUpload + 1);
end;

procedure TThreadStateManager.ResetRetryCountUpload;
begin
	SetRetryCountUpload(0);
end;

function TThreadStateManager.GetRetryCountRenMov: Int32;
begin
	if not FRetryCountRenMov.TryGetValue(GetCurrentThreadID(), Result) then
		Result := 0;
end;

procedure TThreadStateManager.SetRetryCountRenMov(Value: Int32);
begin
	FRetryCountRenMov.AddOrSetValue(GetCurrentThreadID(), Value);
end;

procedure TThreadStateManager.IncrementRetryCountRenMov;
begin
	SetRetryCountRenMov(GetRetryCountRenMov + 1);
end;

procedure TThreadStateManager.ResetRetryCountRenMov;
begin
	SetRetryCountRenMov(0);
end;

{ Operation context }

function TThreadStateManager.GetFsStatusInfo: Int32;
begin
	if not FFsStatusInfo.TryGetValue(GetCurrentThreadID(), Result) then
		Result := 0;
end;

procedure TThreadStateManager.SetFsStatusInfo(Value: Int32);
begin
	FFsStatusInfo.AddOrSetValue(GetCurrentThreadID(), Value);
end;

procedure TThreadStateManager.RemoveFsStatusInfo;
begin
	FFsStatusInfo.Remove(GetCurrentThreadID());
end;

function TThreadStateManager.HasFsStatusInfo: Boolean;
begin
	Result := FFsStatusInfo.ContainsKey(GetCurrentThreadID());
end;

{ Background thread tracking }

function TThreadStateManager.GetBackgroundThreadStatus: Int32;
begin
	if not FBackgroundThreads.TryGetValue(GetCurrentThreadID(), Result) then
		Result := 0;
end;

procedure TThreadStateManager.SetBackgroundThreadStatus(Value: Int32);
begin
	FBackgroundThreads.AddOrSetValue(GetCurrentThreadID(), Value);
end;

procedure TThreadStateManager.RemoveBackgroundThread;
begin
	FBackgroundThreads.Remove(GetCurrentThreadID());
end;

function TThreadStateManager.HasBackgroundThread: Boolean;
begin
	Result := FBackgroundThreads.ContainsKey(GetCurrentThreadID());
end;

{ Path blacklist }

function TThreadStateManager.GetRemoveDirSkippedPath: TStringList;
begin
	if not FRemoveDirSkippedPath.TryGetValue(GetCurrentThreadID(), Result) then
		Result := nil;
end;

procedure TThreadStateManager.CreateRemoveDirSkippedPath;
begin
	FRemoveDirSkippedPath.AddOrSetValue(GetCurrentThreadID(), TStringList.Create());
end;

procedure TThreadStateManager.ClearRemoveDirSkippedPath;
var
	SkippedPath: TStringList;
begin
	if FRemoveDirSkippedPath.TryGetValue(GetCurrentThreadID(), SkippedPath) then
	begin
		if Assigned(SkippedPath) then
			SkippedPath.Free;
		FRemoveDirSkippedPath.AddOrSetValue(GetCurrentThreadID(), nil);
	end;
end;

function TThreadStateManager.HasRemoveDirSkippedPath: Boolean;
var
	SkippedPath: TStringList;
begin
	Result := FRemoveDirSkippedPath.TryGetValue(GetCurrentThreadID(), SkippedPath) and Assigned(SkippedPath);
end;

function TThreadStateManager.IsPathSkipped(const Path: WideString): Boolean;
var
	SkippedPath: TStringList;
begin
	Result := False;
	if FRemoveDirSkippedPath.TryGetValue(GetCurrentThreadID(), SkippedPath) then
		if Assigned(SkippedPath) then
			Result := SkippedPath.Text.Contains(Path);
end;

procedure TThreadStateManager.AddSkippedPath(const Path: WideString);
var
	SkippedPath: TStringList;
begin
	if FRemoveDirSkippedPath.TryGetValue(GetCurrentThreadID(), SkippedPath) then
		if Assigned(SkippedPath) then
			SkippedPath.Add(Path);
end;

procedure TThreadStateManager.RemoveSkippedPath(const Path: WideString);
var
	SkippedPath: TStringList;
	Index: Integer;
begin
	if FRemoveDirSkippedPath.TryGetValue(GetCurrentThreadID(), SkippedPath) then
		if Assigned(SkippedPath) then
		begin
			Index := SkippedPath.IndexOf(Path);
			if Index >= 0 then
				SkippedPath.Delete(Index);
		end;
end;

{ Background jobs (account-keyed) }

function TThreadStateManager.GetBackgroundJobsCount(const Account: WideString): Int32;
begin
	if not FBackgroundJobs.TryGetValue(Account, Result) then
		Result := 0;
end;

procedure TThreadStateManager.IncrementBackgroundJobs(const Account: WideString);
begin
	FBackgroundJobs.AddOrSetValue(Account, GetBackgroundJobsCount(Account) + 1);
end;

procedure TThreadStateManager.DecrementBackgroundJobs(const Account: WideString);
begin
	FBackgroundJobs.AddOrSetValue(Account, GetBackgroundJobsCount(Account) - 1);
end;

function TThreadStateManager.HasActiveBackgroundJobs(const Account: WideString): Boolean;
begin
	Result := GetBackgroundJobsCount(Account) > 0;
end;

end.
