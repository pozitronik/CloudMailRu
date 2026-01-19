unit ListingSkipDecider;

{Decides whether directory listing should be skipped.
 Checks skip-list flags for delete/renmov operations and user abort via progress.
 Extracted from FsFindFirst to enable isolated testing of skip logic.}

interface

uses
	IListingSkipDeciderInterface,
	IThreadStateManagerInterface,
	IProgressInterface;

type
	TListingSkipDecider = class(TInterfacedObject, IListingSkipDecider)
	private
		FThreadState: IThreadStateManager;
		FProgress: IProgress;

		{Checks if user requested abort via progress callback.
		 Only checks if CanAbortRenMov flag is set.
		 @return True if user aborted}
		function CheckUserAbort(const Path: WideString): Boolean;
	public
		constructor Create(ThreadState: IThreadStateManager; Progress: IProgress);

		function ShouldSkipListing(const Path: WideString): TListingSkipResult;
	end;

implementation

constructor TListingSkipDecider.Create(ThreadState: IThreadStateManager; Progress: IProgress);
begin
	inherited Create;
	FThreadState := ThreadState;
	FProgress := Progress;
end;

function TListingSkipDecider.CheckUserAbort(const Path: WideString): Boolean;
begin
	Result := False;

	{Only check abort if CanAbortRenMov flag is set (issue #113)}
	if not FThreadState.GetCanAbortRenMov then
		Exit;

	{Progress returns True if user wants to abort}
	if FProgress.Progress(Path, 0) then
	begin
		FThreadState.SetListingAborted(True);
		Result := True;
	end;
end;

function TListingSkipDecider.ShouldSkipListing(const Path: WideString): TListingSkipResult;
var
	SkipListDelete, SkipListRenMov, UserAborted: Boolean;
begin
	{Get skip flags for current operation}
	SkipListDelete := FThreadState.GetSkipListDelete;
	SkipListRenMov := FThreadState.GetSkipListRenMov;

	{Check if user wants to abort via progress callback}
	UserAborted := CheckUserAbort(Path);

	{Build result}
	Result.WasAborted := UserAborted;
	Result.ShouldSkip := SkipListDelete or SkipListRenMov or UserAborted;
end;

end.
