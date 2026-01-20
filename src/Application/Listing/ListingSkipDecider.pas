unit ListingSkipDecider;

{Interface and implementation for deciding whether directory listing should be skipped.
 Encapsulates skip-list flag checks and user abort detection via progress callback.
 Used by FsFindFirst to determine if listing operation should proceed.}

interface

uses
	ThreadStateManager,
	TCProgress;

type
	{Result of skip decision with abort information}
	TListingSkipResult = record
		ShouldSkip: Boolean;   {True if listing should be skipped}
		WasAborted: Boolean;   {True if user aborted via progress callback}
	end;

	{Decides whether listing should be skipped based on thread state and user input.
	 Checks skip-list flags for delete/renmov operations and abort via progress callback.}
	IListingSkipDecider = interface
		['{7E2A9F81-C4D6-4B3E-A5F8-1D9C6E4B2A7F}']
		{Determines if listing should be skipped.
		 @param Path Directory path being listed (used for progress callback)
		 @return Skip result with ShouldSkip and WasAborted flags}
		function ShouldSkipListing(const Path: WideString): TListingSkipResult;
	end;

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
