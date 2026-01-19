unit IThreadStateManagerInterface;

interface

uses
	Classes;

type
	{ Interface for managing per-thread state in WFX plugin.
	  All methods implicitly use GetCurrentThreadID() for thread-keyed operations,
	  except background jobs which are keyed by account name. }
	IThreadStateManager = interface
		['{A7E8F3B2-4C1D-4E5F-9A8B-7C6D5E4F3A2B}']

		{ Skip listing flags - prevent directory enumeration during bulk operations }
		function GetSkipListDelete: Boolean;
		procedure SetSkipListDelete(Value: Boolean);
		function GetSkipListRenMov: Boolean;
		procedure SetSkipListRenMov(Value: Boolean);

		{ Abort control - enable cancel dialog during long operations (issue #113) }
		function GetCanAbortRenMov: Boolean;
		procedure SetCanAbortRenMov(Value: Boolean);
		function GetListingAborted: Boolean;
		procedure SetListingAborted(Value: Boolean);

		{ Retry counters - track retry attempts per operation type }
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

		{ Operation context - track current filesystem operation for context-aware decisions }
		function GetFsStatusInfo: Int32;
		procedure SetFsStatusInfo(Value: Int32);
		procedure RemoveFsStatusInfo;
		function HasFsStatusInfo: Boolean;

		{ Background thread tracking - track operation status per thread }
		function GetBackgroundThreadStatus: Int32;
		procedure SetBackgroundThreadStatus(Value: Int32);
		procedure RemoveBackgroundThread;
		function HasBackgroundThread: Boolean;

		{ Path blacklist - paths to skip during move operations (issue #168).
		  Ownership: TStringList created internally, caller should not free it. }
		function GetRemoveDirSkippedPath: TStringList;
		procedure CreateRemoveDirSkippedPath;
		procedure ClearRemoveDirSkippedPath;
		function HasRemoveDirSkippedPath: Boolean;
		function IsPathSkipped(const Path: WideString): Boolean;
		procedure AddSkippedPath(const Path: WideString);
		procedure RemoveSkippedPath(const Path: WideString);

		{ Background jobs - track active jobs per account (keyed by account name, not thread ID).
		  Used to prevent connection pool cleanup while operations are in progress. }
		function GetBackgroundJobsCount(const Account: WideString): Int32;
		procedure IncrementBackgroundJobs(const Account: WideString);
		procedure DecrementBackgroundJobs(const Account: WideString);
		function HasActiveBackgroundJobs(const Account: WideString): Boolean;
	end;

implementation

end.
