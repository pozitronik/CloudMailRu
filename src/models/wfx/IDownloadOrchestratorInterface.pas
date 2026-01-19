unit IDownloadOrchestratorInterface;

{Interface for orchestrating file download operations.
 Coordinates validation, conflict resolution, download execution, and retry handling.}

interface

uses
	RealPath;

type
	{Callback for performing the actual download operation}
	TDownloadOperation = reference to function(const RemotePath: TRealPath;
		const LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;

	{Callback for progress reporting, returns True if user aborted}
	TProgressCallback = reference to function(const Source, Target: WideString; PercentDone: Integer): Boolean;

	IDownloadOrchestrator = interface
		['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

		{Orchestrates complete download operation.
		 @param RemoteName Remote file path
		 @param LocalName Local file path
		 @param CopyFlags TC copy operation flags
		 @param DownloadOp Callback to perform actual download
		 @param ProgressOp Callback for progress updates
		 @return FS_FILE_* result code}
		function Execute(const RemoteName, LocalName: WideString; CopyFlags: Integer;
			DownloadOp: TDownloadOperation; ProgressOp: TProgressCallback): Integer;
	end;

implementation

end.
