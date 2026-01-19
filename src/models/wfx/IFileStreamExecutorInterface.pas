unit IFileStreamExecutorInterface;

{Interface for executing file streaming operations.
 Handles URL resolution, file publication, and command execution for streaming.}

interface

uses
	RealPath,
	CMRDirItem,
	StreamingSettings,
	IConnectionManagerInterface;

type
	IFileStreamExecutor = interface
		['{B8E5D3A1-7C9F-4E2B-A6D8-9F1C3E5B7A2D}']

		{Executes file streaming for the given item.
		 Resolves streaming URL based on format, publishes file if needed,
		 and launches the configured streaming command.
		 @param RealPath Path to the file
		 @param Item Directory item with weblink info
		 @param Settings Streaming configuration (command, parameters, format)
		 @param ConnManager Connection manager for account access
		 @return FS_EXEC_OK on success, FS_EXEC_ERROR on failure}
		function Execute(const RealPath: TRealPath; const Item: TCMRDirItem;
			var Settings: TStreamingSettings; ConnManager: IConnectionManager): Integer;
	end;

implementation

end.
