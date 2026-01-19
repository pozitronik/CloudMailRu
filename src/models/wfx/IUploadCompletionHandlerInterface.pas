unit IUploadCompletionHandlerInterface;

{Interface for post-upload success operations.
 Encapsulates progress reporting, logging, move cleanup (local file deletion),
 and description sync after successful file upload.}

interface

uses
	PLUGIN_TYPES,
	RealPath,
	CloudMailRu;

type
	{Context for upload completion handling}
	TUploadCompletionContext = record
		RemotePath: TRealPath;
		LocalName: WideString;
		RemoteName: WideString;
		CopyFlags: Integer;
		Cloud: TCloudMailRu; {Cloud connection for description sync}
	end;

	IUploadCompletionHandler = interface
		['{F8A9B0C1-D2E3-4F5A-6B7C-8D9E0F1A2B3C}']

		{Handles all post-upload success operations.
		 Reports progress, logs completion, handles move flag (deletes local file),
		 and triggers description sync.
		 @param Context Upload context with all required data
		 @return FS_FILE_OK on success, or error code if local file deletion fails}
		function HandleCompletion(const Context: TUploadCompletionContext): Integer;
	end;

	{Null implementation for testing - always succeeds}
	TNullUploadCompletionHandler = class(TInterfacedObject, IUploadCompletionHandler)
	public
		function HandleCompletion(const Context: TUploadCompletionContext): Integer;
	end;

implementation

{TNullUploadCompletionHandler}

function TNullUploadCompletionHandler.HandleCompletion(const Context: TUploadCompletionContext): Integer;
begin
	Result := FS_FILE_OK;
end;

end.
