unit UploadCompletionHandler;

{Post-upload completion handler.
 Performs progress reporting, logging, move cleanup (local file deletion),
 and description sync after successful file upload.}

interface

uses
	ILoggerInterface,
	IProgressInterface,
	LocalFileDeletionHandler,
	IDescriptionSyncGuardInterface,
	PLUGIN_TYPES,
	CMRConstants,
	RealPath,
	CloudMailRu,
	SystemHelper;

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

	TUploadCompletionHandler = class(TInterfacedObject, IUploadCompletionHandler)
	private
		FLogger: ILogger;
		FProgress: IProgress;
		FLocalFileDeletionHandler: ILocalFileDeletionHandler;
		FDescriptionSyncGuard: IDescriptionSyncGuard;

		{Reports completion progress and logs transfer}
		procedure ReportCompletion(const LocalName, RemoteName: WideString);

		{Handles move operation - deletes local file after upload}
		function HandleMoveOperation(const LocalName: WideString): Integer;
	public
		constructor Create(
			Logger: ILogger;
			Progress: IProgress;
			LocalFileDeletionHandler: ILocalFileDeletionHandler;
			DescriptionSyncGuard: IDescriptionSyncGuard
		);

		function HandleCompletion(const Context: TUploadCompletionContext): Integer;
	end;

implementation

{TNullUploadCompletionHandler}

function TNullUploadCompletionHandler.HandleCompletion(const Context: TUploadCompletionContext): Integer;
begin
	Result := FS_FILE_OK;
end;

{TUploadCompletionHandler}

constructor TUploadCompletionHandler.Create(
	Logger: ILogger;
	Progress: IProgress;
	LocalFileDeletionHandler: ILocalFileDeletionHandler;
	DescriptionSyncGuard: IDescriptionSyncGuard
);
begin
	inherited Create;
	FLogger := Logger;
	FProgress := Progress;
	FLocalFileDeletionHandler := LocalFileDeletionHandler;
	FDescriptionSyncGuard := DescriptionSyncGuard;
end;

procedure TUploadCompletionHandler.ReportCompletion(const LocalName, RemoteName: WideString);
begin
	FProgress.Progress(LocalName, RemoteName, 100);
	FLogger.Log(LOG_LEVEL_FILE_OPERATION, MSGTYPE_TRANSFERCOMPLETE, '%s -> %s', [LocalName, RemoteName]);
end;

function TUploadCompletionHandler.HandleMoveOperation(const LocalName: WideString): Integer;
begin
	Result := FLocalFileDeletionHandler.DeleteLocalFile(LocalName);
end;

function TUploadCompletionHandler.HandleCompletion(const Context: TUploadCompletionContext): Integer;
begin
	Result := FS_FILE_OK;

	{Report progress and log completion}
	ReportCompletion(Context.LocalName, Context.RemoteName);

	{Delete local file if this is a move operation}
	if CheckFlag(FS_COPYFLAGS_MOVE, Context.CopyFlags) then
	begin
		Result := HandleMoveOperation(Context.LocalName);
		if Result <> FS_FILE_OK then
			Exit;
	end;

	{Sync description from local to cloud}
	FDescriptionSyncGuard.OnFileUploaded(Context.RemotePath, Context.LocalName, Context.Cloud);
end;

end.
