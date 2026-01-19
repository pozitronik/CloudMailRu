unit DownloadOrchestrator;

{Orchestrates file download operations.
 Coordinates validation, conflict resolution, download execution, and retry handling.}

interface

uses
	DownloadPreparationValidator,
	LocalFileConflictResolver,
	RetryHandler,
	IPluginSettingsManagerInterface,
	RealPath,
	PLUGIN_TYPES;

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

	TDownloadOrchestrator = class(TInterfacedObject, IDownloadOrchestrator)
	private
		FPreparationValidator: IDownloadPreparationValidator;
		FConflictResolver: ILocalFileConflictResolver;
		FRetryHandler: IRetryHandler;
		FSettingsManager: IPluginSettingsManager;
	public
		constructor Create(
			PreparationValidator: IDownloadPreparationValidator;
			ConflictResolver: ILocalFileConflictResolver;
			RetryHandler: IRetryHandler;
			SettingsManager: IPluginSettingsManager);

		function Execute(const RemoteName, LocalName: WideString; CopyFlags: Integer;
			DownloadOp: TDownloadOperation; ProgressOp: TProgressCallback): Integer;
	end;

implementation

uses
	LANGUAGE_STRINGS;

constructor TDownloadOrchestrator.Create(
	PreparationValidator: IDownloadPreparationValidator;
	ConflictResolver: ILocalFileConflictResolver;
	RetryHandler: IRetryHandler;
	SettingsManager: IPluginSettingsManager);
begin
	inherited Create;
	FPreparationValidator := PreparationValidator;
	FConflictResolver := ConflictResolver;
	FRetryHandler := RetryHandler;
	FSettingsManager := SettingsManager;
end;

function TDownloadOrchestrator.Execute(const RemoteName, LocalName: WideString;
	CopyFlags: Integer; DownloadOp: TDownloadOperation;
	ProgressOp: TProgressCallback): Integer;
var
	RealPath: TRealPath;
	ValidationResult: TDownloadValidationResult;
	ConflictResolution: TConflictResolution;
begin
	RealPath.FromPath(RemoteName);

	{Validate download preconditions}
	ValidationResult := FPreparationValidator.Validate(RealPath, CopyFlags);
	if not ValidationResult.ShouldProceed then
		Exit(ValidationResult.ResultCode);

	ProgressOp(RemoteName, LocalName, 0);

	{Check for local file conflict}
	ConflictResolution := FConflictResolver.Resolve(LocalName, CopyFlags,
		FSettingsManager.GetSettings.OverwriteLocalMode);
	if not ConflictResolution.ShouldProceed then
		Exit(ConflictResolution.ResultCode);

	Result := DownloadOp(RealPath, LocalName, RemoteName, CopyFlags);

	if Result <> FS_FILE_READERROR then
		Exit;

	{Handle retry on read error}
	Result := FRetryHandler.HandleOperationError(Result, rotDownload,
		ERR_DOWNLOAD_FILE_ASK, ERR_DOWNLOAD, DOWNLOAD_FILE_RETRY, RemoteName,
		function: Integer
		begin
			Result := DownloadOp(RealPath, LocalName, RemoteName, CopyFlags);
		end,
		function: Boolean
		begin
			Result := ProgressOp(LocalName, RemoteName, 0);
		end
	);
end;

end.
