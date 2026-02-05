unit ChunkedUploadHandler;

{Handles chunked/split file uploads for files exceeding CloudMaxFileSize.
	Extracted from CloudFileUploader to follow SRP - this class handles only
	the complexity of splitting large files into chunks and uploading them
	with retry, quota check, and CRC generation.}

interface

uses
	System.Classes,
	System.SysUtils,
	CloudConstants,
	CloudFileIdentity,
	CloudSpace,
	WFXTypes,
	LanguageStrings,
	SettingsConstants,
	Logger,
	Progress,
	Request,
	TCHandler,
	CloudContext,
	FileSystem,
	FileSplitInfo,
	CloudHashCalculator;

type
	{Settings specific to chunked upload operations}
	TChunkedUploadSettings = record
		CloudMaxFileSize: Int64;
		PrecalculateHash: Boolean;
		ForcePrecalculateSize: Int64;
		OperationErrorMode: Integer;
		RetryAttempts: Integer;
		AttemptWait: Integer;
	end;

	{Action to take after chunk upload result}
	TChunkActionResult = (
		caRetry,    {Retry current chunk}
		caContinue, {Move to next chunk}
		caAbort     {Stop loop and return error}
	);

	{Method reference for uploading a single file stream}
	TStreamUploadFunc = reference to function(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString): Integer;

	{Method reference for deduplication check}
	TAddByIdentityFunc = reference to function(FileIdentity: TCloudFileIdentity; RemotePath: WideString; ConflictMode: WideString; LogErrors: Boolean; LogSuccess: Boolean): Integer;

	{Interface for chunked upload operations}
	IChunkedUploadHandler = interface
		['{F1A2B3C4-D5E6-4F7A-8B9C-0D1E2F3A4B5C}']
		{Upload file in chunks when it exceeds CloudMaxFileSize.
			@param UploadFunc Function to upload individual chunks
			@param AddByIdentityFunc Function for deduplication checks
			@param LocalPath Local file path
			@param RemotePath Remote destination path
			@param ConflictMode How to handle conflicts
			@param ChunkOverwriteMode How to handle existing chunks
			@return FS_FILE_OK on success, or error code}
		function Upload(UploadFunc: TStreamUploadFunc; AddByIdentityFunc: TAddByIdentityFunc; LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
	end;

	{Handles chunked upload of large files}
	TChunkedUploadHandler = class(TInterfacedObject, IChunkedUploadHandler)
	private
		FContext: ICloudContext;
		FHashCalculator: ICloudHashCalculator;
		FFileSystem: IFileSystem;
		FLogger: ILogger;
		FProgress: IProgress;
		FRequest: IRequest;
		FTCHandler: ITCHandler;
		FDoCryptFiles: Boolean;
		FSettings: TChunkedUploadSettings;

		{Handle FS_FILE_EXISTS case based on ChunkOverwriteMode}
		function HandleChunkExists(const ChunkRemotePath: WideString; ChunkOverwriteMode: Integer; var ResultCode: Integer): TChunkActionResult;

		{Handle upload errors based on OperationErrorMode}
		function HandleChunkError(UploadResult: Integer; const ChunkRemotePath: WideString; var RetryAttemptsCount: Integer; var ResultCode: Integer): TChunkActionResult;

		{Generate and upload CRC file after successful chunk uploads}
		procedure GenerateAndUploadCRC(UploadFunc: TStreamUploadFunc; const LocalPath, RemotePath: WideString; const SplitFileInfo: TFileSplitInfo; const ConflictMode: WideString);

		{Check available space before upload, prompt user if partial upload required}
		function CheckQuotaForSplitUpload(const SplitFileInfo: TFileSplitInfo; var ChunksToUpload: Integer): Boolean;

		{Calculate file identity (hash + size) for deduplication check}
		function CalculateFileIdentity(LocalPath: WideString): TCloudFileIdentity;

		{Try whole-file deduplication before chunking}
		function TryWholeFileDedup(AddByIdentityFunc: TAddByIdentityFunc; const LocalPath, RemotePath: WideString): Boolean;
	public
		constructor Create(
			Context: ICloudContext;
			HashCalculator: ICloudHashCalculator;
			FileSystem: IFileSystem;
			Logger: ILogger;
			Progress: IProgress;
			Request: IRequest;
			TCHandler: ITCHandler;
			DoCryptFiles: Boolean;
			const Settings: TChunkedUploadSettings);

		{IChunkedUploadHandler}
		function Upload(UploadFunc: TStreamUploadFunc; AddByIdentityFunc: TAddByIdentityFunc; LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
	end;

implementation

uses
	Winapi.Windows,
	PathHelper,
	StringHelper,
	SystemHelper,
	ChunkedFileStream;

{TChunkedUploadHandler}

constructor TChunkedUploadHandler.Create(
	Context: ICloudContext;
	HashCalculator: ICloudHashCalculator;
	FileSystem: IFileSystem;
	Logger: ILogger;
	Progress: IProgress;
	Request: IRequest;
	TCHandler: ITCHandler;
	DoCryptFiles: Boolean;
	const Settings: TChunkedUploadSettings);
begin
	inherited Create;
	FContext := Context;
	FHashCalculator := HashCalculator;
	FFileSystem := FileSystem;
	FLogger := Logger;
	FProgress := Progress;
	FRequest := Request;
	FTCHandler := TCHandler;
	FDoCryptFiles := DoCryptFiles;
	FSettings := Settings;
end;

function TChunkedUploadHandler.HandleChunkExists(const ChunkRemotePath: WideString; ChunkOverwriteMode: Integer; var ResultCode: Integer): TChunkActionResult;
begin
	case ChunkOverwriteMode of
		ChunkOverwrite: {Silently overwrite chunk by deleting first}
			begin
				FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_DETAILS, CHUNK_OVERWRITE, [ChunkRemotePath]);
				if not FContext.DeleteFile(ChunkRemotePath) then
				begin
					ResultCode := FS_FILE_WRITEERROR;
					Result := caAbort;
				end else begin
					Result := caRetry; {Retry with same chunk after delete}
				end;
			end;
		ChunkOverwriteIgnore: {Skip this chunk and continue}
			begin
				FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_DETAILS, CHUNK_SKIP, [ChunkRemotePath]);
				Result := caContinue;
			end;
		ChunkOverwriteAbort: {Abort the entire operation}
			begin
				FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_DETAILS, CHUNK_ABORT, [ChunkRemotePath]);
				ResultCode := FS_FILE_NOTSUPPORTED;
				Result := caAbort;
			end;
		else
			{Unknown mode - treat as abort}
			ResultCode := CLOUD_OPERATION_FAILED;
			Result := caAbort;
	end;
end;

function TChunkedUploadHandler.HandleChunkError(UploadResult: Integer; const ChunkRemotePath: WideString; var RetryAttemptsCount: Integer; var ResultCode: Integer): TChunkActionResult;
begin
	case FSettings.OperationErrorMode of
		OperationErrorModeAsk:
			begin
				case MessageBoxW(FTCHandler.FindTCWindow, PWideChar(Format(ERR_PARTIAL_UPLOAD_ASK, [UploadResult, ChunkRemotePath])), PWideChar(ERR_UPLOAD), MB_ABORTRETRYIGNORE + MB_ICONERROR) of
					ID_ABORT:
						begin
							ResultCode := FS_FILE_USERABORT;
							Result := caAbort;
						end;
					ID_RETRY:
						Result := caRetry;
					ID_IGNORE:
						Result := caContinue;
					else
						Result := caContinue; {Default to continue for unknown response}
				end;
			end;
		OperationErrorModeIgnore:
			begin
				FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_IGNORE, [UploadResult]);
				Result := caContinue;
			end;
		OperationErrorModeAbort:
			begin
				FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_ABORT, [UploadResult]);
				ResultCode := FS_FILE_USERABORT;
				Result := caAbort;
			end;
		OperationErrorModeRetry:
			begin
				Inc(RetryAttemptsCount);
				if RetryAttemptsCount <= FSettings.RetryAttempts then
				begin
					FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_RETRY, [UploadResult, RetryAttemptsCount, FSettings.RetryAttempts]);
					ProcessMessages;
					Sleep(FSettings.AttemptWait);
					Result := caRetry;
				end else begin
					FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_RETRY_EXCEED, [UploadResult]);
					ResultCode := CLOUD_OPERATION_FAILED;
					Result := caAbort;
				end;
			end;
		else
			{Unknown option value - abort}
			ResultCode := CLOUD_OPERATION_FAILED;
			Result := caAbort;
	end;
end;

function TChunkedUploadHandler.CalculateFileIdentity(LocalPath: WideString): TCloudFileIdentity;
begin
	Result.Hash := FHashCalculator.CalculateHash(LocalPath);
	Result.size := FFileSystem.GetFileSize(LocalPath);
end;

procedure TChunkedUploadHandler.GenerateAndUploadCRC(UploadFunc: TStreamUploadFunc; const LocalPath, RemotePath: WideString; const SplitFileInfo: TFileSplitInfo; const ConflictMode: WideString);
var
	CRCRemotePath: WideString;
	CRCStream: TStringStream;
begin
	CRCRemotePath := ExtractFilePath(RemotePath) + SplitFileInfo.CRCFileName;
	FContext.GetHTTP.SetProgressNames(LocalPath, CRCRemotePath);
	CRCStream := TStringStream.Create;
	try
		SplitFileInfo.GetCRCData(CRCStream);
		UploadFunc(SplitFileInfo.CRCFileName, CRCRemotePath, CRCStream, ConflictMode);
	finally
		CRCStream.Free;
	end;
end;

function TChunkedUploadHandler.CheckQuotaForSplitUpload(const SplitFileInfo: TFileSplitInfo; var ChunksToUpload: Integer): Boolean;
var
	SpaceInfo: TCloudSpace;
	AvailableSpace: Int64;
	ChunksThatFit: Integer;
	ReturnedText: WideString;
	Msg: WideString;
begin
	ChunksToUpload := SplitFileInfo.ChunksCount;
	Result := True;

	{Skip check for public accounts - they don't have quota}
	if FContext.IsPublicAccount then
		Exit;

	{Get available space}
	if not FContext.GetUserSpace(SpaceInfo) then
		Exit; {Can't get space info - proceed anyway}

	AvailableSpace := SpaceInfo.total - SpaceInfo.used;
	if AvailableSpace <= 0 then
		AvailableSpace := 0;

	{Calculate how many chunks fit}
	ChunksThatFit := AvailableSpace div FSettings.CloudMaxFileSize;
	if ChunksThatFit >= SplitFileInfo.ChunksCount then
		Exit; {All chunks fit - proceed normally}

	if ChunksThatFit = 0 then
	begin
		{No space at all - inform and abort}
		Msg := Format(SPLIT_NO_SPACE_MSG, [FormatSize(AvailableSpace), FormatSize(SplitFileInfo.FileSize)]);
		FRequest.Request(RT_MsgOK, SPLIT_PARTIAL_UPLOAD_TITLE, Msg, ReturnedText, 0);
		Result := False;
		Exit;
	end;

	{Not enough space for all chunks - ask user whether to continue with partial upload}
	Msg := Format(SPLIT_PARTIAL_UPLOAD_MSG, [FormatSize(AvailableSpace), FormatSize(SplitFileInfo.FileSize), ChunksThatFit, SplitFileInfo.ChunksCount]);
	Result := FRequest.Request(RT_MsgOKCancel, SPLIT_PARTIAL_UPLOAD_TITLE, Msg, ReturnedText, 0);
	if Result then
		ChunksToUpload := ChunksThatFit;
end;

function TChunkedUploadHandler.TryWholeFileDedup(AddByIdentityFunc: TAddByIdentityFunc; const LocalPath, RemotePath: WideString): Boolean;
var
	FileIdentity: TCloudFileIdentity;
begin
	Result := False;
	if FDoCryptFiles then
		Exit; {Can't dedup encrypted files}

	FileIdentity := CalculateFileIdentity(LocalPath);
	if FileIdentity.Hash = EmptyWideStr then
		Exit; {Hash calculation failed or cancelled}

	Result := AddByIdentityFunc(FileIdentity, RemotePath, CLOUD_CONFLICT_STRICT, False, True) = FS_FILE_OK;
end;

function TChunkedUploadHandler.Upload(UploadFunc: TStreamUploadFunc; AddByIdentityFunc: TAddByIdentityFunc; LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
var
	SplitFileInfo: TFileSplitInfo;
	ChunkIndex: Integer;
	ChunksToUpload: Integer;
	ActualUploads: Integer;
	ChunkRemotePath: WideString;
	ChunkStream: TChunkedFileStream;
	RetryAttemptsCount: Integer;
	UseHash: Boolean;
	IsPartialUpload: Boolean;
	Action: TChunkActionResult;
	ScaledProgress: TScaledProgress;
begin
	{Create split info first to determine chunk count for quota check}
	SplitFileInfo := TFileSplitInfo.Create(GetUNCFilePath(LocalPath), FSettings.CloudMaxFileSize, FFileSystem.GetFileSize(GetUNCFilePath(LocalPath)));
	try
		{Check available space BEFORE expensive hash calculation}
		if not CheckQuotaForSplitUpload(SplitFileInfo, ChunksToUpload) then
			Exit(FS_FILE_USERABORT);

		IsPartialUpload := ChunksToUpload < SplitFileInfo.ChunksCount;
		UseHash := FSettings.PrecalculateHash or (FSettings.ForcePrecalculateSize >= FFileSystem.GetFileSize(LocalPath));

		{Whole-file deduplication: try to register the entire file by hash before splitting.
			Skip for partial uploads - pointless to calculate hash for file we can't fully upload.
			This is a speculative optimization - if the whole file's hash exists on server,
			the file is created instantly without uploading any chunks.
			Note: This rarely succeeds for split files since chunked storage uses different hashes.}
		if UseHash and (not IsPartialUpload) and TryWholeFileDedup(AddByIdentityFunc, GetUNCFilePath(LocalPath), RemotePath) then
			Exit(CLOUD_OPERATION_OK);

		ChunkIndex := 0;
		ActualUploads := 0;
		RetryAttemptsCount := 0;
		Result := FS_FILE_OK;

		{Create scaled progress decorator to show overall file progress instead of per-chunk progress}
		ScaledProgress := TScaledProgress.Create(FProgress);
		FContext.GetHTTP.SetProgress(ScaledProgress);

		{Upload each chunk - iterate all chunks, but limit ACTUAL uploads (not skips) by quota.
			Skipped chunks (already exist with matching hash) don't count toward quota.}
		while (ChunkIndex < SplitFileInfo.ChunksCount) and ((not IsPartialUpload) or (ActualUploads < ChunksToUpload)) do
		begin
			ChunkRemotePath := Format('%s%s', [ExtractFilePath(RemotePath), SplitFileInfo.GetChunks[ChunkIndex].name]);
			{Show original filename with chunk position for user-friendly progress display}
			FContext.GetHTTP.SetProgressNames(LocalPath, Format('%s [%d of %d]', [ExtractFileName(RemotePath), ChunkIndex + 1, SplitFileInfo.ChunksCount]));
			{Update scale for current chunk position}
			ScaledProgress.SetScale(ChunkIndex, SplitFileInfo.ChunksCount);
			FLogger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PARTIAL_UPLOAD_INFO, [LocalPath, ChunkIndex + 1, SplitFileInfo.ChunksCount, ChunkRemotePath]);

			{Upload current chunk}
			try
				ChunkStream := TChunkedFileStream.Create(GetUNCFilePath(LocalPath), fmOpenRead or fmShareDenyWrite, SplitFileInfo.GetChunks[ChunkIndex].start, SplitFileInfo.GetChunks[ChunkIndex].size);
				try
					Result := UploadFunc(ExtractFileName(ChunkRemotePath), ChunkRemotePath, ChunkStream, ConflictMode);
				finally
					ChunkStream.Free;
				end;
			except
				on E: Exception do
				begin
					FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_FILE_OPEN, [LocalPath, E.Message]);
					Result := FS_FILE_READERROR;
				end;
			end;

			{Handle upload result}
			case Result of
				FS_FILE_OK:
					begin
						RetryAttemptsCount := 0;
						Inc(ActualUploads); {Count actual upload toward quota}
						Inc(ChunkIndex);
					end;
				FS_FILE_USERABORT:
					begin
						FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PARTIAL_UPLOAD_ABORTED);
						Break; {Exit loop preserving USERABORT result}
					end;
				FS_FILE_EXISTS:
					begin
						{When hash precalculation is enabled and EXISTS is returned, the chunk's hash
							was verified and exists on server - safe to skip (resume scenario).
							Skipped chunks don't count toward upload quota.
							Otherwise delegate to HandleChunkExists based on ChunkOverwriteMode.}
						if UseHash then
						begin
							FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, CHUNK_SKIP_HASH_MATCH, [ChunkRemotePath]);
							Result := FS_FILE_OK;
							Inc(ChunkIndex); {Skip doesn't count toward ActualUploads}
						end else begin
							Action := HandleChunkExists(ChunkRemotePath, ChunkOverwriteMode, Result);
							case Action of
								caRetry:
									; {Don't increment, retry same chunk}
								caContinue:
									begin
										Result := FS_FILE_OK; {Clear error when ignoring and continuing}
										Inc(ChunkIndex); {Skip doesn't count toward ActualUploads}
									end;
								caAbort:
									Break;
							end;
						end;
					end;
				else
					{Any other error - handle via OperationErrorMode}
					Action := HandleChunkError(Result, ChunkRemotePath, RetryAttemptsCount, Result);
					case Action of
						caRetry:
							; {Don't increment, retry same chunk}
						caContinue:
							begin
								Result := FS_FILE_OK; {Clear error when ignoring and continuing}
								Inc(ChunkIndex);
							end;
						caAbort:
							Break;
					end;
			end;
		end;

		{Restore original progress for subsequent operations}
		FContext.GetHTTP.SetProgress(FProgress);

		{Generate and upload CRC file only after ALL chunks are on cloud.
			ChunkIndex = total means all chunks were processed (uploaded or skipped).
			Skip CRC if upload stopped early due to quota or error.}
		if (Result = FS_FILE_OK) and (ChunkIndex = SplitFileInfo.ChunksCount) then
			GenerateAndUploadCRC(UploadFunc, LocalPath, RemotePath, SplitFileInfo, ConflictMode);
	finally
		SplitFileInfo.Free;
	end;
end;

end.
