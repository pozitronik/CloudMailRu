unit CloudFileUploader;

interface

uses
	System.Classes,
	System.SysUtils,
	CloudConstants,
	CloudOAuth,
	CloudSpace,
	CloudFileIdentity,
	CloudOperationResult,
	CloudOperationResultJsonAdapter,
	WFXTypes,
	LanguageStrings,
	SettingsConstants,
	HashInfo,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	FileCipher,
	PathHelper,
	StringHelper,
	SystemHelper,
	WindowsHelper,
	CloudShardManager,
	CloudHashCalculator,
	CloudHTTP,
	CloudErrorMapper,
	CloudContext,
	WindowsFileSystem,
	FileSplitInfo,
	ChunkedFileStream,
	TokenRetryHelper;

type
	{Action to take after chunk upload result}
	TChunkActionResult = (caRetry, {Retry current chunk}
		caContinue, {Move to next chunk}
		caAbort {Stop loop and return error}
		);

	{Upload settings passed to uploader}
	TUploadSettings = record
		PrecalculateHash: Boolean;
		ForcePrecalculateSize: Int64;
		CheckCRC: Boolean;
		OperationErrorMode: Integer;
		RetryAttempts: Integer;
		AttemptWait: Integer;
		UnlimitedFileSize: Boolean;
		SplitLargeFiles: Boolean;
		CloudMaxFileSize: Int64;
	end;

	{Interface for cloud file upload operations}
	ICloudFileUploader = interface
		['{D4BF6315-520B-4801-A633-965A339484EB}']
		{Upload file from local path to cloud.
			Returns FS_FILE_OK on success, or appropriate error code on failure.}
		function Upload(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer;
		{Add file by its hash identity (fast upload if file already exists in cloud).
			Returns FS_FILE_OK on success, or appropriate error code on failure.}
		function AddFileByIdentity(FileIdentity: TCloudFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = True; LogSuccess: Boolean = False): Integer;
	end;

	{File upload service - handles whole file and chunked uploads}
	TCloudFileUploader = class(TInterfacedObject, ICloudFileUploader)
	private
		FContext: ICloudContext;
		FShardManager: ICloudShardManager;
		FHashCalculator: ICloudHashCalculator;
		FCipher: ICipher;
		FFileSystem: IFileSystem;
		FLogger: ILogger;
		FProgress: IProgress;
		FRequest: IRequest;
		FTCHandler: ITCHandler;
		FRetryOperation: IRetryOperation;
		FDoCryptFiles: Boolean;
		FSettings: TUploadSettings;

		{Internal upload methods}
		function PutFileWhole(LocalPath, RemotePath, ConflictMode: WideString): Integer;
		function PutFileStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString): Integer;
		function PutFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCloudFileIdentity): Integer;
		{Split upload helpers}
		function HandleChunkExists(const ChunkRemotePath: WideString; ChunkOverwriteMode: Integer; var ResultCode: Integer): TChunkActionResult;
		function HandleChunkError(UploadResult: Integer; const ChunkRemotePath: WideString; var RetryAttemptsCount: Integer; var ResultCode: Integer): TChunkActionResult;
		procedure GenerateAndUploadCRC(const LocalPath, RemotePath: WideString; const SplitFileInfo: TFileSplitInfo; const ConflictMode: WideString);
		{Check available space before split upload, prompt user if partial upload required}
		function CheckQuotaForSplitUpload(const SplitFileInfo: TFileSplitInfo; var ChunksToUpload: Integer): Boolean;
		{Calculate file identity (hash + size) for local file}
		function CalculateFileIdentity(LocalPath: WideString): TCloudFileIdentity;
	protected
		{Protected for testability - tests need direct access to split upload logic}
		function PutFileSplit(LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
	public
		constructor Create(Context: ICloudContext; ShardManager: ICloudShardManager; HashCalculator: ICloudHashCalculator; Cipher: ICipher; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; TCHandler: ITCHandler; RetryOperation: IRetryOperation; DoCryptFiles: Boolean; Settings: TUploadSettings);

		{ICloudFileUploader}
		function Upload(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer;
		function AddFileByIdentity(FileIdentity: TCloudFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = True; LogSuccess: Boolean = False): Integer;
	end;

implementation

uses
	Winapi.Windows;

{TCloudFileUploader}

constructor TCloudFileUploader.Create(Context: ICloudContext; ShardManager: ICloudShardManager; HashCalculator: ICloudHashCalculator; Cipher: ICipher; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; TCHandler: ITCHandler; RetryOperation: IRetryOperation; DoCryptFiles: Boolean; Settings: TUploadSettings);
begin
	inherited Create;
	FContext := Context;
	FShardManager := ShardManager;
	FHashCalculator := HashCalculator;
	FCipher := Cipher;
	FFileSystem := FileSystem;
	FLogger := Logger;
	FProgress := Progress;
	FRequest := Request;
	FTCHandler := TCHandler;
	FRetryOperation := RetryOperation;
	FDoCryptFiles := DoCryptFiles;
	FSettings := Settings;
end;

{Attempts to create a file by registering an existing hash in cloud storage.
	Used for deduplication: if content with this hash already exists on server,
	a new file pointing to it is created instantly without uploading.

	API behavior:
	- Returns 200 if hash exists and file was created (deduplication success)
	- Returns HTTP 400 if hash doesn't exist in cloud storage (not an error,
	just means the content must be uploaded first)

	The HTTP 400 response raises EIdHTTPProtocolException which is caught and
	handled, but may trigger debugger breaks during development.}
function TCloudFileUploader.AddFileByIdentity(FileIdentity: TCloudFileIdentity; RemotePath: WideString; ConflictMode: WideString; LogErrors: Boolean; LogSuccess: Boolean): Integer;
var
	CallResult: TAPICallResult;
	{Explicit Self capture for anonymous function closure}
	HTTP: ICloudHTTP;
	UnitedParams: WideString;
	Context: ICloudContext;
	Logger: ILogger;
begin
	if FContext.IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);

	{Capture values before anonymous function to avoid Self capture issues}
	HTTP := FContext.GetHTTP;
	UnitedParams := FContext.GetUnitedParams;
	Context := FContext;
	Logger := FLogger;

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			OperationResult: TCloudOperationResult;
			ResultCode: Integer;
		begin
			if HTTP.PostForm(API_FILE_ADD + '?' + UnitedParams, Format('api=2&conflict=%s&home=/%s&hash=%s&size=%d', [ConflictMode, PathToUrl(RemotePath), FileIdentity.Hash, FileIdentity.size]), JSON, 'application/x-www-form-urlencoded', LogErrors, False) then
			begin
				TCloudOperationResultJsonAdapter.Parse(JSON, OperationResult);
				ResultCode := Context.CloudResultToFsResult(JSON, PREFIX_ERR_FILE_UPLOADING);
				if (CLOUD_OPERATION_OK = OperationResult.OperationResult) and LogSuccess then
					Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, FILE_FOUND_BY_HASH, [RemotePath]);
			end else begin
				ResultCode := FS_FILE_WRITEERROR;
			end;
			Result := TAPICallResult.FromInteger(ResultCode, JSON);
		end);

	Result := CallResult.ResultCode;
end;

function TCloudFileUploader.Upload(LocalPath, RemotePath: WideString; ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
begin
	if FContext.IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);
	FContext.GetHTTP.SetProgressNames(LocalPath, RemotePath);
	if (not FSettings.UnlimitedFileSize) and (FFileSystem.GetFileSize(GetUNCFilePath(LocalPath)) > FSettings.CloudMaxFileSize) then
	begin
		if FSettings.SplitLargeFiles then
		begin
			FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, SPLIT_LARGE_FILE, [FSettings.CloudMaxFileSize]);
			Exit(PutFileSplit(LocalPath, RemotePath, ConflictMode, ChunkOverwriteMode));
		end else begin
			FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, SPLIT_LARGE_FILE_IGNORE, [FSettings.CloudMaxFileSize]);
			Exit(FS_FILE_NOTSUPPORTED);
		end;
	end;

	Result := PutFileWhole(LocalPath, RemotePath, ConflictMode);
end;

function TCloudFileUploader.PutFileWhole(LocalPath, RemotePath, ConflictMode: WideString): Integer;
var
	FileStream: TBufferedFileStream;
begin
	try
		FileStream := TBufferedFileStream.Create(GetUNCFilePath(LocalPath), fmOpenRead or fmShareDenyWrite);
	except
		on E: Exception do
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_FILE_OPEN, [LocalPath, E.Message]);
			Exit(FS_FILE_READERROR);
		end;
	end;
	try
		Result := PutFileStream(ExtractFileName(RemotePath), RemotePath, FileStream, ConflictMode);
	finally
		FileStream.Free;
	end;
end;

function TCloudFileUploader.PutFileStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString): Integer;
var
	LocalFileIdentity, RemoteFileIdentity: TCloudFileIdentity;
	OperationResult: Integer;
	DedupeResult: Integer;
	UploadStream: TStream;
	UseHash: Boolean;
begin
	Result := FS_FILE_WRITEERROR;
	OperationResult := CLOUD_OPERATION_FAILED;

	UseHash := FSettings.PrecalculateHash or (FSettings.ForcePrecalculateSize >= FileStream.size); {issue #231}

	if UseHash or FSettings.CheckCRC then
	begin
		LocalFileIdentity.Hash := FHashCalculator.CalculateHash(FileStream, FileName);
		LocalFileIdentity.size := FileStream.size;
	end;
	{Deduplication: try to create file by hash without uploading.
		If hash exists on server, file is created instantly. If not, AddFileByIdentity
		returns error (HTTP 400) and we proceed to actual upload. See AddFileByIdentity comments.}
	if UseHash and (LocalFileIdentity.Hash <> EmptyWideStr) and (not FDoCryptFiles) then
	begin
		DedupeResult := AddFileByIdentity(LocalFileIdentity, RemotePath, CLOUD_CONFLICT_STRICT, False, True);
		if DedupeResult = FS_FILE_OK then
			Exit(CLOUD_OPERATION_OK) {issue #135}
		else if DedupeResult = FS_FILE_EXISTS then
			{Path already exists - return EXISTS so caller can decide (skip for resume scenario).
				This avoids re-uploading data that's likely already there from previous attempt.}
			Exit(FS_FILE_EXISTS);
		{Other errors (hash not found on server): fall through to upload}
	end;

	try
		{Get encrypting stream wrapper - encrypts on-the-fly during upload.
			TNullCipher returns pass-through wrapper with zero overhead.
			TFileCipher returns encrypting wrapper with constant memory usage.}
		UploadStream := FCipher.GetEncryptingStream(FileStream);
		try
			OperationResult := PutFileToCloud(FileName, UploadStream, RemoteFileIdentity);
		finally
			UploadStream.Free;
		end;
	except
		on E: Exception do
		begin
			if E is EAbort then
			begin
				Result := FS_FILE_USERABORT;
			end else begin
				FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_UPLOAD_INFO, [E.ClassName, E.Message]);
				Result := FS_FILE_WRITEERROR;
			end;
		end;
	end;
	if OperationResult = CLOUD_OPERATION_OK then
	begin
		if FSettings.CheckCRC then
		begin
			if not LocalFileIdentity.IsEqualTo(RemoteFileIdentity) then {При включённой проверке CRC сравниваем хеши и размеры}
				Exit(CLOUD_OPERATION_FAILED);
		end;
	end else if OperationResult = CLOUD_OPERATION_CANCELLED then
	begin
		Result := FS_FILE_USERABORT;
	end;

	if OperationResult = CLOUD_OPERATION_OK then
		Result := AddFileByIdentity(RemoteFileIdentity, RemotePath, ConflictMode, False, False);
end;

function TCloudFileUploader.PutFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCloudFileIdentity): Integer;
var
	CallResult: TAPICallResult;
	LocalIdentity: TCloudFileIdentity;
	OAuthToken: TCloudOAuth;
	HTTP: ICloudHTTP;
begin
	FileIdentity.Hash := EmptyWideStr;
	FileIdentity.size := -1;
	Result := CLOUD_OPERATION_FAILED;
	if FContext.IsPublicAccount then
		Exit;
	FShardManager.EnsureUploadShard;

	LocalIdentity.Hash := EmptyWideStr;
	LocalIdentity.size := -1;
	OAuthToken := FContext.GetOAuthToken;
	HTTP := FContext.GetHTTP;
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			PostAnswer: WideString;
			UploadUrl: WideString;
			ResultCode: Integer;
		begin
			{OAuth requires only client_id and token parameters for upload authentication}
			UploadUrl := Format('%s?client_id=%s&token=%s', [FShardManager.GetUploadShard, OAUTH_CLIENT_ID, OAuthToken.access_token]);
			ResultCode := HTTP.PutFile(UploadUrl, FileName, FileStream, PostAnswer);
			if (ResultCode = CLOUD_OPERATION_OK) then
			begin
				if length(PostAnswer) <> SHA1_HEX_LENGTH then
					ResultCode := CLOUD_OPERATION_FAILED
				else
				begin
					LocalIdentity.Hash := PostAnswer;
					LocalIdentity.size := FileStream.size;
				end;
			end;
			Result := TAPICallResult.FromInteger(ResultCode);
		end);

	Result := CallResult.ResultCode;
	if Result = CLOUD_OPERATION_OK then
		FileIdentity := LocalIdentity;
end;

{Helper method: handles FS_FILE_EXISTS case based on ChunkOverwriteMode}
function TCloudFileUploader.HandleChunkExists(const ChunkRemotePath: WideString; ChunkOverwriteMode: Integer; var ResultCode: Integer): TChunkActionResult;
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

{Helper method: handles upload errors based on OperationErrorMode}
function TCloudFileUploader.HandleChunkError(UploadResult: Integer; const ChunkRemotePath: WideString; var RetryAttemptsCount: Integer; var ResultCode: Integer): TChunkActionResult;
begin
	case FSettings.OperationErrorMode of
		OperationErrorModeAsk:
			begin
				case MsgBox(FTCHandler.FindTCWindow, ERR_PARTIAL_UPLOAD_ASK, [UploadResult, ChunkRemotePath], ERR_UPLOAD, MB_ABORTRETRYIGNORE + MB_ICONERROR) of
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

{Calculate file identity (hash + size) for a local file}
function TCloudFileUploader.CalculateFileIdentity(LocalPath: WideString): TCloudFileIdentity;
begin
	Result.Hash := FHashCalculator.CalculateHash(LocalPath);
	Result.size := FFileSystem.GetFileSize(LocalPath);
end;

{Helper method: generates and uploads CRC file after successful chunk uploads}
procedure TCloudFileUploader.GenerateAndUploadCRC(const LocalPath, RemotePath: WideString; const SplitFileInfo: TFileSplitInfo; const ConflictMode: WideString);
var
	CRCRemotePath: WideString;
	CRCStream: TStringStream;
begin
	CRCRemotePath := ExtractFilePath(RemotePath) + SplitFileInfo.CRCFileName;
	FContext.GetHTTP.SetProgressNames(LocalPath, CRCRemotePath);
	CRCStream := TStringStream.Create;
	try
		SplitFileInfo.GetCRCData(CRCStream);
		PutFileStream(SplitFileInfo.CRCFileName, CRCRemotePath, CRCStream, ConflictMode);
	finally
		CRCStream.Free;
	end;
end;

{Check available space before split upload.
	If not enough space for all chunks, prompts user to continue with partial upload.
	Returns True to continue upload, False to abort.
	ChunksToUpload is set to the number of chunks that can be uploaded (may be limited by available space).}
function TCloudFileUploader.CheckQuotaForSplitUpload(const SplitFileInfo: TFileSplitInfo; var ChunksToUpload: Integer): Boolean;
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

{Uploads file in chunks when it exceeds CloudMaxFileSize.
	Returns FS_FILE_OK on success, or appropriate error code on failure.}
function TCloudFileUploader.PutFileSplit(LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
var
	LocalFileIdentity: TCloudFileIdentity;
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
	SplitFileInfo := TFileSplitInfo.Create(GetUNCFilePath(LocalPath), FSettings.CloudMaxFileSize);
	try
		{Check available space BEFORE expensive hash calculation}
		if not CheckQuotaForSplitUpload(SplitFileInfo, ChunksToUpload) then
			Exit(FS_FILE_USERABORT);

		IsPartialUpload := ChunksToUpload < SplitFileInfo.ChunksCount;
		UseHash := FSettings.PrecalculateHash or (FSettings.ForcePrecalculateSize >= FFileSystem.GetFileSize(LocalPath)); {issue #231}

		{Whole-file deduplication: try to register the entire file by hash before splitting.
			Skip for partial uploads - pointless to calculate hash for file we can't fully upload.
			This is a speculative optimization - if the whole file's hash exists on server,
			the file is created instantly without uploading any chunks.
			Note: This rarely succeeds for split files since chunked storage uses different hashes.
			If hash doesn't exist, server returns HTTP 400 (see AddFileByIdentity comments).
			Each chunk also has its own deduplication check in PutFileStream.}
		if UseHash and (not IsPartialUpload) then
		begin
			LocalFileIdentity := CalculateFileIdentity(GetUNCFilePath(LocalPath));
			{Hash calculation cancellation causes entire operation abort - TC remembers cancel press}
			if (LocalFileIdentity.Hash <> EmptyWideStr) and (not FDoCryptFiles) and (FS_FILE_OK = AddFileByIdentity(LocalFileIdentity, RemotePath, CLOUD_CONFLICT_STRICT, False, True)) then
				Exit(CLOUD_OPERATION_OK); {issue #135}
		end;

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
					Result := PutFileStream(ExtractFileName(ChunkRemotePath), ChunkRemotePath, ChunkStream, ConflictMode);
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
			GenerateAndUploadCRC(LocalPath, RemotePath, SplitFileInfo, ConflictMode);
	finally
		SplitFileInfo.Free;
	end;
end;

end.
