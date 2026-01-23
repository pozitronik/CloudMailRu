unit CloudFileUploader;

interface

uses
	System.Classes,
	System.SysUtils,
	CMRConstants,
	CMROAuth,
	CMRFileIdentity,
	CMROperationResult,
	CMROperationResultJsonAdapter,
	PLUGIN_TYPES,
	LANGUAGE_STRINGS,
	SETTINGS_CONSTANTS,
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

	{Callback types for accessing CloudMailRu state and operations}
	TGetHTTPFunc = reference to function: ICloudHTTP;
	TGetOAuthTokenFunc = reference to function: TCMROAuth;
	TGetBoolFunc = reference to function: Boolean;
	TGetIntFunc = reference to function: Integer;
	TGetInt64Func = reference to function: Int64;
	TGetStringFunc = reference to function: WideString;
	TGetUnitedParamsFunc = reference to function: WideString;
	TRefreshTokenFunc = reference to function: Boolean;
	THashStreamFunc = reference to function(Stream: TStream; Path: WideString): WideString;
	THashFileFunc = reference to function(Path: WideString): WideString;
	TDeleteFileFunc = reference to function(Path: WideString): Boolean;
	TGetRetryOperationFunc = reference to function: TRetryOperation;
	TCloudResultToFsResultFunc = reference to function(JSON: WideString; ErrorPrefix: WideString): Integer;

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
		function AddFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = True; LogSuccess: Boolean = False): Integer;
	end;

	{File upload service - handles whole file and chunked uploads}
	TCloudFileUploader = class(TInterfacedObject, ICloudFileUploader)
	private
		{Callbacks for CloudMailRu state access}
		FGetHTTP: TGetHTTPFunc;
		FShardManager: ICloudShardManager;
		FHashCalculator: ICloudHashCalculator;
		FCipher: ICipher;
		FFileSystem: IFileSystem;
		FLogger: ILogger;
		FProgress: IProgress;
		FRequest: IRequest;
		FTCHandler: ITCHandler;
		FGetOAuthToken: TGetOAuthTokenFunc;
		FIsPublicAccount: TGetBoolFunc;
		FGetRetryOperation: TGetRetryOperationFunc;
		FGetUnitedParams: TGetUnitedParamsFunc;
		FCloudResultToFsResult: TCloudResultToFsResultFunc;
		FDeleteFile: TDeleteFileFunc;
		FDoCryptFiles: Boolean;
		FDoCryptFilenames: Boolean;
		FSettings: TUploadSettings;

		{Internal upload methods}
		function PutFileWhole(LocalPath, RemotePath, ConflictMode: WideString): Integer;
		function PutFileStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString): Integer;
		function PutFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCMRFileIdentity): Integer;
		{Split upload helpers}
		function HandleChunkExists(const ChunkRemotePath: WideString; ChunkOverwriteMode: Integer; var ResultCode: Integer): TChunkActionResult;
		function HandleChunkError(UploadResult: Integer; const ChunkRemotePath: WideString; var RetryAttemptsCount: Integer; var ResultCode: Integer): TChunkActionResult;
		procedure GenerateAndUploadCRC(const LocalPath, RemotePath: WideString; const SplitFileInfo: TFileSplitInfo; const ConflictMode: WideString);
		{Calculate file identity (hash + size) for local file}
		function CalculateFileIdentity(LocalPath: WideString): TCMRFileIdentity;
	protected
		{Protected for testability - tests need direct access to split upload logic}
		function PutFileSplit(LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
	public
		constructor Create(GetHTTP: TGetHTTPFunc; ShardManager: ICloudShardManager; HashCalculator: ICloudHashCalculator; Cipher: ICipher; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; TCHandler: ITCHandler; GetOAuthToken: TGetOAuthTokenFunc; IsPublicAccount: TGetBoolFunc; GetRetryOperation: TGetRetryOperationFunc; GetUnitedParams: TGetUnitedParamsFunc; CloudResultToFsResult: TCloudResultToFsResultFunc; DeleteFile: TDeleteFileFunc; DoCryptFiles, DoCryptFilenames: Boolean; Settings: TUploadSettings);

		{ICloudFileUploader}
		function Upload(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer;
		function AddFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = True; LogSuccess: Boolean = False): Integer;
	end;

implementation

uses
	Winapi.Windows;

{TCloudFileUploader}

constructor TCloudFileUploader.Create(GetHTTP: TGetHTTPFunc; ShardManager: ICloudShardManager; HashCalculator: ICloudHashCalculator; Cipher: ICipher; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; TCHandler: ITCHandler; GetOAuthToken: TGetOAuthTokenFunc; IsPublicAccount: TGetBoolFunc; GetRetryOperation: TGetRetryOperationFunc; GetUnitedParams: TGetUnitedParamsFunc; CloudResultToFsResult: TCloudResultToFsResultFunc; DeleteFile: TDeleteFileFunc; DoCryptFiles, DoCryptFilenames: Boolean; Settings: TUploadSettings);
begin
	inherited Create;
	FGetHTTP := GetHTTP;
	FShardManager := ShardManager;
	FHashCalculator := HashCalculator;
	FCipher := Cipher;
	FFileSystem := FileSystem;
	FLogger := Logger;
	FProgress := Progress;
	FRequest := Request;
	FTCHandler := TCHandler;
	FGetOAuthToken := GetOAuthToken;
	FIsPublicAccount := IsPublicAccount;
	FGetRetryOperation := GetRetryOperation;
	FGetUnitedParams := GetUnitedParams;
	FCloudResultToFsResult := CloudResultToFsResult;
	FDeleteFile := DeleteFile;
	FDoCryptFiles := DoCryptFiles;
	FDoCryptFilenames := DoCryptFilenames;
	FSettings := Settings;
end;

function TCloudFileUploader.AddFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath: WideString; ConflictMode: WideString; LogErrors: Boolean; LogSuccess: Boolean): Integer;
var
	FileName: WideString;
	CallResult: TAPICallResult;
	{Explicit Self capture for anonymous function closure}
	HTTP: ICloudHTTP;
	UnitedParams: WideString;
	CloudResultToFsResult: TCloudResultToFsResultFunc;
	Logger: ILogger;
	RetryOp: TRetryOperation;
begin
	if FIsPublicAccount() then
		Exit(FS_FILE_NOTSUPPORTED);

	if FDoCryptFilenames then
	begin
		FileName := ExtractUniversalFileName(RemotePath);
		FileName := FCipher.CryptFileName(FileName);
		RemotePath := ChangePathFileName(RemotePath, FileName);
	end;

	{Capture values before anonymous function to avoid Self capture issues}
	HTTP := FGetHTTP();
	UnitedParams := FGetUnitedParams();
	CloudResultToFsResult := FCloudResultToFsResult;
	Logger := FLogger;
	RetryOp := FGetRetryOperation();

	CallResult := RetryOp.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			OperationResult: TCMROperationResult;
			ResultCode: Integer;
		begin
			if HTTP.PostForm(API_FILE_ADD + '?' + UnitedParams, Format('api=2&conflict=%s&home=/%s&hash=%s&size=%d', [ConflictMode, PathToUrl(RemotePath), FileIdentity.Hash, FileIdentity.size]), JSON, 'application/x-www-form-urlencoded', LogErrors, False) then
			begin
				TCMROperationResultJsonAdapter.Parse(JSON, OperationResult);
				ResultCode := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_UPLOADING);
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
	if FIsPublicAccount() then
		Exit(FS_FILE_NOTSUPPORTED);
	FGetHTTP().SetProgressNames(LocalPath, RemotePath);
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
	FileStream := TBufferedFileStream.Create(GetUNCFilePath(LocalPath), fmOpenRead or fmShareDenyWrite);
	try
		Result := PutFileStream(ExtractFileName(RemotePath), RemotePath, FileStream, ConflictMode);
	finally
		FileStream.Free;
	end;
end;

function TCloudFileUploader.PutFileStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString): Integer;
var
	LocalFileIdentity, RemoteFileIdentity: TCMRFileIdentity;
	OperationResult: Integer;
	MemoryStream: TMemoryStream;
	UseHash: Boolean;
begin
	Result := FS_FILE_WRITEERROR;
	OperationResult := CLOUD_OPERATION_FAILED;

	UseHash := FSettings.PrecalculateHash or (FSettings.ForcePrecalculateSize >= FileStream.size); {issue #231}

	if UseHash or FSettings.CheckCRC then
	begin
		LocalFileIdentity.Hash := FHashCalculator.CalculateHash(FileStream, CALCULATING_HASH);
		LocalFileIdentity.size := FileStream.size;
	end;
	if UseHash and (LocalFileIdentity.Hash <> EmptyWideStr) and (not FDoCryptFiles) and (FS_FILE_OK = AddFileByIdentity(LocalFileIdentity, RemotePath, CLOUD_CONFLICT_STRICT, False, True)) then {issue #135}
		Exit(CLOUD_OPERATION_OK);

	try
		if FDoCryptFiles then {Will encrypt any type of data passed here}
		begin
			MemoryStream := TMemoryStream.Create;
			try
				FCipher.CryptStream(FileStream, MemoryStream);
				MemoryStream.Position := 0;
				OperationResult := PutFileToCloud(FileName, MemoryStream, RemoteFileIdentity);
			finally
				MemoryStream.Free;
			end;
		end else begin
			OperationResult := PutFileToCloud(FileName, FileStream, RemoteFileIdentity)
		end;
	except
		on E: Exception do
		begin
			if E.ClassName = 'EAbort' then
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
				Result := CLOUD_OPERATION_FAILED;
		end;
	end else if OperationResult = CLOUD_OPERATION_CANCELLED then
	begin
		Result := FS_FILE_USERABORT;
	end;

	if OperationResult = CLOUD_OPERATION_OK then
		Result := AddFileByIdentity(RemoteFileIdentity, RemotePath, ConflictMode, False, False);
end;

function TCloudFileUploader.PutFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCMRFileIdentity): Integer;
var
	CallResult: TAPICallResult;
	LocalIdentity: TCMRFileIdentity;
	DispatcherResponse: WideString;
	Progress: Boolean;
	UploadShard: WideString;
	OAuthToken: TCMROAuth;
begin
	FileIdentity.Hash := EmptyWideStr;
	FileIdentity.size := -1;
	Result := CLOUD_OPERATION_FAILED;
	if FIsPublicAccount() then
		Exit;
	UploadShard := FShardManager.GetUploadShard;
	if (EmptyWideStr = UploadShard) then
	begin
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, UNDEFINED_UPLOAD_SHARD);
		if FShardManager.HasUploadOverride then
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_DETAILS, UPLOAD_URL_OVERRIDDEN);
			UploadShard := FShardManager.GetUploadShardOverride;
			FShardManager.SetUploadShard(UploadShard);
		end else begin
			{OAuth uses different dispatcher endpoint that returns plain text URL}
			Progress := False;
			OAuthToken := FGetOAuthToken();
			if FGetHTTP().GetPage(Format('%s/u?token=%s', [OAUTH_DISPATCHER_URL, OAuthToken.access_token]), DispatcherResponse, Progress) then
			begin
				{Response format: "URL IP COUNT", extract the URL (first word)}
				UploadShard := Trim(Copy(DispatcherResponse, 1, Pos(' ', DispatcherResponse) - 1));
				FShardManager.SetUploadShard(UploadShard);
			end;
		end
	end;

	LocalIdentity.Hash := EmptyWideStr;
	LocalIdentity.size := -1;
	OAuthToken := FGetOAuthToken();
	CallResult := FGetRetryOperation().Execute(
		function: TAPICallResult
		var
			PostAnswer: WideString;
			UploadUrl: WideString;
			ResultCode: Integer;
		begin
			{OAuth requires only client_id and token parameters for upload authentication}
			UploadUrl := Format('%s?client_id=%s&token=%s', [FShardManager.GetUploadShard, OAUTH_CLIENT_ID, OAuthToken.access_token]);
			ResultCode := FGetHTTP().PutFile(UploadUrl, FileName, FileStream, PostAnswer);
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
				if not FDeleteFile(ChunkRemotePath) then
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
function TCloudFileUploader.CalculateFileIdentity(LocalPath: WideString): TCMRFileIdentity;
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
	FGetHTTP().SetProgressNames(LocalPath, CRCRemotePath);
	CRCStream := TStringStream.Create;
	try
		SplitFileInfo.GetCRCData(CRCStream);
		PutFileStream(SplitFileInfo.CRCFileName, CRCRemotePath, CRCStream, ConflictMode);
	finally
		CRCStream.Free;
	end;
end;

{Uploads file in chunks when it exceeds CloudMaxFileSize.
	Returns FS_FILE_OK on success, or appropriate error code on failure.}
function TCloudFileUploader.PutFileSplit(LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
var
	LocalFileIdentity: TCMRFileIdentity;
	SplitFileInfo: TFileSplitInfo;
	ChunkIndex: Integer;
	ChunkRemotePath: WideString;
	ChunkStream: TChunkedFileStream;
	RetryAttemptsCount: Integer;
	UseHash: Boolean;
	Action: TChunkActionResult;
begin
	{Try hash deduplication first - may skip upload entirely if file already exists}
	UseHash := FSettings.PrecalculateHash or (FSettings.ForcePrecalculateSize >= FFileSystem.GetFileSize(LocalPath)); {issue #231}
	if UseHash then
		LocalFileIdentity := CalculateFileIdentity(GetUNCFilePath(LocalPath));
	{Hash calculation cancellation causes entire operation abort - TC remembers cancel press}
	if UseHash and (LocalFileIdentity.Hash <> EmptyWideStr) and (not FDoCryptFiles) and (FS_FILE_OK = AddFileByIdentity(LocalFileIdentity, RemotePath, CLOUD_CONFLICT_STRICT, False, True)) then
		Exit(CLOUD_OPERATION_OK); {issue #135}

	{Create split info to determine chunk boundaries}
	SplitFileInfo := TFileSplitInfo.Create(GetUNCFilePath(LocalPath), FSettings.CloudMaxFileSize);
	try
		ChunkIndex := 0;
		RetryAttemptsCount := 0;
		Result := FS_FILE_OK;

		{Upload each chunk - using while loop because we may need to retry (dec index)}
		while ChunkIndex < SplitFileInfo.ChunksCount do
		begin
			ChunkRemotePath := Format('%s%s', [ExtractFilePath(RemotePath), SplitFileInfo.GetChunks[ChunkIndex].name]);
			FGetHTTP().SetProgressNames(LocalPath, ChunkRemotePath);
			FLogger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PARTIAL_UPLOAD_INFO, [LocalPath, ChunkIndex + 1, SplitFileInfo.ChunksCount, ChunkRemotePath]);

			{Upload current chunk}
			ChunkStream := TChunkedFileStream.Create(GetUNCFilePath(LocalPath), fmOpenRead or fmShareDenyWrite, SplitFileInfo.GetChunks[ChunkIndex].start, SplitFileInfo.GetChunks[ChunkIndex].size);
			try
				Result := PutFileStream(ExtractFileName(ChunkRemotePath), ChunkRemotePath, ChunkStream, ConflictMode);
			finally
				ChunkStream.Free;
			end;

			{Handle upload result}
			case Result of
				FS_FILE_OK:
					begin
						RetryAttemptsCount := 0;
						Inc(ChunkIndex);
					end;
				FS_FILE_USERABORT:
					begin
						FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PARTIAL_UPLOAD_ABORTED);
						Break; {Exit loop preserving USERABORT result}
					end;
				FS_FILE_EXISTS:
					begin
						Action := HandleChunkExists(ChunkRemotePath, ChunkOverwriteMode, Result);
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

		{Generate and upload CRC file only after all chunks uploaded successfully}
		if Result = FS_FILE_OK then
			GenerateAndUploadCRC(LocalPath, RemotePath, SplitFileInfo, ConflictMode);
	finally
		SplitFileInfo.Free;
	end;
end;

end.
