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
	Logger,
	Progress,
	Request,
	TCHandler,
	Cipher,
	PathHelper,
	StringHelper,
	SystemHelper,
	CloudShardManager,
	CloudHashCalculator,
	CloudHTTP,
	CloudErrorMapper,
	CloudContext,
	FileSystem,
	TokenRetryHelper,
	ChunkedUploadHandler;

type
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
		{Upload from an arbitrary stream to cloud.
			KnownHash skips redundant hash calculation and dedup when hash is already known.
			Returns FS_FILE_OK on success, or appropriate error code on failure.}
		function UploadStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString; KnownHash: WideString = ''): Integer;
	end;

	{File upload service - handles whole file and delegates chunked uploads}
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
		FChunkedHandler: IChunkedUploadHandler;

		{Internal upload methods}
		function PutFileWhole(LocalPath, RemotePath, ConflictMode: WideString): Integer;
		function PutFileStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString; KnownHash: WideString = ''): Integer;
		function PutFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCloudFileIdentity): Integer;

		{Create chunked handler on demand}
		function GetChunkedHandler: IChunkedUploadHandler;
	protected
		{Protected for testability - tests need direct access to split upload logic}
		function PutFileSplit(LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
	public
		constructor Create(Context: ICloudContext; ShardManager: ICloudShardManager; HashCalculator: ICloudHashCalculator; Cipher: ICipher; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; TCHandler: ITCHandler; RetryOperation: IRetryOperation; DoCryptFiles: Boolean; Settings: TUploadSettings);

		{ICloudFileUploader}
		function Upload(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer;
		function AddFileByIdentity(FileIdentity: TCloudFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = True; LogSuccess: Boolean = False): Integer;
		function UploadStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString; KnownHash: WideString = ''): Integer;
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
	Context: ICloudContext;
	Logger: ILogger;
begin
	if FContext.IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);

	{Capture values before anonymous function to avoid Self capture issues}
	HTTP := FContext.GetHTTP;
	Context := FContext;
	Logger := FLogger;

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			OperationResult: TCloudOperationResult;
			ResultCode: Integer;
		begin
			if HTTP.PostForm(Context.GetEndpoints.ApiFileAdd + '?' + Context.GetUnitedParams, Format('api=2&conflict=%s&home=/%s&hash=%s&size=%d', [ConflictMode, PathToUrl(RemotePath), FileIdentity.Hash, FileIdentity.size]), JSON, 'application/x-www-form-urlencoded', LogErrors, False) then
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
		{Direct instantiation is intentional - injecting a file stream factory provides no practical
			benefit here: tests work fine with real temp files, and the exception handling is trivial.}
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

function TCloudFileUploader.PutFileStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString; KnownHash: WideString): Integer;
var
	LocalFileIdentity, RemoteFileIdentity: TCloudFileIdentity;
	OperationResult: Integer;
	DedupeResult: Integer;
	UploadStream: TStream;
	UseHash: Boolean;
begin
	Result := FS_FILE_WRITEERROR;
	OperationResult := CLOUD_OPERATION_FAILED;

	if KnownHash <> EmptyWideStr then
	begin
		{Hash already known from caller (e.g. cross-server transfer) -- skip
			expensive recalculation and redundant dedup attempt}
		LocalFileIdentity.Hash := KnownHash;
		LocalFileIdentity.size := FileStream.size;
	end else begin
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
		if FSettings.CheckCRC and (not FDoCryptFiles) then
		begin
			if not LocalFileIdentity.IsEqualTo(RemoteFileIdentity) then {When CRC check is enabled, compare hashes and sizes}
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

{Internal stream upload - delegates to PutFileStream}
function TCloudFileUploader.UploadStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString; KnownHash: WideString): Integer;
begin
	FContext.GetHTTP.SetProgressNames(FileName, RemotePath);
	Result := PutFileStream(FileName, RemotePath, FileStream, ConflictMode, KnownHash);
end;

{Create chunked handler on demand with current settings}
function TCloudFileUploader.GetChunkedHandler: IChunkedUploadHandler;
var
	ChunkedSettings: TChunkedUploadSettings;
begin
	if FChunkedHandler = nil then
	begin
		ChunkedSettings.CloudMaxFileSize := FSettings.CloudMaxFileSize;
		ChunkedSettings.PrecalculateHash := FSettings.PrecalculateHash;
		ChunkedSettings.ForcePrecalculateSize := FSettings.ForcePrecalculateSize;
		ChunkedSettings.OperationErrorMode := FSettings.OperationErrorMode;
		ChunkedSettings.RetryAttempts := FSettings.RetryAttempts;
		ChunkedSettings.AttemptWait := FSettings.AttemptWait;

		FChunkedHandler := TChunkedUploadHandler.Create(
			FContext,
			FHashCalculator,
			FFileSystem,
			FLogger,
			FProgress,
			FRequest,
			FTCHandler,
			FDoCryptFiles,
			ChunkedSettings);
	end;
	Result := FChunkedHandler;
end;

{Delegates chunked upload to TChunkedUploadHandler.
	Kept as protected method for backward compatibility with tests.}
function TCloudFileUploader.PutFileSplit(LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
begin
	{Pass method references - avoids interface resolution issues with Self in subclasses}
	Result := GetChunkedHandler.Upload(
		function(AFileName, ARemotePath: WideString; AFileStream: TStream; AConflictMode: WideString): Integer
		begin
			Result := PutFileStream(AFileName, ARemotePath, AFileStream, AConflictMode);
		end,
		function(AFileIdentity: TCloudFileIdentity; ARemotePath: WideString; AConflictMode: WideString; ALogErrors: Boolean; ALogSuccess: Boolean): Integer
		begin
			Result := AddFileByIdentity(AFileIdentity, ARemotePath, AConflictMode, ALogErrors, ALogSuccess);
		end,
		LocalPath, RemotePath, ConflictMode, ChunkOverwriteMode);
end;

end.
