unit CloudMailRu;

interface

uses
	DebugHelper,
	CMRDirItemList,
	CMRDirItem,
	CMRInviteList,
	CMRIncomingInviteList,
	CMROAuth,
	CMRSpace,
	CMRFileIdentity,
	CMROperationResult,
	CMRTwostep,
	JSONHelper,
	ParsingHelper,
	CMRConstants,
	CloudHTTP,
	LANGUAGE_STRINGS,
	HashInfo,
	System.Hash,
	System.Classes,
	System.Generics.Collections,
	System.SysUtils,
	SETTINGS_CONSTANTS,
	PLUGIN_TYPES,
	Winapi.Windows,
	PluginHelper,
	PathHelper,
	FileHelper,
	StringHelper,
	SystemHelper,
	WindowsHelper,
	TCLogger,
	TCProgress,
	TCRequest,
	FileCipher,
	RealPath,
	CloudSettings,
	FileSplitInfo,
	ChunkedFileStream,
	HTTPManager,
	WindowsFileSystem,
	IdCookieManager,
	DCPbase64,
	IAuthStrategyInterface,
	TokenRetryHelper,
	CloudHashCalculator,
	CloudShardManager,
	CloudErrorMapper,
	CloudFileDownloader,
	CloudFileUploader,
	CloudShareService,
	CloudListingService,
	CloudFileOperations;

type
	TCloudMailRu = class
	private
		FSettings: TCloudSettings; {Current options set for the cloud instance}

		FHTTPManager: IHTTPManager; {Internal connections manager (can be set externally or omitted)}
		FHTTPConnection: ICloudHTTP; {Normally managed by HTTPConnectionsManager. If HTTPConnectionsManager is omitted, Cloud will create its own atomic connection}

		FCookieManager: TIdCookieManager; {The auth cookie, should be stored separately, because it associated with a cloud instance, not a connection}

		FLogger: ILogger;
		FProgress: IProgress;
		FRequest: IRequest;

		FCipher: ICipher; {The encryption instance}
		FAuthStrategy: IAuthStrategy; {Authentication strategy}
		FFileSystem: IFileSystem; {File system abstraction for testability}
		FRetryOperation: TRetryOperation; {Token refresh retry handler}
		FHashCalculator: ICloudHashCalculator; {Cloud hash calculation service}
		FDownloader: ICloudFileDownloader; {File download service}
		FUploader: ICloudFileUploader; {File upload service}
		FShareService: ICloudShareService; {Share and publish service}
		FListingService: ICloudListingService; {Directory listing service}
		FFileOps: ICloudFileOperations; {File operations service}

		{HTTP REQUESTS WRAPPERS}
		function InitSharedConnectionParameters(): Boolean;
		function GetShard(var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean;

		{OTHER ROUTINES}
		function GetHTTPConnection: ICloudHTTP;
		function RefreshCSRFToken: Boolean;
	protected
		FUser: WideString;
		FDomain: WideString;
		FDoCryptFiles: Boolean;
		FDoCryptFilenames: Boolean;
		FPublicLink: WideString; {Holder for GetPublicLink() value - protected for testability}
		FUnitedParams: WideString; {The set of required authentification attributes united to the string}

		{Protected for testability}
		FShardManager: ICloudShardManager; {Shard URL caching and management}
		FAuthToken: WideString; {The current (constantly refreshing) connection token}
		FOAuthToken: TCMROAuth; {OAuth token data}
		{HTTP REQUESTS WRAPPERS - protected for testability}
		function GetUserSpace(var SpaceInfo: TCMRSpace): Boolean;
		{HASHING - exposed for testing via subclass}
		function CloudHash(Path: WideString): WideString; overload; //get cloud hash for specified file
		function CloudHash(Stream: TStream; Path: WideString = CALCULATING_HASH): WideString; overload; //get cloud hash for data in stream
		{Those properties are simple shortcuts to settings fields}
		property Password: WideString read FSettings.AccountSettings.Password;
		property Email: WideString read FSettings.AccountSettings.Email;
		property DownloadShardOverride: WideString read FSettings.AccountSettings.ShardOverride;
		property UploadShardOverride: WideString read FSettings.AccountSettings.UploadUrlOverride;
		property UnlimitedFileSize: Boolean read FSettings.AccountSettings.UnlimitedFileSize;
		property SplitLargeFiles: Boolean read FSettings.AccountSettings.SplitLargeFiles;
		property CloudMaxFileSize: Int64 read FSettings.CloudMaxFileSize;
		property PrecalculateHash: Boolean read FSettings.PrecalculateHash;
		property ForcePrecalculateSize: Int64 read FSettings.ForcePrecalculateSize;
		property CheckCRC: Boolean read FSettings.CheckCRC;
		property OperationErrorMode: Integer read FSettings.OperationErrorMode;
		property RetryAttempts: Integer read FSettings.RetryAttempts;
		property AttemptWait: Integer read FSettings.AttemptWait;
		{Also shortcut properties}
		property HTTP: ICloudHTTP read GetHTTPConnection;

		{REGULAR CLOUD}
		function LoginRegular(Method: Integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function GetFileRegular(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = true): Integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		{SHARED WEBFOLDERS}
		function LoginShared(Method: Integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function GetFileShared(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = true): Integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		function GetPublicLink(): WideString;
	public
		property IsPublicAccount: Boolean read FSettings.AccountSettings.PublicAccount;
		{ERROR RESULT MAPPING - exposed for testing and external use}
		function CloudResultToFsResult(CloudResult: TCMROperationResult; ErrorPrefix: WideString = ''): Integer; overload;
		function CloudResultToFsResult(JSON: WideString; ErrorPrefix: WideString = ''): Integer; overload;
		function CloudResultToBoolean(CloudResult: TCMROperationResult; ErrorPrefix: WideString = ''): Boolean; overload;
		function CloudResultToBoolean(JSON: WideString; ErrorPrefix: WideString = ''): Boolean; overload;
		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(CloudSettings: TCloudSettings; ConnectionManager: IHTTPManager; AuthStrategy: IAuthStrategy; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; Cipher: ICipher = nil);
		destructor Destroy; override;
		{CLOUD INTERFACE METHODS}
		function Login(Method: Integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function GetDirListing(Path: WideString; var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function GetSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
		function GetSharedLinksListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function GetIncomingLinksListing(var IncomingListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean; overload;
		function GetIncomingLinksListing(var IncomingListing: TCMRDirItemList; var InvitesListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean; overload;
		function GetTrashbinListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function CreateDir(Path: WideString): Boolean;
		function RemoveDir(Path: WideString): Boolean;
		function StatusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
		function GetFile(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = true): Integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		function PutFile(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer;
		function RenameFile(OldName, NewName: WideString): Integer; //смена имени без перемещения
		function MoveFile(OldName, ToPath: WideString): Integer; //перемещение по дереву каталогов
		function CopyFile(OldName, ToPath: WideString): Integer; //Копирование файла внутри одного аккаунта
		function FileMove(OldName, NewName: WideString): Integer; //объединяющая функция, определяет делать rename или move
		function FileCopy(OldName, NewName: WideString): Integer; //Копирует файл, и переименует, если нужно
		function DeleteFile(Path: WideString): Boolean;
		function PublishFile(Path: WideString; var PublicLink: WideString; Publish: Boolean = CLOUD_PUBLISH): Boolean;
		function CloneWeblink(Path, Link: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Integer; //клонировать публичную ссылку в текущий каталог

		function AddFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = False): Integer; overload;
		function AddFileByIdentity(FileIdentity: TCMRDirItem; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = False): Integer; overload;

		function GetShareInfo(Path: WideString; var InviteListing: TCMRInviteList): Boolean;
		function ShareFolder(Path, Email: WideString; Access: Integer): Boolean;
		function TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function TrashbinEmpty(): Boolean;
		function MountFolder(Home, InviteToken: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function UnmountFolder(Home: WideString; Clone_copy: Boolean): Boolean;
		function RejectInvite(InviteToken: WideString): Boolean;
		function GetPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; Publish: Boolean = CLOUD_PUBLISH): Boolean;
		{OTHER ROUTINES}
		function GetDescriptionFile(RemotePath, LocalCopy: WideString): Boolean; //Если в каталоге remotePath есть descript.ion - скопировать его в файл localcopy
		function PutDescriptionFile(RemotePath, LocalCopy: WideString): Boolean; //Скопировать descript.ion из временного файла на сервер
		procedure LogUserSpaceInfo();
		function FileIdentity(LocalPath: WideString): TCMRFileIdentity;
		{STATIC ROUTINES}
		class function CloudAccessToString(Access: WideString; Invert: Boolean = False): WideString; static;
		class function StringToCloudAccess(AccessString: WideString; Invert: Boolean = False): Integer; static;
		class function ErrorCodeText(ErrorCode: Integer): WideString; static;
		class function TempPublicCloudInit(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean; static;
	end;

implementation

{TCloudMailRu}
function TCloudMailRu.AddFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = False): Integer;
var
	FileName: WideString;
	CallResult: TAPICallResult;
begin
	if IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);

	if FDoCryptFilenames then
	begin
		FileName := ExtractUniversalFileName(RemotePath);
		FileName := FCipher.CryptFileName(FileName);
		RemotePath := ChangePathFileName(RemotePath, FileName);
	end;

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			OperationResult: TCMROperationResult;
			ResultCode: Integer;
		begin
			{Экспериментально выяснено, что параметры api, build, email, x-email, x-page-id в запросе не обязательны}
			if HTTP.PostForm(API_FILE_ADD + '?' + FUnitedParams, Format('api=2&conflict=%s&home=/%s&hash=%s&size=%d', [ConflictMode, PathToUrl(RemotePath), FileIdentity.Hash, FileIdentity.size]), JSON, 'application/x-www-form-urlencoded', LogErrors, False) then {Do not allow to cancel operation here}
			begin
				OperationResult.FromJSON(JSON);
				ResultCode := CloudResultToFsResult(OperationResult, PREFIX_ERR_FILE_UPLOADING);
				if (CLOUD_OPERATION_OK = OperationResult.OperationResult) and LogSuccess then
					FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, FILE_FOUND_BY_HASH, [RemotePath]);
			end else
				ResultCode := FS_FILE_WRITEERROR;
			Result := TAPICallResult.FromInteger(ResultCode, JSON);
		end);

	Result := CallResult.ResultCode;
end;

function TCloudMailRu.AddFileByIdentity(FileIdentity: TCMRDirItem; RemotePath, ConflictMode: WideString; LogErrors, LogSuccess: Boolean): Integer;
var
	CloudFileIdentity: TCMRFileIdentity;
begin
	CloudFileIdentity.Hash := FileIdentity.Hash;
	CloudFileIdentity.size := FileIdentity.size;
	Result := AddFileByIdentity(CloudFileIdentity, RemotePath, ConflictMode, LogErrors, LogSuccess)
end;

function TCloudMailRu.CloneWeblink(Path, Link, ConflictMode: WideString): Integer;
var
	CallResult: TAPICallResult;
begin
	if IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);

	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Progress: Boolean;
			ResultCode: Integer;
		begin
			Progress := true;
			if HTTP.GetPage(Format('%s?folder=/%s&weblink=%s&conflict=%s&%s',
				[API_CLONE, PathToUrl(Path), Link, ConflictMode, FUnitedParams]), JSON, Progress) then
			begin
				ResultCode := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_PUBLISH);
				if (ResultCode <> FS_FILE_OK) and not(Progress) then
					ResultCode := FS_FILE_USERABORT; {user cancelled}
			end else
				ResultCode := FS_FILE_WRITEERROR;
			Result := TAPICallResult.FromInteger(ResultCode, JSON);
		end);

	Result := CallResult.ResultCode;
end;

{Delegates to TCloudErrorMapper - kept for backward compatibility}
function TCloudMailRu.CloudResultToBoolean(JSON, ErrorPrefix: WideString): Boolean;
begin
	Result := TCloudErrorMapper.ToBoolean(JSON, FLogger, ErrorPrefix);
end;

{Delegates to TCloudErrorMapper - kept for backward compatibility}
function TCloudMailRu.CloudResultToBoolean(CloudResult: TCMROperationResult; ErrorPrefix: WideString): Boolean;
begin
	Result := TCloudErrorMapper.ToBoolean(CloudResult, FLogger, ErrorPrefix);
end;

{Delegates to TCloudErrorMapper - kept for backward compatibility}
function TCloudMailRu.CloudResultToFsResult(JSON, ErrorPrefix: WideString): Integer;
begin
	Result := TCloudErrorMapper.ToFsResult(JSON, FLogger, ErrorPrefix);
end;

{Delegates to TCloudErrorMapper - kept for backward compatibility}
function TCloudMailRu.CloudResultToFsResult(CloudResult: TCMROperationResult; ErrorPrefix: WideString): Integer;
begin
	Result := TCloudErrorMapper.ToFsResult(CloudResult, FLogger, ErrorPrefix);
end;

{Delegates to FFileOps}
function TCloudMailRu.CopyFile(OldName, ToPath: WideString): Integer;
begin
	Result := FFileOps.CopyToPath(OldName, ToPath);
end;

{Delegates to FFileOps}
function TCloudMailRu.FileCopy(OldName, NewName: WideString): Integer;
begin
	Result := FFileOps.Copy(OldName, NewName);
end;

constructor TCloudMailRu.Create(CloudSettings: TCloudSettings; ConnectionManager: IHTTPManager; AuthStrategy: IAuthStrategy; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; Cipher: ICipher = nil);
begin
	try
		FSettings := CloudSettings;
		ExtractEmailParts(Email, FUser, FDomain);

		FHTTPManager := ConnectionManager;
		FAuthStrategy := AuthStrategy;
		FFileSystem := FileSystem;

		FProgress := Progress;
		FLogger := Logger;
		FRequest := Request;

		FCookieManager := TIdCookieManager.Create();

		{Initialize hash calculator service}
		FHashCalculator := TCloudHashCalculator.Create(Progress, FileSystem);

		{Initialize shard manager with overrides from settings}
		FShardManager := TCloudShardManager.Create(Logger,
			FSettings.AccountSettings.ShardOverride,
			FSettings.AccountSettings.UploadUrlOverride);

		{Initialize retry operation handler with HTTP callbacks}
		FRetryOperation := TRetryOperation.Create(
			function: Boolean begin Result := RefreshCSRFToken; end,
			function(const URL, Params: WideString; var JSON: WideString): Boolean begin Result := HTTP.PostForm(URL, Params, JSON); end,
			function(const URL: WideString; var JSON: WideString; var ShowProgress: Boolean): Boolean begin Result := HTTP.GetPage(URL, JSON, ShowProgress); end,
			function(const JSON, ErrorPrefix: WideString): Boolean begin Result := CloudResultToBoolean(JSON, ErrorPrefix); end,
			function(const JSON, ErrorPrefix: WideString): Integer begin Result := CloudResultToFsResult(JSON, ErrorPrefix); end);

		{Use injected cipher for encryption operations}
		FCipher := Cipher;
		FDoCryptFiles := Assigned(Cipher);
		FDoCryptFilenames := FDoCryptFiles and FSettings.AccountSettings.EncryptFilenames;

		{Initialize file downloader service with callbacks for dynamic state}
		FDownloader := TCloudFileDownloader.Create(
			function: ICloudHTTP begin Result := Self.HTTP; end,
			FShardManager,
			FHashCalculator,
			FCipher,
			FFileSystem,
			FLogger,
			FProgress,
			FRequest,
			function: TCMROAuth begin Result := Self.FOAuthToken; end,
			function: Boolean begin Result := Self.IsPublicAccount; end,
			function: WideString begin Result := Self.GetPublicLink; end,
			function: Boolean begin Result := Self.RefreshCSRFToken; end,
			function(var Shard: WideString; ShardType: WideString): Boolean begin Result := Self.GetShard(Shard, ShardType); end,
			FDoCryptFiles,
			FDoCryptFilenames);

		{Initialize file uploader service with callbacks and settings}
		var UploadSettings: TUploadSettings;
		UploadSettings.PrecalculateHash := Self.PrecalculateHash;
		UploadSettings.ForcePrecalculateSize := Self.ForcePrecalculateSize;
		UploadSettings.CheckCRC := Self.CheckCRC;
		UploadSettings.OperationErrorMode := Self.OperationErrorMode;
		UploadSettings.RetryAttempts := Self.RetryAttempts;
		UploadSettings.AttemptWait := Self.AttemptWait;
		UploadSettings.UnlimitedFileSize := Self.UnlimitedFileSize;
		UploadSettings.SplitLargeFiles := Self.SplitLargeFiles;
		UploadSettings.CloudMaxFileSize := Self.CloudMaxFileSize;

		FUploader := TCloudFileUploader.Create(
			function: ICloudHTTP begin Result := Self.HTTP; end,
			FShardManager,
			FHashCalculator,
			FCipher,
			FFileSystem,
			FLogger,
			FProgress,
			FRequest,
			function: TCMROAuth begin Result := Self.FOAuthToken; end,
			function: Boolean begin Result := Self.IsPublicAccount; end,
			function: TRetryOperation begin Result := Self.FRetryOperation; end,
			function(FileIdentity: TCMRFileIdentity; RemotePath, ConflictMode: WideString; LogErrors, LogSuccess: Boolean): Integer begin Result := Self.AddFileByIdentity(FileIdentity, RemotePath, ConflictMode, LogErrors, LogSuccess); end,
			function(Path: WideString): Boolean begin Result := Self.DeleteFile(Path); end,
			function(LocalPath: WideString): TCMRFileIdentity begin Result := Self.FileIdentity(LocalPath); end,
			FDoCryptFiles,
			FDoCryptFilenames,
			UploadSettings);

		{Initialize share service with callbacks for dynamic state}
		FShareService := TCloudShareService.Create(
			Self.HTTP,
			FLogger,
			FRetryOperation,
			function: Boolean begin Result := Self.IsPublicAccount; end,
			function: WideString begin Result := Self.FUnitedParams; end,
			function(Path: WideString; var PublicLink: WideString; Publish: Boolean): Boolean begin Result := Self.PublishFile(Path, PublicLink, Publish); end,
			function(JSON: WideString; ErrorPrefix: WideString): Boolean begin Result := Self.CloudResultToBoolean(JSON, ErrorPrefix); end,
			function(var Shard: WideString; ShardType: WideString): Boolean begin Result := Self.GetShard(Shard, ShardType); end
		);

		{Initialize listing service with callbacks for dynamic state}
		FListingService := TCloudListingService.Create(
			Self.HTTP,
			FCipher,
			FLogger,
			FRetryOperation,
			function: Boolean begin Result := Self.IsPublicAccount; end,
			function: WideString begin Result := Self.FUnitedParams; end,
			function: WideString begin Result := Self.GetPublicLink; end,
			function(JSON: WideString; ErrorPrefix: WideString): Boolean begin Result := Self.CloudResultToBoolean(JSON, ErrorPrefix); end,
			function(OperationResult: TCMROperationResult; ErrorPrefix: WideString): Boolean begin Result := Self.CloudResultToBoolean(OperationResult, ErrorPrefix); end,
			FDoCryptFilenames
		);

		{Initialize file operations service with callbacks for dynamic state}
		FFileOps := TCloudFileOperations.Create(
			Self.HTTP,
			FLogger,
			FRetryOperation,
			function: Boolean begin Result := Self.IsPublicAccount; end,
			function: WideString begin Result := Self.FUnitedParams; end
		);

	except
		on E: Exception do
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_ERR_FILE_UPLOADING, E.Message]);
		end;
	end;
end;

{Delegates to FFileOps}
function TCloudMailRu.CreateDir(Path: WideString): Boolean;
begin
	Result := FFileOps.CreateDirectory(Path);
end;

{Delegates to FFileOps}
function TCloudMailRu.DeleteFile(Path: WideString): Boolean;
begin
	Result := FFileOps.Delete(Path);
end;

destructor TCloudMailRu.Destroy;
begin
	//HTTP.Destroy;

	FCookieManager.Destroy;

	{Interface reference - will be released automatically}
	FHTTPConnection := nil;

	if Assigned(FRetryOperation) then
		FRetryOperation.Free;

	FCipher := nil; {Release interface reference}
	FHashCalculator := nil; {Release interface reference}
	FShardManager := nil; {Release interface reference}
	FDownloader := nil; {Release interface reference}
	FUploader := nil; {Release interface reference}
	FShareService := nil; {Release interface reference}
	FListingService := nil; {Release interface reference}
	FFileOps := nil; {Release interface reference}

	inherited;
end;

{Delegates to TCloudErrorMapper - kept for backward compatibility}
class function TCloudMailRu.ErrorCodeText(ErrorCode: Integer): WideString;
begin
	Result := TCloudErrorMapper.ErrorCodeText(ErrorCode);
end;

function TCloudMailRu.FileIdentity(LocalPath: WideString): TCMRFileIdentity;
begin
	Result.Hash := CloudHash(LocalPath);
	Result.size := FFileSystem.GetFileSize(LocalPath);
end;

function TCloudMailRu.GetDescriptionFile(RemotePath, LocalCopy: WideString): Boolean;
var
	ResultHash: WideString;
begin
	Result := GetFile(RemotePath, LocalCopy, ResultHash, False) = FS_FILE_OK;
end;

function TCloudMailRu.PutDescriptionFile(RemotePath, LocalCopy: WideString): Boolean;
begin
	if FFileSystem.FileExists(LocalCopy) then
		Result := PutFile(LocalCopy, RemotePath) = FS_FILE_OK
	else
		Result := DeleteFile(RemotePath);
end;

function TCloudMailRu.GetSharedLinksListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
begin
	Result := FListingService.GetSharedLinks(DirListing, ShowProgress);
end;

function TCloudMailRu.GetIncomingLinksListing(var IncomingListing: TCMRIncomingInviteList; ShowProgress: Boolean): Boolean;
begin
	Result := FListingService.GetIncomingInvites(IncomingListing, ShowProgress);
end;

function TCloudMailRu.GetIncomingLinksListing(var IncomingListing: TCMRDirItemList; var InvitesListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean;
begin
	Result := FListingService.GetIncomingInvitesAsDirItems(IncomingListing, InvitesListing, ShowProgress);
end;

function TCloudMailRu.GetTrashbinListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean): Boolean;
begin
	Result := FListingService.GetTrashbin(DirListing, ShowProgress);
end;

function TCloudMailRu.GetDirListing(Path: WideString; var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
begin
	Result := FListingService.GetDirectory(Path, DirListing, ShowProgress);
end;

{Delegates to FDownloader - kept for backward compatibility}
function TCloudMailRu.GetFile(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
begin
	Result := FDownloader.Download(RemotePath, LocalPath, ResultHash, LogErrors);
end;

{Delegates to FDownloader - kept for backward compatibility with tests}
function TCloudMailRu.GetFileRegular(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
begin
	Result := FDownloader.Download(RemotePath, LocalPath, ResultHash, LogErrors);
end;

{Delegates to FDownloader - kept for backward compatibility}
{since 29.07.2022: изменена логика получения ссылок, см. issue #285. URL теперь всегда должны быть кодированы, иначе в некоторых случаях приходит 400}
function TCloudMailRu.GetSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
begin
	Result := FDownloader.GetSharedFileUrl(RemotePath, ShardType);
end;

{Delegates to FDownloader - kept for backward compatibility with tests}
function TCloudMailRu.GetFileShared(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
begin
	Result := FDownloader.Download(RemotePath, LocalPath, ResultHash, LogErrors);
end;

function TCloudMailRu.GetHTTPConnection: ICloudHTTP;
begin
	if (nil = FHTTPManager) then
	begin
		if not Assigned(FHTTPConnection) then
			FHTTPConnection := TCloudMailRuHTTP.Create(FSettings.ConnectionSettings, FLogger, FProgress);

		Result := FHTTPConnection;
	end
	else
		Result := FHTTPManager.get(GetCurrentThreadID());
	Result.AuthCookie := FCookieManager;
	if (EmptyWideStr <> FAuthToken) and Assigned(Result.HTTP) then
		Result.HTTP.Request.CustomHeaders.Values['X-CSRF-Token'] := FAuthToken;
end;

function TCloudMailRu.GetPublicLink: WideString;
begin
	if FPublicLink <> '' then
		Exit(FPublicLink); {Already have a public link}

	if IsPublicAccount and (FSettings.AccountSettings.PublicUrl <> EmptyWideStr) then
	begin
		FPublicLink := FSettings.AccountSettings.PublicUrl;
		FSettings.AccountSettings.PublicUrl := IncludeSlash(FSettings.AccountSettings.PublicUrl);
		Delete(FPublicLink, 1, length(PUBLIC_ACCESS_URL));
		if (FPublicLink <> EmptyWideStr) and (FPublicLink[length(FPublicLink)] = '/') then
			Delete(FPublicLink, length(FPublicLink), 1);
	end;
	Exit(FPublicLink)
end;

function TCloudMailRu.GetPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; Publish: Boolean = CLOUD_PUBLISH): Boolean;
begin
	Result := FShareService.GetPublishedFileStreamUrl(FileIdentity, StreamUrl, ShardType, Publish);
end;

function TCloudMailRu.GetShard(var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean;
var
	JSON: WideString;
begin
	Result := HTTP.PostForm(API_DISPATCHER + '?' + FUnitedParams, '', JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_SHARD_RECEIVE);
	if Result then
	begin
		Result := JSONHelper.GetShard(JSON, Shard, ShardType) and (Shard <> EmptyWideStr);
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PREFIX_SHARD_RECEIVED, [Shard, ShardType]);
	end;
end;

function TCloudMailRu.RefreshCSRFToken: Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	HTTP.GetPage(API_CSRF, JSON, Progress);
	Result := getBodyToken(JSON, FAuthToken);
	if Result then
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, TOKEN_UPDATED)
	else
		FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_TOKEN_UPDATE)
end;

function TCloudMailRu.InitSharedConnectionParameters(): Boolean;
var
	PageContent: WideString;
	PublicShard: WideString;
	Progress: Boolean;
begin
	Progress := False;
	Result := HTTP.GetPage(FSettings.AccountSettings.PublicUrl, PageContent, Progress);
	if Result then
	begin
		if not extractPublicShard(PageContent, PublicShard) then
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_GET_PUBLIC_SHARE);
			Exit(False);
		end;
		FShardManager.SetPublicShard(PublicShard);
	end;
end;

function TCloudMailRu.GetUserSpace(var SpaceInfo: TCMRSpace): Boolean;
var
	CallResult: TAPICallResult;
	LocalSpace: TCMRSpace;
begin
	LocalSpace := default(TCMRSpace);
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Progress: Boolean;
			Success: Boolean;
		begin
			Progress := False;
			Success := HTTP.GetPage(Format('%s?%s', [API_USER_SPACE, FUnitedParams]), JSON, Progress);
			if Success then
				Success := CloudResultToBoolean(JSON, PREFIX_ERR_GET_USER_SPACE) and LocalSpace.FromJSON(JSON);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		SpaceInfo := LocalSpace;
end;

function TCloudMailRu.Login(Method: Integer): Boolean;
begin
	HTTP.SetProgressNames(LOGIN_IN_PROGRESS, EmptyWideStr);
	if IsPublicAccount then
		Exit(LoginShared());
	Result := LoginRegular(Method);
end;

{Delegates authentication to the injected IAuthStrategy.
 The strategy is responsible for obtaining auth tokens and setting up connection parameters.
 Method parameter is ignored - the injected strategy determines the auth method.}
function TCloudMailRu.LoginRegular(Method: Integer): Boolean;
var
	Credentials: TAuthCredentials;
	AuthResult: TAuthResult;
begin
	Result := False;

	FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, Format(LOGIN_TO, [Email]));

	{Build credentials from account settings}
	Credentials := TAuthCredentials.Create(Email, Password, FUser, FDomain);

	{Delegate to auth strategy}
	AuthResult := FAuthStrategy.Authenticate(Credentials, HTTP, FLogger);

	if AuthResult.Success then
	begin
		{Apply auth result to connection state}
		FAuthToken := AuthResult.AuthToken;
		FOAuthToken := AuthResult.OAuthToken;
		FUnitedParams := AuthResult.UnitedParams;
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, CONNECTED_TO, [Email]);
		LogUserSpaceInfo;
		Result := True;
	end;
end;

function TCloudMailRu.LoginShared(Method: Integer): Boolean;
begin
	FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, URL_OPEN, [FSettings.AccountSettings.PublicUrl]);
	Exit(InitSharedConnectionParameters());
end;

procedure TCloudMailRu.LogUserSpaceInfo;
var
	US: TCMRSpace;
	QuotaInfo: WideString;
begin
	if IsPublicAccount then
		Exit;
	if GetUserSpace(US) then
	begin
		if (US.overquota) then
			QuotaInfo := WARN_QUOTA_EXHAUSTED
		else
			QuotaInfo := EmptyWideStr;
		FLogger.Log(LOG_LEVEL_FILE_OPERATION, MSGTYPE_DETAILS, USER_SPACE_INFO, [FormatSize(US.total), FormatSize(US.used), FormatSize(US.total - US.used), QuotaInfo]);
	end else begin
		FLogger.Log(LOG_LEVEL_DEBUG, MSGTYPE_IMPORTANTERROR, ERR_GET_USER_SPACE, [Email]);
	end;
end;

{Delegates to FFileOps}
function TCloudMailRu.MoveFile(OldName, ToPath: WideString): Integer;
begin
	Result := FFileOps.MoveToPath(OldName, ToPath);
end;

{Delegates to FFileOps}
function TCloudMailRu.FileMove(OldName, NewName: WideString): Integer;
begin
	Result := FFileOps.Move(OldName, NewName);
end;

function TCloudMailRu.PublishFile(Path: WideString; var PublicLink: WideString; Publish: Boolean): Boolean;
var
	CallResult: TAPICallResult;
	ExtractedLink: WideString;
	CurrentLink: WideString;
begin
	Result := False;
	if IsPublicAccount then
		Exit;

	ExtractedLink := '';
	CurrentLink := PublicLink; {Capture var parameter value for anonymous method}
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			if Publish then
				Success := HTTP.PostForm(API_FILE_PUBLISH + '?' + FUnitedParams,
					Format('home=/%s&conflict', [PathToUrl(Path)]), JSON, 'application/x-www-form-urlencoded', true, False)
			else
				Success := HTTP.PostForm(API_FILE_UNPUBLISH + '?' + FUnitedParams,
					Format('weblink=%s&conflict', [CurrentLink]), JSON, 'application/x-www-form-urlencoded', true, False);

			if Success then
				Success := CloudResultToBoolean(JSON, PREFIX_ERR_FILE_PUBLISH);

			if Success and Publish then
			begin
				Success := JSONHelper.GetPublicLink(JSON, ExtractedLink);
			end;

			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result and Publish then
		PublicLink := ExtractedLink;
end;

function TCloudMailRu.GetShareInfo(Path: WideString; var InviteListing: TCMRInviteList): Boolean;
begin
	Result := FShareService.GetShareInfo(Path, InviteListing);
end;

function TCloudMailRu.ShareFolder(Path, Email: WideString; Access: Integer): Boolean;
begin
	{ShareFolder handles both sharing and unsharing based on Access value}
	if Access in [CLOUD_SHARE_RW, CLOUD_SHARE_RO] then
		Result := FShareService.Share(Path, Email, Access)
	else
		Result := FShareService.Unshare(Path, Email);
end;

function TCloudMailRu.TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString): Boolean;
begin
	Result := False;
	if IsPublicAccount then
		Exit;
	Result := FRetryOperation.PostFormBoolean(
		API_TRASHBIN_RESTORE + '?' + FUnitedParams,
		Format('path=%s&restore_revision=%d&conflict=%s', [PathToUrl(Path), RestoreRevision, ConflictMode]),
		PREFIX_ERR_FILE_RESTORE);
end;

function TCloudMailRu.TrashbinEmpty(): Boolean;
begin
	Result := False;
	if IsPublicAccount then
		Exit;
	Result := FRetryOperation.PostFormBoolean(
		API_TRASHBIN_EMPTY + '?' + FUnitedParams,
		EmptyWideStr,
		PREFIX_ERR_TRASH_CLEAN);
end;

function TCloudMailRu.MountFolder(Home, InviteToken, ConflictMode: WideString): Boolean;
begin
	Result := FShareService.Mount(Home, InviteToken, ConflictMode);
end;

function TCloudMailRu.UnmountFolder(Home: WideString; Clone_copy: Boolean): Boolean;
begin
	Result := FShareService.Unmount(Home, Clone_copy);
end;

function TCloudMailRu.RejectInvite(InviteToken: WideString): Boolean;
begin
	Result := FShareService.RejectInvite(InviteToken);
end;

{Delegates to FUploader}
function TCloudMailRu.PutFile(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer;
begin
	Result := FUploader.Upload(LocalPath, RemotePath, ConflictMode, ChunkOverwriteMode);
end;

{Delegates to FFileOps}
function TCloudMailRu.RemoveDir(Path: WideString): Boolean;
begin
	Result := FFileOps.RemoveDirectory(Path);
end;

{Delegates to FFileOps}
function TCloudMailRu.RenameFile(OldName, NewName: WideString): Integer;
begin
	Result := FFileOps.Rename(OldName, NewName);
end;

function TCloudMailRu.StatusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
var
	CallResult: TAPICallResult;
	LocalInfo: TCMRDirItem;
begin
	{Public accounts don't need token refresh}
	if IsPublicAccount then
	begin
		var JSON: WideString;
		var Progress: Boolean := False;
		Result := HTTP.GetPage(Format('%s?weblink=%s%s&%s', [API_FILE, IncludeSlash(GetPublicLink), PathToUrl(Path), FUnitedParams]), JSON, Progress);
		if Result then
			Result := CloudResultToBoolean(JSON, PREFIX_ERR_FILE_STATUS) and FileInfo.FromJSON(JSON);
		Exit;
	end;

	LocalInfo := default(TCMRDirItem);
	CallResult := FRetryOperation.Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Progress: Boolean;
			Success: Boolean;
		begin
			Progress := False;
			Success := HTTP.GetPage(Format('%s?home=%s&%s', [API_FILE, PathToUrl(Path), FUnitedParams]), JSON, Progress);
			if Success then
				Success := CloudResultToBoolean(JSON, PREFIX_ERR_FILE_STATUS) and LocalInfo.FromJSON(JSON);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);

	Result := CallResult.Success;
	if Result then
		FileInfo := LocalInfo;
end;

class function TCloudMailRu.CloudAccessToString(Access: WideString; Invert: Boolean): WideString;
begin
	if Access = 'read only' then
		Access := CLOUD_SHARE_ACCESS_READ_ONLY;
	if Access = 'read and write' then
		Access := CLOUD_SHARE_ACCESS_READ_WRITE;
	if Invert then
	begin
		if (Access = CLOUD_SHARE_ACCESS_READ_ONLY) then
			Access := CLOUD_SHARE_ACCESS_READ_WRITE
		else
			Access := CLOUD_SHARE_ACCESS_READ_ONLY;
	end;
	if Access = CLOUD_SHARE_ACCESS_READ_ONLY then
		Result := 'read only'
	else
		Result := 'read and write';
end;

class function TCloudMailRu.StringToCloudAccess(AccessString: WideString; Invert: Boolean): Integer;
begin
	if AccessString = 'read only' then
		AccessString := CLOUD_SHARE_ACCESS_READ_ONLY;
	if AccessString = 'read and write' then
		AccessString := CLOUD_SHARE_ACCESS_READ_WRITE;
	if Invert then
	begin
		if (AccessString = CLOUD_SHARE_ACCESS_READ_ONLY) then
			AccessString := CLOUD_SHARE_ACCESS_READ_WRITE
		else
			AccessString := CLOUD_SHARE_ACCESS_READ_ONLY;
	end;
	if AccessString = CLOUD_SHARE_ACCESS_READ_ONLY then
		Result := CLOUD_SHARE_RO
	else
		Result := CLOUD_SHARE_RW;
end;

class function TCloudMailRu.TempPublicCloudInit(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean;
var
	TempCloudSettings: TCloudSettings;
begin
	TempCloudSettings := default (TCloudSettings);
	TempCloudSettings.AccountSettings.PublicAccount := true;
	TempCloudSettings.AccountSettings.PublicUrl := PublicUrl;
	TempCloud := TCloudMailRu.Create(TempCloudSettings, nil, TNullAuthStrategy.Create, TWindowsFileSystem.Create, TNullLogger.Create, TNullProgress.Create, TNullRequest.Create);
	Result := TempCloud.Login;
end;

{Delegates to FHashCalculator - kept for backward compatibility with tests}
function TCloudMailRu.CloudHash(Path: WideString): WideString;
begin
	Result := FHashCalculator.CalculateHash(Path);
end;

{Delegates to FHashCalculator - kept for backward compatibility with tests}
function TCloudMailRu.CloudHash(Stream: TStream; Path: WideString = CALCULATING_HASH): WideString;
begin
	Result := FHashCalculator.CalculateHash(Stream, Path);
end;

end.
