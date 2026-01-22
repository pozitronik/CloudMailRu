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
		{SHARED WEBFOLDERS}
		function LoginShared(Method: Integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function GetPublicLink(): WideString;
	public
		property IsPublicAccount: Boolean read FSettings.AccountSettings.PublicAccount;
		{Service accessors - exposed for external use and MVP architecture}
		property Downloader: ICloudFileDownloader read FDownloader;
		property Uploader: ICloudFileUploader read FUploader;
		property ShareService: ICloudShareService read FShareService;
		property ListingService: ICloudListingService read FListingService;
		property FileOps: ICloudFileOperations read FFileOps;
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
		procedure LogUserSpaceInfo();
	end;

implementation

{TCloudMailRu}

{Delegates to FUploader}
function TCloudMailRu.AddFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = False): Integer;
begin
	Result := FUploader.AddFileByIdentity(FileIdentity, RemotePath, ConflictMode, LogErrors, LogSuccess);
end;

function TCloudMailRu.AddFileByIdentity(FileIdentity: TCMRDirItem; RemotePath, ConflictMode: WideString; LogErrors, LogSuccess: Boolean): Integer;
var
	CloudFileIdentity: TCMRFileIdentity;
begin
	CloudFileIdentity.Hash := FileIdentity.Hash;
	CloudFileIdentity.size := FileIdentity.size;
	Result := AddFileByIdentity(CloudFileIdentity, RemotePath, ConflictMode, LogErrors, LogSuccess)
end;

{Delegates to FShareService}
function TCloudMailRu.CloneWeblink(Path, Link, ConflictMode: WideString): Integer;
begin
	Result := FShareService.CloneWeblink(Path, Link, ConflictMode);
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

		{Initialize shard manager with HTTP callbacks for resolution}
		FShardManager := TCloudShardManager.Create(Logger,
			function(const URL, Data: WideString; var Answer: WideString): Boolean
			begin
				Result := Self.HTTP.PostForm(URL, Data, Answer);
			end,
			function(const JSON, ErrorPrefix: WideString): Boolean
			begin
				Result := Self.CloudResultToBoolean(JSON, ErrorPrefix);
			end,
			function: WideString
			begin
				Result := Self.FUnitedParams;
			end,
			FSettings.AccountSettings.ShardOverride, FSettings.AccountSettings.UploadUrlOverride);

		{Initialize retry operation handler with HTTP callbacks}
		FRetryOperation := TRetryOperation.Create(
			function: Boolean
			begin
				Result := RefreshCSRFToken;
			end,
			function(const URL, Params: WideString; var JSON: WideString): Boolean
			begin
				Result := HTTP.PostForm(URL, Params, JSON);
			end,
			function(const URL: WideString; var JSON: WideString; var ShowProgress: Boolean): Boolean
			begin
				Result := HTTP.GetPage(URL, JSON, ShowProgress);
			end,
			function(const JSON, ErrorPrefix: WideString): Boolean
			begin
				Result := CloudResultToBoolean(JSON, ErrorPrefix);
			end,
			function(const JSON, ErrorPrefix: WideString): Integer
			begin
				Result := CloudResultToFsResult(JSON, ErrorPrefix);
			end);

		{Use injected cipher for encryption operations}
		FCipher := Cipher;
		FDoCryptFiles := Assigned(Cipher);
		FDoCryptFilenames := FDoCryptFiles and FSettings.AccountSettings.EncryptFilenames;

		{Initialize file downloader service with callbacks for dynamic state}
		FDownloader := TCloudFileDownloader.Create(
			function: ICloudHTTP
			begin
				Result := Self.HTTP;
			end, FShardManager, FHashCalculator, FCipher, FFileSystem, FLogger, FProgress, FRequest,
			function: TCMROAuth
			begin
				Result := Self.FOAuthToken;
			end,
			function: Boolean
			begin
				Result := Self.IsPublicAccount;
			end,
			function: WideString
			begin
				Result := Self.GetPublicLink;
			end,
			function: Boolean
			begin
				Result := Self.RefreshCSRFToken;
			end, FDoCryptFiles, FDoCryptFilenames);

		{Initialize file uploader service with callbacks and settings}
		var
			UploadSettings: TUploadSettings;
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
			function: ICloudHTTP
			begin
				Result := Self.HTTP;
			end, FShardManager, FHashCalculator, FCipher, FFileSystem, FLogger, FProgress, FRequest,
			function: TCMROAuth
			begin
				Result := Self.FOAuthToken;
			end,
			function: Boolean
			begin
				Result := Self.IsPublicAccount;
			end,
			function: TRetryOperation
			begin
				Result := Self.FRetryOperation;
			end,
			function: WideString
			begin
				Result := Self.FUnitedParams;
			end,
			function(JSON: WideString; ErrorPrefix: WideString): Integer
			begin
				Result := Self.CloudResultToFsResult(JSON, ErrorPrefix);
			end,
			function(Path: WideString): Boolean
			begin
				Result := Self.DeleteFile(Path);
			end, FDoCryptFiles, FDoCryptFilenames, UploadSettings);

		{Initialize share service with callbacks for dynamic state}
		FShareService := TCloudShareService.Create(Self.HTTP, FLogger, FRetryOperation,
			function: Boolean
			begin
				Result := Self.IsPublicAccount;
			end,
			function: WideString
			begin
				Result := Self.FUnitedParams;
			end,
			function(JSON: WideString; ErrorPrefix: WideString): Boolean
			begin
				Result := Self.CloudResultToBoolean(JSON, ErrorPrefix);
			end,
			function(JSON: WideString; ErrorPrefix: WideString): Integer
			begin
				Result := Self.CloudResultToFsResult(JSON, ErrorPrefix);
			end, FShardManager);

		{Initialize listing service with callbacks for dynamic state}
		FListingService := TCloudListingService.Create(Self.HTTP, FCipher, FLogger, FRetryOperation,
			function: Boolean
			begin
				Result := Self.IsPublicAccount;
			end,
			function: WideString
			begin
				Result := Self.FUnitedParams;
			end,
			function: WideString
			begin
				Result := Self.GetPublicLink;
			end,
			function(JSON: WideString; ErrorPrefix: WideString): Boolean
			begin
				Result := Self.CloudResultToBoolean(JSON, ErrorPrefix);
			end,
			function(OperationResult: TCMROperationResult; ErrorPrefix: WideString): Boolean
			begin
				Result := Self.CloudResultToBoolean(OperationResult, ErrorPrefix);
			end, FDoCryptFilenames);

		{Initialize file operations service with callbacks for dynamic state}
		FFileOps := TCloudFileOperations.Create(Self.HTTP, FLogger, FRetryOperation,
			function: Boolean
			begin
				Result := Self.IsPublicAccount;
			end,
			function: WideString
			begin
				Result := Self.FUnitedParams;
			end);

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

{Delegates to FDownloader}
function TCloudMailRu.GetFile(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
begin
	Result := FDownloader.Download(RemotePath, LocalPath, ResultHash, LogErrors);
end;

{Delegates to FDownloader}
{since 29.07.2022: изменена логика получения ссылок, см. issue #285. URL теперь всегда должны быть кодированы, иначе в некоторых случаях приходит 400}
function TCloudMailRu.GetSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
begin
	Result := FDownloader.GetSharedFileUrl(RemotePath, ShardType);
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

{Delegates to FListingService}
function TCloudMailRu.GetUserSpace(var SpaceInfo: TCMRSpace): Boolean;
begin
	Result := FListingService.GetUserSpace(SpaceInfo);
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
		Result := true;
	end;
end;

function TCloudMailRu.LoginShared(Method: Integer): Boolean;
begin
	FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, URL_OPEN, [FSettings.AccountSettings.PublicUrl]);
	Exit(InitSharedConnectionParameters());
end;

{Delegates to FListingService}
procedure TCloudMailRu.LogUserSpaceInfo;
begin
	FListingService.LogUserSpaceInfo(Email);
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

{Delegates to FShareService}
function TCloudMailRu.PublishFile(Path: WideString; var PublicLink: WideString; Publish: Boolean): Boolean;
begin
	if Publish then
		Result := FShareService.Publish(Path, PublicLink)
	else
		Result := FShareService.Unpublish(Path, PublicLink);
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

{Delegates to FListingService}
function TCloudMailRu.TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString): Boolean;
begin
	Result := FListingService.TrashbinRestore(Path, RestoreRevision, ConflictMode);
end;

{Delegates to FListingService}
function TCloudMailRu.TrashbinEmpty(): Boolean;
begin
	Result := FListingService.TrashbinEmpty();
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

{Delegates to FListingService}
function TCloudMailRu.StatusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
begin
	Result := FListingService.StatusFile(Path, FileInfo);
end;

{Delegates to FHashCalculator - protected for testability}
function TCloudMailRu.CloudHash(Path: WideString): WideString;
begin
	Result := FHashCalculator.CalculateHash(Path);
end;

{Delegates to FHashCalculator - protected for testability}
function TCloudMailRu.CloudHash(Stream: TStream; Path: WideString = CALCULATING_HASH): WideString;
begin
	Result := FHashCalculator.CalculateHash(Stream, Path);
end;

end.
