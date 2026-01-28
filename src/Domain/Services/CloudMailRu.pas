unit CloudMailRu;

interface

uses
	DebugHelper,
	CloudDirItemList,
	CloudDirItem,
	CloudInviteList,
	CloudIncomingInviteList,
	CloudOAuth,
	CloudSpace,
	CloudFileIdentity,
	CloudOperationResult,
	CloudTwostep,
	JSONHelper,
	ParsingHelper,
	CloudConstants,
	CloudHTTP,
	LanguageStrings,
	HashInfo,
	System.Hash,
	System.Classes,
	System.Generics.Collections,
	System.SysUtils,
	SettingsConstants,
	WFXTypes,
	Winapi.Windows,
	PathHelper,
	FileHelper,
	StringHelper,
	SystemHelper,
	WindowsHelper,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	FileCipher,
	RealPath,
	CloudSettings,
	FileSplitInfo,
	ChunkedFileStream,
	HTTPManager,
	WindowsFileSystem,
	IdCookieManager,
	DCPbase64,
	AuthStrategy,
	TokenRetryHelper,
	CloudHashCalculator,
	CloudShardManager,
	CloudErrorMapper,
	CloudFileDownloader,
	CloudFileUploader,
	CloudShareService,
	CloudListingService,
	CloudFileOperations,
	CloudAuthorizationState,
	OpenSSLProvider;

type
	TCloudMailRu = class(TInterfacedObject)
	private
		FSettings: TCloudSettings; {Current options set for the cloud instance}
		FAuthorizationState: TAuthorizationState; {Authorization state machine}
		FAuthorizationError: TAuthorizationError; {Last authorization error}

		FHTTPManager: IHTTPManager; {HTTP connection manager - required, provides connections per thread}

		FCookieManager: TIdCookieManager; {The auth cookie, should be stored separately, because it associated with a cloud instance, not a connection}

		FLogger: ILogger;
		FProgress: IProgress;
		FRequest: IRequest;
		FTCHandler: ITCHandler;

		FCipher: ICipher; {The encryption instance}
		FAuthStrategy: IAuthStrategy; {Authentication strategy}
		FFileSystem: IFileSystem; {File system abstraction for testability}
		FRetryOperation: IRetryOperation; {Token refresh retry handler}
		FHashCalculator: ICloudHashCalculator; {Cloud hash calculation service}
		FDownloader: ICloudFileDownloader; {File download service}
		FUploader: ICloudFileUploader; {File upload service}
		FShareService: ICloudShareService; {Share and publish service}
		FListingService: ICloudListingService; {Directory listing service}
		FFileOperations: ICloudFileOperations; {File operations service}

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
		FOAuthToken: TCloudOAuth; {OAuth token data}
		{HTTP REQUESTS WRAPPERS - protected for testability}
		function GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
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
		function LoginRegular: Boolean;
		{SHARED WEBFOLDERS}
		function LoginShared: Boolean;
		function GetPublicLink(): WideString;
	public
		{Authorization state machine}
		function Authorize: Boolean;
		procedure InvalidateAuthorization;
		property AuthorizationState: TAuthorizationState read FAuthorizationState;
		property AuthorizationError: TAuthorizationError read FAuthorizationError;

		{Service accessors}
		property IsPublicAccount: Boolean read FSettings.AccountSettings.PublicAccount;
		property Downloader: ICloudFileDownloader read FDownloader;
		property Uploader: ICloudFileUploader read FUploader;
		property ShareService: ICloudShareService read FShareService;
		property ListingService: ICloudListingService read FListingService;
		property FileOperations: ICloudFileOperations read FFileOperations;
		{ERROR RESULT MAPPING - exposed for testing and external use}
		function CloudResultToFsResult(CloudResult: TCloudOperationResult; ErrorPrefix: WideString = ''): Integer; overload;
		function CloudResultToFsResult(JSON: WideString; ErrorPrefix: WideString = ''): Integer; overload;
		function CloudResultToBoolean(CloudResult: TCloudOperationResult; ErrorPrefix: WideString = ''): Boolean; overload;
		function CloudResultToBoolean(JSON: WideString; ErrorPrefix: WideString = ''): Boolean; overload;
		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(CloudSettings: TCloudSettings; ConnectionManager: IHTTPManager; AuthStrategy: IAuthStrategy; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; TCHandler: ITCHandler; Cipher: ICipher; OpenSSLProvider: IOpenSSLProvider);
		destructor Destroy; override;
		{CLOUD INTERFACE METHODS}
		function Login: Boolean;
		{Methods with logic - kept as they add value beyond simple delegation}
		function PublishFile(Path: WideString; var PublicLink: WideString; Publish: Boolean = CLOUD_PUBLISH): Boolean;
		function ShareFolder(Path, Email: WideString; Access: Integer): Boolean;
		{OTHER ROUTINES}
		procedure LogUserSpaceInfo();
	end;

implementation

{TCloudMailRu}

{Delegates to TCloudErrorMapper - used in constructor callbacks}
function TCloudMailRu.CloudResultToBoolean(JSON, ErrorPrefix: WideString): Boolean;
begin
	Result := TCloudErrorMapper.ToBoolean(JSON, FLogger, ErrorPrefix);
end;

{Delegates to TCloudErrorMapper - kept for backward compatibility}
function TCloudMailRu.CloudResultToBoolean(CloudResult: TCloudOperationResult; ErrorPrefix: WideString): Boolean;
begin
	Result := TCloudErrorMapper.ToBoolean(CloudResult, FLogger, ErrorPrefix);
end;

{Delegates to TCloudErrorMapper - kept for backward compatibility}
function TCloudMailRu.CloudResultToFsResult(JSON, ErrorPrefix: WideString): Integer;
begin
	Result := TCloudErrorMapper.ToFsResult(JSON, FLogger, ErrorPrefix);
end;

{Delegates to TCloudErrorMapper - used in constructor callbacks}
function TCloudMailRu.CloudResultToFsResult(CloudResult: TCloudOperationResult; ErrorPrefix: WideString): Integer;
begin
	Result := TCloudErrorMapper.ToFsResult(CloudResult, FLogger, ErrorPrefix);
end;

constructor TCloudMailRu.Create(CloudSettings: TCloudSettings; ConnectionManager: IHTTPManager; AuthStrategy: IAuthStrategy; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; TCHandler: ITCHandler; Cipher: ICipher; OpenSSLProvider: IOpenSSLProvider);
begin
	try
		FAuthorizationState := asPending;
		FAuthorizationError := TAuthorizationError.Empty;
		FSettings := CloudSettings;
		ExtractEmailParts(Email, FUser, FDomain);

		FHTTPManager := ConnectionManager;
		FAuthStrategy := AuthStrategy;
		FFileSystem := FileSystem;

		FProgress := Progress;
		FLogger := Logger;
		FRequest := Request;
		FTCHandler := TCHandler;

		FCookieManager := TIdCookieManager.Create();

		{Initialize hash calculator service using strategy from settings and centralized OpenSSL provider}
		FHashCalculator := CreateHashCalculator(CloudSettings.HashCalculatorStrategy, Progress, FileSystem, OpenSSLProvider);

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
			end, FSettings.AccountSettings.ShardOverride, FSettings.AccountSettings.UploadUrlOverride);

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

		{Use injected cipher for encryption operations (NullCipher when encryption disabled)}
		FCipher := Cipher;
		FDoCryptFiles := FSettings.AccountSettings.EncryptFilesMode <> EncryptModeNone;
		FDoCryptFilenames := FDoCryptFiles and FSettings.AccountSettings.EncryptFilenames;

		{Initialize file downloader service with callbacks for dynamic state}
		FDownloader := TCloudFileDownloader.Create(
			function: ICloudHTTP
			begin
				Result := Self.HTTP;
			end, FShardManager, FHashCalculator, FCipher, FFileSystem, FLogger, FProgress, FRequest,
			function: TCloudOAuth
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

		{Initialize file operations service - must be before FUploader which uses it}
		FFileOperations := TCloudFileOperations.Create(Self.HTTP, FLogger, FRetryOperation,
			function: Boolean
			begin
				Result := Self.IsPublicAccount;
			end,
			function: WideString
			begin
				Result := Self.FUnitedParams;
			end);

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
			end, FShardManager, FHashCalculator, FCipher, FFileSystem, FLogger, FProgress, FRequest, FTCHandler,
			function: TCloudOAuth
			begin
				Result := Self.FOAuthToken;
			end,
			function: Boolean
			begin
				Result := Self.IsPublicAccount;
			end,
			function: IRetryOperation
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
				Result := Self.FFileOperations.Delete(Path);
			end,
			function(var SpaceInfo: TCloudSpace): Boolean
			begin
				Result := Self.GetUserSpace(SpaceInfo);
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
			function(OperationResult: TCloudOperationResult; ErrorPrefix: WideString): Boolean
			begin
				Result := Self.CloudResultToBoolean(OperationResult, ErrorPrefix);
			end, FDoCryptFilenames);

	except
		on E: Exception do
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_ERR_CLOUD_INIT, E.Message]);
			FAuthorizationState := asFailed;
			FAuthorizationError := TAuthorizationError.Create(aecInitFailed, E.Message);
		end;
	end;
end;

destructor TCloudMailRu.Destroy;
begin
	FCookieManager.Free;

	FRetryOperation := nil; {Release interface reference}
	FCipher := nil; {Release interface reference}
	FHashCalculator := nil; {Release interface reference}
	FShardManager := nil; {Release interface reference}
	FDownloader := nil; {Release interface reference}
	FUploader := nil; {Release interface reference}
	FShareService := nil; {Release interface reference}
	FListingService := nil; {Release interface reference}
	FFileOperations := nil; {Release interface reference}

	inherited;
end;

function TCloudMailRu.GetHTTPConnection: ICloudHTTP;
begin
	Result := FHTTPManager.Get(GetCurrentThreadID());
	Result.AuthCookie := FCookieManager;
	if EmptyWideStr <> FAuthToken then
		Result.SetCSRFToken(FAuthToken);
end;

{Extracts link identifier from public URL (removes PUBLIC_ACCESS_URL prefix and trailing slash)}
function TCloudMailRu.GetPublicLink: WideString;
begin
	if FPublicLink <> '' then
		Exit(FPublicLink); {Already have a public link}

	if IsPublicAccount and (FSettings.AccountSettings.PublicUrl <> EmptyWideStr) then
	begin
		FPublicLink := FSettings.AccountSettings.PublicUrl;
		Delete(FPublicLink, 1, length(PUBLIC_ACCESS_URL));
		if (FPublicLink <> EmptyWideStr) and (FPublicLink[length(FPublicLink)] = '/') then
			Delete(FPublicLink, length(FPublicLink), 1);
	end;
	Exit(FPublicLink)
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
function TCloudMailRu.GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
begin
	Result := FListingService.GetUserSpace(SpaceInfo);
end;

function TCloudMailRu.Login: Boolean;
begin
	HTTP.SetProgressNames(LOGIN_IN_PROGRESS, EmptyWideStr);
	if IsPublicAccount then
		Exit(LoginShared);
	Result := LoginRegular;
end;

{Delegates authentication to the injected IAuthStrategy.
	The strategy is responsible for obtaining auth tokens and setting up connection parameters.}
function TCloudMailRu.LoginRegular: Boolean;
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

function TCloudMailRu.LoginShared: Boolean;
begin
	FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, URL_OPEN, [FSettings.AccountSettings.PublicUrl]);
	Exit(InitSharedConnectionParameters());
end;

{Delegates to FListingService}
procedure TCloudMailRu.LogUserSpaceInfo;
begin
	FListingService.LogUserSpaceInfo(Email);
end;

{Has branching logic - kept as it adds value beyond simple delegation}
function TCloudMailRu.PublishFile(Path: WideString; var PublicLink: WideString; Publish: Boolean): Boolean;
begin
	if Publish then
		Result := FShareService.Publish(Path, PublicLink)
	else
		Result := FShareService.Unpublish(Path, PublicLink);
end;

{Has branching logic - kept as it adds value beyond simple delegation}
function TCloudMailRu.ShareFolder(Path, Email: WideString; Access: Integer): Boolean;
begin
	{ShareFolder handles both sharing and unsharing based on Access value}
	if Access in [CLOUD_SHARE_RW, CLOUD_SHARE_RO] then
		Result := FShareService.Share(Path, Email, Access)
	else
		Result := FShareService.Unshare(Path, Email);
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

{Authorization state machine}

function TCloudMailRu.Authorize: Boolean;
begin
	if FAuthorizationState = asAuthorized then
		Exit(true);

	FAuthorizationState := asAuthorizing;
	FAuthorizationError := TAuthorizationError.Empty;

	Result := Login;

	if Result then
		FAuthorizationState := asAuthorized
	else
	begin
		FAuthorizationState := asFailed;
		FAuthorizationError := TAuthorizationError.Create(aecAuthFailed, ERR_AUTH_FAILURE);
	end;
end;

procedure TCloudMailRu.InvalidateAuthorization;
begin
	FAuthorizationState := asPending;
	FAuthorizationError := TAuthorizationError.Empty;
end;

end.
