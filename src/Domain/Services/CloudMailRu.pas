unit CloudMailRu;

interface

uses
	DebugHelper,
	CloudDirItemList,
	CloudDirItem,
	CloudOAuth,
	CloudSpace,
	CloudOperationResult,
	JSONHelper,
	ParsingHelper,
	CloudConstants,
	CloudHTTP,
	LanguageStrings,
	System.Classes,
	System.SysUtils,
	SettingsConstants,
	WFXTypes,
	PathHelper,
	StringHelper,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	FileCipher,
	CloudSettings,
	HTTPManager,
	WindowsFileSystem,
	IdCookieManager,
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
	OpenSSLProvider,
	CloudCallbackTypes,
	AccountCredentialsProvider,
	CloudContext;

type
	{TCloudMailRu implements ICloudContext but disables automatic reference counting.
		This allows the class to be used both as a direct object reference (by tests and external code)
		and as ICloudContext provider for internal services without causing premature destruction.}
	TCloudMailRu = class(TInterfacedObject, ICloudContext)
	protected
		function _AddRef: Integer; stdcall;
		function _Release: Integer; stdcall;
	private
		FSettings: TCloudSettings; {Current options set for the cloud instance}
		FAuthorizationState: TAuthorizationState; {Authorization state machine}
		FAuthorizationError: TAuthorizationError; {Last authorization error}

		FHTTPManager: IHTTPManager; {HTTP connection manager - required, provides connections per thread}
		FGetThreadID: TGetThreadIDFunc; {Thread identity callback - injectable for testability}

		FCookieManager: TIdCookieManager; {The auth cookie, should be stored separately, because it associated with a cloud instance, not a connection}

		FLogger: ILogger;
		FProgress: IProgress;
		FRequest: IRequest;
		FTCHandler: ITCHandler;

		FCipher: ICipher; {The encryption instance}
		FAuthStrategy: IAuthStrategy; {Authentication strategy}
		FFileSystem: IFileSystem; {File system abstraction for testability}
		FAccountCredentialsProvider: IAccountCredentialsProvider; {Account password retrieval service}
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
		function GetHTTP: ICloudHTTP;
		function RefreshCSRFToken: Boolean;
	protected
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
		{Protected for testability - allows tests to set authorization state}
		procedure SetAuthorizationState(State: TAuthorizationState);
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
		property HTTP: ICloudHTTP read GetHTTP;

		{REGULAR CLOUD}
		function LoginRegular: Boolean;
		{SHARED WEBFOLDERS}
		function LoginShared: Boolean;
		function GetPublicLink(): WideString;
		procedure InitPublicLink;
	public
		{Authorization state machine}
		function Authorize: Boolean;
		procedure InvalidateAuthorization;
		property AuthorizationState: TAuthorizationState read FAuthorizationState;
		property AuthorizationError: TAuthorizationError read FAuthorizationError;

		{Service accessors}
		property Downloader: ICloudFileDownloader read FDownloader;
		property Uploader: ICloudFileUploader read FUploader;
		property ShareService: ICloudShareService read FShareService;
		property ListingService: ICloudListingService read FListingService;
		property FileOperations: ICloudFileOperations read FFileOperations;

		{ICloudContext implementation - provides access to cloud state for services}
		function IsPublicAccount: Boolean;
		function GetOAuthToken: TCloudOAuth;
		function GetUnitedParams: WideString;
		function DeleteFile(const Path: WideString): Boolean;
		function CloudResultToFsResult(const JSON, ErrorPrefix: WideString): Integer; overload;
		function CloudResultToFsResult(const OperationResult: TCloudOperationResult; const ErrorPrefix: WideString): Integer; overload;
		function CloudResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean; overload;
		function CloudResultToBoolean(const OperationResult: TCloudOperationResult; const ErrorPrefix: WideString): Boolean; overload;
		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(CloudSettings: TCloudSettings; ConnectionManager: IHTTPManager; GetThreadID: TGetThreadIDFunc; AuthStrategy: IAuthStrategy; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; TCHandler: ITCHandler; Cipher: ICipher; OpenSSLProvider: IOpenSSLProvider; AccountCredentialsProvider: IAccountCredentialsProvider);
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

{Disable automatic reference counting - TCloudMailRu lifetime is managed externally}
function TCloudMailRu._AddRef: Integer;
begin
	Result := -1;
end;

function TCloudMailRu._Release: Integer;
begin
	Result := -1;
end;

{ICloudContext implementation - delegates to TCloudErrorMapper}
function TCloudMailRu.CloudResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean;
begin
	Result := TCloudErrorMapper.ToBoolean(JSON, FLogger, ErrorPrefix);
end;

function TCloudMailRu.CloudResultToBoolean(const OperationResult: TCloudOperationResult; const ErrorPrefix: WideString): Boolean;
begin
	Result := TCloudErrorMapper.ToBoolean(OperationResult, FLogger, ErrorPrefix);
end;

function TCloudMailRu.CloudResultToFsResult(const JSON, ErrorPrefix: WideString): Integer;
begin
	Result := TCloudErrorMapper.ToFsResult(JSON, FLogger, ErrorPrefix);
end;

function TCloudMailRu.CloudResultToFsResult(const OperationResult: TCloudOperationResult; const ErrorPrefix: WideString): Integer;
begin
	Result := TCloudErrorMapper.ToFsResult(OperationResult, FLogger, ErrorPrefix);
end;

function TCloudMailRu.DeleteFile(const Path: WideString): Boolean;
begin
	Result := FFileOperations.Delete(Path);
end;

function TCloudMailRu.GetOAuthToken: TCloudOAuth;
begin
	Result := FOAuthToken;
end;

function TCloudMailRu.GetUnitedParams: WideString;
begin
	Result := FUnitedParams;
end;

function TCloudMailRu.IsPublicAccount: Boolean;
begin
	Result := FSettings.AccountSettings.PublicAccount;
end;

constructor TCloudMailRu.Create(CloudSettings: TCloudSettings; ConnectionManager: IHTTPManager; GetThreadID: TGetThreadIDFunc; AuthStrategy: IAuthStrategy; FileSystem: IFileSystem; Logger: ILogger; Progress: IProgress; Request: IRequest; TCHandler: ITCHandler; Cipher: ICipher; OpenSSLProvider: IOpenSSLProvider; AccountCredentialsProvider: IAccountCredentialsProvider);
var
	GetUnitedParamsCallback: TGetStringFunc;
begin
	try
		FAuthorizationState := asPending;
		FAuthorizationError := TAuthorizationError.Empty;
		FSettings := CloudSettings;

		FHTTPManager := ConnectionManager;
		FGetThreadID := GetThreadID;
		FAuthStrategy := AuthStrategy;
		FFileSystem := FileSystem;
		FAccountCredentialsProvider := AccountCredentialsProvider;

		FProgress := Progress;
		FLogger := Logger;
		FRequest := Request;
		FTCHandler := TCHandler;

		FCookieManager := TIdCookieManager.Create();

		{Reusable callbacks - avoid duplicating anonymous functions}
		GetUnitedParamsCallback := function: WideString
			begin
				Result := Self.FUnitedParams;
			end;

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
			end, GetUnitedParamsCallback, FSettings.AccountSettings.ShardOverride, FSettings.AccountSettings.UploadUrlOverride);

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

		{Initialize file downloader service}
		FDownloader := TCloudFileDownloader.Create(Self, FShardManager, FHashCalculator, FCipher, FFileSystem, FLogger, FProgress, FRequest, FDoCryptFiles, FDoCryptFilenames);

		{Initialize file operations service - must be before FUploader which uses it}
		FFileOperations := TCloudFileOperations.Create(Self, FLogger, FRetryOperation);

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

		FUploader := TCloudFileUploader.Create(Self, FShardManager, FHashCalculator, FCipher, FFileSystem, FLogger, FProgress, FRequest, FTCHandler, FRetryOperation, FDoCryptFiles, FDoCryptFilenames, UploadSettings);

		{Initialize share service}
		FShareService := TCloudShareService.Create(Self, FLogger, FRetryOperation, FShardManager);

		{Initialize listing service}
		FListingService := TCloudListingService.Create(Self, FCipher, FLogger, FRetryOperation, FDoCryptFilenames);

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

function TCloudMailRu.GetHTTP: ICloudHTTP;
begin
	Result := FHTTPManager.Get(FGetThreadID());
	Result.AuthCookie := FCookieManager;
	if EmptyWideStr <> FAuthToken then
		Result.SetCSRFToken(FAuthToken);
end;

{Simple getter - FPublicLink is initialized in InitPublicLink during LoginShared}
function TCloudMailRu.GetPublicLink: WideString;
begin
	Result := FPublicLink;
end;

{Extracts link identifier from public URL (removes PUBLIC_ACCESS_URL prefix and trailing slash)}
procedure TCloudMailRu.InitPublicLink;
begin
	if FSettings.AccountSettings.PublicUrl = EmptyWideStr then
		Exit;

	FPublicLink := FSettings.AccountSettings.PublicUrl;
	Delete(FPublicLink, 1, length(PUBLIC_ACCESS_URL));
	if (FPublicLink <> EmptyWideStr) and (FPublicLink[length(FPublicLink)] = '/') then
		Delete(FPublicLink, length(FPublicLink), 1);
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
		InitPublicLink;
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
	Credentials := TAuthCredentials.Create(Email, Password, FSettings.AccountSettings.User, FSettings.AccountSettings.Domain);

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

{Authorization state machine}

function TCloudMailRu.Authorize: Boolean;
begin
	if FAuthorizationState = asAuthorized then
		Exit(true);

	FAuthorizationState := asAuthorizing;
	FAuthorizationError := TAuthorizationError.Empty;

	{For non-public accounts, retrieve password via provider before login}
	if not IsPublicAccount then
	begin
		if not FAccountCredentialsProvider.GetPassword(FSettings.AccountSettings.Account, FSettings.AccountSettings) then
		begin
			FAuthorizationState := asFailed;
			FAuthorizationError := TAuthorizationError.Create(aecAuthFailed, ERR_PASSWORD_CANCELLED);
			Exit(false);
		end;
	end;

	Result := Login;

	if Result then
		FAuthorizationState := asAuthorized
	else
	begin
		FAuthorizationState := asFailed;
		FAuthorizationError := TAuthorizationError.Create(aecAuthFailed, ERR_AUTH_FAILURE);
	end;
end;

procedure TCloudMailRu.SetAuthorizationState(State: TAuthorizationState);
begin
	FAuthorizationState := State;
end;

procedure TCloudMailRu.InvalidateAuthorization;
begin
	FAuthorizationState := asPending;
	FAuthorizationError := TAuthorizationError.Empty;
end;

end.
