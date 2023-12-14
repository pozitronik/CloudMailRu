unit CloudMailRu;

interface

uses
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
	CloudMailRuHTTP,
	LANGUAGE_STRINGS,
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
	RealPath,
	CloudSettings,
	FileCipher,
	FileSplitInfo,
	ChunkedFileStream,
	HTTPManager,
	IdCookieManager,
	DCPbase64,
	AskPassword;

type
	TCloudMailRu = class
	private
		Settings: TCloudSettings; {Current options set for the cloud instance}

		HTTPConnectionsManager: THTTPManager; {Internal connections manager (can be set externally or omitted)}
		InternalHTTPConnection: TCloudMailRuHTTP; {Normally managed by HTTPConnectionsManager. If HTTPConnectionsManager is omitted, Cloud will create its own atomic connection}

		AuthCookie: TIdCookieManager; {The auth cookie, should be stored separately, because it associated with a cloud instance, not a connection}

		Logger: TTCLogger;
		Progress: TTCProgress;
		Request: TTCRequest;

		FileCipher: TFileCipher; {The encryption instance}

		FPublicLink: WideString; {Holder for GetPublicLink() value, should not be accessed directly}
		FPublicShard: WideString; {Public shard url, used for public downloads}
		FDownloadShard: WideString; {Holder of the current instance download shard adress, retrieved on the first download attempt}
		FUploadShard: WideString; {Holder of the current instance upload shard adress, retrieved on the first upload attempt}

		FAuthToken: WideString; {The current (constantly refreshing) connection token}
		FOAuthToken: TCMROAuth; {Unused at this moment}

		FUnitedParams: WideString; {The set of required authentification attributes united to the string — just for a handy usage}

		{HTTP REQUESTS WRAPPERS}
		function InitConnectionParameters(): Boolean;
		function initSharedConnectionParameters(): Boolean;
		function GetOAuthToken(var OAuthToken: TCMROAuth): Boolean;
		function GetShard(var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean;
		function GetUserSpace(var SpaceInfo: TCMRSpace): Boolean;
		function PutFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCMRFileIdentity): Integer; overload; //отправка на сервер данных из потока
		{PRIVATE UPLOAD METHODS CHAIN (CALLED FROM putFile())}
		function PutFileWhole(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): Integer; //Загрузка файла целиком
		function PutFileSplit(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer; //Загрузка файла по частям
		function PutFileStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): Integer;

		{OTHER ROUTINES}
		function CloudResultToFsResult(CloudResult: TCMROperationResult; ErrorPrefix: WideString = ''): Integer; overload;
		function CloudResultToFsResult(JSON: WideString; ErrorPrefix: WideString = ''): Integer; overload;
		function CloudResultToBoolean(CloudResult: TCMROperationResult; ErrorPrefix: WideString = ''): Boolean; overload;
		function CloudResultToBoolean(JSON: WideString; ErrorPrefix: WideString = ''): Boolean; overload;
		function CloudHash(Path: WideString): WideString; overload; //get cloud hash for specified file
		function CloudHash(Stream: TStream; Path: WideString = CALCULATING_HASH): WideString; overload; //get cloud hash for data in stream
		function GetHTTPConnection: TCloudMailRuHTTP;
		function RefreshCSRFToken: Boolean;
	protected
		FUser: WideString;
		FDomain: WideString;
		FDoCryptFiles: Boolean;
		FDoCryptFilenames: Boolean;
		property FPassword: WideString read Settings.AccountSettings.Password;
		property FEmail: WideString read Settings.AccountSettings.email;
		{REGULAR CLOUD}
		function LoginRegular(Method: Integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function GetFileRegular(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = true): Integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		{SHARED WEBFOLDERS}
		function LoginShared(Method: Integer = CLOUD_AUTH_METHOD_WEB): Boolean;

		function GetFileShared(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = true): Integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)

		function GetPublicLink(): WideString;
	public
		property Cookie: TIdCookieManager read AuthCookie;
		property IsPublicAccount: Boolean read Settings.AccountSettings.PublicAccount;

		property shard_override: WideString read Settings.AccountSettings.ShardOverride;
		property upload_url_override: WideString read Settings.AccountSettings.UploadUrlOverride;
		property unlimited_filesize: Boolean read Settings.AccountSettings.UnlimitedFilesize;
		property split_large_files: Boolean read Settings.AccountSettings.SplitLargeFiles;

		property HTTP: TCloudMailRuHTTP read GetHTTPConnection;

		property CloudMaxFileSize: int64 read Settings.CloudMaxFileSize;
		property PrecalculateHash: Boolean read Settings.PrecalculateHash;
		property ForcePrecalculateSize: int64 read Settings.ForcePrecalculateSize;
		property CheckCRC: Boolean read Settings.CheckCRC;

		property OperationErrorMode: Integer read Settings.OperationErrorMode;
		property RetryAttempts: Integer read Settings.RetryAttempts;
		property AttemptWait: Integer read Settings.AttemptWait;

		function getSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;

		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(CloudSettings: TCloudSettings; ConnectionManager: THTTPManager; Progress: TTCProgress = nil; Logger: TTCLogger = nil; Request: TTCRequest = nil);
		destructor Destroy; override;
		{CLOUD INTERFACE METHODS}
		function login(Method: Integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function getDirListing(Path: WideString; var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function getSharedLinksListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function getIncomingLinksListing(var IncomingListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean; overload;
		function getIncomingLinksListing(var IncomingListing: TCMRDirItemList; var InvitesListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean; overload;
		function getTrashbinListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function createDir(Path: WideString): Boolean;
		function removeDir(Path: WideString): Boolean;
		function statusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
		function getFile(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = true): Integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		function putFile(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer;
		function renameFile(OldName, NewName: WideString): Integer; //смена имени без перемещения
		function moveFile(OldName, ToPath: WideString): Integer; //перемещение по дереву каталогов
		function copyFile(OldName, ToPath: WideString): Integer; //Копирование файла внутри одного аккаунта
		function mvFile(OldName, NewName: WideString): Integer; //объединяющая функция, определяет делать rename или move
		function cpFile(OldName, NewName: WideString): Integer; //Копирует файл, и переименует, если нужно
		function deleteFile(Path: WideString): Boolean;
		function publishFile(Path: WideString; var PublicLink: WideString; publish: Boolean = CLOUD_PUBLISH): Boolean;
		function cloneWeblink(Path, link: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Integer; //клонировать публичную ссылку в текущий каталог

		function addFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = False): Integer; overload;
		function addFileByIdentity(FileIdentity: TCMRDirItem; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = False): Integer; overload;

		function getShareInfo(Path: WideString; var InviteListing: TCMRInviteList): Boolean;
		function shareFolder(Path, email: WideString; access: Integer): Boolean;
		function trashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function trashbinEmpty(): Boolean;
		function mountFolder(home, invite_token: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function unmountFolder(home: WideString; clone_copy: Boolean): Boolean;
		function rejectInvite(invite_token: WideString): Boolean;
		function getPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; publish: Boolean = CLOUD_PUBLISH): Boolean;
		{OTHER ROUTINES}
		function getDescriptionFile(RemotePath, localCopy: WideString): Boolean; //Если в каталоге remotePath есть descript.ion - скопировать его в файл localcopy
		function putDesriptionFile(RemotePath, localCopy: WideString): Boolean; //Скопировать descript.ion из временного файла на сервер
		procedure logUserSpaceInfo();
		function FileIdentity(LocalPath: WideString): TCMRFileIdentity;
		{STATIC ROUTINES}
		class function CloudAccessToString(access: WideString; Invert: Boolean = False): WideString; static;
		class function StringToCloudAccess(accessString: WideString; Invert: Boolean = False): Integer; static;
		class function ErrorCodeText(ErrorCode: Integer): WideString; static;
		class function IsSameIdentity(IdentityOne, IdentityTwo: TCMRFileIdentity): Boolean; static;
		class function TempPublicCloudInit(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean; static;
	end;

implementation

{TCloudMailRu}
function TCloudMailRu.addFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = False): Integer;
var
	FileName: WideString;
	JSON: WideString;
	OperationResult: TCMROperationResult;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);

	if self.FDoCryptFilenames then
	begin
		FileName := ExtractUniversalFileName(RemotePath);
		FileName := FileCipher.CryptFileName(FileName);
		RemotePath := ChangePathFileName(RemotePath, FileName);
	end;
	{Экспериментально выяснено, что параметры api, build, email, x-email, x-page-id в запросе не обязательны}
	if self.HTTP.PostForm(API_FILE_ADD, Format('api=2&conflict=%s&home=/%s&hash=%s&size=%d%s', [ConflictMode, PathToUrl(RemotePath), FileIdentity.Hash, FileIdentity.size, self.FUnitedParams]), JSON, 'application/x-www-form-urlencoded', LogErrors, False) then {Do not allow to cancel operation here}
	begin
		OperationResult.FromJSON(JSON);
		Result := CloudResultToFsResult(OperationResult, PREFIX_ERR_FILE_UPLOADING);
		if (CLOUD_OPERATION_OK = OperationResult.OperationResult) and LogSuccess then
			Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, FILE_FOUND_BY_HASH, [RemotePath]);
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := self.addFileByIdentity(FileIdentity, RemotePath, ConflictMode, LogErrors, LogSuccess);
	end;
end;

function TCloudMailRu.addFileByIdentity(FileIdentity: TCMRDirItem; RemotePath, ConflictMode: WideString; LogErrors, LogSuccess: Boolean): Integer;
var
	CloudFileIdentity: TCMRFileIdentity;
begin
	CloudFileIdentity.Hash := FileIdentity.Hash;
	CloudFileIdentity.size := FileIdentity.size;
	Result := self.addFileByIdentity(CloudFileIdentity, RemotePath, ConflictMode, LogErrors, LogSuccess)
end;

function TCloudMailRu.cloneWeblink(Path, link, ConflictMode: WideString): Integer;
var
	JSON: WideString;
	Progress: Boolean;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);
	Progress := true;
	if self.HTTP.GetPage(Format('%s?folder=/%s&weblink=%s&conflict=%s%s', [API_CLONE, PathToUrl(Path), link, ConflictMode, self.FUnitedParams]), JSON, Progress) then
	begin //Парсим ответ
		Result := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_PUBLISH);
		if (Result <> FS_FILE_OK) and not(Progress) then
			Result := FS_FILE_USERABORT; //user cancelled
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := cloneWeblink(Path, link, ConflictMode);
	end;
end;

function TCloudMailRu.CloudResultToBoolean(JSON, ErrorPrefix: WideString): Boolean;
begin
	Result := CloudResultToBoolean(TCMROperationResult.GetOperationResult(JSON), ErrorPrefix)
end;

function TCloudMailRu.CloudResultToBoolean(CloudResult: TCMROperationResult; ErrorPrefix: WideString): Boolean;
begin
	Result := CloudResult.ToBoolean;
	if not(Result) and (ErrorPrefix <> EmptyWideStr) then
		Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s%s%s', [ErrorPrefix, self.ErrorCodeText(CloudResult.OperationResult), PREFIX_STATUS, CloudResult.OperationStatus]);
end;

function TCloudMailRu.CloudResultToFsResult(JSON, ErrorPrefix: WideString): Integer;
begin
	Result := CloudResultToFsResult(TCMROperationResult.GetOperationResult(JSON), ErrorPrefix);
end;

function TCloudMailRu.CloudResultToFsResult(CloudResult: TCMROperationResult; ErrorPrefix: WideString): Integer;
begin
	case CloudResult.OperationResult of
		CLOUD_OPERATION_OK:
			Exit(FS_FILE_OK);
		CLOUD_ERROR_EXISTS:
			Exit(FS_FILE_EXISTS);
		CLOUD_ERROR_REQUIRED, CLOUD_ERROR_INVALID, CLOUD_ERROR_READONLY, CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
			Exit(FS_FILE_WRITEERROR);
		CLOUD_ERROR_UNKNOWN:
			Exit(FS_FILE_NOTSUPPORTED);
		CLOUD_ERROR_OVERQUOTA:
			begin
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_INSUFFICIENT_STORAGE);
				Exit(FS_FILE_WRITEERROR);
			end;
		CLOUD_ERROR_NAME_TOO_LONG:
			begin
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_NAME_TOO_LONG);
				Exit(FS_FILE_WRITEERROR);
			end;
		else
			begin //что-то неизвестное
				if (ErrorPrefix <> EmptyWideStr) then
					Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s%s%d', [ErrorPrefix, self.ErrorCodeText(CloudResult.OperationResult), PREFIX_STATUS, CloudResult.OperationStatus]);
				Exit(FS_FILE_WRITEERROR);
			end;
	end;
end;

class function TCloudMailRu.IsSameIdentity(IdentityOne, IdentityTwo: TCMRFileIdentity): Boolean;
begin
	Result := (IdentityOne.size = IdentityTwo.size) and (IdentityOne.Hash = IdentityTwo.Hash);
end;

function TCloudMailRu.copyFile(OldName, ToPath: WideString): Integer;
var
	JSON: WideString;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);
	self.HTTP.SetProgressNames(OldName, Format('%s%s', [IncludeTrailingPathDelimiter(ToPath), ExtractFileName(OldName)]));
	if self.HTTP.PostForm(API_FILE_COPY, Format('home=/%s&folder=/%s%s&conflict', [PathToUrl(OldName), PathToUrl(ToPath), self.FUnitedParams]), JSON) then
	begin //Парсим ответ
		Result := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_COPY);
	end;
	if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
		Result := self.copyFile(OldName, ToPath);
end;

function TCloudMailRu.cpFile(OldName, NewName: WideString): Integer;
var
	NewPath: WideString;
	SameDir, SameName: Boolean;
begin //Облако умеет скопировать файл, но не сможет его переименовать, поэтому хитрим
	NewPath := ExtractFilePath(NewName);
	SameDir := ExtractFilePath(OldName) = ExtractFilePath(NewName);
	SameName := ExtractFileName(OldName) = ExtractFileName(NewName);
	if (SameDir) then //копирование в тот же каталог не поддерживается напрямую, а мудрить со временными каталогами я не хочу
	begin
		Logger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, ERR_COPY_SAME_DIR_NOT_SUPPORTED);
		Exit(FS_FILE_NOTSUPPORTED);
	end else begin
		{TODO: issue #219}
		//if (self.statusFile(NewName,FileInfo)) then //file already exists
		//begin
		//
		//end;
		Result := self.copyFile(OldName, NewPath);
		if Result <> CLOUD_OPERATION_OK then
			Exit;
	end;
	if not(SameName) then
	begin //скопированный файл лежит в новом каталоге со старым именем
		Result := self.renameFile(Format('%s%s', [NewPath, ExtractFileName(OldName)]), ExtractFileName(NewName));
	end;
end;

constructor TCloudMailRu.Create(CloudSettings: TCloudSettings; ConnectionManager: THTTPManager; Progress: TTCProgress; Logger: TTCLogger; Request: TTCRequest);
begin
	try
		self.Settings := CloudSettings;
		ExtractEmailParts(FEmail, FUser, FDomain);

		self.HTTPConnectionsManager := ConnectionManager;

		self.Progress := Progress;
		if not Assigned(self.Progress) then
			self.Progress := TTCProgress.Create();
		self.Logger := Logger;
		if not Assigned(self.Logger) then
			self.Logger := TTCLogger.Create();
		self.Request := Request;
		if not Assigned(self.Request) then
			self.Request := TTCRequest.Create();

		self.AuthCookie := TIdCookieManager.Create();

		if Settings.AccountSettings.EncryptFilesMode <> EncryptModeNone then
		begin
			self.FileCipher := TFileCipher.Create(Settings.CryptFilesPassword, Settings.AccountSettings.CryptedGUIDFiles, Settings.AccountSettings.EncryptFilenames);
			if self.FileCipher.IsWrongPassword then
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_WRONG_ENCRYPT_PASSWORD);

			self.FDoCryptFiles := not(self.FileCipher.IsWrongPassword);
			self.FDoCryptFilenames := self.FDoCryptFiles and Settings.AccountSettings.EncryptFilenames and not(self.FileCipher.IsWrongPassword);
		end;

	except
		on E: Exception do
		begin
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_ERR_FILE_UPLOADING, E.Message]);
		end;
	end;
end;

function TCloudMailRu.createDir(Path: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit;
	self.HTTP.SetProgressNames(CREATE_DIRECTORY, Path);
	Result := self.HTTP.PostForm(API_FOLDER_ADD, Format('home=/%s%s&conflict', [PathToUrl(Path), self.FUnitedParams]), JSON) and CloudResultToBoolean(JSON);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := self.createDir(Path);
end;

function TCloudMailRu.deleteFile(Path: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit;
	self.HTTP.SetProgressNames(DELETE_FILE, Path);
	Result := self.HTTP.PostForm(API_FILE_REMOVE, Format('home=/%s%s&conflict', [PathToUrl(Path), self.FUnitedParams]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_DELETE_FILE);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := self.deleteFile(Path);
end;

destructor TCloudMailRu.Destroy;
begin
	//self.HTTP.Destroy;

	self.AuthCookie.Destroy;

	if Assigned(InternalHTTPConnection) then
		InternalHTTPConnection.Destroy;

	if Assigned(self.FileCipher) then
		self.FileCipher.Destroy;

	inherited;
end;

class function TCloudMailRu.ErrorCodeText(ErrorCode: Integer): WideString;
begin
	case ErrorCode of
		CLOUD_ERROR_EXISTS:
			Exit(ERR_CLOUD_ERROR_EXISTS);
		CLOUD_ERROR_REQUIRED:
			Exit(ERR_CLOUD_ERROR_REQUIRED);
		CLOUD_ERROR_INVALID:
			Exit(ERR_CLOUD_ERROR_INVALID);
		CLOUD_ERROR_READONLY:
			Exit(ERR_CLOUD_ERROR_READONLY);
		CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
			Exit(ERR_CLOUD_ERROR_NAME_LENGTH_EXCEEDED);
		CLOUD_ERROR_OVERQUOTA:
			Exit(ERR_CLOUD_ERROR_OVERQUOTA);
		CLOUD_ERROR_NOT_EXISTS:
			Exit(ERR_CLOUD_ERROR_NOT_EXISTS);
		CLOUD_ERROR_OWN:
			Exit(ERR_CLOUD_ERROR_OWN);
		CLOUD_ERROR_NAME_TOO_LONG:
			Exit(ERR_CLOUD_ERROR_NAME_TOO_LONG);
		CLOUD_ERROR_VIRUS_SCAN_FAIL:
			Exit(ERR_CLOUD_ERROR_VIRUS_SCAN_FAIL);
		CLOUD_ERROR_OWNER:
			Exit(ERR_CLOUD_ERROR_OWNER);
		CLOUD_ERROR_FAHRENHEIT:
			Exit(ERR_CLOUD_ERROR_FAHRENHEIT);
		CLOUD_ERROR_BAD_REQUEST:
			Exit(ERR_CLOUD_ERROR_BAD_REQUEST);
		CLOUD_ERROR_TREES_CONFLICT:
			Exit(ERR_CLOUD_ERROR_TREES_CONFLICT);
		CLOUD_ERROR_UNPROCESSABLE_ENTRY:
			Exit(ERR_CLOUD_ERROR_UNPROCESSABLE_ENTRY);
		CLOUD_ERROR_USER_LIMIT_EXCEEDED:
			Exit(ERR_CLOUD_ERROR_USER_LIMIT_EXCEEDED);
		CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED:
			Exit(ERR_CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED);
		CLOUD_ERROR_NOT_ACCEPTABLE:
			Exit(ERR_CLOUD_ERROR_NOT_ACCEPTABLE);
		else
			Exit(Format(ERR_CLOUD_ERROR_UNKNOWN, [ErrorCode]));
	end;
end;

function TCloudMailRu.FileIdentity(LocalPath: WideString): TCMRFileIdentity;
begin
	Result.Hash := CloudHash(LocalPath);
	Result.size := SizeOfFile(LocalPath);
end;

function TCloudMailRu.getDescriptionFile(RemotePath, localCopy: WideString): Boolean;
var
	ResultHash: WideString;
begin
	Result := self.getFile(RemotePath, localCopy, ResultHash, False) = FS_FILE_OK;
end;

function TCloudMailRu.putDesriptionFile(RemotePath, localCopy: WideString): Boolean;
begin
	if FileExists(localCopy) then
		Result := self.putFile(localCopy, RemotePath) = FS_FILE_OK
	else
		Result := self.deleteFile(RemotePath);
end;

function TCloudMailRu.getSharedLinksListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if self.IsPublicAccount then
		Exit;
	if (ShowProgress) then
		self.HTTP.SetProgressNames(SHARED_LINKS_LISTING, UNKNOWN_ITEM);

	Result := self.HTTP.GetPage(Format('%s?%s', [API_FOLDER_SHARED_LINKS, self.FUnitedParams]), JSON, ShowProgress);
	if Result then
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_SHARED_LINKS_LISTING) and getDirListing(JSON, DirListing)
	else
	begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := getSharedLinksListing(DirListing, ShowProgress);
	end;

end;

function TCloudMailRu.getIncomingLinksListing(var IncomingListing: TCMRIncomingInviteList; ShowProgress: Boolean): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	SetLength(IncomingListing, 0);
	if self.IsPublicAccount then
		Exit;
	if (ShowProgress) then
		self.HTTP.SetProgressNames(INCOMING_LINKS_LISTING, UNKNOWN_ITEM);
	Result := self.HTTP.GetPage(Format('%s?%s', [API_FOLDER_SHARED_INCOMING, self.FUnitedParams]), JSON, ShowProgress);

	if Result then
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_INCOMING_REQUESTS_LISTING) and IncomingListing.FromJSON(JSON)
	else
	begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := getIncomingLinksListing(IncomingListing, ShowProgress);
	end;
end;

function TCloudMailRu.getIncomingLinksListing(var IncomingListing: TCMRDirItemList; var InvitesListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean;
var
	i: Integer;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	SetLength(IncomingListing, 0);
	Result := self.getIncomingLinksListing(InvitesListing, ShowProgress);
	if Result then
	begin
		SetLength(IncomingListing, length(InvitesListing));
		for i := 0 to length(InvitesListing) - 1 do
		begin

			IncomingListing[i].name := InvitesListing[i].name;
			IncomingListing[i].size := InvitesListing[i].size;
			IncomingListing[i].tree := InvitesListing[i].tree;
			//IncomingListing[length(IncomingListing)].
		end;

	end;
end;

function TCloudMailRu.getTrashbinListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if self.IsPublicAccount then
		Exit;
	if (ShowProgress) then
		self.HTTP.SetProgressNames(TRASH_LISTING, UNKNOWN_ITEM);
	Result := self.HTTP.GetPage(Format('%s?%s', [API_TRASHBIN, self.FUnitedParams]), JSON, ShowProgress);

	if Result then
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_TRASH_LISTING) and getDirListing(JSON, DirListing)
	else
	begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := getTrashbinListing(DirListing, ShowProgress);
	end;

end;

function TCloudMailRu.getDirListing(Path: WideString; var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
var
	JSON: WideString;
	OperationResult: TCMROperationResult;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if self.IsPublicAccount then
		Result := self.HTTP.GetPage(Format('%s&weblink=%s%s%s', [API_FOLDER, IncludeSlash(GetPublicLink), PathToUrl(Path, False), self.FUnitedParams]), JSON, ShowProgress)
	else
	begin
		self.HTTP.SetProgressNames(DIR_LISTING, Path);
		Result := self.HTTP.GetPage(Format('%s&home=%s%s', [API_FOLDER, PathToUrl(Path), self.FUnitedParams]), JSON, ShowProgress);
	end;
	if Result then
	begin
		OperationResult.FromJSON(JSON);
		Result := CloudResultToBoolean(OperationResult, PREFIX_ERR_DIR_LISTING);
		if Result then
		begin
			Result := DirListing.FromJSON(JSON);
			if Result and self.FDoCryptFilenames then
				self.FileCipher.DecryptDirListing(DirListing);
		end else if OperationResult.OperationResult = CLOUD_ERROR_NOT_EXISTS then
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_ERR_PATH_NOT_EXISTS, Path]);
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := getDirListing(Path, DirListing, ShowProgress);
	end;

end;

function TCloudMailRu.getFile(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации

	self.HTTP.SetProgressNames(RemotePath, LocalPath);
	if self.IsPublicAccount then
		Result := self.GetFileShared(RemotePath, LocalPath, ResultHash, LogErrors)
	else
		Result := self.GetFileRegular(RemotePath, LocalPath, ResultHash, LogErrors);

end;

function TCloudMailRu.GetFileRegular(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
var
	FileStream: TBufferedFileStream;
	URL, FileName: WideString;
	MemoryStream: TMemoryStream;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if FDownloadShard = EmptyWideStr then
	begin
		Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, UNDEFINED_DOWNLOAD_SHARD);
		if not GetShard(FDownloadShard) then
			Exit;
	end;
	if self.FDoCryptFilenames then
	begin
		FileName := ExtractUniversalFileName(RemotePath);
		FileName := FileCipher.DecryptFileName(FileName);
		LocalPath := ChangePathFileName(LocalPath, FileName);
	end;

	try
		FileStream := TBufferedFileStream.Create(GetUNCFilePath(LocalPath), fmCreate);
	except
		on E: Exception do
		begin
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, E.Message);
			Exit(FS_FILE_WRITEERROR);
		end;
	end;

	if self.FDoCryptFiles then //Загрузка файла в память, дешифрация в файл
	begin
		MemoryStream := TMemoryStream.Create;
		URL := Format('%s%s', [FDownloadShard, PathToUrl(RemotePath, False)]);
		Result := self.HTTP.getFile(URL, MemoryStream, LogErrors);
		if (CLOUD_ERROR_TOKEN_OUTDATED = Result) and RefreshCSRFToken() then
			Result := self.GetFileRegular(RemotePath, LocalPath, ResultHash, LogErrors);

		if Result in [FS_FILE_NOTSUPPORTED] then //this code returned on shard connection error
		begin
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_REDIRECTION_LIMIT, URL]);
			if (Request.Request(RT_MsgYesNo, REDIRECTION_LIMIT, TRY_ANOTHER_SHARD, EmptyWideStr, 0)) and (self.GetShard(FDownloadShard)) then
				Result := self.GetFileRegular(RemotePath, LocalPath, ResultHash, LogErrors);
		end;

		if Result in [FS_FILE_OK] then
		begin
			ResultHash := CloudHash(MemoryStream);
			MemoryStream.Position := 0;
			self.FileCipher.DecryptStream(MemoryStream, FileStream);
		end;
		MemoryStream.free;

	end else begin
		Result := self.HTTP.getFile(Format('%s%s', [FDownloadShard, PathToUrl(RemotePath, False)]), FileStream, LogErrors);
		if (CLOUD_ERROR_TOKEN_OUTDATED = Result) and RefreshCSRFToken() then
			Result := self.GetFileRegular(RemotePath, LocalPath, ResultHash, LogErrors);
		if ((Result in [FS_FILE_OK]) and (EmptyWideStr = ResultHash)) then
			ResultHash := CloudHash(FileStream);
	end;

	FlushFileBuffers(FileStream.Handle);
	FileStream.free;

	if not(Result in [FS_FILE_OK]) then
		System.SysUtils.deleteFile(GetUNCFilePath(LocalPath));
end;

{since 29.07.2022: изменена логика получения ссылок, см. issue #285. URL теперь всегда должны быть кодированы, иначе в некоторых случаях приходит 400}
function TCloudMailRu.getSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
var
	usedShard: WideString;
	ProgressEnabled: Boolean;
begin
	if ShardType = SHARD_TYPE_DEFAULT then
		usedShard := self.FPublicShard
	else
		self.GetShard(usedShard, ShardType);
	if (self.IsPublicAccount) then
		Exit(Format('%s%s%s', [IncludeSlash(usedShard), IncludeSlash(GetPublicLink), PathToUrl(RemotePath, true, true)]));

	if (TRealPath.GetRealPath(RemotePath).isDir = ID_True) then {для ссылок внутри каталогов перебираются файлы внутри «публичной ссылки» на каталог}
	begin
		Result := Format('%s%s%s', [IncludeSlash(usedShard), GetPublicLink, PathToUrl(RemotePath, true, true)]);
	end else begin {для прямых ссылок берутся публичные ссылки файлов}
		Result := Format('%s%s%s', [IncludeSlash(usedShard), GetPublicLink])
	end;

	ProgressEnabled := False;
	InternalHTTPConnection.GetRedirection(Result, Result, ProgressEnabled);

end;

function TCloudMailRu.GetFileShared(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
var
	FileStream: TBufferedFileStream;
begin
	Result := FS_FILE_NOTFOUND;
	if (FPublicShard = EmptyWideStr) then
		Exit;
	try
		FileStream := TBufferedFileStream.Create(GetUNCFilePath(LocalPath), fmCreate);
	except
		on E: Exception do
		begin
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, E.Message);
			Exit(FS_FILE_WRITEERROR);
		end;
	end;
	if (Assigned(FileStream)) then
	begin
		Result := self.HTTP.getFile(getSharedFileUrl(RemotePath), FileStream, LogErrors);
		if ((Result in [FS_FILE_OK]) and (EmptyWideStr = ResultHash)) then
			ResultHash := CloudHash(FileStream);
		FlushFileBuffers(FileStream.Handle);
		FileStream.free;
	end;
	if Result <> FS_FILE_OK then
		System.SysUtils.deleteFile(GetUNCFilePath(LocalPath));
end;

function TCloudMailRu.GetHTTPConnection: TCloudMailRuHTTP;
begin
	if not(Assigned(self)) then
		Exit(nil); //Проверка на вызов без инициализации
	if (nil = self.HTTPConnectionsManager) then
	begin
		if not Assigned(InternalHTTPConnection) then
			self.InternalHTTPConnection := TCloudMailRuHTTP.Create(Settings.ConnectionSettings, Progress, Logger);

		Result := self.InternalHTTPConnection;
	end
	else
		Result := self.HTTPConnectionsManager.get(GetCurrentThreadID());
	Result.AuthCookie := self.AuthCookie;
	if EmptyWideStr <> FAuthToken then
		Result.HTTP.Request.CustomHeaders.Values['X-CSRF-Token'] := FAuthToken;
end;

function TCloudMailRu.GetOAuthToken(var OAuthToken: TCMROAuth): Boolean;
var
	Answer: WideString;
begin
	Result := False;
	if self.HTTP.PostForm(OAUTH_TOKEN_URL, Format('client_id=cloud-win&grant_type=password&username=%s@%s&password=%s', [FUser, FDomain, UrlEncode(FPassword)]), Answer) then
	begin
		if not OAuthToken.FromJSON(Answer) then
			Exit(False);
		Result := OAuthToken.error_code = NOERROR;
	end;
end;

function TCloudMailRu.GetPublicLink: WideString;
begin
	if FPublicLink <> '' then
		Exit(FPublicLink); {Already have a public link}

	if self.IsPublicAccount and (self.Settings.AccountSettings.PublicUrl <> EmptyWideStr) then
	begin
		FPublicLink := self.Settings.AccountSettings.PublicUrl;
		self.Settings.AccountSettings.PublicUrl := IncludeSlash(self.Settings.AccountSettings.PublicUrl);
		Delete(FPublicLink, 1, length(PUBLIC_ACCESS_URL));
		if (FPublicLink <> EmptyWideStr) and (FPublicLink[length(Result)] = '/') then
			Delete(FPublicLink, length(FPublicLink), 1);
	end;
	Exit(FPublicLink)
end;

function TCloudMailRu.getPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; publish: Boolean = CLOUD_PUBLISH): Boolean;
var
	shard_url: WideString;
begin
	Result := False;
	if (EmptyWideStr = FileIdentity.weblink) then //publish and fill weblink, if required
	begin
		if (not publish) or (not self.publishFile(FileIdentity.home, FileIdentity.weblink)) then
			Exit;
	end;

	if not self.GetShard(shard_url, ShardType) then
		Exit;
	StreamUrl := Format('%s0p/%s.m3u8?double_encode=1', [shard_url, DCPbase64.Base64EncodeStr(String(RawByteString(FileIdentity.weblink)))]); //UTF2Ansi is required
	Result := true;
end;

function TCloudMailRu.GetShard(var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.shard_override <> EmptyWideStr then
	begin
		Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_DETAILS, SHARD_OVERRIDDEN);
		Shard := self.shard_override;
		Exit(true);
	end;
	Result := self.HTTP.PostForm(API_DISPATCHER, self.FUnitedParams, JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_SHARD_RECEIVE);
	if Result then
	begin
		Result := JSONHelper.GetShard(JSON, Shard, ShardType) and (Shard <> EmptyWideStr);
		Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PREFIX_SHARD_RECEIVED, [Shard, ShardType]);
	end;
end;

function TCloudMailRu.InitConnectionParameters: Boolean;
var
	JSON: WideString;
	Progress: Boolean;
	x_page_id, build: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	Progress := False;
	Result := self.HTTP.GetPage(TOKEN_HOME_URL, JSON, Progress);
	if Result then
	begin
		{При первоначальной инициализации получаем токен из страницы ответа, затем он обновляется по необходимости}
		Result := extractTokenFromText(JSON, FAuthToken) and extract_x_page_id_FromText(JSON, x_page_id) and extract_build_FromText(JSON, build); //and extract_upload_url_FromText(JSON, self.upload_url);
		self.FUnitedParams := Format('&api=2&build=%s&x-page-id=%s&email=%s@%s&x-email=%s@%s&_=%d810', [build, x_page_id, FUser, FDomain, FUser, FDomain, DateTimeToUnix(now)]);
	end;
end;

function TCloudMailRu.RefreshCSRFToken: Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	self.HTTP.GetPage(API_CSRF, JSON, Progress);
	Result := getBodyToken(JSON, FAuthToken);
	if Result then
		Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, TOKEN_UPDATED)
	else
		Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_TOKEN_UPDATE)
end;

function TCloudMailRu.initSharedConnectionParameters(): Boolean;
var
	PageContent: WideString;
	Progress: Boolean;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	Progress := False;
	Result := self.HTTP.GetPage(self.Settings.AccountSettings.PublicUrl, PageContent, Progress);
	if Result then
	begin
		if not extractPublicShard(PageContent, FPublicShard) then
		begin
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_GET_PUBLIC_SHARE);
			Exit(False);
		end;
	end;
end;

function TCloudMailRu.GetUserSpace(var SpaceInfo: TCMRSpace): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	Progress := False;
	Result := self.HTTP.GetPage(Format('%s?home=/%s', [API_USER_SPACE, self.FUnitedParams]), JSON, Progress);
	if Result then
	begin
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_GET_USER_SPACE) and SpaceInfo.FromJSON(JSON);
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := GetUserSpace(SpaceInfo)
	end;
end;

function TCloudMailRu.login(Method: Integer): Boolean;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	HTTP.SetProgressNames(LOGIN_IN_PROGRESS, EmptyWideStr);
	if self.IsPublicAccount then
		Result := self.LoginShared()
	else
	begin
		Result := self.LoginRegular(Method);
		if (Result and (EmptyWideStr <> self.upload_url_override)) then
		begin
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_DETAILS, UPLOAD_URL_OVERRIDDEN);
			self.FUploadShard := self.upload_url_override;
			Exit(true);
		end;
	end;
end;

function TCloudMailRu.LoginRegular(Method: Integer): Boolean;
var
	PostAnswer: WideString;
	TwoStepJson: WideString;
	AuthMessage: WideString;
	TwostepData: TCMRTwostep;
	SecurityKey: WideString;
	FormFields: TDictionary<WideString, WideString>;
begin
	Result := False;

	Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, Format(LOGIN_TO, [FEmail]));
	case Method of
		CLOUD_AUTH_METHOD_TWO_STEP:
			begin
				FormFields := TDictionary<WideString, WideString>.Create();
				FormFields.AddOrSetValue('Domain', FDomain);
				FormFields.AddOrSetValue('Login', FUser);
				FormFields.AddOrSetValue('Password', FPassword);
				Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, REQUESTING_FIRST_STEP_AUTH_TOKEN, [FEmail]);
				Result := self.HTTP.PostMultipart(LOGIN_URL, FormFields, PostAnswer);
				if Result then
				begin
					Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PARSING_AUTH_DATA);
					if extractTwostepJson(PostAnswer, TwoStepJson) and TwostepData.FromJSON(TwoStepJson) then
					begin
						if TwostepData.secstep_timeout = AUTH_APP_USED then
							AuthMessage := ASK_AUTH_APP_CODE //mobile app used
						else if TwostepData.secstep_resend_fail = '1' then
							AuthMessage := Format(SMS_TIMEOUT, [TwostepData.secstep_phone, TwostepData.secstep_timeout])
						else
							AuthMessage := Format(ASK_SENT_CODE, [TwostepData.secstep_phone]);

						Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, AWAIT_SECURITY_KEY);

						if (true = TAskPasswordForm.AskText(ASK_AUTH_KEY, AuthMessage, SecurityKey)) then
						begin
							FormFields.Clear;
							FormFields.AddOrSetValue('Login', FEmail);
							FormFields.AddOrSetValue('csrf', TwostepData.csrf);
							FormFields.AddOrSetValue('AuthCode', SecurityKey);
							Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, SECOND_STEP_AUTH);
							Result := self.HTTP.PostMultipart(SECSTEP_URL, FormFields, PostAnswer);
							FormFields.free;
							if Result then
							begin
								Result := self.InitConnectionParameters();
								if (Result) then
								begin
									Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, CONNECTED_TO, [FEmail]);
									self.logUserSpaceInfo;
								end else begin
									Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_TWOSTEP_AUTH);
								end;
							end;
						end else begin
							Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_SECURITY_KEY);
							Exit(False);
						end;

					end else begin
						Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARSE_AUTH_DATA);
						Exit(False);
					end;

				end else begin
					Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_GET_FIRST_STEP_AUTH_TOKEN, [FEmail]);
					FormFields.free;
				end;
			end;
		CLOUD_AUTH_METHOD_WEB: //todo: вынести в отдельный метод
			begin
				Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, REQUESTING_AUTH_TOKEN, [FEmail]);
				Result := self.HTTP.PostForm(LOGIN_URL, Format('page=https://cloud.mail.ru/?new_auth_form=1&Domain=%s&Login=%s&Password=%s&FailPage=', [FDomain, FUser, UrlEncode(FPassword)]), PostAnswer);
				if (Result) then
				begin
					Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PARSING_TOKEN_DATA);
					Result := self.InitConnectionParameters();
					if (Result) then
					begin
						Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, CONNECTED_TO, [FEmail]);
						self.logUserSpaceInfo;
					end else begin
						Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARSING_AUTH_TOKEN, [FEmail]);
						Exit(False);
					end;
				end
				else
					Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_GET_AUTH_TOKEN, [FEmail]);
			end;
		CLOUD_AUTH_METHOD_OAUTH:
			begin
				Result := self.GetOAuthToken(self.FOAuthToken);
				if not Result then
					Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, PREFIX_ERR_OAUTH, [self.FOAuthToken.error, self.FOAuthToken.error_description]);
			end;
	end;
end;

function TCloudMailRu.LoginShared(Method: Integer): Boolean;
begin
	Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, URL_OPEN, [self.Settings.AccountSettings.PublicUrl]);
	Exit(self.initSharedConnectionParameters());
end;

procedure TCloudMailRu.logUserSpaceInfo;
var
	US: TCMRSpace;
	QuotaInfo: WideString;
begin
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit;
	if self.GetUserSpace(US) then
	begin
		if (US.overquota) then
			QuotaInfo := WARN_QUOTA_EXHAUSTED
		else
			QuotaInfo := EmptyWideStr;
		Logger.Log(LOG_LEVEL_FILE_OPERATION, MSGTYPE_DETAILS, USER_SPACE_INFO, [FormatSize(US.total), FormatSize(US.used), FormatSize(US.total - US.used), QuotaInfo]);
	end else begin
		Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_IMPORTANTERROR, ERR_GET_USER_SPACE, [FEmail]);
	end;
end;

function TCloudMailRu.moveFile(OldName, ToPath: WideString): Integer;
var
	JSON: WideString;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);
	if self.HTTP.PostForm(API_FILE_MOVE, Format('home=%s&folder=%s%s&conflict', [PathToUrl(OldName), PathToUrl(ToPath), self.FUnitedParams]), JSON) then
		Result := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_MOVE);
	if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
		Result := self.moveFile(OldName, ToPath);
end;

function TCloudMailRu.mvFile(OldName, NewName: WideString): Integer;
var
	NewPath: WideString;
	SameDir, SameName: Boolean;
begin //К сожалению, переименование и перемещение в облаке - разные действия
	NewPath := ExtractFilePath(NewName);
	SameDir := ExtractFilePath(OldName) = ExtractFilePath(NewName);
	SameName := ExtractFileName(OldName) = ExtractFileName(NewName);
	if SameDir then
	begin //один каталог
		Result := self.renameFile(OldName, ExtractFileName(NewName));
	end else begin
		Result := self.moveFile(OldName, ExtractFilePath(NewName)); //Если файл со старым именем лежит в новом каталоге, вернётся ошибка. Так реализовано в облаке, а мудрить со временными каталогами я не хочу
		if Result <> CLOUD_OPERATION_OK then
			Exit;
		if not(SameName) then
		begin //скопированный файл лежит в новом каталоге со старым именем
			Result := self.renameFile(Format('%s%s', [NewPath, ExtractFileName(OldName)]), ExtractFileName(NewName));
		end;
	end;
end;

function TCloudMailRu.publishFile(Path: WideString; var PublicLink: WideString; publish: Boolean): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit;
	if publish then
	begin
		Result := self.HTTP.PostForm(API_FILE_PUBLISH, Format('home=/%s%s&conflict', [PathToUrl(Path), self.FUnitedParams]), JSON, 'application/x-www-form-urlencoded', true, False);
	end else begin
		Result := self.HTTP.PostForm(API_FILE_UNPUBLISH, Format('weblink=%s%s&conflict', [PublicLink, self.FUnitedParams]), JSON, 'application/x-www-form-urlencoded', true, False);
	end;

	if Result then
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_FILE_PUBLISH);

	if Result and publish then
		Result := JSONHelper.GetPublicLink(JSON, PublicLink);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := self.publishFile(Path, PublicLink, publish);
end;

function TCloudMailRu.getShareInfo(Path: WideString; var InviteListing: TCMRInviteList): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	Progress := False;
	if self.HTTP.GetPage(Format('%s?home=%s%s', [API_FOLDER_SHARED_INFO, PathToUrl(Path), self.FUnitedParams]), JSON, Progress) then
	begin
		Result := InviteListing.FromJSON(JSON);
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := getShareInfo(Path, InviteListing);
	end;
end;

function TCloudMailRu.shareFolder(Path, email: WideString; access: Integer): Boolean;
var
	JSON: WideString;
	access_string: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if access in [CLOUD_SHARE_RW, CLOUD_SHARE_RO] then
	begin
		if access = CLOUD_SHARE_RW then
			access_string := CLOUD_SHARE_ACCESS_READ_WRITE
		else
			access_string := CLOUD_SHARE_ACCESS_READ_ONLY;

		Result := self.HTTP.PostForm(API_FOLDER_SHARE, Format('home=/%s%s&invite={"email":"%s","access":"%s"}', [PathToUrl(Path), self.FUnitedParams, email, access_string]), JSON)
	end else begin
		Result := self.HTTP.PostForm(API_FOLDER_UNSHARE, Format('home=/%s%s&invite={"email":"%s"}', [PathToUrl(Path), self.FUnitedParams, email]), JSON);
	end;
	if Result then
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_INVITE_MEMBER);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := self.shareFolder(Path, email, access);
end;

function TCloudMailRu.trashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit;
	Result := self.HTTP.PostForm(API_TRASHBIN_RESTORE, Format('path=%s&restore_revision=%d%s&conflict=%s', [PathToUrl(Path), RestoreRevision, self.FUnitedParams, ConflictMode]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_FILE_RESTORE);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := self.trashbinRestore(Path, RestoreRevision, ConflictMode);
end;

function TCloudMailRu.trashbinEmpty(): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit;

	Result := self.HTTP.PostForm(API_TRASHBIN_EMPTY, self.FUnitedParams, JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_TRASH_CLEAN);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := self.trashbinEmpty();
end;

function TCloudMailRu.mountFolder(home, invite_token, ConflictMode: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit;
	Result := self.HTTP.PostForm(API_FOLDER_MOUNT, Format('home=%s&invite_token=%s%s&conflict=%s', [UrlEncode(home), invite_token, self.FUnitedParams, ConflictMode]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_FOLDER_MOUNT);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := self.mountFolder(home, invite_token, ConflictMode);
end;

function TCloudMailRu.unmountFolder(home: WideString; clone_copy: Boolean): Boolean;
var
	JSON: WideString;
	CopyStr: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit;
	if clone_copy then
		CopyStr := 'true'
	else
		CopyStr := 'false';
	Result := self.HTTP.PostForm(API_FOLDER_UNMOUNT, Format('home=%s&clone_copy=%s%s', [UrlEncode(home), CopyStr, self.FUnitedParams]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_FOLDER_UNMOUNT);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := self.unmountFolder(home, clone_copy);
end;

function TCloudMailRu.rejectInvite(invite_token: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit;
	Result := self.HTTP.PostForm(API_INVITE_REJECT, Format('invite_token=%s%s', [invite_token, self.FUnitedParams]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_INVITE_REJECT);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := self.rejectInvite(invite_token);
end;

function TCloudMailRu.PutFileStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString): Integer;
var
	LocalFileIdentity, RemoteFileIdentity: TCMRFileIdentity;
	OperationResult: Integer;
	MemoryStream: TMemoryStream;
	UseHash: Boolean;
begin

	Result := FS_FILE_WRITEERROR;
	OperationResult := CLOUD_OPERATION_FAILED;

	UseHash := self.PrecalculateHash or (self.ForcePrecalculateSize >= FileStream.size); //issue #231

	if UseHash or self.CheckCRC then
	begin
		LocalFileIdentity.Hash := CloudHash(FileStream);
		LocalFileIdentity.size := FileStream.size;
	end;
	if UseHash and (LocalFileIdentity.Hash <> EmptyWideStr) and (not self.FDoCryptFiles) and (FS_FILE_OK = self.addFileByIdentity(LocalFileIdentity, RemotePath, CLOUD_CONFLICT_STRICT, False, true)) then {issue #135}
		Exit(CLOUD_OPERATION_OK);

	try
		if self.FDoCryptFiles then {Will encrypt any type of data passed here}
		begin
			MemoryStream := TMemoryStream.Create;
			self.FileCipher.CryptStream(FileStream, MemoryStream);
			MemoryStream.Position := 0;
			OperationResult := self.PutFileToCloud(FileName, MemoryStream, RemoteFileIdentity);
			MemoryStream.Destroy;
		end else begin
			OperationResult := self.PutFileToCloud(FileName, FileStream, RemoteFileIdentity)
		end;
	except
		on E: Exception do
		begin
			if E.ClassName = 'EAbort' then
			begin
				Result := FS_FILE_USERABORT;
			end else begin
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_UPLOAD_INFO, [E.ClassName, E.Message]);
				Result := FS_FILE_WRITEERROR;
			end;
		end;
	end;
	if OperationResult = CLOUD_OPERATION_OK then
	begin
		if self.CheckCRC then
		begin
			if not IsSameIdentity(LocalFileIdentity, RemoteFileIdentity) then {При включённой проверке CRC сравниваем хеши и размеры}
				Result := CLOUD_OPERATION_FAILED;

		end;
	end else if OperationResult = CLOUD_OPERATION_CANCELLED then
	begin
		Result := FS_FILE_USERABORT;
	end;

	if OperationResult = CLOUD_OPERATION_OK then
		Result := self.addFileByIdentity(RemoteFileIdentity, RemotePath, ConflictMode, False); //Не логируем HTTP-ошибку, она распарсится и обработается уровнем выше
end;

function TCloudMailRu.PutFileWhole(LocalPath, RemotePath, ConflictMode: WideString): Integer;
var
	FileStream: TBufferedFileStream;
begin
	FileStream := TBufferedFileStream.Create(GetUNCFilePath(LocalPath), fmOpenRead or fmShareDenyWrite);
	Result := self.PutFileStream(ExtractFileName(RemotePath), RemotePath, FileStream, ConflictMode); {putFileStream может обойтись без параметра имени - оно всегда берётся из remotePath}
	FileStream.free;
end;

{$WARN NO_RETVAL OFF}
(*
 The W1035 compiler warning could be a false positive in this case
 BUT this code needs to be covered with tests and perhaps refactored due its length and complexity.
*)

function TCloudMailRu.PutFileSplit(LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
var
	LocalFileIdentity: TCMRFileIdentity;
	SplitFileInfo: TFileSplitInfo;
	SplittedPartIndex: Integer;
	ChunkRemotePath, CRCRemotePath: WideString;
	ChunkStream: TChunkedFileStream;
	CRCStream: TStringStream;
	RetryAttemptsCount: Integer;
	UseHash: Boolean;
begin
	UseHash := self.PrecalculateHash or (self.ForcePrecalculateSize >= SizeOfFile(LocalPath)); //issue #231
	if UseHash then //try to add whole file by hash at first.
		LocalFileIdentity := FileIdentity(GetUNCFilePath(LocalPath));
	{Отмена расчёта хеша приведёт к отмене всей операции: TC запоминает нажатие отмены и ExternalProgressProc будет возвращать 1 до следующего вызова копирования}
	if UseHash and (LocalFileIdentity.Hash <> EmptyWideStr) and (not self.FDoCryptFiles) and (FS_FILE_OK = self.addFileByIdentity(LocalFileIdentity, RemotePath, CLOUD_CONFLICT_STRICT, False, true)) then {issue #135}
		Exit(CLOUD_OPERATION_OK);

	SplitFileInfo := TFileSplitInfo.Create(GetUNCFilePath(LocalPath), self.CloudMaxFileSize); //quickly get information about file parts
	RetryAttemptsCount := 0;
	SplittedPartIndex := 0;

	while SplittedPartIndex < SplitFileInfo.ChunksCount do {use while instead for..loop, need to modify loop counter sometimes}
	begin
		ChunkRemotePath := Format('%s%s', [ExtractFilePath(RemotePath), SplitFileInfo.GetChunks[SplittedPartIndex].name]);
		self.HTTP.SetProgressNames(LocalPath, ChunkRemotePath);
		Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PARTIAL_UPLOAD_INFO, [LocalPath, (SplittedPartIndex + 1), SplitFileInfo.ChunksCount, ChunkRemotePath]);
		ChunkStream := TChunkedFileStream.Create(GetUNCFilePath(LocalPath), fmOpenRead or fmShareDenyWrite, SplitFileInfo.GetChunks[SplittedPartIndex].start, SplitFileInfo.GetChunks[SplittedPartIndex].size);
		Result := self.PutFileStream(ExtractFileName(ChunkRemotePath), ChunkRemotePath, ChunkStream, ConflictMode);
		ChunkStream.Destroy;

		case Result of
			FS_FILE_OK:
				begin
					RetryAttemptsCount := 0;
				end;
			FS_FILE_USERABORT:
				begin
					Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PARTIAL_UPLOAD_ABORTED);
					Break;
				end;
			FS_FILE_EXISTS:
				begin
					case ChunkOverwriteMode of
						ChunkOverwrite: //silently overwrite chunk
							begin
								Logger.Log(LOG_LEVEL_WARNING, MSGTYPE_DETAILS, CHUNK_OVERWRITE, [ChunkRemotePath]);
								if not(self.deleteFile(ChunkRemotePath)) then
								begin
									Result := FS_FILE_WRITEERROR;
									Break;
								end else begin
									Dec(SplittedPartIndex); //retry with this chunk
								end;
							end;
						ChunkOverwriteIgnore: //ignore this chunk
							begin
								Logger.Log(LOG_LEVEL_WARNING, MSGTYPE_DETAILS, CHUNK_SKIP, [ChunkRemotePath]); //ignore and continue
							end;
						ChunkOverwriteAbort: //abort operation
							begin
								Logger.Log(LOG_LEVEL_WARNING, MSGTYPE_DETAILS, CHUNK_ABORT, [ChunkRemotePath]);
								Result := FS_FILE_NOTSUPPORTED;
								Break;
							end;
					end;
				end;
			else {any other error}
				begin
					case OperationErrorMode of
						OperationErrorModeAsk:
							begin
								case (MsgBox(ERR_PARTIAL_UPLOAD_ASK, [Result, ChunkRemotePath], ERR_UPLOAD, MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
									ID_ABORT:
										begin
											Result := FS_FILE_USERABORT;
											Break;
										end;
									ID_RETRY:
										Dec(SplittedPartIndex); //retry with this chunk
									ID_IGNORE:
										begin {do nothing && continue}
										end;
								end;
							end;
						OperationErrorModeIgnore:
							begin
								Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_IGNORE, [Result]);
							end;
						OperationErrorModeAbort:
							begin
								Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_ABORT, [Result]);
								Result := FS_FILE_USERABORT;
								Break;
							end;
						OperationErrorModeRetry:
							begin
								Inc(RetryAttemptsCount);
								if RetryAttemptsCount <> RetryAttempts + 1 then
								begin
									Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_RETRY, [Result, RetryAttemptsCount, RetryAttempts]);
									Dec(SplittedPartIndex); //retry with this chunk
									ProcessMessages;
									Sleep(AttemptWait);
								end else begin
									Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_RETRY_EXCEED, [Result]);
									Result := CLOUD_OPERATION_FAILED;
									Break;
								end;
							end
						else {unknown option value}
							begin
								Result := CLOUD_OPERATION_FAILED;
								Break;
							end;
					end
				end;
		end;
		Inc(SplittedPartIndex); //all ok, continue with next chunk
	end; {end while}

	if Result = FS_FILE_OK then {Only after successful upload}
	begin
		CRCRemotePath := ExtractFilePath(RemotePath) + SplitFileInfo.CRCFileName;
		self.HTTP.TargetName := CRCRemotePath;
		CRCStream := TStringStream.Create;
		SplitFileInfo.GetCRCData(CRCStream);
		self.PutFileStream(SplitFileInfo.CRCFileName, CRCRemotePath, CRCStream, ConflictMode);
		CRCStream.Destroy;
	end;

	SplitFileInfo.Destroy;
	Exit(FS_FILE_OK); //Файлик залит по частям, выходим
end;
{$WARN NO_RETVAL ON}

{Wrapper for putFileWhole/putFileSplit}
function TCloudMailRu.putFile(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer;
begin
	if not(Assigned(self)) then
		Exit(FS_FILE_WRITEERROR); //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);
	self.HTTP.SetProgressNames(LocalPath, RemotePath);
	if (not(self.unlimited_filesize)) and (SizeOfFile(GetUNCFilePath(LocalPath)) > self.CloudMaxFileSize) then
	begin
		if self.split_large_files then
		begin
			Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, SPLIT_LARGE_FILE, [self.CloudMaxFileSize]);
			Exit(PutFileSplit(LocalPath, RemotePath, ConflictMode, ChunkOverwriteMode));
		end else begin
			Logger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, SPLIT_LARGE_FILE_IGNORE, [self.CloudMaxFileSize]);
			Exit(FS_FILE_NOTSUPPORTED);
		end;
	end;

	Result := PutFileWhole(LocalPath, RemotePath, ConflictMode);
end;

function TCloudMailRu.PutFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCMRFileIdentity): Integer;
var
	PostAnswer: WideString;
	return: TStringList;
	UploadUrl: WideString;
begin
	FileIdentity.Hash := EmptyWideStr;
	FileIdentity.size := -1;
	Result := CLOUD_OPERATION_FAILED;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit;
	if (EmptyWideStr = self.FUploadShard) then
	begin
		Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, UNDEFINED_UPLOAD_SHARD);
		self.GetShard(self.FUploadShard, SHARD_TYPE_UPLOAD);
	end;

	UploadUrl := Format('%s?cloud_domain=2&x-email=%s@%s', [self.FUploadShard, FUser, FDomain])(*+ '&fileapi' + DateTimeToUnix(now).ToString + '0246'*);
	return := TStringList.Create;
	//self.HTTP.OptionsMethod(UploadUrl, PostAnswer, ProgressEnabled); //not required at current moment, see issue #232
	Result := self.HTTP.putFile(UploadUrl, FileName, FileStream, PostAnswer);
	if (CLOUD_ERROR_TOKEN_OUTDATED = Result) and RefreshCSRFToken() then
		Result := PutFileToCloud(FileName, FileStream, FileIdentity);
	if (Result = CLOUD_OPERATION_OK) then
	begin
		if length(PostAnswer) <> 40 then
		begin
			Result := CLOUD_OPERATION_FAILED;
		end else begin
			FileIdentity.Hash := PostAnswer;
			FileIdentity.size := FileStream.size;
		end;
	end;
	return.Destroy;
end;

function TCloudMailRu.removeDir(Path: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit;
	self.HTTP.SetProgressNames(DELETE_DIR, Path);
	Result := self.HTTP.PostForm(API_FILE_REMOVE, Format('home=/%s%s&conflict', [IncludeSlash(PathToUrl(Path)), self.FUnitedParams]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_DELETE_DIR); //API всегда отвечает true, даже если путь не существует
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := self.removeDir(Path);
end;

function TCloudMailRu.renameFile(OldName, NewName: WideString): Integer;
var
	JSON: WideString;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if self.IsPublicAccount then
		Exit;
	if self.HTTP.PostForm(API_FILE_RENAME, Format('home=%s&name=%s%s', [PathToUrl(OldName), PathToUrl(NewName), self.FUnitedParams]), JSON) then
		Result := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_RENAME);
	if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
		Result := self.renameFile(OldName, NewName);
end;

function TCloudMailRu.statusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	Progress := False;
	if self.IsPublicAccount then
		Result := self.HTTP.GetPage(Format('%s?weblink=%s%s%s', [API_FILE, IncludeSlash(GetPublicLink), PathToUrl(Path), self.FUnitedParams]), JSON, Progress)
	else
		Result := self.HTTP.GetPage(Format('%s?home=%s%s', [API_FILE, PathToUrl(Path), self.FUnitedParams]), JSON, Progress);
	if Result then
	begin
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_FILE_STATUS) and FileInfo.FromJSON(JSON);
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := statusFile(Path, FileInfo);
	end;

end;

class function TCloudMailRu.CloudAccessToString(access: WideString; Invert: Boolean): WideString;
begin
	if access = 'read only' then
		access := CLOUD_SHARE_ACCESS_READ_ONLY;
	if access = 'read and write' then
		access := CLOUD_SHARE_ACCESS_READ_WRITE;
	if Invert then
	begin
		if (access = CLOUD_SHARE_ACCESS_READ_ONLY) then
			access := CLOUD_SHARE_ACCESS_READ_WRITE
		else
			access := CLOUD_SHARE_ACCESS_READ_ONLY;
	end;
	if access = CLOUD_SHARE_ACCESS_READ_ONLY then
		Result := 'read only'
	else
		Result := 'read and write';
end;

class function TCloudMailRu.StringToCloudAccess(accessString: WideString; Invert: Boolean): Integer;
begin
	if accessString = 'read only' then
		accessString := CLOUD_SHARE_ACCESS_READ_ONLY;
	if accessString = 'read and write' then
		accessString := CLOUD_SHARE_ACCESS_READ_WRITE;
	if Invert then
	begin
		if (accessString = CLOUD_SHARE_ACCESS_READ_ONLY) then
			accessString := CLOUD_SHARE_ACCESS_READ_WRITE
		else
			accessString := CLOUD_SHARE_ACCESS_READ_ONLY;
	end;
	if accessString = CLOUD_SHARE_ACCESS_READ_ONLY then
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
	TempCloud := TCloudMailRu.Create(TempCloudSettings, nil);
	Result := TempCloud.login;
end;

function TCloudMailRu.CloudHash(Path: WideString): WideString;
var
	Stream: TStream;
begin
	Result := EmptyWideStr;
	if not FileExists(Path) then
		Exit;

	try
		Stream := TBufferedFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
	except
		Exit;
	end;
	Result := CloudHash(Stream, GetLFCFilePath(Path));
	Stream.Destroy;

end;

function TCloudMailRu.CloudHash(Stream: TStream; Path: WideString = CALCULATING_HASH): WideString;
const
	bufSize = 8192;
var
	sha1: THashSHA1;
	buffer: array [0 .. bufSize - 1] of byte;
	read, iteration, processedBytes: int64;
	initBuffer, finalBuffer: TBytes;
	Percent: Integer;
	Aborted: Boolean;
begin
	Stream.Position := 0;
	Result := EmptyWideStr;
	if Stream.size < 21 then
	begin
		SetLength(initBuffer, 20);
		Stream.read(initBuffer, Stream.size);
		Result := UpperCase(THash.DigestAsString(initBuffer));
		Exit;
	end;

	FillChar(buffer, sizeof(buffer), 0);
	initBuffer := TEncoding.UTF8.GetBytes('mrCloud');

	sha1 := THashSHA1.Create;
	sha1.Update(initBuffer, length(initBuffer));
	iteration := 0;
	repeat
		iteration := iteration + 1;
		processedBytes := bufSize * iteration;
		Percent := Round((processedBytes / Stream.size) * 100);
		if Percent > 100 then
			Percent := 100;

		read := Stream.read(buffer, bufSize);
		sha1.Update(buffer, read);
		Aborted := Progress.Progress(Path, CALCULATING_HASH, Percent);
	until (read < sizeof(buffer)) or Aborted;

	finalBuffer := TEncoding.UTF8.GetBytes(Stream.size.ToString);
	sha1.Update(finalBuffer, length(finalBuffer));
	if (not Aborted) then
		Result := UpperCase(sha1.HashAsString);
	sha1.Reset;
end;

end.
