unit CloudMailRu;

interface

uses
	CloudMailRuDirListing,
	CMRDirItem,
	CloudMailRuInviteInfoListing,
	CloudMailRuIncomingInviteInfoListing,
	CMROAuth,
	CMRSpace,
	CMRFileIdentity,
	CMROperationResult,
	CMRTwostep,
	JSONHelper,
	ParsingHelper,
	CMRConstants,
	CloudMailRuHTTP,
	CMRStrings,
	System.Hash,
	System.Classes,
	System.Generics.Collections,
	System.SysUtils,
	PLUGIN_Types,
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
	Settings,
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
		{VARIABLES}
		OptionsSet: TCloudSettings;
		HTTPConnectionsManager: THTTPManager;
		InternalHTTPConnection: TCloudMailRuHTTP; //Если не задан HTTPConnectionsManager, класс создаст своё атомарное соединение

		AuthCookie: TIdCookieManager; //Авторизационная кука - должна храниться отдельно от HTTP-соединения, т.к. ассоциируется с облаком. Передаётся в менеджер HTTP-подключений внутри ConnetionManager

		Logger: TTCLogger;
		Progress: TTCProgress;
		Request: TTCRequest;

		FileCipher: TFileCipher; //Encryption class
		//JSONParser: TCloudMailRuJSONParser; //JSON parser

		public_link: WideString; //public_ params is active for public clouds only
		(*seems to be not used since 25.03.2020 - shared downloads do not require token anymore*)
		//public_download_token: WideString; //token for public urls, refreshes on request
		public_shard: WideString; //public downloads shard url
		Shard: WideString; //download shard url

		AuthToken: WideString; {Текущий (постоянно обновляемый) токен соединения}
		OAuthToken: TCMROAuth; {unused at this moment}

		upload_url: WideString; //stored upload url, filled on initConnectionParameters()
		united_params: WideString; //Объединённый набор авторизационных параметров для подстановки в URL

		{HTTP REQUESTS WRAPPERS}
		function initConnectionParameters(): Boolean;
		function initSharedConnectionParameters(): Boolean;
		function getOAuthToken(var OAuthToken: TCMROAuth): Boolean;
		function getShard(var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean;
		function getUserSpace(var SpaceInfo: TCMRSpace): Boolean;
		function putFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCMRFileIdentity): integer; overload; //отправка на сервер данных из потока
		{PRIVATE UPLOAD METHODS CHAIN (CALLED FROM putFile())}
		function putFileWhole(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer; //Загрузка файла целиком
		function putFileSplit(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: integer = 0): integer; //Загрузка файла по частям
		function putFileStream(FileName, remotePath: WideString; FileStream: TStream; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer;

		{OTHER ROUTINES}
		function CloudResultToFsResult(CloudResult: TCMROperationResult; ErrorPrefix: WideString = ''): integer; overload;
		function CloudResultToFsResult(JSON: WideString; ErrorPrefix: WideString = ''): integer; overload;
		function CloudResultToBoolean(CloudResult: TCMROperationResult; ErrorPrefix: WideString = ''): Boolean; overload;
		function CloudResultToBoolean(JSON: WideString; ErrorPrefix: WideString = ''): Boolean; overload;
		function cloudHash(Path: WideString): WideString; overload; //get cloud hash for specified file
		function cloudHash(Stream: TStream; Path: WideString = CALCULATING_HASH): WideString; overload; //get cloud hash for data in stream
		function getHTTPConnection: TCloudMailRuHTTP;
		function RefreshCSRFToken: Boolean;
	protected
		{REGULAR CLOUD}
		function loginRegular(method: integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function getFileRegular(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean = true): integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		{SHARED WEBFOLDERS}
		function loginShared(method: integer = CLOUD_AUTH_METHOD_WEB): Boolean;

		function getFileShared(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean = true): integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)

		function getPublicLink(): WideString;
	public
		crypt_files: Boolean;
		crypt_filenames: Boolean;

		Property Cookie: TIdCookieManager read AuthCookie;
		Property public_account: Boolean read OptionsSet.AccountSettings.public_account;
		Property user: WideString read OptionsSet.AccountSettings.user;
		Property domain: WideString read OptionsSet.AccountSettings.domain;
		Property password: WideString read OptionsSet.AccountSettings.password;
		Property email: WideString read OptionsSet.AccountSettings.email;

		Property HTTP: TCloudMailRuHTTP read getHTTPConnection;

		Property CloudMaxFileSize: int64 read OptionsSet.CloudMaxFileSize;
		Property PrecalculateHash: Boolean read OptionsSet.PrecalculateHash;
		property ForcePrecalculateSize: int64 read OptionsSet.ForcePrecalculateSize;
		Property CheckCRC: Boolean read OptionsSet.CheckCRC;
		Property shard_override: WideString read OptionsSet.AccountSettings.shard_override;
		Property upload_url_override: WideString read OptionsSet.AccountSettings.upload_url_override;
		Property unlimited_filesize: Boolean read OptionsSet.AccountSettings.unlimited_filesize;
		Property split_large_files: Boolean read OptionsSet.AccountSettings.split_large_files;
		Property OperationErrorMode: integer read OptionsSet.OperationErrorMode;
		Property RetryAttempts: integer read OptionsSet.RetryAttempts;
		Property AttemptWait: integer read OptionsSet.AttemptWait;

		function getSharedFileUrl(remotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;

		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(CloudSettings: TCloudSettings; ConnectionManager: THTTPManager; Progress: TTCProgress = nil; Logger: TTCLogger = nil; Request: TTCRequest = nil);
		destructor Destroy; override;
		{CLOUD INTERFACE METHODS}
		function login(method: integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function getDirListing(Path: WideString; var DirListing: TCMRDirItemList; ShowProgress: Boolean = false): Boolean;
		function getSharedLinksListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean = false): Boolean;
		function getIncomingLinksListing(var IncomingListing: TCloudMailRuIncomingInviteInfoListing; ShowProgress: Boolean = false): Boolean; overload;
		function getIncomingLinksListing(var IncomingListing: TCMRDirItemList; var InvitesListing: TCloudMailRuIncomingInviteInfoListing; ShowProgress: Boolean = false): Boolean; overload;
		function getTrashbinListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean = false): Boolean;
		function createDir(Path: WideString): Boolean;
		function removeDir(Path: WideString): Boolean;
		function statusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
		function getFile(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean = true): integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		function putFile(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: integer = 0): integer;
		function renameFile(OldName, NewName: WideString): integer; //смена имени без перемещения
		function moveFile(OldName, ToPath: WideString): integer; //перемещение по дереву каталогов
		function copyFile(OldName, ToPath: WideString): integer; //Копирование файла внутри одного аккаунта
		function mvFile(OldName, NewName: WideString): integer; //объединяющая функция, определяет делать rename или move
		function cpFile(OldName, NewName: WideString): integer; //Копирует файл, и переименует, если нужно
		function deleteFile(Path: WideString): Boolean;
		function publishFile(Path: WideString; var PublicLink: WideString; publish: Boolean = CLOUD_PUBLISH): Boolean;
		function cloneWeblink(Path, link: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): integer; //клонировать публичную ссылку в текущий каталог

		function addFileByIdentity(FileIdentity: TCMRFileIdentity; remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = false): integer; overload;
		function addFileByIdentity(FileIdentity: TCMRDirItem; remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = false): integer; overload;

		function getShareInfo(Path: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
		function shareFolder(Path, email: WideString; access: integer): Boolean;
		function trashbinRestore(Path: WideString; RestoreRevision: integer; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function trashbinEmpty(): Boolean;
		function mountFolder(home, invite_token: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function unmountFolder(home: WideString; clone_copy: Boolean): Boolean;
		function rejectInvite(invite_token: WideString): Boolean;
		function getPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; publish: Boolean = CLOUD_PUBLISH): Boolean;
		{OTHER ROUTINES}
		function getDescriptionFile(remotePath, localCopy: WideString): Boolean; //Если в каталоге remotePath есть descript.ion - скопировать его в файл localcopy
		function putDesriptionFile(remotePath, localCopy: WideString): Boolean; //Скопировать descript.ion из временного файла на сервер
		procedure logUserSpaceInfo();
		function FileIdentity(localPath: WideString): TCMRFileIdentity;
		{STATIC ROUTINES}
		class function CloudAccessToString(access: WideString; Invert: Boolean = false): WideString; static;
		class function StringToCloudAccess(accessString: WideString; Invert: Boolean = false): integer; static;
		class function ErrorCodeText(ErrorCode: integer): WideString; static;
		class function IsSameIdentity(IdentityOne, IdentityTwo: TCMRFileIdentity): Boolean; static;
		class function TempPublicCloudInit(var TempCloud: TCloudMailRu; publicUrl: WideString): Boolean; static;
	end;

implementation

{TCloudMailRu}
function TCloudMailRu.addFileByIdentity(FileIdentity: TCMRFileIdentity; remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = false): integer;
var
	FileName: WideString;
	JSON: WideString;
	OperationResult: TCMROperationResult;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);

	if self.crypt_filenames then
	begin
		FileName := ExtractUniversalFileName(remotePath);
		FileName := FileCipher.CryptFileName(FileName);
		remotePath := ChangePathFileName(remotePath, FileName);
	end;
	{Экспериментально выяснено, что параметры api, build, email, x-email, x-page-id в запросе не обязательны}
	if self.HTTP.PostForm(API_FILE_ADD, Format('api=2&conflict=%s&home=/%s&hash=%s&size=%d%s', [ConflictMode, PathToUrl(remotePath), FileIdentity.Hash, FileIdentity.size, self.united_params]), JSON, 'application/x-www-form-urlencoded', LogErrors, false) then {Do not allow to cancel operation here}
	begin
		OperationResult.FromJSON(JSON);
		result := CloudResultToFsResult(OperationResult, PREFIX_ERR_FILE_UPLOADING);
		if (CLOUD_OPERATION_OK = OperationResult.OperationResult) and LogSuccess then
			Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, FILE_FOUND_BY_HASH, [remotePath]);
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			result := self.addFileByIdentity(FileIdentity, remotePath, ConflictMode, LogErrors, LogSuccess);
	end;
end;

function TCloudMailRu.addFileByIdentity(FileIdentity: TCMRDirItem; remotePath, ConflictMode: WideString; LogErrors, LogSuccess: Boolean): integer;
var
	CloudFileIdentity: TCMRFileIdentity;
begin
	CloudFileIdentity.Hash := FileIdentity.Hash;
	CloudFileIdentity.size := FileIdentity.size;
	result := self.addFileByIdentity(CloudFileIdentity, remotePath, ConflictMode, LogErrors, LogSuccess)
end;

function TCloudMailRu.cloneWeblink(Path, link, ConflictMode: WideString): integer;
var
	JSON: WideString;
	Progress: Boolean;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);
	Progress := true;
	if self.HTTP.GetPage(Format('%s?folder=/%s&weblink=%s&conflict=%s%s', [API_CLONE, PathToUrl(Path), link, ConflictMode, self.united_params]), JSON, Progress) then
	begin //Парсим ответ
		result := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_PUBLISH);
		if (result <> FS_FILE_OK) and not(Progress) then
			result := FS_FILE_USERABORT; //user cancelled
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			result := cloneWeblink(Path, link, ConflictMode);
	end;
end;

function TCloudMailRu.CloudResultToBoolean(JSON, ErrorPrefix: WideString): Boolean;
begin
	result := CloudResultToBoolean(TCMROperationResult.GetOperationResult(JSON), ErrorPrefix)
end;

function TCloudMailRu.CloudResultToBoolean(CloudResult: TCMROperationResult; ErrorPrefix: WideString): Boolean;
begin
	result := CloudResult.ToBoolean;
	if not(result) and (ErrorPrefix <> EmptyWideStr) then
		Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s%s%s', [ErrorPrefix, self.ErrorCodeText(CloudResult.OperationResult), PREFIX_STATUS, CloudResult.OperationStatus]);
end;

function TCloudMailRu.CloudResultToFsResult(JSON, ErrorPrefix: WideString): integer;
begin
	result := CloudResultToFsResult(TCMROperationResult.GetOperationResult(JSON), ErrorPrefix);
end;

function TCloudMailRu.CloudResultToFsResult(CloudResult: TCMROperationResult; ErrorPrefix: WideString): integer;
begin
	case CloudResult.OperationResult of
		CLOUD_OPERATION_OK:
			exit(FS_FILE_OK);
		CLOUD_ERROR_EXISTS:
			exit(FS_FILE_EXISTS);
		CLOUD_ERROR_REQUIRED, CLOUD_ERROR_INVALID, CLOUD_ERROR_READONLY, CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
			exit(FS_FILE_WRITEERROR);
		CLOUD_ERROR_UNKNOWN:
			exit(FS_FILE_NOTSUPPORTED);
		CLOUD_ERROR_OVERQUOTA:
			begin
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_INSUFFICIENT_STORAGE);
				exit(FS_FILE_WRITEERROR);
			end;
		CLOUD_ERROR_NAME_TOO_LONG:
			begin
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_NAME_TOO_LONG);
				exit(FS_FILE_WRITEERROR);
			end;
		else
			begin //что-то неизвестное
				if (ErrorPrefix <> EmptyWideStr) then
					Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s%s%d', [ErrorPrefix, self.ErrorCodeText(CloudResult.OperationResult), PREFIX_STATUS, CloudResult.OperationStatus]);
				exit(FS_FILE_WRITEERROR);
			end;
	end;
end;

class function TCloudMailRu.IsSameIdentity(IdentityOne, IdentityTwo: TCMRFileIdentity): Boolean;
begin
	result := (IdentityOne.size = IdentityTwo.size) and (IdentityOne.Hash = IdentityTwo.Hash);
end;

function TCloudMailRu.copyFile(OldName, ToPath: WideString): integer;
var
	JSON: WideString;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);
	self.HTTP.SetProgressNames(OldName, Format('%s%s', [IncludeTrailingPathDelimiter(ToPath), ExtractFileName(OldName)]));
	if self.HTTP.PostForm(API_FILE_COPY, Format('home=/%s&folder=/%s%s&conflict', [PathToUrl(OldName), PathToUrl(ToPath), self.united_params]), JSON) then
	begin //Парсим ответ
		result := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_COPY);
	end;
	if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
		result := self.copyFile(OldName, ToPath);
end;

function TCloudMailRu.cpFile(OldName, NewName: WideString): integer;
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
		exit(FS_FILE_NOTSUPPORTED);
	end else begin
		{TODO: issue #219}
		//if (self.statusFile(NewName,FileInfo)) then //file already exists
		//begin
		//
		//end;
		result := self.copyFile(OldName, NewPath);
		if result <> CLOUD_OPERATION_OK then
			exit;
	end;
	if not(SameName) then
	begin //скопированный файл лежит в новом каталоге со старым именем
		result := self.renameFile(Format('%s%s', [NewPath, ExtractFileName(OldName)]), ExtractFileName(NewName));
	end;
end;

constructor TCloudMailRu.Create(CloudSettings: TCloudSettings; ConnectionManager: THTTPManager; Progress: TTCProgress; Logger: TTCLogger; Request: TTCRequest);
begin
	try
		self.OptionsSet := CloudSettings;

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

		//self.HTTP := TCloudMailRuHTTP.Create(CloudSettings.ConnectionSettings, ExternalProgressProc, ExternalLogProc);
		//self.JSONParser := TCloudMailRuJSONParser.Create();

		if CloudSettings.AccountSettings.encrypt_files_mode <> EncryptModeNone then
		begin
			self.FileCipher := TFileCipher.Create(CloudSettings.AccountSettings.crypt_files_password, CloudSettings.AccountSettings.CryptedGUID_files, CloudSettings.AccountSettings.encrypt_filenames);
			if self.FileCipher.WrongPassword then
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_WRONG_ENCRYPT_PASSWORD);

			self.crypt_files := not(self.FileCipher.WrongPassword);
			self.crypt_filenames := self.crypt_files and CloudSettings.AccountSettings.encrypt_filenames and not(self.FileCipher.WrongPassword);
		end;

		self.public_link := getPublicLink;

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
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	self.HTTP.SetProgressNames(CREATE_DIRECTORY, Path);
	result := self.HTTP.PostForm(API_FOLDER_ADD, Format('home=/%s%s&conflict', [PathToUrl(Path), self.united_params]), JSON) and CloudResultToBoolean(JSON);
	if (not result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		result := self.createDir(Path);
end;

function TCloudMailRu.deleteFile(Path: WideString): Boolean;
var
	JSON: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	self.HTTP.SetProgressNames(DELETE_FILE, Path);
	result := self.HTTP.PostForm(API_FILE_REMOVE, Format('home=/%s%s&conflict', [PathToUrl(Path), self.united_params]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_DELETE_FILE);
	if (not result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		result := self.deleteFile(Path);
end;

destructor TCloudMailRu.Destroy;
begin
	//self.HTTP.Destroy;

	self.AuthCookie.Destroy;
	//self.TCloudMailRuJSONParser.Destroy;
	if Assigned(InternalHTTPConnection) then
		InternalHTTPConnection.Destroy;

	if Assigned(self.FileCipher) then
		self.FileCipher.Destroy;
	inherited;
end;

class function TCloudMailRu.ErrorCodeText(ErrorCode: integer): WideString;
begin
	case ErrorCode of
		CLOUD_ERROR_EXISTS:
			exit(ERR_CLOUD_ERROR_EXISTS);
		CLOUD_ERROR_REQUIRED:
			exit(ERR_CLOUD_ERROR_REQUIRED);
		CLOUD_ERROR_INVALID:
			exit(ERR_CLOUD_ERROR_INVALID);
		CLOUD_ERROR_READONLY:
			exit(ERR_CLOUD_ERROR_READONLY);
		CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
			exit(ERR_CLOUD_ERROR_NAME_LENGTH_EXCEEDED);
		CLOUD_ERROR_OVERQUOTA:
			exit(ERR_CLOUD_ERROR_OVERQUOTA);
		CLOUD_ERROR_NOT_EXISTS:
			exit(ERR_CLOUD_ERROR_NOT_EXISTS);
		CLOUD_ERROR_OWN:
			exit(ERR_CLOUD_ERROR_OWN);
		CLOUD_ERROR_NAME_TOO_LONG:
			exit(ERR_CLOUD_ERROR_NAME_TOO_LONG);
		CLOUD_ERROR_VIRUS_SCAN_FAIL:
			exit(ERR_CLOUD_ERROR_VIRUS_SCAN_FAIL);
		CLOUD_ERROR_OWNER:
			exit(ERR_CLOUD_ERROR_OWNER);
		CLOUD_ERROR_FAHRENHEIT:
			exit(ERR_CLOUD_ERROR_FAHRENHEIT);
		CLOUD_ERROR_BAD_REQUEST:
			exit(ERR_CLOUD_ERROR_BAD_REQUEST);
		CLOUD_ERROR_TREES_CONFLICT:
			exit(ERR_CLOUD_ERROR_TREES_CONFLICT);
		CLOUD_ERROR_UNPROCESSABLE_ENTRY:
			exit(ERR_CLOUD_ERROR_UNPROCESSABLE_ENTRY);
		CLOUD_ERROR_USER_LIMIT_EXCEEDED:
			exit(ERR_CLOUD_ERROR_USER_LIMIT_EXCEEDED);
		CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED:
			exit(ERR_CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED);
		CLOUD_ERROR_NOT_ACCEPTABLE:
			exit(ERR_CLOUD_ERROR_NOT_ACCEPTABLE);
		else
			exit(Format(ERR_CLOUD_ERROR_UNKNOWN, [ErrorCode]));
	end;
end;

function TCloudMailRu.FileIdentity(localPath: WideString): TCMRFileIdentity;
begin
	result.Hash := cloudHash(localPath);
	result.size := SizeOfFile(localPath);
end;

function TCloudMailRu.getDescriptionFile(remotePath, localCopy: WideString): Boolean;
var
	resultHash: WideString;
begin
	result := self.getFile(remotePath, localCopy, resultHash, false) = FS_FILE_OK;
end;

function TCloudMailRu.putDesriptionFile(remotePath, localCopy: WideString): Boolean;
begin
	if FileExists(localCopy) then
		result := self.putFile(localCopy, remotePath) = FS_FILE_OK
	else
		result := self.deleteFile(remotePath);
end;

function TCloudMailRu.getSharedLinksListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean = false): Boolean;
var
	JSON: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if self.public_account then
		exit;
	if (ShowProgress) then
		self.HTTP.SetProgressNames(SHARED_LINKS_LISTING, UNKNOWN_ITEM);

	result := self.HTTP.GetPage(Format('%s?%s', [API_FOLDER_SHARED_LINKS, self.united_params]), JSON, ShowProgress);
	if result then
		result := CloudResultToBoolean(JSON, PREFIX_ERR_SHARED_LINKS_LISTING) and getDirListing(JSON, DirListing)
	else
	begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			result := getSharedLinksListing(DirListing, ShowProgress);
	end;

end;

function TCloudMailRu.getIncomingLinksListing(var IncomingListing: TCloudMailRuIncomingInviteInfoListing; ShowProgress: Boolean): Boolean;
var
	JSON: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	SetLength(IncomingListing, 0);
	if self.public_account then
		exit;
	if (ShowProgress) then
		self.HTTP.SetProgressNames(INCOMING_LINKS_LISTING, UNKNOWN_ITEM);
	result := self.HTTP.GetPage(Format('%s?%s', [API_FOLDER_SHARED_INCOMING, self.united_params]), JSON, ShowProgress);

	if result then
		result := CloudResultToBoolean(JSON, PREFIX_ERR_INCOMING_REQUESTS_LISTING) and getIncomingInviteListing(JSON, IncomingListing)
	else
	begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			result := getIncomingLinksListing(IncomingListing, ShowProgress);
	end;
end;

function TCloudMailRu.getIncomingLinksListing(var IncomingListing: TCMRDirItemList; var InvitesListing: TCloudMailRuIncomingInviteInfoListing; ShowProgress: Boolean = false): Boolean;
var
	i: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	SetLength(IncomingListing, 0);
	result := self.getIncomingLinksListing(InvitesListing, ShowProgress);
	if result then
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
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if self.public_account then
		exit;
	if (ShowProgress) then
		self.HTTP.SetProgressNames(TRASH_LISTING, UNKNOWN_ITEM);
	result := self.HTTP.GetPage(Format('%s?%s', [API_TRASHBIN, self.united_params]), JSON, ShowProgress);

	if result then
		result := CloudResultToBoolean(JSON, PREFIX_ERR_TRASH_LISTING) and getDirListing(JSON, DirListing)
	else
	begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			result := getTrashbinListing(DirListing, ShowProgress);
	end;

end;

function TCloudMailRu.getDirListing(Path: WideString; var DirListing: TCMRDirItemList; ShowProgress: Boolean = false): Boolean;
var
	JSON: WideString;
	OperationResult: TCMROperationResult;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if self.public_account then
		result := self.HTTP.GetPage(Format('%s&weblink=%s%s%s', [API_FOLDER, IncludeSlash(self.public_link), PathToUrl(Path, false), self.united_params]), JSON, ShowProgress)
	else
	begin
		self.HTTP.SetProgressNames(DIR_LISTING, Path);
		result := self.HTTP.GetPage(Format('%s&home=%s%s', [API_FOLDER, PathToUrl(Path), self.united_params]), JSON, ShowProgress);
	end;
	if result then
	begin
		OperationResult.FromJSON(JSON);
		result := CloudResultToBoolean(OperationResult, PREFIX_ERR_DIR_LISTING);
		if result then
		begin
			result := DirListing.FromJSON(JSON);
			if result and self.crypt_filenames then
				self.FileCipher.DecryptDirListing(DirListing);
		end else if OperationResult.OperationResult = CLOUD_ERROR_NOT_EXISTS then
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_ERR_PATH_NOT_EXISTS, Path]);
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			result := getDirListing(Path, DirListing, ShowProgress);
	end;

end;

function TCloudMailRu.getFile(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean): integer;
begin
	result := FS_FILE_NOTSUPPORTED;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации

	self.HTTP.SetProgressNames(remotePath, localPath);
	if self.public_account then
		result := self.getFileShared(remotePath, localPath, resultHash, LogErrors)
	else
		result := self.getFileRegular(remotePath, localPath, resultHash, LogErrors);

end;

function TCloudMailRu.getFileRegular(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean): integer;
var
	FileStream: TBufferedFileStream;
	URL, FileName: WideString;
	MemoryStream: TMemoryStream;
begin
	result := FS_FILE_NOTSUPPORTED;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.Shard = EmptyWideStr then
	begin
		Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, UNDEFINED_DOWNLOAD_SHARD);
		if not self.getShard(self.Shard) then
			exit;
	end;
	if self.crypt_filenames then
	begin
		FileName := ExtractUniversalFileName(remotePath);
		FileName := FileCipher.DecryptFileName(FileName);
		localPath := ChangePathFileName(localPath, FileName);
	end;

	try
		FileStream := TBufferedFileStream.Create(GetUNCFilePath(localPath), fmCreate);
	except
		on E: Exception do
		begin
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, E.Message);
			exit(FS_FILE_WRITEERROR);
		end;
	end;

	if self.crypt_files then //Загрузка файла в память, дешифрация в файл
	begin
		MemoryStream := TMemoryStream.Create;
		URL := Format('%s%s', [self.Shard, PathToUrl(remotePath, false)]);
		result := self.HTTP.getFile(URL, MemoryStream, LogErrors);
		if (CLOUD_ERROR_TOKEN_OUTDATED = result) and RefreshCSRFToken() then
			result := self.getFileRegular(remotePath, localPath, resultHash, LogErrors);

		if result in [FS_FILE_NOTSUPPORTED] then //this code returned on shard connection error
		begin
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_REDIRECTION_LIMIT, URL]);
			if (Request.Request(RT_MsgYesNo, REDIRECTION_LIMIT, TRY_ANOTHER_SHARD, EmptyWideStr, 0)) and (self.getShard(self.Shard)) then
				result := self.getFileRegular(remotePath, localPath, resultHash, LogErrors);
		end;

		if result in [FS_FILE_OK] then
		begin
			resultHash := cloudHash(MemoryStream);
			MemoryStream.Position := 0;
			self.FileCipher.DecryptStream(MemoryStream, FileStream);
		end;
		MemoryStream.free;

	end else begin
		result := self.HTTP.getFile(Format('%s%s', [self.Shard, PathToUrl(remotePath, false)]), FileStream, LogErrors);
		if (CLOUD_ERROR_TOKEN_OUTDATED = result) and RefreshCSRFToken() then
			result := self.getFileRegular(remotePath, localPath, resultHash, LogErrors);
		if ((result in [FS_FILE_OK]) and (EmptyWideStr = resultHash)) then
			resultHash := cloudHash(FileStream);
	end;

	FlushFileBuffers(FileStream.Handle);
	FileStream.free;

	if not(result in [FS_FILE_OK]) then
		System.SysUtils.deleteFile(GetUNCFilePath(localPath));
end;

{since 29.07.2022: изменена логика получения ссылок, см. issue #285. URL теперь всегда должны быть кодированы, иначе в некоторых случаях приходит 400}
function TCloudMailRu.getSharedFileUrl(remotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
var
	usedShard: WideString;
	ProgressEnabled: Boolean;
begin
	if ShardType = SHARD_TYPE_DEFAULT then
		usedShard := self.public_shard
	else
		self.getShard(usedShard, ShardType);
	if (self.public_account) then
		exit(Format('%s%s%s', [IncludeSlash(usedShard), IncludeSlash(self.public_link), PathToUrl(remotePath, true, true)]));

	if (TRealPath.GetRealPath(remotePath).isDir = ID_Yes) then {для ссылок внутри каталогов перебираются файлы внутри «публичной ссылки» на каталог}
	begin
		result := Format('%s%s%s', [IncludeSlash(usedShard), self.public_link, PathToUrl(remotePath, true, true)]);
	end else begin {для прямых ссылок берутся публичные ссылки файлов}
		result := Format('%s%s%s', [IncludeSlash(usedShard), self.public_link])
	end;

	ProgressEnabled := false;
	InternalHTTPConnection.GetRedirection(result, result, ProgressEnabled);

end;

function TCloudMailRu.getFileShared(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean): integer;
var
	FileStream: TBufferedFileStream;
begin
	result := FS_FILE_NOTFOUND;
	if (self.public_shard = EmptyWideStr) then
		exit;
	try
		FileStream := TBufferedFileStream.Create(GetUNCFilePath(localPath), fmCreate);
	except
		on E: Exception do
		begin
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, E.Message);
			exit(FS_FILE_WRITEERROR);
		end;
	end;
	if (Assigned(FileStream)) then
	begin
		result := self.HTTP.getFile(getSharedFileUrl(remotePath), FileStream, LogErrors);
		if ((result in [FS_FILE_OK]) and (EmptyWideStr = resultHash)) then
			resultHash := cloudHash(FileStream);
		FlushFileBuffers(FileStream.Handle);
		FileStream.free;
	end;
	if result <> FS_FILE_OK then
		System.SysUtils.deleteFile(GetUNCFilePath(localPath));
end;

function TCloudMailRu.getHTTPConnection: TCloudMailRuHTTP;
begin
	if not(Assigned(self)) then
		exit(nil); //Проверка на вызов без инициализации
	if (nil = self.HTTPConnectionsManager) then
	begin
		if not Assigned(InternalHTTPConnection) then
			self.InternalHTTPConnection := TCloudMailRuHTTP.Create(OptionsSet.ConnectionSettings, Progress, Logger);

		result := self.InternalHTTPConnection;
	end
	else
		result := self.HTTPConnectionsManager.get(GetCurrentThreadID());
	result.AuthCookie := self.AuthCookie;
	if EmptyWideStr <> AuthToken then
		result.HTTP.Request.CustomHeaders.Values['X-CSRF-Token'] := AuthToken;
end;

function TCloudMailRu.getOAuthToken(var OAuthToken: TCMROAuth): Boolean;
var
	Answer: WideString;
begin
	result := false;
	if self.HTTP.PostForm(OAUTH_TOKEN_URL, Format('client_id=cloud-win&grant_type=password&username=%s@%s&password=%s', [self.user, self.domain, UrlEncode(self.password)]), Answer) then
	begin
		if not OAuthToken.FromJSON(Answer) then
			exit(false);
		result := OAuthToken.error_code = NOERROR;
	end;
end;

function TCloudMailRu.getPublicLink: WideString;
begin
	result := EmptyWideStr;
	if self.public_account and (self.OptionsSet.AccountSettings.public_url <> EmptyWideStr) then
	begin
		result := self.OptionsSet.AccountSettings.public_url;
		self.OptionsSet.AccountSettings.public_url := IncludeSlash(self.OptionsSet.AccountSettings.public_url);
		Delete(result, 1, length(PUBLIC_ACCESS_URL));
		if (result <> EmptyWideStr) and (result[length(result)] = '/') then
			Delete(result, length(result), 1);
	end;
end;

function TCloudMailRu.getPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; publish: Boolean = CLOUD_PUBLISH): Boolean;
var
	shard_url: WideString;
begin
	result := false;
	if (EmptyWideStr = FileIdentity.weblink) then //publish and fill weblink, if required
	begin
		if (not publish) or (not self.publishFile(FileIdentity.home, FileIdentity.weblink)) then
			exit;
	end;

	if not self.getShard(shard_url, ShardType) then
		exit;
	StreamUrl := Format('%s0p/%s.m3u8?double_encode=1', [shard_url, DCPbase64.Base64EncodeStr(String(RawByteString(FileIdentity.weblink)))]); //UTF2Ansi is required
	result := true;
end;

function TCloudMailRu.getShard(var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean;
var
	JSON: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.shard_override <> EmptyWideStr then
	begin
		Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_DETAILS, SHARD_OVERRIDDEN);
		Shard := self.shard_override;
		exit(true);
	end;
	result := self.HTTP.PostForm(API_DISPATCHER, self.united_params, JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_SHARD_RECEIVE);
	if result then
	begin
		result := JSONHelper.getShard(JSON, Shard, ShardType) and (Shard <> EmptyWideStr);
		Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PREFIX_SHARD_RECEIVED, [Shard, ShardType]);
	end;
end;

function TCloudMailRu.initConnectionParameters: Boolean;
var
	JSON: WideString;
	Progress: Boolean;
	x_page_id, build: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	result := self.HTTP.GetPage(TOKEN_HOME_URL, JSON, Progress);
	if result then
	begin
		{При первоначальной инициализации получаем токен из страницы ответа, затем он обновляется по необходимости}
		result := extractTokenFromText(JSON, AuthToken) and extract_x_page_id_FromText(JSON, x_page_id) and extract_build_FromText(JSON, build); //and extract_upload_url_FromText(JSON, self.upload_url);
		self.united_params := Format('&api=2&build=%s&x-page-id=%s&email=%s@%s&x-email=%s@%s&_=%d810', [build, x_page_id, self.user, self.domain, self.user, self.domain, DateTimeToUnix(now)]);
	end;
end;

function TCloudMailRu.RefreshCSRFToken: Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	self.HTTP.GetPage(API_CSRF, JSON, Progress);
	result := getBodyToken(JSON, AuthToken);
	if result then
		Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, TOKEN_UPDATED)
	else
		Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_TOKEN_UPDATE)
end;

function TCloudMailRu.initSharedConnectionParameters(): Boolean;
var
	PageContent: WideString;
	Progress: Boolean;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	result := self.HTTP.GetPage(self.OptionsSet.AccountSettings.public_url, PageContent, Progress);
	if result then
	begin
		if not extractPublicShard(PageContent, self.public_shard) then
		begin
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_GET_PUBLIC_SHARE);
			exit(false);
		end;
	end;
end;

function TCloudMailRu.getUserSpace(var SpaceInfo: TCMRSpace): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	result := self.HTTP.GetPage(Format('%s?home=/%s', [API_USER_SPACE, self.united_params]), JSON, Progress);
	if result then
	begin
		result := CloudResultToBoolean(JSON, PREFIX_ERR_GET_USER_SPACE) and SpaceInfo.FromJSON(JSON);
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			result := getUserSpace(SpaceInfo)
	end;
end;

function TCloudMailRu.login(method: integer): Boolean;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	HTTP.SetProgressNames(LOGIN_IN_PROGRESS, EmptyWideStr);
	if self.public_account then
		result := self.loginShared()
	else
	begin
		result := self.loginRegular(method);
		if (result and (EmptyWideStr <> self.upload_url_override)) then
		begin
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_DETAILS, UPLOAD_URL_OVERRIDDEN);
			self.upload_url := self.upload_url_override;
			exit(true);
		end;
	end;
end;

function TCloudMailRu.loginRegular(method: integer): Boolean;
var
	PostAnswer: WideString;
	TwoStepJson: WideString;
	AuthMessage: WideString;
	TwostepData: TCMRTwostep;
	SecurityKey: WideString;
	FormFields: TDictionary<WideString, WideString>;
begin
	result := false;

	Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, Format(LOGIN_TO, [self.email]));
	case method of
		CLOUD_AUTH_METHOD_TWO_STEP:
			begin
				FormFields := TDictionary<WideString, WideString>.Create();
				FormFields.AddOrSetValue('Domain', self.domain);
				FormFields.AddOrSetValue('Login', self.user);
				FormFields.AddOrSetValue('Password', self.password);
				Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, REQUESTING_FIRST_STEP_AUTH_TOKEN, [self.email]);
				result := self.HTTP.PostMultipart(LOGIN_URL, FormFields, PostAnswer);
				if result then
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
							FormFields.AddOrSetValue('Login', self.email);
							FormFields.AddOrSetValue('csrf', TwostepData.csrf);
							FormFields.AddOrSetValue('AuthCode', SecurityKey);
							Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, SECOND_STEP_AUTH);
							result := self.HTTP.PostMultipart(SECSTEP_URL, FormFields, PostAnswer);
							FormFields.free;
							if result then
							begin
								result := self.initConnectionParameters();
								if (result) then
								begin
									Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, CONNECTED_TO, [self.email]);
									self.logUserSpaceInfo;
								end else begin
									Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_TWOSTEP_AUTH);
								end;
							end;
						end else begin
							Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_SECURITY_KEY);
							exit(false);
						end;

					end else begin
						Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARSE_AUTH_DATA);
						exit(false);
					end;

				end else begin
					Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_GET_FIRST_STEP_AUTH_TOKEN, [self.email]);
					FormFields.free;
				end;
			end;
		CLOUD_AUTH_METHOD_WEB: //todo: вынести в отдельный метод
			begin
				Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, REQUESTING_AUTH_TOKEN, [self.email]);
				result := self.HTTP.PostForm(LOGIN_URL, Format('page=https://cloud.mail.ru/?new_auth_form=1&Domain=%s&Login=%s&Password=%s&FailPage=', [self.domain, self.user, UrlEncode(self.password)]), PostAnswer);
				if (result) then
				begin
					Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PARSING_TOKEN_DATA);
					result := self.initConnectionParameters();
					if (result) then
					begin
						Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, CONNECTED_TO, [self.email]);
						self.logUserSpaceInfo;
					end else begin
						Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARSING_AUTH_TOKEN, [self.email]);
						exit(false);
					end;
				end
				else
					Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_GET_AUTH_TOKEN, [self.email]);
			end;
		CLOUD_AUTH_METHOD_OAUTH:
			begin
				result := self.getOAuthToken(self.OAuthToken);
				if not result then
					Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, PREFIX_ERR_OAUTH, [self.OAuthToken.error, self.OAuthToken.error_description]);
			end;
	end;
end;

function TCloudMailRu.loginShared(method: integer): Boolean;
begin
	Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, URL_OPEN, [self.OptionsSet.AccountSettings.public_url]);
	result := self.initSharedConnectionParameters();
	//exit(true);
end;

procedure TCloudMailRu.logUserSpaceInfo;
var
	US: TCMRSpace;
	QuotaInfo: WideString;
begin
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if self.getUserSpace(US) then
	begin
		if (US.overquota) then
			QuotaInfo := WARN_QUOTA_EXHAUSTED
		else
			QuotaInfo := EmptyWideStr;
		Logger.Log(LOG_LEVEL_FILE_OPERATION, MSGTYPE_DETAILS, USER_SPACE_INFO, [FormatSize(US.total), FormatSize(US.used), FormatSize(US.total - US.used), QuotaInfo]);
	end else begin
		Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_IMPORTANTERROR, ERR_GET_USER_SPACE, [self.email]);
	end;
end;

function TCloudMailRu.moveFile(OldName, ToPath: WideString): integer;
var
	JSON: WideString;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);
	if self.HTTP.PostForm(API_FILE_MOVE, Format('home=%s&folder=%s%s&conflict', [PathToUrl(OldName), PathToUrl(ToPath), self.united_params]), JSON) then
		result := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_MOVE);
	if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
		result := self.moveFile(OldName, ToPath);
end;

function TCloudMailRu.mvFile(OldName, NewName: WideString): integer;
var
	NewPath: WideString;
	SameDir, SameName: Boolean;
begin //К сожалению, переименование и перемещение в облаке - разные действия
	NewPath := ExtractFilePath(NewName);
	SameDir := ExtractFilePath(OldName) = ExtractFilePath(NewName);
	SameName := ExtractFileName(OldName) = ExtractFileName(NewName);
	if SameDir then
	begin //один каталог
		result := self.renameFile(OldName, ExtractFileName(NewName));
	end else begin
		result := self.moveFile(OldName, ExtractFilePath(NewName)); //Если файл со старым именем лежит в новом каталоге, вернётся ошибка. Так реализовано в облаке, а мудрить со временными каталогами я не хочу
		if result <> CLOUD_OPERATION_OK then
			exit;
		if not(SameName) then
		begin //скопированный файл лежит в новом каталоге со старым именем
			result := self.renameFile(Format('%s%s', [NewPath, ExtractFileName(OldName)]), ExtractFileName(NewName));
		end;
	end;
end;

function TCloudMailRu.publishFile(Path: WideString; var PublicLink: WideString; publish: Boolean): Boolean;
var
	JSON: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if publish then
	begin
		result := self.HTTP.PostForm(API_FILE_PUBLISH, Format('home=/%s%s&conflict', [PathToUrl(Path), self.united_params]), JSON, 'application/x-www-form-urlencoded', true, false);
	end else begin
		result := self.HTTP.PostForm(API_FILE_UNPUBLISH, Format('weblink=%s%s&conflict', [PublicLink, self.united_params]), JSON, 'application/x-www-form-urlencoded', true, false);
	end;

	if result then
		result := CloudResultToBoolean(JSON, PREFIX_ERR_FILE_PUBLISH);

	if result and publish then
		result := JSONHelper.getPublicLink(JSON, PublicLink);
	if (not result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		result := self.publishFile(Path, PublicLink, publish);
end;

function TCloudMailRu.getShareInfo(Path: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	if self.HTTP.GetPage(Format('%s?home=%s%s', [API_FOLDER_SHARED_INFO, PathToUrl(Path), self.united_params]), JSON, Progress) then
	begin
		result := getInviteListing(JSON, InviteListing);
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			result := getShareInfo(Path, InviteListing);
	end;
end;

function TCloudMailRu.shareFolder(Path, email: WideString; access: integer): Boolean;
var
	JSON: WideString;
	access_string: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if access in [CLOUD_SHARE_RW, CLOUD_SHARE_RO] then
	begin
		if access = CLOUD_SHARE_RW then
			access_string := CLOUD_SHARE_ACCESS_READ_WRITE
		else
			access_string := CLOUD_SHARE_ACCESS_READ_ONLY;

		result := self.HTTP.PostForm(API_FOLDER_SHARE, Format('home=/%s%s&invite={"email":"%s","access":"%s"}', [PathToUrl(Path), self.united_params, email, access_string]), JSON)
	end else begin
		result := self.HTTP.PostForm(API_FOLDER_UNSHARE, Format('home=/%s%s&invite={"email":"%s"}', [PathToUrl(Path), self.united_params, email]), JSON);
	end;
	if result then
		result := CloudResultToBoolean(JSON, PREFIX_ERR_INVITE_MEMBER);
	if (not result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		result := self.shareFolder(Path, email, access);
end;

function TCloudMailRu.trashbinRestore(Path: WideString; RestoreRevision: integer; ConflictMode: WideString): Boolean;
var
	JSON: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	result := self.HTTP.PostForm(API_TRASHBIN_RESTORE, Format('path=%s&restore_revision=%d%s&conflict=%s', [PathToUrl(Path), RestoreRevision, self.united_params, ConflictMode]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_FILE_RESTORE);
	if (not result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		result := self.trashbinRestore(Path, RestoreRevision, ConflictMode);
end;

function TCloudMailRu.trashbinEmpty(): Boolean;
var
	JSON: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;

	result := self.HTTP.PostForm(API_TRASHBIN_EMPTY, self.united_params, JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_TRASH_CLEAN);
	if (not result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		result := self.trashbinEmpty();
end;

function TCloudMailRu.mountFolder(home, invite_token, ConflictMode: WideString): Boolean;
var
	JSON: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	result := self.HTTP.PostForm(API_FOLDER_MOUNT, Format('home=%s&invite_token=%s%s&conflict=%s', [UrlEncode(home), invite_token, self.united_params, ConflictMode]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_FOLDER_MOUNT);
	if (not result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		result := self.mountFolder(home, invite_token, ConflictMode);
end;

function TCloudMailRu.unmountFolder(home: WideString; clone_copy: Boolean): Boolean;
var
	JSON: WideString;
	CopyStr: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if clone_copy then
		CopyStr := 'true'
	else
		CopyStr := 'false';
	result := self.HTTP.PostForm(API_FOLDER_UNMOUNT, Format('home=%s&clone_copy=%s%s', [UrlEncode(home), CopyStr, self.united_params]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_FOLDER_UNMOUNT);
	if (not result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		result := self.unmountFolder(home, clone_copy);
end;

function TCloudMailRu.rejectInvite(invite_token: WideString): Boolean;
var
	JSON: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	result := self.HTTP.PostForm(API_INVITE_REJECT, Format('invite_token=%s%s', [invite_token, self.united_params]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_INVITE_REJECT);
	if (not result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		result := self.rejectInvite(invite_token);
end;

function TCloudMailRu.putFileStream(FileName, remotePath: WideString; FileStream: TStream; ConflictMode: WideString): integer;
var
	LocalFileIdentity, RemoteFileIdentity: TCMRFileIdentity;
	OperationResult: integer;
	MemoryStream: TMemoryStream;
	UseHash: Boolean;
begin

	result := FS_FILE_WRITEERROR;
	OperationResult := CLOUD_OPERATION_FAILED;

	UseHash := self.PrecalculateHash or (self.ForcePrecalculateSize >= FileStream.size); //issue #231

	if UseHash or self.CheckCRC then
	begin
		LocalFileIdentity.Hash := cloudHash(FileStream);
		LocalFileIdentity.size := FileStream.size;
	end;
	if UseHash and (LocalFileIdentity.Hash <> EmptyWideStr) and (not self.crypt_files) and (FS_FILE_OK = self.addFileByIdentity(LocalFileIdentity, remotePath, CLOUD_CONFLICT_STRICT, false, true)) then {issue #135}
		exit(CLOUD_OPERATION_OK);

	try
		if self.crypt_files then {Will encrypt any type of data passed here}
		begin
			MemoryStream := TMemoryStream.Create;
			self.FileCipher.CryptStream(FileStream, MemoryStream);
			MemoryStream.Position := 0;
			OperationResult := self.putFileToCloud(FileName, MemoryStream, RemoteFileIdentity);
			MemoryStream.Destroy;
		end else begin
			OperationResult := self.putFileToCloud(FileName, FileStream, RemoteFileIdentity)
		end;
	Except
		on E: Exception do
		begin
			if E.ClassName = 'EAbort' then
			begin
				result := FS_FILE_USERABORT;
			end else begin
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_UPLOAD_INFO, [E.ClassName, E.Message]);
				result := FS_FILE_WRITEERROR;
			end;
		end;
	end;
	if OperationResult = CLOUD_OPERATION_OK then
	begin
		if self.CheckCRC then
		begin
			if not IsSameIdentity(LocalFileIdentity, RemoteFileIdentity) then {При включённой проверке CRC сравниваем хеши и размеры}
				result := CLOUD_OPERATION_FAILED;

		end;
	end else if OperationResult = CLOUD_OPERATION_CANCELLED then
	begin
		result := FS_FILE_USERABORT;
	end;

	if OperationResult = CLOUD_OPERATION_OK then
		result := self.addFileByIdentity(RemoteFileIdentity, remotePath, ConflictMode, false); //Не логируем HTTP-ошибку, она распарсится и обработается уровнем выше
end;

function TCloudMailRu.putFileWhole(localPath, remotePath, ConflictMode: WideString): integer;
var
	FileStream: TBufferedFileStream;
begin
	FileStream := TBufferedFileStream.Create(GetUNCFilePath(localPath), fmOpenRead or fmShareDenyWrite);
	result := self.putFileStream(ExtractFileName(remotePath), remotePath, FileStream, ConflictMode); {putFileStream может обойтись без параметра имени - оно всегда берётся из remotePath}
	FileStream.free;
end;

{$WARN NO_RETVAL OFF}
(*
 The W1035 compiler warning could be a false positive in this case
 BUT this code needs to be covered with tests and perhaps refactored due its length and complexity.
*)

function TCloudMailRu.putFileSplit(localPath, remotePath, ConflictMode: WideString; ChunkOverwriteMode: integer): integer;
var
	LocalFileIdentity: TCMRFileIdentity;
	SplitFileInfo: TFileSplitInfo;
	SplittedPartIndex: integer;
	ChunkRemotePath, CRCRemotePath: WideString;
	ChunkStream: TChunkedFileStream;
	CRCStream: TStringStream;
	RetryAttemptsCount: integer;
	UseHash: Boolean;
begin
	UseHash := self.PrecalculateHash or (self.ForcePrecalculateSize >= SizeOfFile(localPath)); //issue #231
	if UseHash then //try to add whole file by hash at first.
		LocalFileIdentity := FileIdentity(GetUNCFilePath(localPath));
	{Отмена расчёта хеша приведёт к отмене всей операции: TC запоминает нажатие отмены и ExternalProgressProc будет возвращать 1 до следующего вызова копирования}
	if UseHash and (LocalFileIdentity.Hash <> EmptyWideStr) and (not self.crypt_files) and (FS_FILE_OK = self.addFileByIdentity(LocalFileIdentity, remotePath, CLOUD_CONFLICT_STRICT, false, true)) then {issue #135}
		exit(CLOUD_OPERATION_OK);

	SplitFileInfo := TFileSplitInfo.Create(GetUNCFilePath(localPath), self.CloudMaxFileSize); //quickly get information about file parts
	RetryAttemptsCount := 0;
	SplittedPartIndex := 0;

	while SplittedPartIndex < SplitFileInfo.ChunksCount do {use while instead for..loop, need to modify loop counter sometimes}
	begin
		ChunkRemotePath := Format('%s%s', [ExtractFilePath(remotePath), SplitFileInfo.GetChunks[SplittedPartIndex].name]);
		self.HTTP.SetProgressNames(localPath, ChunkRemotePath);
		Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PARTIAL_UPLOAD_INFO, [localPath, (SplittedPartIndex + 1), SplitFileInfo.ChunksCount, ChunkRemotePath]);
		ChunkStream := TChunkedFileStream.Create(GetUNCFilePath(localPath), fmOpenRead or fmShareDenyWrite, SplitFileInfo.GetChunks[SplittedPartIndex].start, SplitFileInfo.GetChunks[SplittedPartIndex].size);
		result := self.putFileStream(ExtractFileName(ChunkRemotePath), ChunkRemotePath, ChunkStream, ConflictMode);
		ChunkStream.Destroy;

		case result of
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
									result := FS_FILE_WRITEERROR;
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
								result := FS_FILE_NOTSUPPORTED;
								Break;
							end;
					end;
				end;
			else {any other error}
				begin
					case OperationErrorMode of
						OperationErrorModeAsk:
							begin
								case (MsgBox(ERR_PARTIAL_UPLOAD_ASK, [result, ChunkRemotePath], ERR_UPLOAD, MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
									ID_ABORT:
										begin
											result := FS_FILE_USERABORT;
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
								Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_IGNORE, [result]);
							end;
						OperationErrorModeAbort:
							begin
								Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_ABORT, [result]);
								result := FS_FILE_USERABORT;
								Break;
							end;
						OperationErrorModeRetry:
							begin
								Inc(RetryAttemptsCount);
								if RetryAttemptsCount <> RetryAttempts + 1 then
								begin
									Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_RETRY, [result, RetryAttemptsCount, RetryAttempts]);
									Dec(SplittedPartIndex); //retry with this chunk
									ProcessMessages;
									Sleep(AttemptWait);
								end else begin
									Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_RETRY_EXCEED, [result]);
									result := CLOUD_OPERATION_FAILED;
									Break;
								end;
							end
						else {unknown option value}
							begin
								result := CLOUD_OPERATION_FAILED;
								Break;
							end;
					end
				end;
		end;
		Inc(SplittedPartIndex); //all ok, continue with next chunk
	end; {end while}

	if result = FS_FILE_OK then {Only after successful upload}
	begin
		CRCRemotePath := ExtractFilePath(remotePath) + SplitFileInfo.CRCFileName;
		self.HTTP.TargetName := CRCRemotePath;
		CRCStream := TStringStream.Create;
		SplitFileInfo.GetCRCData(CRCStream);
		self.putFileStream(SplitFileInfo.CRCFileName, CRCRemotePath, CRCStream, ConflictMode);
		CRCStream.Destroy;
	end;

	SplitFileInfo.Destroy;
	exit(FS_FILE_OK); //Файлик залит по частям, выходим
end;
{$WARN NO_RETVAL ON}

{Wrapper for putFileWhole/putFileSplit}
function TCloudMailRu.putFile(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: integer = 0): integer;
begin
	if not(Assigned(self)) then
		exit(FS_FILE_WRITEERROR); //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);
	self.HTTP.SetProgressNames(localPath, remotePath);
	if (not(self.unlimited_filesize)) and (SizeOfFile(GetUNCFilePath(localPath)) > self.CloudMaxFileSize) then
	begin
		if self.split_large_files then
		begin
			Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, SPLIT_LARGE_FILE, [self.CloudMaxFileSize]);
			exit(putFileSplit(localPath, remotePath, ConflictMode, ChunkOverwriteMode));
		end else begin
			Logger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, SPLIT_LARGE_FILE_IGNORE, [self.CloudMaxFileSize]);
			exit(FS_FILE_NOTSUPPORTED);
		end;
	end;

	result := putFileWhole(localPath, remotePath, ConflictMode);
end;

function TCloudMailRu.putFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCMRFileIdentity): integer;
var
	PostAnswer: WideString;
	return: TStringList;
	UploadUrl: WideString;
begin
	FileIdentity.Hash := EmptyWideStr;
	FileIdentity.size := -1;
	result := CLOUD_OPERATION_FAILED;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if (EmptyWideStr = self.upload_url) then
	begin
		Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, UNDEFINED_UPLOAD_SHARD);
		self.getShard(self.upload_url, SHARD_TYPE_UPLOAD);
	end;

	UploadUrl := Format('%s?cloud_domain=2&x-email=%s@%s', [self.upload_url, self.user, self.domain])(*+ '&fileapi' + DateTimeToUnix(now).ToString + '0246'*);
	return := TStringList.Create;
	//self.HTTP.OptionsMethod(UploadUrl, PostAnswer, ProgressEnabled); //not required at current moment, see issue #232
	result := self.HTTP.putFile(UploadUrl, FileName, FileStream, PostAnswer);
	if (CLOUD_ERROR_TOKEN_OUTDATED = result) and RefreshCSRFToken() then
		result := putFileToCloud(FileName, FileStream, FileIdentity);
	if (result = CLOUD_OPERATION_OK) then
	begin
		if length(PostAnswer) <> 40 then
		begin
			result := CLOUD_OPERATION_FAILED;
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
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	self.HTTP.SetProgressNames(DELETE_DIR, Path);
	result := self.HTTP.PostForm(API_FILE_REMOVE, Format('home=/%s%s&conflict', [IncludeSlash(PathToUrl(Path)), self.united_params]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_DELETE_DIR); //API всегда отвечает true, даже если путь не существует
	if (not result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		result := self.removeDir(Path);
end;

function TCloudMailRu.renameFile(OldName, NewName: WideString): integer;
var
	JSON: WideString;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if self.HTTP.PostForm(API_FILE_RENAME, Format('home=%s&name=%s%s', [PathToUrl(OldName), PathToUrl(NewName), self.united_params]), JSON) then
		result := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_RENAME);
	if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
		result := self.renameFile(OldName, NewName);
end;

function TCloudMailRu.statusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	if self.public_account then
		result := self.HTTP.GetPage(Format('%s?weblink=%s%s%s', [API_FILE, IncludeSlash(self.public_link), PathToUrl(Path), self.united_params]), JSON, Progress)
	else
		result := self.HTTP.GetPage(Format('%s?home=%s%s', [API_FILE, PathToUrl(Path), self.united_params]), JSON, Progress);
	if result then
	begin
		result := CloudResultToBoolean(JSON, PREFIX_ERR_FILE_STATUS) and FileInfo.FromJSON(JSON);
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			result := statusFile(Path, FileInfo);
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
		result := 'read only'
	else
		result := 'read and write';
end;

class function TCloudMailRu.StringToCloudAccess(accessString: WideString; Invert: Boolean): integer;
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
		result := CLOUD_SHARE_RO
	else
		result := CLOUD_SHARE_RW;
end;

class function TCloudMailRu.TempPublicCloudInit(var TempCloud: TCloudMailRu; publicUrl: WideString): Boolean;
var
	TempCloudSettings: TCloudSettings;
begin
	TempCloudSettings := default (TCloudSettings);
	TempCloudSettings.AccountSettings.public_account := true;
	TempCloudSettings.AccountSettings.public_url := publicUrl;
	TempCloud := TCloudMailRu.Create(TempCloudSettings, nil);
	result := TempCloud.login;
end;

function TCloudMailRu.cloudHash(Path: WideString): WideString;
var
	Stream: TStream;
begin
	result := EmptyWideStr;
	if not FileExists(Path) then
		exit;

	try
		Stream := TBufferedFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
	except
		exit;
	end;
	result := cloudHash(Stream, GetLFCFilePath(Path));
	Stream.Destroy;

end;

function TCloudMailRu.cloudHash(Stream: TStream; Path: WideString = CALCULATING_HASH): WideString;
const
	bufSize = 8192;
var
	sha1: THashSHA1;
	buffer: array [0 .. bufSize - 1] of byte;
	read, iteration, processedBytes: int64;
	initBuffer, finalBuffer: TBytes;
	Percent: integer;
	Aborted: Boolean;
begin
	Stream.Position := 0;
	result := EmptyWideStr;
	if Stream.size < 21 then
	begin
		SetLength(initBuffer, 20);
		Stream.read(initBuffer, Stream.size);
		result := UpperCase(THash.DigestAsString(initBuffer));
		exit;
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
		result := UpperCase(sha1.HashAsString);
	sha1.Reset;
end;

end.
