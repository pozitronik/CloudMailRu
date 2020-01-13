unit CloudMailRu;

interface

uses CMLJSON, CMLParsers, CMLTypes, CMLHTTP, System.Hash, System.Classes, System.Generics.Collections, System.SysUtils, PLUGIN_Types, Winapi.Windows, MRC_helper, Settings, Cipher, Splitfile, ChunkedFileStream, HTTPManager, IdCookieManager, DCPbase64;

type
	TCloudMailRu = class
	private
		{VARIABLES}
		OptionsSet: TCloudSettings;
		HTTPConnectionsManager: THTTPManager;
		InternalHTTPConnection: TCloudMailRuHTTP; //Если не задан HTTPConnectionsManager, класс создаст своё атомарное соединение

		AuthCookie: TIdCookieManager; //Авторизационная кука - должна храниться отдельно от HTTP-соединения, т.к. ассоциируется с облаком. Передаётся в менеджер HTTP-подключений внутри ConnetionManager

		ExternalProgressProc: TProgressHandler;
		ExternalLogProc: TLogHandler;
		ExternalRequestProc: TRequestHandler;

		FileCipher: TFileCipher; //Encryption class
		//JSONParser: TCloudMailRuJSONParser; //JSON parser

		public_link: WideString; //public_ params is active for public clouds only
		public_download_token: WideString; //token for public urls, refreshes on request
		public_shard: WideString; //public downloads shard url
		Shard: WideString; //download shard url

		OAuthToken: TCloudMailRuOAuthInfo; {unused at this moment}

		upload_url: WideString; //stored upload url, filled on getToken()
		united_params: WideString; //Объединённый набор авторизационных параметров для подстановки в URL

		{HTTP REQUESTS WRAPPERS}
		function getToken(): Boolean;
		function getSharedToken(): Boolean;
		function getOAuthToken(var OAuthToken: TCloudMailRuOAuthInfo): Boolean;
		function getShard(var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean;
		function getUserSpace(var SpaceInfo: TCloudMailRuSpaceInfo): Boolean;
		function putFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCloudMailRuFileIdentity): integer; overload; //отправка на сервер данных из потока
		{PRIVATE UPLOAD METHODS CHAIN (CALLED FROM putFile())}
		function putFileWhole(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer; //Загрузка файла целиком
		function putFileSplit(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: integer = 0): integer; //Загрузка файла по частям
		function putFileStream(FileName, remotePath: WideString; FileStream: TStream; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer;

		{OTHER ROUTINES}
		procedure Log(LogLevel, MsgType: integer; LogString: WideString);
		function CloudResultToFsResult(CloudResult: TCloudMailRuOperationResult; ErrorPrefix: WideString = ''): integer;
		function CloudResultToBoolean(CloudResult: TCloudMailRuOperationResult; ErrorPrefix: WideString = ''): Boolean;
		function cloudHash(Path: WideString): WideString; overload; //get cloud hash for specified file
		function cloudHash(Stream: TStream; Path: WideString = 'Calculating cloud hash'): WideString; overload; //get cloud hash for data in stream
		function getHTTPConnection: TCloudMailRuHTTP;
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

		function getSharedFileUrl(remotePath: WideString; DoUrlEncode: Boolean = true; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;

		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(CloudSettings: TCloudSettings; ConnectionManager: THTTPManager; ExternalProgressProc: TProgressHandler = nil; ExternalLogProc: TLogHandler = nil; ExternalRequestProc: TRequestHandler = nil);
		destructor Destroy; override;
		{CLOUD INTERFACE METHODS}
		function login(method: integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function getDirListing(Path: WideString; var DirListing: TCloudMailRuDirListing; ShowProgress: Boolean = false): Boolean;
		function getSharedLinksListing(var DirListing: TCloudMailRuDirListing; ShowProgress: Boolean = false): Boolean;
		function getIncomingLinksListing(var IncomingListing: TCloudMailRuIncomingInviteInfoListing; ShowProgress: Boolean = false): Boolean; overload;
		function getIncomingLinksListing(var IncomingListing: TCloudMailRuDirListing; var InvitesListing: TCloudMailRuIncomingInviteInfoListing; ShowProgress: Boolean = false): Boolean; overload;
		function getTrashbinListing(var DirListing: TCloudMailRuDirListing; ShowProgress: Boolean = false): Boolean;
		function createDir(Path: WideString): Boolean;
		function removeDir(Path: WideString): Boolean;
		function statusFile(Path: WideString; var FileInfo: TCloudMailRuDirListingItem): Boolean;
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

		function addFileByIdentity(FileIdentity: TCloudMailRuFileIdentity; remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = false): integer; overload;
		function addFileByIdentity(FileIdentity: TCloudMailRuDirListingItem; remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = false): integer; overload;

		function getShareInfo(Path: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
		function shareFolder(Path, email: WideString; access: integer): Boolean;
		function trashbinRestore(Path: WideString; RestoreRevision: integer; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function trashbinEmpty(): Boolean;
		function mountFolder(home, invite_token: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function unmountFolder(home: WideString; clone_copy: Boolean): Boolean;
		function rejectInvite(invite_token: WideString): Boolean;
		function getPublishedFileStreamUrl(FileIdentity: TCloudMailRuDirListingItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; publish: Boolean = CLOUD_PUBLISH): Boolean;
		{OTHER ROUTINES}
		function getDescriptionFile(remotePath, localCopy: WideString): Boolean; //Если в каталоге remotePath есть descript.ion - скопировать его в файл localcopy
		function putDesriptionFile(remotePath, localCopy: WideString): Boolean; //Скопировать descript.ion из временного файла на сервер
		procedure logUserSpaceInfo();
		function FileIdentity(localPath: WideString): TCloudMailRuFileIdentity;
		{STATIC ROUTINES}
		class function CloudAccessToString(access: WideString; Invert: Boolean = false): WideString; static;
		class function StringToCloudAccess(accessString: WideString; Invert: Boolean = false): integer; static;
		class function ErrorCodeText(ErrorCode: integer): WideString; static;
		class function IsSameIdentity(IdentityOne, IdentityTwo: TCloudMailRuFileIdentity): Boolean; static;
		class function TempPublicCloudInit(var TempCloud: TCloudMailRu; publicUrl: WideString): Boolean; static;
	end;

implementation

{TCloudMailRu}
function TCloudMailRu.addFileByIdentity(FileIdentity: TCloudMailRuFileIdentity; remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = false): integer;
var
	FileName: WideString;
	JSON: WideString;
	OperationResult: TCloudMailRuOperationResult;
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
	if self.HTTP.PostForm(API_FILE_ADD, 'conflict=' + ConflictMode + '&home=/' + PathToUrl(remotePath) + '&hash=' + FileIdentity.Hash + '&size=' + FileIdentity.size.ToString + self.united_params, JSON, 'application/x-www-form-urlencoded', LogErrors, false) then {Do not allow to cancel operation here}
	begin
		OperationResult := CMLJSONParser.getOperationResult(JSON);
		result := CloudResultToFsResult(OperationResult, 'File uploading error: ');
		if (CLOUD_OPERATION_OK = OperationResult.OperationResult) and LogSuccess then
			Log(LogLevelDetail, MSGTYPE_DETAILS, 'File ' + remotePath + ' found by hash.')

	end;
end;

function TCloudMailRu.addFileByIdentity(FileIdentity: TCloudMailRuDirListingItem; remotePath, ConflictMode: WideString; LogErrors, LogSuccess: Boolean): integer;
var
	CloudFileIdentity: TCloudMailRuFileIdentity;
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
	if self.HTTP.GetPage(API_CLONE + '?folder=' + PathToUrl(Path) + '&weblink=' + link + '&conflict=' + ConflictMode + self.united_params, JSON, Progress) then
	begin //Парсим ответ
		result := CloudResultToFsResult(CMLJSONParser.getOperationResult(JSON), 'File publish error: ');
		if (result <> FS_FILE_OK) and not(Progress) then
			result := FS_FILE_USERABORT; //user cancelled
	end;
end;

function TCloudMailRu.CloudResultToBoolean(CloudResult: TCloudMailRuOperationResult; ErrorPrefix: WideString): Boolean;
begin
	result := CloudResult.OperationResult = CLOUD_OPERATION_OK;
	if not(result) and (ErrorPrefix <> EmptyWideStr) then
		Log(LogLevelError, MSGTYPE_IMPORTANTERROR, ErrorPrefix + self.ErrorCodeText(CloudResult.OperationResult) + ' Status: ' + CloudResult.OperationStatus.ToString());
end;

function TCloudMailRu.CloudResultToFsResult(CloudResult: TCloudMailRuOperationResult; ErrorPrefix: WideString): integer;
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
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Insufficient Storage');
				exit(FS_FILE_WRITEERROR);
			end;
		CLOUD_ERROR_NAME_TOO_LONG:
			begin
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Name too long');
				exit(FS_FILE_WRITEERROR);
			end;
		else
			begin //что-то неизвестное
				if (ErrorPrefix <> EmptyWideStr) then
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, ErrorPrefix + self.ErrorCodeText(CloudResult.OperationResult) + ' Status: ' + CloudResult.OperationStatus.ToString());
				exit(FS_FILE_WRITEERROR);
			end;
	end;
end;

class function TCloudMailRu.IsSameIdentity(IdentityOne, IdentityTwo: TCloudMailRuFileIdentity): Boolean;
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
	self.HTTP.SetProgressNames(OldName, IncludeTrailingPathDelimiter(ToPath) + ExtractFileName(OldName));
	if self.HTTP.PostForm(API_FILE_COPY, 'home=' + PathToUrl(OldName) + '&folder=' + PathToUrl(ToPath) + self.united_params + '&conflict', JSON) then
	begin //Парсим ответ
		result := CloudResultToFsResult(CMLJSONParser.getOperationResult(JSON), 'File copy error: ');
	end;
end;

function TCloudMailRu.cpFile(OldName, NewName: WideString): integer;
var
	NewPath: WideString;
	SameDir, SameName: Boolean;
	//FileInfo:TCloudMailRuDirListingItem;
begin //Облако умеет скопировать файл, но не сможет его переименовать, поэтому хитрим
	NewPath := ExtractFilePath(NewName);
	SameDir := ExtractFilePath(OldName) = ExtractFilePath(NewName);
	SameName := ExtractFileName(OldName) = ExtractFileName(NewName);
	if (SameDir) then //копирование в тот же каталог не поддерживается напрямую, а мудрить со временными каталогами я не хочу
	begin
		Log(LogLevelWarning, MSGTYPE_IMPORTANTERROR, 'Copying in same dir not supported by cloud');
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
		result := self.renameFile(NewPath + ExtractFileName(OldName), ExtractFileName(NewName));
	end;
end;

constructor TCloudMailRu.Create(CloudSettings: TCloudSettings; ConnectionManager: THTTPManager; ExternalProgressProc: TProgressHandler; ExternalLogProc: TLogHandler; ExternalRequestProc: TRequestHandler);
begin
	try
		self.OptionsSet := CloudSettings;

		self.HTTPConnectionsManager := ConnectionManager;

		self.ExternalProgressProc := ExternalProgressProc;
		self.ExternalLogProc := ExternalLogProc;
		self.ExternalRequestProc := ExternalRequestProc;

		self.AuthCookie := TIdCookieManager.Create();

		//self.HTTP := TCloudMailRuHTTP.Create(CloudSettings.ConnectionSettings, ExternalProgressProc, ExternalLogProc);
		//self.JSONParser := TCloudMailRuJSONParser.Create();

		if CloudSettings.AccountSettings.encrypt_files_mode <> EncryptModeNone then
		begin
			self.FileCipher := TFileCipher.Create(CloudSettings.AccountSettings.crypt_files_password, CloudSettings.AccountSettings.CryptedGUID_files, CloudSettings.AccountSettings.encrypt_filenames);
			if self.FileCipher.WrongPassword then
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Wrong encryption password, encryption support disabled');

			self.crypt_files := not(self.FileCipher.WrongPassword);
			self.crypt_filenames := self.crypt_files and CloudSettings.AccountSettings.encrypt_filenames and not(self.FileCipher.WrongPassword);
		end;

		self.public_link := getPublicLink;

	except
		on E: Exception do
		begin
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Cloud initialization error: ' + E.Message);
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
	self.HTTP.SetProgressNames('Create directory', Path);
	result := self.HTTP.PostForm(API_FOLDER_ADD, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', JSON) and CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON))

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
	self.HTTP.SetProgressNames('Delete file', Path);
	result := self.HTTP.PostForm(API_FILE_REMOVE, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', JSON) and CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'Delete file error: ');
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
			exit('Объект с таким названием уже существует. Попробуйте другое название.');
		CLOUD_ERROR_REQUIRED:
			exit('Название папки не может быть пустым.');
		CLOUD_ERROR_INVALID:
			exit('Неправильное название папки. В названии папок нельзя использовать символы «" * / : < > ?  \\ |».');
		CLOUD_ERROR_READONLY:
			exit('Невозможно создать. Доступ только для просмотра.');
		CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
			exit('Превышена длина имени папки.');
		CLOUD_ERROR_OVERQUOTA:
			exit('Невозможно скопировать, в вашем Облаке недостаточно места.');
		CLOUD_ERROR_NOT_EXISTS:
			exit('Копируемая ссылка не существует.');
		CLOUD_ERROR_OWN:
			exit('Невозможно клонировать собственную ссылку.');
		CLOUD_ERROR_NAME_TOO_LONG:
			exit('Превышена длина имени файла.');
		CLOUD_ERROR_VIRUS_SCAN_FAIL:
			exit('Файл заражен вирусом');
		CLOUD_ERROR_OWNER:
			exit('Нельзя использовать собственный email');
		CLOUD_ERROR_FAHRENHEIT:
			exit('Невозможно создать ссылку. Публикация контента заблокирована по требованию правообладателя или уполномоченного государственного ведомства.');
		CLOUD_ERROR_BAD_REQUEST:
			exit('Ошибка запроса к серверу.');
		CLOUD_ERROR_TREES_CONFLICT:
			exit('Нельзя сделать папку общей, если она содержит другие общие папки или находится в общей папке');
		CLOUD_ERROR_UNPROCESSABLE_ENTRY:
			exit('Нельзя открыть доступ к файлу');
		CLOUD_ERROR_USER_LIMIT_EXCEEDED:
			exit('Невозможно добавить пользователя. Вы можете иметь не более 200 пользователей в одной общей папке ');
		CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED:
			exit('Невозможно добавить пользователя. Вы можете создать не более 50 общих папок');
		CLOUD_ERROR_NOT_ACCEPTABLE:
			exit('Нельзя добавить этого пользователя');
		else
			exit('Неизвестная ошибка (' + ErrorCode.ToString + ')');
	end;
end;

function TCloudMailRu.FileIdentity(localPath: WideString): TCloudMailRuFileIdentity;
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

function TCloudMailRu.getSharedLinksListing(var DirListing: TCloudMailRuDirListing; ShowProgress: Boolean = false): Boolean;
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
		self.HTTP.SetProgressNames('Shared links listing', '...');

	result := self.HTTP.GetPage(API_FOLDER_SHARED_LINKS + '?' + self.united_params, JSON, ShowProgress);
	if result then
		result := CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'Shared links listing error: ');
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
		self.HTTP.SetProgressNames('Incoming links listing', '...');
	result := self.HTTP.GetPage(API_FOLDER_SHARED_INCOMING + '?' + self.united_params, JSON, ShowProgress) and CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'Incoming requests listing error: ');
end;

function TCloudMailRu.getIncomingLinksListing(var IncomingListing: TCloudMailRuDirListing; var InvitesListing: TCloudMailRuIncomingInviteInfoListing; ShowProgress: Boolean = false): Boolean;
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

function TCloudMailRu.getTrashbinListing(var DirListing: TCloudMailRuDirListing; ShowProgress: Boolean): Boolean;
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
		self.HTTP.SetProgressNames('Trashbin listing', '...');
	result := self.HTTP.GetPage(API_TRASHBIN + '?' + self.united_params, JSON, ShowProgress);

	if result then
		result := CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'Trashbin listing error: ');

	if result then
		result := CMLJSONParser.getDirListing(JSON, DirListing);

end;

function TCloudMailRu.getDirListing(Path: WideString; var DirListing: TCloudMailRuDirListing; ShowProgress: Boolean = false): Boolean;
var
	JSON: WideString;
	OperationResult: TCloudMailRuOperationResult;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if self.public_account then
		result := self.HTTP.GetPage(API_FOLDER + '&weblink=' + IncludeSlash(self.public_link) + PathToUrl(Path, false) + self.united_params, JSON, ShowProgress)
	else
	begin
		self.HTTP.SetProgressNames('Directory listing:', Path);
		result := self.HTTP.GetPage(API_FOLDER + '&home=' + PathToUrl(Path) + self.united_params, JSON, ShowProgress);
	end;
	if result then
	begin
		OperationResult := CMLJSONParser.getOperationResult(JSON);
		result := CloudResultToBoolean(OperationResult, 'Directory listing error: ');
		if result then
		begin
			result := CMLJSONParser.getDirListing(JSON, DirListing);
			if result and self.crypt_filenames then
				self.FileCipher.DecryptDirListing(DirListing);
		end else if OperationResult.OperationResult = CLOUD_ERROR_NOT_EXISTS then
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Path not exists: ' + Path);
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
		Log(LogLevelDetail, MSGTYPE_DETAILS, 'Current shard is undefined, trying to get one');
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
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.Message);
			exit(FS_FILE_WRITEERROR);
		end;
	end;

	if self.crypt_files then //Загрузка файла в память, дешифрация в файл
	begin
		MemoryStream := TMemoryStream.Create;
		URL := self.Shard + PathToUrl(remotePath, false);
		result := self.HTTP.getFile(URL, MemoryStream, LogErrors);
		if result in [FS_FILE_NOTSUPPORTED] then //this code returned on shard connection error
		begin
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Redirection limit reached when trying to download ' + URL);
			if (self.ExternalRequestProc(RT_MsgYesNo, 'Redirection limit', 'Try with another shard?', '', 0)) and (self.getShard(self.Shard)) then
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
		result := self.HTTP.getFile(self.Shard + PathToUrl(remotePath, false), FileStream, LogErrors);
		if ((result in [FS_FILE_OK]) and (EmptyWideStr = resultHash)) then
			resultHash := cloudHash(FileStream);
	end;

	FlushFileBuffers(FileStream.Handle);
	FileStream.free;

	if not(result in [FS_FILE_OK]) then
		System.SysUtils.deleteFile(GetUNCFilePath(localPath));
end;

function TCloudMailRu.getSharedFileUrl(remotePath: WideString; DoUrlEncode: Boolean = true; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
var
	usedShard: WideString;
begin
	if ShardType = SHARD_TYPE_DEFAULT then
		usedShard := self.public_shard
	else
		self.getShard(usedShard, ShardType);

	result := IncludeSlash(usedShard) + IncludeSlash(self.public_link) + PathToUrl(remotePath, true, DoUrlEncode) + '?key=' + self.public_download_token
end;

function TCloudMailRu.getFileShared(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean): integer;
var
	FileStream: TBufferedFileStream;
begin
	result := FS_FILE_NOTFOUND;
	if (self.public_shard = EmptyWideStr) or (self.public_download_token = EmptyWideStr) then
		exit;
	try
		FileStream := TBufferedFileStream.Create(GetUNCFilePath(localPath), fmCreate);
	except
		on E: Exception do
		begin
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.Message);
			exit(FS_FILE_WRITEERROR);
		end;
	end;
	if (Assigned(FileStream)) then
	begin
		result := self.HTTP.getFile(getSharedFileUrl(remotePath), FileStream, LogErrors);
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
			self.InternalHTTPConnection := TCloudMailRuHTTP.Create(OptionsSet.ConnectionSettings, ExternalProgressProc, ExternalLogProc);

		result := self.InternalHTTPConnection;
	end
	else
		result := self.HTTPConnectionsManager.get(GetCurrentThreadID());
	result.AuthCookie := self.AuthCookie;
end;

function TCloudMailRu.getOAuthToken(var OAuthToken: TCloudMailRuOAuthInfo): Boolean;
var
	Answer: WideString;
begin
	result := false;
	if self.HTTP.PostForm(OAUTH_TOKEN_URL, 'client_id=cloud-win&grant_type=password&username=' + self.user + '%40' + self.domain + '&password=' + UrlEncode(self.password), Answer) then
	begin
		if not CMLJSONParser.getOAuthTokenInfo(Answer, OAuthToken) then
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

function TCloudMailRu.getPublishedFileStreamUrl(FileIdentity: TCloudMailRuDirListingItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; publish: Boolean = CLOUD_PUBLISH): Boolean;
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

	StreamUrl := shard_url + '0p/' + DCPbase64.Base64EncodeStr(RawByteString(FileIdentity.weblink)) + '.m3u8?double_encode=1'; //UTF2Ansi is required
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
		Log(LogLevelError, MSGTYPE_DETAILS, 'Shard overriden via config!');
		Shard := self.shard_override;
		exit(true);
	end;
	result := self.HTTP.PostForm(API_DISPATCHER, self.united_params, JSON) and CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'Shard receive error: ');
	if result then
	begin
		result := CMLJSONParser.getShard(JSON, Shard, ShardType) and (Shard <> EmptyWideStr);
		Log(LogLevelDetail, MSGTYPE_DETAILS, 'Shard received: ' + Shard);
	end;

end;

function TCloudMailRu.getToken: Boolean;
var
	JSON: WideString;
	Progress: Boolean;
	token, x_page_id, build: WideString;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	result := self.HTTP.GetPage(TOKEN_URL, JSON, Progress);
	if result then
	begin
		result := extractTokenFromText(JSON, token) and extract_x_page_id_FromText(JSON, x_page_id) and extract_build_FromText(JSON, build) and extract_upload_url_FromText(JSON, self.upload_url);
		self.united_params := '&api=2&build=' + build + '&x-page-id=' + x_page_id + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&token=' + token + '&_=' + DateTimeToUnix(now).ToString + '810';
	end;
end;

function TCloudMailRu.getSharedToken(): Boolean;
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
		if not extractPublicTokenFromText(PageContent, self.public_download_token) then //refresh public download token
		begin
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Can''t get public share download token');
			exit(false);
		end;
		if not extractPublicShard(PageContent, self.public_shard) then
		begin
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Can''t get public share download share');
			exit(false);
		end;
	end;
end;

function TCloudMailRu.getUserSpace(var SpaceInfo: TCloudMailRuSpaceInfo): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	result := self.HTTP.GetPage(API_USER_SPACE + '?home=/' + self.united_params, JSON, Progress) and CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'User space receiving error: ') and CMLJSONParser.getUserSpace(JSON, SpaceInfo);
end;

procedure TCloudMailRu.Log(LogLevel, MsgType: integer; LogString: WideString);
begin
	if Assigned(ExternalLogProc) then
		ExternalLogProc(LogLevel, MsgType, PWideChar(LogString));
end;

function TCloudMailRu.login(method: integer): Boolean;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	HTTP.SetProgressNames('Login to account...', '');
	if self.public_account then
		result := self.loginShared()
	else
	begin
		result := self.loginRegular(method);
		if (result and (EmptyWideStr <> self.upload_url_override)) then
		begin
			Log(LogLevelError, MSGTYPE_DETAILS, 'Upload url overriden via config!');
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
	TwostepData: TCloudMailRuTwostepData;
	SecurityKey: PWideChar;
	FormFields: TDictionary<WideString, WideString>;
begin
	result := false;

	Log(LogLevelDetail, MSGTYPE_DETAILS, 'Login to ' + self.user + '@' + self.domain);
	case method of
		CLOUD_AUTH_METHOD_TWO_STEP:
			begin
				FormFields := TDictionary<WideString, WideString>.Create();
				FormFields.AddOrSetValue('Domain', self.domain);
				FormFields.AddOrSetValue('Login', self.user);
				FormFields.AddOrSetValue('Password', self.password);
				Log(LogLevelDebug, MSGTYPE_DETAILS, 'Requesting first step auth token for ' + self.user + '@' + self.domain);
				result := self.HTTP.PostMultipart(LOGIN_URL, FormFields, PostAnswer);
				if result then
				begin
					Log(LogLevelDebug, MSGTYPE_DETAILS, 'Parsing authorization data...');
					if extractTwostepJson(PostAnswer, TwoStepJson) and CMLJSONParser.getTwostepData(TwoStepJson, TwostepData) then
					begin
						if TwostepData.secstep_timeout = AUTH_APP_USED then
							AuthMessage := 'Enter code from authentication app.' //mobile app used
						else if TwostepData.secstep_resend_fail = '1' then
							AuthMessage := 'SMS timeout to ' + TwostepData.secstep_phone + ' (' + TwostepData.secstep_timeout.ToString + ' sec).'
						else
							AuthMessage := 'Enter code sended to ' + TwostepData.secstep_phone + '.';

						Log(LogLevelDebug, MSGTYPE_DETAILS, 'Awaiting for security key... ');
						try
							SecurityKey := AllocMem(32);
						except
							on E: EOutOfMemory do
								exit(false);
						end;

						if (true = ExternalRequestProc(RT_Other, 'Enter auth key', PWideChar(AuthMessage), SecurityKey, 32)) then
						begin
							FormFields.Clear;
							FormFields.AddOrSetValue('Login', self.user + '@' + self.domain);
							FormFields.AddOrSetValue('csrf', TwostepData.csrf);
							FormFields.AddOrSetValue('AuthCode', SecurityKey);
							Log(LogLevelDebug, MSGTYPE_DETAILS, 'Performing second step auth...');
							result := self.HTTP.PostMultipart(SECSTEP_URL, FormFields, PostAnswer);
							FormFields.free;
							if result then
							begin
								result := self.getToken();
								if (result) then
								begin
									Log(LogLevelDetail, MSGTYPE_DETAILS, 'Connected to ' + self.user + '@' + self.domain);
									self.logUserSpaceInfo;
								end else begin
									Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'error: twostep auth failed');
									exit(false);
								end;
							end;
						end else begin
							Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'error: security key not provided');
							exit(false);
						end;
						FreeMem(SecurityKey);
					end else begin
						Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'error: parsing authorization data');
						exit(false);
					end;

				end else begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'error: getting first step auth token for ' + self.user + '@' + self.domain);
					FormFields.free;
				end;

			end;
		CLOUD_AUTH_METHOD_WEB: //todo: вынести в отдельный метод
			begin
				Log(LogLevelDebug, MSGTYPE_DETAILS, 'Requesting auth token for ' + self.user + '@' + self.domain);
				result := self.HTTP.PostForm(LOGIN_URL, 'page=https://cloud.mail.ru/?new_auth_form=1&Domain=' + self.domain + '&Login=' + self.user + '&Password=' + UrlEncode(self.password) + '&FailPage=', PostAnswer);
				if (result) then
				begin
					Log(LogLevelDebug, MSGTYPE_DETAILS, 'Parsing token data...');
					result := self.getToken();
					if (result) then
					begin
						Log(LogLevelDetail, MSGTYPE_DETAILS, 'Connected to ' + self.user + '@' + self.domain);
						self.logUserSpaceInfo;
					end else begin
						Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'error: parsing auth token for ' + self.user + '@' + self.domain);
						exit(false);
					end;
				end
				else
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'error: getting auth token for ' + self.user + '@' + self.domain);
			end;
		CLOUD_AUTH_METHOD_OAUTH:
			begin
				result := self.getOAuthToken(self.OAuthToken);
				if not result then
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'OAuth error: ' + self.OAuthToken.error + '(' + self.OAuthToken.error_description + ')');
			end;
	end;
end;

function TCloudMailRu.loginShared(method: integer): Boolean;
begin
	Log(LogLevelDetail, MSGTYPE_DETAILS, 'Open ' + self.OptionsSet.AccountSettings.public_url);
	result := self.getSharedToken();
	//exit(true);
end;

procedure TCloudMailRu.logUserSpaceInfo;
var
	US: TCloudMailRuSpaceInfo;
	QuotaInfo: WideString;

begin
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if self.getUserSpace(US) then
	begin
		if (US.overquota) then
			QuotaInfo := ' Warning: space quota exhausted!'
		else
			QuotaInfo := EmptyWideStr;
		Log(LogLevelFileOperation, MSGTYPE_DETAILS, 'Total space: ' + FormatSize(US.total) + ', used: ' + FormatSize(US.used) + ', free: ' + FormatSize(US.total - US.used) + '.' + QuotaInfo);
	end else begin
		Log(LogLevelDebug, MSGTYPE_IMPORTANTERROR, 'error: getting user space information for ' + self.user + '@' + self.domain);
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
	if self.HTTP.PostForm(API_FILE_MOVE, 'home=' + PathToUrl(OldName) + '&folder=' + PathToUrl(ToPath) + self.united_params + '&conflict', JSON) then
		result := CloudResultToFsResult(CMLJSONParser.getOperationResult(JSON), 'File move error: ');
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
			result := self.renameFile(NewPath + ExtractFileName(OldName), ExtractFileName(NewName));
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
		result := self.HTTP.PostForm(API_FILE_PUBLISH, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', JSON, 'application/x-www-form-urlencoded', true, false);
	end else begin
		result := self.HTTP.PostForm(API_FILE_UNPUBLISH, 'weblink=' + PublicLink + self.united_params + '&conflict', JSON, 'application/x-www-form-urlencoded', true, false);
	end;

	if result then
		result := CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'File publish error: ');
	if result and publish then
		result := CMLJSONParser.getPublicLink(JSON, PublicLink);
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
	if self.HTTP.GetPage(API_FOLDER_SHARED_INFO + '?home=' + PathToUrl(Path) + self.united_params, JSON, Progress) then
	begin
		result := CMLJSONParser.getInviteListing(JSON, InviteListing);
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

		result := self.HTTP.PostForm(API_FOLDER_SHARE, 'home=/' + PathToUrl(Path) + self.united_params + '&invite={"email":"' + email + '","access":"' + access_string + '"}', JSON)
	end else begin
		result := self.HTTP.PostForm(API_FOLDER_UNSHARE, 'home=/' + PathToUrl(Path) + self.united_params + '&invite={"email":"' + email + '"}', JSON);
	end;
	if result then
		result := CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'Invite member error: ')
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
	result := self.HTTP.PostForm(API_TRASHBIN_RESTORE, 'path=' + PathToUrl(Path) + '&restore_revision=' + RestoreRevision.ToString + self.united_params + '&conflict=' + ConflictMode, JSON) and CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'File restore error: ');
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

	result := self.HTTP.PostForm(API_TRASHBIN_EMPTY, self.united_params, JSON) and CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'Trashbin clearing error: ');

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
	result := self.HTTP.PostForm(API_FOLDER_MOUNT, 'home=' + UrlEncode(home) + '&invite_token=' + invite_token + self.united_params + '&conflict=' + ConflictMode, JSON) and CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'Folder mount error: ');
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
	result := self.HTTP.PostForm(API_FOLDER_UNMOUNT, 'home=' + UrlEncode(home) + '&clone_copy=' + CopyStr + self.united_params, JSON) and CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'Folder unmount error: ');
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
	result := self.HTTP.PostForm(API_INVITE_REJECT, 'invite_token=' + invite_token + self.united_params, JSON) and CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'Invite rejection error: ');
end;

function TCloudMailRu.putFileStream(FileName, remotePath: WideString; FileStream: TStream; ConflictMode: WideString): integer;
var
	LocalFileIdentity, RemoteFileIdentity: TCloudMailRuFileIdentity;
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
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'error: uploading to cloud: ' + E.ClassName + ' with message: ' + E.Message);
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

function TCloudMailRu.putFileSplit(localPath, remotePath, ConflictMode: WideString; ChunkOverwriteMode: integer): integer;
var
	LocalFileIdentity: TCloudMailRuFileIdentity;
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
		ChunkRemotePath := ExtractFilePath(remotePath) + SplitFileInfo.GetChunks[SplittedPartIndex].name;
		self.HTTP.SetProgressNames(localPath, ChunkRemotePath);
		Log(LogLevelDebug, MSGTYPE_DETAILS, 'Partial upload of ' + localPath + ' part ' + (SplittedPartIndex + 1).ToString + ' of ' + SplitFileInfo.ChunksCount.ToString + ' => ' + ChunkRemotePath);

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
					Log(LogLevelDetail, MSGTYPE_DETAILS, 'Partial upload aborted.');
					Break;
				end;
			FS_FILE_EXISTS:
				begin
					case ChunkOverwriteMode of
						ChunkOverwrite: //silently overwrite chunk
							begin
								Log(LogLevelWarning, MSGTYPE_DETAILS, 'Chunk ' + ChunkRemotePath + ' already exists, overwriting.');
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
								Log(LogLevelWarning, MSGTYPE_DETAILS, 'Chunk ' + ChunkRemotePath + ' already exists, skipping.'); //ignore and continue
							end;
						ChunkOverwriteAbort: //abort operation
							begin
								Log(LogLevelWarning, MSGTYPE_DETAILS, 'Chunk ' + ChunkRemotePath + ' already exists, aborting.');
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
								case (messagebox(FindTCWindow, PWideChar('Partial upload error, code:' + result.ToString + sLineBreak + 'partname: ' + ChunkRemotePath + sLineBreak + 'Continue operation?'), 'Upload error', MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
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
								Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Partial upload error, code: ' + result.ToString + ', ignored');
							end;

						OperationErrorModeAbort:
							begin
								Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Partial upload error, code: ' + result.ToString + ', aborted');
								result := FS_FILE_USERABORT;
								Break;
							end;
						OperationErrorModeRetry:
							begin
								Inc(RetryAttemptsCount);
								if RetryAttemptsCount <> RetryAttempts + 1 then
								begin
									Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Partial upload error, code: ' + result.ToString + ' Retry attempt ' + RetryAttemptsCount.ToString + RetryAttemptsToString(RetryAttempts));
									Dec(SplittedPartIndex); //retry with this chunk
									ProcessMessages;
									Sleep(AttemptWait);
								end else begin
									Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Partial upload error, code: ' + result.ToString + ' Retry attempt limit exceed, aborted');
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

	if result = FS_FILE_OK then {Only after succesful upload}
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
			Log(LogLevelDetail, MSGTYPE_DETAILS, 'File size > ' + self.CloudMaxFileSize.ToString() + ' bytes, file will be splitted.');
			exit(putFileSplit(localPath, remotePath, ConflictMode, ChunkOverwriteMode));
		end else begin
			Log(LogLevelWarning, MSGTYPE_IMPORTANTERROR, 'File size > ' + self.CloudMaxFileSize.ToString() + ' bytes, ignored.');
			exit(FS_FILE_NOTSUPPORTED);
		end;
	end;

	result := putFileWhole(localPath, remotePath, ConflictMode);
end;

function TCloudMailRu.putFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCloudMailRuFileIdentity): integer;
var
	PostAnswer: WideString;
	Return: TStringList;
	code: integer;
begin
	FileIdentity.Hash := EmptyWideStr;
	FileIdentity.size := -1;
	result := CLOUD_OPERATION_FAILED;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	Return := TStringList.Create;
	result := self.HTTP.PostFile(self.upload_url + '/?cloud_domain=1&x-email=' + self.user + '%40' + self.domain + '&fileapi' + DateTimeToUnix(now).ToString + '0246', FileName, FileStream, PostAnswer);
	if (result = CLOUD_OPERATION_OK) then
	begin
		ExtractStrings([';'], [], PWideChar(PostAnswer), Return);
		if length(Return.Strings[0]) <> 40 then //? добавить анализ ответа?
		begin
			result := CLOUD_OPERATION_FAILED;
		end else begin
			FileIdentity.Hash := Return.Strings[0];
			val(Return.Strings[1], FileIdentity.size, code);
		end;
	end;
	Return.Destroy;
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
	self.HTTP.SetProgressNames('Remove directory', Path);
	result := self.HTTP.PostForm(API_FILE_REMOVE, 'home=/' + IncludeSlash(PathToUrl(Path)) + self.united_params + '&conflict', JSON) and CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'Directory deletion error: '); //API всегда отвечает true, даже если путь не существует
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
	if self.HTTP.PostForm(API_FILE_RENAME, 'home=' + PathToUrl(OldName) + '&name=' + PathToUrl(NewName) + self.united_params, JSON) then
		result := CloudResultToFsResult(CMLJSONParser.getOperationResult(JSON), 'File renaming error: ')
end;

function TCloudMailRu.statusFile(Path: WideString; var FileInfo: TCloudMailRuDirListingItem): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	if self.public_account then
		result := self.HTTP.GetPage(API_FILE + '?weblink=' + IncludeSlash(self.public_link) + PathToUrl(Path) + self.united_params, JSON, Progress)
	else
		result := self.HTTP.GetPage(API_FILE + '?home=' + PathToUrl(Path) + self.united_params, JSON, Progress);
	if result then
		result := CloudResultToBoolean(CMLJSONParser.getOperationResult(JSON), 'File status error: ') and CMLJSONParser.getFileStatus(JSON, FileInfo);
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

function TCloudMailRu.cloudHash(Stream: TStream; Path: WideString = 'Calculating cloud hash'): WideString;
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
	Aborted := false;
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
		if (1 = ExternalProgressProc(PWideChar(Path), 'Calculating cloud hash', Percent)) then
		begin
			Aborted := true;
		end;
	until (read < sizeof(buffer)) or Aborted;

	finalBuffer := TEncoding.UTF8.GetBytes(Stream.size.ToString);
	sha1.Update(finalBuffer, length(finalBuffer));
	if (not Aborted) then
		result := UpperCase(sha1.HashAsString);
	sha1.Reset;
end;

end.
