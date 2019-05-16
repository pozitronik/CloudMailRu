unit CloudMailRu;

interface

uses CMLJSON, CMLParsers, CMLTypes, System.Hash, System.Classes, System.Generics.Collections, System.SysUtils, PLUGIN_Types, Winapi.Windows, IdStack, MRC_helper, Settings, IdCookieManager, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdSocks, IdHTTP, IdAuthentication, IdIOHandlerStream, IdInterceptThrottler, IdCookie, IdMultipartFormData, Cipher, Splitfile, ChunkedFileStream;

type
	TCloudMailRu = class
	private
		{VARIABLES}
		ExternalSourceName: PWideChar;
		ExternalTargetName: PWideChar;
		domain: WideString;
		user: WideString;
		password: WideString;
		unlimited_filesize: Boolean;
		split_large_files: Boolean;
		split_file_size: integer;
		public_account: Boolean;
		PUBLIC_URL: WideString;
		public_link: WideString; //public url without adress prefix
		public_download_token: WideString;
		public_shard: WideString;
		shard_override: WideString;
		upload_url_override: WideString;
		token: WideString;
		OAuthToken: TCloudMailRuOAuthInfo;
		x_page_id: WideString;
		build: WideString;
		upload_url: WideString;
		login_method: integer;
		Cookie: TIdCookieManager;
		Socks: TIdSocksInfo;
		Throttle: TIdInterceptThrottler;
		ExternalProgressProc: TProgressHandler;
		ExternalLogProc: TLogHandler;
		ExternalRequestProc: TRequestHandler;
		Shard: WideString;
		Proxy: TProxySettings;
		ConnectTimeout: integer;
		UploadBPS: integer;
		DownloadBPS: integer;
		PrecalculateHash: Boolean;
		CheckCRC: Boolean;

		FileCipher: TFileCipher;

		united_params: WideString; //Объединённый набор авторизационных параметров для подстановки в URL

		{BASE HTTP METHODS}
		procedure HTTPInit(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL; var Socks: TIdSocksInfo; var Cookie: TIdCookieManager);
		procedure HTTPDestroy(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL);

		function HTTPGetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean; //если ProgressEnabled - включаем обработчик onWork, возвращаем ProgressEnabled=false при отмене
		function HTTPGetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean = true): integer;

		function HTTPPostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = true; ProgressEnabled: Boolean = true): Boolean; //Постинг данных с возможным получением ответа.
		function HTTPPostMultipart(URL: WideString; Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean;
		function HTTPPostFile(URL: WideString; FileName: WideString; var Answer: WideString): integer; overload; //Постинг файла и получение ответа
		function HTTPPostFile(URL: WideString; FileName: WideString; var Answer: WideString; ChunkInfo: TFileChunkInfo): integer; overload; //Постинг файла и получение ответа
		function HTTPPostFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): integer; overload; //Постинг потока данных как файла

		function HTTPPost(URL: WideString; PostData, ResultData: TStringStream; UnderstandResponseCode: Boolean = false; ContentType: WideString = ''; LogErrors: Boolean = true; ProgressEnabled: Boolean = true): integer; overload; //Постинг подготовленных данных, отлов ошибок
		function HTTPPost(URL: WideString; var PostData: TIdMultiPartFormDataStream; ResultData: TStringStream): integer; overload; //TIdMultiPartFormDataStream should be passed via var
		function HTTPExceptionHandler(E: Exception; URL: WideString; HTTPMethod: integer = HTTP_METHOD_POST; LogErrors: Boolean = true): integer;

		procedure HTTPProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
		{RAW TEXT PARSING}{TODO: Move to separate unit}
		//Moved to CMLParsers
		{JSON MANIPULATION}
		//Moved to CMLJSON
		{HTTP REQUESTS WRAPPERS}
		function getToken(): Boolean;
		function getSharedToken(): Boolean;
		function getOAuthToken(var OAuthToken: TCloudMailRuOAuthInfo): Boolean;
		function getShard(var Shard: WideString): Boolean;
		function getUserSpace(var SpaceInfo: TCloudMailRuSpaceInfo): Boolean;
		function putFileToCloud(localPath: WideString; Return: TStringList; ChunkInfo: PFileChunkInfo = nil): integer; overload; //Отправка на сервер файла или куска файла
		function putFileToCloud(FileName: WideString; FileStream: TStream; Return: TStringList): integer; overload; //отправка на сервер данных из потока
		function addFileToCloud(Hash: WideString; size: int64; remotePath: WideString; var JSONAnswer: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true): Boolean; //LogErrors=false => не логируем результат операции, нужно для поиска данных в облаке по хешу

		{PRIVATE UPLOAD METHODS CHAIN (CALLED FROM putFile())}
		function putFileWhole(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer; //Загрузка файла целиком
		function putFileSplit(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: integer = 0): integer; //Загрузка файла по частям
		function putFileChunk(localPath, remotePath: WideString; ChunkInfo: TFileChunkInfo; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer; //Загрузка указанной части файла
		function putFileChunkCRC(remotePath: WideString; SplitFileInfo: TFileSplitInfo; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer; //Загрузка CRC-файла для разбитого файла

		{OTHER ROUTINES}
		procedure Log(LogLevel, MsgType: integer; LogString: WideString);
		function CloudResultToFsResult(CloudResult: integer; OperationStatus: integer; ErrorPrefix: WideString = ''): integer;
		function cloudHash(Path: WideString; ChunkInfo: PFileChunkInfo = nil): WideString; overload; //get cloud hash for specified file/file part
		function cloudHash(Stream: TStream; SourceName: WideString = ''): WideString; overload; //get cloud hash for data in stream
		function addByHash(Hash: WideString; size: int64; localPath, remotePath: WideString): Boolean; //addFileToCloud function wrapper with result parsing
	protected
		{REGULAR CLOUD}
		function loginRegular(method: integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function getFileRegular(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean = true): integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		{SHARED WEBFOLDERS}
		function loginShared(method: integer = CLOUD_AUTH_METHOD_WEB): Boolean;

		function getFileShared(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean = true): integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
	public
		crypt_files: Boolean;
		crypt_filenames: Boolean;

		Property isPublicShare: Boolean read public_account;
		Property ProxySettings: TProxySettings read Proxy;
		Property UploadLimit: integer read UploadBPS;
		Property DownloadLimit: integer read DownloadBPS;

		Property ConnectTimeoutValue: integer read ConnectTimeout;
		function getSharedFileUrl(remotePath: WideString; DoUrlEncode: Boolean = true): WideString;
		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(AccountSettings: TAccountSettings; split_file_size: integer; Proxy: TProxySettings; ConnectTimeout: integer; UploadBPS: integer; DownloadBPS: integer; PrecalculateHash: Boolean = true; CheckCRC: Boolean = true; ExternalProgressProc: TProgressHandler = nil; ExternalLogProc: TLogHandler = nil; ExternalRequestProc: TRequestHandler = nil);
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
		function copyFile(OldName, ToPath: WideString): integer; //Копирование файла внутри одного каталога
		function mvFile(OldName, NewName: WideString): integer; //объединяющая функция, определяет делать rename или move
		function cpFile(OldName, NewName: WideString): integer; //Копирует файл, и переименует, если нужно
		function deleteFile(Path: WideString): Boolean;
		function publishFile(Path: WideString; var PublicLink: WideString; publish: Boolean = CLOUD_PUBLISH): Boolean;
		function cloneWeblink(Path, link: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): integer; //клонировать публичную ссылку в текущий каталог
		function cloneHash(Path, Hash: WideString; size: int64; FileName: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): integer; //создать клон файла по известному хешу и размеру
		function getShareInfo(Path: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
		function shareFolder(Path, email: WideString; access: integer): Boolean;
		function trashbinRestore(Path: WideString; RestoreRevision: integer; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function trashbinEmpty(): Boolean;
		function mountFolder(home, invite_token: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function unmountFolder(home: WideString; clone_copy: Boolean): Boolean;
		function rejectInvite(invite_token: WideString): Boolean;
		{OTHER ROUTINES}
		function getDescriptionFile(remotePath, localCopy: WideString): Boolean; //Если в каталоге remotePath есть descript.ion - скопировать его в файл localcopy
		function putDesriptionFile(remotePath, localCopy: WideString): Boolean; //Скопировать descript.ion из временного файла на сервер
		procedure logUserSpaceInfo();
		{STATIC ROUTINES}
		class function CloudAccessToString(access: WideString; Invert: Boolean = false): WideString; static;
		class function StringToCloudAccess(accessString: WideString; Invert: Boolean = false): integer; static;
		class function ErrorCodeText(ErrorCode: integer): WideString; static;
	end;

implementation

{TCloudMailRu}

function TCloudMailRu.addByHash(Hash: WideString; size: int64; localPath, remotePath: WideString): Boolean;
var
	JSONAnswer: WideString;
	OperationStatus: integer;
begin
	OperationStatus := 0;
	if self.addFileToCloud(Hash, size, remotePath, JSONAnswer, CLOUD_CONFLICT_STRICT, false) then
	begin
		if fromJSON_OperationResult(JSONAnswer, OperationStatus) = CLOUD_OPERATION_OK then
		begin
			Log(LogLevelDetail, MSGTYPE_DETAILS, 'File ' + remotePath + ' found by hash.');
			exit(true);
		end;
	end;
	exit(false);
end;

function TCloudMailRu.addFileToCloud(Hash: WideString; size: int64; remotePath: WideString; var JSONAnswer: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true): Boolean;
var
	FileName: WideString;
begin
	{Экспериментально выяснено, что параметры api, build, email, x-email, x-page-id в запросе не обязательны}
	if self.crypt_filenames then
	begin
		FileName := ExtractUniversalFileName(remotePath);
		FileName := FileCipher.CryptFileName(FileName);
		remotePath := ChangePathFileName(remotePath, FileName);
	end;
	result := self.HTTPPostForm(API_FILE_ADD, 'conflict=' + ConflictMode + '&home=/' + PathToUrl(remotePath) + '&hash=' + Hash + '&size=' + size.ToString + self.united_params, JSONAnswer, 'application/x-www-form-urlencoded', LogErrors);

end;

function TCloudMailRu.cloneHash(Path, Hash: WideString; size: int64; FileName: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): integer; //создать клон файла по известному хешу и размеру
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);
	if self.addFileToCloud(Hash, size, IncludeTrailingBackslash(Path) + FileName, JSON, ConflictMode, true) then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		result := CloudResultToFsResult(OperationResult, OperationStatus, 'File uploading error: ');
	end;
end;

function TCloudMailRu.cloneWeblink(Path, link, ConflictMode: WideString): integer;
var
	JSON: WideString;
	OperationStatus: integer;
	Progress: Boolean;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);
	Progress := true;
	if self.HTTPGetPage(API_CLONE + '?folder=' + PathToUrl(Path) + '&weblink=' + link + '&conflict=' + ConflictMode + self.united_params, JSON, Progress) then
	begin //Парсим ответ
		result := fromJSON_OperationResult(JSON, OperationStatus);
		if result <> CLOUD_OPERATION_OK then
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'File publish error: ' + self.ErrorCodeText(result) + ' Status: ' + OperationStatus.ToString());

	end else begin
		if not(Progress) then
		begin //user cancelled
			result := FS_FILE_USERABORT;
		end else begin //unknown error
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Public link clone error: got ' + OperationStatus.ToString + ' status');
			result := FS_FILE_WRITEERROR;
		end;
	end;
end;

function TCloudMailRu.CloudResultToFsResult(CloudResult: integer; OperationStatus: integer; ErrorPrefix: WideString): integer;
begin
	case CloudResult of
		CLOUD_OPERATION_OK:
			exit(CLOUD_OPERATION_OK);
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
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, ErrorPrefix + self.ErrorCodeText(CloudResult) + ' Status: ' + OperationStatus.ToString());
				exit(FS_FILE_WRITEERROR);
			end;
	end;
end;

function TCloudMailRu.copyFile(OldName, ToPath: WideString): integer;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);
	if self.HTTPPostForm(API_FILE_COPY, 'home=' + PathToUrl(OldName) + '&folder=' + PathToUrl(ToPath) + self.united_params + '&conflict', JSON) then
	begin //Парсим ответ
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		result := CloudResultToFsResult(OperationResult, OperationStatus, 'File copy error: ');
	end;
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
		Log(LogLevelWarning, MSGTYPE_IMPORTANTERROR, 'Copying in same dir not supported by cloud');
		exit(FS_FILE_NOTSUPPORTED);
	end else begin
		result := self.copyFile(OldName, NewPath);
		if result <> CLOUD_OPERATION_OK then
			exit;
	end;
	if not(SameName) then
	begin //скопированный файл лежит в новом каталоге со старым именем
		result := self.renameFile(NewPath + ExtractFileName(OldName), ExtractFileName(NewName));
	end;
end;

constructor TCloudMailRu.Create(AccountSettings: TAccountSettings; split_file_size: integer; Proxy: TProxySettings; ConnectTimeout: integer; UploadBPS: integer; DownloadBPS: integer; PrecalculateHash: Boolean = true; CheckCRC: Boolean = true; ExternalProgressProc: TProgressHandler = nil; ExternalLogProc: TLogHandler = nil; ExternalRequestProc: TRequestHandler = nil);
begin
	try
		self.ExternalProgressProc := ExternalProgressProc;
		self.ExternalLogProc := ExternalLogProc;
		self.ExternalRequestProc := ExternalRequestProc;

		self.Cookie := TIdCookieManager.Create();
		self.Throttle := TIdInterceptThrottler.Create();

		self.Proxy := Proxy;
		if Proxy.ProxyType in SocksProxyTypes then //SOCKS proxy initialization
		begin
			self.Socks := TIdSocksInfo.Create();
			self.Socks.Host := Proxy.Server;
			self.Socks.Port := Proxy.Port;
			if Proxy.user <> EmptyWideStr then
			begin
				self.Socks.Authentication := saUsernamePassword;
				self.Socks.Username := Proxy.user;
				self.Socks.password := Proxy.password;
			end
			else
				self.Socks.Authentication := saNoAuthentication;

			case Proxy.ProxyType of
				ProxySocks5:
					Socks.Version := svSocks5;
				ProxySocks4:
					Socks.Version := svSocks4;
			end;
			self.Socks.Enabled := true;
		end;

		self.user := AccountSettings.user;
		self.password := AccountSettings.password;
		self.domain := AccountSettings.domain;
		self.unlimited_filesize := AccountSettings.unlimited_filesize;
		self.split_large_files := AccountSettings.split_large_files;
		self.public_account := AccountSettings.public_account;
		self.PUBLIC_URL := AccountSettings.PUBLIC_URL;

		if AccountSettings.encrypt_files_mode <> EncryptModeNone then
		begin
			self.FileCipher := TFileCipher.Create(AccountSettings.crypt_files_password, AccountSettings.CryptedGUID_files, AccountSettings.encrypt_filenames);
			if self.FileCipher.WrongPassword then
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Wrong encryption password, encryption support disabled');

			self.crypt_files := not(self.FileCipher.WrongPassword);
			self.crypt_filenames := self.crypt_files and AccountSettings.encrypt_filenames and not(self.FileCipher.WrongPassword);
		end;

		self.shard_override := AccountSettings.shard_override;
		self.upload_url_override := AccountSettings.upload_url_override;
		if self.public_account and (self.PUBLIC_URL <> EmptyWideStr) then
		begin
			self.public_link := self.PUBLIC_URL;
			self.PUBLIC_URL := IncludeSlash(self.PUBLIC_URL);
			Delete(self.public_link, 1, length(PUBLIC_ACCESS_URL));
			if self.public_link[length(self.public_link)] = '/' then
				Delete(self.public_link, length(self.public_link), 1);
		end;

		self.split_file_size := split_file_size;
		self.ConnectTimeout := ConnectTimeout;
		self.UploadBPS := UploadBPS;
		self.DownloadBPS := DownloadBPS;
		self.PrecalculateHash := PrecalculateHash;
		self.CheckCRC := CheckCRC;

		self.ExternalSourceName := nil;
		self.ExternalTargetName := nil;
	except
		on E: Exception do
		begin
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Cloud initialization error: ' + E.Message);
		end;
	end;
end;

function TCloudMailRu.createDir(Path: WideString): Boolean;
var
	PostAnswer: WideString;
	OperationStatus, OperationResult: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if self.HTTPPostForm(API_FOLDER_ADD, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', PostAnswer) then
	begin
		OperationResult := fromJSON_OperationResult(PostAnswer, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := true;
			else
				begin
					//Log(MSGTYPE_IMPORTANTERROR, 'Directory creation error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					result := false;
				end;
		end;
	end;
end;

function TCloudMailRu.deleteFile(Path: WideString): Boolean;
var
	JSON: WideString;
	OperationResult, OperationStatus: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	result := self.HTTPPostForm(API_FILE_REMOVE, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', JSON);
	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := true;
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Delete file error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					result := false;
				end;
		end;
	end;
end;

destructor TCloudMailRu.Destroy;
begin
	if Assigned(self.Cookie) then
		self.Cookie.free;
	if Assigned(self.Throttle) then
		self.Throttle.free;
	if Assigned(self.Socks) then
		self.Socks.free;
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
	OperationStatus, OperationResult: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if self.public_account then
		exit;
	result := self.HTTPGetPage(API_FOLDER_SHARED_LINKS + '?' + self.united_params, JSON, ShowProgress);

	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := fromJSON_DirListing(JSON, DirListing);
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Shared links listing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					result := false;
				end;
		end;
	end;
end;

function TCloudMailRu.getIncomingLinksListing(var IncomingListing: TCloudMailRuIncomingInviteInfoListing; ShowProgress: Boolean): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	SetLength(IncomingListing, 0);
	if self.public_account then
		exit;
	result := self.HTTPGetPage(API_FOLDER_SHARED_INCOMING + '?' + self.united_params, JSON, ShowProgress);

	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := fromJSON_IncomingInviteListing(JSON, IncomingListing);
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Incoming requests listing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					result := false;
				end;
		end;
	end;
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
	OperationStatus, OperationResult: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if self.public_account then
		exit;
	result := self.HTTPGetPage(API_TRASHBIN + '?' + self.united_params, JSON, ShowProgress);

	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := fromJSON_DirListing(JSON, DirListing);
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Incoming requests listing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					result := false;
				end;
		end;
	end;
end;

function TCloudMailRu.getDirListing(Path: WideString; var DirListing: TCloudMailRuDirListing; ShowProgress: Boolean = false): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if self.public_account then
		result := self.HTTPGetPage(API_FOLDER + '&weblink=' + IncludeSlash(self.public_link) + PathToUrl(Path, false) + self.united_params, JSON, ShowProgress)
	else
		result := self.HTTPGetPage(API_FOLDER + '&home=' + PathToUrl(Path) + self.united_params, JSON, ShowProgress);
	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				begin
					result := fromJSON_DirListing(JSON, DirListing);
					if result and self.crypt_filenames then
						self.FileCipher.DecryptDirListing(DirListing);
				end;
			CLOUD_ERROR_NOT_EXISTS:
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Path not exists: ' + Path);
					result := false;
				end
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Directory listing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString()); //?? WUT
					result := false;
				end;
		end;
	end;
end;

function TCloudMailRu.getFile(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean): integer;
begin
	result := FS_FILE_NOTSUPPORTED;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		result := self.getFileShared(remotePath, localPath, resultHash, LogErrors)
	else
		result := self.getFileRegular(remotePath, localPath, resultHash, LogErrors);
end;

function TCloudMailRu.getFileRegular(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean): integer;
var
	FileStream: TFileStream; //TODO: use TBufferedFileStream instead
	FileName: WideString;
	MemoryStream: TMemoryStream;
begin
	result := FS_FILE_NOTSUPPORTED;
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
		FileStream := TFileStream.Create(GetUNCFilePath(localPath), fmCreate);
	except
		on E: Exception do
		begin
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.Message);
			exit(FS_FILE_WRITEERROR);
		end;
	end;
	self.ExternalSourceName := PWideChar(remotePath);
	self.ExternalTargetName := PWideChar(localPath);

	if self.crypt_files then //Загрузка файла в память, дешифрация в файл
	begin
		MemoryStream := TMemoryStream.Create;
		result := self.HTTPGetFile(self.Shard + PathToUrl(remotePath, false), MemoryStream, LogErrors);
		if result in [FS_FILE_OK] then
		begin
			resultHash := cloudHash(MemoryStream, ExtractFileName(localPath));
			MemoryStream.Position := 0;
			self.FileCipher.DecryptStream(MemoryStream, FileStream);
		end;
		MemoryStream.free;

	end else begin
		result := self.HTTPGetFile(self.Shard + PathToUrl(remotePath, false), FileStream, LogErrors);
		if (result in [FS_FILE_OK]) then
			resultHash := cloudHash(FileStream, ExtractFileName(localPath));
	end;

	FlushFileBuffers(FileStream.Handle);
	FileStream.free;
	self.ExternalSourceName := nil;
	self.ExternalTargetName := nil;

	if not(result in [FS_FILE_OK]) then
		System.SysUtils.deleteFile(GetUNCFilePath(localPath));
end;

function TCloudMailRu.getSharedFileUrl(remotePath: WideString; DoUrlEncode: Boolean = true): WideString;
begin
	result := IncludeSlash(self.public_shard) + IncludeSlash(self.public_link) + PathToUrl(remotePath, true, DoUrlEncode) + '?key=' + self.public_download_token
end;

function TCloudMailRu.getFileShared(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean): integer;
var
	FileStream: TFileStream;
begin
	result := FS_FILE_NOTFOUND;
	if (self.public_shard = EmptyWideStr) or (self.public_download_token = EmptyWideStr) then
		exit;
	try
		FileStream := TFileStream.Create(GetUNCFilePath(localPath), fmCreate);
	except
		on E: Exception do
		begin
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.Message);
			exit(FS_FILE_WRITEERROR);
		end;
	end;
	if (Assigned(FileStream)) then
	begin
		result := self.HTTPGetFile(getSharedFileUrl(remotePath), FileStream, LogErrors);
		resultHash := cloudHash(FileStream);
		FlushFileBuffers(FileStream.Handle);
		FileStream.free;
	end;
	if result <> FS_FILE_OK then
		System.SysUtils.deleteFile(GetUNCFilePath(localPath));
end;

function TCloudMailRu.getOAuthToken(var OAuthToken: TCloudMailRuOAuthInfo): Boolean;
var
	Answer: WideString;
begin
	result := false;
	if self.HTTPPostForm(OAUTH_TOKEN_URL, 'client_id=cloud-win&grant_type=password&username=' + self.user + '%40' + self.domain + '&password=' + UrlEncode(self.password), Answer) then
	begin
		if not fromJSON_OAuthTokenInfo(Answer, OAuthToken) then
			exit(false);
		result := OAuthToken.error_code = NOERROR;
	end;
end;

function TCloudMailRu.getShard(var Shard: WideString): Boolean;
var
	JSON: WideString;
	OperationResult, OperationStatus: integer;
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

	if self.HTTPPostForm(API_DISPATCHER, self.united_params, JSON) then //checkme
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				begin
					result := fromJSON_Shard(JSON, Shard) and (Shard <> EmptyWideStr);
					Log(LogLevelDetail, MSGTYPE_DETAILS, 'Shard received: ' + Shard);
				end
			else
				begin
					result := false;
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Shard receive error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
				end;
		end;
	end;
end;

function TCloudMailRu.getToken: Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	result := self.HTTPGetPage(TOKEN_URL, JSON, Progress);
	if result then
	begin
		result := extractTokenFromText(JSON, self.token) and extract_x_page_id_FromText(JSON, self.x_page_id) and extract_build_FromText(JSON, self.build) and extract_upload_url_FromText(JSON, self.upload_url);
		self.united_params := '&api=2&build=' + self.build + '&x-page-id=' + self.x_page_id + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&token=' + self.token + '&_=' + DateTimeToUnix(now).ToString + '810';
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
	result := self.HTTPGetPage(self.PUBLIC_URL, PageContent, Progress);
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
	OperationResult, OperationStatus: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	result := self.HTTPGetPage(API_USER_SPACE + '?home=/' + self.united_params, JSON, Progress);
	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := fromJSON_UserSpace(JSON, SpaceInfo);
			else
				begin
					result := false;
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'User space receiving error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
				end;
		end;
	end;
end;

procedure TCloudMailRu.HTTPInit(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL; var Socks: TIdSocksInfo; var Cookie: TIdCookieManager);
begin
	SSL := TIdSSLIOHandlerSocketOpenSSL.Create();
	SSL.SSLOptions.SSLVersions := [sslvSSLv23];
	HTTP := TIdHTTP.Create();
	if (self.Proxy.ProxyType in SocksProxyTypes) and (self.Socks.Enabled) then
		SSL.TransparentProxy := self.Socks;
	if self.Proxy.ProxyType = ProxyHTTP then
	begin
		HTTP.ProxyParams.ProxyServer := self.Proxy.Server;
		HTTP.ProxyParams.ProxyPort := self.Proxy.Port;
		if self.Proxy.user <> EmptyWideStr then
		begin
			HTTP.ProxyParams.BasicAuthentication := true;
			HTTP.ProxyParams.ProxyUsername := self.Proxy.user;
			HTTP.ProxyParams.ProxyPassword := self.Proxy.password;
		end
	end;
	HTTP.CookieManager := Cookie;
	HTTP.IOHandler := SSL;
	HTTP.AllowCookies := true;
	HTTP.HTTPOptions := [hoForceEncodeParams, hoNoParseMetaHTTPEquiv, hoKeepOrigProtocol, hoTreat302Like303];
	HTTP.HandleRedirects := true;
	if (self.ConnectTimeout < 0) then
	begin
		HTTP.ConnectTimeout := self.ConnectTimeout;
		HTTP.ReadTimeout := self.ConnectTimeout;
	end;
	if (self.UploadBPS > 0) or (self.DownloadBPS > 0) then
	begin
		self.Throttle.RecvBitsPerSec := self.DownloadBPS;
		self.Throttle.SendBitsPerSec := self.UploadBPS;

	end;

	HTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.57 Safari/537.17/TCWFX(' + PlatformX + ')';
	HTTP.Request.Connection := EmptyWideStr;
end;

procedure TCloudMailRu.HTTPDestroy(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL);
begin
	HTTP.free;
	SSL.free;
end;

function TCloudMailRu.HTTPGetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
var
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
	Socks: TIdSocksInfo;
begin
	result := false;
	try
		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		if ProgressEnabled then
			HTTP.OnWork := self.HTTPProgress; //Вызов прогресса ведёт к возможности отменить получение списка каталогов и других операций, поэтому он нужен не всегда
		Answer := HTTP.Get(URL);
		self.HTTPDestroy(HTTP, SSL);
		result := Answer <> EmptyWideStr;
	Except
		on E: Exception do
		begin
			case self.HTTPExceptionHandler(E, URL) of
				CLOUD_OPERATION_CANCELLED:
					begin
						ProgressEnabled := false; //сообщаем об отмене
					end;
				CLOUD_OPERATION_FAILED:
					begin
						case HTTP.ResponseCode of
							HTTP_ERROR_BAD_REQUEST, HTTP_ERROR_OVERQUOTA: //recoverable errors
								begin
									//Answer := (E as EIdHTTPProtocolException).ErrorMessage; //TODO: нужно протестировать, наверняка тут не json
								end;
						end;
					end;
			end;
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
		end;

	end;

end;

function TCloudMailRu.HTTPGetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean): integer;
var
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
	Socks: TIdSocksInfo;
begin
	result := FS_FILE_OK;
	try
		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		HTTP.Intercept := Throttle;
		HTTP.Request.ContentType := 'application/octet-stream';
		HTTP.Response.KeepAlive := true;
		HTTP.OnWork := self.HTTPProgress;
		HTTP.Get(URL, FileStream);
		if (HTTP.RedirectCount = HTTP.RedirectMaximum) and (FileStream.size = 0) then
		begin
			result := FS_FILE_READERROR;
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Redirection limit reached when trying to download ' + URL);
			if (ExternalRequestProc(RT_MsgYesNo, 'Redirection limit', 'Try with another shard?', '', 0)) and (self.getShard(self.Shard)) then
				result := self.HTTPGetFile(URL, FileStream, LogErrors);
		end;
		self.HTTPDestroy(HTTP, SSL);
	except
		on E: Exception do
		begin
			result := self.HTTPExceptionHandler(E, URL, HTTP_METHOD_GET, LogErrors);
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
		end;
	end;
end;

function TCloudMailRu.HTTPPostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = true; ProgressEnabled: Boolean = true): Boolean;
var
	ResultStream, PostData: TStringStream;
	PostResult: integer;
begin
	ResultStream := TStringStream.Create;
	PostData := TStringStream.Create(PostDataString, TEncoding.UTF8);

	PostResult := self.HTTPPost(URL, PostData, ResultStream, true, ContentType, LogErrors, ProgressEnabled);
	result := PostResult = CLOUD_OPERATION_OK;
	Answer := ResultStream.DataString;

	ResultStream.free;
	PostData.free;
end;

function TCloudMailRu.HTTPPostMultipart(URL: WideString; Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean; //test
var
	ResultStream: TStringStream;
	PostData: TIdMultiPartFormDataStream;
	ParamItem: TPair<WideString, WideString>;
	PostResult: integer;
begin

	ResultStream := TStringStream.Create;

	PostData := TIdMultiPartFormDataStream.Create;
	for ParamItem in Params do
		PostData.AddFormField(ParamItem.Key, ParamItem.Value);

	PostResult := self.HTTPPost(URL, PostData, ResultStream);
	result := PostResult = CLOUD_OPERATION_OK;
	Answer := ResultStream.DataString;

	ResultStream.free;
	PostData.free;
end;

function TCloudMailRu.HTTPPostFile(URL: WideString; FileName: WideString; var Answer: WideString): integer;
var
	FileStream: TFileStream;
begin
	FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
	result := self.HTTPPostFile(URL, FileName, FileStream, Answer);
	FileStream.free;
end;

function TCloudMailRu.HTTPPostFile(URL, FileName: WideString; var Answer: WideString; ChunkInfo: TFileChunkInfo): integer;
var
	ChunkStream: TChunkedFileStream;
begin
	ChunkStream := TChunkedFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite, ChunkInfo.start, ChunkInfo.size); {FIXME TODO: Bug here - TChunkedFileStream некорректно обрабатывает файлы больше какого-то лимита}
	result := self.HTTPPostFile(URL, ChunkInfo.name, ChunkStream, Answer);
	ChunkStream.free;
end;

function TCloudMailRu.HTTPPostFile(URL, FileName: WideString; FileStream: TStream; var Answer: WideString): integer;
var
	PostData: TIdMultiPartFormDataStream;
	ResultStream: TStringStream;
	MemoryStream: TMemoryStream;
begin
	ResultStream := TStringStream.Create;
	PostData := TIdMultiPartFormDataStream.Create;
	MemoryStream := TMemoryStream.Create;

	if self.crypt_files then {Will encrypt any type of data passed here}
	begin
		self.FileCipher.CryptStream(FileStream, MemoryStream);
		MemoryStream.Position := 0;
		PostData.AddFormField('file', 'application/octet-stream', EmptyWideStr, MemoryStream, FileName);
	end else begin
		PostData.AddFormField('file', 'application/octet-stream', EmptyWideStr, FileStream, FileName);
	end;

	result := self.HTTPPost(URL, PostData, ResultStream);
	Answer := ResultStream.DataString;
	MemoryStream.free;
	ResultStream.free;
	PostData.free;
end;

function TCloudMailRu.HTTPPost(URL: WideString; PostData, ResultData: TStringStream; UnderstandResponseCode: Boolean = false; ContentType: WideString = ''; LogErrors: Boolean = true; ProgressEnabled: Boolean = true): integer;
var
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
	Socks: TIdSocksInfo;
begin
	result := CLOUD_OPERATION_OK;
	ResultData.Position := 0;
	try
		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		if ContentType <> EmptyWideStr then
			HTTP.Request.ContentType := ContentType;
		if ProgressEnabled then
			HTTP.OnWork := self.HTTPProgress;
		HTTP.Post(URL, PostData, ResultData);
		self.HTTPDestroy(HTTP, SSL);
	except
		on E: Exception do
		begin
			result := self.HTTPExceptionHandler(E, URL, HTTP_METHOD_POST, LogErrors);
			if UnderstandResponseCode and (E is EIdHTTPProtocolException) then
			begin
				case HTTP.ResponseCode of
					HTTP_ERROR_BAD_REQUEST, HTTP_ERROR_OVERQUOTA: //recoverable errors
						begin
							ResultData.WriteString((E as EIdHTTPProtocolException).ErrorMessage);
							result := CLOUD_OPERATION_OK;
						end;
				end;
			end;
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
		end;
	end;
end;

function TCloudMailRu.HTTPPost(URL: WideString; var PostData: TIdMultiPartFormDataStream; ResultData: TStringStream): integer;
var
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
	Socks: TIdSocksInfo;
begin
	result := CLOUD_OPERATION_OK;
	ResultData.Position := 0;
	try
		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		HTTP.Intercept := Throttle;
		HTTP.OnWork := self.HTTPProgress;
		HTTP.Post(URL, PostData, ResultData);
		self.HTTPDestroy(HTTP, SSL);
	except
		On E: Exception do
		begin
			result := self.HTTPExceptionHandler(E, URL);
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
		end;
	end;
end;

function TCloudMailRu.HTTPExceptionHandler(E: Exception; URL: WideString; HTTPMethod: integer = HTTP_METHOD_POST; LogErrors: Boolean = true): integer;
var
	method_string: WideString; //в зависимости от метода исходного запроса меняется текст сообщения
begin
	if HTTPMethod = HTTP_METHOD_GET then
	begin
		method_string := 'получении данных с адреса ';
		result := FS_FILE_READERROR; //для HTTPGetFile, GetForm не интересует код ошибки
	end else begin
		method_string := 'отправке данных на адрес ';
		result := CLOUD_OPERATION_FAILED; //Для всех Post-запросов
	end;

	if E is EAbort then
	begin
		result := CLOUD_OPERATION_CANCELLED;
	end else if LogErrors then //разбирать ошибку дальше имеет смысл только для логирования - что вернуть уже понятно
	begin
		if E is EIdHTTPProtocolException then
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при ' + method_string + URL + ', ответ сервера: ' + (E as EIdHTTPProtocolException).ErrorMessage)
		else if E is EIdSocketerror then
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка сети: ' + E.Message + ' при ' + method_string + URL)
		else
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при ' + method_string + URL);

	end;

end;

procedure TCloudMailRu.HTTPProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
var
	HTTP: TIdHTTP;
	ContentLength: int64;
	Percent: integer;
begin
	HTTP := TIdHTTP(ASender);
	if AWorkMode = wmRead then
		ContentLength := HTTP.Response.ContentLength
	else
		ContentLength := HTTP.Request.ContentLength; //Считаем размер обработанных данных зависимости от того, скачивание это или загрузка
	if (Pos('chunked', LowerCase(HTTP.Response.TransferEncoding)) = 0) and (ContentLength > 0) then
	begin
		Percent := 100 * AWorkCount div ContentLength;
		if Assigned(ExternalProgressProc) and (ExternalProgressProc(self.ExternalSourceName, self.ExternalTargetName, Percent) = 1) then
			abort;
	end;
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
	self.login_method := method;
	Log(LogLevelDetail, MSGTYPE_DETAILS, 'Login to ' + self.user + '@' + self.domain);
	case self.login_method of
		CLOUD_AUTH_METHOD_TWO_STEP:
			begin
				FormFields := TDictionary<WideString, WideString>.Create();
				FormFields.AddOrSetValue('Domain', self.domain);
				FormFields.AddOrSetValue('Login', self.user);
				FormFields.AddOrSetValue('Password', self.password);
				Log(LogLevelDebug, MSGTYPE_DETAILS, 'Requesting first step auth token for ' + self.user + '@' + self.domain);
				result := self.HTTPPostMultipart(LOGIN_URL, FormFields, PostAnswer);
				if result then
				begin
					Log(LogLevelDebug, MSGTYPE_DETAILS, 'Parsing authorization data...');
					if extractTwostepJson(PostAnswer, TwoStepJson) and fromJSON_TwostepData(TwoStepJson, TwostepData) then
					begin
						if TwostepData.secstep_timeout = AUTH_APP_USED then
							AuthMessage := 'Enter code from authentication app.'//mobile app used
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
							result := self.HTTPPostMultipart(SECSTEP_URL, FormFields, PostAnswer);
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
				result := self.HTTPPostForm(LOGIN_URL, 'page=https://cloud.mail.ru/?new_auth_form=1&Domain=' + self.domain + '&Login=' + self.user + '&Password=' + UrlEncode(self.password) + '&FailPage=', PostAnswer);
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
	Log(LogLevelDetail, MSGTYPE_DETAILS, 'Open ' + self.PUBLIC_URL);
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
	OperationStatus, OperationResult: integer;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);
	if self.HTTPPostForm(API_FILE_MOVE, 'home=' + PathToUrl(OldName) + '&folder=' + PathToUrl(ToPath) + self.united_params + '&conflict', JSON) then
	begin //Парсим ответ
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		result := CloudResultToFsResult(OperationResult, OperationStatus, 'File move error: ');
	end;
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
	OperationStatus, OperationResult: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if publish then
	begin
		result := self.HTTPPostForm(API_FILE_PUBLISH, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', JSON, 'application/x-www-form-urlencoded', true, false);
	end else begin
		result := self.HTTPPostForm(API_FILE_UNPUBLISH, 'weblink=' + PublicLink + self.united_params + '&conflict', JSON, 'application/x-www-form-urlencoded', true, false);
	end;

	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				if publish then
					result := fromJSON_PublicLink(JSON, PublicLink);
			else
				begin
					result := false;
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'File publish error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
				end;
		end;
	end;
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
	if self.HTTPGetPage(API_FOLDER_SHARED_INFO + '?home=' + PathToUrl(Path) + self.united_params, JSON, Progress) then
	begin
		result := fromJSON_InviteListing(JSON, InviteListing);
	end;

end;

function TCloudMailRu.shareFolder(Path, email: WideString; access: integer): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
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

		result := self.HTTPPostForm(API_FOLDER_SHARE, 'home=/' + PathToUrl(Path) + self.united_params + '&invite={"email":"' + email + '","access":"' + access_string + '"}', JSON)
	end else begin
		result := (self.HTTPPostForm(API_FOLDER_UNSHARE, 'home=/' + PathToUrl(Path) + self.united_params + '&invite={"email":"' + email + '"}', JSON));
	end;

	if (result) then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);

		result := OperationResult = CLOUD_OPERATION_OK;
		if not result then
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Invite member error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());

	end;
end;

function TCloudMailRu.trashbinRestore(Path: WideString; RestoreRevision: integer; ConflictMode: WideString): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;

	result := self.HTTPPostForm(API_TRASHBIN_RESTORE, 'path=' + PathToUrl(Path) + '&restore_revision=' + RestoreRevision.ToString + self.united_params + '&conflict=' + ConflictMode, JSON);

	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := true;
			else
				begin
					result := false;
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'File restore error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
				end;
		end;
	end;
end;

function TCloudMailRu.trashbinEmpty(): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;

	result := self.HTTPPostForm(API_TRASHBIN_EMPTY, self.united_params, JSON);

	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := true;
			else
				begin
					result := false;
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Trashbin clearing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
				end;
		end;
	end;
end;

function TCloudMailRu.mountFolder(home, invite_token, ConflictMode: WideString): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;

	result := self.HTTPPostForm(API_FOLDER_MOUNT, 'home=' + UrlEncode(home) + '&invite_token=' + invite_token + self.united_params + '&conflict=' + ConflictMode, JSON);

	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := true;
			else
				begin
					result := false;
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Folder mount error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
				end;
		end;
	end;
end;

function TCloudMailRu.unmountFolder(home: WideString; clone_copy: Boolean): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
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

	result := self.HTTPPostForm(API_FOLDER_UNMOUNT, 'home=' + UrlEncode(home) + '&clone_copy=' + CopyStr + self.united_params, JSON);

	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := true;
			else
				begin
					result := false;
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Folder mount error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
				end;
		end;
	end;
end;

function TCloudMailRu.rejectInvite(invite_token: WideString): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;

	result := self.HTTPPostForm(API_INVITE_REJECT, 'invite_token=' + invite_token + self.united_params, JSON);

	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := true;
			else
				begin
					result := false;
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Folder mount error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
				end;
		end;
	end;
end;

function TCloudMailRu.putFileWhole(localPath, remotePath, ConflictMode: WideString): integer;
var
	PutResult: TStringList;
	JSONAnswer: WideString;
	LocalFileHash, UploadedFileHash: WideString;
	LocalFileSize, UploadedFileSize: int64;
	code, OperationStatus: integer;
	OperationResult: integer;
begin
	LocalFileSize := 0;
	UploadedFileSize := 0;
	result := FS_FILE_WRITEERROR;
	OperationResult := CLOUD_OPERATION_FAILED;
	PutResult := TStringList.Create;
	self.ExternalSourceName := PWideChar(localPath);
	self.ExternalTargetName := PWideChar(remotePath);

	if self.PrecalculateHash or self.CheckCRC then
	begin
		LocalFileHash := cloudHash(localPath);
		LocalFileSize := SizeOfFile(localPath);
	end;
	if self.PrecalculateHash and (LocalFileHash <> EmptyWideStr) and (not self.crypt_files) and (self.addByHash(LocalFileHash, LocalFileSize, localPath, remotePath)) then {issue #135}
		exit(CLOUD_OPERATION_OK);

	try
		OperationResult := self.putFileToCloud(localPath, PutResult);
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
		UploadedFileHash := PutResult.Strings[0];
		val(PutResult.Strings[1], UploadedFileSize, code);
		if self.CheckCRC then
		begin
			if (LocalFileSize <> UploadedFileSize) or (LocalFileHash <> UploadedFileHash) then {При включённой проверке CRC сравниваем хеши и размеры}
				result := CLOUD_OPERATION_FAILED;

		end;
	end else if OperationResult = CLOUD_OPERATION_CANCELLED then
	begin
		result := FS_FILE_USERABORT;
	end;
	PutResult.free;
	if OperationResult = CLOUD_OPERATION_OK then
	begin
		if self.addFileToCloud(UploadedFileHash, UploadedFileSize, remotePath, JSONAnswer, ConflictMode) then
		begin
			OperationResult := fromJSON_OperationResult(JSONAnswer, OperationStatus);
			result := CloudResultToFsResult(OperationResult, OperationStatus, 'File uploading error: ');
		end;
	end;
	self.ExternalSourceName := nil;
	self.ExternalTargetName := nil;
end;

function TCloudMailRu.putFileChunk(localPath, remotePath: WideString; ChunkInfo: TFileChunkInfo; ConflictMode: WideString): integer;
var
	JSONAnswer: WideString;
	LocalChunkHash, UploadedChunkHash: WideString;
	code, OperationResult, OperationStatus: integer;
	PutResult: TStringList;
	UploadedChunkSize: int64;
begin
	OperationResult := CLOUD_OPERATION_FAILED;
	result := CLOUD_OPERATION_FAILED;
	UploadedChunkSize := 0;
	self.ExternalSourceName := PWideChar(localPath);
	self.ExternalTargetName := PWideChar(remotePath);

	if self.PrecalculateHash or self.CheckCRC then
	begin
		LocalChunkHash := cloudHash(localPath, @ChunkInfo);
	end;
	if self.PrecalculateHash and (LocalChunkHash <> EmptyWideStr) and (not self.crypt_files) and (self.addByHash(LocalChunkHash, ChunkInfo.size, localPath, remotePath)) then {issue #135}
		exit(CLOUD_OPERATION_OK);

	PutResult := TStringList.Create;
	try
		OperationResult := self.putFileToCloud(localPath, PutResult, @ChunkInfo);
	except
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

	if OperationResult = CLOUD_OPERATION_OK then {TODO: вытащить в функцию}
	begin
		UploadedChunkHash := PutResult.Strings[0];
		val(PutResult.Strings[1], UploadedChunkSize, code);
		if self.CheckCRC then
		begin
			if (ChunkInfo.size <> UploadedChunkSize) or (LocalChunkHash <> UploadedChunkHash) then {При включённой проверке CRC сравниваем хеши и размеры}
				result := CLOUD_OPERATION_FAILED;
		end;
	end else if OperationResult = CLOUD_OPERATION_CANCELLED then
	begin
		result := FS_FILE_USERABORT;
	end;
	PutResult.Destroy;

	if OperationResult = CLOUD_OPERATION_OK then
	begin
		if self.addFileToCloud(UploadedChunkHash, UploadedChunkSize, remotePath, JSONAnswer, ConflictMode) then
		begin
			OperationResult := fromJSON_OperationResult(JSONAnswer, OperationStatus);
			result := CloudResultToFsResult(OperationResult, OperationStatus, 'File uploading error: ');
		end;
	end;

	self.ExternalSourceName := nil;
	self.ExternalTargetName := nil;
end;

function TCloudMailRu.putFileChunkCRC(remotePath: WideString; SplitFileInfo: TFileSplitInfo; ConflictMode: WideString): integer;
var
	CRCStream: TMemoryStream;
	CRCRemotePath, UploadedCRCHash: WideString;
	code, OperationResult, OperationStatus: integer;
	UploadedCRCSize: int64;

	PutResult: TStringList;
	JSONAnswer: WideString;
begin
	OperationResult := CLOUD_OPERATION_FAILED;
	result := CLOUD_OPERATION_FAILED;
	UploadedCRCSize := 0;
	CRCRemotePath := ExtractFilePath(remotePath) + SplitFileInfo.CRCFileName;
	self.ExternalTargetName := PWideChar(CRCRemotePath);
	CRCStream := TMemoryStream.Create;
	SplitFileInfo.GetCRCData(CRCStream);
	PutResult := TStringList.Create;
	try
		self.putFileToCloud(SplitFileInfo.CRCFileName, CRCStream, PutResult);
	except
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
		UploadedCRCHash := PutResult.Strings[0];
		val(PutResult.Strings[1], UploadedCRCSize, code);
		if self.CheckCRC then
		begin
			if (CRCStream.size <> UploadedCRCSize) or (self.cloudHash(CRCStream, CRCRemotePath) <> UploadedCRCHash) then {При включённой проверке CRC сравниваем хеши и размеры}
				result := CLOUD_OPERATION_FAILED;
		end;
	end else if OperationResult = CLOUD_OPERATION_CANCELLED then
	begin
		result := FS_FILE_USERABORT;
	end;
	PutResult.Destroy;

	if OperationResult = CLOUD_OPERATION_OK then
	begin
		if self.addFileToCloud(UploadedCRCHash, UploadedCRCSize, CRCRemotePath, JSONAnswer, ConflictMode) then
		begin
			OperationResult := fromJSON_OperationResult(JSONAnswer, OperationStatus);
			result := CloudResultToFsResult(OperationResult, OperationStatus, 'File uploading error: ');
		end;
	end;

	self.ExternalSourceName := nil;
	self.ExternalTargetName := nil;

	CRCStream.Destroy;
end;

function TCloudMailRu.putFileSplit(localPath, remotePath, ConflictMode: WideString; ChunkOverwriteMode: integer): integer;
var
	SplitFileInfo: TFileSplitInfo;
	SplittedPartIndex: integer;
	ChunkRemotePath, LocalFileHash: WideString;

begin

	if self.PrecalculateHash then //try to add whole file by hash at first.
	begin
		LocalFileHash := cloudHash(localPath);
	end;
	if self.PrecalculateHash and (LocalFileHash <> EmptyWideStr) and (not self.crypt_files) and (self.addByHash(LocalFileHash, SizeOfFile(localPath), localPath, remotePath)) then {issue #135}
		exit(CLOUD_OPERATION_OK);

	SplitFileInfo := TFileSplitInfo.Create(localPath, self.split_file_size); //quickly get information about file parts

	for SplittedPartIndex := 0 to SplitFileInfo.ChunksCount - 1 do
	begin
		ChunkRemotePath := ExtractFilePath(remotePath) + SplitFileInfo.GetChunks[SplittedPartIndex].name;
		result := self.putFileChunk(localPath, ChunkRemotePath, SplitFileInfo.GetChunks[SplittedPartIndex], ConflictMode);

		case result of
			FS_FILE_OK:
				Continue;
			FS_FILE_USERABORT:
				begin
					Log(LogLevelDetail, MSGTYPE_DETAILS, 'Partial upload aborted.');
					SplitFileInfo.Destroy;
					exit(FS_FILE_USERABORT);
				end;
			FS_FILE_EXISTS:
				begin
					case ChunkOverwriteMode of
						ChunkOverwrite: //silently overwrite chunk
							begin
								Log(LogLevelWarning, MSGTYPE_DETAILS, 'Chunk ' + ChunkRemotePath + ' already exists, overwriting.');
								if not(self.deleteFile(ChunkRemotePath)) then
								begin
									SplitFileInfo.Destroy;
									exit(FS_FILE_WRITEERROR);
								end else if (self.putFileChunk(localPath, ChunkRemotePath, SplitFileInfo.GetChunks[SplittedPartIndex], ConflictMode) <> FS_FILE_OK) then
								begin
									SplitFileInfo.Destroy;
									exit(FS_FILE_WRITEERROR);
								end;

							end;
						ChunkOverwriteIgnore: //ignore this chunk
							begin
								Log(LogLevelWarning, MSGTYPE_DETAILS, 'Chunk ' + ChunkRemotePath + ' already exists, skipping.');
								Continue; //ignore and continue
							end;
						ChunkOverwriteAbort: //abort operation
							begin
								Log(LogLevelWarning, MSGTYPE_DETAILS, 'Chunk ' + ChunkRemotePath + ' already exists, aborting.');
								SplitFileInfo.Destroy;
								exit(FS_FILE_NOTSUPPORTED);
							end;
					end;
				end;
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Partial upload error, code: ' + result.ToString);
					SplitFileInfo.Destroy;
					exit(CLOUD_OPERATION_FAILED);
				end;
		end;
	end;
	putFileChunkCRC(remotePath, SplitFileInfo, ConflictMode); {Заливаем CRC-файл, обработка ошибок внутри функции}
	exit(FS_FILE_OK); //Файлик залит по частям, выходим
end;

{Wrapper for putFileWhole/putFileSplit}
function TCloudMailRu.putFile(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: integer = 0): integer;
begin
	if not(Assigned(self)) then
		exit(FS_FILE_WRITEERROR); //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);

	if (not(self.unlimited_filesize)) and (SizeOfFile(GetUNCFilePath(localPath)) > self.split_file_size) then
	begin
		if self.split_large_files then
		begin
			Log(LogLevelDetail, MSGTYPE_DETAILS, 'File size > ' + self.split_file_size.ToString() + ' bytes, file will be splitted.');
			exit(putFileSplit(localPath, remotePath, ConflictMode, ChunkOverwriteMode));
		end else begin
			Log(LogLevelWarning, MSGTYPE_IMPORTANTERROR, 'File size > ' + self.split_file_size.ToString() + ' bytes, ignored.');
			exit(FS_FILE_NOTSUPPORTED);
		end;
	end;
	exit(putFileWhole(localPath, remotePath, ConflictMode));

end;

function TCloudMailRu.putFileToCloud(localPath: WideString; Return: TStringList; ChunkInfo: PFileChunkInfo = nil): integer; {Заливка на сервер состоит из двух шагов: заливаем файл на сервер в putFileToCloud и добавляем его в облако addFileToCloud}
var
	PostAnswer: WideString;
begin
	result := CLOUD_OPERATION_FAILED;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if nil = ChunkInfo then

		result := self.HTTPPostFile(self.upload_url + '/?cloud_domain=1&x-email=' + self.user + '%40' + self.domain + '&fileapi' + DateTimeToUnix(now).ToString + '0246', GetUNCFilePath(localPath), PostAnswer)
	else
		result := self.HTTPPostFile(self.upload_url + '/?cloud_domain=1&x-email=' + self.user + '%40' + self.domain + '&fileapi' + DateTimeToUnix(now).ToString + '0246', GetUNCFilePath(localPath), PostAnswer, ChunkInfo^);
	if (result = CLOUD_OPERATION_OK) then
	begin
		ExtractStrings([';'], [], PWideChar(PostAnswer), Return);
		if length(Return.Strings[0]) <> 40 then //? добавить анализ ответа?
		begin
			result := CLOUD_OPERATION_FAILED;
		end
	end;
end;

function TCloudMailRu.putFileToCloud(FileName: WideString; FileStream: TStream; Return: TStringList): integer;
var
	PostAnswer: WideString;
begin
	result := CLOUD_OPERATION_FAILED;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	result := self.HTTPPostFile(self.upload_url + '/?cloud_domain=1&x-email=' + self.user + '%40' + self.domain + '&fileapi' + DateTimeToUnix(now).ToString + '0246', FileName, FileStream, PostAnswer);
	if (result = CLOUD_OPERATION_OK) then
	begin
		ExtractStrings([';'], [], PWideChar(PostAnswer), Return);
		if length(Return.Strings[0]) <> 40 then //? добавить анализ ответа?
		begin
			result := CLOUD_OPERATION_FAILED;
		end
	end;
end;

function TCloudMailRu.removeDir(Path: WideString): Boolean;
var
	JSON: WideString;
	OperationResult, OperationStatus: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	result := self.HTTPPostForm(API_FILE_REMOVE, 'home=/' + IncludeSlash(PathToUrl(Path)) + self.united_params + '&conflict', JSON); //API всегда отвечает true, даже если путь не существует
	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := true;
			else
				begin
					result := false;
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Delete directory error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
				end;
		end;
	end;
end;

function TCloudMailRu.renameFile(OldName, NewName: WideString): integer;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if self.HTTPPostForm(API_FILE_RENAME, 'home=' + PathToUrl(OldName) + '&name=' + PathToUrl(NewName) + self.united_params, JSON) then
	begin //Парсим ответ
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		result := CloudResultToFsResult(OperationResult, OperationStatus, 'Rename file error: ');
	end;
end;

function TCloudMailRu.statusFile(Path: WideString; var FileInfo: TCloudMailRuDirListingItem): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
	OperationResult, OperationStatus: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	if self.public_account then
		result := self.HTTPGetPage(API_FILE + '?weblink=' + IncludeSlash(self.public_link) + PathToUrl(Path) + self.united_params, JSON, Progress)
	else
		result := self.HTTPGetPage(API_FILE + '?home=' + PathToUrl(Path) + self.united_params, JSON, Progress);

	if result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				result := fromJSON_FileStatus(JSON, FileInfo);
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'File status error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					result := false;
				end;
		end;
	end;
	//if not Result then exit(false);
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

function TCloudMailRu.cloudHash(Path: WideString; ChunkInfo: PFileChunkInfo = nil): WideString;
var
	Stream: TStream;
begin
	result := EmptyWideStr;
	if not FileExists(Path) then
		exit;

	try
		if (nil = ChunkInfo) then
		begin
			Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
		end else begin
			Stream := TChunkedFileStream.Create(Path, fmOpenRead or fmShareDenyWrite, ChunkInfo^.start, ChunkInfo^.size);
		end;
	except
		exit;
	end;
	result := cloudHash(Stream, Path);
	Stream.free;

end;

function TCloudMailRu.cloudHash(Stream: TStream; SourceName: WideString = ''): WideString;
const
	bufSize = 8192;
var
	sha1: THashSHA1;
	buffer: array [0 .. bufSize - 1] of byte;
	read: LongInt;
	initBuffer, finalBuffer: TBytes;
	Percent, iteration: integer;
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
		Percent := Round((8192 * iteration / Stream.size) * 100);
		if Percent > 100 then
			Percent := 100;

		read := Stream.read(buffer, bufSize);
		sha1.Update(buffer, read);
		if (1 = ExternalProgressProc(PWideChar(SourceName), 'Calculating cloud hash', Percent)) then
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
