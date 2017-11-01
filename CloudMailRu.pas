unit CloudMailRu;

interface

uses CMLJSON, CMLTypes, System.Classes, System.Generics.Collections, System.SysUtils, PLUGIN_Types, Winapi.Windows, IdStack, MRC_helper, Settings, IdCookieManager, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdSocks, IdHTTP, IdAuthentication, IdIOHandlerStream, FileSplitter, IdCookie, IdMultipartFormData, Cipher;

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
		token: WideString;
		OAuthToken: TCloudMailRuOAuthInfo;
		x_page_id: WideString;
		build: WideString;
		upload_url: WideString;
		login_method: integer;
		Cookie: TIdCookieManager;
		Socks: TIdSocksInfo;
		ExternalProgressProc: TProgressHandler;
		ExternalLogProc: TLogHandler;
		ExternalRequestProc: TRequestHandler;
		Shard: WideString;
		Proxy: TProxySettings;
		ConnectTimeout: integer;

		united_params: WideString; //Объединённый набор авторизационных параметров для подстановки в URL

		{BASE HTTP METHODS}
		procedure HTTPInit(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL; var Socks: TIdSocksInfo; var Cookie: TIdCookieManager);
		procedure HTTPDestroy(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL);
		function HTTPGet(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean; //если ProgressEnabled - включаем обработчик onWork, возвращаем ProgressEnabled=false при отмене
		function HTTPGetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean = true): integer;
		function HTTPPostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'): Boolean; //Постинг данных с возможным получением ответа.
		function HTTPPostMultipart(URL: WideString; Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean;
		function HTTPPostFile(URL: WideString; FileName: WideString; var Answer: WideString): integer; //Постинг файла и получение ответа

    
		procedure HTTPProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
		{RAW TEXT PARSING}
		function extractTokenFromText(Text: WideString; var token: WideString): Boolean;
		function extractPublicTokenFromText(Text: WideString; var PublicToken: WideString): Boolean;
		function extract_x_page_id_FromText(Text: WideString; var PageId: WideString): Boolean;
		function extract_build_FromText(Text: WideString; var build: WideString): Boolean;
		function extract_upload_url_FromText(Text: WideString; var UploadUrl: WideString): Boolean;
		function extractPublicShard(Text: WideString; var Shard: WideString): Boolean;
		function extractTwostepJson(Text: WideString; var JSON: WideString): Boolean;
		{JSON MANIPULATION}
		//Moved to CMLJSON
		{HTTP REQUESTS WRAPPERS}
		function getToken(): Boolean;
		function getSharedToken(): Boolean;
		function getOAuthToken(var OAuthToken: TCloudMailRuOAuthInfo): Boolean;
		function getShard(var Shard: WideString): Boolean;
		function getUserSpace(var SpaceInfo: TCloudMailRuSpaceInfo): Boolean;
		function putFileToCloud(localPath: WideString; Return: TStringList): integer;
		function addFileToCloud(hash: WideString; size: int64; remotePath: WideString; var JSONAnswer: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): Boolean;
		{OTHER ROUTINES}
		procedure Log(LogLevel, MsgType: integer; LogString: WideString);
		function CloudResultToFsResult(CloudResult: integer; OperationStatus: integer; ErrorPrefix: WideString = ''): integer;
	protected
		{REGULAR CLOUD}
		function loginRegular(method: integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function getFileRegular(remotePath, localPath: WideString; LogErrors: Boolean = true): integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		{SHARED WEBFOLDERS}
		function loginShared(method: integer = CLOUD_AUTH_METHOD_WEB): Boolean;

		function getFileShared(remotePath, localPath: WideString; LogErrors: Boolean = true): integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
	public
		crypt_files: Boolean;
		crypt_filenames: Boolean;
		crypt_files_password: WideString;
		crypt_filenames_password: WideString;

		Property isPublicShare: Boolean read public_account;
		Property ProxySettings: TProxySettings read Proxy;
		Property ConnectTimeoutValue: integer read ConnectTimeout;
		Property CryptFilesPassword: WideString read crypt_files_password write crypt_files_password; //deprecated
		Property CryptFileNamesPassword: WideString read crypt_filenames_password write crypt_filenames_password; //deprecated
		function isCryptFilesPasswordRequired(): Boolean;
		function isCryptFileNamesPasswordRequired(): Boolean;
		function getSharedFileUrl(remotePath: WideString; DoUrlEncode: Boolean = true): WideString;
		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(AccountSettings: TAccountSettings; split_file_size: integer; Proxy: TProxySettings; ConnectTimeout: integer; ExternalProgressProc: TProgressHandler = nil; ExternalLogProc: TLogHandler = nil; ExternalRequestProc: TRequestHandler = nil);
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
		function getFile(remotePath, localPath: WideString; LogErrors: Boolean = true): integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		function putFile(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: integer = 0): integer;
		function renameFile(OldName, NewName: WideString): integer; //смена имени без перемещения
		function moveFile(OldName, ToPath: WideString): integer; //перемещение по дереву каталогов
		function copyFile(OldName, ToPath: WideString): integer; //Копирование файла внутри одного каталога
		function mvFile(OldName, NewName: WideString): integer; //объединяющая функция, определяет делать rename или move
		function cpFile(OldName, NewName: WideString): integer; //Копирует файл, и переименует, если нужно
		function deleteFile(Path: WideString): Boolean;
		function publishFile(Path: WideString; var PublicLink: WideString; publish: Boolean = CLOUD_PUBLISH): Boolean;
		function cloneWeblink(Path, link: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): integer; //клонировать публичную ссылку в текущий каталог
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
		function putFileSplit(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: integer = 0): integer;
		{STATIC ROUTINES}
		class function CloudAccessToString(access: WideString; Invert: Boolean = false): WideString; static;
		class function StringToCloudAccess(accessString: WideString; Invert: Boolean = false): integer; static;
		class function ErrorCodeText(ErrorCode: integer): WideString; static;
	end;

implementation

{TCloudMailRu}

function TCloudMailRu.addFileToCloud(hash: WideString; size: int64; remotePath: WideString; var JSONAnswer: WideString; ConflictMode: WideString): Boolean;
var
	Cipher: TCipher;
	FileName: WideString;
begin
	{Экспериментально выяснено, что параметры api, build, email, x-email, x-page-id в запросе не обязательны}
	if self.crypt_files and self.crypt_filenames then
	begin
		Cipher := TCipher.Create(self.crypt_files_password, self.crypt_filenames_password);
		FileName := ExtractUniversalFileName(remotePath);
		FileName := Cipher.CryptFileName(FileName);
		remotePath := ChangePathFileName(remotePath, FileName);
		Cipher.free;
	end;
	Result := self.HTTPPostForm(API_FILE_ADD, 'conflict=' + ConflictMode + '&home=/' + PathToUrl(remotePath) + '&hash=' + hash + '&size=' + size.ToString + self.united_params, JSONAnswer);

end;

function TCloudMailRu.cloneWeblink(Path, link, ConflictMode: WideString): integer;
var
	JSON: WideString;
	OperationStatus: integer;
	Progress: Boolean;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);
	if self.HTTPGet(API_CLONE + '?folder=' + PathToUrl(Path) + '&weblink=' + link + '&conflict=' + ConflictMode + self.united_params, JSON, Progress) then
	begin //Парсим ответ
		Result := fromJSON_OperationResult(JSON, OperationStatus);
		if Result <> CLOUD_OPERATION_OK then
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'File publish error: ' + self.ErrorCodeText(Result) + ' Status: ' + OperationStatus.ToString());

	end else begin //посмотреть это
		if not(Progress) then
		begin //user cancelled
			Result := FS_FILE_USERABORT;
		end else begin //unknown error
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Public link clone error: got ' + OperationStatus.ToString + ' status');
			Result := FS_FILE_WRITEERROR;
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
				if (ErrorPrefix <> '') then
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
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);
	if self.HTTPPostForm(API_FILE_COPY, 'home=' + PathToUrl(OldName) + '&folder=' + PathToUrl(ToPath) + self.united_params + '&conflict', JSON) then
	begin //Парсим ответ
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		Result := CloudResultToFsResult(OperationResult, OperationStatus, 'File copy error: ');
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
		Result := self.copyFile(OldName, NewPath);
		if Result <> CLOUD_OPERATION_OK then
			exit;
	end;
	if not(SameName) then
	begin //скопированный файл лежит в новом каталоге со старым именем
		Result := self.renameFile(NewPath + ExtractFileName(OldName), ExtractFileName(NewName));
	end;
end;

constructor TCloudMailRu.Create(AccountSettings: TAccountSettings; split_file_size: integer; Proxy: TProxySettings; ConnectTimeout: integer; ExternalProgressProc: TProgressHandler = nil; ExternalLogProc: TLogHandler = nil; ExternalRequestProc: TRequestHandler = nil);
begin
	try
		self.Cookie := TIdCookieManager.Create();
		self.Proxy := Proxy;
		if Proxy.ProxyType in SocksProxyTypes then //SOCKS proxy initialization
		begin
			self.Socks := TIdSocksInfo.Create();
			self.Socks.Host := Proxy.Server;
			self.Socks.Port := Proxy.Port;
			if Proxy.user <> '' then
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
		self.crypt_files := AccountSettings.encrypt_files_mode <> EncryptModeNone;
		self.crypt_filenames := AccountSettings.encrypt_filenames;
		self.shard_override := AccountSettings.shard_override;
		if self.public_account and (self.PUBLIC_URL <> '') then
		begin
			self.public_link := self.PUBLIC_URL;
			self.PUBLIC_URL := IncludeSlash(self.PUBLIC_URL);
			Delete(self.public_link, 1, length(PUBLIC_ACCESS_URL));
			if self.public_link[length(self.public_link)] = '/' then
				Delete(self.public_link, length(self.public_link), 1);
		end;

		self.split_file_size := split_file_size;
		self.ConnectTimeout := ConnectTimeout;
		self.ExternalProgressProc := ExternalProgressProc;
		self.ExternalLogProc := ExternalLogProc;
		self.ExternalRequestProc := ExternalRequestProc;

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
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if self.HTTPPostForm(API_FOLDER_ADD, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', PostAnswer) then
	begin
		OperationResult := fromJSON_OperationResult(PostAnswer, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := true;
			else
				begin
					//Log(MSGTYPE_IMPORTANTERROR, 'Directory creation error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					Result := false;
				end;
		end;
	end;
end;

function TCloudMailRu.deleteFile(Path: WideString): Boolean;
var
	JSON: WideString;
	OperationResult, OperationStatus: integer;
begin
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	Result := self.HTTPPostForm(API_FILE_REMOVE, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', JSON);
	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := true;
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Delete file error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					Result := false;
				end;
		end;
	end;
end;

destructor TCloudMailRu.Destroy;
begin
	if Assigned(self.Cookie) then
		self.Cookie.free;
	if Assigned(self.Socks) then
		self.Socks.free;

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

function TCloudMailRu.extractTokenFromText(Text: WideString; var token: WideString): Boolean;
var
	start: integer;
begin
	Result := false;
	start := Pos(WideString('"csrf"'), Text);
	if start > 0 then
	begin
		token := copy(Text, start + 8, 32);
		Result := true;
	end;
end;

function TCloudMailRu.extractTwostepJson(Text: WideString; var JSON: WideString): Boolean;
var
	start, finish: integer;
begin
	Result := false;
	start := Pos(WideString('<script type="text/html" id="json">'), Text);
	finish := Pos(WideString('</script>'), Text);
	if (start > 0) and (finish > 0) then
	begin
		JSON := copy(Text, start + 35, finish - start - 35);

		Result := true;
	end;
end;

function TCloudMailRu.extract_build_FromText(Text: WideString; var build: WideString): Boolean;
var
	start, finish: integer;
	temp: WideString;
begin
	Result := false;
	start := Pos(WideString('"BUILD"'), Text);
	if start > 0 then
	begin
		temp := copy(Text, start + 9, 100);
		finish := Pos(WideString('"'), temp);
		build := copy(temp, 0, finish - 1);
		Result := true;
	end;
end;

function TCloudMailRu.extract_upload_url_FromText(Text: WideString; var UploadUrl: WideString): Boolean;
var
	start, start1, start2, finish, length: Cardinal;
	temp: WideString;
begin
	Result := false;
	start := Pos(WideString('mail.ru/upload/"'), Text);
	if start > 0 then
	begin
		start1 := start - 50;
		finish := start + 15;
		length := finish - start1;
		temp := copy(Text, start1, length);
		start2 := Pos(WideString('https://'), temp);
		UploadUrl := copy(temp, start2, StrLen(PWideChar(temp)) - start2);
		Result := true;
	end;
end;

function TCloudMailRu.extract_x_page_id_FromText(Text: WideString; var PageId: WideString): Boolean;
var
	start: integer;
begin
	Result := false;
	start := Pos(WideString('"x-page-id"'), Text);
	if start > 0 then
	begin
		PageId := copy(Text, start + 13, 10);
		Result := true;
	end;
end;

function TCloudMailRu.extractPublicShard(Text: WideString; var Shard: WideString): Boolean;
var
	start: integer;
	finish: integer;
begin
	start := Pos(WideString('"weblink_get":['), Text);
	Result := start <> 0;
	start := Pos(WideString('"url":'), Text, start) + 7;
	finish := Pos(WideString('"}]'), Text, start);
	Shard := copy(Text, start, finish - start);
end;

function TCloudMailRu.extractPublicTokenFromText(Text: WideString; var PublicToken: WideString): Boolean;
var
	start: integer;
	finish: integer;
begin
	start := Pos(WideString('"tokens":{"download":'), Text);
	Result := start <> 0;
	start := start + 22;
	finish := Pos(WideString('"}'), Text, start);
	PublicToken := copy(Text, start, finish - start);
end;

function TCloudMailRu.getDescriptionFile(remotePath, localCopy: WideString): Boolean;
begin
	Result := self.getFile(remotePath, localCopy, false) = FS_FILE_OK;
end;

function TCloudMailRu.putDesriptionFile(remotePath, localCopy: WideString): Boolean;
begin
	if FileExists(localCopy) then
		Result := self.putFile(localCopy, remotePath) = FS_FILE_OK
	else
		Result := self.deleteFile(remotePath);
end;

function TCloudMailRu.getSharedLinksListing(var DirListing: TCloudMailRuDirListing; ShowProgress: Boolean = false): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	Result := self.HTTPGet(API_FOLDER_SHARED_LINKS + '?' + self.united_params, JSON, ShowProgress);

	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := fromJSON_DirListing(JSON, DirListing);
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Shared links listing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					Result := false;
				end;
		end;
	end;
end;

function TCloudMailRu.getIncomingLinksListing(var IncomingListing: TCloudMailRuIncomingInviteInfoListing; ShowProgress: Boolean): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	Result := self.HTTPGet(API_FOLDER_SHARED_INCOMING + '?' + self.united_params, JSON, ShowProgress);

	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := fromJSON_IncomingInviteListing(JSON, IncomingListing);
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Incoming requests listing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					Result := false;
				end;
		end;
	end;
end;

function TCloudMailRu.getIncomingLinksListing(var IncomingListing: TCloudMailRuDirListing; var InvitesListing: TCloudMailRuIncomingInviteInfoListing; ShowProgress: Boolean = false): Boolean;
var
	i: integer;
begin
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

function TCloudMailRu.getTrashbinListing(var DirListing: TCloudMailRuDirListing; ShowProgress: Boolean): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	Result := self.HTTPGet(API_TRASHBIN + '?' + self.united_params, JSON, ShowProgress);

	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := fromJSON_DirListing(JSON, DirListing);
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Incoming requests listing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					Result := false;
				end;
		end;
	end;
end;

function TCloudMailRu.getDirListing(Path: WideString; var DirListing: TCloudMailRuDirListing; ShowProgress: Boolean = false): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации

	if self.public_account then
		Result := self.HTTPGet(API_FOLDER + '&weblink=' + IncludeSlash(self.public_link) + PathToUrl(Path, false) + self.united_params, JSON, ShowProgress)
	else
		Result := self.HTTPGet(API_FOLDER + '&home=' + PathToUrl(Path) + self.united_params, JSON, ShowProgress);
	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := fromJSON_DirListing(JSON, DirListing);
			CLOUD_ERROR_NOT_EXISTS:
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Path not exists: ' + Path);
					Result := false;
				end
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Directory listing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString()); //?? WUT
					Result := false;
				end;
		end;
	end;
end;

function TCloudMailRu.getFile(remotePath, localPath: WideString; LogErrors: Boolean): integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		Result := self.getFileShared(remotePath, localPath, LogErrors)
	else
		Result := self.getFileRegular(remotePath, localPath, LogErrors);
end;

function TCloudMailRu.getFileRegular(remotePath, localPath: WideString; LogErrors: Boolean): integer;
var
	FileStream: TFileStream;
	Cipher: TCipher;
	FileName: WideString;
	MemoryStream: TMemoryStream;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if self.Shard = '' then
	begin
		Log(LogLevelDetail, MSGTYPE_DETAILS, 'Current shard is undefined, trying to get one');
		if self.getShard(self.Shard) then
		begin
			Log(LogLevelDetail, MSGTYPE_DETAILS, 'Current shard: ' + self.Shard);
		end else begin //А вот теперь это критическая ошибка, тут уже не получится копировать
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Sorry, downloading impossible');
			exit;
		end;
	end;
	if self.crypt_files and self.crypt_filenames then
	begin
		Cipher := TCipher.Create(self.crypt_files_password, self.crypt_filenames_password);
		FileName := ExtractUniversalFileName(remotePath);
		FileName := Cipher.DecryptFileName(FileName);
		localPath := ChangePathFileName(localPath, FileName);
		Cipher.free;
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
		Result := self.HTTPGetFile(self.Shard + PathToUrl(remotePath, false), MemoryStream, LogErrors);
		if Result in [FS_FILE_OK] then
		begin
			MemoryStream.Position := 0;
			Cipher := TCipher.Create(self.crypt_files_password, self.crypt_filenames_password);
			Cipher.DecryptStream(MemoryStream, FileStream);
			Cipher.free;
		end;
		MemoryStream.free;

	end else begin
		Result := self.HTTPGetFile(self.Shard + PathToUrl(remotePath, false), FileStream, LogErrors);
	end;

	FlushFileBuffers(FileStream.Handle);
	FileStream.free;
	self.ExternalSourceName := nil;
	self.ExternalTargetName := nil;

	if not Result in [FS_FILE_OK] then
		System.SysUtils.deleteFile(GetUNCFilePath(localPath));
end;

function TCloudMailRu.getSharedFileUrl(remotePath: WideString; DoUrlEncode: Boolean = true): WideString;
begin
	Result := IncludeSlash(self.public_shard) + IncludeSlash(self.public_link) + PathToUrl(remotePath, true, DoUrlEncode) + '?key=' + self.public_download_token
end;

function TCloudMailRu.getFileShared(remotePath, localPath: WideString; LogErrors: Boolean): integer;
var
	FileStream: TFileStream;
begin
	Result := FS_FILE_NOTFOUND;
	if (self.public_shard = '') or (self.public_download_token = '') then
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
		Result := self.HTTPGetFile(getSharedFileUrl(remotePath), FileStream, LogErrors);
		FlushFileBuffers(FileStream.Handle);
		FileStream.free;
	end;
	if Result <> FS_FILE_OK then
		System.SysUtils.deleteFile(GetUNCFilePath(localPath));
end;

function TCloudMailRu.getOAuthToken(var OAuthToken: TCloudMailRuOAuthInfo): Boolean;
var
	Answer: WideString;
begin
	Result := false;
	if self.HTTPPostForm(OAUTH_TOKEN_URL, 'client_id=cloud-win&grant_type=password&username=' + self.user + '%40' + self.domain + '&password=' + UrlEncode(self.password), Answer) then
	begin
		if not fromJSON_OAuthTokenInfo(Answer, OAuthToken) then
			exit(false);
		Result := OAuthToken.error_code = NOERROR;
	end;
end;

function TCloudMailRu.getShard(var Shard: WideString): Boolean;
var
	JSON: WideString;
	OperationResult, OperationStatus: integer;
begin
	Result := false;
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
				Result := fromJSON_Shard(JSON, Shard) and (Shard <> '');
			else
				begin
					Result := false;
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
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	Result := self.HTTPGet(TOKEN_URL, JSON, Progress);
	if Result then
	begin
		Result := self.extractTokenFromText(JSON, self.token) and self.extract_x_page_id_FromText(JSON, self.x_page_id) and self.extract_build_FromText(JSON, self.build) and self.extract_upload_url_FromText(JSON, self.upload_url);
		self.united_params := '&api=2&build=' + self.build + '&x-page-id=' + self.x_page_id + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&token=' + self.token + '&_=' + DateTimeToUnix(now).ToString + '810';
	end;
end;

function TCloudMailRu.getSharedToken(): Boolean;
var
	PageContent: WideString;
	Progress: Boolean;
begin
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	Result := self.HTTPGet(self.PUBLIC_URL, PageContent, Progress);
	if Result then
	begin
		PageContent := StringReplace(PageContent, #$A, '', [rfReplaceAll]); //так нам проще ковыряться в тексте
		PageContent := StringReplace(PageContent, #$D, '', [rfReplaceAll]);
		PageContent := StringReplace(PageContent, #9, '', [rfReplaceAll]);
		if not self.extractPublicTokenFromText(PageContent, self.public_download_token) then //refresh public download token
		begin
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Can''t get public share download token');
			exit(false);
		end;
		if not self.extractPublicShard(PageContent, self.public_shard) then
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
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	Result := self.HTTPGet(API_USER_SPACE + '?home=/' + self.united_params, JSON, Progress);
	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := fromJSON_UserSpace(JSON, SpaceInfo);
			else
				begin
					Result := false;
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'User space receiving error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
				end;
		end;
	end;
end;

procedure TCloudMailRu.HTTPDestroy(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL);
begin
	HTTP.free;
	SSL.free;
end;

function TCloudMailRu.HTTPGet(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
var
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
	Socks: TIdSocksInfo;
begin
	try
		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		if ProgressEnabled then
			HTTP.OnWork := self.HTTPProgress; //Вызов прогресса ведёт к возможности отменить получение списка каталогов и других операций, поэтому он нужен не всегда
		Answer := HTTP.Get(URL);
		self.HTTPDestroy(HTTP, SSL);
	Except
		on E: EAbort do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			Answer := E.Message;
			ProgressEnabled := false; //сообщаем об отмене
			exit(false);
		end;
		on E: EIdHTTPProtocolException do
		begin
			if HTTP.ResponseCode = 400 then
			begin {сервер вернёт 400, но нужно пропарсить результат для дальнейшего определения действий}
				Answer := E.ErrorMessage;
				Result := true;
			end else if HTTP.ResponseCode = 507 then //кончилось место
			begin
				Answer := E.ErrorMessage;
				Result := true;
			end else begin
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL + ', ответ сервера: ' + E.ErrorMessage);
				Result := false;
			end;
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			exit;
		end;
		on E: EIdSocketerror do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка сети: ' + E.Message + ' при запросе данных с адреса ' + URL);
			exit(false);
		end;
		on E: Exception do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при запросе данных с адреса ' + URL);
			exit(false);
		end;
	end;
	Result := Answer <> '';
end;

function TCloudMailRu.HTTPGetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean): integer;
var
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
	Socks: TIdSocksInfo;
begin
	Result := FS_FILE_OK;
	try
		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		HTTP.Request.ContentType := 'application/octet-stream';
		HTTP.Response.KeepAlive := true;
		HTTP.OnWork := self.HTTPProgress;
		HTTP.Get(URL, FileStream);
		if (HTTP.RedirectCount = HTTP.RedirectMaximum) and (FileStream.size = 0) then
		begin
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Достигнуто максимальное количество перенаправлений при запросе файла с адреса ' + URL);
			Result := FS_FILE_READERROR;
		end;
		self.HTTPDestroy(HTTP, SSL);
	except
		on E: EAbort do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			Result := FS_FILE_USERABORT;
		end;
		on E: EIdSocketerror do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			if LogErrors then
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка сети: ' + E.Message + ' при копировании файла с адреса ' + URL);
			Result := FS_FILE_READERROR;
		end;
		on E: Exception do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			if LogErrors then
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при копировании файла с адреса ' + URL);
			Result := FS_FILE_READERROR;
		end;

	end;
end;

procedure TCloudMailRu.HTTPInit(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL; var Socks: TIdSocksInfo; var Cookie: TIdCookieManager);
begin
	SSL := TIdSSLIOHandlerSocketOpenSSL.Create();
	HTTP := TIdHTTP.Create();
	if (self.Proxy.ProxyType in SocksProxyTypes) and (self.Socks.Enabled) then
		SSL.TransparentProxy := self.Socks;
	if self.Proxy.ProxyType = ProxyHTTP then
	begin
		HTTP.ProxyParams.ProxyServer := self.Proxy.Server;
		HTTP.ProxyParams.ProxyPort := self.Proxy.Port;
		if self.Proxy.user <> '' then
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
	HTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.57 Safari/537.17/TCWFX(' + PlatformX + ')';
	HTTP.Request.Connection := '';
end;

function TCloudMailRu.HTTPPostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString): Boolean;
var
	MemStream, PostData: TStringStream;
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
	Socks: TIdSocksInfo;
begin
	Result := true;
	MemStream := TStringStream.Create;
	PostData := TStringStream.Create(PostDataString, TEncoding.UTF8);

	try
		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		if ContentType <> '' then
			HTTP.Request.ContentType := ContentType;
		try
			HTTP.Post(URL, PostData, MemStream);
		except
			//on E: EIdOSSLCouldNotLoadSSLLibrary do
			on E: Exception do
			begin
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL);
				MemStream.free;
				PostData.free;
				self.HTTPDestroy(HTTP, SSL);
				Answer := E.Message;
				exit(false);
			end;

		end;
		self.HTTPDestroy(HTTP, SSL);
		Answer := MemStream.DataString;
	except
		on E: EAbort do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			MemStream.free;
			PostData.free;
			exit(false);
		end;
		on E: EIdHTTPProtocolException do
		begin
			if HTTP.ResponseCode = 400 then
			begin {сервер вернёт 400, но нужно пропарсить результат для дальнейшего определения действий}
				Answer := E.ErrorMessage;
				Result := true;
			end else if HTTP.ResponseCode = 507 then //кончилось место
			begin
				Answer := E.ErrorMessage;
				Result := true;
				//end else if (HTTP.ResponseCode = 500) then // Внезапно, сервер так отвечает, если при перемещении файл уже существует, но полагаться на это мы не можем
			end else begin
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL + ', ответ сервера: ' + E.ErrorMessage);
				Result := false;
			end;
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
		end;
		on E: EIdSocketerror do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка сети: ' + E.Message + ' при отправке данных на адрес ' + URL);
			Result := false;
		end;
		on E: Exception do
		begin
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL);
			Result := false;
		end;
	end;
	MemStream.free;
	PostData.free;
end;

function TCloudMailRu.HTTPPostMultipart(URL: WideString; Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean; //test
var
	MemStream: TStringStream;
	Fields: TIdMultipartFormDataStream;
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
	Socks: TIdSocksInfo;
	ParamItem: TPair<WideString, WideString>;
begin
	Result := true;
	MemStream := TStringStream.Create;

	Fields := TIdMultipartFormDataStream.Create;
	for ParamItem in Params do
		Fields.AddFormField(ParamItem.Key, ParamItem.Value);

	try
		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		try
			HTTP.Post(URL, Fields, MemStream);
		except
			//on E: EIdOSSLCouldNotLoadSSLLibrary do
			on E: Exception do
			begin
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL);
				MemStream.free;
				Fields.free;
				self.HTTPDestroy(HTTP, SSL);
				Answer := E.Message;
				exit(false);
			end;
		end;
		self.HTTPDestroy(HTTP, SSL);
		Answer := MemStream.DataString;
	except
		on E: EAbort do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			MemStream.free;
			Fields.free;
			exit(false);
		end;
		on E: EIdHTTPProtocolException do
		begin
			if HTTP.ResponseCode = 400 then
			begin {сервер вернёт 400, но нужно пропарсить результат для дальнейшего определения действий}
				Answer := E.ErrorMessage;
				Result := true;
			end else if HTTP.ResponseCode = 507 then //кончилось место
			begin
				Answer := E.ErrorMessage;
				Result := true;
				//end else if (HTTP.ResponseCode = 500) then // Внезапно, сервер так отвечает, если при перемещении файл уже существует, но полагаться на это мы не можем
			end else begin
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL + ', ответ сервера: ' + E.ErrorMessage);
				Result := false;
			end;
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
		end;
		on E: EIdSocketerror do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка сети: ' + E.Message + ' при отправке данных на адрес ' + URL);
			Result := false;
		end;
		on E: Exception do
		begin
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL);
			Result := false;
		end;
	end;
	MemStream.free;
	Fields.free;
end;

function TCloudMailRu.HTTPPostFile(URL: WideString; FileName: WideString; var Answer: WideString): integer;
var
	MemStream: TStringStream;
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
	Socks: TIdSocksInfo;
	PostData: TIdMultipartFormDataStream;
	Cipher: TCipher;

	MemoryStream: TMemoryStream;
	FileStream: TFileStream;
begin
	Result := CLOUD_OPERATION_OK;
	MemStream := TStringStream.Create;
	PostData := TIdMultipartFormDataStream.Create;

	if self.crypt_files then
	begin
		Cipher := TCipher.Create(self.crypt_files_password, self.crypt_filenames_password);
		MemoryStream := TMemoryStream.Create;
		FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
		Cipher.CryptStream(FileStream, MemoryStream);

		MemoryStream.Position := 0;
		PostData.AddFormField('file', 'application/octet-stream', '', MemoryStream, FileName);
		Cipher.free;
		FileStream.free;

	end else begin
		PostData.AddFile('file', FileName, 'application/octet-stream');
	end;

	try

		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		HTTP.OnWork := self.HTTPProgress;

		HTTP.Post(URL, PostData, MemStream);
		Answer := MemStream.DataString;
		self.HTTPDestroy(HTTP, SSL);
	except
		on E: EAbort do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			Result := CLOUD_OPERATION_CANCELLED;
		end;
		on E: EIdHTTPProtocolException do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL + ', ответ сервера: ' + E.ErrorMessage);
			Result := CLOUD_OPERATION_FAILED;
		end;
		on E: EIdSocketerror do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка сети: ' + E.Message + ' при отправке данных на адрес ' + URL);
			Result := CLOUD_OPERATION_FAILED;
		end;
		on E: Exception do
		begin
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL);
			Result := CLOUD_OPERATION_FAILED;
		end;
	end;
	MemStream.free;
	PostData.free;
	if self.crypt_files then
		MemoryStream.free;

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

function TCloudMailRu.isCryptFileNamesPasswordRequired: Boolean;
begin
	Result := self.crypt_filenames and (EmptyWideStr = self.crypt_filenames_password);

end;

function TCloudMailRu.isCryptFilesPasswordRequired: Boolean;
begin
	Result := self.crypt_files and (EmptyWideStr = self.crypt_files_password);
end;

procedure TCloudMailRu.Log(LogLevel, MsgType: integer; LogString: WideString);
begin
	if Assigned(ExternalLogProc) then
		ExternalLogProc(LogLevel, MsgType, PWideChar(LogString));
end;

function TCloudMailRu.login(method: integer): Boolean;
begin
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		Result := self.loginShared()
	else
		Result := self.loginRegular(method);
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
	Result := false;
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
				Result := self.HTTPPostMultipart(LOGIN_URL, FormFields, PostAnswer);
				if Result then
				begin
					Log(LogLevelDebug, MSGTYPE_DETAILS, 'Parsing authorization data...');
					if self.extractTwostepJson(PostAnswer, TwoStepJson) and fromJSON_TwostepData(TwoStepJson, TwostepData) then
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
							Result := self.HTTPPostMultipart(SECSTEP_URL, FormFields, PostAnswer);
							FormFields.free;
							if Result then
							begin
								Result := self.getToken();
								if (Result) then
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
				Result := self.HTTPPostForm(LOGIN_URL, 'page=https://cloud.mail.ru/?new_auth_form=1&Domain=' + self.domain + '&Login=' + self.user + '&Password=' + UrlEncode(self.password) + '&FailPage=', PostAnswer);
				if (Result) then
				begin
					Log(LogLevelDebug, MSGTYPE_DETAILS, 'Parsing token data...');
					Result := self.getToken();
					if (Result) then
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
				Result := self.getOAuthToken(self.OAuthToken);
				if not Result then
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'OAuth error: ' + self.OAuthToken.error + '(' + self.OAuthToken.error_description + ')');
			end;
	end;
end;

function TCloudMailRu.loginShared(method: integer): Boolean;
begin
	Log(LogLevelDetail, MSGTYPE_DETAILS, 'Open ' + self.PUBLIC_URL);
	Result := self.getSharedToken();
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
			QuotaInfo := '';
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
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);
	if self.HTTPPostForm(API_FILE_MOVE, 'home=' + PathToUrl(OldName) + '&folder=' + PathToUrl(ToPath) + self.united_params + '&conflict', JSON) then
	begin //Парсим ответ
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		Result := CloudResultToFsResult(OperationResult, OperationStatus, 'File move error: ');
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
		Result := self.renameFile(OldName, ExtractFileName(NewName));
	end else begin
		Result := self.moveFile(OldName, ExtractFilePath(NewName)); //Если файл со старым именем лежит в новом каталоге, вернётся ошибка. Так реализовано в облаке, а мудрить со временными каталогами я не хочу
		if Result <> CLOUD_OPERATION_OK then
			exit;
		if not(SameName) then
		begin //скопированный файл лежит в новом каталоге со старым именем
			Result := self.renameFile(NewPath + ExtractFileName(OldName), ExtractFileName(NewName));
		end;
	end;
end;

function TCloudMailRu.publishFile(Path: WideString; var PublicLink: WideString; publish: Boolean): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if publish then
	begin
		Result := self.HTTPPostForm(API_FILE_PUBLISH, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', JSON);
	end else begin
		Result := self.HTTPPostForm(API_FILE_UNPUBLISH, 'weblink=' + PublicLink + self.united_params + '&conflict', JSON);
	end;

	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				if publish then
					Result := fromJSON_PublicLink(JSON, PublicLink);
			else
				begin
					Result := false;
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
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	if self.HTTPGet(API_FOLDER_SHARED_INFO + '?home=' + PathToUrl(Path) + self.united_params, JSON, Progress) then
	begin
		Result := fromJSON_InviteListing(JSON, InviteListing);
	end;

end;

function TCloudMailRu.shareFolder(Path, email: WideString; access: integer): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
	access_string: WideString;
begin
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if access in [CLOUD_SHARE_RW, CLOUD_SHARE_RO] then
	begin
		if access = CLOUD_SHARE_RW then
			access_string := CLOUD_SHARE_ACCESS_READ_WRITE
		else
			access_string := CLOUD_SHARE_ACCESS_READ_ONLY;

		Result := self.HTTPPostForm(API_FOLDER_SHARE, 'home=/' + PathToUrl(Path) + self.united_params + '&invite={"email":"' + email + '","access":"' + access_string + '"}', JSON)
	end else begin
		Result := (self.HTTPPostForm(API_FOLDER_UNSHARE, 'home=/' + PathToUrl(Path) + self.united_params + '&invite={"email":"' + email + '"}', JSON));
	end;

	if (Result) then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);

		Result := OperationResult = CLOUD_OPERATION_OK;
		if not Result then
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Invite member error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());

	end;
end;

function TCloudMailRu.trashbinRestore(Path: WideString; RestoreRevision: integer; ConflictMode: WideString): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;

	Result := self.HTTPPostForm(API_TRASHBIN_RESTORE, 'path=' + PathToUrl(Path) + '&restore_revision=' + RestoreRevision.ToString + self.united_params + '&conflict=' + ConflictMode, JSON);

	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := true;
			else
				begin
					Result := false;
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
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;

	Result := self.HTTPPostForm(API_TRASHBIN_EMPTY, self.united_params, JSON);

	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := true;
			else
				begin
					Result := false;
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
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;

	Result := self.HTTPPostForm(API_FOLDER_MOUNT, 'home=' + UrlEncode(home) + '&invite_token=' + invite_token + self.united_params + '&conflict=' + ConflictMode, JSON);

	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := true;
			else
				begin
					Result := false;
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
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if clone_copy then
		CopyStr := 'true'
	else
		CopyStr := 'false';

	Result := self.HTTPPostForm(API_FOLDER_UNMOUNT, 'home=' + UrlEncode(home) + '&clone_copy=' + CopyStr + self.united_params, JSON);

	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := true;
			else
				begin
					Result := false;
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
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;

	Result := self.HTTPPostForm(API_INVITE_REJECT, 'invite_token=' + invite_token + self.united_params, JSON);

	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := true;
			else
				begin
					Result := false;
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Folder mount error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
				end;
		end;
	end;
end;

function TCloudMailRu.putFileSplit(localPath, remotePath, ConflictMode: WideString; ChunkOverwriteMode: integer): integer;
var
	Splitter: TFileSplitter;
	SplitResult, SplittedPartIndex: integer;
	CRCFileName: WideString;
	ChunkFileName: WideString;
begin
	try
		Splitter := TFileSplitter.Create(localPath, self.split_file_size, self.ExternalProgressProc); //memleak possible
		SplitResult := Splitter.split();
	except
		on E: Exception do
		begin
			Log(LogLevelWarning, MSGTYPE_IMPORTANTERROR, 'File splitting error: ' + E.Message + ', ignored');
			exit(FS_FILE_NOTSUPPORTED);
		end;
	end;

	case SplitResult of
		FS_FILE_OK:
			begin
				//all ok
			end;
		FS_FILE_USERABORT:
			begin
				Log(LogLevelWarning, MSGTYPE_DETAILS, 'File splitting aborted by user, uploading aborted');
				Splitter.Destroy;
				exit(FS_FILE_USERABORT);
			end;
		else
			begin
				Log(LogLevelWarning, MSGTYPE_IMPORTANTERROR, 'File splitting error, code: ' + SplitResult.ToString + ', ignored');
				Splitter.Destroy;
				exit(FS_FILE_NOTSUPPORTED);
			end;
	end;
	for SplittedPartIndex := 0 to length(Splitter.SplitResult.parts) - 1 do
	begin
		ChunkFileName := CopyExt(Splitter.SplitResult.parts[SplittedPartIndex].FileName, remotePath);
		Result := self.putFile(Splitter.SplitResult.parts[SplittedPartIndex].FileName, ChunkFileName, ConflictMode);
		if Result <> FS_FILE_OK then
		begin
			case Result of
				FS_FILE_USERABORT:
					begin
						Log(LogLevelDetail, MSGTYPE_DETAILS, 'Partial upload aborted');
						Splitter.Destroy;
						exit(FS_FILE_USERABORT);
					end;
				FS_FILE_EXISTS:
					begin
						case ChunkOverwriteMode of
							ChunkOverwrite: //silently overwrite chunk
								begin
									Log(LogLevelWarning, MSGTYPE_DETAILS, 'Chunk ' + ChunkFileName + ' already exists, overwriting.');
									if not(self.deleteFile(ChunkFileName)) then
									begin
										Splitter.Destroy;
										exit(FS_FILE_WRITEERROR);
									end else begin
										if (self.putFile(Splitter.SplitResult.parts[SplittedPartIndex].FileName, ChunkFileName, ConflictMode) <> FS_FILE_OK) then
										begin
											Splitter.Destroy;
											exit(FS_FILE_WRITEERROR);
										end;
									end;
								end;
							ChunkOverwriteIgnore: //ignore this chunk
								begin
									Log(LogLevelWarning, MSGTYPE_DETAILS, 'Chunk ' + ChunkFileName + ' already exists, skipping.');
									Continue;
								end;
							ChunkOverwriteAbort: //abort operation
								begin
									Log(LogLevelWarning, MSGTYPE_DETAILS, 'Chunk ' + ChunkFileName + ' already exists, aborting.');
									Splitter.Destroy;
									exit(FS_FILE_NOTSUPPORTED);
								end;
						end;
					end;
				else
					begin
						Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Partial upload error, code: ' + Result.ToString);
						Splitter.Destroy;
						exit;
					end;
			end;
		end;
	end;
	CRCFileName := Splitter.writeCRCFile;
	Result := self.putFile(CRCFileName, CopyExt(CRCFileName, remotePath), ConflictMode);
	case Result of
		FS_FILE_USERABORT:
			begin
				Log(LogLevelDetail, MSGTYPE_DETAILS, 'Partial upload aborted');
				Splitter.Destroy;
				exit(FS_FILE_USERABORT);
			end;
		FS_FILE_EXISTS:
			begin
				case ChunkOverwriteMode of
					ChunkOverwrite: //silently overwrite chunk
						begin
							Log(LogLevelDetail, MSGTYPE_DETAILS, CRCFileName + ' checksum file already exists, overwriting.');
							if not(self.deleteFile(CopyExt(CRCFileName, remotePath))) then
							begin
								Splitter.Destroy;
								exit(FS_FILE_WRITEERROR);
							end else begin
								if (self.putFile(CRCFileName, CopyExt(CRCFileName, remotePath), ConflictMode) <> FS_FILE_OK) then
								begin
									Splitter.Destroy;
									exit(FS_FILE_WRITEERROR);
								end;
							end;
						end;
					ChunkOverwriteIgnore: //ignore this chunk
						begin
							Log(LogLevelDetail, MSGTYPE_DETAILS, CRCFileName + ' checksum file already exists, skipping.');
						end;
					ChunkOverwriteAbort: //abort operation
						begin
							Log(LogLevelWarning, MSGTYPE_DETAILS, CRCFileName + ' checksum file already exists, aborting.');
							Splitter.Destroy;
							exit(FS_FILE_NOTSUPPORTED);
						end;
				end;
			end;
		else
			begin
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Checksum file upload error, code: ' + Result.ToString);
				Splitter.Destroy;
				exit;
			end;
	end;
	Splitter.Destroy;
	exit(FS_FILE_OK); //Файлик залит по частям, выходим
end;

function TCloudMailRu.putFile(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: integer = 0): integer;
var
	PutResult: TStringList;
	JSONAnswer, FileHash: WideString;
	FileSize: int64;
	code, OperationStatus: integer;
	OperationResult: integer;
begin
	if not(Assigned(self)) then
		exit(FS_FILE_WRITEERROR); //Проверка на вызов без инициализации
	if self.public_account then
		exit(FS_FILE_NOTSUPPORTED);

	if (not(self.unlimited_filesize)) and (SizeOfFile(GetUNCFilePath(localPath)) > self.split_file_size) then //todo вынести в процiдурку
	begin
		if self.split_large_files then
		begin
			Log(LogLevelDetail, MSGTYPE_DETAILS, 'File size > ' + self.split_file_size.ToString() + ' bytes, file will be splitted.');
			exit(putFileSplit(localPath, remotePath, ConflictMode, ChunkOverwriteMode));
		end else begin
			Log(LogLevelWarning, MSGTYPE_IMPORTANTERROR, 'File size > ' + self.split_file_size.ToString() + ' bytes, ignored');
			exit(FS_FILE_NOTSUPPORTED);
		end;
	end;
	FileSize := 0;
	Result := FS_FILE_WRITEERROR;
	OperationResult := CLOUD_OPERATION_FAILED;
	PutResult := TStringList.Create;
	self.ExternalSourceName := PWideChar(localPath);
	self.ExternalTargetName := PWideChar(remotePath);
	try
		OperationResult := self.putFileToCloud(localPath, PutResult);
	Except
		on E: Exception do
		begin
			if E.ClassName = 'EAbort' then
			begin
				Result := FS_FILE_USERABORT;
			end else begin
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'error: uploading to cloud: ' + E.ClassName + ' ошибка с сообщением: ' + E.Message);
				Result := FS_FILE_WRITEERROR;
			end;
		end;
	end;
	if OperationResult = CLOUD_OPERATION_OK then
	begin
		FileHash := PutResult.Strings[0];
		val(PutResult.Strings[1], FileSize, code); //Тут ошибка маловероятна
	end else if OperationResult = CLOUD_OPERATION_CANCELLED then
	begin
		Result := FS_FILE_USERABORT;
	end;
	PutResult.free;
	if OperationResult = CLOUD_OPERATION_OK then
	begin
		//Log( MSGTYPE_DETAILS, 'putFileToCloud result: ' + PutResult.Text);
		if self.addFileToCloud(FileHash, FileSize, remotePath, JSONAnswer) then
		begin
			OperationResult := fromJSON_OperationResult(JSONAnswer, OperationStatus);
			Result := CloudResultToFsResult(OperationResult, OperationStatus, 'File uploading error: ');
		end;
	end;
	self.ExternalSourceName := nil;
	self.ExternalTargetName := nil;
end;

function TCloudMailRu.putFileToCloud(localPath: WideString; Return: TStringList): integer; {Заливка на сервер состоит из двух шагов: заливаем файл на сервер в putFileToCloud и добавляем его в облако addFileToCloud}
var
	PostAnswer: WideString;
begin
	Result := CLOUD_OPERATION_FAILED;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	Result := self.HTTPPostFile(self.upload_url + '/?cloud_domain=1&x-email=' + self.user + '%40' + self.domain + '&fileapi' + DateTimeToUnix(now).ToString + '0246', GetUNCFilePath(localPath), PostAnswer);
	if (Result = CLOUD_OPERATION_OK) then
	begin
		ExtractStrings([';'], [], PWideChar(PostAnswer), Return);
		if length(Return.Strings[0]) <> 40 then //? добавить анализ ответа?
		begin
			Result := CLOUD_OPERATION_FAILED;
		end
	end;
end;

function TCloudMailRu.removeDir(Path: WideString): Boolean;
var
	JSON: WideString;
	OperationResult, OperationStatus: integer;
begin
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	Result := self.HTTPPostForm(API_FILE_REMOVE, 'home=/' + IncludeSlash(PathToUrl(Path)) + self.united_params + '&conflict', JSON); //API всегда отвечает true, даже если путь не существует
	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := true;
			else
				begin
					Result := false;
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
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.public_account then
		exit;
	if self.HTTPPostForm(API_FILE_RENAME, 'home=' + PathToUrl(OldName) + '&name=' + PathToUrl(NewName) + self.united_params, JSON) then
	begin //Парсим ответ
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		Result := CloudResultToFsResult(OperationResult, OperationStatus, 'Rename file error: ');
	end;
end;

function TCloudMailRu.statusFile(Path: WideString; var FileInfo: TCloudMailRuDirListingItem): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
	OperationResult, OperationStatus: integer;
begin
	Result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	if self.public_account then
		Result := self.HTTPGet(API_FILE + '?weblink=' + IncludeSlash(self.public_link) + PathToUrl(Path) + self.united_params, JSON, Progress)
	else
		Result := self.HTTPGet(API_FILE + '?home=' + PathToUrl(Path) + self.united_params, JSON, Progress);

	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				Result := fromJSON_FileStatus(JSON, FileInfo);
			else
				begin
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'File status error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					Result := false;
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
		Result := 'read only'
	else
		Result := 'read and write';
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
		Result := CLOUD_SHARE_RO
	else
		Result := CLOUD_SHARE_RW;
end;

end.
