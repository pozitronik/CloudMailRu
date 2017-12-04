unit CloudMailRu;

interface

uses CMLJSON, CMLTypes, System.Hash, System.Classes, System.Generics.Collections, System.SysUtils, PLUGIN_Types, Winapi.Windows, IdStack, MRC_helper, Settings, IdCookieManager, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdSocks, IdHTTP, IdAuthentication, IdIOHandlerStream, FileSplitter, IdCookie, IdMultipartFormData, Cipher;

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
		PrecalculateHash: Boolean;
		CheckCRC: Boolean;

		FileCipher: TFileCipher;

		united_params: WideString; //Объединённый набор авторизационных параметров для подстановки в URL

		{BASE HTTP METHODS}
		procedure HTTPInit(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL; var Socks: TIdSocksInfo; var Cookie: TIdCookieManager);
		procedure HTTPDestroy(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL);

		function HTTPGetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean; //если ProgressEnabled - включаем обработчик onWork, возвращаем ProgressEnabled=false при отмене
		function HTTPGetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean = true): integer;

		function HTTPPostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = true): Boolean; //Постинг данных с возможным получением ответа.
		function HTTPPostMultipart(URL: WideString; Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean;
		function HTTPPostFile(URL: WideString; FileName: WideString; var Answer: WideString): integer; //Постинг файла и получение ответа
		function HTTPPost(URL: WideString; PostData, ResultData: TStringStream; UnderstandResponseCode: Boolean = false; ContentType: WideString = ''; LogErrors: Boolean = true): integer; overload; //Постинг подготовленных данных, отлов ошибок
		function HTTPPost(URL: WideString; var PostData: TIdMultiPartFormDataStream; ResultData: TStringStream): integer; overload; //TIdMultiPartFormDataStream should be passed via var
		function HTTPExceptionHandler(E: Exception; URL: WideString; HTTPMethod: integer = HTTP_METHOD_POST; LogErrors: Boolean = true): integer;

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
		function addFileToCloud(Hash: WideString; size: int64; remotePath: WideString; var JSONAnswer: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true): Boolean; //LogErrors=false => не логируем результат операции, нужно для поиска данных в облаке по хешу
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

		Property isPublicShare: Boolean read public_account;
		Property ProxySettings: TProxySettings read Proxy;
		Property ConnectTimeoutValue: integer read ConnectTimeout;
		function getSharedFileUrl(remotePath: WideString; DoUrlEncode: Boolean = true): WideString;
		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(AccountSettings: TAccountSettings; split_file_size: integer; Proxy: TProxySettings; ConnectTimeout: integer; PrecalculateHash: Boolean = true; CheckCRC: Boolean = true; ExternalProgressProc: TProgressHandler = nil; ExternalLogProc: TLogHandler = nil; ExternalRequestProc: TRequestHandler = nil);
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
		class function CloudHash(Path: WideString): WideString;
	end;

implementation

{TCloudMailRu}

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
	Result := self.HTTPPostForm(API_FILE_ADD, 'conflict=' + ConflictMode + '&home=/' + PathToUrl(remotePath) + '&hash=' + Hash + '&size=' + size.ToString + self.united_params, JSONAnswer, 'application/x-www-form-urlencoded', LogErrors);

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
	if self.HTTPGetPage(API_CLONE + '?folder=' + PathToUrl(Path) + '&weblink=' + link + '&conflict=' + ConflictMode + self.united_params, JSON, Progress) then
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

constructor TCloudMailRu.Create(AccountSettings: TAccountSettings; split_file_size: integer; Proxy: TProxySettings; ConnectTimeout: integer; PrecalculateHash: Boolean = true; CheckCRC: Boolean = true; ExternalProgressProc: TProgressHandler = nil; ExternalLogProc: TLogHandler = nil; ExternalRequestProc: TRequestHandler = nil);
begin
	try
		self.ExternalProgressProc := ExternalProgressProc;
		self.ExternalLogProc := ExternalLogProc;
		self.ExternalRequestProc := ExternalRequestProc;

		self.Cookie := TIdCookieManager.Create();
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
	Result := self.HTTPGetPage(API_FOLDER_SHARED_LINKS + '?' + self.united_params, JSON, ShowProgress);

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
	Result := self.HTTPGetPage(API_FOLDER_SHARED_INCOMING + '?' + self.united_params, JSON, ShowProgress);

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
	Result := self.HTTPGetPage(API_TRASHBIN + '?' + self.united_params, JSON, ShowProgress);

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
		Result := self.HTTPGetPage(API_FOLDER + '&weblink=' + IncludeSlash(self.public_link) + PathToUrl(Path, false) + self.united_params, JSON, ShowProgress)
	else
		Result := self.HTTPGetPage(API_FOLDER + '&home=' + PathToUrl(Path) + self.united_params, JSON, ShowProgress);
	if Result then
	begin
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK:
				begin
					Result := fromJSON_DirListing(JSON, DirListing);
					if Result and self.crypt_filenames then
						self.FileCipher.DecryptDirListing(DirListing);
				end;
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
	FileName: WideString;
	MemoryStream: TMemoryStream;
begin
	Result := FS_FILE_NOTSUPPORTED;
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
		Result := self.HTTPGetFile(self.Shard + PathToUrl(remotePath, false), MemoryStream, LogErrors);
		if Result in [FS_FILE_OK] then
		begin
			MemoryStream.Position := 0;
			self.FileCipher.DecryptStream(MemoryStream, FileStream);
		end;
		MemoryStream.free;

	end else begin
		Result := self.HTTPGetFile(self.Shard + PathToUrl(remotePath, false), FileStream, LogErrors);
	end;

	FlushFileBuffers(FileStream.Handle);
	FileStream.free;
	self.ExternalSourceName := nil;
	self.ExternalTargetName := nil;

	if not(Result in [FS_FILE_OK]) then
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
				begin
					Result := fromJSON_Shard(JSON, Shard) and (Shard <> EmptyWideStr);
					Log(LogLevelDetail, MSGTYPE_DETAILS, 'Shard received: ' + Shard);
				end
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
	Result := self.HTTPGetPage(TOKEN_URL, JSON, Progress);
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
	Result := self.HTTPGetPage(self.PUBLIC_URL, PageContent, Progress);
	if Result then
	begin
		PageContent := StringReplace(PageContent, #$A, EmptyWideStr, [rfReplaceAll]); //так нам проще ковыряться в тексте
		PageContent := StringReplace(PageContent, #$D, EmptyWideStr, [rfReplaceAll]);
		PageContent := StringReplace(PageContent, #9, EmptyWideStr, [rfReplaceAll]);
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
	Result := self.HTTPGetPage(API_USER_SPACE + '?home=/' + self.united_params, JSON, Progress);
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
	try
		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		if ProgressEnabled then
			HTTP.OnWork := self.HTTPProgress; //Вызов прогресса ведёт к возможности отменить получение списка каталогов и других операций, поэтому он нужен не всегда
		Answer := HTTP.Get(URL);
		self.HTTPDestroy(HTTP, SSL);
	Except
		on E: Exception do
		begin
			self.HTTPExceptionHandler(E, URL);
			if (E is EAbort) then
			begin
				Answer := E.Message;
				ProgressEnabled := false; //сообщаем об отмене
			end;

			if (E is EIdHTTPProtocolException) then
			begin
				case HTTP.ResponseCode of
					HTTP_ERROR_BAD_REQUEST, HTTP_ERROR_OVERQUOTA: //recoverable errors
						begin
							Answer := (E as EIdHTTPProtocolException).ErrorMessage;
						end;
				end;
			end;
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
		end;

	end;
	Result := Answer <> EmptyWideStr;
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
			Result := FS_FILE_READERROR;
			Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Redirection limit reached when trying to download ' + URL);
			if (ExternalRequestProc(RT_MsgYesNo, 'Redirection limit', 'Try with another shard?', '', 0)) and (self.getShard(self.Shard)) then
				Result := self.HTTPGetFile(URL, FileStream, LogErrors);
		end;
		self.HTTPDestroy(HTTP, SSL);
	except
		on E: Exception do
		begin
			Result := self.HTTPExceptionHandler(E, URL, HTTP_METHOD_GET, LogErrors);
			if Assigned(HTTP) then
				self.HTTPDestroy(HTTP, SSL);
		end;
	end;
end;

function TCloudMailRu.HTTPPostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = true): Boolean;
var
	ResultStream, PostData: TStringStream;
	PostResult: integer;
begin
	ResultStream := TStringStream.Create;
	PostData := TStringStream.Create(PostDataString, TEncoding.UTF8);

	PostResult := self.HTTPPost(URL, PostData, ResultStream, true, ContentType, LogErrors);
	Result := PostResult = CLOUD_OPERATION_OK;
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
	Result := PostResult = CLOUD_OPERATION_OK;
	Answer := ResultStream.DataString;

	ResultStream.free;
	PostData.free;
end;

function TCloudMailRu.HTTPPostFile(URL: WideString; FileName: WideString; var Answer: WideString): integer;
var
	PostData: TIdMultiPartFormDataStream;
	ResultStream: TStringStream;
	MemoryStream: TMemoryStream;
	FileStream: TFileStream;
begin
	ResultStream := TStringStream.Create;
	PostData := TIdMultiPartFormDataStream.Create;

	if self.crypt_files then
	begin
		MemoryStream := TMemoryStream.Create;
		FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
		self.FileCipher.CryptStream(FileStream, MemoryStream);
		MemoryStream.Position := 0;
		PostData.AddFormField('file', 'application/octet-stream', EmptyWideStr, MemoryStream, FileName);
		Result := self.HTTPPost(URL, PostData, ResultStream);
		MemoryStream.free;
		FileStream.free;
	end else begin
		PostData.AddFile('file', FileName, 'application/octet-stream');
		Result := self.HTTPPost(URL, PostData, ResultStream);
	end;

	Answer := ResultStream.DataString;
	ResultStream.free;
	PostData.free;

end;

function TCloudMailRu.HTTPPost(URL: WideString; PostData, ResultData: TStringStream; UnderstandResponseCode: Boolean = false; ContentType: WideString = ''; LogErrors: Boolean = true): integer;
var
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
	Socks: TIdSocksInfo;
begin
	Result := CLOUD_OPERATION_OK;
	ResultData.Position := 0;
	try
		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		if ContentType <> EmptyWideStr then
			HTTP.Request.ContentType := ContentType;
		HTTP.OnWork := self.HTTPProgress;
		HTTP.Post(URL, PostData, ResultData);
		self.HTTPDestroy(HTTP, SSL);
	except
		on E: Exception do
		begin
			Result := self.HTTPExceptionHandler(E, URL, HTTP_METHOD_POST, LogErrors);
			if UnderstandResponseCode and (E is EIdHTTPProtocolException) then
			begin
				case HTTP.ResponseCode of
					HTTP_ERROR_BAD_REQUEST, HTTP_ERROR_OVERQUOTA: //recoverable errors
						begin
							ResultData.WriteString((E as EIdHTTPProtocolException).ErrorMessage);
							Result := CLOUD_OPERATION_OK;
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
	Result := CLOUD_OPERATION_OK;
	ResultData.Position := 0;
	try
		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		HTTP.OnWork := self.HTTPProgress;
		HTTP.Post(URL, PostData, ResultData);
		self.HTTPDestroy(HTTP, SSL);
	except
		On E: Exception do
		begin
			Result := self.HTTPExceptionHandler(E, URL);
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
		Result := FS_FILE_READERROR; //для HTTPGetFile, GetForm не интересует код ошибки
	end else begin
		method_string := 'отправке данных на адрес ';
		Result := CLOUD_OPERATION_FAILED; //Для всех Post-запросов
	end;

	if E is EAbort then
	begin
		Result := CLOUD_OPERATION_CANCELLED;
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
	if self.HTTPGetPage(API_FOLDER_SHARED_INFO + '?home=' + PathToUrl(Path) + self.united_params, JSON, Progress) then
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
				Log(LogLevelWarning, MSGTYPE_DETAILS, 'File splitting aborted by user, uploading aborted.');
				Splitter.Destroy;
				exit(FS_FILE_USERABORT);
			end;
		else
			begin
				Log(LogLevelWarning, MSGTYPE_IMPORTANTERROR, 'File splitting error, code: ' + SplitResult.ToString + ', ignored.');
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
						Log(LogLevelDetail, MSGTYPE_DETAILS, 'Partial upload aborted.');
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
				Log(LogLevelDetail, MSGTYPE_DETAILS, 'Partial upload aborted.');
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
	JSONAnswer: WideString;
	LocalFileHash, UploadedFileHash: WideString;
	LocalFileSize, UploadedFileSize: int64;
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
			Log(LogLevelWarning, MSGTYPE_IMPORTANTERROR, 'File size > ' + self.split_file_size.ToString() + ' bytes, ignored.');
			exit(FS_FILE_NOTSUPPORTED);
		end;
	end;
	LocalFileSize := 0;
	UploadedFileSize := 0;
	Result := FS_FILE_WRITEERROR;
	OperationResult := CLOUD_OPERATION_FAILED;
	PutResult := TStringList.Create;
	self.ExternalSourceName := PWideChar(localPath);
	self.ExternalTargetName := PWideChar(remotePath);

	if self.PrecalculateHash or self.CheckCRC then
	begin
		LocalFileHash := CloudHash(localPath);
		LocalFileSize := SizeOfFile(localPath);
	end;
	if self.PrecalculateHash and (LocalFileHash <> EmptyWideStr) then {issue #135}
	begin
		if self.addFileToCloud(LocalFileHash, LocalFileSize, remotePath, JSONAnswer, CLOUD_CONFLICT_STRICT, false) then
		begin
			OperationResult := fromJSON_OperationResult(JSONAnswer, OperationStatus);
			if OperationResult = CLOUD_OPERATION_OK then
			begin
				Log(LogLevelDetail, MSGTYPE_DETAILS, 'File ' + localPath + ' found by hash.');
				exit(CLOUD_OPERATION_OK);
			end;
		end;
	end;

	try
		OperationResult := self.putFileToCloud(localPath, PutResult);
	Except
		on E: Exception do
		begin
			if E.ClassName = 'EAbort' then
			begin
				Result := FS_FILE_USERABORT;
			end else begin
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'error: uploading to cloud: ' + E.ClassName + ' with message: ' + E.Message);
				Result := FS_FILE_WRITEERROR;
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
				Result := CLOUD_OPERATION_FAILED;

		end;
	end else if OperationResult = CLOUD_OPERATION_CANCELLED then
	begin
		Result := FS_FILE_USERABORT;
	end;
	PutResult.free;
	if OperationResult = CLOUD_OPERATION_OK then
	begin
		if self.addFileToCloud(UploadedFileHash, UploadedFileSize, remotePath, JSONAnswer) then
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
		Result := self.HTTPGetPage(API_FILE + '?weblink=' + IncludeSlash(self.public_link) + PathToUrl(Path) + self.united_params, JSON, Progress)
	else
		Result := self.HTTPGetPage(API_FILE + '?home=' + PathToUrl(Path) + self.united_params, JSON, Progress);

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

class function TCloudMailRu.CloudHash(Path: WideString): WideString;
var
	sha1: THashSHA1;
	buffer: array [0 .. 8191] of byte;
	read: LongInt;
	stream: TFileStream;
	initBuffer, finalBuffer: TBytes;
begin
	Result := EmptyWideStr;
	if not FileExists(Path) then
	exit;
	try
		stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
	except
		exit;
	end;
	if stream.size < 21 then
	begin
		SetLength(initBuffer, 20);
		stream.read(initBuffer, stream.size);
		Result := UpperCase(THash.DigestAsString(initBuffer));

		stream.free;
		exit;
	end;

	FillChar(buffer, sizeof(buffer), 0);
	initBuffer := TEncoding.UTF8.GetBytes('mrCloud');

	sha1 := THashSHA1.Create;
	sha1.Update(initBuffer, length(initBuffer));
	repeat
		read := stream.read(buffer, sizeof(buffer));
		sha1.Update(buffer, read);
	until read < sizeof(buffer);

	finalBuffer := TEncoding.UTF8.GetBytes(stream.size.ToString);
	sha1.Update(finalBuffer, length(finalBuffer));
	Result := UpperCase(sha1.HashAsString);
	sha1.Reset;
	stream.free;

end;

end.
