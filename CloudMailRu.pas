unit CloudMailRu;

interface

uses System.Classes, System.SysUtils, PLUGIN_Types, JSON, Winapi.Windows, IdStack, MRC_helper, Settings, IdCookieManager, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdSocks, IdHTTP, IdAuthentication, IdIOHandlerStream, IdMultipartFormData, FileSplitter;

const
{$IFDEF WIN64}
	PlatformX = 'x64';
{$ENDIF}
{$IFDEF WIN32}
	PlatformX = 'x32';
{$ENDIF}
	PUBLIC_ACCESS_URL = 'https://cloud.mail.ru/public/';
	OAUTH_TOKEN_URL = 'https://o2.mail.ru/token';
	TOKEN_URL = 'https://cloud.mail.ru/?from=promo&from=authpopup';
	LOGIN_URL = 'https://auth.mail.ru/cgi-bin/auth';
	SECSTEP_URL = 'https://auth.mail.ru/cgi-bin/secstep';

	API_FILE = 'https://cloud.mail.ru/api/v2/file';
	API_FILE_MOVE = 'https://cloud.mail.ru/api/v2/file/move';
	API_FILE_PUBLISH = 'https://cloud.mail.ru/api/v2/file/publish';
	API_FILE_UNPUBLISH = 'https://cloud.mail.ru/api/v2/file/unpublish';
	API_FILE_RENAME = 'https://cloud.mail.ru/api/v2/file/rename';
	API_FILE_ADD = 'https://cloud.mail.ru/api/v2/file/add';
	API_FILE_REMOVE = 'https://cloud.mail.ru/api/v2/file/remove';
	API_FILE_COPY = 'https://cloud.mail.ru/api/v2/file/copy';
	API_FOLDER = 'https://cloud.mail.ru/api/v2/folder?sort={%22type%22%3A%22name%22%2C%22order%22%3A%22asc%22}&offset=0&limit=65535';
	API_FOLDER_ADD = 'https://cloud.mail.ru/api/v2/folder/add';
	API_FOLDER_SHARED_INFO = 'https://cloud.mail.ru/api/v2/folder/shared/info'; //get
	API_FOLDER_INVITES = 'https://cloud.mail.ru/api/v2/folder/invites';
	API_FOLDER_SHARE = 'https://cloud.mail.ru/api/v2/folder/share';
	API_FOLDER_UNSHARE = 'https://cloud.mail.ru/api/v2/folder/unshare';
	API_FOLDER_MOUNT = 'https://cloud.mail.ru/api/v2/folder/mount';
	API_FOLDER_UNMOUNT = 'https://cloud.mail.ru/api/v2/folder/unmount';
	API_FOLDER_SHARED_LINKS = 'https://cloud.mail.ru/api/v2/folder/shared/links';
	API_FOLDER_SHARED_INCOMING = 'https://cloud.mail.ru/api/v2/folder/shared/incoming';
	API_TRASHBIN = 'https://cloud.mail.ru/api/v2/trashbin';
	API_TRASHBIN_RESTORE = 'https://cloud.mail.ru/api/v2/trashbin/restore';
	API_TRASHBIN_EMPTY = 'https://cloud.mail.ru/api/v2/trashbin/empty';
	API_AB_CONTACTS = ''; //todo
	API_DISPATCHER = 'https://cloud.mail.ru/api/v2/dispatcher/';
	API_USER_SPACE = 'https://cloud.mail.ru/api/v2/user/space';
	API_CLONE = 'https://cloud.mail.ru/api/v2/clone';
	API_INVITE_REJECT = 'https://cloud.mail.ru/api/v2/folder/invites/reject';

	TYPE_DIR = 'folder';
	TYPE_FILE = 'file';

	KIND_SHARED = 'shared';
	{Константы для обозначения ошибок, возвращаемых при парсинге ответов облака. Дополняем по мере обнаружения}
	CLOUD_ERROR_UNKNOWN = -2; //unknown: 'Ошибка на сервере'
	CLOUD_OPERATION_ERROR_STATUS_UNKNOWN = -1;
	CLOUD_OPERATION_OK = 0;
	CLOUD_OPERATION_FAILED = 1;
	CLOUD_OPERATION_CANCELLED = 5;

	CLOUD_ERROR_EXISTS = 1; //exists: 'Папка с таким названием уже существует. Попробуйте другое название'
	CLOUD_ERROR_REQUIRED = 2; //required: 'Название папки не может быть пустым'
	CLOUD_ERROR_INVALID = 3; //invalid: '&laquo;' + app.escapeHTML(name) + '&raquo; это неправильное название папки. В названии папок нельзя использовать символы «" * / : < > ?  \\ |»'
	CLOUD_ERROR_READONLY = 4; //readonly|read_only: 'Невозможно создать. Доступ только для просмотра'
	CLOUD_ERROR_NAME_LENGTH_EXCEEDED = 5; //name_length_exceeded: 'Ошибка: Превышена длина имени папки. <a href="https://help.mail.ru/cloud_web/confines" target="_blank">Подробнее…</a>'
	CLOUD_ERROR_OVERQUOTA = 7; //overquota: 'Невозможно скопировать, в вашем Облаке недостаточно места'
	CLOUD_ERROR_QUOTA_EXCEEDED = 7; //"quota_exceeded": 'Невозможно скопировать, в вашем Облаке недостаточно места'
	CLOUD_ERROR_NOT_EXISTS = 8; //"not_exists": 'Копируемая ссылка не существует'
	CLOUD_ERROR_OWN = 9; //"own": 'Невозможно клонировать собственную ссылку'
	CLOUD_ERROR_NAME_TOO_LONG = 10; //"name_too_long": 'Превышен размер имени файла'
	CLOUD_ERROR_VIRUS_SCAN_FAIL = 11; //"virus_scan_fail": 'Файл заражен вирусом'
	CLOUD_ERROR_OWNER = 12; //Нельзя использовать собственный email
	CLOUD_ERROR_FAHRENHEIT = 451; //Публикация контента заблокирована по требованию правообладателя или уполномоченного государственного ведомства.
	CLOUD_ERROR_BAD_REQUEST = 400; //
	CLOUD_ERROR_TREES_CONFLICT = 15; //Нельзя сделать папку общей, если она содержит другие общие папки или находится в общей папке
	CLOUD_ERROR_UNPROCESSABLE_ENTRY = 16; //Нельзя открыть доступ к файлу
	CLOUD_ERROR_USER_LIMIT_EXCEEDED = 17; //Невозможно добавить пользователя. Вы можете иметь не более 200 пользователей в одной общей папке
	CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED = 18; //Невозможно добавить пользователя. Вы можете создать не более 50 общих папок
	CLOUD_ERROR_NOT_ACCEPTABLE = 406; //Нельзя добавить этого пользователя

	{Режимы работы при конфликтах копирования}
	CLOUD_CONFLICT_STRICT = 'strict'; //возвращаем ошибку при существовании файла
	CLOUD_CONFLICT_IGNORE = 'ignore'; //В API, видимо, не реализовано
	CLOUD_CONFLICT_RENAME = 'rename'; //Переименуем новый файл
	//CLOUD_CONFLICT_REPLACE = 'overwrite'; // хз, этот ключ не вскрыт

	CLOUD_SHARE_ACCESS_READ_ONLY = 'read_only';
	CLOUD_SHARE_ACCESS_READ_WRITE = 'read_write';

	CLOUD_MAX_NAME_LENGTH = 255;
	CLOUD_PUBLISH = true;
	CLOUD_UNPUBLISH = false;

	CLOUD_SHARE_RW = 0;
	CLOUD_SHARE_RO = 1;
	CLOUD_SHARE_NO = 2;

	{Поддерживаемые методы авторизации}
	CLOUD_AUTH_METHOD_WEB = 0; //Через парсинг HTTP-страницы
	CLOUD_AUTH_METHOD_TWO_STEP = 1; //Через парсинг HTTP-страницы, двухфакторная
	CLOUD_AUTH_METHOD_OAUTH = 2; //Через сервер OAuth-авторизации

type
	TCloudMailRuDirListingItem = Record
		tree: WideString;
		name: WideString;
		grev: integer;
		size: int64;
		kind: WideString;
		weblink: WideString;
		rev: integer;
		type_: WideString;
		home: WideString;
		mtime: int64;
		hash: WideString;
		virus_scan: WideString;
		folders_count: integer;
		files_count: integer;
		deleted_at: integer;
		deleted_from: WideString;
		deleted_by: integer;
	End;

	TCloudMailRuOAuthInfo = Record
		error: WideString;
		error_code: integer;
		error_description: WideString;
		expires_in: integer;
		refresh_token: WideString;
		access_token: WideString;
	end;

	TCloudMailRuSpaceInfo = record
		overquota: Boolean;
		total: int64;
		used: int64;
	End;

	TCloudMailRuOwnerInfo = record
		email: WideString;
		name: WideString;
	end;

	TCloudMailRuInviteInfo = record
		email: WideString;
		status: WideString;
		access: WideString;
		name: WideString;

	end;

	TCloudMailRuIncomingInviteInfo = record
		owner: TCloudMailRuOwnerInfo;
		tree: WideString;
		access: WideString;
		name: WideString;
		size: int64;
		home: WideString; //only on already mounted items
		invite_token: WideString;
	end;

	TCloudMailRuDirListing = array of TCloudMailRuDirListingItem;
	TCloudMailRuInviteInfoListing = array of TCloudMailRuInviteInfo;
	TCloudMailRuIncomingInviteInfoListing = array of TCloudMailRuIncomingInviteInfo;

	TCloudMailRu = class
	private
		{VARIABLES}
		ExternalPluginNr: integer;
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
		token: WideString;
		OAuthToken: TCloudMailRuOAuthInfo;
		x_page_id: WideString;
		build: WideString;
		upload_url: WideString;
		login_method: integer;
		Cookie: TIdCookieManager;
		Socks: TIdSocksInfo;
		ExternalProgressProc: TProgressProcW;
		ExternalLogProc: TLogProcW;
		ExternalRequestProc: TRequestProcW;
		Shard: WideString;
		Proxy: TProxySettings;
		ConnectTimeout: integer;

		united_params: WideString; //Объединённый набор авторизационных параметров для подстановки в URL
		{BASE HTTP METHODS}
		procedure HTTPInit(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL; var Socks: TIdSocksInfo; var Cookie: TIdCookieManager);
		procedure HTTPDestroy(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL);
		function HTTPGet(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean; //если ProgressEnabled - включаем обработчик onWork, возвращаем ProgressEnabled=false при отмене
		function HTTPGetFile(URL: WideString; var FileStream: TFileStream; LogErrors: Boolean = true): integer;
		function HTTPPost(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'): Boolean; //Постинг данных с возможным получением ответа.
		function HTTPPostFile(URL: WideString; FileName: WideString; var Answer: WideString): integer; //Постинг файла и получение ответа
		procedure HTTPProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
		{RAW TEXT PARSING}
		function extractTokenFromText(Text: WideString; var token: WideString): Boolean;
		function extractPublicTokenFromText(Text: WideString; var PublicToken: WideString): Boolean;
		function extract_x_page_id_FromText(Text: WideString; var PageId: WideString): Boolean;
		function extract_build_FromText(Text: WideString; var build: WideString): Boolean;
		function extract_upload_url_FromText(Text: WideString; var UploadUrl: WideString): Boolean;
		function extractPublicShard(Text: WideString; var Shard: WideString): Boolean;
		{JSON MANIPULATION}
		function fromJSON_DirListing(JSON: WideString; var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean;
		function fromJSON_UserSpace(JSON: WideString; var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean;
		function fromJSON_FileStatus(JSON: WideString; var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean;
		function fromJSON_Shard(JSON: WideString; var Shard: WideString): Boolean;
		function fromJSON_OAuthTokenInfo(JSON: WideString; var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean;
		function fromJSON_PublicLink(JSON: WideString; var PublicLink: WideString): Boolean;
		function fromJSON_OperationResult(JSON: WideString; var OperationStatus: integer): integer;
		function fromJSON_InviteListing(JSON: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
		function fromJSON_IncomingInviteListing(JSON: WideString; var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean;
		{HTTP REQUESTS WRAPPERS}
		function getToken(): Boolean;
		function getSharedToken(): Boolean;
		function getOAuthToken(var OAuthToken: TCloudMailRuOAuthInfo): Boolean;
		function getShard(var Shard: WideString): Boolean;
		function getUserSpace(var SpaceInfo: TCloudMailRuSpaceInfo): Boolean;
		function putFileToCloud(localPath: WideString; Return: TStringList): integer;
		function addFileToCloud(hash: WideString; size: int64; remotePath: WideString; var JSONAnswer: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): Boolean;
		{OTHER ROUTINES}
		procedure Log(MsgType: integer; LogString: WideString);
		function CloudResultToFsResult(CloudResult: integer; OperationStatus: integer; ErrorPrefix: WideString = ''): integer;
	protected
		{REGULAR CLOUD}
		function loginRegular(method: integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function getFileRegular(remotePath, localPath: WideString; LogErrors: Boolean = true): integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		{SHARED WEBFOLDERS}
		function loginShared(method: integer = CLOUD_AUTH_METHOD_WEB): Boolean;

		function getFileShared(remotePath, localPath: WideString; LogErrors: Boolean = true): integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
	public
		Property isPublicShare: Boolean read public_account;
		Property ProxySettings: TProxySettings read Proxy;
		Property ConnectTimeoutValue: integer read ConnectTimeout;
		function getSharedFileUrl(remotePath: WideString; DoUrlEncode: Boolean = true): WideString;
		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(AccountSettings: TAccountSettings; split_file_size: integer; Proxy: TProxySettings; ConnectTimeout: integer; ExternalProgressProc: TProgressProcW = nil; PluginNr: integer = -1; ExternalLogProc: TLogProcW = nil; ExternalRequestProc: TRequestProcW = nil);
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
		function getDescriptionFile(remotePath, localCopy: WideString): integer; //Если в каталоге remotePath есть descript.ion - скопировать его в файл localcopy
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
begin
	{Экспериментально выяснено, что параметры api, build, email, x-email, x-page-id в запросе не обязательны}
	Result := self.HTTPPost(API_FILE_ADD, 'conflict=' + ConflictMode + '&home=/' + remotePath + '&hash=' + hash + '&size=' + size.ToString + self.united_params, JSONAnswer);
end;

function TCloudMailRu.cloneWeblink(Path, link, ConflictMode: WideString): integer;
var
	JSON: WideString;
	OperationStatus: integer;
	Progress: Boolean;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit(FS_FILE_NOTSUPPORTED);
	if self.HTTPGet(API_CLONE + '?folder=' + PathToUrl(Path) + '&weblink=' + link + '&conflict=' + ConflictMode + self.united_params, JSON, Progress) then
	begin //Парсим ответ
		Result := self.fromJSON_OperationResult(JSON, OperationStatus);
		if Result <> CLOUD_OPERATION_OK then Log(MSGTYPE_IMPORTANTERROR, 'File publish error: ' + self.ErrorCodeText(Result) + ' Status: ' + OperationStatus.ToString());

	end else begin //посмотреть это
		if not(Progress) then
		begin //user cancelled
			Result := FS_FILE_USERABORT;
		end else begin //unknown error
			Log(MSGTYPE_IMPORTANTERROR, 'Public link clone error: got ' + OperationStatus.ToString + ' status');
			Result := FS_FILE_WRITEERROR;
		end;
	end;
end;

function TCloudMailRu.CloudResultToFsResult(CloudResult: integer; OperationStatus: integer; ErrorPrefix: WideString): integer;
begin
	case CloudResult of
		CLOUD_OPERATION_OK: exit(CLOUD_OPERATION_OK);
		CLOUD_ERROR_EXISTS: exit(FS_FILE_EXISTS);
		CLOUD_ERROR_REQUIRED, CLOUD_ERROR_INVALID, CLOUD_ERROR_READONLY, CLOUD_ERROR_NAME_LENGTH_EXCEEDED: exit(FS_FILE_WRITEERROR);
		CLOUD_ERROR_UNKNOWN: exit(FS_FILE_NOTSUPPORTED);
		CLOUD_ERROR_OVERQUOTA:
			begin
				Log(MSGTYPE_IMPORTANTERROR, 'Insufficient Storage');
				exit(FS_FILE_WRITEERROR);
			end;
		CLOUD_ERROR_NAME_TOO_LONG:
			begin
				Log(MSGTYPE_IMPORTANTERROR, 'Name too long');
				exit(FS_FILE_WRITEERROR);
			end;
		else
			begin //что-то неизвестное
				if (ErrorPrefix <> '') then Log(MSGTYPE_IMPORTANTERROR, ErrorPrefix + self.ErrorCodeText(CloudResult) + ' Status: ' + OperationStatus.ToString());
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit(FS_FILE_NOTSUPPORTED);
	if self.HTTPPost(API_FILE_COPY, 'home=' + PathToUrl(OldName) + '&folder=' + PathToUrl(ToPath) + self.united_params + '&conflict', JSON) then
	begin //Парсим ответ
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
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
		Log(MSGTYPE_IMPORTANTERROR, 'Copying in same dir not supported by cloud');
		exit(FS_FILE_NOTSUPPORTED);
	end else begin
		Result := self.copyFile(OldName, NewPath);
		if Result <> CLOUD_OPERATION_OK then exit;
	end;
	if not(SameName) then
	begin //скопированный файл лежит в новом каталоге со старым именем
		Result := self.renameFile(NewPath + ExtractFileName(OldName), ExtractFileName(NewName));
	end;
end;

constructor TCloudMailRu.Create(AccountSettings: TAccountSettings; split_file_size: integer; Proxy: TProxySettings; ConnectTimeout: integer; ExternalProgressProc: TProgressProcW; PluginNr: integer; ExternalLogProc: TLogProcW; ExternalRequestProc: TRequestProcW);
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
			else self.Socks.Authentication := saNoAuthentication;

			case Proxy.ProxyType of
				ProxySocks5: Socks.Version := svSocks5;
				ProxySocks4: Socks.Version := svSocks4;
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
		if self.public_account and (self.PUBLIC_URL <> '') then
		begin
			self.public_link := self.PUBLIC_URL;
			self.PUBLIC_URL := IncludeSlash(self.PUBLIC_URL);
			Delete(self.public_link, 1, length(PUBLIC_ACCESS_URL));
			if self.public_link[length(self.public_link)] = '/' then Delete(self.public_link, length(self.public_link), 1);
		end;

		self.split_file_size := split_file_size;
		self.ConnectTimeout := ConnectTimeout;
		self.ExternalProgressProc := ExternalProgressProc;
		self.ExternalLogProc := ExternalLogProc;
		self.ExternalRequestProc := ExternalRequestProc;

		self.ExternalPluginNr := PluginNr;
		self.ExternalSourceName := '';
		self.ExternalTargetName := '';
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Cloud initialization error: ' + E.Message);
		end;
	end;
end;

function TCloudMailRu.createDir(Path: WideString): Boolean;
var
	PostAnswer: WideString;
	OperationStatus, OperationResult: integer;
begin
	Result := false;
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;
	if self.HTTPPost(API_FOLDER_ADD, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', PostAnswer) then
	begin
		OperationResult := self.fromJSON_OperationResult(PostAnswer, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := true;
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;
	Result := self.HTTPPost(API_FILE_REMOVE, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', JSON);
	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := true;
			else
				begin
					Log(MSGTYPE_IMPORTANTERROR, 'Delete file error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					Result := false;
				end;
		end;
	end;
end;

destructor TCloudMailRu.Destroy;
begin
	if Assigned(self.Cookie) then self.Cookie.free;
	if Assigned(self.Socks) then self.Socks.free;
	inherited;
end;

class function TCloudMailRu.ErrorCodeText(ErrorCode: integer): WideString;
begin
	case ErrorCode of
		CLOUD_ERROR_EXISTS: exit('Объект с таким названием уже существует. Попробуйте другое название.');
		CLOUD_ERROR_REQUIRED: exit('Название папки не может быть пустым.');
		CLOUD_ERROR_INVALID: exit('Неправильное название папки. В названии папок нельзя использовать символы «" * / : < > ?  \\ |».');
		CLOUD_ERROR_READONLY: exit('Невозможно создать. Доступ только для просмотра.');
		CLOUD_ERROR_NAME_LENGTH_EXCEEDED: exit('Превышена длина имени папки.');
		CLOUD_ERROR_OVERQUOTA: exit('Невозможно скопировать, в вашем Облаке недостаточно места.');
		CLOUD_ERROR_NOT_EXISTS: exit('Копируемая ссылка не существует.');
		CLOUD_ERROR_OWN: exit('Невозможно клонировать собственную ссылку.');
		CLOUD_ERROR_NAME_TOO_LONG: exit('Превышена длина имени файла.');
		CLOUD_ERROR_VIRUS_SCAN_FAIL: exit('Файл заражен вирусом');
		CLOUD_ERROR_OWNER: exit('Нельзя использовать собственный email');
		CLOUD_ERROR_FAHRENHEIT: exit('Невозможно создать ссылку. Публикация контента заблокирована по требованию правообладателя или уполномоченного государственного ведомства.');
		CLOUD_ERROR_BAD_REQUEST: exit('Ошибка запроса к серверу.');
		CLOUD_ERROR_TREES_CONFLICT: exit('Нельзя сделать папку общей, если она содержит другие общие папки или находится в общей папке');
		CLOUD_ERROR_UNPROCESSABLE_ENTRY: exit('Нельзя открыть доступ к файлу');
		CLOUD_ERROR_USER_LIMIT_EXCEEDED: exit('Невозможно добавить пользователя. Вы можете иметь не более 200 пользователей в одной общей папке ');
		CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED: exit('Невозможно добавить пользователя. Вы можете создать не более 50 общих папок');
		CLOUD_ERROR_NOT_ACCEPTABLE: exit('Нельзя добавить этого пользователя');
		else exit('Неизвестная ошибка (' + ErrorCode.ToString + ')');
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

function TCloudMailRu.fromJSON_DirListing(JSON: WideString; var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean;
var
	Obj: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	Result := true;
	try
		A := ((TJSONObject.ParseJSONValue(JSON) as TJSONObject).values['body'] as TJSONObject).values['list'] as TJSONArray;
		SetLength(CloudMailRuDirListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			Obj := A.Items[J] as TJSONObject;
			with CloudMailRuDirListing[J] do
			begin
				if Assigned(Obj.values['size']) then size := Obj.values['size'].Value.ToInt64;
				if Assigned(Obj.values['kind']) then kind := Obj.values['kind'].Value;
				if Assigned(Obj.values['weblink']) then weblink := Obj.values['weblink'].Value;
				if Assigned(Obj.values['type']) then type_ := Obj.values['type'].Value;
				if Assigned(Obj.values['home']) then home := Obj.values['home'].Value;
				if Assigned(Obj.values['name']) then name := Obj.values['name'].Value;
				if Assigned(Obj.values['deleted_at']) then deleted_at := Obj.values['deleted_at'].Value.ToInteger;
				if Assigned(Obj.values['deleted_from']) then deleted_from := Obj.values['deleted_from'].Value;
				if Assigned(Obj.values['deleted_by']) then deleted_by := Obj.values['deleted_by'].Value.ToInteger;
				if Assigned(Obj.values['grev']) then grev := Obj.values['grev'].Value.ToInteger;
				if Assigned(Obj.values['rev']) then rev := Obj.values['rev'].Value.ToInteger;
				if (type_ = TYPE_FILE) then
				begin
					if Assigned(Obj.values['mtime']) then mtime := Obj.values['mtime'].Value.ToInt64;
					if Assigned(Obj.values['virus_scan']) then virus_scan := Obj.values['virus_scan'].Value;
					if Assigned(Obj.values['hash']) then hash := Obj.values['hash'].Value;
				end else begin
					if Assigned(Obj.values['tree']) then tree := Obj.values['tree'].Value;

					if Assigned(Obj.values['count']) then
					begin
						folders_count := (Obj.values['count'] as TJSONObject).values['folders'].Value.ToInteger();
						files_count := (Obj.values['count'] as TJSONObject).values['files'].Value.ToInteger();
					end;
					mtime := 0;
				end;
			end;
		end;
	except
		Result := false;
	end;

end;

function TCloudMailRu.fromJSON_FileStatus(JSON: WideString; var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean;
var
	Obj: TJSONObject;
begin
	Result := true;
	try
		Obj := (TJSONObject.ParseJSONValue(JSON) as TJSONObject).values['body'] as TJSONObject;
		with CloudMailRuDirListingItem do
		begin
			if Assigned(Obj.values['size']) then size := Obj.values['size'].Value.ToInt64;
			if Assigned(Obj.values['kind']) then kind := Obj.values['kind'].Value;
			if Assigned(Obj.values['weblink']) then weblink := Obj.values['weblink'].Value;
			if Assigned(Obj.values['type']) then type_ := Obj.values['type'].Value;
			if Assigned(Obj.values['home']) then home := Obj.values['home'].Value;
			if Assigned(Obj.values['name']) then name := Obj.values['name'].Value;
			if (type_ = TYPE_FILE) then
			begin
				if Assigned(Obj.values['mtime']) then mtime := Obj.values['mtime'].Value.ToInteger;
				if Assigned(Obj.values['virus_scan']) then virus_scan := Obj.values['virus_scan'].Value;
				if Assigned(Obj.values['hash']) then hash := Obj.values['hash'].Value;
			end else begin
				if Assigned(Obj.values['tree']) then tree := Obj.values['tree'].Value;
				if Assigned(Obj.values['grev']) then grev := Obj.values['grev'].Value.ToInteger;
				if Assigned(Obj.values['rev']) then rev := Obj.values['rev'].Value.ToInteger;
				if Assigned((Obj.values['count'] as TJSONObject).values['folders']) then folders_count := (Obj.values['count'] as TJSONObject).values['folders'].Value.ToInteger();
				if Assigned((Obj.values['count'] as TJSONObject).values['files']) then files_count := (Obj.values['count'] as TJSONObject).values['files'].Value.ToInteger();
				mtime := 0;
			end;
		end;
	except
		Result := false;
	end;
end;

{TODO Разобраться с полями TCloudMailRuInviteInfoListing - возможно ли объединение с TCloudMailRuINcomingInviteInfoListing}
function TCloudMailRu.fromJSON_InviteListing(JSON: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
var
	Obj: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	Result := true;
	SetLength(InviteListing, 0);
	try
		A := ((TJSONObject.ParseJSONValue(JSON) as TJSONObject).values['body'] as TJSONObject).values['invited'] as TJSONArray;
		if not Assigned(A) then exit; //no invites
		SetLength(InviteListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			Obj := A.Items[J] as TJSONObject;
			with InviteListing[J] do
			begin
				if Assigned(Obj.values['email']) then email := Obj.values['email'].Value;
				if Assigned(Obj.values['status']) then status := Obj.values['status'].Value;
				if Assigned(Obj.values['access']) then access := Obj.values['access'].Value;
				if Assigned(Obj.values['name']) then name := Obj.values['name'].Value;
			end;
		end;
	except
		on E: {EJSON}Exception do
		begin
			Result := false;
			Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON);
		end;
	end;
end;

function TCloudMailRu.fromJSON_IncomingInviteListing(JSON: WideString; var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean;
var
	Obj, OwnerObj: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	Result := true;
	SetLength(IncomingInviteListing, 0);
	try
		A := ((TJSONObject.ParseJSONValue(JSON) as TJSONObject).values['body'] as TJSONObject).values['list'] as TJSONArray;
		if not Assigned(A) then exit; //no invites
		SetLength(IncomingInviteListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			Obj := A.Items[J] as TJSONObject;
			with IncomingInviteListing[J] do
			begin
				if Assigned(Obj.values['owner']) then
				begin
					OwnerObj := Obj.values['owner'] as TJSONObject;
					if Assigned(OwnerObj.values['email']) then owner.email := OwnerObj.values['email'].Value;
					if Assigned(OwnerObj.values['name']) then owner.name := OwnerObj.values['name'].Value;
				end;

				if Assigned(Obj.values['tree']) then tree := Obj.values['tree'].Value;
				if Assigned(Obj.values['access']) then access := Obj.values['access'].Value;
				if Assigned(Obj.values['name']) then name := Obj.values['name'].Value;
				if Assigned(Obj.values['home']) then home := Obj.values['home'].Value;
				if Assigned(Obj.values['size']) then size := Obj.values['size'].Value.ToInt64;
				if Assigned(Obj.values['invite_token']) then invite_token := Obj.values['invite_token'].Value;
			end;
		end;
	except
		on E: {EJSON}Exception do
		begin
			Result := false;
			Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON);
		end;
	end;
end;

function TCloudMailRu.fromJSON_OAuthTokenInfo(JSON: WideString; var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean;
var
	Obj: TJSONObject;
begin
	Result := true;
	try
		Obj := (TJSONObject.ParseJSONValue(JSON) as TJSONObject);
		with CloudMailRuOAuthInfo do
		begin
			if Assigned(Obj.values['error']) then error := Obj.values['error'].Value;
			if Assigned(Obj.values['error_code']) then error_code := Obj.values['error_code'].Value.ToInteger;
			if Assigned(Obj.values['error_description']) then error_description := Obj.values['error_description'].Value;
			if Assigned(Obj.values['expires_in']) then expires_in := Obj.values['expires_in'].Value.ToInteger;
			if Assigned(Obj.values['refresh_token']) then refresh_token := Obj.values['refresh_token'].Value;
			if Assigned(Obj.values['access_token']) then access_token := Obj.values['access_token'].Value;
		end;
	except
		on E: {EJSON}Exception do
		begin
			Result := false;
			Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON);
			CloudMailRuOAuthInfo.error_code := CLOUD_ERROR_UNKNOWN;
			CloudMailRuOAuthInfo.error := 'Answer parsing';
			CloudMailRuOAuthInfo.error_description := 'JSON parsing error: at ' + JSON;
		end;
	end;
end;

function TCloudMailRu.fromJSON_OperationResult(JSON: WideString; var OperationStatus: integer): integer;
var
	Obj: TJSONObject;
	error, nodename: WideString;
begin
	//Result:=CLOUD_ERROR_BAD_REQUEST;
	try
		Obj := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
		OperationStatus := Obj.values['status'].Value.ToInteger;
		if OperationStatus <> 200 then
		begin
			//if OperationStatus = 400 then exit(CLOUD_ERROR_BAD_REQUEST);
			if OperationStatus = 451 then exit(CLOUD_ERROR_FAHRENHEIT);
			if OperationStatus = 507 then exit(CLOUD_ERROR_OVERQUOTA);
			if OperationStatus = 406 then exit(CLOUD_ERROR_NOT_ACCEPTABLE);

			if (Assigned((Obj.values['body'] as TJSONObject).values['home'])) then nodename := 'home'
			else if (Assigned((Obj.values['body'] as TJSONObject).values['weblink'])) then nodename := 'weblink'
			else if (Assigned((Obj.values['body'] as TJSONObject).values['invite.email'])) then
			begin //invite errors
				error := (((Obj.values['body'] as TJSONObject).values['invite.email']) as TJSONObject).values['error'].Value;
			end else begin
				Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON);
				exit(CLOUD_ERROR_UNKNOWN);
			end;
			if error = '' then error := ((Obj.values['body'] as TJSONObject).values[nodename] as TJSONObject).values['error'].Value;
			if error = 'exists' then exit(CLOUD_ERROR_EXISTS);
			if error = 'required' then exit(CLOUD_ERROR_REQUIRED);
			if error = 'readonly' then exit(CLOUD_ERROR_READONLY);
			if error = 'read_only' then exit(CLOUD_ERROR_READONLY);
			if error = 'name_length_exceeded' then exit(CLOUD_ERROR_NAME_LENGTH_EXCEEDED);
			if error = 'unknown' then exit(CLOUD_ERROR_UNKNOWN);
			if error = 'overquota' then exit(CLOUD_ERROR_OVERQUOTA);
			if error = 'quota_exceeded' then exit(CLOUD_ERROR_OVERQUOTA);
			if error = 'invalid' then exit(CLOUD_ERROR_INVALID);
			if error = 'not_exists' then exit(CLOUD_ERROR_NOT_EXISTS);
			if error = 'own' then exit(CLOUD_ERROR_OWN);
			if error = 'name_too_long' then exit(CLOUD_ERROR_NAME_TOO_LONG);
			if error = 'virus_scan_fail' then exit(CLOUD_ERROR_VIRUS_SCAN_FAIL);
			if error = 'owner' then exit(CLOUD_ERROR_OWNER);
			if error = 'trees_conflict' then exit(CLOUD_ERROR_TREES_CONFLICT);
			if error = 'user_limit_exceeded' then exit(CLOUD_ERROR_USER_LIMIT_EXCEEDED);
			if error = 'export_limit_exceeded' then exit(CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED);
			if error = 'unprocessable_entry' then exit(CLOUD_ERROR_UNPROCESSABLE_ENTRY);

			exit(CLOUD_ERROR_UNKNOWN); //Эту ошибку мы пока не встречали
		end;
	except
		on E: {EJSON}Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON);
			exit(CLOUD_ERROR_UNKNOWN);
		end;
	end;
	Result := CLOUD_OPERATION_OK;
end;

function TCloudMailRu.fromJSON_PublicLink(JSON: WideString; var PublicLink: WideString): Boolean;
begin
	Result := true;
	try
		PublicLink := (TJSONObject.ParseJSONValue(JSON) as TJSONObject).values['body'].Value;
	except
		Result := false;
	end;
end;

function TCloudMailRu.fromJSON_Shard(JSON: WideString; var Shard: WideString): Boolean;
begin
	Result := true;
	try
		Shard := ((((TJSONObject.ParseJSONValue(JSON) as TJSONObject).values['body'] as TJSONObject).values['get'] as TJSONArray).Items[0] as TJSONObject).values['url'].Value;
	except
		Result := false;
	end;

end;

function TCloudMailRu.fromJSON_UserSpace(JSON: WideString; var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean;
var
	Obj: TJSONObject;
begin
	Result := true;
	try
		Obj := (TJSONObject.ParseJSONValue(JSON) as TJSONObject).values['body'] as TJSONObject;
		with CloudMailRuSpaceInfo do
		begin
			if Assigned(Obj.values['overquota']) then overquota := Obj.values['overquota'].Value.ToBoolean;
			if Assigned(Obj.values['total']) then total := Obj.values['total'].Value.ToInt64;
			if Assigned(Obj.values['used']) then used := Obj.values['used'].Value.ToInt64;
		end;
	except
		Result := false;
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

function TCloudMailRu.getDescriptionFile(remotePath, localCopy: WideString): integer; //0 - ok, else error
begin
	Result := self.getFile(remotePath, localCopy, false);
end;

function TCloudMailRu.getSharedLinksListing(var DirListing: TCloudMailRuDirListing; ShowProgress: Boolean = false): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	Result := false;
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;
	Result := self.HTTPGet(API_FOLDER_SHARED_LINKS + '?' + self.united_params, JSON, ShowProgress);

	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := self.fromJSON_DirListing(JSON, DirListing);
			else
				begin
					Log(MSGTYPE_IMPORTANTERROR, 'Shared links listing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;
	Result := self.HTTPGet(API_FOLDER_SHARED_INCOMING + '?' + self.united_params, JSON, ShowProgress);

	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := self.fromJSON_IncomingInviteListing(JSON, IncomingListing);
			else
				begin
					Log(MSGTYPE_IMPORTANTERROR, 'Incoming requests listing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;
	Result := self.HTTPGet(API_TRASHBIN + '?' + self.united_params, JSON, ShowProgress);

	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := self.fromJSON_DirListing(JSON, DirListing);
			else
				begin
					Log(MSGTYPE_IMPORTANTERROR, 'Incoming requests listing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации

	if self.public_account then Result := self.HTTPGet(API_FOLDER + '&weblink=' + IncludeSlash(self.public_link) + PathToUrl(Path, false) + self.united_params, JSON, ShowProgress)
	else Result := self.HTTPGet(API_FOLDER + '&home=' + PathToUrl(Path) + self.united_params, JSON, ShowProgress);
	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := self.fromJSON_DirListing(JSON, DirListing);
			CLOUD_ERROR_NOT_EXISTS:
				begin
					Log(MSGTYPE_IMPORTANTERROR, 'Path not exists: ' + Path);
					Result := false;
				end
			else
				begin
					Log(MSGTYPE_IMPORTANTERROR, 'Directory listing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString()); //?? WUT
					Result := false;
				end;
		end;
	end;
end;

function TCloudMailRu.getFile(remotePath, localPath: WideString; LogErrors: Boolean): integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then Result := self.getFileShared(remotePath, localPath, LogErrors)
	else Result := self.getFileRegular(remotePath, localPath, LogErrors);
end;

function TCloudMailRu.getFileRegular(remotePath, localPath: WideString; LogErrors: Boolean): integer;
var
	FileStream: TFileStream;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if self.Shard = '' then
	begin
		Log(MSGTYPE_DETAILS, 'Current shard is undefined, trying to get one');
		if self.getShard(self.Shard) then
		begin
			Log(MSGTYPE_DETAILS, 'Current shard: ' + self.Shard);
		end else begin //А вот теперь это критическая ошибка, тут уже не получится копировать
			Log(MSGTYPE_IMPORTANTERROR, 'Sorry, downloading impossible');
			exit;
		end;
	end;
	try
		FileStream := TFileStream.Create(GetUNCFilePath(localPath), fmCreate);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, E.Message);
			exit(FS_FILE_WRITEERROR);
		end;
	end;
	if (Assigned(FileStream)) then
	begin
		self.ExternalSourceName := PWideChar(remotePath);
		self.ExternalTargetName := PWideChar(localPath);
		Result := self.HTTPGetFile(self.Shard + PathToUrl(remotePath, false), FileStream, LogErrors);
		FlushFileBuffers(FileStream.Handle);
		FileStream.free;
	end;
	if Result <> FS_FILE_OK then System.SysUtils.deleteFile(GetUNCFilePath(localPath));
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
	if (self.public_shard = '') or (self.public_download_token = '') then exit;
	try
		FileStream := TFileStream.Create(GetUNCFilePath(localPath), fmCreate);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, E.Message);
			exit(FS_FILE_WRITEERROR);
		end;
	end;
	if (Assigned(FileStream)) then
	begin
		Result := self.HTTPGetFile(getSharedFileUrl(remotePath), FileStream, LogErrors);
		FlushFileBuffers(FileStream.Handle);
		FileStream.free;
	end;
	if Result <> FS_FILE_OK then System.SysUtils.deleteFile(GetUNCFilePath(localPath));
end;

function TCloudMailRu.getOAuthToken(var OAuthToken: TCloudMailRuOAuthInfo): Boolean;
var
	Answer: WideString;
begin
	Result := false;
	if self.HTTPPost(OAUTH_TOKEN_URL, 'client_id=cloud-win&grant_type=password&username=' + self.user + '%40' + self.domain + '&password=' + UrlEncode(self.password), Answer) then
	begin
		if not self.fromJSON_OAuthTokenInfo(Answer, OAuthToken) then exit(false);
		Result := OAuthToken.error_code = NOERROR;
	end;
end;

function TCloudMailRu.getShard(var Shard: WideString): Boolean;
var
	JSON: WideString;
	OperationResult, OperationStatus: integer;
begin
	Result := false;
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.HTTPPost(API_DISPATCHER, self.united_params, JSON) then //checkme
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := self.fromJSON_Shard(JSON, Shard) and (Shard <> '');
			else
				begin
					Result := false;
					Log(MSGTYPE_IMPORTANTERROR, 'Get shard error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	Progress := false;
	Result := self.HTTPGet(self.PUBLIC_URL, PageContent, Progress);
	if Result then
	begin
		PageContent := StringReplace(PageContent, #$A, '', [rfReplaceAll]); //так нам проще ковыряться в тексте
		PageContent := StringReplace(PageContent, #$D, '', [rfReplaceAll]);
		PageContent := StringReplace(PageContent, #9, '', [rfReplaceAll]);
		if not self.extractPublicTokenFromText(PageContent, self.public_download_token) then //refresh public download token
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Can''t get public share download token');
			exit(false);
		end;
		if not self.extractPublicShard(PageContent, self.public_shard) then
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Can''t get public share download share');
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	Progress := false;
	Result := self.HTTPGet(API_USER_SPACE + '?home=/' + self.united_params, JSON, Progress);
	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := self.fromJSON_UserSpace(JSON, SpaceInfo);
			else
				begin
					Result := false;
					Log(MSGTYPE_IMPORTANTERROR, 'User space receiving error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
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
		if ProgressEnabled then HTTP.OnWork := self.HTTPProgress; //Вызов прогресса ведёт к возможности отменить получение списка каталогов и других операций, поэтому он нужен не всегда
		Answer := HTTP.Get(URL);
		self.HTTPDestroy(HTTP, SSL);
	Except
		on E: EAbort do
		begin
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
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
				Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL + ', ответ сервера: ' + E.ErrorMessage);
				Result := false;
			end;
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
			exit;
		end;
		on E: EIdSocketerror do
		begin
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
			Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка сети: ' + E.Message + ' при запросе данных с адреса ' + URL);
			exit(false);
		end;
		on E: Exception do
		begin
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
			Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при запросе данных с адреса ' + URL);
			exit(false);
		end;
	end;
	Result := Answer <> '';
end;

function TCloudMailRu.HTTPGetFile(URL: WideString; var FileStream: TFileStream; LogErrors: Boolean): integer;
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
			Log(MSGTYPE_IMPORTANTERROR, 'Достигнуто максимальное количество перенаправлений при запросе файла с адреса ' + URL);
			Result := FS_FILE_READERROR;
		end;
		self.HTTPDestroy(HTTP, SSL);
	except
		on E: EAbort do
		begin
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
			Result := FS_FILE_USERABORT;
		end;
		on E: EIdSocketerror do
		begin
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
			if LogErrors then Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка сети: ' + E.Message + ' при копировании файла с адреса ' + URL);
			Result := FS_FILE_READERROR;
		end;
		on E: Exception do
		begin
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
			if LogErrors then Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при копировании файла с адреса ' + URL);
			Result := FS_FILE_READERROR;
		end;
	end;
end;

procedure TCloudMailRu.HTTPInit(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL; var Socks: TIdSocksInfo; var Cookie: TIdCookieManager);
begin
	SSL := TIdSSLIOHandlerSocketOpenSSL.Create();
	HTTP := TIdHTTP.Create();
	if (self.Proxy.ProxyType in SocksProxyTypes) and (self.Socks.Enabled) then SSL.TransparentProxy := self.Socks;
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
	HTTP.HTTPOptions := [hoForceEncodeParams, hoNoParseMetaHTTPEquiv];
	HTTP.HandleRedirects := true;
	if (self.ConnectTimeout < 0) then
	begin
		HTTP.ConnectTimeout := self.ConnectTimeout;
		HTTP.ReadTimeout := self.ConnectTimeout;
	end;
	HTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.57 Safari/537.17/TCWFX(' + PlatformX + ')';
end;

function TCloudMailRu.HTTPPost(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString): Boolean;
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
		if ContentType <> '' then HTTP.Request.ContentType := ContentType;
		try
			HTTP.Post(URL, PostData, MemStream);
		except
			on E: EIdOSSLCouldNotLoadSSLLibrary do
			begin
				Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL);
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
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
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
				Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL + ', ответ сервера: ' + E.ErrorMessage);
				Result := false;
			end;
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
		end;
		on E: EIdSocketerror do
		begin
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
			Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка сети: ' + E.Message + ' при отправке данных на адрес ' + URL);
			Result := false;
		end;
	end;
	MemStream.free;
	PostData.free;
end;

function TCloudMailRu.HTTPPostFile(URL: WideString; FileName: WideString; var Answer: WideString): integer;
var
	MemStream: TStringStream;
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
	Socks: TIdSocksInfo;
	PostData: TIdMultipartFormDataStream;
begin
	Result := CLOUD_OPERATION_OK;
	MemStream := TStringStream.Create;
	PostData := TIdMultipartFormDataStream.Create;
	PostData.AddFile('file', FileName, 'application/octet-stream');
	try
		self.HTTPInit(HTTP, SSL, Socks, self.Cookie);
		HTTP.OnWork := self.HTTPProgress;
		HTTP.Post(URL, PostData, MemStream);
		Answer := MemStream.DataString;
		self.HTTPDestroy(HTTP, SSL);
	except
		on E: EAbort do
		begin
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
			Result := CLOUD_OPERATION_CANCELLED;
		end;
		on E: EIdHTTPProtocolException do
		begin
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
			Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL + ', ответ сервера: ' + E.ErrorMessage);
			Result := CLOUD_OPERATION_FAILED;
		end;
		on E: EIdSocketerror do
		begin
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
			Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка сети: ' + E.Message + ' при отправке данных на адрес ' + URL);
			Result := CLOUD_OPERATION_FAILED;
		end;
		on E: Exception do
		begin
			if Assigned(HTTP) then self.HTTPDestroy(HTTP, SSL);
			Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL);
			Result := CLOUD_OPERATION_FAILED;
		end;
	end;
	MemStream.free;
	PostData.free;
end;

procedure TCloudMailRu.HTTPProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
var
	HTTP: TIdHTTP;
	ContentLength: int64;
	Percent: integer;
begin
	HTTP := TIdHTTP(ASender);
	if AWorkMode = wmRead then ContentLength := HTTP.Response.ContentLength
	else ContentLength := HTTP.Request.ContentLength; //Считаем размер обработанных данных зависимости от того, скачивание это или загрузка
	if (Pos('chunked', LowerCase(HTTP.Response.TransferEncoding)) = 0) and (ContentLength > 0) then
	begin
		Percent := 100 * AWorkCount div ContentLength;
		if Assigned(ExternalProgressProc) and (ExternalProgressProc(self.ExternalPluginNr, self.ExternalSourceName, self.ExternalTargetName, Percent) = 1) then abort;
	end;
end;

procedure TCloudMailRu.Log(MsgType: integer; LogString: WideString);
begin
	if Assigned(ExternalLogProc) then ExternalLogProc(ExternalPluginNr, MsgType, PWideChar(LogString));
end;

function TCloudMailRu.login(method: integer): Boolean;
begin
	Result := false;
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then Result := self.loginShared()
	else Result := self.loginRegular(method);
end;

function TCloudMailRu.loginRegular(method: integer): Boolean;
var
	PostAnswer: WideString;
	FirstStepToken: WideString;
	SecurityKey: PWideChar;
begin
	Result := false;
	SecurityKey := nil;
	self.login_method := method;
	Log(MSGTYPE_DETAILS, 'Login to ' + self.user + '@' + self.domain);
	case self.login_method of
		CLOUD_AUTH_METHOD_TWO_STEP:
			begin
				Result := self.HTTPPost(LOGIN_URL, 'page=https://cloud.mail.ru/?new_auth_form=1&Domain=' + self.domain + '&Login=' + self.user + '&Password=' + UrlEncode(self.password) + '&FailPage=', PostAnswer);
				if Result then
				begin
					Log(MSGTYPE_DETAILS, 'Requesting auth token for ' + self.user + '@' + self.domain);
					if (self.extractTokenFromText(PostAnswer, FirstStepToken)) then
					begin
						Log(MSGTYPE_DETAILS, 'Awaiting for security key... ');
						if (ExternalRequestProc(self.ExternalPluginNr, RT_Other, 'Enter auth key', nil, SecurityKey, 32)) then
						begin
							Result := self.HTTPPost(SECSTEP_URL, 'Login=' + self.user + '@' + self.domain + '&csrf=' + FirstStepToken + '&AuthCode=' + SecurityKey, PostAnswer);
							if Result then
							begin
								Result := self.getToken();
								if (Result) then
								begin
									Log(MSGTYPE_DETAILS, 'Connected to ' + self.user + '@' + self.domain);
									self.logUserSpaceInfo;
								end else begin
									Log(MSGTYPE_IMPORTANTERROR, 'error: twostep auth failed');
									exit(false);
								end;
							end;
						end else begin
							Log(MSGTYPE_IMPORTANTERROR, 'error: security key not provided');
							exit(false);
						end;

					end else begin
						Log(MSGTYPE_IMPORTANTERROR, 'error: getting auth token for ' + self.user + '@' + self.domain);
						exit(false);
					end;

				end;
			end;
		CLOUD_AUTH_METHOD_WEB: //todo: вынести в отдельный метод
			begin
				Result := self.HTTPPost(LOGIN_URL, 'page=https://cloud.mail.ru/?new_auth_form=1&Domain=' + self.domain + '&Login=' + self.user + '&Password=' + UrlEncode(self.password) + '&FailPage=', PostAnswer);
				if (Result) then
				begin
					Log(MSGTYPE_DETAILS, 'Requesting auth token for ' + self.user + '@' + self.domain);
					Result := self.getToken();
					if (Result) then
					begin
						Log(MSGTYPE_DETAILS, 'Connected to ' + self.user + '@' + self.domain);
						self.logUserSpaceInfo;
					end else begin
						Log(MSGTYPE_IMPORTANTERROR, 'error: getting auth token for ' + self.user + '@' + self.domain);
						exit(false);
					end;
				end
				else Log(MSGTYPE_IMPORTANTERROR, 'error: login to ' + self.user + '@' + self.domain);
			end;
		CLOUD_AUTH_METHOD_OAUTH:
			begin
				Result := self.getOAuthToken(self.OAuthToken);
				if not Result then Log(MSGTYPE_IMPORTANTERROR, 'OAuth error: ' + self.OAuthToken.error + '(' + self.OAuthToken.error_description + ')');
			end;
	end;
end;

function TCloudMailRu.loginShared(method: integer): Boolean;
begin
	Log(MSGTYPE_DETAILS, 'Open ' + self.PUBLIC_URL);
	Result := self.getSharedToken();
	//exit(true);
end;

procedure TCloudMailRu.logUserSpaceInfo;
var
	US: TCloudMailRuSpaceInfo;
	QuotaInfo: WideString;

begin
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;
	if self.getUserSpace(US) then
	begin
		if (US.overquota) then QuotaInfo := ' Warning: space quota exhausted!'
		else QuotaInfo := '';
		Log(MSGTYPE_DETAILS, 'Total space: ' + FormatSize(US.total) + ', used: ' + FormatSize(US.used) + ', free: ' + FormatSize(US.total - US.used) + '.' + QuotaInfo);
	end else begin
		Log(MSGTYPE_IMPORTANTERROR, 'error: getting user space information for ' + self.user + '@' + self.domain);
	end;
end;

function TCloudMailRu.moveFile(OldName, ToPath: WideString): integer;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit(FS_FILE_NOTSUPPORTED);
	if self.HTTPPost(API_FILE_MOVE, 'home=' + PathToUrl(OldName) + '&folder=' + PathToUrl(ToPath) + self.united_params + '&conflict', JSON) then
	begin //Парсим ответ
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
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
		if Result <> CLOUD_OPERATION_OK then exit;
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;
	if publish then
	begin
		Result := self.HTTPPost(API_FILE_PUBLISH, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', JSON);
	end else begin
		Result := self.HTTPPost(API_FILE_UNPUBLISH, 'weblink=' + PublicLink + self.united_params + '&conflict', JSON);
	end;

	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: if publish then Result := self.fromJSON_PublicLink(JSON, PublicLink);
			else
				begin
					Result := false;
					Log(MSGTYPE_IMPORTANTERROR, 'File publish error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	Progress := false;
	if self.HTTPGet(API_FOLDER_SHARED_INFO + '?home=' + PathToUrl(Path) + self.united_params, JSON, Progress) then
	begin
		Result := self.fromJSON_InviteListing(JSON, InviteListing);
	end;

end;

function TCloudMailRu.shareFolder(Path, email: WideString; access: integer): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
	access_string: WideString;
begin
	Result := false;
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if access in [CLOUD_SHARE_RW, CLOUD_SHARE_RO] then
	begin
		if access = CLOUD_SHARE_RW then access_string := CLOUD_SHARE_ACCESS_READ_WRITE
		else access_string := CLOUD_SHARE_ACCESS_READ_ONLY;

		Result := self.HTTPPost(API_FOLDER_SHARE, 'home=/' + PathToUrl(Path) + self.united_params + '&invite={"email":"' + email + '","access":"' + access_string + '"}', JSON)
	end else begin
		Result := (self.HTTPPost(API_FOLDER_UNSHARE, 'home=/' + PathToUrl(Path) + self.united_params + '&invite={"email":"' + email + '"}', JSON));
	end;

	if (Result) then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);

		Result := OperationResult = CLOUD_OPERATION_OK;
		if not Result then Log(MSGTYPE_IMPORTANTERROR, 'Invite member error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());

	end;
end;

function TCloudMailRu.trashbinRestore(Path: WideString; RestoreRevision: integer; ConflictMode: WideString): Boolean;
var
	JSON: WideString;
	OperationStatus, OperationResult: integer;
begin
	Result := false;
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;

	Result := self.HTTPPost(API_TRASHBIN_RESTORE, 'path=' + PathToUrl(Path) + '&restore_revision=' + RestoreRevision.ToString + self.united_params + '&conflict=' + ConflictMode, JSON);

	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := true;
			else
				begin
					Result := false;
					Log(MSGTYPE_IMPORTANTERROR, 'File restore error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;

	Result := self.HTTPPost(API_TRASHBIN_EMPTY, self.united_params, JSON);

	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := true;
			else
				begin
					Result := false;
					Log(MSGTYPE_IMPORTANTERROR, 'Trashbin clearing error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;

	Result := self.HTTPPost(API_FOLDER_MOUNT, 'home=' + UrlEncode(home) + '&invite_token=' + invite_token + self.united_params + '&conflict=' + ConflictMode, JSON);

	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := true;
			else
				begin
					Result := false;
					Log(MSGTYPE_IMPORTANTERROR, 'Folder mount error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;
	if clone_copy then CopyStr := 'true'
	else CopyStr := 'false';

	Result := self.HTTPPost(API_FOLDER_UNMOUNT, 'home=' + UrlEncode(home) + '&clone_copy=' + CopyStr + self.united_params, JSON);

	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := true;
			else
				begin
					Result := false;
					Log(MSGTYPE_IMPORTANTERROR, 'Folder mount error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;

	Result := self.HTTPPost(API_INVITE_REJECT, 'invite_token=' + invite_token + self.united_params, JSON);

	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := true;
			else
				begin
					Result := false;
					Log(MSGTYPE_IMPORTANTERROR, 'Folder mount error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
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
		Splitter := TFileSplitter.Create(localPath, self.split_file_size, self.ExternalProgressProc, self.ExternalPluginNr);
		SplitResult := Splitter.split();
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'File splitting error: ' + E.Message + ', ignored');
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
				Log(MSGTYPE_DETAILS, 'File splitting aborted by user, uploading aborted');
				Splitter.Destroy;
				exit(FS_FILE_USERABORT);
			end;
		else
			begin
				Log(MSGTYPE_IMPORTANTERROR, 'File splitting error, code: ' + SplitResult.ToString + ', ignored');
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
						Log(MSGTYPE_DETAILS, 'Partial upload aborted');
						Splitter.Destroy;
						exit(FS_FILE_USERABORT);
					end;
				FS_FILE_EXISTS:
					begin
						case ChunkOverwriteMode of
							ChunkOverwrite: //silently overwrite chunk
								begin
									Log(MSGTYPE_DETAILS, 'Chunk ' + ChunkFileName + ' already exists, overwriting.');
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
									Log(MSGTYPE_DETAILS, 'Chunk ' + ChunkFileName + ' already exists, skipping.');
									Continue;
								end;
							ChunkOverwriteAbort: //abort operation
								begin
									Log(MSGTYPE_DETAILS, 'Chunk ' + ChunkFileName + ' already exists, aborting.');
									Splitter.Destroy;
									exit(FS_FILE_NOTSUPPORTED);
								end;
						end;
					end;
				else
					begin
						Log(MSGTYPE_IMPORTANTERROR, 'Partial upload error, code: ' + Result.ToString);
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
				Log(MSGTYPE_DETAILS, 'Partial upload aborted');
				Splitter.Destroy;
				exit(FS_FILE_USERABORT);
			end;
		FS_FILE_EXISTS:
			begin
				case ChunkOverwriteMode of
					ChunkOverwrite: //silently overwrite chunk
						begin
							Log(MSGTYPE_DETAILS, CRCFileName + ' checksum file already exists, overwriting.');
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
							Log(MSGTYPE_DETAILS, CRCFileName + ' checksum file already exists, skipping.');
						end;
					ChunkOverwriteAbort: //abort operation
						begin
							Log(MSGTYPE_DETAILS, CRCFileName + ' checksum file already exists, aborting.');
							Splitter.Destroy;
							exit(FS_FILE_NOTSUPPORTED);
						end;
				end;
			end;
		else
			begin
				Log(MSGTYPE_IMPORTANTERROR, 'Checksum file  upload error, code: ' + Result.ToString);
				Splitter.Destroy;
				exit;
			end;
	end;
	Splitter.Destroy;
	exit(FS_FILE_OK); //Файлик залит по частям, выходим
end;

function TCloudMailRu.putFile(localPath, remotePath, ConflictMode: WideString; ChunkOverwriteMode: integer): integer;
var
	PutResult: TStringList;
	JSONAnswer, FileHash: WideString;
	FileSize: int64;
	Code, OperationStatus: integer;
	OperationResult: integer;
begin
	if not(Assigned(self)) then exit(FS_FILE_WRITEERROR); //Проверка на вызов без инициализации
	if self.public_account then exit(FS_FILE_NOTSUPPORTED);

	if (not(self.unlimited_filesize)) and (SizeOfFile(GetUNCFilePath(localPath)) > self.split_file_size) then //todo вынести в процiдурку
	begin
		if self.split_large_files then
		begin
			Log(MSGTYPE_DETAILS, 'File size > ' + self.split_file_size.ToString() + ' bytes, file will be splitted.');
			exit(putFileSplit(localPath, remotePath, ConflictMode, ChunkOverwriteMode));
		end else begin
			Log(MSGTYPE_IMPORTANTERROR, 'File size > ' + self.split_file_size.ToString() + ' bytes, ignored');
			exit(FS_FILE_NOTSUPPORTED);
		end;
	end;
	FileSize := 0;
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
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
				Log(MSGTYPE_IMPORTANTERROR, 'error: uploading to cloud: ' + E.ClassName + ' ошибка с сообщением: ' + E.Message);
				Result := FS_FILE_WRITEERROR;
			end;
		end;
	end;
	if OperationResult = CLOUD_OPERATION_OK then
	begin
		FileHash := PutResult.Strings[0];
		Val(PutResult.Strings[1], FileSize, Code); //Тут ошибка маловероятна
	end else if OperationResult = CLOUD_OPERATION_CANCELLED then
	begin
		Result := FS_FILE_USERABORT;
	end;
	PutResult.free;
	if OperationResult = CLOUD_OPERATION_OK then
	begin
		//Log( MSGTYPE_DETAILS, 'putFileToCloud result: ' + PutResult.Text);
		if self.addFileToCloud(FileHash, FileSize, PathToUrl(remotePath), JSONAnswer) then
		begin
			OperationResult := self.fromJSON_OperationResult(JSONAnswer, OperationStatus);
			Result := CloudResultToFsResult(OperationResult, OperationStatus, 'File uploading error: ');
		end;
	end;
end;

function TCloudMailRu.putFileToCloud(localPath: WideString; Return: TStringList): integer; {Заливка на сервер состоит из двух шагов: заливаем файл на сервер в putFileToCloud и добавляем его в облако addFileToCloud}
var
	PostAnswer: WideString;
begin
	Result := CLOUD_OPERATION_FAILED;
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;
	Result := self.HTTPPost(API_FILE_REMOVE, 'home=/' + IncludeSlash(PathToUrl(Path)) + self.united_params + '&conflict', JSON); //API всегда отвечает true, даже если путь не существует
	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := true;
			else
				begin
					Result := false;
					Log(MSGTYPE_IMPORTANTERROR, 'Delete directory error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	if self.public_account then exit;
	if self.HTTPPost(API_FILE_RENAME, 'home=' + PathToUrl(OldName) + '&name=' + PathToUrl(NewName) + self.united_params, JSON) then
	begin //Парсим ответ
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
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
	if not(Assigned(self)) then exit; //Проверка на вызов без инициализации
	Progress := false;
	if self.public_account then Result := self.HTTPGet(API_FILE + '?weblink=' + IncludeSlash(self.public_link) + PathToUrl(Path) + self.united_params, JSON, Progress)
	else Result := self.HTTPGet(API_FILE + '?home=' + PathToUrl(Path) + self.united_params, JSON, Progress);

	if Result then
	begin
		OperationResult := self.fromJSON_OperationResult(JSON, OperationStatus);
		case OperationResult of
			CLOUD_OPERATION_OK: Result := fromJSON_FileStatus(JSON, FileInfo);
			else
				begin
					Log(MSGTYPE_IMPORTANTERROR, 'File status error: ' + self.ErrorCodeText(OperationResult) + ' Status: ' + OperationStatus.ToString());
					Result := false;
				end;
		end;
	end;
	//if not Result then exit(false);
end;

class function TCloudMailRu.CloudAccessToString(access: WideString; Invert: Boolean): WideString;
begin
	if access = 'read only' then access := CLOUD_SHARE_ACCESS_READ_ONLY;
	if access = 'read and write' then access := CLOUD_SHARE_ACCESS_READ_WRITE;
	if Invert then
	begin
		if (access = CLOUD_SHARE_ACCESS_READ_ONLY) then access := CLOUD_SHARE_ACCESS_READ_WRITE
		else access := CLOUD_SHARE_ACCESS_READ_ONLY;
	end;
	if access = CLOUD_SHARE_ACCESS_READ_ONLY then Result := 'read only'
	else Result := 'read and write';
end;

class function TCloudMailRu.StringToCloudAccess(accessString: WideString; Invert: Boolean): integer;
begin
	if accessString = 'read only' then accessString := CLOUD_SHARE_ACCESS_READ_ONLY;
	if accessString = 'read and write' then accessString := CLOUD_SHARE_ACCESS_READ_WRITE;
	if Invert then
	begin
		if (accessString = CLOUD_SHARE_ACCESS_READ_ONLY) then accessString := CLOUD_SHARE_ACCESS_READ_WRITE
		else accessString := CLOUD_SHARE_ACCESS_READ_ONLY;
	end;
	if accessString = CLOUD_SHARE_ACCESS_READ_ONLY then Result := CLOUD_SHARE_RO
	else Result := CLOUD_SHARE_RW;
end;

end.
