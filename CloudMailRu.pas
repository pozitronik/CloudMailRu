unit CloudMailRu;

interface

uses CMLJSON, CMLParsers, CMLTypes, CMLHTTP, System.Hash, System.Classes, System.Generics.Collections, System.SysUtils, PLUGIN_Types, Winapi.Windows, MRC_helper, Settings, Cipher, Splitfile, ChunkedFileStream;

type
	TCloudMailRu = class
	private
		{VARIABLES}
		OptionsSet: TCloudSettings;

		public_download_token: WideString;
		public_shard: WideString;

		token: WideString;
		OAuthToken: TCloudMailRuOAuthInfo;
		x_page_id: WideString;
		build: WideString;
		upload_url: WideString;
		login_method: integer;

		ExternalProgressProc: TProgressHandler;
		ExternalLogProc: TLogHandler;
		ExternalRequestProc: TRequestHandler;
		Shard: WideString;

		OperationErrorMode: integer; {implementation in progress}
		RetryAttempts: integer;
		AttemptWait: integer;
		{todo: use plugin settings as is}

		HTTP: TCloudMailRuHTTP; //HTTP transport class
		FileCipher: TFileCipher;

		united_params: WideString; //Объединённый набор авторизационных параметров для подстановки в URL

		{HTTP REQUESTS WRAPPERS}
		function getToken(): Boolean;
		function getSharedToken(): Boolean;
		function getOAuthToken(var OAuthToken: TCloudMailRuOAuthInfo): Boolean;
		function getShard(var Shard: WideString): Boolean;
		function getUserSpace(var SpaceInfo: TCloudMailRuSpaceInfo): Boolean;
		function putFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCloudMailRuFileIdentity): integer; overload; //отправка на сервер данных из потока
		{PRIVATE UPLOAD METHODS CHAIN (CALLED FROM putFile())}
		function putFileWhole(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer; //Загрузка файла целиком
		function putFileSplit(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: integer = 0): integer; //Загрузка файла по частям
		function putFileStream(FileName, remotePath: WideString; FileStream: TStream; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer;

		{OTHER ROUTINES}
		procedure Log(LogLevel, MsgType: integer; LogString: WideString);
		function CloudResultToFsResult(CloudResult: integer; OperationStatus: integer; ErrorPrefix: WideString = ''): integer;
		function cloudHash(Path: WideString): WideString; overload; //get cloud hash for specified file
		function cloudHash(Stream: TStream): WideString; overload; //get cloud hash for data in stream
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

		Property isPublicShare: Boolean read OptionsSet.AccountSettings.public_account;

		Property user: WideString read OptionsSet.AccountSettings.user;
		Property domain: WideString read OptionsSet.AccountSettings.domain;

		Property password: WideString read OptionsSet.AccountSettings.password;

		Property split_file_size: integer read OptionsSet.CloudMaxFileSize;
		Property PrecalculateHash: Boolean read OptionsSet.PrecalculateHash;
		Property CheckCRC: Boolean read OptionsSet.CheckCRC;
		Property shard_override: WideString read OptionsSet.AccountSettings.shard_override;
		Property upload_url_override: WideString read OptionsSet.AccountSettings.upload_url_override;
		Property unlimited_filesize: Boolean read OptionsSet.AccountSettings.unlimited_filesize;
		Property split_large_files: Boolean read OptionsSet.AccountSettings.split_large_files;

		Property Transport: TCloudMailRuHTTP read HTTP;

		function getSharedFileUrl(remotePath: WideString; DoUrlEncode: Boolean = true): WideString;
		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(CloudSettings: TCloudSettings; ExternalProgressProc: TProgressHandler = nil; ExternalLogProc: TLogHandler = nil; ExternalRequestProc: TRequestHandler = nil);
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

		function addFileByIdentity(FileIdentity: TCloudMailRuFileIdentity; remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = false): integer; overload;
		function addFileByIdentity(FileIdentity: TCloudMailRuDirListingItem; remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = false): integer; overload;

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
		class function IsSameIdentity(IdentityOne, IdentityTwo: TCloudMailRuFileIdentity): Boolean;
	end;

implementation

{TCloudMailRu}
{TODO: LogErrors unused}
function TCloudMailRu.addFileByIdentity(FileIdentity: TCloudMailRuFileIdentity; remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = false): integer;
var
	JSON, FileName: WideString;
	OperationStatus, OperationResult: integer;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.isPublicShare then
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
		OperationResult := fromJSON_OperationResult(JSON, OperationStatus);
		if CLOUD_OPERATION_OK = OperationResult then
		begin
			if LogSuccess then
				Log(LogLevelDetail, MSGTYPE_DETAILS, 'File ' + remotePath + ' found by hash.');

			exit(FS_FILE_OK);
		end else begin
			result := CloudResultToFsResult(OperationResult, OperationStatus, 'File uploading error: ');
		end;
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
	OperationStatus: integer;
	Progress: Boolean;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.isPublicShare then
		exit(FS_FILE_NOTSUPPORTED);
	Progress := true;
	if self.HTTP.GetPage(API_CLONE + '?folder=' + PathToUrl(Path) + '&weblink=' + link + '&conflict=' + ConflictMode + self.united_params, JSON, Progress) then
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
					Log(LogLevelError, MSGTYPE_IMPORTANTERROR, ErrorPrefix + self.ErrorCodeText(CloudResult) + ' Status: ' + OperationStatus.ToString());
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
	OperationStatus, OperationResult: integer;
begin
	result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.isPublicShare then
		exit(FS_FILE_NOTSUPPORTED);
	if self.HTTP.PostForm(API_FILE_COPY, 'home=' + PathToUrl(OldName) + '&folder=' + PathToUrl(ToPath) + self.united_params + '&conflict', JSON) then
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


constructor TCloudMailRu.Create(CloudSettings: TCloudSettings; ExternalProgressProc: TProgressHandler; ExternalLogProc: TLogHandler; ExternalRequestProc: TRequestHandler);
begin
	try
		self.ExternalProgressProc := ExternalProgressProc;
		self.ExternalLogProc := ExternalLogProc;
		self.ExternalRequestProc := ExternalRequestProc;



		self.HTTP := TCloudMailRuHTTP.Create(CloudSettings.ConnectionSettings, ExternalProgressProc, ExternalLogProc);

		if CloudSettings.AccountSettings.encrypt_files_mode <> EncryptModeNone then
		begin
			self.FileCipher := TFileCipher.Create(CloudSettings.AccountSettings.crypt_files_password, CloudSettings.AccountSettings.CryptedGUID_files, CloudSettings.AccountSettings.encrypt_filenames);
			if self.FileCipher.WrongPassword then
				Log(LogLevelError, MSGTYPE_IMPORTANTERROR, 'Wrong encryption password, encryption support disabled');

			self.crypt_files := not(self.FileCipher.WrongPassword);
			self.crypt_filenames := self.crypt_files and CloudSettings.AccountSettings.encrypt_filenames and not(self.FileCipher.WrongPassword);
		end;

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
	if self.isPublicShare then
		exit;
	if self.HTTP.PostForm(API_FOLDER_ADD, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', PostAnswer) then
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
	if self.isPublicShare then
		exit;
	result := self.HTTP.PostForm(API_FILE_REMOVE, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', JSON);
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
	self.HTTP.Destroy;
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
	if self.isPublicShare then
		exit;
	result := self.HTTP.GetPage(API_FOLDER_SHARED_LINKS + '?' + self.united_params, JSON, ShowProgress);

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
	if self.isPublicShare then
		exit;
	result := self.HTTP.GetPage(API_FOLDER_SHARED_INCOMING + '?' + self.united_params, JSON, ShowProgress);

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
	if self.isPublicShare then
		exit;
	result := self.HTTP.GetPage(API_TRASHBIN + '?' + self.united_params, JSON, ShowProgress);

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
	if self.isPublicShare then
		result := self.HTTP.GetPage(API_FOLDER + '&weblink=' + IncludeSlash(self.getPublicLink) + PathToUrl(Path, false) + self.united_params, JSON, ShowProgress)
	else
		result := self.HTTP.GetPage(API_FOLDER + '&home=' + PathToUrl(Path) + self.united_params, JSON, ShowProgress);
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

	self.HTTP.ExternalSourceName := PWideChar(remotePath);
	self.HTTP.ExternalTargetName := PWideChar(localPath);

	if self.isPublicShare then
		result := self.getFileShared(remotePath, localPath, resultHash, LogErrors)
	else
		result := self.getFileRegular(remotePath, localPath, resultHash, LogErrors);

	self.HTTP.ExternalSourceName := nil;
	self.HTTP.ExternalTargetName := nil;
end;

function TCloudMailRu.getFileRegular(remotePath, localPath: WideString; var resultHash: WideString; LogErrors: Boolean): integer;
var
	FileStream: TBufferedFileStream;
	URL, FileName: WideString;
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
		if (result in [FS_FILE_OK]) then
			resultHash := cloudHash(FileStream);
	end;

	FlushFileBuffers(FileStream.Handle);
	FileStream.free;

	if not(result in [FS_FILE_OK]) then
		System.SysUtils.deleteFile(GetUNCFilePath(localPath));
end;

function TCloudMailRu.getSharedFileUrl(remotePath: WideString; DoUrlEncode: Boolean = true): WideString;
begin
	result := IncludeSlash(self.public_shard) + IncludeSlash(self.getPublicLink) + PathToUrl(remotePath, true, DoUrlEncode) + '?key=' + self.public_download_token
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

function TCloudMailRu.getOAuthToken(var OAuthToken: TCloudMailRuOAuthInfo): Boolean;
var
	Answer: WideString;
begin
	result := false;
	if self.HTTP.PostForm(OAUTH_TOKEN_URL, 'client_id=cloud-win&grant_type=password&username=' + self.user + '%40' + self.domain + '&password=' + UrlEncode(self.password), Answer) then
	begin
		if not fromJSON_OAuthTokenInfo(Answer, OAuthToken) then
			exit(false);
		result := OAuthToken.error_code = NOERROR;
	end;
end;

function TCloudMailRu.getPublicLink: WideString; {Todo variable instaed of getter?}
begin
	result := EmptyWideStr;
	if self.isPublicShare and (self.OptionsSet.AccountSettings.public_url <> EmptyWideStr) then
	begin
		result := self.OptionsSet.AccountSettings.public_url;
		self.OptionsSet.AccountSettings.public_url := IncludeSlash(self.OptionsSet.AccountSettings.public_url);
		Delete(result, 1, length(PUBLIC_ACCESS_URL));
		if result[length(result)] = '/' then
			Delete(result, length(result), 1);
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

	if self.HTTP.PostForm(API_DISPATCHER, self.united_params, JSON) then //checkme
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
	result := self.HTTP.GetPage(TOKEN_URL, JSON, Progress);
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
	OperationResult, OperationStatus: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	Progress := false;
	result := self.HTTP.GetPage(API_USER_SPACE + '?home=/' + self.united_params, JSON, Progress);
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
	if self.isPublicShare then
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
				result := self.HTTP.PostMultipart(LOGIN_URL, FormFields, PostAnswer);
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
	if self.isPublicShare then
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
	if self.isPublicShare then
		exit(FS_FILE_NOTSUPPORTED);
	if self.HTTP.PostForm(API_FILE_MOVE, 'home=' + PathToUrl(OldName) + '&folder=' + PathToUrl(ToPath) + self.united_params + '&conflict', JSON) then
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
	if self.isPublicShare then
		exit;
	if publish then
	begin
		result := self.HTTP.PostForm(API_FILE_PUBLISH, 'home=/' + PathToUrl(Path) + self.united_params + '&conflict', JSON, 'application/x-www-form-urlencoded', true, false);
	end else begin
		result := self.HTTP.PostForm(API_FILE_UNPUBLISH, 'weblink=' + PublicLink + self.united_params + '&conflict', JSON, 'application/x-www-form-urlencoded', true, false);
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
	if self.HTTP.GetPage(API_FOLDER_SHARED_INFO + '?home=' + PathToUrl(Path) + self.united_params, JSON, Progress) then
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

		result := self.HTTP.PostForm(API_FOLDER_SHARE, 'home=/' + PathToUrl(Path) + self.united_params + '&invite={"email":"' + email + '","access":"' + access_string + '"}', JSON)
	end else begin
		result := (self.HTTP.PostForm(API_FOLDER_UNSHARE, 'home=/' + PathToUrl(Path) + self.united_params + '&invite={"email":"' + email + '"}', JSON));
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
	if self.isPublicShare then
		exit;

	result := self.HTTP.PostForm(API_TRASHBIN_RESTORE, 'path=' + PathToUrl(Path) + '&restore_revision=' + RestoreRevision.ToString + self.united_params + '&conflict=' + ConflictMode, JSON);

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
	if self.isPublicShare then
		exit;

	result := self.HTTP.PostForm(API_TRASHBIN_EMPTY, self.united_params, JSON);

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
	if self.isPublicShare then
		exit;

	result := self.HTTP.PostForm(API_FOLDER_MOUNT, 'home=' + UrlEncode(home) + '&invite_token=' + invite_token + self.united_params + '&conflict=' + ConflictMode, JSON);

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
	if self.isPublicShare then
		exit;
	if clone_copy then
		CopyStr := 'true'
	else
		CopyStr := 'false';

	result := self.HTTP.PostForm(API_FOLDER_UNMOUNT, 'home=' + UrlEncode(home) + '&clone_copy=' + CopyStr + self.united_params, JSON);

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
	if self.isPublicShare then
		exit;

	result := self.HTTP.PostForm(API_INVITE_REJECT, 'invite_token=' + invite_token + self.united_params, JSON);

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

function TCloudMailRu.putFileStream(FileName, remotePath: WideString; FileStream: TStream; ConflictMode: WideString): integer;
var
	LocalFileIdentity, RemoteFileIdentity: TCloudMailRuFileIdentity;
	OperationResult: integer;
	MemoryStream: TMemoryStream;
begin

	result := FS_FILE_WRITEERROR;
	OperationResult := CLOUD_OPERATION_FAILED;

	if self.PrecalculateHash or self.CheckCRC then
	begin
		LocalFileIdentity.Hash := cloudHash(FileStream);
		LocalFileIdentity.size := FileStream.size;
	end;
	if self.PrecalculateHash and (LocalFileIdentity.Hash <> EmptyWideStr) and (not self.crypt_files) and (FS_FILE_OK = self.addFileByIdentity(LocalFileIdentity, remotePath, CLOUD_CONFLICT_STRICT, false, true)) then {issue #135}
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
		result := self.addFileByIdentity(RemoteFileIdentity, remotePath, ConflictMode);
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
begin
	if self.PrecalculateHash then //try to add whole file by hash at first.
	begin
		LocalFileIdentity.Hash := cloudHash(GetUNCFilePath(localPath)); {TODO: GetIdentityOfFile}
		LocalFileIdentity.size := SizeOfFile(GetUNCFilePath(localPath));
	end;
	if self.PrecalculateHash and (LocalFileIdentity.Hash <> EmptyWideStr) and (not self.crypt_files) and (FS_FILE_OK = self.addFileByIdentity(LocalFileIdentity, remotePath, CLOUD_CONFLICT_STRICT, false, true)) then {issue #135}
		exit(CLOUD_OPERATION_OK);

	SplitFileInfo := TFileSplitInfo.Create(GetUNCFilePath(localPath), self.split_file_size); //quickly get information about file parts
	RetryAttemptsCount := 0;
	SplittedPartIndex := 0;

	while SplittedPartIndex < SplitFileInfo.ChunksCount do {use while instead for..loop, need to modify loop counter sometimes}
	begin
		ChunkRemotePath := ExtractFilePath(remotePath) + SplitFileInfo.GetChunks[SplittedPartIndex].name;
		self.HTTP.ExternalTargetName := PWideChar(ChunkRemotePath);
		Log(LogLevelDebug, MSGTYPE_DETAILS, 'Partial upload of ' + localPath + ' part ' + (SplittedPartIndex + 1).ToString + ' of ' + SplitFileInfo.ChunksCount.ToString + ' => ' + ChunkRemotePath);

		ChunkStream := TChunkedFileStream.Create(GetUNCFilePath(localPath), fmOpenRead or fmShareDenyWrite, SplitFileInfo.GetChunks[SplittedPartIndex].start, SplitFileInfo.GetChunks[SplittedPartIndex].size); {FIXME TODO: Bug here - TChunkedFileStream некорректно обрабатывает файлы больше какого-то лимита}
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
										begin
											{do nothing && continue}
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
						OperationErrorModeRetry: {TODO: documentation}
							begin
								Inc(RetryAttemptsCount);
								if RetryAttemptsCount <> RetryAttempts then
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
		self.HTTP.ExternalTargetName := PWideChar(CRCRemotePath);
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
	if self.isPublicShare then
		exit(FS_FILE_NOTSUPPORTED);
	self.HTTP.ExternalSourceName := PWideChar(remotePath);
	self.HTTP.ExternalTargetName := PWideChar(localPath);
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

	result := putFileWhole(localPath, remotePath, ConflictMode);
	self.HTTP.ExternalSourceName := nil;
	self.HTTP.ExternalTargetName := nil;

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
	if self.isPublicShare then
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
	OperationResult, OperationStatus: integer;
begin
	result := false;
	if not(Assigned(self)) then
		exit; //Проверка на вызов без инициализации
	if self.isPublicShare then
		exit;
	result := self.HTTP.PostForm(API_FILE_REMOVE, 'home=/' + IncludeSlash(PathToUrl(Path)) + self.united_params + '&conflict', JSON); //API всегда отвечает true, даже если путь не существует
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
	if self.isPublicShare then
		exit;
	if self.HTTP.PostForm(API_FILE_RENAME, 'home=' + PathToUrl(OldName) + '&name=' + PathToUrl(NewName) + self.united_params, JSON) then
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
	if self.isPublicShare then
		result := self.HTTP.GetPage(API_FILE + '?weblink=' + IncludeSlash(self.getPublicLink) + PathToUrl(Path) + self.united_params, JSON, Progress)
	else
		result := self.HTTP.GetPage(API_FILE + '?home=' + PathToUrl(Path) + self.united_params, JSON, Progress);

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
	result := cloudHash(Stream);
	Stream.Destroy;

end;

function TCloudMailRu.cloudHash(Stream: TStream): WideString;
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
		if (1 = ExternalProgressProc(nil, 'Calculating cloud hash', Percent)) then
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
