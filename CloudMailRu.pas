unit CloudMailRu;

interface

uses
	System.Classes, System.SysUtils, PLUGIN_Types, JSON,
	MRC_helper, IdCookieManager, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL,
	IdSSLOpenSSL, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
	IdHTTP, IdAuthentication, IdIOHandlerStream, IdMultipartFormData;

const
	TYPE_DIR = 'folder';
	TYPE_FILE = 'file';
	{ Константы для обозначения ошибок, возвращаемых при парсинге ответов облака. Дополняем по мере обнаружения }
	CLOUD_ERROR_UNKNOWN = -2; // unknown: 'Ошибка на сервере'
	CLOUD_OPERATION_ERROR_STATUS_UNKNOWN = -1;
	CLOUD_OPERATION_OK = 0;
	CLOUD_OPERATION_FAILED = 1;
	CLOUD_OPERATION_CANCELLED = 5;

	CLOUD_ERROR_EXISTS = 1; // exists: 'Папка с таким названием уже существует. Попробуйте другое название'
	CLOUD_ERROR_REQUIRED = 2; // required: 'Название папки не может быть пустым'
	CLOUD_ERROR_INVALID = 3; // invalid: '&laquo;' + app.escapeHTML(name) + '&raquo; это неправильное название папки. В названии папок нельзя использовать символы «" * / : < > ?  \\ |»'
	CLOUD_ERROR_READONLY = 4; // readonly|read_only: 'Невозможно создать. Доступ только для просмотра'
	CLOUD_ERROR_NAME_LENGTH_EXCEEDED = 5; // name_length_exceeded: 'Ошибка: Превышена длина имени папки. <a href="https://help.mail.ru/cloud_web/confines" target="_blank">Подробнее…</a>'
	CLOUD_ERROR_OVERQUOTA = 7; // overquota: 'Невозможно скопировать, в вашем Облаке недостаточно места'
	CLOUD_ERROR_QUOTA_EXCEEDED = 7; // "quota_exceeded": 'Невозможно скопировать, в вашем Облаке недостаточно места'

	{ Режимы работы при конфликтах копирования }
	CLOUD_CONFLICT_STRICT = 'strict'; // возвращаем ошибку при существовании файла
	CLOUD_CONFLICT_IGNORE = 'ignore'; // В API, видимо, не реализовано
	CLOUD_CONFLICT_RENAME = 'rename'; // Переименуем новый файл
	// CLOUD_CONFLICT_REPLACE = 'overwrite'; // хз, этот ключ не вскрыт

	CLOUD_MAX_FILESIZE = $80000000; // 2Gb
	CLOUD_MAX_NAME_LENGTH = 255;
	CLOUD_PUBLISH = true;
	CLOUD_UNPUBLISH = false;

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
		mtime: integer;
		hash: WideString;
		virus_scan: WideString;
		folders_count: integer;
		files_count: integer;
	End;

	TCloudMailRuSpaceInfo = record
		overquota: Boolean;
		total: int64;
		used: int64 End;

		TCloudMailRuDirListing = array of TCloudMailRuDirListingItem;

		TCloudMailRu = class private domain: WideString;
		user: WideString;
		password: WideString;
		token: WideString;
		x_page_id: WideString;
		build: WideString;
		upload_url: WideString;
		Cookie: TIdCookieManager;
		ExternalProgressProc: TProgressProcW;
		ExternalLogProc: TLogProcW;

		Shard: WideString;

		function getToken(): Boolean;
		function getShard(var Shard: WideString): Boolean;
		function putFileToCloud(localPath: WideString; Return: TStringList): integer;
		function addFileToCloud(hash: WideString; size: integer; remotePath: WideString; var JSONAnswer: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): Boolean;
		function HTTPPost(URL: WideString; PostData: TStringStream; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'): Boolean; // Постинг данных с возможным получением ответа.

		function HTTPPostFile(URL: WideString; PostData: TIdMultipartFormDataStream; var Answer: WideString): integer; // Постинг файла и получение ответа
		function HTTPGetFile(URL: WideString; var FileStream: TFileStream): integer;
		function HTTPGet(URL: WideString; var Answer: WideString): Boolean;
		function getTokenFromText(Text: WideString): WideString;
		function get_x_page_id_FromText(Text: WideString): WideString;
		function get_build_FromText(Text: WideString): WideString;
		function get_upload_url_FromText(Text: WideString): WideString;
		function getDirListingFromJSON(JSON: WideString): TCloudMailRuDirListing;
		function getUserSpaceFromJSON(JSON: WideString): TCloudMailRuSpaceInfo;
		function getFileStatusFromJSON(JSON: WideString): TCloudMailRuDirListingItem;
		function getShardFromJSON(JSON: WideString): WideString;
		function getPublicLinkFromJSON(JSON: WideString): WideString;
		function getOperationResultFromJSON(JSON: WideString; var OperationStatus: integer): integer;
		procedure HttpProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
		procedure Log(MsgType: integer; LogString: WideString);
	protected
		procedure HTTPInit(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL; var Cookie: TIdCookieManager);
		procedure HTTPDestroy(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL);
	public
		ExternalPluginNr: integer;
		ExternalSourceName: PWideChar;
		ExternalTargetName: PWideChar;
		constructor Create(user, domain, password: WideString; ExternalProgressProc: TProgressProcW = nil; PluginNr: integer = -1; ExternalLogProc: TLogProcW = nil);
		destructor Destroy; override;
		function login(): Boolean;

		function getUserSpace(var SpaceInfo: TCloudMailRuSpaceInfo): Boolean;
		function getDir(path: WideString; var DirListing: TCloudMailRuDirListing): Boolean;
		function getFile(remotePath, localPath: WideString): integer;
		function putFile(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer;
		function deleteFile(path: WideString): Boolean;
		function createDir(path: WideString): Boolean;
		function removeDir(path: WideString): Boolean;
		function renameFile(OldName, NewName: WideString): integer; // смена имени без перемещения
		function moveFile(OldName, ToPath: WideString): integer; // перемещение по дереву каталогов
		function mvFile(OldName, NewName: WideString): integer; // объединяющая функция, определяет делать rename или move
		function publishFile(path: WideString; var PublicLink: WideString; publish: Boolean = CLOUD_PUBLISH): Boolean;
		function statusFile(path: WideString; var FileInfo: TCloudMailRuDirListingItem): Boolean;

	end;

implementation

{ TCloudMailRu }

{ CONSTRUCTOR/DESTRUCTOR }

constructor TCloudMailRu.Create(user, domain, password: WideString; ExternalProgressProc: TProgressProcW; PluginNr: integer; ExternalLogProc: TLogProcW);
begin
	try
		self.Cookie := TIdCookieManager.Create();

		self.user := user;
		self.password := password;
		self.domain := domain;
		self.ExternalProgressProc := ExternalProgressProc;
		self.ExternalLogProc := ExternalLogProc;

		self.ExternalPluginNr := PluginNr;
		self.ExternalSourceName := '';
		self.ExternalTargetName := '';
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Cloud initialization error ' + E.Message);
		end;

	end;

end;

destructor TCloudMailRu.Destroy;
begin
	if Assigned(self.Cookie) then self.Cookie.free;
end;

{ PRIVATE METHODS }

function TCloudMailRu.login(): Boolean;
var
	URL: WideString;
	PostData: TStringStream;
	PostAnswer: WideString; { Не используется }
begin
	Result := false;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации

	Log(MSGTYPE_DETAILS, 'Login to ' + self.user + '@' + self.domain);
	URL := 'http://auth.mail.ru/cgi-bin/auth?lang=ru_RU&from=authpopup';
	try
		PostData := TStringStream.Create('page=https://cloud.mail.ru/?from=promo&new_auth_form=1&Domain=' + self.domain + '&Login=' + self.user + '&Password=' + self.password + '&FailPage=', TEncoding.UTF8);
		Result := self.HTTPPost(URL, PostData, PostAnswer);

	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Cloud login error: ' + E.Message);
		end;
	end;
	PostData.free;
	if (Result) then
	begin
		Log(MSGTYPE_DETAILS, 'Requesting auth token for ' + self.user + '@' + self.domain);
		Result := self.getToken();
		if (Result) then
		begin
			Log(MSGTYPE_DETAILS, 'Connected to ' + self.user + '@' + self.domain);
		end else begin
			Log(MSGTYPE_IMPORTANTERROR, 'Error getting auth token for ' + self.user + '@' + self.domain);
			exit(false);
		end;
	end
	else Log(MSGTYPE_IMPORTANTERROR, 'Error login to ' + self.user + '@' + self.domain);
end;

function TCloudMailRu.getToken(): Boolean;
var
	URL: WideString;
	PostResult: Boolean;
	Answer: WideString;
begin
	URL := 'https://cloud.mail.ru/?from=promo&from=authpopup';
	Result := false;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	try
		PostResult := self.HTTPGet(URL, Answer);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Get token error ' + E.Message);
		end;

	end;
	if PostResult then
	begin
		Result := true;
		self.token := self.getTokenFromText(Answer);
		self.x_page_id := self.get_x_page_id_FromText(Answer);
		self.build := self.get_build_FromText(Answer);
		self.upload_url := self.get_upload_url_FromText(Answer);
		if (self.token = '') or (self.x_page_id = '') or (self.build = '') or (self.upload_url = '') then Result := false; // В полученной странице нет нужных данных
	end;
end;

function TCloudMailRu.getShard(var Shard: WideString): Boolean;
var
	URL: WideString;
	PostData: TStringStream;
	Answer: WideString;
	SuccessPost: Boolean;
begin
	Result := false;
	SuccessPost := false;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	URL := 'https://cloud.mail.ru/api/v2/dispatcher/';
	try
		PostData := TStringStream.Create('api=2&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&token=' + self.token + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id, TEncoding.UTF8);
		SuccessPost := self.HTTPPost(URL, PostData, Answer);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Get shard error ' + E.Message);
			PostData.free;
		end;
	end;
	if SuccessPost then
	begin
		Shard := self.getShardFromJSON(Answer);
		if Shard = '' then Result := false
		else Result := true;
	end;
	PostData.free;
end;

function TCloudMailRu.putFileToCloud(localPath: WideString; Return: TStringList): integer; { Заливка на сервер состоит из двух шагов: заливаем файл на сервер в putFileToCloud и добавляем его в облако addFileToCloud }
var
	URL, PostAnswer: WideString;
	PostData: TIdMultipartFormDataStream;
begin
	Result := CLOUD_OPERATION_FAILED;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	URL := self.upload_url + '/?cloud_domain=1&x-email=' + self.user + '%40' + self.domain + '&fileapi' + IntToStr(DateTimeToUnix(now)) + '0246';
	// Log( MSGTYPE_DETAILS, 'Uploading to ' + URL);
	try
		PostData := TIdMultipartFormDataStream.Create;
		PostData.AddFile('file', localPath, 'application/octet-stream');
		Result := self.HTTPPostFile(URL, PostData, PostAnswer);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Posting file error ' + E.Message);
		end;
	end;
	PostData.free;
	if (Result = CLOUD_OPERATION_OK) then
	begin
		ExtractStrings([';'], [], PWideChar(PostAnswer), Return);
		if Length(Return.Strings[0]) <> 40 then
		begin
			Result := CLOUD_OPERATION_FAILED;
		end
	end;
end;

function TCloudMailRu.addFileToCloud(hash: WideString; size: integer; remotePath: WideString; var JSONAnswer: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): Boolean;
var
	URL: WideString;
	PostData: TStringStream;
begin
	Result := false;
	URL := 'https://cloud.mail.ru/api/v2/file/add';
	try
		PostData := TStringStream.Create('conflict=' + ConflictMode + '&home=/' + remotePath + '&hash=' + hash + '&size=' + IntToStr(size) + '&token=' + self.token + '&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id + '&conflict', TEncoding.UTF8);
		{ Экспериментально выяснено, что параметры api, build, email, x-email, x-page-id в запросе не обязательны }
		Result := self.HTTPPost(URL, PostData, JSONAnswer);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Adding file error ' + E.Message);
			PostData.free;
		end;
	end;
	PostData.free;
end;

function TCloudMailRu.HTTPPost(URL: WideString; PostData: TStringStream; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'): Boolean;
var
	MemStream: TStringStream;
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
begin
	Result := true;
	try
		MemStream := TStringStream.Create;
		self.HTTPInit(HTTP, SSL, self.Cookie);
		if ContentType <> '' then HTTP.Request.ContentType := ContentType;
		HTTP.Post(URL, PostData, MemStream);
		self.HTTPDestroy(HTTP, SSL);
		Answer := MemStream.DataString;
	except
		on E: EAbort do
		begin
			exit(false);
		end;
		on E: EIdHTTPProtocolException do
		begin
			if HTTP.ResponseCode = 400 then
			begin { сервер вернёт 400, но нужно пропарсить результат для дальнейшего определения действий }
				Answer := E.ErrorMessage;
				Result := true;
			end else if HTTP.ResponseCode = 507 then // кончилось место
			begin
				Answer := E.ErrorMessage;
				Result := true;
			end else begin
				Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL + ', ответ сервера: ' + E.ErrorMessage);
				Result := false;
			end;
		end;
	end;
	MemStream.free;
end;

function TCloudMailRu.HTTPPostFile(URL: WideString; PostData: TIdMultipartFormDataStream; var Answer: WideString): integer;
var
	MemStream: TStringStream;
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
begin
	Result := CLOUD_OPERATION_OK;
	try
		self.HTTPInit(HTTP, SSL, self.Cookie);
		MemStream := TStringStream.Create;
		HTTP.OnWork := self.HttpProgress;
		HTTP.Post(URL, PostData, MemStream);
		Answer := MemStream.DataString;
		self.HTTPDestroy(HTTP, SSL);
	except
		on E: EAbort do
		begin
			Result := CLOUD_OPERATION_CANCELLED;
		end;
		on E: EIdHTTPProtocolException do
		begin
			Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL + ', ответ сервера: ' + E.ErrorMessage);
			Result := CLOUD_OPERATION_FAILED;
		end;
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при отправке данных на адрес ' + URL);
			Result := CLOUD_OPERATION_FAILED;
		end;
	end;
	MemStream.free
end;

function TCloudMailRu.HTTPGet(URL: WideString; var Answer: WideString): Boolean;
var
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
begin
	try
		self.HTTPInit(HTTP, SSL, self.Cookie);
		Answer := HTTP.Get(URL);
		self.HTTPDestroy(HTTP, SSL);
	Except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при запросе данных с адреса ' + URL);
			exit(false);
		end;
	end;
	Result := Answer <> '';
end;

function TCloudMailRu.HTTPGetFile(URL: WideString; var FileStream: TFileStream): integer;
var
	HTTP: TIdHTTP;
	SSL: TIdSSLIOHandlerSocketOpenSSL;
begin
	Result := FS_FILE_OK;
	try
		self.HTTPInit(HTTP, SSL, self.Cookie);
		HTTP.Request.ContentType := 'application/octet-stream';
		HTTP.Response.KeepAlive := true;
		HTTP.OnWork := self.HttpProgress;
		HTTP.Get(URL, FileStream);
		self.HTTPDestroy(HTTP, SSL);
	except
		on E: EAbort do
		begin
			Result := FS_FILE_USERABORT;
		end;
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, E.ClassName + ' ошибка с сообщением: ' + E.Message + ' при копировании файла с адреса ' + URL);
			Result := FS_FILE_READERROR;
		end;
	end;
end;

procedure TCloudMailRu.HTTPInit(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL; var Cookie: TIdCookieManager);
begin
	SSL := TIdSSLIOHandlerSocketOpenSSL.Create();
	HTTP := TIdHTTP.Create();
	HTTP.CookieManager := Cookie;
	HTTP.IOHandler := SSL;
	HTTP.AllowCookies := true;
	HTTP.HTTPOptions := [hoForceEncodeParams, hoNoParseMetaHTTPEquiv];
	HTTP.HandleRedirects := true;
	// HTTP.ConnectTimeout:=10;
	HTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.57 Safari/537.17';
end;

procedure TCloudMailRu.HTTPDestroy(var HTTP: TIdHTTP; var SSL: TIdSSLIOHandlerSocketOpenSSL);
begin
	HTTP.free;
	SSL.free;
end;

procedure TCloudMailRu.HttpProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
var
	HTTP: TIdHTTP;
	ContentLength: int64;
	Percent: integer;
begin
	HTTP := TIdHTTP(ASender);
	if AWorkMode = wmRead then ContentLength := HTTP.Response.ContentLength
	else ContentLength := HTTP.Request.ContentLength; // Считаем размер обработанных данных зависимости от того, скачивание это или загрузка
	if (Pos('chunked', LowerCase(HTTP.Response.TransferEncoding)) = 0) and (ContentLength > 0) then
	begin
		Percent := 100 * AWorkCount div ContentLength;
		if Assigned(ExternalProgressProc) then
		begin
			if ExternalProgressProc(self.ExternalPluginNr, self.ExternalSourceName, self.ExternalTargetName, Percent) = 1 then Abort;
		end;
	end;
end;

procedure TCloudMailRu.Log(MsgType: integer; LogString: WideString);
begin
	if Assigned(ExternalLogProc) then
	begin
		ExternalLogProc(ExternalPluginNr, MsgType, PWideChar(LogString));
	end;
end;

{ PUBLIC METHODS }

function TCloudMailRu.getUserSpace(var SpaceInfo: TCloudMailRuSpaceInfo): Boolean;
var
	URL: WideString;
	JSON: WideString;
begin
	Result := false;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	URL := 'https://cloud.mail.ru/api/v2/user/space?api=2&home=/&build=' + self.build + '&x-page-id=' + self.x_page_id + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&token=' + self.token + '&_=1433249148810';
	try
		Result := self.HTTPGet(URL, JSON);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'User space receiving error ' + E.Message);
		end;
	end;
	if not Result then exit(false);
	SpaceInfo := self.getUserSpaceFromJSON(JSON);
end;

function TCloudMailRu.deleteFile(path: WideString): Boolean;
var
	URL: WideString;
	PostData: TStringStream;
	PostAnswer: WideString; { Не используется }
begin
	Result := false;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	path := UrlEncode(StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	URL := 'https://cloud.mail.ru/api/v2/file/remove';
	try
		PostData := TStringStream.Create('api=2&home=/' + path + '&token=' + self.token + '&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id + '&conflict', TEncoding.UTF8);
		Result := self.HTTPPost(URL, PostData, PostAnswer);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Delete file error ' + E.Message);
			Result := false;
		end;
	end;
	PostData.free;
end;

function TCloudMailRu.getDir(path: WideString; var DirListing: TCloudMailRuDirListing): Boolean;
var
	URL: WideString;
	JSON: WideString;
begin
	Result := false;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	path := UrlEncode(StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	URL := 'https://cloud.mail.ru/api/v2/folder?sort={%22type%22%3A%22name%22%2C%22order%22%3A%22asc%22}&offset=0&limit=10000&home=' + path + '&api=2&build=' + self.build + '&x-page-id=' + self.x_page_id + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&token=' + self.token + '&_=1433249148810';
	try
		Result := self.HTTPGet(URL, JSON);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Directory list receiving error ' + E.Message);
		end;
	end;
	if not Result then exit(false);
	DirListing := self.getDirListingFromJSON(JSON);
end;

function TCloudMailRu.getFile(remotePath, localPath: WideString): integer; // 0 - ok, else error
var
	FileStream: TFileStream;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	if self.Shard = '' then
	begin
		Log(MSGTYPE_DETAILS, 'Current shard is undefined, trying to get one');
		if self.getShard(self.Shard) then
		begin
			Log(MSGTYPE_DETAILS, 'Current shard: ' + self.Shard);
		end else begin // А вот теперь это критическая ошибка, тут уже не получится копировать
			Log(MSGTYPE_IMPORTANTERROR, 'Sorry, downloading impossible');
			exit(FS_FILE_NOTSUPPORTED);
		end;
	end;

	Result := FS_FILE_OK;
	remotePath := UrlEncode(StringReplace(remotePath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));

	try
		FileStream := TFileStream.Create(localPath, fmCreate);
		Result := self.HTTPGetFile(self.Shard + remotePath, FileStream);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'File receiving error ' + E.Message);
		end;
	end;
	FileStream.free;
	if Result <> FS_FILE_OK then
	begin
		System.SysUtils.deleteFile(localPath);
	end;
end;

function TCloudMailRu.publishFile(path: WideString; var PublicLink: WideString; publish: Boolean = CLOUD_PUBLISH): Boolean;
var
	URL: WideString;
	PostData: TStringStream;
	PostAnswer: WideString;
	SucessPublish: Boolean;
	OperationStatus: integer;
begin
	Result := false;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	SucessPublish := false;
	path := UrlEncode(StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	if publish then URL := 'https://cloud.mail.ru/api/v2/file/publish'
	else URL := 'https://cloud.mail.ru/api/v2/file/unpublish';
	try
		if publish then
		begin
			PostData := TStringStream.Create('api=2&home=/' + path + '&token=' + self.token + '&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id + '&conflict', TEncoding.UTF8);
		end else begin
			PostData := TStringStream.Create('api=2&weblink=' + PublicLink + '&token=' + self.token + '&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id + '&conflict', TEncoding.UTF8);
		end;
		SucessPublish := self.HTTPPost(URL, PostData, PostAnswer);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'File publish error ' + E.Message);
		end;
	end;
	PostData.free;
	if SucessPublish then
	begin
		case self.getOperationResultFromJSON(PostAnswer, OperationStatus) of
			CLOUD_OPERATION_OK:
				begin
					if publish then
					begin
						PublicLink := self.getPublicLinkFromJSON(PostAnswer);
					end;
					Result := true;
				end;
			CLOUD_ERROR_EXISTS:
				begin
					Result := false;
				end;
			CLOUD_ERROR_REQUIRED:
				begin
					Result := false;
				end;
			CLOUD_ERROR_INVALID:
				begin
					Result := false;
				end;
			CLOUD_ERROR_READONLY:
				begin
					Result := false;
				end;
			CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
				begin
					Result := false;
				end;
			CLOUD_ERROR_UNKNOWN:
				begin
					Result := false;
				end;
		else
			begin
				Log(MSGTYPE_IMPORTANTERROR, 'Error publishing file: got ' + IntToStr(OperationStatus) + ' status');
				Result := false;
			end;
		end;
	end;

end;

function TCloudMailRu.putFile(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer;
var
	PutResult: TStringList;
	JSONAnswer, FileHash: WideString;
	FileSize, Code, OperationStatus: integer;
	OperationResult: integer;
begin
	if (SizeOfFile(localPath) > CLOUD_MAX_FILESIZE) then exit(FS_FILE_NOTSUPPORTED);
	FileSize := 0;
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	OperationResult := CLOUD_OPERATION_FAILED;
	try
		PutResult := TStringList.Create;
		OperationResult := self.putFileToCloud(localPath, PutResult);
	Except
		on E: Exception do
		begin
			if E.ClassName = 'EAbort' then
			begin
				Result := FS_FILE_USERABORT;
			end else begin
				Log(MSGTYPE_IMPORTANTERROR, 'Error uploading to cloud: ' + E.ClassName + ' ошибка с сообщением: ' + E.Message);
				Result := FS_FILE_WRITEERROR;
			end;
		end;
	end;
	if OperationResult = CLOUD_OPERATION_OK then
	begin
		FileHash := PutResult.Strings[0];
		Val(PutResult.Strings[1], FileSize, Code); // Тут ошибка маловероятна
	end else if OperationResult = CLOUD_OPERATION_CANCELLED then
	begin
		Result := FS_FILE_USERABORT;
	end;
	PutResult.free;

	if OperationResult = CLOUD_OPERATION_OK then
	begin
		// Log( MSGTYPE_DETAILS, 'putFileToCloud result: ' + PutResult.Text);
		if self.addFileToCloud(FileHash, FileSize, UrlEncode(StringReplace(remotePath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase])), JSONAnswer) then
		begin
			// Log( MSGTYPE_DETAILS, JSONAnswer);
			case self.getOperationResultFromJSON(JSONAnswer, OperationStatus) of
				CLOUD_OPERATION_OK:
					begin
						Result := FS_FILE_OK;
					end;
				CLOUD_ERROR_EXISTS:
					begin
						Result := FS_FILE_EXISTS;
					end;
				CLOUD_ERROR_REQUIRED:
					begin
						Result := FS_FILE_WRITEERROR;
					end;
				CLOUD_ERROR_INVALID:
					begin
						Result := FS_FILE_WRITEERROR;
					end;
				CLOUD_ERROR_READONLY:
					begin
						Result := FS_FILE_WRITEERROR;
					end;
				CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
					begin
						Result := FS_FILE_WRITEERROR;
					end;
				CLOUD_ERROR_OVERQUOTA:
					begin
						Log(MSGTYPE_IMPORTANTERROR, 'Insufficient Storage');
						Result := FS_FILE_WRITEERROR;
					end;
				CLOUD_ERROR_UNKNOWN:
					begin
						Result := FS_FILE_NOTSUPPORTED;
					end;
			else
				begin // что-то неизвестное
					Log(MSGTYPE_IMPORTANTERROR, 'Error uploading to cloud: got ' + IntToStr(OperationStatus) + ' status');
					Result := FS_FILE_WRITEERROR;
				end;
			end;
		end;
	end;
end;

function TCloudMailRu.createDir(path: WideString): Boolean;
var
	URL: WideString;
	PostData: TStringStream;
	PostAnswer: WideString;
	SucessCreate: Boolean;
	OperationStatus: integer;
begin
	Result := false;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	SucessCreate := false;
	path := UrlEncode(StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	URL := 'https://cloud.mail.ru/api/v2/folder/add';
	try
		PostData := TStringStream.Create('api=2&home=/' + path + '&token=' + self.token + '&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id + '&conflict', TEncoding.UTF8);
		SucessCreate := self.HTTPPost(URL, PostData, PostAnswer);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Directory creation error ' + E.Message);
		end;
	end;
	PostData.free;
	if SucessCreate then
	begin
		case self.getOperationResultFromJSON(PostAnswer, OperationStatus) of
			CLOUD_OPERATION_OK:
				begin
					Result := true;
				end;
			CLOUD_ERROR_EXISTS:
				begin
					Result := false;
				end;
			CLOUD_ERROR_REQUIRED:
				begin
					Result := false;
				end;
			CLOUD_ERROR_INVALID:
				begin
					Result := false;
				end;
			CLOUD_ERROR_READONLY:
				begin
					Result := false;
				end;
			CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
				begin
					Result := false;
				end;
			CLOUD_ERROR_UNKNOWN:
				begin
					Result := false;
				end;
		else
			begin
				Log(MSGTYPE_IMPORTANTERROR, 'Error creating directory: got ' + IntToStr(OperationStatus) + ' status');
				Result := false;
			end;
		end;
	end;
end;

function TCloudMailRu.removeDir(path: WideString): Boolean;
var
	URL: WideString;
	PostData: TStringStream;
	PostAnswer: WideString; { Не используется }
begin
	Result := false;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	path := UrlEncode(StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	URL := 'https://cloud.mail.ru/api/v2/file/remove';
	try
		PostData := TStringStream.Create('api=2&home=/' + path + '/&token=' + self.token + '&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id + '&conflict', TEncoding.UTF8);
		Result := self.HTTPPost(URL, PostData, PostAnswer); // API всегда отвечает true, даже если путь не существует
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Delete directory error ' + E.Message);
			Result := false;
		end;
	end;
	PostData.free;
end;

function TCloudMailRu.renameFile(OldName, NewName: WideString): integer;
var
	URL: WideString;
	PostData: TStringStream;
	PostAnswer: WideString;
	PostResult: Boolean;
	OperationStatus: integer;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	OldName := UrlEncode(StringReplace(OldName, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	NewName := UrlEncode(StringReplace(NewName, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	URL := 'https://cloud.mail.ru/api/v2/file/rename';
	PostResult := false;
	try
		PostData := TStringStream.Create('api=2&home=' + OldName + '&name=' + NewName + '&token=' + self.token + '&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id, TEncoding.UTF8);
		PostResult := self.HTTPPost(URL, PostData, PostAnswer);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Rename file error ' + E.Message);
		end;
	end;
	PostData.free;
	if PostResult then
	begin // Парсим ответ
		case self.getOperationResultFromJSON(PostAnswer, OperationStatus) of
			CLOUD_OPERATION_OK:
				begin
					Result := CLOUD_OPERATION_OK
				end;
			CLOUD_ERROR_EXISTS:
				begin
					Result := FS_FILE_EXISTS;
				end;
			CLOUD_ERROR_REQUIRED:
				begin
					Result := FS_FILE_WRITEERROR;
				end;
			CLOUD_ERROR_INVALID:
				begin
					Result := FS_FILE_WRITEERROR;
				end;
			CLOUD_ERROR_READONLY:
				begin
					Result := FS_FILE_WRITEERROR;
				end;
			CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
				begin
					Result := FS_FILE_WRITEERROR;
				end;
			CLOUD_ERROR_UNKNOWN:
				begin
					Result := FS_FILE_NOTSUPPORTED;
				end;
		else
			begin // что-то неизвестное
				Log(MSGTYPE_IMPORTANTERROR, 'Error file rename: got ' + IntToStr(OperationStatus) + ' status');
				Result := FS_FILE_WRITEERROR;
			end;
		end;
	end;
end;

function TCloudMailRu.statusFile(path: WideString; var FileInfo: TCloudMailRuDirListingItem): Boolean;
var
	URL: WideString;
	JSON: WideString;
begin
	Result := false;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	path := UrlEncode(StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	URL := 'https://cloud.mail.ru/api/v2/file?home=' + path + '&api=2&build=' + self.build + '&x-page-id=' + self.x_page_id + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&token=' + self.token + '&_=1433249148810';
	try
		Result := self.HTTPGet(URL, JSON);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'File status getting error ' + E.Message);
		end;
	end;
	if not Result then exit(false);
	FileInfo := getFileStatusFromJSON(JSON);
end;

function TCloudMailRu.moveFile(OldName, ToPath: WideString): integer;
var
	URL: WideString;
	PostData: TStringStream;
	PostAnswer: WideString;
	PostResult: Boolean;
	OperationStatus: integer;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then exit; // Проверка на вызов без инициализации
	OldName := UrlEncode(StringReplace(OldName, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	ToPath := UrlEncode(StringReplace(ToPath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	URL := 'https://cloud.mail.ru/api/v2/file/move';
	PostResult := false;
	try
		PostData := TStringStream.Create('api=2&home=' + OldName + '&folder=' + ToPath + '&token=' + self.token + '&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id + '&conflict', TEncoding.UTF8);
		PostResult := self.HTTPPost(URL, PostData, PostAnswer);
	except
		on E: Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Rename file error ' + E.Message);
		end;
	end;
	PostData.free;
	if PostResult then
	begin // Парсим ответ
		case self.getOperationResultFromJSON(PostAnswer, OperationStatus) of
			CLOUD_OPERATION_OK:
				begin
					Result := CLOUD_OPERATION_OK
				end;
			CLOUD_ERROR_EXISTS:
				begin
					Result := FS_FILE_EXISTS;
				end;
			CLOUD_ERROR_REQUIRED:
				begin
					Result := FS_FILE_WRITEERROR;
				end;
			CLOUD_ERROR_INVALID:
				begin
					Result := FS_FILE_WRITEERROR;
				end;
			CLOUD_ERROR_READONLY:
				begin
					Result := FS_FILE_WRITEERROR;
				end;
			CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
				begin
					Result := FS_FILE_WRITEERROR;
				end;
			CLOUD_ERROR_UNKNOWN:
				begin
					Result := FS_FILE_NOTSUPPORTED;
				end;
		else
			begin // что-то неизвестное
				Log(MSGTYPE_IMPORTANTERROR, 'Error file move: got ' + IntToStr(OperationStatus) + ' status');
				Result := FS_FILE_WRITEERROR;
			end;
		end;
	end;
end;

function TCloudMailRu.mvFile(OldName, NewName: WideString): integer;
begin
	if ExtractFilePath(OldName) = ExtractFilePath(NewName) then
	begin // один каталог
		Result := self.renameFile(OldName, ExtractFileName(NewName));
	end else begin
		Result := self.moveFile(OldName, ExtractFilePath(NewName));
	end;
end;

{ PRIVATE STATIC METHODS (kinda) }

function TCloudMailRu.getTokenFromText(Text: WideString): WideString;
var
	start: integer;
begin
	start := Pos(WideString('"csrf"'), Text);
	if start > 0 then
	begin
		getTokenFromText := Copy(Text, start + 8, 32);
	end else begin
		getTokenFromText := '';
	end;
end;

function TCloudMailRu.get_build_FromText(Text: WideString): WideString;
var
	start, finish: integer;
	temp: WideString;
begin
	start := Pos(WideString('"BUILD"'), Text);
	if start > 0 then
	begin
		temp := Copy(Text, start + 9, 100);
		finish := Pos(WideString('"'), temp);
		get_build_FromText := Copy(temp, 0, finish - 1);
	end else begin
		get_build_FromText := '';
	end;
end;

function TCloudMailRu.get_upload_url_FromText(Text: WideString): WideString;
var
	start, start1, start2, finish, Length: Cardinal;
	temp: WideString;
begin
	start := Pos(WideString('mail.ru/upload/"'), Text);
	if start > 0 then
	begin
		start1 := start - 50;
		finish := start + 15;
		Length := finish - start1;
		temp := Copy(Text, start1, Length);
		start2 := Pos(WideString('https://'), temp);
		get_upload_url_FromText := Copy(temp, start2, StrLen(PWideChar(temp)) - start2);
	end else begin
		get_upload_url_FromText := '';
	end;
end;

function TCloudMailRu.get_x_page_id_FromText(Text: WideString): WideString;
var
	start: integer;
begin
	start := Pos(WideString('"x-page-id"'), Text);
	if start > 0 then
	begin
		get_x_page_id_FromText := Copy(Text, start + 13, 10);
	end else begin
		get_x_page_id_FromText := '';
	end;
end;

function TCloudMailRu.getShardFromJSON(JSON: WideString): WideString;
begin
	Result := ((((TJSONObject.ParseJSONValue(JSON) as TJSONObject).values['body'] as TJSONObject).values['get'] as TJSONArray).Items[0] as TJSONObject).values['url'].Value;
end;

function TCloudMailRu.getUserSpaceFromJSON(JSON: WideString): TCloudMailRuSpaceInfo;
var
	Obj: TJSONObject;
begin
	Obj := (TJSONObject.ParseJSONValue(JSON) as TJSONObject).values['body'] as TJSONObject;
	with Result do
	begin
		if Assigned(Obj.values['overquota']) then overquota := Obj.values['overquota'].Value.ToBoolean;
		if Assigned(Obj.values['total']) then total := Obj.values['total'].Value.ToInt64;
		if Assigned(Obj.values['used']) then used := Obj.values['used'].Value.ToInt64;
	end;

end;

function TCloudMailRu.getDirListingFromJSON(JSON: WideString): TCloudMailRuDirListing;
var
	Obj: TJSONObject;
	J: integer;
	ResultItems: TCloudMailRuDirListing;
	A: TJSONArray;
begin
	A := ((TJSONObject.ParseJSONValue(JSON) as TJSONObject).values['body'] as TJSONObject).values['list'] as TJSONArray;
	SetLength(ResultItems, A.count);
	for J := 0 to A.count - 1 do
	begin
		Obj := A.Items[J] as TJSONObject;
		with ResultItems[J] do
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
	end;

	Result := ResultItems;
end;

function TCloudMailRu.getFileStatusFromJSON(JSON: WideString): TCloudMailRuDirListingItem;
var
	Obj: TJSONObject;
begin
	Obj := (TJSONObject.ParseJSONValue(JSON) as TJSONObject).values['body'] as TJSONObject;
	with Result do
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
end;

function TCloudMailRu.getOperationResultFromJSON(JSON: WideString; var OperationStatus: integer): integer;
var
	Obj: TJSONObject;
	Error: WideString;
begin
	try
		Obj := TJSONObject.ParseJSONValue(JSON) as TJSONObject;

		OperationStatus := Obj.values['status'].Value.ToInteger;
		if OperationStatus <> 200 then
		begin
			Error := ((Obj.values['body'] as TJSONObject).values['home'] as TJSONObject).values['error'].Value;
			if Error = 'exists' then exit(CLOUD_ERROR_EXISTS);
			if Error = 'required' then exit(CLOUD_ERROR_REQUIRED);
			if Error = 'readonly' then exit(CLOUD_ERROR_READONLY);
			if Error = 'read_only' then exit(CLOUD_ERROR_READONLY);
			if Error = 'name_length_exceeded' then exit(CLOUD_ERROR_NAME_LENGTH_EXCEEDED);
			if Error = 'unknown' then exit(CLOUD_ERROR_UNKNOWN);
			if Error = 'overquota' then exit(CLOUD_ERROR_OVERQUOTA);
			if Error = 'quota_exceeded' then exit(CLOUD_ERROR_OVERQUOTA);
			if Error = 'invalid' then exit(CLOUD_ERROR_INVALID);

			exit(CLOUD_ERROR_UNKNOWN); // Эту ошибку мы пока не встречали
		end;

	except
		on E: { EJSON } Exception do
		begin
			Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON);
			exit(CLOUD_ERROR_UNKNOWN);
		end;
	end;
	Result := CLOUD_OPERATION_OK;
end;

function TCloudMailRu.getPublicLinkFromJSON(JSON: WideString): WideString;
begin
	Result := (TJSONObject.ParseJSONValue(JSON) as TJSONObject).values['body'].Value;
end;

end.
