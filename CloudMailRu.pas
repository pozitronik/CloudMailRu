unit CloudMailRu;

interface

uses
	System.Classes, System.SysUtils, XSuperJson, XSuperObject, PLUGIN_Types,
	MRC_helper,
	IdCookieManager, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL,
	IdSSLOpenSSL, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
	IdHTTP, IdAuthentication, IdIOHandlerStream, IdMultipartFormData;

const
	TYPE_DIR = 'folder';
	TYPE_FILE = 'file';
	{ Константы для обозначения ошибок, возвращаемых при парсинге ответов облака. Дополняем по мере обнаружения }
	CLOUD_ERROR_UNKNOWN = -2;
	CLOUD_OPERATION_ERROR_STATUS_UNKNOWN = -1;
	CLOUD_OPERATION_OK = 0;
	CLOUD_ERROR_FILE_EXISTS = 1;

	{ Режимы работы при конфликтах копирования }
	CLOUD_CONFLICT_STRICT = 'strict'; // возвращаем ошибку при существовании файла
	CLOUD_CONFLICT_RENAME = 'rename'; // Переименуем новый файл
	// CLOUD_CONFLICT_REPLACE = 'overwrite'; // хз, этот ключ не вскрыт

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
	End;

	TCloudMailRuDirListing = array of TCloudMailRuDirListingItem;

	TCloudMailRu = class
	private
		domain: WideString;
		user: WideString;
		password: WideString;
		// dir: WideString;
		token: WideString;
		x_page_id: WideString;
		build: WideString;
		upload_url: WideString;
		HTTP: TIdHTTP;
		Cookie: TIdCookieManager;
		SSL: TIdSSLIOHandlerSocketOpenSSL;
		ExternalProgressProc: TProgressProc;
		ExternalLogProc: TLogProc;

		Shard: WideString;

		function getToken(): boolean;
		function getShard(var Shard: WideString): boolean;
		function putFileToCloud(localPath: WideString; Return: TStringList): boolean;
		function addFileToCloud(hash: WideString; size: integer; remotePath: WideString; var JSONAnswer: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): boolean;
		function HTTPPost(URL: WideString; PostData: TStringStream; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'): boolean; // Постинг данных с возможным получением ответа.

		function HTTPPostFile(URL: WideString; PostData: TIdMultipartFormDataStream; var Answer: WideString): boolean; overload; // Постинг файла и получение ответа
		function HTTPGet(URL: WideString; var Answer: WideString): boolean;
		function getTokenFromText(Text: WideString): WideString;
		function get_x_page_id_FromText(Text: WideString): WideString;
		function get_build_FromText(Text: WideString): WideString;
		function get_upload_url_FromText(Text: WideString): WideString;
		function getDirListingFromJSON(JSON: WideString): TCloudMailRuDirListing;
		function getShardFromJSON(JSON: WideString): WideString;
		function getOperationResultFromJSON(JSON: WideString; var OperationStatus: integer): integer;
		function UrlEncode(URL: UTF8String): WideString;
		procedure HttpProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
	public
		CancelCopy: boolean;
		ExternalPluginNr: integer;
		ExternalSourceName: PWideChar;
		ExternalTargetName: PWideChar;
		constructor Create(user, domain, password: WideString; ExternalProgressProc: TProgressProc; PluginNr: integer; ExternalLogProc: TLogProc);
		destructor Destroy;
		function login(): boolean;

		function getDir(path: WideString; var DirListing: TCloudMailRuDirListing): boolean;
		function getFile(remotePath, localPath: WideString): integer;
		function putFile(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer;
		function deleteFile(path: WideString): boolean;
		function createDir(path: WideString): boolean;
		function removeDir(path: WideString): boolean;

	end;

implementation

{ TCloudMailRu }

{ CONSTRUCTOR/DESTRUCTOR }

constructor TCloudMailRu.Create(user, domain, password: WideString; ExternalProgressProc: TProgressProc; PluginNr: integer; ExternalLogProc: TLogProc);
begin
	self.SSL := TIdSSLIOHandlerSocketOpenSSL.Create();
	self.Cookie := TIdCookieManager.Create();
	self.HTTP := TIdHTTP.Create();
	self.HTTP.CookieManager := Cookie;
	self.HTTP.IOHandler := SSL;
	self.HTTP.AllowCookies := true;
	self.HTTP.HandleRedirects := true;
	// self.HTTP.ConnectTimeout:=10;
	self.HTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.57 Safari/537.17';

	self.user := user;
	self.password := password;
	self.domain := domain;
	self.ExternalProgressProc := ExternalProgressProc;
	self.ExternalLogProc := ExternalLogProc;

	self.ExternalPluginNr := PluginNr;
	self.ExternalSourceName := '';
	self.ExternalTargetName := '';
end;

destructor TCloudMailRu.Destroy;
begin
	self.HTTP.Destroy;
	self.SSL.Destroy;
	self.Cookie.Destroy;
end;

{ PRIVATE METHODS }

function TCloudMailRu.login(): boolean;
var
	URL: WideString;
	PostData: TStringStream;
	PostAnswer: WideString; { Не используется }
begin
	self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Login to ' + self.user + '@' + self.domain));
	URL := 'http://auth.mail.ru/cgi-bin/auth?lang=ru_RU&from=authpopup';
	PostData := TStringStream.Create('page=https://cloud.mail.ru/?from=promo&new_auth_form=1&Domain=' + self.domain + '&Login=' + self.user + '&Password=' + self.password + '&FailPage=', TEncoding.UTF8);
	result := self.HTTPPost(URL, PostData, PostAnswer);
	PostData.Destroy;
	if (result) then
	begin
		self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Requesting auth token for ' + self.user + '@' + self.domain));
		result := self.getToken();
		if (result) then
		begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Connected to ' + self.user + '@' + self.domain));
		end else begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar('Error getting auth token for ' + self.user + '@' + self.domain));
			exit(false);
		end;
		self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Requesting download shard for current session'));
		if self.getShard(self.Shard) then
		begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Current shard: ' + self.Shard));
		end else begin
			// Это не критическая ошибка, попробуем получить шард прямо в процессе копирования
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Current shard is undefined, downloading can be unsupported'));
		end;
	end
	else self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar('Error login to ' + self.user + '@' + self.domain));
end;

function TCloudMailRu.getToken(): boolean;
var
	URL: WideString;
	PostResult: boolean;
	Answer: WideString;
begin
	URL := 'https://cloud.mail.ru/?from=promo&from=authpopup';
	getToken := true;
	PostResult := self.HTTPGet(URL, Answer);
	if PostResult then
	begin
		self.token := self.getTokenFromText(Answer);
		self.x_page_id := self.get_x_page_id_FromText(Answer);
		self.build := self.get_build_FromText(Answer);
		self.upload_url := self.get_upload_url_FromText(Answer);
		if (self.token = '') or (self.x_page_id = '') or (self.build = '') or (self.upload_url = '') then getToken := false; // В полученной странице нет нужных данных
	end else begin
		getToken := false;
	end;
end;

function TCloudMailRu.getShard(var Shard: WideString): boolean;
var
	URL: WideString;
	PostData: TStringStream;
	Answer: WideString;
begin
	URL := 'https://cloud.mail.ru/api/v2/dispatcher/';
	PostData := TStringStream.Create('api=2&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&token=' + self.token + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id, TEncoding.UTF8);
	if self.HTTPPost(URL, PostData, Answer) then
	begin
		Shard := self.getShardFromJSON(Answer);
		if Shard = '' then result := false
		else result := true;
	end;
	PostData.Destroy;
end;

function TCloudMailRu.putFileToCloud(localPath: WideString; Return: TStringList): boolean; { Заливка на сервер состоит из двух шагов: заливаем файл на сервер в putFileToCloud и добавляем его в облако addFileToCloud }
var
	URL, PostAnswer: WideString;
	PostData: TIdMultipartFormDataStream;
begin
	URL := self.upload_url + '/?cloud_domain=1&x-email=' + self.user + '%40' + self.domain + '&fileapi' + IntToStr(DateTimeToUnix(now)) + '0246';
	PostData := TIdMultipartFormDataStream.Create;
	self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Uploading to ' + URL));
	PostData.AddFile('file', localPath, 'application/octet-stream');
	result := self.HTTPPostFile(URL, PostData, PostAnswer);
	PostData.Destroy;
	if (result) then
	begin
		ExtractStrings([';'], [], PWideChar(PostAnswer), Return);
		if Length(Return.Strings[0]) = 40 then
		begin
			exit(true);
		end
		else exit(false);
	end;
end;

function TCloudMailRu.addFileToCloud(hash: WideString; size: integer; remotePath: WideString; var JSONAnswer: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): boolean;
var
	URL: WideString;
	PostData: TStringStream;
begin
	URL := 'https://cloud.mail.ru/api/v2/file/add';
	PostData := TStringStream.Create('conflict=' + ConflictMode + '&home=/' + remotePath + '&hash=' + hash + '&size=' + IntToStr(size) + '&token=' + self.token + '&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id + '&conflict', TEncoding.UTF8);
	{ Экспериментально выяснено, что параметры api, build, email, x-email, x-page-id в запросе не обязательны }
	result := self.HTTPPost(URL, PostData, JSONAnswer);
	PostData.Destroy;
end;

function TCloudMailRu.HTTPPost(URL: WideString; PostData: TStringStream; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'): boolean;
var
	MemStream: TStringStream;
begin
	result := true;
	MemStream := TStringStream.Create;
	try
		if ContentType <> '' then self.HTTP.Request.ContentType := ContentType;
		self.HTTP.Post(URL, PostData, MemStream); { TODO : При отсутствии сети TIdHTTP вываливается без всяких там }
		Answer := MemStream.DataString;
	except
		on E: EAbort do
		begin
			exit(false);
		end;
		on E: EIdHTTPProtocolException do
		begin
			if self.HTTP.ResponseCode = 400 then
			begin { сервер вернёт 400, но нужно пропарсить результат для дальнейшего определения действий }
				Answer := E.ErrorMessage;
				result := true;
			end else begin
				self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar(E.ClassName + ' ошибка с сообщением : ' + E.Message + ' при отправке данных на адрес ' + URL + ', ответ сервера: ' + E.ErrorMessage));
				result := false;
			end;
		end;
	end;
	MemStream.Free;
end;

function TCloudMailRu.HTTPPostFile(URL: WideString; PostData: TIdMultipartFormDataStream; var Answer: WideString): boolean;
var
	MemStream: TStringStream;
begin
	result := true;
	MemStream := TStringStream.Create;
	try
		self.HTTP.OnWork := self.HttpProgress;
		self.HTTP.Post(URL, PostData, MemStream);
		Answer := MemStream.DataString;
	except
		on E: EIdHTTPProtocolException do
		begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar(E.ClassName + ' ошибка с сообщением : ' + E.Message + ' при отправке данных на адрес ' + URL + ', ответ сервера: ' + E.ErrorMessage));
			result := false;
		end;
		on E: Exception do
		begin
			exit(false);
		end;
	end;
	MemStream.Free;
end;

function TCloudMailRu.HTTPGet(URL: WideString; var Answer: WideString): boolean;
begin
	result := true;
	try
		Answer := self.HTTP.Get(URL);
	Except
		exit(false);
	end;
	result := Answer <> '';
end;

procedure TCloudMailRu.HttpProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
var
	HTTP: TIdHTTP;
	ContentLength: int64;
	Percent: integer;
begin
	if self.CancelCopy then Abort;
	HTTP := TIdHTTP(ASender);
	if AWorkMode = wmRead then ContentLength := HTTP.Response.ContentLength
	else ContentLength := HTTP.Request.ContentLength; // Считаем размер обработанных данных зависимости от того, скачивание это или загрузка
	if (Pos('chunked', LowerCase(HTTP.Response.TransferEncoding)) = 0) and (ContentLength > 0) then
	begin
		Percent := 100 * AWorkCount div ContentLength;
		if self.ExternalProgressProc(self.ExternalPluginNr, self.ExternalSourceName, self.ExternalTargetName, Percent) = 1 then
		begin
			self.CancelCopy := true;
			Abort;
		end;
	end;
end;

{ PUBLIC METHODS }

function TCloudMailRu.deleteFile(path: WideString): boolean;
var
	URL: WideString;
	PostData: TStringStream;
	PostAnswer: WideString; { Не используется }
begin
	path := self.UrlEncode(StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	URL := 'https://cloud.mail.ru/api/v2/file/remove';
	PostData := TStringStream.Create('api=2&home=/' + path + '&token=' + self.token + '&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id + '&conflict', TEncoding.UTF8);
	result := self.HTTPPost(URL, PostData, PostAnswer);
	PostData.Destroy;
end;

function TCloudMailRu.getDir(path: WideString; var DirListing: TCloudMailRuDirListing): boolean;
var
	URL: WideString;
	JSON: WideString;

begin
	path := self.UrlEncode(StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	URL := 'https://cloud.mail.ru/api/v2/folder?sort={%22type%22%3A%22name%22%2C%22order%22%3A%22asc%22}&offset=0&limit=10000&home=' + path + '&api=2&build=' + self.build + '&x-page-id=' + self.x_page_id + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&token=' + self.token + '&_=1433249148810';
	result := self.HTTPGet(URL, JSON);
	if not result then exit(false);
	DirListing := self.getDirListingFromJSON(JSON);
end;

function TCloudMailRu.getFile(remotePath, localPath: WideString): integer; // 0 - ok, else error
var
	FileStream: TMemoryStream;
begin
	if self.Shard = '' then
	begin
		self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Current shard is undefined, trying to get one'));
		if self.getShard(self.Shard) then
		begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Current shard: ' + self.Shard));
		end else begin
			// А вот теперь это критическая ошибка, тут уже не получится копировать
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar('Sorry, downloading impossible'));
			exit(FS_FILE_NOTSUPPORTED);
		end;
	end;

	if self.CancelCopy then exit(FS_FILE_USERABORT);

	result := FS_FILE_OK;
	FileStream := TMemoryStream.Create;

	remotePath := self.UrlEncode(StringReplace(remotePath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	self.HTTP.OnWork := self.HttpProgress;
	try
		self.HTTP.Get(self.Shard + remotePath, FileStream);
	except
		on E: Exception do
		begin
			if E.ClassName = 'EAbort' then
			begin
				exit(FS_FILE_USERABORT);
			end else begin
				self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar(E.ClassName + ' ошибка с сообщением : ' + E.Message));
				exit(FS_FILE_READERROR);
			end;

		end;
	end;
	FileStream.SaveToFile(localPath);
end;

function TCloudMailRu.putFile(localPath, remotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): integer;
var
	PutResult: TStringList;
	JSONAnswer, FileHash: WideString;
	FileSize, Code, OperationStatus: integer;
	successPut: boolean;
begin
	if self.CancelCopy then exit(FS_FILE_USERABORT);
	result := FS_FILE_WRITEERROR;

	try
		PutResult := TStringList.Create;
		successPut := self.putFileToCloud(localPath, PutResult);
		FileHash := PutResult.Strings[0];
		Val(PutResult.Strings[1], FileSize, Code); // Тут ошибка маловероятна
		PutResult.Destroy;
	Except
		on E: Exception do
		begin
			if E.ClassName = 'EAbort' then
			begin
				result := FS_FILE_USERABORT;
			end else begin
				self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar('Error uploading to cloud: ' + E.ClassName + ' ошибка с сообщением : ' + E.Message));
			end;
		end;
	end;

	if successPut then
	begin
		self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('putFileToCloud result: ' + PutResult.Text));
		if self.addFileToCloud(FileHash, FileSize, self.UrlEncode(StringReplace(remotePath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase])), JSONAnswer) then
		begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar(JSONAnswer));
			case self.getOperationResultFromJSON(JSONAnswer, OperationStatus) of
				CLOUD_OPERATION_OK:
					begin
						result := FS_FILE_OK;
					end;
				CLOUD_ERROR_FILE_EXISTS:
					begin
						result := FS_FILE_EXISTS;
					end;
			else
				begin
					self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar('Error uploading to cloud: got ' + IntToStr(OperationStatus) + ' status'));
					result := FS_FILE_WRITEERROR;
				end;
			end;
		end;
	end;
end;

function TCloudMailRu.createDir(path: WideString): boolean;
var
	URL: WideString;
	PostData: TStringStream;
	PostAnswer: WideString;
	SucessCreate: boolean;
	OperationStatus: integer;
begin
	path := self.UrlEncode(StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	URL := 'https://cloud.mail.ru/api/v2/folder/add';
	PostData := TStringStream.Create('api=2&home=/' + path + '&token=' + self.token + '&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id + '&conflict', TEncoding.UTF8);
	SucessCreate := self.HTTPPost(URL, PostData, PostAnswer);
	PostData.Destroy;
	if SucessCreate then
	begin
		case self.getOperationResultFromJSON(PostAnswer, OperationStatus) of
			CLOUD_OPERATION_OK:
				begin
					result := true;
				end;
			CLOUD_ERROR_FILE_EXISTS:
				begin
					result := false;
				end;
		else
			begin
				self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar('Error creating directory: got ' + IntToStr(OperationStatus) + ' status'));
				result := false;
			end;
		end;
	end;
end;

function TCloudMailRu.removeDir(path: WideString): boolean;
var
	URL: WideString;
	PostData: TStringStream;
	PostAnswer: WideString; { Не используется }
begin
	path := self.UrlEncode(StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	URL := 'https://cloud.mail.ru/api/v2/file/remove';
	PostData := TStringStream.Create('api=2&home=/' + path + '/&token=' + self.token + '&build=' + self.build + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&x-page-id=' + self.x_page_id + '&conflict', TEncoding.UTF8);
	result := self.HTTPPost(URL, PostData, PostAnswer); // API всегда отвечает true, даже если путь не существует
	PostData.Destroy;
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
	start, start1, start2, finish, Length: integer;
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
var
	X: ISuperObject;
begin
	X := TSuperObject.Create(JSON);
	X := X['body'].AsObject;
	result := X.A['get'].O[0].S['url'];
end;

function TCloudMailRu.getDirListingFromJSON(JSON: WideString): TCloudMailRuDirListing;
var
	X, Obj: ISuperObject; // Это интерфейсы, им дестрой не нужен
	J: integer;
	ResultItems: TCloudMailRuDirListing;
begin
	X := TSuperObject.Create(JSON);
	X := X['body'].AsObject;
	SetLength(ResultItems, X.A['list'].Length);
	if (X.A['list'].Length = 0) then
	begin
		exit(ResultItems);
	end;

	with X.A['list'] do
		for J := 0 to X.A['list'].Length - 1 do
		begin
			Obj := O[J];
			With ResultItems[J] do
			begin
				tree := Obj.S['tree'];
				grev := Obj.I['grev'];
				size := Obj.I['size'];
				kind := Obj.S['kind'];
				weblink := Obj.S['weblink'];
				rev := Obj.I['rev'];
				type_ := Obj.S['type'];
				home := Obj.S['home'];
				name := Obj.S['name'];
				if (type_ = TYPE_FILE) then
				begin
					mtime := Obj.I['mtime'];
					virus_scan := Obj.S['virus_scan'];
					hash := Obj.S['hash'];
				end else begin
					mtime := 0;
				end;
			end;
		end;
	result := ResultItems;
end;

function TCloudMailRu.getOperationResultFromJSON(JSON: WideString; var OperationStatus: integer): integer;
var
	X: ISuperObject;
	Error: WideString;
	ErrCode: integer;
begin
	X := TSuperObject.Create(JSON).AsObject;
	OperationStatus := X.I['status'];
	if OperationStatus <> 200 then
	begin
		Error := X.O['body'].O['home'].S['error'];
		if Error = 'exists' then exit(CLOUD_ERROR_FILE_EXISTS)
		else exit(CLOUD_ERROR_UNKNOWN); // Эту ошибку мы пока не встречали
	end;
	result := CLOUD_OPERATION_OK;

end;

function TCloudMailRu.UrlEncode(URL: UTF8String): WideString; // todo нужно добиться корректного формирования урлов
var
	I: integer;
begin
	result := '';
	for I := 1 to Length(URL) do
		if URL[I] in ['a' .. 'z', 'A' .. 'Z', '/', '_', '-', '.'] then result := result + URL[I]
		else result := result + '%' + IntToHex(Ord(URL[I]), 2);
end;

end.
