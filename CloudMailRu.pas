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
		function addFileToCloud(putFileArray: TStringList; remotePath: WideString; var JSONAnswer: WideString): boolean;
		function HTTPPost(URL: WideString; PostData: TStringList): boolean; overload; // Постинг без ответа
		function HTTPPost(URL: WideString; PostData: TStringList; var Answer: WideString): boolean; overload; // Постинг и получение ответа
		function HTTPPost(URL: WideString; PostData: TStringStream; var Answer: WideString): boolean; overload; // Постинг строчкой и получение ответа
		function HTTPPost(URL: WideString; PostData: TIdMultipartFormDataStream; var Answer: WideString): boolean; overload; // Постинг файла и получение ответа
		function HTTPGet(URL: WideString; var Answer: WideString): boolean;
		function getTokenFromText(Text: WideString): WideString;
		function get_x_page_id_FromText(Text: WideString): WideString;
		function get_build_FromText(Text: WideString): WideString;
		function get_upload_url_FromText(Text: WideString): WideString;
		function getDirListingFromJSON(JSON: WideString): TCloudMailRuDirListing;
		function getShardFromJSON(JSON: WideString): WideString;
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
		function putFile(localPath, remotePath: WideString): integer;

	end;

implementation

{ TCloudMailRu }

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

function TCloudMailRu.getDir(path: WideString; var DirListing: TCloudMailRuDirListing): boolean;
var
	URL: WideString;
	JSON: WideString;

begin
	path := self.UrlEncode(StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	URL := 'https://cloud.mail.ru/api/v2/folder?sort={%22type%22%3A%22name%22%2C%22order%22%3A%22asc%22}&offset=0&limit=10000&home=' + path + '&api=2&build=' +
		self.build + '&x-page-id=' + self.x_page_id + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&token=' +
		self.token + '&_=1433249148810';
	result := self.HTTPGet(URL, JSON);
	if not result then exit(false);
	DirListing := self.getDirListingFromJSON(JSON);
end;

function TCloudMailRu.getFile(remotePath, localPath: WideString): integer; // 0 - ok, else error
var
	FileStream: TMemoryStream;
begin
	if self.Shard = '' then begin
		self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Current shard is undefined, trying to get one'));
		if self.getShard(self.Shard) then begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Current shard: ' + self.Shard));
		end
		else begin
			// А вот теперь это критическая ошибка, тут уже не получится копировать
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar('Sorry, downloading unsupported'));
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
		on E: Exception do begin
			if E.ClassName = 'EAbort' then begin
				exit(FS_FILE_USERABORT);
			end
			else begin
				self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar(E.ClassName + ' ошибка с сообщением : ' + E.Message));
				exit(FS_FILE_READERROR);
			end;

		end;
	end;
	FileStream.SaveToFile(localPath);
end;

procedure TCloudMailRu.HttpProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
var
	HTTP: TIdHTTP;
	ContentLength: int64;
	Percent: integer;
begin
	if self.CancelCopy then Abort;

	HTTP := TIdHTTP(ASender);
	ContentLength := HTTP.Response.ContentLength;
	if (Pos('chunked', LowerCase(HTTP.Response.TransferEncoding)) = 0) and (ContentLength > 0) then begin
		Percent := 100 * AWorkCount div ContentLength;
		if self.ExternalProgressProc(self.ExternalPluginNr, self.ExternalSourceName, self.ExternalTargetName, Percent) = 1 then begin
			self.CancelCopy := true;
			// HTTP.Disconnect;

			Abort;
		end;
	end;
end;

function TCloudMailRu.getShard(var Shard: WideString): boolean;
var
	URL: WideString;
	PostData: TStringList;
	Answer: WideString;
begin
	// todo error handling
	URL := 'https://cloud.mail.ru/api/v2/dispatcher/';
	PostData := TStringList.Create;
	PostData.Values['api'] := '2';
	PostData.Values['build'] := self.build;
	PostData.Values['email'] := self.user + '%40' + self.domain;
	PostData.Values['token'] := self.token;
	PostData.Values['x-email'] := self.user + '%40' + self.domain;
	PostData.Values['x-page-id'] := self.x_page_id;
	if not self.HTTPPost(URL, PostData, Answer) then exit(false);

	Shard := self.getShardFromJSON(Answer);
	if Shard = '' then result := false
	else result := true;
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
	if PostResult then begin
		self.token := self.getTokenFromText(Answer);
		self.x_page_id := self.get_x_page_id_FromText(Answer);
		self.build := self.get_build_FromText(Answer);
		self.upload_url := self.get_upload_url_FromText(Answer);
		if (self.token = '') or (self.x_page_id = '') or (self.build = '') or (self.upload_url = '') then getToken := false;
		// В полученной странице нет нужных данных

	end
	else begin
		getToken := false;
	end;

end;

function TCloudMailRu.login(): boolean;
var
	URL: WideString;
	PostData: TStringList;
	PostResult: boolean;
begin
	self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Login to ' + self.user + '@' + self.domain));
	URL := 'http://auth.mail.ru/cgi-bin/auth?lang=ru_RU&from=authpopup';
	PostData := TStringList.Create;
	PostData.Values['page'] := 'https://cloud.mail.ru/?from=promo';
	PostData.Values['FailPage'] := '';
	PostData.Values['Domain'] := self.domain;
	PostData.Values['Login'] := self.user;
	PostData.Values['Password'] := self.password;
	PostData.Values['new_auth_form'] := '1';
	PostResult := self.HTTPPost(URL, PostData); // todo проверять успешность авторизации
	PostData.Destroy;
	result := PostResult;
	if (PostResult) then begin
		self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Requesting auth token for ' + self.user + '@' + self.domain));
		result := self.getToken();
		if (result) then begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Connected to ' + self.user + '@' + self.domain));
		end
		else begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar('Error getting auth token for ' + self.user + '@' + self.domain));
		end;
		self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Requesting download shard for current session'));
		if self.getShard(self.Shard) then begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Current shard: ' + self.Shard));
		end
		else begin
			// Это не критическая ошибка, попробуем получить шард прямо в процессе копирования
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Current shard is undefined, downloading can be unsupported'));
		end;
	end
	else self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar('Error login to ' + self.user + '@' + self.domain));
end;

function TCloudMailRu.putFile(localPath, remotePath: WideString): integer;
var
	PutResult: TStringList;
	JSONAnswer: WideString;
begin
	PutResult := TStringList.Create;
	if self.putFileToCloud(localPath, PutResult) then begin
		self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('putFileToCloud result: ' + PutResult.Text));
		self.addFileToCloud(PutResult, ExtractFileDir(remotePath), JSONAnswer);
		self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar(JSONAnswer));
	end;
	PutResult.Destroy;
end;

function TCloudMailRu.putFileToCloud(localPath: WideString; Return: TStringList): boolean;
var
	URL, Answer: WideString;
	PostData: TIdMultipartFormDataStream;
begin
	URL := self.upload_url + '/?cloud_domain=1&x-email=' + self.user + '%40' + self.domain + '&fileapi' + IntToStr(DateTimeToUnix(now)) + '0246';
	PostData := TIdMultipartFormDataStream.Create;
	self.ExternalLogProc(ExternalPluginNr, MSGTYPE_DETAILS, PWideChar('Uploading to ' + URL));
	PostData.AddFile('file', localPath, 'application/octet-stream');
	result := self.HTTPPost(URL, PostData, Answer);
	PostData.Destroy;
	if (result) then begin
		ExtractStrings([';'], [], PWideChar(Answer), Return);
		if Length(Return.Strings[0]) = 40 then begin
			exit(true);
		end
		else exit(false);
	end;
end;

function TCloudMailRu.addFileToCloud(putFileArray: TStringList; remotePath: WideString; var JSONAnswer: WideString): boolean;
var
	URL: WideString;
	PostData: TStringStream;
begin
	URL := 'https://cloud.mail.ru/api/v2/file/add';
	PostData := TStringStream.Create('conflict=rename&home=test.txt&hash=C097B6E19A922A189B23657E3B2CE0CC7D8FE4FF&size=36766&token=' + self.token,
		TEncoding.UTF8);
	{
		PostData.Values['api'] := '2';
		PostData.Values['build'] := self.build;
		PostData.Values['conflict'] := 'rename';
		PostData.Values['email'] := self.user + '%40' + self.domain;
		PostData.Values['home'] := 'test.txt';
		PostData.Values['hash'] := putFileArray.Strings[0];
		PostData.Values['size'] := putFileArray.Strings[1];
		PostData.Values['token'] := self.token;
		PostData.Values['x-email'] := self.user + '%40' + self.domain;
		PostData.Values['x-page-id'] := self.x_page_id;
	}

	result := self.HTTPPost(URL, PostData, JSONAnswer);
	PostData.Destroy;
end;

function TCloudMailRu.HTTPPost(URL: WideString; PostData: TStringList): boolean;
var
	MemStream: TStream;
begin
	result := true;
	MemStream := TMemoryStream.Create;
	try
		self.HTTP.Post(URL, PostData, MemStream);
	except
		result := false;
	end;
	MemStream.Free;
end;

function TCloudMailRu.HTTPPost(URL: WideString; PostData: TStringList; var Answer: WideString): boolean;
var
	MemStream: TStringStream;
begin
	result := true;
	MemStream := TStringStream.Create;
	try
		// self.HTTP.Request.ContentType:='application/x-www-form-urlencoded';
		self.HTTP.Post(URL, PostData, MemStream);
		Answer := MemStream.DataString;
	except
		on E: Exception do begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR,
				PWideChar(E.ClassName + ' ошибка с сообщением : ' + E.Message + ' при отправке данных на адрес ' + URL + ', response: ' + self.HTTP.ResponseText));
			result := false;
		end;
	end;
	MemStream.Free;
end;

function TCloudMailRu.HTTPPost(URL: WideString; PostData: TStringStream; var Answer: WideString): boolean;
var
	MemStream: TStringStream;
begin
	result := true;
	MemStream := TStringStream.Create;
	try
		self.HTTP.Request.ContentType:='application/x-www-form-urlencoded';
		self.HTTP.Post(URL, PostData, MemStream);
		Answer := MemStream.DataString;
	except
		on E: Exception do begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR,
				PWideChar(E.ClassName + ' ошибка с сообщением : ' + E.Message + ' при отправке данных на адрес ' + URL + ', response: ' + self.HTTP.ResponseText));
			result := false;
		end;
	end;
	MemStream.Free;
end;

function TCloudMailRu.HTTPPost(URL: WideString; PostData: TIdMultipartFormDataStream; var Answer: WideString): boolean;
var
	MemStream: TStringStream;
begin
	result := true;
	MemStream := TStringStream.Create;
	try
		self.HTTP.Post(URL, PostData, MemStream);
		Answer := MemStream.DataString;
	except
		on E: Exception do begin
			self.ExternalLogProc(ExternalPluginNr, MSGTYPE_IMPORTANTERROR, PWideChar(E.ClassName + ' ошибка с сообщением : ' + E.Message));
			result := false;
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

function TCloudMailRu.getTokenFromText(Text: WideString): WideString;
var
	start: integer;
begin
	start := Pos(WideString('"csrf"'), Text);
	if start > 0 then begin
		getTokenFromText := Copy(Text, start + 8, 32);
	end
	else begin
		getTokenFromText := '';
	end;

end;

function TCloudMailRu.get_build_FromText(Text: WideString): WideString;
var
	start, finish: integer;
	temp: WideString;
begin
	start := Pos(WideString('"BUILD"'), Text);
	if start > 0 then begin
		temp := Copy(Text, start + 9, 100);
		finish := Pos(WideString('"'), temp);
		get_build_FromText := Copy(temp, 0, finish - 1);
	end
	else begin
		get_build_FromText := '';
	end;

end;

function TCloudMailRu.get_upload_url_FromText(Text: WideString): WideString;
var
	start, start1, start2, finish, Length: integer;
	temp: WideString;
begin
	start := Pos(WideString('mail.ru/upload/"'), Text);
	if start > 0 then begin
		start1 := start - 50;
		finish := start + 15;
		Length := finish - start1;

		temp := Copy(Text, start1, Length);
		start2 := Pos(WideString('https://'), temp);
		get_upload_url_FromText := Copy(temp, start2, StrLen(PWideChar(temp)) - start2);
	end
	else begin
		get_upload_url_FromText := '';
	end;
end;

function TCloudMailRu.get_x_page_id_FromText(Text: WideString): WideString;
var
	start: integer;
begin
	start := Pos(WideString('"x-page-id"'), Text);
	if start > 0 then begin
		get_x_page_id_FromText := Copy(Text, start + 13, 10);
	end
	else begin
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
	X, Obj: ISuperObject;
	J: integer;
	ResultItems: TCloudMailRuDirListing;
begin
	X := TSuperObject.Create(JSON);
	X := X['body'].AsObject;
	SetLength(ResultItems, X.A['list'].Length);
	if (X.A['list'].Length = 0) then begin
		exit(ResultItems);
	end;

	with X.A['list'] do
		for J := 0 to X.A['list'].Length - 1 do begin
			Obj := O[J];
			With ResultItems[J] do begin
				tree := Obj.S['tree'];
				grev := Obj.I['grev'];
				size := Obj.I['size'];
				kind := Obj.S['kind'];
				weblink := Obj.S['weblink'];
				rev := Obj.I['rev'];
				type_ := Obj.S['type'];
				home := Obj.S['home'];
				name := Obj.S['name'];
				if (type_ = TYPE_FILE) then begin
					mtime := Obj.I['mtime'];
					virus_scan := Obj.S['virus_scan'];
					hash := Obj.S['hash'];
				end
				else begin
					mtime := 0;
				end;
			end;
		end;
	result := ResultItems;
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
