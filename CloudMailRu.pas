unit CloudMailRu;

interface

uses
	System.Classes, System.SysUtils, XSuperJson, XSuperObject, PLUGIN_Types,
	IdCookieManager, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL,
	IdSSLOpenSSL, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
	IdHTTP, IdAuthentication, IdIOHandlerStream;

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
		dir: WideString;
		token: WideString;
		x_page_id: WideString;
		build: WideString;
		upload_url: WideString;
		HTTP: TIdHTTP;
		Cookie: TIdCookieManager;
		SSL: TIdSSLIOHandlerSocketOpenSSL;
		ExternalProgressProc: TProgressProc;

		function HTTPPost(URL: WideString; PostData: TStringList): boolean; overload;
		function HTTPPost(URL: WideString; PostData: TStringList; var Answer: WideString): boolean; overload;
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
		ExternalPluginNr: integer;
		ExternalSourceName: PWideChar;
		ExternalTargetName: PWideChar;
		constructor Create(user, domain, password: WideString; ExternalProgressProc: TProgressProc; PluginNr: integer);
		destructor Destroy;
		function login(): boolean;
		function getToken(var ParseData: WideString): boolean;
		function getShard(var Shard: WideString): boolean;
		function getDir(path: WideString; var DirListing: TCloudMailRuDirListing): boolean;
		function getFile(remotePath, localPath: WideString; ProgressProc: TProgressProc): boolean;
	end;

implementation

{ TCloudMailRu }

constructor TCloudMailRu.Create(user, domain, password: WideString; ExternalProgressProc: TProgressProc; PluginNr: integer);
begin
	self.SSL := TIdSSLIOHandlerSocketOpenSSL.Create();
	self.Cookie := TIdCookieManager.Create();
	self.HTTP := TIdHTTP.Create();
	self.HTTP.CookieManager := Cookie;
	self.HTTP.IOHandler := SSL;
	self.HTTP.AllowCookies := true;
	self.HTTP.HandleRedirects := true;
	self.HTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.57 Safari/537.17';

	self.user := user;
	self.password := password;
	self.domain := domain;
	self.ExternalProgressProc := ExternalProgressProc;
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
	// path := StringReplace(path, WideString('/'), WideString('%2F'), [rfReplaceAll, rfIgnoreCase]);
	URL := 'https://cloud.mail.ru/api/v2/folder?sort={%22type%22%3A%22name%22%2C%22order%22%3A%22asc%22}&offset=0&limit=10000&home=' + path + '&api=2&build=' +
		self.build + '&x-page-id=' + self.x_page_id + '&email=' + self.user + '%40' + self.domain + '&x-email=' + self.user + '%40' + self.domain + '&token=' +
		self.token + '&_=1433249148810';
	getDir := self.HTTPGet(URL, JSON);
	DirListing := self.getDirListingFromJSON(JSON);

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
		result := ResultItems;
		exit;
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

function TCloudMailRu.getFile(remotePath, localPath: WideString; ProgressProc: TProgressProc): boolean;
var
	FileStream: TMemoryStream;
	Shard: WideString; // todo make this global
begin
	FileStream := TMemoryStream.Create;
	self.getShard(Shard);
	remotePath := self.UrlEncode(StringReplace(remotePath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]));
	remotePath := Shard + remotePath;

	self.HTTP.OnWork := self.HttpProgress;
	self.HTTP.Get(remotePath, FileStream);
	FileStream.SaveToFile(localPath);
end;

function TCloudMailRu.getShardFromJSON(JSON: WideString): WideString;
var
	X: ISuperObject;
begin
	X := TSuperObject.Create(JSON);
	X := X['body'].AsObject;
	result := X.A['get'].O[0].S['url'];

end;

function TCloudMailRu.getShard(var Shard: WideString): boolean;
var
	URL: WideString;
	PostData: TStringList;
	Answer: WideString;
	JSON: TJSONObject;
begin
	URL := 'https://cloud.mail.ru/api/v2/dispatcher/';
	PostData := TStringList.Create;
	PostData.Values['api'] := '2';
	PostData.Values['build'] := self.build;
	PostData.Values['email'] := self.user + '%40' + self.domain;
	PostData.Values['token'] := self.token;
	PostData.Values['x-email'] := self.user + '%40' + self.domain;
	PostData.Values['x-page-id'] := self.x_page_id;
	self.HTTPPost(URL, PostData, Answer);
	Shard := self.getShardFromJSON(Answer);
end;

function TCloudMailRu.getToken(var ParseData: WideString): boolean;
var
	URL: WideString;
	PostData: TStringList;
	PostResult: boolean;
	Answer: WideString;
begin
	URL := 'https://cloud.mail.ru/?from=promo&from=authpopup';
	PostResult := self.HTTPGet(URL, Answer);
	ParseData := Answer;
	if PostResult then begin
		self.token := self.getTokenFromText(Answer);
		self.x_page_id := self.get_x_page_id_FromText(Answer);
		self.build := self.get_build_FromText(Answer);
		self.upload_url := self.get_upload_url_FromText(Answer);
		getToken := true;
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
	URL := 'http://auth.mail.ru/cgi-bin/auth?lang=ru_RU&from=authpopup';
	PostData := TStringList.Create;
	PostData.Values['page'] := 'https://cloud.mail.ru/?from=promo';
	PostData.Values['FailPage'] := '';
	PostData.Values['Domain'] := self.domain;
	PostData.Values['Login'] := self.user;
	PostData.Values['Password'] := self.password;
	PostData.Values['new_auth_form'] := '1';
	PostResult := self.HTTPPost(URL, PostData);
	PostData.Destroy;
	login := PostResult; // todo проверять успешность авторизации
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

function TCloudMailRu.HTTPPost(URL: WideString; PostData: TStringList): boolean;
var
	MemStream: TStream;
begin
	MemStream := TMemoryStream.Create;
	HTTP.Post(URL, PostData, MemStream);
	MemStream.Free;
end;

function TCloudMailRu.HTTPGet(URL: WideString; var Answer: WideString): boolean;
begin
	HTTPGet := true;
	try
		Answer := self.HTTP.Get(URL);
	Except
		HTTPGet := false;
	end;

	// todo: проверку на состояние ответа

end;

function TCloudMailRu.HTTPPost(URL: WideString; PostData: TStringList; var Answer: WideString): boolean;
var
	MemStream: TStringStream;
begin
	MemStream := TStringStream.Create;
	HTTP.Post(URL, PostData, MemStream);
	Answer := MemStream.DataString;
	MemStream.Free;
end;

procedure TCloudMailRu.HttpProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
var
	HTTP: TIdHTTP;
	ContentLength: int64;
	Percent: integer;
begin
	HTTP := TIdHTTP(ASender);
	ContentLength := HTTP.Response.ContentLength;

	if (Pos('chunked', LowerCase(HTTP.Response.TransferEncoding)) = 0) and (ContentLength > 0) then begin
		Percent := 100 * AWorkCount div ContentLength;
		self.ExternalProgressProc(self.ExternalPluginNr, self.ExternalSourceName, self.ExternalTargetName, Percent);
	end;
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
	start, start1, start2, finish, finish1, Length: integer;
	temp: WideString;
begin
	start := Pos(WideString('mail.ru/upload/"'), Text);
	if start > 0 then begin
		start1 := start - 50;
		finish1 := start + 15;
		Length := finish1 - start1;

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

end.
