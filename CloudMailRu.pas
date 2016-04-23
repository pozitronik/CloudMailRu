unit CloudMailRu;

interface

uses
	System.Classes, System.SysUtils,
	IdCookieManager, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL,
	IdSSLOpenSSL, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
	IdHTTP, IdAuthentication, IdIOHandlerStream;

type
	TCloudMailRu = class

	private
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
		function HTTPPost(URL: WideString; PostData: TStringList): boolean;
		function HTTPGet(URL: WideString; var Answer: WideString): boolean;
		function getTokenFromText(Text: WideString): WideString;
		function get_x_page_id_FromText(Text: WideString): WideString;
		function get_build_FromText(Text: WideString): WideString;
		function get_upload_url_FromText(Text: WideString): WideString;
	public
		constructor Create;
		destructor Destroy;
		function login(): boolean;
		function getToken(): boolean;
		function getDir(path: WideString; var data: WideString): boolean;
	end;

implementation

{ TCloudMailRu }

constructor TCloudMailRu.Create;
begin
	self.SSL := TIdSSLIOHandlerSocketOpenSSL.Create();
	self.Cookie := TIdCookieManager.Create();
	self.HTTP := TIdHTTP.Create();
	self.HTTP.CookieManager := Cookie;
	self.HTTP.IOHandler := SSL;
	self.HTTP.AllowCookies := true;
	self.HTTP.HandleRedirects := true;
	self.HTTP.Request.UserAgent :=
		'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.57 Safari/537.17';

	self.user := 'mds_free';
	self.password := 'd;jgedst,kbe;f';
end;

destructor TCloudMailRu.Destroy;
begin

end;

function TCloudMailRu.getDir(path: WideString; var data: WideString): boolean;
begin

end;

function TCloudMailRu.getToken(): boolean;
var
	URL: WideString;
	PostData: TStringList;
	PostResult: boolean;
	Answer: WideString;
begin
	URL := 'https://cloud.mail.ru/?from=promo&from=authpopup';
	PostResult := self.HTTPGet(URL, Answer);
	if PostResult then begin
		self.token := self.getTokenFromText(Answer);
		self.x_page_id := self.get_x_page_id_FromText(Answer);
		self.build := self.get_build_FromText(Answer);
		self.upload_url := self.get_upload_url_FromText(Answer);
    getToken:=true;
	end
	else begin
		getToken := false;
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
		temp := Copy(Text, start + 10, 100);
		finish := Pos(WideString('"'), temp);
		get_build_FromText := Copy(temp, 0, finish - 1);
	end
	else begin
		get_build_FromText := '';
	end;

end;

function TCloudMailRu.get_upload_url_FromText(Text: WideString): WideString;
var
	start, start1, start2, finish, finish1, length: integer;
	temp: WideString;
begin
	start := Pos(WideString('mail.ru/upload/"'), Text);
	if start > 0 then begin
		start1 := start - 50;
		finish1 := start + 15;
		length := finish1 - start1;

		temp := Copy(Text, start1, length);
		start2 := Pos(WideString('https://'), temp);
		get_upload_url_FromText := Copy(temp, start2, StrLen(PWideChar(temp))
			- start2);
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
		get_x_page_id_FromText := Copy(Text, start + 14, 11);
	end
	else begin
		get_x_page_id_FromText := '';
	end;
end;

function TCloudMailRu.HTTPPost(URL: WideString; PostData: TStringList): boolean;
var
	MemStream: TStream;
begin
	MemStream := TMemoryStream.Create;
	HTTP.Post(URL, PostData, MemStream);
end;

function TCloudMailRu.HTTPGet(URL: WideString; var Answer: WideString): boolean;
begin
	Answer := self.HTTP.Get(URL);
	// todo: проверку на состояние ответа
	HTTPGet := true;

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
	PostData.Values['Domain'] := 'mail.ru';
	PostData.Values['Login'] := self.user;
	PostData.Values['Password'] := self.password;
	PostData.Values['new_auth_form'] := '1';
	PostResult := self.HTTPPost(URL, PostData);
	PostData.Destroy;
	login := PostResult;
end;

end.
