unit ExampleApp;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CloudMailRu, Vcl.StdCtrls,
	IdCookieManager, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL,
	IdSSLOpenSSL, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
	IdHTTP, IdAuthentication, IdIOHandlerStream;

type
	TForm1 = class(TForm)
		IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
		IdCookieManager1: TIdCookieManager;
		Memo1: TMemo;
		Button1: TButton;
		HTTP: TIdHTTP;
		Button2: TButton;
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
	html: Widestring;
	url: Widestring;
	ret: Widestring;
	PostData: TStringList;
	MemStream: TStream;
begin
	PostData := TStringList.Create;
	MemStream := TMemoryStream.Create;
	url := 'http://auth.mail.ru/cgi-bin/auth?lang=ru_RU&from=authpopup';
	HTTP.Request.Referer := url;
	// url := 'http://localhost/test.php';
	PostData.Values['page'] := 'https://cloud.mail.ru/?from=promo';
	PostData.Values['FailPage'] := '';
	PostData.Values['Domain'] := 'mail.ru';
	PostData.Values['Login'] := 'mds_free';
	PostData.Values['Password'] := 'd;jgedst,kbe;f';
	PostData.Values['new_auth_form'] := '1';

	HTTP.Post(url, PostData, MemStream);
	Memo1.Lines.Add(Format('Response Code: %d', [HTTP.ResponseCode]));
	Memo1.Lines.Add(Format('Response Text: %s', [HTTP.ResponseText]));

	url := 'https://cloud.mail.ru/?from=promo&from=authpopup';
	Memo1.Lines.Add(HTTP.Get(url));

end;

procedure TForm1.Button2Click(Sender: TObject);
var
	cloud: TCloudMailRu;
	loginResult: Boolean;
begin
	cloud := TCloudMailRu.Create;
	loginResult := cloud.login;
	cloud.getToken();
end;

end.
