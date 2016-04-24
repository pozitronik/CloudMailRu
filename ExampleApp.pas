unit ExampleApp;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
	System.Classes, Vcl.Graphics, DBXJSON, System.JSON, REST.JSON,
	Vcl.Controls, Vcl.Forms, XSuperJson, XSuperObject,
	Vcl.Dialogs, CloudMailRu, Vcl.StdCtrls,
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
		Button3: TButton;
		Memo2: TMemo;
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
		procedure Button3Click(Sender: TObject);
		function LoadJSONStringLists(S: WideString): WideString;
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
	html: WideString;
	url: WideString;
	ret: WideString;
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
	dirJSON, tmp, ShardData: WideString;
	Listing: TCloudMailRuDirListing;
	I: Integer;
begin
	cloud := TCloudMailRu.Create;
	loginResult := cloud.login;
	cloud.getToken(tmp);
	(*cloud.getDir('/', Listing);
	for I := 0 to length(Listing) - 1 do begin
		Memo1.Lines.Add(Listing[I].name + '|' + Listing[I].type_ + '|' +
			IntToStr(Listing[I].size) + '|' + Listing[I].home);
	end;*)
	 cloud.getShard(ShardData);
	 Memo1.Lines.Add(ShardData);
end;

procedure TForm1.Button3Click(Sender: TObject);
const
	fJSON = '{"email":"mds_free@mail.ru","body":{"count":{"folders":4,"files":1},'
		+ '"tree":"316234396237373230303030","name":"/","grev":4185,"size":721768655677,"sort":{"order":"asc","type":"name"},"kind":"folder","rev":4183,"type":"folder","home":"/","list":[{"count":{"folders":0,"files":2},'
		+ '"tree":"316234396237373230303030","name":"dir","grev":4185,"size":52,"kind":"folder","rev":4185,"type":"folder","home":"/dir"},{"count":{"folders":8,"files":3},'
		+ '"tree":"316234396237373230303030","name":"LIBRARY","grev":787,"size":279690172507,"kind":"folder","weblink":"af39052fc910/LIBRARY","rev":3,"type":"folder","home":"/LIBRARY"},{"count":{"folders":2,"files":0},'
		+ '"tree":"316234396237373230303030","name":"MDS","grev":4182,"size":167412628990,"kind":"folder","weblink":"ffb97e431d2a/MDS","rev":1107,"type":"folder","home":"/MDS"},{"count":{"folders":26,"files":2},'
		+ '"tree":"316234396237373230303030","name":"ÌÄÑ","grev":8,"size":274665817362,"kind":"folder","rev":3,"type":"folder","home":"/ÌÄÑ"},'
		+ '{"mtime":1460875792,"virus_scan":"pass","name":"test.txt","size":36766,"hash":"C097B6E19A922A189B23657E3B2CE0CC7D8FE4FF","kind":"file","type":"file","home":"/test.txt"}]},"time":1461478442441,"status":200}';

var
	t: TStrings;
begin
	t := TStrings.Create;

	Memo2.Lines.Text := LoadJSONStringLists(fJSON);
	t.Free;
end;

function TForm1.LoadJSONStringLists(S: WideString): WideString;
var
	X, Obj: ISuperObject;
	AMember, OMember: IMember;
	J, length: Integer;
begin
	X := TSuperObject.Create(S);
	X := X['body'].AsObject;
	// Length := X.Count;
	with X.A['list'] do
		for J := 0 to X.A['list'].length - 1 do begin
			Obj := O[J];
			Obj.First;
			while not Obj.EoF do begin
				Memo1.Lines.Add(Obj.CurrentKey + ' = ' +
					VarToStr(Obj.CurrentValue.AsVariant));
				Obj.Next;
			end;
			Memo1.Lines.Add('------');
		end;

end;

end.
