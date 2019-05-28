unit Registration;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CloudMailRu, Settings, Vcl.StdCtrls, Vcl.ExtCtrls, Jpeg, CMLHTTP, CMLJSON, CMLTypes, PLUGIN_Types;

type
	TRegistrationForm = class(TForm)
		FirstNameLabel: TLabel;
		LastNameLabel: TLabel;
		FirstNameEdit: TEdit;
		LastNameEdit: TEdit;
		LoginLabel: TLabel;
		AtLabel: TLabel;
		LoginEdit: TEdit;
		DomainCombo: TComboBox;
		SignupBTN: TButton;
		UserAgreementLink: TLinkLabel;
		PasswordLabel: TLabel;
		PasswordEdit: TEdit;
		UseTCPwdMngrCB: TCheckBox;
		CaptchaImg: TImage;
		CaptchaLabel: TLabel;
		CaptchaEdit: TEdit;
		SendBtn: TButton;

		procedure SignupBTNClick(Sender: TObject);

	private
		{Private declarations}
	protected
		HTTPConnection: TCloudMailRuHTTP;
		JSONParser: TCloudMailRuJSONParser;
		procedure Init();
		procedure Free();
		function createAccount(firstname, lastname, login, password, domain: WideString; var Code: WideString): Boolean;
		function getRegisrationCaptcha(CaptchaStream: TStream): Boolean;
		function confirmRegistration(email, Code, captcha: WideString): Boolean;
	public
		{Public declarations}
		class function ShowRegistration(parentWindow: HWND; var UseTCPwdMngr: Boolean): integer;
	end;

implementation

{$R *.dfm}
{TRegistrationForm}

function TRegistrationForm.confirmRegistration(email, Code, captcha: WideString): Boolean;
var
	JSON, confirmationJSON: WideString;
begin
	confirmationJSON := '{"id":"' + Code + '","capcha":"' + captcha + '"}'; //todo
	result := HTTPConnection.PostForm('https://account.mail.ru/api/v1/user/signup/confirm', 'email=' + email + 'mail.ru&reg_anketa=' + confirmationJSON, JSON);
end;

function TRegistrationForm.createAccount(firstname, lastname, login, password, domain: WideString; var Code: WideString): Boolean;
var
	JSON: WideString;
	poststring: WideString;

begin
	HTTPConnection.HTTP.Request.UserAgent := 'curl/7.63.0'; //required by server
	HTTPConnection.HTTP.Request.Connection := EmptyWideStr;
	HTTPConnection.HTTP.Request.Accept := '*/*';

	HTTPConnection.HTTP.Request.Referer := 'https://account.mail.ru/api/v1/user/signup';

	poststring := 'name={"first":"' + firstname + '","last":"' + lastname + '"}&login=' + login + '&domain=' + domain + '&password=' + password;
	result := HTTPConnection.PostForm('https://account.mail.ru/api/v1/user/signup', poststring, JSON);
	if result then
	begin
		result := (CLOUD_OPERATION_OK = JSONParser.getRegistrationOperationResult(JSON).OperationResult);
		if result then
			result := JSONParser.getRegistrationBody(JSON, Code)
		else
			MessageBox(Handle, PWideChar(JSON), 'Registration error', MB_ICONERROR + MB_OK);
	end;
end;

procedure TRegistrationForm.Free;
begin
	HTTPConnection.Free;
	JSONParser.Free;
end;

function TRegistrationForm.getRegisrationCaptcha(CaptchaStream: TStream): Boolean;
begin
	result := FS_FILE_OK = HTTPConnection.getFile('https://c.mail.ru/c/6', CaptchaStream);
end;

procedure TRegistrationForm.Init;
var
	ConnectionSettings: TConnectionSettings; //anything goes
begin
	HTTPConnection := TCloudMailRuHTTP.Create(ConnectionSettings);
	JSONParser := TCloudMailRuJSONParser.Create();;
end;

class function TRegistrationForm.ShowRegistration(parentWindow: HWND; var UseTCPwdMngr: Boolean): integer;
var
	RegistrationForm: TRegistrationForm;

begin
	try
		RegistrationForm := TRegistrationForm.Create(nil);
		RegistrationForm.parentWindow := parentWindow;

		//RegistrationForm.UseTCPwdMngrCB.Checked := UseTCPwdMngr;

		result := RegistrationForm.ShowModal;

		if result = mrOk then
		begin
			//Password := AskPasswordForm.PasswordEdit.Text;
			//UseTCPwdMngr := AskPasswordForm.UseTCPwdMngrCB.Checked;
		end;
	finally
		FreeAndNil(RegistrationForm);
	end;
end;

procedure TRegistrationForm.SignupBTNClick(Sender: TObject);
var
	Code: WideString;
	MemStream: TMemoryStream;
	JPEGImage: TJPEGImage;
begin
	self.Init;

	if (createAccount(self.FirstNameEdit.Text, self.LastNameEdit.Text, self.LoginEdit.Text, self.PasswordEdit.Text, self.DomainCombo.Text, Code)) then
	begin
		MemStream := TMemoryStream.Create();
		getRegisrationCaptcha(MemStream);
		MemStream.Position := 0;
		JPEGImage := TJPEGImage.Create;
		JPEGImage.LoadFromStream(MemStream);
		CaptchaImg.Picture.Assign(JPEGImage);
		JPEGImage.Free;
		MemStream.Free;
	end;

	self.Free;
end;

end.
