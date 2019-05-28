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
		procedure SendBtnClick(Sender: TObject);
		procedure FirstNameEditChange(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);

	private
		{Private declarations}
		Login, Domain, Code: WideString;
		function RegistrationValid: boolean;
	protected
		HTTPConnection: TCloudMailRuHTTP;
		JSONParser: TCloudMailRuJSONParser;
		procedure InitComponents();
		procedure FreeComponents();
		function createAccount(firstname, lastname, Login, password, Domain: WideString; var Code: WideString): boolean;
		function getRegisrationCaptcha(CaptchaStream: TStream): boolean;
		function confirmRegistration(email, Code, captcha: WideString): boolean;
	public
		property isRegistrationValid: boolean read RegistrationValid;
		{Public declarations}
		class function ShowRegistration(parentWindow: HWND; var UseTCPwdMngr: boolean): integer;
	end;

implementation

{$R *.dfm}
{TRegistrationForm}

function TRegistrationForm.confirmRegistration(email, Code, captcha: WideString): boolean;
var
	JSON, confirmationJSON: WideString;
begin
	confirmationJSON := '{"id":"' + Code + '","capcha":"' + captcha + '"}'; //capcha, lol
	result := HTTPConnection.PostForm('https://account.mail.ru/api/v1/user/signup/confirm', 'email=' + email + '&reg_anketa=' + confirmationJSON, JSON);
end;

function TRegistrationForm.createAccount(firstname, lastname, Login, password, Domain: WideString; var Code: WideString): boolean;
var
	JSON: WideString;
	poststring: WideString;

begin
	HTTPConnection.HTTP.Request.UserAgent := 'curl/7.63.0'; //required by server
	HTTPConnection.HTTP.Request.Connection := EmptyWideStr;
	HTTPConnection.HTTP.Request.Accept := '*/*';

	HTTPConnection.HTTP.Request.Referer := 'https://account.mail.ru/api/v1/user/signup';

	poststring := 'name={"first":"' + firstname + '","last":"' + lastname + '"}&login=' + Login + '&domain=' + Domain + '&password=' + password;
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

procedure TRegistrationForm.FirstNameEditChange(Sender: TObject);
begin
	SignupBTN.Enabled := isRegistrationValid;
end;

procedure TRegistrationForm.FormCreate(Sender: TObject);
begin
	self.InitComponents;
end;

procedure TRegistrationForm.FormDestroy(Sender: TObject);
begin
	self.FreeComponents;
end;

procedure TRegistrationForm.FreeComponents;
begin
	HTTPConnection.Free;
	JSONParser.Free;
end;

function TRegistrationForm.getRegisrationCaptcha(CaptchaStream: TStream): boolean;
begin
	result := FS_FILE_OK = HTTPConnection.getFile('https://c.mail.ru/c/6', CaptchaStream);
end;

procedure TRegistrationForm.InitComponents;
var
	ConnectionSettings: TConnectionSettings; //todo:load plugin settings
begin

	HTTPConnection := TCloudMailRuHTTP.Create(ConnectionSettings);
	JSONParser := TCloudMailRuJSONParser.Create();;
end;

function TRegistrationForm.RegistrationValid: boolean;
begin
	result := (FirstNameEdit.Text <> EmptyWideStr) and (LastNameEdit.Text <> EmptyWideStr) and (LoginEdit.Text <> EmptyWideStr) and (PasswordEdit.Text <> EmptyWideStr);
end;

procedure TRegistrationForm.SendBtnClick(Sender: TObject);
begin
	confirmRegistration(Login + '@' + Domain, Code, CaptchaEdit.Text)
end;

class function TRegistrationForm.ShowRegistration(parentWindow: HWND; var UseTCPwdMngr: boolean): integer;
var
	RegistrationForm: TRegistrationForm;

begin
	try
		RegistrationForm := TRegistrationForm.Create(nil);
		RegistrationForm.parentWindow := parentWindow;

		RegistrationForm.UseTCPwdMngrCB.Checked := UseTCPwdMngr;

		result := RegistrationForm.ShowModal;

		if result = mrOk then
		begin

			UseTCPwdMngr := AskPasswordForm.UseTCPwdMngrCB.Checked;
		end;
	finally
		FreeAndNil(RegistrationForm);
	end;
end;

procedure TRegistrationForm.SignupBTNClick(Sender: TObject);
var
	MemStream: TMemoryStream;
	JPEGImage: TJPEGImage;
begin
	CaptchaEdit.Enabled := false;
	SendBtn.Enabled := false;
	Login := self.LoginEdit.Text;
	Domain := self.DomainCombo.Text;
	self.Enabled := false;

	if (createAccount(self.FirstNameEdit.Text, self.LastNameEdit.Text, Login, self.PasswordEdit.Text, Domain, Code)) then
	begin
		MemStream := TMemoryStream.Create();
		if getRegisrationCaptcha(MemStream) then
		begin
			MemStream.Position := 0;
			JPEGImage := TJPEGImage.Create;
			JPEGImage.LoadFromStream(MemStream);
			CaptchaImg.Picture.Assign(JPEGImage);
			JPEGImage.Free;
			CaptchaEdit.Enabled := true;
			SendBtn.Enabled := true;
		end
		else
			MessageBox(Handle, 'Can''t load captcha image!', 'Registration error', MB_ICONERROR + MB_OK);
		MemStream.Free;
	end;
	self.Enabled := true;
end;

end.
