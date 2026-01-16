unit Registration;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	CloudMailRu,
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	JSONHelper,
	CloudMailRuHTTP,
	CMROperationResult,
	CMRConstants,
	LANGUAGE_STRINGS,
	SETTINGS_CONSTANTS,
	PLUGIN_TYPES,
	AccountSettings,
	ConnectionSettings,
	Vcl.Imaging.JPEG;

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
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure CaptchaEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

	private
		{Private declarations}
		AccountSettings: TAccountSettings;
		ConnectionSettings: TConnectionSettings;
		Code: WideString;
		function RegistrationValid: boolean;
	protected
		HTTPConnection: TCloudMailRuHTTP;
		//		JSONParser: TCloudMailRuJSONParser;
		procedure InitComponents();
		procedure FreeComponents();
		function createAccount(firstname, lastname, Login, password, Domain: WideString; var Code: WideString): boolean;
		function getRegisrationCaptcha(CaptchaStream: TStream): boolean;
		function confirmRegistration(Email, Code, captcha: WideString): boolean;
	public
		property isRegistrationValid: boolean read RegistrationValid;
		{Public declarations}
		class function ShowRegistration(parentWindow: HWND; ConnectionSettings: TConnectionSettings; AccountSettings: TAccountSettings): integer;
	end;

implementation

{$R *.dfm}
{TRegistrationForm}

procedure TRegistrationForm.CaptchaEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	case Key of
		VK_RETURN:
			if SendBtn.Enabled then
				SendBtn.Click;
	end;
end;

function TRegistrationForm.confirmRegistration(Email, Code, captcha: WideString): boolean;
var
	JSON: WideString;
begin
	result := HTTPConnection.PostForm(MAILRU_REGISTRATION_CONFIRM, Format('email=%s&reg_anketa={"id":"%s","capcha":"%s"}', [Email, Code, captcha]), JSON);
	if result then
	begin
		result := TCMROperationResult.GetRegistrationOperationResult(JSON).OperationResult = CLOUD_OPERATION_OK;
		if not result then
			MessageBox(Handle, PWideChar(JSON), ERR_CONFIRMATION, MB_ICONERROR + MB_OK);
	end;

end;

function TRegistrationForm.createAccount(firstname, lastname, Login, password, Domain: WideString; var Code: WideString): boolean;
var
	JSON: WideString;

begin
	HTTPConnection.HTTP.Request.UserAgent := 'curl/7.63.0'; //required by the server
	HTTPConnection.HTTP.Request.Connection := EmptyWideStr;
	HTTPConnection.HTTP.Request.Accept := '*/*';

	HTTPConnection.HTTP.Request.Referer := MAILRU_REGISTRATION_SIGNUP;

	result := HTTPConnection.PostForm(MAILRU_REGISTRATION_SIGNUP, Format('name={"first":"%s","last":"%s"}&login=%s&domain=%s&password=%s', [firstname, lastname, Login, Domain, password]), JSON);
	if result then
	begin
		result := TCMROperationResult.GetRegistrationOperationResult(JSON).OperationResult = CLOUD_OPERATION_OK;
		if result then
			result := getRegistrationBody(JSON, Code)
		else
			MessageBox(Handle, PWideChar(JSON), ERR_REGISTRATION, MB_ICONERROR + MB_OK);
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

procedure TRegistrationForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	case Key of
		VK_ESCAPE:
			Close;
	end;
end;

procedure TRegistrationForm.FreeComponents;
begin
	HTTPConnection.Free;
	//	JSONParser.Free;
end;

function TRegistrationForm.getRegisrationCaptcha(CaptchaStream: TStream): boolean;
begin
	result := FS_FILE_OK = HTTPConnection.getFile(MAILRU_CAPTCHA, CaptchaStream);
end;

procedure TRegistrationForm.InitComponents;
begin
	HTTPConnection := TCloudMailRuHTTP.Create(ConnectionSettings);
	//	JSONParser := TCloudMailRuJSONParser.Create();
end;

function TRegistrationForm.RegistrationValid: boolean;
begin
	result := (FirstNameEdit.Text <> EmptyWideStr) and (LastNameEdit.Text <> EmptyWideStr) and (LoginEdit.Text <> EmptyWideStr) and (PasswordEdit.Text <> EmptyWideStr);
end;

procedure TRegistrationForm.SendBtnClick(Sender: TObject);
begin
	if confirmRegistration(AccountSettings.Email, Code, CaptchaEdit.Text) then
		self.ModalResult := mrOk
	else
		self.ModalResult := mrNone;
end;

class function TRegistrationForm.ShowRegistration(parentWindow: HWND; ConnectionSettings: TConnectionSettings; AccountSettings: TAccountSettings): integer;
var
	RegistrationForm: TRegistrationForm;

begin
	try
		RegistrationForm := TRegistrationForm.Create(nil);
		RegistrationForm.parentWindow := parentWindow;
		RegistrationForm.AccountSettings := AccountSettings;
		RegistrationForm.ConnectionSettings := ConnectionSettings;
		RegistrationForm.LoginEdit.Text := AccountSettings.User;
		RegistrationForm.UseTCPwdMngrCB.Checked := AccountSettings.UseTCPasswordManager;
		RegistrationForm.ModalResult := mrNone;
		result := RegistrationForm.ShowModal;
		if result = mrOk then
		begin
			AccountSettings := RegistrationForm.AccountSettings;
			AccountSettings.UseTCPasswordManager := RegistrationForm.UseTCPwdMngrCB.Checked;
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
	AccountSettings.Email := Format('%s@%s', [LoginEdit.Text, DomainCombo.Text]);
	AccountSettings.password := PasswordEdit.Text;
	AccountSettings.PublicAccount := false;
	AccountSettings.EncryptFilesMode := EncryptModeNone;
	AccountSettings.TwostepAuth := false;

	self.Enabled := false;

	if (createAccount(FirstNameEdit.Text, LastNameEdit.Text, AccountSettings.User, AccountSettings.password, AccountSettings.Domain, Code)) then
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
			MessageBox(Handle, ERR_LOAD_CAPTCHA, ERR_REGISTRATION, MB_ICONERROR + MB_OK);
		MemStream.Free;
	end;
	self.Enabled := true;
end;

end.
