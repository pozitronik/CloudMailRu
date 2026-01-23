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
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	Vcl.Imaging.JPEG,
	CloudHTTP,
	TCLogger,
	TCProgress,
	CMROperationResult,
	CMROperationResultJsonAdapter,
	CMRConstants,
	LANGUAGE_STRINGS,
	SETTINGS_CONSTANTS,
	PLUGIN_TYPES,
	JSONHelper,
	AccountSettings,
	ConnectionSettings,
	RegistrationPresenter;

type
	TRegistrationForm = class(TForm, IRegistrationView)
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
		FPresenter: TRegistrationPresenter;
		FConnectionSettings: TConnectionSettings;
		HTTPConnection: TCloudMailRuHTTP;

		{IRegistrationView implementation}
		function GetFirstName: WideString;
		function GetLastName: WideString;
		function GetLogin: WideString;
		function GetPassword: WideString;
		function GetDomain: WideString;
		function GetCaptcha: WideString;
		function GetUseTCPwdMngr: Boolean;
		procedure SetSignupEnabled(Enabled: Boolean);
		procedure SetCaptchaEnabled(Enabled: Boolean);
		procedure SetSendEnabled(Enabled: Boolean);
		procedure SetFormEnabled(Enabled: Boolean);
		procedure DisplayCaptchaImage(Stream: TStream);
		procedure ShowError(Title, Message: WideString);

		{HTTP operations}
		function DoCreateAccount(FirstName, LastName, Login, Password, Domain: WideString; var Code: WideString): Boolean;
		function DoGetCaptcha(CaptchaStream: TStream): Boolean;
		function DoConfirmRegistration(Email, Code, Captcha: WideString): Boolean;

		procedure InitComponents;
		procedure FreeComponents;
	public
		class function ShowRegistration(parentWindow: HWND; ConnectionSettings: TConnectionSettings; AccountSettings: TAccountSettings): integer;
	end;

implementation

{$R *.dfm}

{TRegistrationForm - IRegistrationView implementation}

function TRegistrationForm.GetFirstName: WideString;
begin
	Result := FirstNameEdit.Text;
end;

function TRegistrationForm.GetLastName: WideString;
begin
	Result := LastNameEdit.Text;
end;

function TRegistrationForm.GetLogin: WideString;
begin
	Result := LoginEdit.Text;
end;

function TRegistrationForm.GetPassword: WideString;
begin
	Result := PasswordEdit.Text;
end;

function TRegistrationForm.GetDomain: WideString;
begin
	Result := DomainCombo.Text;
end;

function TRegistrationForm.GetCaptcha: WideString;
begin
	Result := CaptchaEdit.Text;
end;

function TRegistrationForm.GetUseTCPwdMngr: Boolean;
begin
	Result := UseTCPwdMngrCB.Checked;
end;

procedure TRegistrationForm.SetSignupEnabled(Enabled: Boolean);
begin
	SignupBTN.Enabled := Enabled;
end;

procedure TRegistrationForm.SetCaptchaEnabled(Enabled: Boolean);
begin
	CaptchaEdit.Enabled := Enabled;
end;

procedure TRegistrationForm.SetSendEnabled(Enabled: Boolean);
begin
	SendBtn.Enabled := Enabled;
end;

procedure TRegistrationForm.SetFormEnabled(Enabled: Boolean);
begin
	Self.Enabled := Enabled;
end;

procedure TRegistrationForm.DisplayCaptchaImage(Stream: TStream);
var
	JPEGImage: TJPEGImage;
begin
	JPEGImage := TJPEGImage.Create;
	try
		JPEGImage.LoadFromStream(Stream);
		CaptchaImg.Picture.Assign(JPEGImage);
	finally
		JPEGImage.Free;
	end;
end;

procedure TRegistrationForm.ShowError(Title, Message: WideString);
begin
	MessageBox(Handle, PWideChar(Message), PWideChar(Title), MB_ICONERROR + MB_OK);
end;

{TRegistrationForm - HTTP operations}

function TRegistrationForm.DoCreateAccount(FirstName, LastName, Login, Password, Domain: WideString; var Code: WideString): Boolean;
var
	JSON: WideString;
	OperationResult: TCMROperationResult;
begin
	HTTPConnection.HTTP.Request.UserAgent := 'curl/7.63.0';
	HTTPConnection.HTTP.Request.Connection := EmptyWideStr;
	HTTPConnection.HTTP.Request.Accept := '*/*';
	HTTPConnection.HTTP.Request.Referer := MAILRU_REGISTRATION_SIGNUP;

	Result := HTTPConnection.PostForm(MAILRU_REGISTRATION_SIGNUP,
		Format('name={"first":"%s","last":"%s"}&login=%s&domain=%s&password=%s',
			[FirstName, LastName, Login, Domain, Password]), JSON);

	if Result then
	begin
		TCMROperationResultJsonAdapter.ParseRegistration(JSON, OperationResult);
		Result := OperationResult.OperationResult = CLOUD_OPERATION_OK;
		if Result then
			Result := getRegistrationBody(JSON, Code)
		else
			ShowError(ERR_REGISTRATION, JSON);
	end;
end;

function TRegistrationForm.DoGetCaptcha(CaptchaStream: TStream): Boolean;
begin
	Result := FS_FILE_OK = HTTPConnection.getFile(MAILRU_CAPTCHA, CaptchaStream);
	if not Result then
		ShowError(ERR_REGISTRATION, ERR_LOAD_CAPTCHA);
end;

function TRegistrationForm.DoConfirmRegistration(Email, Code, Captcha: WideString): Boolean;
var
	JSON: WideString;
	OperationResult: TCMROperationResult;
begin
	Result := HTTPConnection.PostForm(MAILRU_REGISTRATION_CONFIRM,
		Format('email=%s&reg_anketa={"id":"%s","capcha":"%s"}', [Email, Code, Captcha]), JSON);

	if Result then
	begin
		TCMROperationResultJsonAdapter.ParseRegistration(JSON, OperationResult);
		Result := OperationResult.OperationResult = CLOUD_OPERATION_OK;
		if not Result then
			ShowError(ERR_CONFIRMATION, JSON);
	end;
end;

{TRegistrationForm}

procedure TRegistrationForm.InitComponents;
begin
	HTTPConnection := TCloudMailRuHTTP.Create(FConnectionSettings, TNullLogger.Create, TNullProgress.Create);
end;

procedure TRegistrationForm.FreeComponents;
begin
	HTTPConnection.Free;
end;

procedure TRegistrationForm.CaptchaEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	case Key of
		VK_RETURN:
			if SendBtn.Enabled then
				SendBtn.Click;
	end;
end;

procedure TRegistrationForm.FirstNameEditChange(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnFieldChanged;
end;

procedure TRegistrationForm.FormCreate(Sender: TObject);
begin
	InitComponents;
end;

procedure TRegistrationForm.FormDestroy(Sender: TObject);
begin
	FreeAndNil(FPresenter);
	FreeComponents;
end;

procedure TRegistrationForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	case Key of
		VK_ESCAPE:
			Close;
	end;
end;

procedure TRegistrationForm.SendBtnClick(Sender: TObject);
begin
	if FPresenter.OnConfirmClick then
		Self.ModalResult := mrOk
	else
		Self.ModalResult := mrNone;
end;

procedure TRegistrationForm.SignupBTNClick(Sender: TObject);
begin
	FPresenter.OnSignupClick;
end;

class function TRegistrationForm.ShowRegistration(parentWindow: HWND; ConnectionSettings: TConnectionSettings; AccountSettings: TAccountSettings): integer;
var
	RegistrationForm: TRegistrationForm;
begin
	RegistrationForm := TRegistrationForm.Create(nil);
	try
		RegistrationForm.parentWindow := parentWindow;
		RegistrationForm.FConnectionSettings := ConnectionSettings;
		RegistrationForm.LoginEdit.Text := AccountSettings.User;
		RegistrationForm.UseTCPwdMngrCB.Checked := AccountSettings.UseTCPasswordManager;
		RegistrationForm.ModalResult := mrNone;

		RegistrationForm.FPresenter := TRegistrationPresenter.Create(RegistrationForm);
		RegistrationForm.FPresenter.Initialize(AccountSettings);
		RegistrationForm.FPresenter.SetCallbacks(
			RegistrationForm.DoCreateAccount,
			RegistrationForm.DoGetCaptcha,
			RegistrationForm.DoConfirmRegistration
		);

		Result := RegistrationForm.ShowModal;
		if Result = mrOk then
			AccountSettings := RegistrationForm.FPresenter.GetAccountSettings;
	finally
		FreeAndNil(RegistrationForm);
	end;
end;

end.
