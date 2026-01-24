unit RegistrationPresenterTest;

{Unit tests for TRegistrationPresenter - registration dialog business logic.
 Tests validation, signup workflow, and confirmation workflow.}

interface

uses
	DUnitX.TestFramework,
	System.SysUtils,
	System.Classes,
	RegistrationPresenter,
	AccountSettings,
	SETTINGS_CONSTANTS;

type
	{Mock view for testing RegistrationPresenter}
	TMockRegistrationView = class(TInterfacedObject, IRegistrationView)
	public
		{Field values}
		FirstName: WideString;
		LastName: WideString;
		Login: WideString;
		Password: WideString;
		Domain: WideString;
		Captcha: WideString;
		UseTCPwdMngr: Boolean;

		{Control state}
		SignupEnabled: Boolean;
		CaptchaEnabled: Boolean;
		SendEnabled: Boolean;
		FormEnabled: Boolean;

		{Captcha display}
		CaptchaDisplayed: Boolean;
		CaptchaStreamSize: Int64;

		{Error tracking}
		ErrorShown: Boolean;
		ErrorTitle: WideString;
		ErrorMessage: WideString;

		{Call tracking}
		SetSignupEnabledCalls: Integer;
		SetCaptchaEnabledCalls: Integer;
		SetSendEnabledCalls: Integer;
		SetFormEnabledCalls: Integer;

		constructor Create;

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
	end;

	[TestFixture]
	TRegistrationPresenterTest = class
	private
		FMockView: TMockRegistrationView;
		FPresenter: TRegistrationPresenter;

		{Callback tracking}
		FCreateAccountCalled: Boolean;
		FCreateAccountFirstName: WideString;
		FCreateAccountLastName: WideString;
		FCreateAccountLogin: WideString;
		FCreateAccountPassword: WideString;
		FCreateAccountDomain: WideString;
		FCreateAccountResult: Boolean;
		FCreateAccountCode: WideString;

		FGetCaptchaCalled: Boolean;
		FGetCaptchaResult: Boolean;

		FConfirmRegistrationCalled: Boolean;
		FConfirmEmail: WideString;
		FConfirmCode: WideString;
		FConfirmCaptcha: WideString;
		FConfirmResult: Boolean;

		{Test callbacks}
		function MockCreateAccount(FirstName, LastName, Login, Password, Domain: WideString; var Code: WideString): Boolean;
		function MockGetCaptcha(CaptchaStream: TStream): Boolean;
		function MockConfirmRegistration(Email, Code, Captcha: WideString): Boolean;

		procedure SetupValidForm;
		procedure SetupCallbacks;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Constructor tests}
		[Test]
		procedure Create_StoresViewReference;

		{Initialize tests}
		[Test]
		procedure Initialize_StoresAccountSettings;
		[Test]
		procedure Initialize_DisablesSignupButton;
		[Test]
		procedure Initialize_DisablesCaptcha;
		[Test]
		procedure Initialize_DisablesSendButton;

		{SetCallbacks tests}
		[Test]
		procedure SetCallbacks_StoresCallbacks;

		{IsFormValid / OnFieldChanged tests}
		[Test]
		procedure OnFieldChanged_AllFieldsEmpty_DisablesSignup;
		[Test]
		procedure OnFieldChanged_FirstNameEmpty_DisablesSignup;
		[Test]
		procedure OnFieldChanged_LastNameEmpty_DisablesSignup;
		[Test]
		procedure OnFieldChanged_LoginEmpty_DisablesSignup;
		[Test]
		procedure OnFieldChanged_PasswordEmpty_DisablesSignup;
		[Test]
		procedure OnFieldChanged_AllFieldsFilled_EnablesSignup;

		{OnSignupClick tests}
		[Test]
		procedure OnSignupClick_DisablesCaptchaAndSend;
		[Test]
		procedure OnSignupClick_BuildsEmailFromLoginAndDomain;
		[Test]
		procedure OnSignupClick_SetsPasswordInSettings;
		[Test]
		procedure OnSignupClick_SetsPublicAccountFalse;
		[Test]
		procedure OnSignupClick_SetsEncryptModeNone;
		[Test]
		procedure OnSignupClick_SetsTwostepAuthFalse;
		[Test]
		procedure OnSignupClick_DisablesFormDuringOperation;
		[Test]
		procedure OnSignupClick_ReenablesFormAfterOperation;
		[Test]
		procedure OnSignupClick_CallsCreateAccountCallback;
		[Test]
		procedure OnSignupClick_PassesCorrectParameters;
		[Test]
		procedure OnSignupClick_CreateAccountFails_ReturnsFalse;
		[Test]
		procedure OnSignupClick_CaptchaFails_ReturnsFalse;
		[Test]
		procedure OnSignupClick_Success_DisplaysCaptcha;
		[Test]
		procedure OnSignupClick_Success_EnablesCaptchaAndSend;
		[Test]
		procedure OnSignupClick_Success_ReturnsTrue;

		{OnConfirmClick tests}
		[Test]
		procedure OnConfirmClick_CallsConfirmCallback;
		[Test]
		procedure OnConfirmClick_PassesCorrectEmail;
		[Test]
		procedure OnConfirmClick_PassesCorrectCode;
		[Test]
		procedure OnConfirmClick_PassesCorrectCaptcha;
		[Test]
		procedure OnConfirmClick_ConfirmFails_ReturnsFalse;
		[Test]
		procedure OnConfirmClick_ConfirmSucceeds_ReturnsTrue;

		{GetAccountSettings tests}
		[Test]
		procedure GetAccountSettings_ReturnsStoredSettings;
		[Test]
		procedure GetAccountSettings_IncludesUseTCPwdMngr;
	end;

implementation

{TMockRegistrationView}

constructor TMockRegistrationView.Create;
begin
	inherited;
	FormEnabled := True;
	Domain := 'mail.ru';
end;

function TMockRegistrationView.GetFirstName: WideString;
begin
	Result := FirstName;
end;

function TMockRegistrationView.GetLastName: WideString;
begin
	Result := LastName;
end;

function TMockRegistrationView.GetLogin: WideString;
begin
	Result := Login;
end;

function TMockRegistrationView.GetPassword: WideString;
begin
	Result := Password;
end;

function TMockRegistrationView.GetDomain: WideString;
begin
	Result := Domain;
end;

function TMockRegistrationView.GetCaptcha: WideString;
begin
	Result := Captcha;
end;

function TMockRegistrationView.GetUseTCPwdMngr: Boolean;
begin
	Result := UseTCPwdMngr;
end;

procedure TMockRegistrationView.SetSignupEnabled(Enabled: Boolean);
begin
	SignupEnabled := Enabled;
	Inc(SetSignupEnabledCalls);
end;

procedure TMockRegistrationView.SetCaptchaEnabled(Enabled: Boolean);
begin
	CaptchaEnabled := Enabled;
	Inc(SetCaptchaEnabledCalls);
end;

procedure TMockRegistrationView.SetSendEnabled(Enabled: Boolean);
begin
	SendEnabled := Enabled;
	Inc(SetSendEnabledCalls);
end;

procedure TMockRegistrationView.SetFormEnabled(Enabled: Boolean);
begin
	FormEnabled := Enabled;
	Inc(SetFormEnabledCalls);
end;

procedure TMockRegistrationView.DisplayCaptchaImage(Stream: TStream);
begin
	CaptchaDisplayed := True;
	CaptchaStreamSize := Stream.Size;
end;

procedure TMockRegistrationView.ShowError(Title, Message: WideString);
begin
	ErrorShown := True;
	ErrorTitle := Title;
	ErrorMessage := Message;
end;

{TRegistrationPresenterTest}

procedure TRegistrationPresenterTest.Setup;
begin
	FMockView := TMockRegistrationView.Create;
	FPresenter := TRegistrationPresenter.Create(FMockView);

	{Reset callback tracking}
	FCreateAccountCalled := False;
	FCreateAccountResult := True;
	FCreateAccountCode := 'test_code_123';
	FGetCaptchaCalled := False;
	FGetCaptchaResult := True;
	FConfirmRegistrationCalled := False;
	FConfirmResult := True;
end;

procedure TRegistrationPresenterTest.TearDown;
begin
	FPresenter.Free;
	{FMockView is reference counted}
end;

function TRegistrationPresenterTest.MockCreateAccount(FirstName, LastName, Login, Password, Domain: WideString; var Code: WideString): Boolean;
begin
	FCreateAccountCalled := True;
	FCreateAccountFirstName := FirstName;
	FCreateAccountLastName := LastName;
	FCreateAccountLogin := Login;
	FCreateAccountPassword := Password;
	FCreateAccountDomain := Domain;
	Code := FCreateAccountCode;
	Result := FCreateAccountResult;
end;

function TRegistrationPresenterTest.MockGetCaptcha(CaptchaStream: TStream): Boolean;
var
	TestData: AnsiString;
begin
	FGetCaptchaCalled := True;
	if FGetCaptchaResult then
	begin
		TestData := 'fake_captcha_image_data';
		CaptchaStream.Write(TestData[1], Length(TestData));
	end;
	Result := FGetCaptchaResult;
end;

function TRegistrationPresenterTest.MockConfirmRegistration(Email, Code, Captcha: WideString): Boolean;
begin
	FConfirmRegistrationCalled := True;
	FConfirmEmail := Email;
	FConfirmCode := Code;
	FConfirmCaptcha := Captcha;
	Result := FConfirmResult;
end;

procedure TRegistrationPresenterTest.SetupValidForm;
begin
	FMockView.FirstName := 'John';
	FMockView.LastName := 'Doe';
	FMockView.Login := 'johndoe';
	FMockView.Password := 'secret123';
	FMockView.Domain := 'mail.ru';
	FMockView.Captcha := 'abc123';
end;

procedure TRegistrationPresenterTest.SetupCallbacks;
begin
	FPresenter.SetCallbacks(MockCreateAccount, MockGetCaptcha, MockConfirmRegistration);
end;

{Constructor tests}

procedure TRegistrationPresenterTest.Create_StoresViewReference;
begin
	{Verify by calling method that uses view}
	FMockView.FirstName := 'Test';
	FMockView.LastName := 'User';
	FMockView.Login := 'testuser';
	FMockView.Password := 'pass';

	FPresenter.OnFieldChanged;

	Assert.IsTrue(FMockView.SignupEnabled, 'Presenter should interact with view');
end;

{Initialize tests}

procedure TRegistrationPresenterTest.Initialize_StoresAccountSettings;
var
	Settings: TAccountSettings;
begin
	Settings := Default(TAccountSettings);
	Settings.Email := 'test@mail.ru';

	FPresenter.Initialize(Settings);

	Assert.AreEqual('test@mail.ru', FPresenter.AccountSettings.Email);
end;

procedure TRegistrationPresenterTest.Initialize_DisablesSignupButton;
begin
	FMockView.SignupEnabled := True;

	FPresenter.Initialize(Default(TAccountSettings));

	Assert.IsFalse(FMockView.SignupEnabled);
end;

procedure TRegistrationPresenterTest.Initialize_DisablesCaptcha;
begin
	FMockView.CaptchaEnabled := True;

	FPresenter.Initialize(Default(TAccountSettings));

	Assert.IsFalse(FMockView.CaptchaEnabled);
end;

procedure TRegistrationPresenterTest.Initialize_DisablesSendButton;
begin
	FMockView.SendEnabled := True;

	FPresenter.Initialize(Default(TAccountSettings));

	Assert.IsFalse(FMockView.SendEnabled);
end;

{SetCallbacks tests}

procedure TRegistrationPresenterTest.SetCallbacks_StoresCallbacks;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));

	FPresenter.SetCallbacks(MockCreateAccount, MockGetCaptcha, MockConfirmRegistration);
	FPresenter.OnSignupClick;

	Assert.IsTrue(FCreateAccountCalled, 'CreateAccount callback should be stored and called');
end;

{IsFormValid / OnFieldChanged tests}

procedure TRegistrationPresenterTest.OnFieldChanged_AllFieldsEmpty_DisablesSignup;
begin
	FMockView.FirstName := '';
	FMockView.LastName := '';
	FMockView.Login := '';
	FMockView.Password := '';

	FPresenter.OnFieldChanged;

	Assert.IsFalse(FMockView.SignupEnabled);
end;

procedure TRegistrationPresenterTest.OnFieldChanged_FirstNameEmpty_DisablesSignup;
begin
	FMockView.FirstName := '';
	FMockView.LastName := 'Doe';
	FMockView.Login := 'johndoe';
	FMockView.Password := 'secret';

	FPresenter.OnFieldChanged;

	Assert.IsFalse(FMockView.SignupEnabled);
end;

procedure TRegistrationPresenterTest.OnFieldChanged_LastNameEmpty_DisablesSignup;
begin
	FMockView.FirstName := 'John';
	FMockView.LastName := '';
	FMockView.Login := 'johndoe';
	FMockView.Password := 'secret';

	FPresenter.OnFieldChanged;

	Assert.IsFalse(FMockView.SignupEnabled);
end;

procedure TRegistrationPresenterTest.OnFieldChanged_LoginEmpty_DisablesSignup;
begin
	FMockView.FirstName := 'John';
	FMockView.LastName := 'Doe';
	FMockView.Login := '';
	FMockView.Password := 'secret';

	FPresenter.OnFieldChanged;

	Assert.IsFalse(FMockView.SignupEnabled);
end;

procedure TRegistrationPresenterTest.OnFieldChanged_PasswordEmpty_DisablesSignup;
begin
	FMockView.FirstName := 'John';
	FMockView.LastName := 'Doe';
	FMockView.Login := 'johndoe';
	FMockView.Password := '';

	FPresenter.OnFieldChanged;

	Assert.IsFalse(FMockView.SignupEnabled);
end;

procedure TRegistrationPresenterTest.OnFieldChanged_AllFieldsFilled_EnablesSignup;
begin
	SetupValidForm;

	FPresenter.OnFieldChanged;

	Assert.IsTrue(FMockView.SignupEnabled);
end;

{OnSignupClick tests}

procedure TRegistrationPresenterTest.OnSignupClick_DisablesCaptchaAndSend;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	FMockView.CaptchaEnabled := True;
	FMockView.SendEnabled := True;
	SetupCallbacks;

	FPresenter.OnSignupClick;

	{Should have been disabled at the start (even if re-enabled later on success)}
	Assert.IsTrue(FMockView.SetCaptchaEnabledCalls >= 1);
	Assert.IsTrue(FMockView.SetSendEnabledCalls >= 1);
end;

procedure TRegistrationPresenterTest.OnSignupClick_BuildsEmailFromLoginAndDomain;
begin
	SetupValidForm;
	FMockView.Login := 'mylogin';
	FMockView.Domain := 'inbox.ru';
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;

	FPresenter.OnSignupClick;

	Assert.AreEqual('mylogin@inbox.ru', FPresenter.AccountSettings.Email);
end;

procedure TRegistrationPresenterTest.OnSignupClick_SetsPasswordInSettings;
begin
	SetupValidForm;
	FMockView.Password := 'mypassword123';
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;

	FPresenter.OnSignupClick;

	Assert.AreEqual('mypassword123', FPresenter.AccountSettings.password);
end;

procedure TRegistrationPresenterTest.OnSignupClick_SetsPublicAccountFalse;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;

	FPresenter.OnSignupClick;

	Assert.IsFalse(FPresenter.AccountSettings.PublicAccount);
end;

procedure TRegistrationPresenterTest.OnSignupClick_SetsEncryptModeNone;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;

	FPresenter.OnSignupClick;

	Assert.AreEqual(EncryptModeNone, FPresenter.AccountSettings.EncryptFilesMode);
end;

procedure TRegistrationPresenterTest.OnSignupClick_SetsTwostepAuthFalse;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;

	FPresenter.OnSignupClick;

	Assert.IsFalse(FPresenter.AccountSettings.TwostepAuth);
end;

procedure TRegistrationPresenterTest.OnSignupClick_DisablesFormDuringOperation;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;

	FPresenter.OnSignupClick;

	{Form should have been disabled during operation}
	Assert.IsTrue(FMockView.SetFormEnabledCalls >= 2, 'Form should be disabled then re-enabled');
end;

procedure TRegistrationPresenterTest.OnSignupClick_ReenablesFormAfterOperation;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;

	FPresenter.OnSignupClick;

	Assert.IsTrue(FMockView.FormEnabled, 'Form should be re-enabled after operation');
end;

procedure TRegistrationPresenterTest.OnSignupClick_CallsCreateAccountCallback;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;

	FPresenter.OnSignupClick;

	Assert.IsTrue(FCreateAccountCalled);
end;

procedure TRegistrationPresenterTest.OnSignupClick_PassesCorrectParameters;
begin
	SetupValidForm;
	FMockView.FirstName := 'Alice';
	FMockView.LastName := 'Smith';
	FMockView.Login := 'alicesmith';
	FMockView.Password := 'pass456';
	FMockView.Domain := 'list.ru';
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;

	FPresenter.OnSignupClick;

	Assert.AreEqual('Alice', FCreateAccountFirstName);
	Assert.AreEqual('Smith', FCreateAccountLastName);
	Assert.AreEqual('alicesmith', FCreateAccountLogin);
	Assert.AreEqual('pass456', FCreateAccountPassword);
	Assert.AreEqual('list.ru', FCreateAccountDomain);
end;

procedure TRegistrationPresenterTest.OnSignupClick_CreateAccountFails_ReturnsFalse;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;
	FCreateAccountResult := False;

	var Result := FPresenter.OnSignupClick;

	Assert.IsFalse(Result);
	Assert.IsFalse(FGetCaptchaCalled, 'Should not proceed to captcha if create fails');
end;

procedure TRegistrationPresenterTest.OnSignupClick_CaptchaFails_ReturnsFalse;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;
	FGetCaptchaResult := False;

	var Result := FPresenter.OnSignupClick;

	Assert.IsFalse(Result);
end;

procedure TRegistrationPresenterTest.OnSignupClick_Success_DisplaysCaptcha;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;

	FPresenter.OnSignupClick;

	Assert.IsTrue(FMockView.CaptchaDisplayed);
	Assert.IsTrue(FMockView.CaptchaStreamSize > 0, 'Captcha stream should have content');
end;

procedure TRegistrationPresenterTest.OnSignupClick_Success_EnablesCaptchaAndSend;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;

	FPresenter.OnSignupClick;

	Assert.IsTrue(FMockView.CaptchaEnabled);
	Assert.IsTrue(FMockView.SendEnabled);
end;

procedure TRegistrationPresenterTest.OnSignupClick_Success_ReturnsTrue;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;

	var Result := FPresenter.OnSignupClick;

	Assert.IsTrue(Result);
end;

{OnConfirmClick tests}

procedure TRegistrationPresenterTest.OnConfirmClick_CallsConfirmCallback;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;
	FPresenter.OnSignupClick; {Sets up code}

	FPresenter.OnConfirmClick;

	Assert.IsTrue(FConfirmRegistrationCalled);
end;

procedure TRegistrationPresenterTest.OnConfirmClick_PassesCorrectEmail;
begin
	SetupValidForm;
	FMockView.Login := 'testuser';
	FMockView.Domain := 'mail.ru';
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;
	FPresenter.OnSignupClick;

	FPresenter.OnConfirmClick;

	Assert.AreEqual('testuser@mail.ru', FConfirmEmail);
end;

procedure TRegistrationPresenterTest.OnConfirmClick_PassesCorrectCode;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;
	FCreateAccountCode := 'unique_code_xyz';
	FPresenter.OnSignupClick;

	FPresenter.OnConfirmClick;

	Assert.AreEqual('unique_code_xyz', FConfirmCode);
end;

procedure TRegistrationPresenterTest.OnConfirmClick_PassesCorrectCaptcha;
begin
	SetupValidForm;
	FMockView.Captcha := 'captcha_answer';
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;
	FPresenter.OnSignupClick;

	FPresenter.OnConfirmClick;

	Assert.AreEqual('captcha_answer', FConfirmCaptcha);
end;

procedure TRegistrationPresenterTest.OnConfirmClick_ConfirmFails_ReturnsFalse;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;
	FPresenter.OnSignupClick;
	FConfirmResult := False;

	var Result := FPresenter.OnConfirmClick;

	Assert.IsFalse(Result);
end;

procedure TRegistrationPresenterTest.OnConfirmClick_ConfirmSucceeds_ReturnsTrue;
begin
	SetupValidForm;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;
	FPresenter.OnSignupClick;
	FConfirmResult := True;

	var Result := FPresenter.OnConfirmClick;

	Assert.IsTrue(Result);
end;

{GetAccountSettings tests}

procedure TRegistrationPresenterTest.GetAccountSettings_ReturnsStoredSettings;
begin
	SetupValidForm;
	FMockView.Login := 'myuser';
	FMockView.Domain := 'bk.ru';
	FMockView.Password := 'mypass';
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;
	FPresenter.OnSignupClick;

	var Settings := FPresenter.GetAccountSettings;

	Assert.AreEqual('myuser@bk.ru', Settings.Email);
	Assert.AreEqual('mypass', Settings.password);
end;

procedure TRegistrationPresenterTest.GetAccountSettings_IncludesUseTCPwdMngr;
begin
	SetupValidForm;
	FMockView.UseTCPwdMngr := True;
	FPresenter.Initialize(Default(TAccountSettings));
	SetupCallbacks;
	FPresenter.OnSignupClick;

	var Settings := FPresenter.GetAccountSettings;

	Assert.IsTrue(Settings.UseTCPasswordManager);
end;

initialization
	TDUnitX.RegisterTestFixture(TRegistrationPresenterTest);

end.
