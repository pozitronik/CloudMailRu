unit RegistrationPresenter;

{Presenter for Registration dialog - handles business logic for account registration.
 Follows MVP pattern: View (TRegistrationForm) implements IRegistrationView,
 Presenter orchestrates registration workflow including validation and HTTP operations.}

interface

uses
	AccountSettings,
	ConnectionSettings,
	System.SysUtils,
	System.Classes;

type
	{Callback types for HTTP operations performed by the view}
	TCreateAccountFunc = reference to function(FirstName, LastName, Login, Password, Domain: WideString; var Code: WideString): Boolean;
	TGetCaptchaFunc = reference to function(CaptchaStream: TStream): Boolean;
	TConfirmRegistrationFunc = reference to function(Email, Code, Captcha: WideString): Boolean;

	{View interface for Registration dialog}
	IRegistrationView = interface
		['{E5F6A7B8-C9D0-1234-EFAB-567890123456}']
		{Form fields}
		function GetFirstName: WideString;
		function GetLastName: WideString;
		function GetLogin: WideString;
		function GetPassword: WideString;
		function GetDomain: WideString;
		function GetCaptcha: WideString;
		function GetUseTCPwdMngr: Boolean;

		{Control state}
		procedure SetSignupEnabled(Enabled: Boolean);
		procedure SetCaptchaEnabled(Enabled: Boolean);
		procedure SetSendEnabled(Enabled: Boolean);
		procedure SetFormEnabled(Enabled: Boolean);

		{Captcha display}
		procedure DisplayCaptchaImage(Stream: TStream);

		{Feedback}
		procedure ShowError(Title, Message: WideString);
	end;

	{Presenter for Registration dialog}
	TRegistrationPresenter = class
	private
		FView: IRegistrationView;
		FAccountSettings: TAccountSettings;
		FCode: WideString;
		FOnCreateAccount: TCreateAccountFunc;
		FOnGetCaptcha: TGetCaptchaFunc;
		FOnConfirmRegistration: TConfirmRegistrationFunc;

		function IsFormValid: Boolean;
	public
		constructor Create(View: IRegistrationView);

		{Initialize with account settings}
		procedure Initialize(AccountSettings: TAccountSettings);

		{Set HTTP operation callbacks}
		procedure SetCallbacks(CreateAccount: TCreateAccountFunc; GetCaptcha: TGetCaptchaFunc; ConfirmRegistration: TConfirmRegistrationFunc);

		{Called when form field changes - validates and updates button state}
		procedure OnFieldChanged;

		{Called when Signup button is clicked}
		function OnSignupClick: Boolean;

		{Called when Send/Confirm button is clicked}
		function OnConfirmClick: Boolean;

		{Get final account settings after successful registration}
		function GetAccountSettings: TAccountSettings;

		{Properties}
		property AccountSettings: TAccountSettings read FAccountSettings;
	end;

implementation

uses
	SETTINGS_CONSTANTS;

{TRegistrationPresenter}

constructor TRegistrationPresenter.Create(View: IRegistrationView);
begin
	inherited Create;
	FView := View;
end;

procedure TRegistrationPresenter.Initialize(AccountSettings: TAccountSettings);
begin
	FAccountSettings := AccountSettings;
	FCode := '';
	FView.SetSignupEnabled(False);
	FView.SetCaptchaEnabled(False);
	FView.SetSendEnabled(False);
end;

procedure TRegistrationPresenter.SetCallbacks(CreateAccount: TCreateAccountFunc; GetCaptcha: TGetCaptchaFunc; ConfirmRegistration: TConfirmRegistrationFunc);
begin
	FOnCreateAccount := CreateAccount;
	FOnGetCaptcha := GetCaptcha;
	FOnConfirmRegistration := ConfirmRegistration;
end;

function TRegistrationPresenter.IsFormValid: Boolean;
begin
	Result := (FView.GetFirstName <> EmptyWideStr) and
		(FView.GetLastName <> EmptyWideStr) and
		(FView.GetLogin <> EmptyWideStr) and
		(FView.GetPassword <> EmptyWideStr);
end;

procedure TRegistrationPresenter.OnFieldChanged;
begin
	FView.SetSignupEnabled(IsFormValid);
end;

function TRegistrationPresenter.OnSignupClick: Boolean;
var
	CaptchaStream: TMemoryStream;
begin
	Result := False;

	{Disable controls during signup}
	FView.SetCaptchaEnabled(False);
	FView.SetSendEnabled(False);

	{Build account settings}
	FAccountSettings.Email := Format('%s@%s', [FView.GetLogin, FView.GetDomain]);
	FAccountSettings.password := FView.GetPassword;
	FAccountSettings.PublicAccount := False;
	FAccountSettings.EncryptFilesMode := EncryptModeNone;
	FAccountSettings.TwostepAuth := False;

	FView.SetFormEnabled(False);
	try
		{Create account}
		if not Assigned(FOnCreateAccount) then
			Exit;

		if not FOnCreateAccount(FView.GetFirstName, FView.GetLastName, FView.GetLogin, FView.GetPassword, FView.GetDomain, FCode) then
			Exit;

		{Load captcha}
		if not Assigned(FOnGetCaptcha) then
			Exit;

		CaptchaStream := TMemoryStream.Create;
		try
			if FOnGetCaptcha(CaptchaStream) then
			begin
				CaptchaStream.Position := 0;
				FView.DisplayCaptchaImage(CaptchaStream);
				FView.SetCaptchaEnabled(True);
				FView.SetSendEnabled(True);
				Result := True;
			end;
		finally
			CaptchaStream.Free;
		end;
	finally
		FView.SetFormEnabled(True);
	end;
end;

function TRegistrationPresenter.OnConfirmClick: Boolean;
begin
	Result := False;
	if not Assigned(FOnConfirmRegistration) then
		Exit;

	Result := FOnConfirmRegistration(FAccountSettings.Email, FCode, FView.GetCaptcha);
end;

function TRegistrationPresenter.GetAccountSettings: TAccountSettings;
begin
	Result := FAccountSettings;
	Result.UseTCPasswordManager := FView.GetUseTCPwdMngr;
end;

end.
