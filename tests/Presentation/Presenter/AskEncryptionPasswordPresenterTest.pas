unit AskEncryptionPasswordPresenterTest;

interface

uses
	DUnitX.TestFramework,
	AskEncryptionPasswordPresenter,
	System.SysUtils,
	LanguageStrings;

type
	{Mock view for testing TAskEncryptionPasswordPresenter}
	TMockAskEncryptionPasswordView = class(TInterfacedObject, IAskEncryptionPasswordView)
	public
		Caption: WideString;
		LabelText: WideString;
		PasswordChar: Char;
		Password: WideString;
		OkButtonEnabled: Boolean;
		SkipButtonCaption: WideString;

		{IAskEncryptionPasswordView implementation}
		procedure SetCaption(const Value: WideString);
		procedure SetLabelText(const Value: WideString);
		procedure SetPasswordChar(Value: Char);
		function GetPassword: WideString;
		procedure SetOkButtonEnabled(Value: Boolean);
		procedure SetSkipButtonCaption(const Value: WideString);
	end;

	[TestFixture]
	TAskEncryptionPasswordPresenterTest = class
	private
		FMockView: TMockAskEncryptionPasswordView;
		FPresenter: TAskEncryptionPasswordPresenter;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Initialize tests}
		[Test]
		procedure Initialize_SetsCaption;

		[Test]
		procedure Initialize_SetsLabelText;

		[Test]
		procedure Initialize_SetsPasswordChar;

		[Test]
		procedure Initialize_DisablesOkButtonInitially;

		[Test]
		procedure Initialize_SetsSkipButtonCaption;

		{Password change tests}
		[Test]
		procedure OnPasswordChanged_EmptyPassword_DisablesOkButton;

		[Test]
		procedure OnPasswordChanged_NonEmptyPassword_EnablesOkButton;

		{Result tests}
		[Test]
		procedure GetPassword_ReturnsViewPassword;
	end;

implementation

{TMockAskEncryptionPasswordView}

procedure TMockAskEncryptionPasswordView.SetCaption(const Value: WideString);
begin
	Caption := Value;
end;

procedure TMockAskEncryptionPasswordView.SetLabelText(const Value: WideString);
begin
	LabelText := Value;
end;

procedure TMockAskEncryptionPasswordView.SetPasswordChar(Value: Char);
begin
	PasswordChar := Value;
end;

function TMockAskEncryptionPasswordView.GetPassword: WideString;
begin
	Result := Password;
end;

procedure TMockAskEncryptionPasswordView.SetOkButtonEnabled(Value: Boolean);
begin
	OkButtonEnabled := Value;
end;

procedure TMockAskEncryptionPasswordView.SetSkipButtonCaption(const Value: WideString);
begin
	SkipButtonCaption := Value;
end;

{TAskEncryptionPasswordPresenterTest}

procedure TAskEncryptionPasswordPresenterTest.Setup;
begin
	FMockView := TMockAskEncryptionPasswordView.Create;
	FPresenter := TAskEncryptionPasswordPresenter.Create(FMockView);
end;

procedure TAskEncryptionPasswordPresenterTest.TearDown;
begin
	FPresenter.Free;
	{FMockView is reference counted}
end;

{Initialize tests}

procedure TAskEncryptionPasswordPresenterTest.Initialize_SetsCaption;
begin
	FPresenter.Initialize('Test Title', 'Enter password:');

	Assert.AreEqual('Test Title', FMockView.Caption);
end;

procedure TAskEncryptionPasswordPresenterTest.Initialize_SetsLabelText;
begin
	FPresenter.Initialize('Test Title', 'Enter password:');

	Assert.AreEqual('Enter password:', FMockView.LabelText);
end;

procedure TAskEncryptionPasswordPresenterTest.Initialize_SetsPasswordChar;
begin
	FPresenter.Initialize('Test Title', 'Enter password:');

	Assert.AreEqual('*', FMockView.PasswordChar);
end;

procedure TAskEncryptionPasswordPresenterTest.Initialize_DisablesOkButtonInitially;
begin
	FPresenter.Initialize('Test Title', 'Enter password:');

	Assert.IsFalse(FMockView.OkButtonEnabled);
end;

procedure TAskEncryptionPasswordPresenterTest.Initialize_SetsSkipButtonCaption;
begin
	FPresenter.Initialize('Test Title', 'Enter password:');

	Assert.AreEqual(DFM_ASKENC_BTN_SKIP, FMockView.SkipButtonCaption);
end;

{Password change tests}

procedure TAskEncryptionPasswordPresenterTest.OnPasswordChanged_EmptyPassword_DisablesOkButton;
begin
	FPresenter.Initialize('Test', 'Enter:');

	FPresenter.OnPasswordChanged('');

	Assert.IsFalse(FMockView.OkButtonEnabled);
end;

procedure TAskEncryptionPasswordPresenterTest.OnPasswordChanged_NonEmptyPassword_EnablesOkButton;
begin
	FPresenter.Initialize('Test', 'Enter:');

	FPresenter.OnPasswordChanged('secret');

	Assert.IsTrue(FMockView.OkButtonEnabled);
end;

{Result tests}

procedure TAskEncryptionPasswordPresenterTest.GetPassword_ReturnsViewPassword;
begin
	FMockView.Password := 'TestPassword123';

	Assert.AreEqual('TestPassword123', FPresenter.GetPassword);
end;

initialization
	TDUnitX.RegisterTestFixture(TAskEncryptionPasswordPresenterTest);

end.
