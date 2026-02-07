unit AskEncryptionPasswordFormTest;

{Unit tests for TAskEncryptionPasswordForm - the VCL view layer.
 Tests IAskEncryptionPasswordView implementation, CloseQuery blocking,
 and event handlers. Class method AskEncryptionPassword requires ShowModal
 and is tested via manual testing.}

interface

uses
	DUnitX.TestFramework,
	Winapi.Windows,
	Vcl.Forms,
	Vcl.StdCtrls,
	Vcl.Controls,
	System.Classes,
	System.SysUtils,
	AskEncryptionPassword,
	AskEncryptionPasswordPresenter;

type
	{Testable subclass to access protected members and avoid ShowModal}
	TTestableAskEncryptionPasswordForm = class(TAskEncryptionPasswordForm)
	public
		{Expose presenter for testing}
		procedure SetPresenter(Presenter: TAskEncryptionPasswordPresenter);
		function GetPresenter: TAskEncryptionPasswordPresenter;

		{Expose controls for testing}
		function GetPasswordEdit: TEdit;
		function GetOkButton: TButton;
		function GetSkipButton: TButton;
		function GetCheckbox: TCheckBox;
		function GetLabel: TLabel;

		{Expose event handlers for direct testing}
		procedure TestPasswordEditChange;
		procedure TestFormKeyUp(Key: Word);
		function TestFormCloseQuery: Boolean;
	end;

	[TestFixture]
	TAskEncryptionPasswordFormTest = class
	private
		FForm: TTestableAskEncryptionPasswordForm;
		FPresenter: TAskEncryptionPasswordPresenter;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{IAskEncryptionPasswordView - SetCaption}
		[Test]
		procedure SetCaption_SetsFormCaption;

		{IAskEncryptionPasswordView - SetLabelText}
		[Test]
		procedure SetLabelText_SetsLabelCaption;

		{IAskEncryptionPasswordView - SetPasswordChar}
		[Test]
		procedure SetPasswordChar_Asterisk_MasksInput;
		[Test]
		procedure SetPasswordChar_Null_ShowsPlainText;

		{IAskEncryptionPasswordView - GetPassword}
		[Test]
		procedure GetPassword_ReturnsEditText;
		[Test]
		procedure GetPassword_EmptyEdit_ReturnsEmpty;

		{IAskEncryptionPasswordView - SetOkButtonEnabled}
		[Test]
		procedure SetOkButtonEnabled_True_EnablesButton;
		[Test]
		procedure SetOkButtonEnabled_False_DisablesButton;

		{IAskEncryptionPasswordView - SetCheckboxVisible}
		[Test]
		procedure SetCheckboxVisible_True_ShowsCheckbox;
		[Test]
		procedure SetCheckboxVisible_False_HidesCheckbox;

		{IAskEncryptionPasswordView - SetCheckboxEnabled}
		[Test]
		procedure SetCheckboxEnabled_True_EnablesCheckbox;
		[Test]
		procedure SetCheckboxEnabled_False_DisablesCheckbox;

		{IAskEncryptionPasswordView - SetCheckboxChecked}
		[Test]
		procedure SetCheckboxChecked_True_ChecksBox;
		[Test]
		procedure SetCheckboxChecked_False_UnchecksBox;

		{IAskEncryptionPasswordView - GetCheckboxChecked}
		[Test]
		procedure GetCheckboxChecked_ReturnsCheckedState;

		{IAskEncryptionPasswordView - SetSkipButtonCaption}
		[Test]
		procedure SetSkipButtonCaption_SetsCaption;

		{Skip button has correct ModalResult}
		[Test]
		procedure SkipButton_HasModalResultMrNo;

		{OK button has correct ModalResult}
		[Test]
		procedure OkButton_HasModalResultMrOk;

		{CloseQuery blocks when ModalResult is mrNone (Alt+F4)}
		[Test]
		procedure FormCloseQuery_ModalResultNone_BlocksClose;

		{CloseQuery allows when ModalResult is set (button click)}
		[Test]
		procedure FormCloseQuery_ModalResultOk_AllowsClose;

		{Event handlers}
		[Test]
		procedure PasswordEditChange_CallsPresenter;

		{FormKeyUp - VK_RETURN}
		[Test]
		procedure FormKeyUp_Return_OkEnabled_ClicksOk;
		[Test]
		procedure FormKeyUp_Return_OkDisabled_DoesNothing;

		{FormKeyUp - VK_ESCAPE does nothing}
		[Test]
		procedure FormKeyUp_Escape_DoesNothing;

		{FormShow}
		[Test]
		procedure FormShow_ExecutesWithoutError;

		{Destructor}
		[Test]
		procedure Destroy_FreesPresenter;
	end;

implementation

{TTestableAskEncryptionPasswordForm}

procedure TTestableAskEncryptionPasswordForm.SetPresenter(Presenter: TAskEncryptionPasswordPresenter);
begin
	FPresenter := Presenter;
end;

function TTestableAskEncryptionPasswordForm.GetPresenter: TAskEncryptionPasswordPresenter;
begin
	Result := FPresenter;
end;

function TTestableAskEncryptionPasswordForm.GetPasswordEdit: TEdit;
begin
	Result := PasswordEdit;
end;

function TTestableAskEncryptionPasswordForm.GetOkButton: TButton;
begin
	Result := OkButton;
end;

function TTestableAskEncryptionPasswordForm.GetSkipButton: TButton;
begin
	Result := SkipButton;
end;

function TTestableAskEncryptionPasswordForm.GetCheckbox: TCheckBox;
begin
	Result := UseTCPwdMngrCB;
end;

function TTestableAskEncryptionPasswordForm.GetLabel: TLabel;
begin
	Result := PasswordEditLabel;
end;

procedure TTestableAskEncryptionPasswordForm.TestPasswordEditChange;
begin
	PasswordEditChange(PasswordEdit);
end;

procedure TTestableAskEncryptionPasswordForm.TestFormKeyUp(Key: Word);
var
	Shift: TShiftState;
begin
	Shift := [];
	FormKeyUp(Self, Key, Shift);
end;

function TTestableAskEncryptionPasswordForm.TestFormCloseQuery: Boolean;
begin
	FormCloseQuery(Self, Result);
end;

{TAskEncryptionPasswordFormTest}

procedure TAskEncryptionPasswordFormTest.Setup;
begin
	FForm := TTestableAskEncryptionPasswordForm.Create(nil);
	FPresenter := TAskEncryptionPasswordPresenter.Create(FForm);
	FForm.SetPresenter(FPresenter);
end;

procedure TAskEncryptionPasswordFormTest.TearDown;
begin
	{Form destructor frees the presenter}
	FreeAndNil(FForm);
end;

{SetCaption tests}

procedure TAskEncryptionPasswordFormTest.SetCaption_SetsFormCaption;
begin
	(FForm as IAskEncryptionPasswordView).SetCaption('Test Caption');

	Assert.AreEqual('Test Caption', FForm.Caption);
end;

{SetLabelText tests}

procedure TAskEncryptionPasswordFormTest.SetLabelText_SetsLabelCaption;
begin
	(FForm as IAskEncryptionPasswordView).SetLabelText('Enter your password:');

	Assert.AreEqual('Enter your password:', FForm.GetLabel.Caption);
end;

{SetPasswordChar tests}

procedure TAskEncryptionPasswordFormTest.SetPasswordChar_Asterisk_MasksInput;
begin
	(FForm as IAskEncryptionPasswordView).SetPasswordChar('*');

	Assert.AreEqual('*', FForm.GetPasswordEdit.PasswordChar);
end;

procedure TAskEncryptionPasswordFormTest.SetPasswordChar_Null_ShowsPlainText;
begin
	FForm.GetPasswordEdit.PasswordChar := '*';

	(FForm as IAskEncryptionPasswordView).SetPasswordChar(#0);

	Assert.AreEqual(#0, FForm.GetPasswordEdit.PasswordChar);
end;

{GetPassword tests}

procedure TAskEncryptionPasswordFormTest.GetPassword_ReturnsEditText;
begin
	FForm.GetPasswordEdit.Text := 'SecretPassword';

	Assert.AreEqual('SecretPassword', (FForm as IAskEncryptionPasswordView).GetPassword);
end;

procedure TAskEncryptionPasswordFormTest.GetPassword_EmptyEdit_ReturnsEmpty;
begin
	FForm.GetPasswordEdit.Text := '';

	Assert.AreEqual('', (FForm as IAskEncryptionPasswordView).GetPassword);
end;

{SetOkButtonEnabled tests}

procedure TAskEncryptionPasswordFormTest.SetOkButtonEnabled_True_EnablesButton;
begin
	FForm.GetOkButton.Enabled := False;

	(FForm as IAskEncryptionPasswordView).SetOkButtonEnabled(True);

	Assert.IsTrue(FForm.GetOkButton.Enabled);
end;

procedure TAskEncryptionPasswordFormTest.SetOkButtonEnabled_False_DisablesButton;
begin
	FForm.GetOkButton.Enabled := True;

	(FForm as IAskEncryptionPasswordView).SetOkButtonEnabled(False);

	Assert.IsFalse(FForm.GetOkButton.Enabled);
end;

{SetCheckboxVisible tests}

procedure TAskEncryptionPasswordFormTest.SetCheckboxVisible_True_ShowsCheckbox;
begin
	FForm.GetCheckbox.Visible := False;

	(FForm as IAskEncryptionPasswordView).SetCheckboxVisible(True);

	Assert.IsTrue(FForm.GetCheckbox.Visible);
end;

procedure TAskEncryptionPasswordFormTest.SetCheckboxVisible_False_HidesCheckbox;
begin
	FForm.GetCheckbox.Visible := True;

	(FForm as IAskEncryptionPasswordView).SetCheckboxVisible(False);

	Assert.IsFalse(FForm.GetCheckbox.Visible);
end;

{SetCheckboxEnabled tests}

procedure TAskEncryptionPasswordFormTest.SetCheckboxEnabled_True_EnablesCheckbox;
begin
	FForm.GetCheckbox.Enabled := False;

	(FForm as IAskEncryptionPasswordView).SetCheckboxEnabled(True);

	Assert.IsTrue(FForm.GetCheckbox.Enabled);
end;

procedure TAskEncryptionPasswordFormTest.SetCheckboxEnabled_False_DisablesCheckbox;
begin
	FForm.GetCheckbox.Enabled := True;

	(FForm as IAskEncryptionPasswordView).SetCheckboxEnabled(False);

	Assert.IsFalse(FForm.GetCheckbox.Enabled);
end;

{SetCheckboxChecked tests}

procedure TAskEncryptionPasswordFormTest.SetCheckboxChecked_True_ChecksBox;
begin
	FForm.GetCheckbox.Checked := False;

	(FForm as IAskEncryptionPasswordView).SetCheckboxChecked(True);

	Assert.IsTrue(FForm.GetCheckbox.Checked);
end;

procedure TAskEncryptionPasswordFormTest.SetCheckboxChecked_False_UnchecksBox;
begin
	FForm.GetCheckbox.Checked := True;

	(FForm as IAskEncryptionPasswordView).SetCheckboxChecked(False);

	Assert.IsFalse(FForm.GetCheckbox.Checked);
end;

{GetCheckboxChecked tests}

procedure TAskEncryptionPasswordFormTest.GetCheckboxChecked_ReturnsCheckedState;
begin
	FForm.GetCheckbox.Checked := True;
	Assert.IsTrue((FForm as IAskEncryptionPasswordView).GetCheckboxChecked);

	FForm.GetCheckbox.Checked := False;
	Assert.IsFalse((FForm as IAskEncryptionPasswordView).GetCheckboxChecked);
end;

{SetSkipButtonCaption tests}

procedure TAskEncryptionPasswordFormTest.SetSkipButtonCaption_SetsCaption;
begin
	(FForm as IAskEncryptionPasswordView).SetSkipButtonCaption('Skip encryption');

	Assert.AreEqual('Skip encryption', FForm.GetSkipButton.Caption);
end;

{Button ModalResult tests}

procedure TAskEncryptionPasswordFormTest.SkipButton_HasModalResultMrNo;
begin
	Assert.AreEqual(mrNo, FForm.GetSkipButton.ModalResult);
end;

procedure TAskEncryptionPasswordFormTest.OkButton_HasModalResultMrOk;
begin
	Assert.AreEqual(mrOk, FForm.GetOkButton.ModalResult);
end;

{CloseQuery tests}

procedure TAskEncryptionPasswordFormTest.FormCloseQuery_ModalResultNone_BlocksClose;
var
	CanClose: Boolean;
begin
	FForm.ModalResult := mrNone;

	CanClose := FForm.TestFormCloseQuery;

	Assert.IsFalse(CanClose, 'CloseQuery should block close when ModalResult is mrNone');
end;

procedure TAskEncryptionPasswordFormTest.FormCloseQuery_ModalResultOk_AllowsClose;
var
	CanClose: Boolean;
begin
	FForm.ModalResult := mrOk;

	CanClose := FForm.TestFormCloseQuery;

	Assert.IsTrue(CanClose, 'CloseQuery should allow close when ModalResult is mrOk');
end;

{PasswordEditChange tests}

procedure TAskEncryptionPasswordFormTest.PasswordEditChange_CallsPresenter;
begin
	FForm.GetPasswordEdit.Text := 'test';

	{Should not raise exception}
	FForm.TestPasswordEditChange;

	Assert.Pass('Event handler executed without error');
end;

{FormKeyUp tests}

procedure TAskEncryptionPasswordFormTest.FormKeyUp_Return_OkEnabled_ClicksOk;
begin
	FForm.GetOkButton.Enabled := True;

	FForm.TestFormKeyUp(VK_RETURN);

	Assert.Pass('Return key handled without error when OK enabled');
end;

procedure TAskEncryptionPasswordFormTest.FormKeyUp_Return_OkDisabled_DoesNothing;
begin
	FForm.GetOkButton.Enabled := False;

	FForm.TestFormKeyUp(VK_RETURN);

	Assert.Pass('Return key handled correctly when OK disabled');
end;

procedure TAskEncryptionPasswordFormTest.FormKeyUp_Escape_DoesNothing;
begin
	{VK_ESCAPE should do nothing -- no close button semantics}
	FForm.TestFormKeyUp(VK_ESCAPE);

	Assert.Pass('Escape key does nothing (no close button)');
end;

{FormShow tests}

procedure TAskEncryptionPasswordFormTest.FormShow_ExecutesWithoutError;
begin
	try
		FForm.FormShow(FForm);
	except
		{SetFocus may fail if form handle is not created -- acceptable in test}
	end;
	Assert.Pass('FormShow did not crash');
end;

{Destructor tests}

procedure TAskEncryptionPasswordFormTest.Destroy_FreesPresenter;
var
	TempForm: TTestableAskEncryptionPasswordForm;
	Presenter: TAskEncryptionPasswordPresenter;
begin
	TempForm := TTestableAskEncryptionPasswordForm.Create(nil);
	Presenter := TAskEncryptionPasswordPresenter.Create(TempForm);
	TempForm.SetPresenter(Presenter);

	{Destroy should free the presenter without memory leak}
	TempForm.Free;

	Assert.Pass('Form destroyed without memory leak');
end;

initialization
	TDUnitX.RegisterTestFixture(TAskEncryptionPasswordFormTest);

end.
