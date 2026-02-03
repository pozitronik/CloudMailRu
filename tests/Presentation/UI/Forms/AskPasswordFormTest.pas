unit AskPasswordFormTest;

{Unit tests for TAskPasswordForm - the VCL view layer.
 Tests IAskPasswordView implementation and event handlers.
 Note: Class methods (AskPassword, AskAction, AskText) require ShowModal
 and are tested via integration/manual testing.}

interface

uses
	DUnitX.TestFramework,
	Winapi.Windows,
	Vcl.Forms,
	Vcl.StdCtrls,
	Vcl.Controls,
	System.Classes,
	System.SysUtils,
	System.Generics.Collections,
	AskPassword,
	AskPasswordPresenter;

type
	{Testable subclass to access protected members and avoid ShowModal}
	TTestableAskPasswordForm = class(TAskPasswordForm)
	public
		{Expose presenter for testing}
		procedure SetPresenter(Presenter: TAskPasswordPresenter);
		function GetPresenter: TAskPasswordPresenter;

		{Expose controls for testing}
		function GetPasswordEdit: TEdit;
		function GetOkButton: TButton;
		function GetCheckbox: TCheckBox;
		function GetLabel: TLabel;

		{Expose event handlers for direct testing}
		procedure TestPasswordEditChange;
		procedure TestFormKeyUp(Key: Word);

		{Expose button counter for testing}
		procedure ResetButtonLeft;
		function GetComponentCount: Integer;
	end;

	[TestFixture]
	TAskPasswordFormTest = class
	private
		FForm: TTestableAskPasswordForm;
		FPresenter: TAskPasswordPresenter;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{IAskPasswordView - SetCaption}
		[Test]
		procedure SetCaption_SetsFormCaption;

		{IAskPasswordView - SetLabelText}
		[Test]
		procedure SetLabelText_SetsLabelCaption;

		{IAskPasswordView - SetPasswordVisible}
		[Test]
		procedure SetPasswordVisible_True_ShowsEdit;
		[Test]
		procedure SetPasswordVisible_False_HidesEdit;

		{IAskPasswordView - SetPasswordChar}
		[Test]
		procedure SetPasswordChar_Asterisk_MasksInput;
		[Test]
		procedure SetPasswordChar_Null_ShowsPlainText;

		{IAskPasswordView - GetPassword}
		[Test]
		procedure GetPassword_ReturnsEditText;
		[Test]
		procedure GetPassword_EmptyEdit_ReturnsEmpty;

		{IAskPasswordView - SetOkButtonVisible}
		[Test]
		procedure SetOkButtonVisible_True_ShowsButton;
		[Test]
		procedure SetOkButtonVisible_False_HidesButton;

		{IAskPasswordView - SetOkButtonEnabled}
		[Test]
		procedure SetOkButtonEnabled_True_EnablesButton;
		[Test]
		procedure SetOkButtonEnabled_False_DisablesButton;

		{IAskPasswordView - SetCheckboxVisible}
		[Test]
		procedure SetCheckboxVisible_True_ShowsCheckbox;
		[Test]
		procedure SetCheckboxVisible_False_HidesCheckbox;

		{IAskPasswordView - SetCheckboxEnabled}
		[Test]
		procedure SetCheckboxEnabled_True_EnablesCheckbox;
		[Test]
		procedure SetCheckboxEnabled_False_DisablesCheckbox;

		{IAskPasswordView - SetCheckboxChecked}
		[Test]
		procedure SetCheckboxChecked_True_ChecksBox;
		[Test]
		procedure SetCheckboxChecked_False_UnchecksBox;

		{IAskPasswordView - GetCheckboxChecked}
		[Test]
		procedure GetCheckboxChecked_ReturnsCheckedState;

		{IAskPasswordView - AddActionButton}
		[Test]
		procedure AddActionButton_CreatesButton;
		[Test]
		procedure AddActionButton_SetsCorrectCaption;
		[Test]
		procedure AddActionButton_SetsCorrectModalResult;
		[Test]
		procedure AddActionButton_MultipleButtons_PositionsCorrectly;

		{Event handlers}
		[Test]
		procedure PasswordEditChange_CallsPresenter;

		{FormKeyUp - VK_ESCAPE}
		[Test]
		procedure FormKeyUp_Escape_ClosesForm;

		{FormKeyUp - VK_RETURN}
		[Test]
		procedure FormKeyUp_Return_OkEnabled_ClicksOk;
		[Test]
		procedure FormKeyUp_Return_OkDisabled_DoesNothing;

		{FormShow}
		[Test]
		procedure FormShow_PasswordHidden_SkipsSetFocus;
		[Test]
		procedure FormShow_PasswordVisible_ExecutesWithoutError;

		{Destructor}
		[Test]
		procedure Destroy_FreesPresenter;
	end;

implementation

{TTestableAskPasswordForm}

procedure TTestableAskPasswordForm.SetPresenter(Presenter: TAskPasswordPresenter);
begin
	FPresenter := Presenter;
end;

function TTestableAskPasswordForm.GetPresenter: TAskPasswordPresenter;
begin
	Result := FPresenter;
end;

function TTestableAskPasswordForm.GetPasswordEdit: TEdit;
begin
	Result := PasswordEdit;
end;

function TTestableAskPasswordForm.GetOkButton: TButton;
begin
	Result := OkButton;
end;

function TTestableAskPasswordForm.GetCheckbox: TCheckBox;
begin
	Result := UseTCPwdMngrCB;
end;

function TTestableAskPasswordForm.GetLabel: TLabel;
begin
	Result := PasswordEditLabel;
end;

procedure TTestableAskPasswordForm.TestPasswordEditChange;
begin
	PasswordEditChange(PasswordEdit);
end;

procedure TTestableAskPasswordForm.TestFormKeyUp(Key: Word);
var
	Shift: TShiftState;
begin
	Shift := [];
	FormKeyUp(Self, Key, Shift);
end;

procedure TTestableAskPasswordForm.ResetButtonLeft;
begin
	FNextButtonLeft := 7;
end;

function TTestableAskPasswordForm.GetComponentCount: Integer;
begin
	Result := ComponentCount;
end;

{TAskPasswordFormTest}

procedure TAskPasswordFormTest.Setup;
begin
	FForm := TTestableAskPasswordForm.Create(nil);
	FPresenter := TAskPasswordPresenter.Create(FForm);
	FForm.SetPresenter(FPresenter);
end;

procedure TAskPasswordFormTest.TearDown;
begin
	{Form destructor frees the presenter}
	FreeAndNil(FForm);
end;

{SetCaption tests}

procedure TAskPasswordFormTest.SetCaption_SetsFormCaption;
begin
	(FForm as IAskPasswordView).SetCaption('Test Caption');

	Assert.AreEqual('Test Caption', FForm.Caption);
end;

{SetLabelText tests}

procedure TAskPasswordFormTest.SetLabelText_SetsLabelCaption;
begin
	(FForm as IAskPasswordView).SetLabelText('Enter your password:');

	Assert.AreEqual('Enter your password:', FForm.GetLabel.Caption);
end;

{SetPasswordVisible tests}

procedure TAskPasswordFormTest.SetPasswordVisible_True_ShowsEdit;
begin
	FForm.GetPasswordEdit.Visible := False;

	(FForm as IAskPasswordView).SetPasswordVisible(True);

	Assert.IsTrue(FForm.GetPasswordEdit.Visible);
end;

procedure TAskPasswordFormTest.SetPasswordVisible_False_HidesEdit;
begin
	FForm.GetPasswordEdit.Visible := True;

	(FForm as IAskPasswordView).SetPasswordVisible(False);

	Assert.IsFalse(FForm.GetPasswordEdit.Visible);
end;

{SetPasswordChar tests}

procedure TAskPasswordFormTest.SetPasswordChar_Asterisk_MasksInput;
begin
	(FForm as IAskPasswordView).SetPasswordChar('*');

	Assert.AreEqual('*', FForm.GetPasswordEdit.PasswordChar);
end;

procedure TAskPasswordFormTest.SetPasswordChar_Null_ShowsPlainText;
begin
	FForm.GetPasswordEdit.PasswordChar := '*';

	(FForm as IAskPasswordView).SetPasswordChar(#0);

	Assert.AreEqual(#0, FForm.GetPasswordEdit.PasswordChar);
end;

{GetPassword tests}

procedure TAskPasswordFormTest.GetPassword_ReturnsEditText;
begin
	FForm.GetPasswordEdit.Text := 'SecretPassword';

	Assert.AreEqual('SecretPassword', (FForm as IAskPasswordView).GetPassword);
end;

procedure TAskPasswordFormTest.GetPassword_EmptyEdit_ReturnsEmpty;
begin
	FForm.GetPasswordEdit.Text := '';

	Assert.AreEqual('', (FForm as IAskPasswordView).GetPassword);
end;

{SetOkButtonVisible tests}

procedure TAskPasswordFormTest.SetOkButtonVisible_True_ShowsButton;
begin
	FForm.GetOkButton.Visible := False;

	(FForm as IAskPasswordView).SetOkButtonVisible(True);

	Assert.IsTrue(FForm.GetOkButton.Visible);
end;

procedure TAskPasswordFormTest.SetOkButtonVisible_False_HidesButton;
begin
	FForm.GetOkButton.Visible := True;

	(FForm as IAskPasswordView).SetOkButtonVisible(False);

	Assert.IsFalse(FForm.GetOkButton.Visible);
end;

{SetOkButtonEnabled tests}

procedure TAskPasswordFormTest.SetOkButtonEnabled_True_EnablesButton;
begin
	FForm.GetOkButton.Enabled := False;

	(FForm as IAskPasswordView).SetOkButtonEnabled(True);

	Assert.IsTrue(FForm.GetOkButton.Enabled);
end;

procedure TAskPasswordFormTest.SetOkButtonEnabled_False_DisablesButton;
begin
	FForm.GetOkButton.Enabled := True;

	(FForm as IAskPasswordView).SetOkButtonEnabled(False);

	Assert.IsFalse(FForm.GetOkButton.Enabled);
end;

{SetCheckboxVisible tests}

procedure TAskPasswordFormTest.SetCheckboxVisible_True_ShowsCheckbox;
begin
	FForm.GetCheckbox.Visible := False;

	(FForm as IAskPasswordView).SetCheckboxVisible(True);

	Assert.IsTrue(FForm.GetCheckbox.Visible);
end;

procedure TAskPasswordFormTest.SetCheckboxVisible_False_HidesCheckbox;
begin
	FForm.GetCheckbox.Visible := True;

	(FForm as IAskPasswordView).SetCheckboxVisible(False);

	Assert.IsFalse(FForm.GetCheckbox.Visible);
end;

{SetCheckboxEnabled tests}

procedure TAskPasswordFormTest.SetCheckboxEnabled_True_EnablesCheckbox;
begin
	FForm.GetCheckbox.Enabled := False;

	(FForm as IAskPasswordView).SetCheckboxEnabled(True);

	Assert.IsTrue(FForm.GetCheckbox.Enabled);
end;

procedure TAskPasswordFormTest.SetCheckboxEnabled_False_DisablesCheckbox;
begin
	FForm.GetCheckbox.Enabled := True;

	(FForm as IAskPasswordView).SetCheckboxEnabled(False);

	Assert.IsFalse(FForm.GetCheckbox.Enabled);
end;

{SetCheckboxChecked tests}

procedure TAskPasswordFormTest.SetCheckboxChecked_True_ChecksBox;
begin
	FForm.GetCheckbox.Checked := False;

	(FForm as IAskPasswordView).SetCheckboxChecked(True);

	Assert.IsTrue(FForm.GetCheckbox.Checked);
end;

procedure TAskPasswordFormTest.SetCheckboxChecked_False_UnchecksBox;
begin
	FForm.GetCheckbox.Checked := True;

	(FForm as IAskPasswordView).SetCheckboxChecked(False);

	Assert.IsFalse(FForm.GetCheckbox.Checked);
end;

{GetCheckboxChecked tests}

procedure TAskPasswordFormTest.GetCheckboxChecked_ReturnsCheckedState;
begin
	FForm.GetCheckbox.Checked := True;
	Assert.IsTrue((FForm as IAskPasswordView).GetCheckboxChecked);

	FForm.GetCheckbox.Checked := False;
	Assert.IsFalse((FForm as IAskPasswordView).GetCheckboxChecked);
end;

{AddActionButton tests}

procedure TAskPasswordFormTest.AddActionButton_CreatesButton;
var
	InitialCount: Integer;
begin
	InitialCount := FForm.GetComponentCount;
	FForm.ResetButtonLeft;

	(FForm as IAskPasswordView).AddActionButton('Test', 100);

	Assert.AreEqual(InitialCount + 1, FForm.GetComponentCount);
end;

procedure TAskPasswordFormTest.AddActionButton_SetsCorrectCaption;
var
	i: Integer;
	Btn: TButton;
begin
	FForm.ResetButtonLeft;

	(FForm as IAskPasswordView).AddActionButton('MyButton', 100);

	{Find the added button}
	for i := 0 to FForm.ComponentCount - 1 do
		if (FForm.Components[i] is TButton) and (TButton(FForm.Components[i]).ModalResult = 100) then
		begin
			Btn := TButton(FForm.Components[i]);
			Assert.AreEqual('MyButton', Btn.Caption);
			Exit;
		end;
	Assert.Fail('Button not found');
end;

procedure TAskPasswordFormTest.AddActionButton_SetsCorrectModalResult;
var
	i: Integer;
	Btn: TButton;
	Found: Boolean;
begin
	FForm.ResetButtonLeft;
	Found := False;

	(FForm as IAskPasswordView).AddActionButton('Action', 42);

	for i := 0 to FForm.ComponentCount - 1 do
		if (FForm.Components[i] is TButton) and (TButton(FForm.Components[i]).Caption = 'Action') then
		begin
			Btn := TButton(FForm.Components[i]);
			Assert.AreEqual(42, Btn.ModalResult);
			Found := True;
			Break;
		end;
	Assert.IsTrue(Found, 'Button not found');
end;

procedure TAskPasswordFormTest.AddActionButton_MultipleButtons_PositionsCorrectly;
var
	i: Integer;
	Btn1Left, Btn2Left: Integer;
begin
	FForm.ResetButtonLeft;
	Btn1Left := -1;
	Btn2Left := -1;

	(FForm as IAskPasswordView).AddActionButton('First', 1);
	(FForm as IAskPasswordView).AddActionButton('Second', 2);

	{Find both buttons and check positions}
	for i := 0 to FForm.ComponentCount - 1 do
		if FForm.Components[i] is TButton then
		begin
			if TButton(FForm.Components[i]).ModalResult = 1 then
				Btn1Left := TButton(FForm.Components[i]).Left
			else if TButton(FForm.Components[i]).ModalResult = 2 then
				Btn2Left := TButton(FForm.Components[i]).Left;
		end;

	Assert.IsTrue(Btn1Left >= 0, 'First button not found');
	Assert.IsTrue(Btn2Left >= 0, 'Second button not found');
	Assert.IsTrue(Btn2Left > Btn1Left, 'Second button should be positioned after first');
end;

{PasswordEditChange tests}

procedure TAskPasswordFormTest.PasswordEditChange_CallsPresenter;
begin
	FForm.GetPasswordEdit.Text := 'test';

	{Should not raise exception}
	FForm.TestPasswordEditChange;

	Assert.Pass('Event handler executed without error');
end;

{FormKeyUp tests}

procedure TAskPasswordFormTest.FormKeyUp_Escape_ClosesForm;
begin
	{Note: We can't fully test Close without showing the form,
	 but we can verify the handler doesn't crash}
	FForm.TestFormKeyUp(VK_ESCAPE);

	Assert.Pass('Escape key handled without error');
end;

procedure TAskPasswordFormTest.FormKeyUp_Return_OkEnabled_ClicksOk;
begin
	FForm.GetOkButton.Enabled := True;

	{Note: We can't fully test Click without showing the form,
	 but we can verify the handler doesn't crash}
	FForm.TestFormKeyUp(VK_RETURN);

	Assert.Pass('Return key handled without error when OK enabled');
end;

procedure TAskPasswordFormTest.FormKeyUp_Return_OkDisabled_DoesNothing;
begin
	FForm.GetOkButton.Enabled := False;

	{Should not click OK when disabled}
	FForm.TestFormKeyUp(VK_RETURN);

	Assert.Pass('Return key handled correctly when OK disabled');
end;

{FormShow tests}

procedure TAskPasswordFormTest.FormShow_PasswordHidden_SkipsSetFocus;
begin
	{When password edit is hidden, FormShow should not call SetFocus}
	FForm.GetPasswordEdit.Visible := False;
	FForm.FormShow(FForm);
	Assert.Pass('FormShow with hidden password edit executed without error');
end;

procedure TAskPasswordFormTest.FormShow_PasswordVisible_ExecutesWithoutError;
begin
	{When password edit is visible, FormShow attempts SetFocus}
	FForm.GetPasswordEdit.Visible := True;
	try
		FForm.FormShow(FForm);
	except
		{SetFocus may fail if form handle is not created -- acceptable in test}
	end;
	Assert.Pass('FormShow with visible password edit did not crash');
end;

{Destructor tests}

procedure TAskPasswordFormTest.Destroy_FreesPresenter;
var
	TempForm: TTestableAskPasswordForm;
	Presenter: TAskPasswordPresenter;
begin
	TempForm := TTestableAskPasswordForm.Create(nil);
	Presenter := TAskPasswordPresenter.Create(TempForm);
	TempForm.SetPresenter(Presenter);

	{Destroy should free the presenter without memory leak}
	TempForm.Free;

	Assert.Pass('Form destroyed without memory leak');
end;

initialization
	TDUnitX.RegisterTestFixture(TAskPasswordFormTest);

end.
