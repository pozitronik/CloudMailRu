unit AskPasswordPresenterTest;

interface

uses
	DUnitX.TestFramework,
	AskPasswordPresenter,
	System.Generics.Collections,
	System.SysUtils,
	LANGUAGE_STRINGS;

type
	{Mock view for testing AskPasswordPresenter}
	TMockAskPasswordView = class(TInterfacedObject, IAskPasswordView)
	public
		Caption: WideString;
		LabelText: WideString;
		PasswordVisible: Boolean;
		PasswordChar: Char;
		Password: WideString;
		OkButtonVisible: Boolean;
		OkButtonEnabled: Boolean;
		CheckboxVisible: Boolean;
		CheckboxEnabled: Boolean;
		CheckboxChecked: Boolean;
		ActionButtons: TList<TActionButton>;

		constructor Create;
		destructor Destroy; override;

		{IAskPasswordView implementation}
		procedure SetCaption(ACaption: WideString);
		procedure SetLabelText(AText: WideString);
		procedure SetPasswordVisible(AVisible: Boolean);
		procedure SetPasswordChar(ACh: Char);
		function GetPassword: WideString;
		procedure SetOkButtonVisible(AVisible: Boolean);
		procedure SetOkButtonEnabled(AEnabled: Boolean);
		procedure SetCheckboxVisible(AVisible: Boolean);
		procedure SetCheckboxEnabled(AEnabled: Boolean);
		procedure SetCheckboxChecked(AChecked: Boolean);
		function GetCheckboxChecked: Boolean;
		procedure AddActionButton(ATitle: WideString; AResultCode: Integer);
	end;

	[TestFixture]
	TAskPasswordPresenterTest = class
	private
		FMockView: TMockAskPasswordView;
		FPresenter: TAskPasswordPresenter;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Password mode tests}
		[Test]
		procedure InitializePasswordMode_SetsCaption;

		[Test]
		procedure InitializePasswordMode_SetsLabelText;

		[Test]
		procedure InitializePasswordMode_ShowsPasswordEdit;

		[Test]
		procedure InitializePasswordMode_SetsPasswordChar;

		[Test]
		procedure InitializePasswordMode_ShowsOkButton;

		[Test]
		procedure InitializePasswordMode_DisablesOkButtonInitially;

		[Test]
		procedure InitializePasswordMode_ShowsCheckbox;

		[Test]
		procedure InitializePasswordMode_SetsCheckboxEnabled;

		[Test]
		procedure InitializePasswordMode_DisablesCheckboxWhenRequested;

		[Test]
		procedure InitializePasswordMode_SetsCheckboxChecked;

		[Test]
		procedure InitializePasswordMode_SetsCheckboxUnchecked;

		{Text mode tests}
		[Test]
		procedure InitializeTextMode_SetsCaption;

		[Test]
		procedure InitializeTextMode_SetsLabelText;

		[Test]
		procedure InitializeTextMode_ShowsPasswordEdit;

		[Test]
		procedure InitializeTextMode_ClearsPasswordChar;

		[Test]
		procedure InitializeTextMode_HidesOkButton;

		[Test]
		procedure InitializeTextMode_HidesCheckbox;

		[Test]
		procedure InitializeTextMode_AddsOkAndCancelButtons;

		[Test]
		procedure InitializeTextMode_OkButtonHasCorrectCode;

		[Test]
		procedure InitializeTextMode_CancelButtonHasCorrectCode;

		{Action mode tests}
		[Test]
		procedure InitializeActionMode_SetsCaption;

		[Test]
		procedure InitializeActionMode_SetsLabelText;

		[Test]
		procedure InitializeActionMode_HidesPasswordEdit;

		[Test]
		procedure InitializeActionMode_HidesOkButton;

		[Test]
		procedure InitializeActionMode_HidesCheckbox;

		[Test]
		procedure InitializeActionMode_AddsAllActionButtons;

		[Test]
		procedure InitializeActionMode_ButtonsHaveCorrectCodes;

		[Test]
		procedure InitializeActionMode_EmptyActions_AddsNoButtons;

		{Password change tests}
		[Test]
		procedure OnPasswordChanged_EmptyPassword_DisablesOkButton;

		[Test]
		procedure OnPasswordChanged_NonEmptyPassword_EnablesOkButton;

		{Result tests}
		[Test]
		procedure GetPassword_ReturnsViewPassword;

		[Test]
		procedure GetUseTCPwdMngr_ReturnsCheckboxStateTrue;

		[Test]
		procedure GetUseTCPwdMngr_ReturnsCheckboxStateFalse;
	end;

implementation

{TMockAskPasswordView}

constructor TMockAskPasswordView.Create;
begin
	inherited;
	ActionButtons := TList<TActionButton>.Create;
end;

destructor TMockAskPasswordView.Destroy;
begin
	ActionButtons.Free;
	inherited;
end;

procedure TMockAskPasswordView.SetCaption(ACaption: WideString);
begin
	Caption := ACaption;
end;

procedure TMockAskPasswordView.SetLabelText(AText: WideString);
begin
	LabelText := AText;
end;

procedure TMockAskPasswordView.SetPasswordVisible(AVisible: Boolean);
begin
	PasswordVisible := AVisible;
end;

procedure TMockAskPasswordView.SetPasswordChar(ACh: Char);
begin
	PasswordChar := ACh;
end;

function TMockAskPasswordView.GetPassword: WideString;
begin
	Result := Password;
end;

procedure TMockAskPasswordView.SetOkButtonVisible(AVisible: Boolean);
begin
	OkButtonVisible := AVisible;
end;

procedure TMockAskPasswordView.SetOkButtonEnabled(AEnabled: Boolean);
begin
	OkButtonEnabled := AEnabled;
end;

procedure TMockAskPasswordView.SetCheckboxVisible(AVisible: Boolean);
begin
	CheckboxVisible := AVisible;
end;

procedure TMockAskPasswordView.SetCheckboxEnabled(AEnabled: Boolean);
begin
	CheckboxEnabled := AEnabled;
end;

procedure TMockAskPasswordView.SetCheckboxChecked(AChecked: Boolean);
begin
	CheckboxChecked := AChecked;
end;

function TMockAskPasswordView.GetCheckboxChecked: Boolean;
begin
	Result := CheckboxChecked;
end;

procedure TMockAskPasswordView.AddActionButton(ATitle: WideString; AResultCode: Integer);
var
	Btn: TActionButton;
begin
	Btn.Title := ATitle;
	Btn.Code := AResultCode;
	ActionButtons.Add(Btn);
end;

{TAskPasswordPresenterTest}

procedure TAskPasswordPresenterTest.Setup;
begin
	FMockView := TMockAskPasswordView.Create;
	FPresenter := TAskPasswordPresenter.Create(FMockView);
end;

procedure TAskPasswordPresenterTest.TearDown;
begin
	FPresenter.Free;
	{FMockView is reference counted}
end;

{Password mode tests}

procedure TAskPasswordPresenterTest.InitializePasswordMode_SetsCaption;
begin
	FPresenter.InitializePasswordMode('Test Title', 'Enter password:', False, False);

	Assert.AreEqual('Test Title', FMockView.Caption);
end;

procedure TAskPasswordPresenterTest.InitializePasswordMode_SetsLabelText;
begin
	FPresenter.InitializePasswordMode('Test Title', 'Enter password:', False, False);

	Assert.AreEqual('Enter password:', FMockView.LabelText);
end;

procedure TAskPasswordPresenterTest.InitializePasswordMode_ShowsPasswordEdit;
begin
	FPresenter.InitializePasswordMode('Test Title', 'Enter password:', False, False);

	Assert.IsTrue(FMockView.PasswordVisible);
end;

procedure TAskPasswordPresenterTest.InitializePasswordMode_SetsPasswordChar;
begin
	FPresenter.InitializePasswordMode('Test Title', 'Enter password:', False, False);

	Assert.AreEqual('*', FMockView.PasswordChar);
end;

procedure TAskPasswordPresenterTest.InitializePasswordMode_ShowsOkButton;
begin
	FPresenter.InitializePasswordMode('Test Title', 'Enter password:', False, False);

	Assert.IsTrue(FMockView.OkButtonVisible);
end;

procedure TAskPasswordPresenterTest.InitializePasswordMode_DisablesOkButtonInitially;
begin
	FPresenter.InitializePasswordMode('Test Title', 'Enter password:', False, False);

	Assert.IsFalse(FMockView.OkButtonEnabled);
end;

procedure TAskPasswordPresenterTest.InitializePasswordMode_ShowsCheckbox;
begin
	FPresenter.InitializePasswordMode('Test Title', 'Enter password:', False, False);

	Assert.IsTrue(FMockView.CheckboxVisible);
end;

procedure TAskPasswordPresenterTest.InitializePasswordMode_SetsCheckboxEnabled;
begin
	FPresenter.InitializePasswordMode('Test Title', 'Enter password:', False, False);

	Assert.IsTrue(FMockView.CheckboxEnabled);
end;

procedure TAskPasswordPresenterTest.InitializePasswordMode_DisablesCheckboxWhenRequested;
begin
	FPresenter.InitializePasswordMode('Test Title', 'Enter password:', False, True);

	Assert.IsFalse(FMockView.CheckboxEnabled);
end;

procedure TAskPasswordPresenterTest.InitializePasswordMode_SetsCheckboxChecked;
begin
	FPresenter.InitializePasswordMode('Test Title', 'Enter password:', True, False);

	Assert.IsTrue(FMockView.CheckboxChecked);
end;

procedure TAskPasswordPresenterTest.InitializePasswordMode_SetsCheckboxUnchecked;
begin
	FPresenter.InitializePasswordMode('Test Title', 'Enter password:', False, False);

	Assert.IsFalse(FMockView.CheckboxChecked);
end;

{Text mode tests}

procedure TAskPasswordPresenterTest.InitializeTextMode_SetsCaption;
begin
	FPresenter.InitializeTextMode('Enter Text', 'Please enter value:');

	Assert.AreEqual('Enter Text', FMockView.Caption);
end;

procedure TAskPasswordPresenterTest.InitializeTextMode_SetsLabelText;
begin
	FPresenter.InitializeTextMode('Enter Text', 'Please enter value:');

	Assert.AreEqual('Please enter value:', FMockView.LabelText);
end;

procedure TAskPasswordPresenterTest.InitializeTextMode_ShowsPasswordEdit;
begin
	FPresenter.InitializeTextMode('Enter Text', 'Please enter value:');

	Assert.IsTrue(FMockView.PasswordVisible);
end;

procedure TAskPasswordPresenterTest.InitializeTextMode_ClearsPasswordChar;
begin
	FPresenter.InitializeTextMode('Enter Text', 'Please enter value:');

	Assert.AreEqual(#0, FMockView.PasswordChar);
end;

procedure TAskPasswordPresenterTest.InitializeTextMode_HidesOkButton;
begin
	FPresenter.InitializeTextMode('Enter Text', 'Please enter value:');

	Assert.IsFalse(FMockView.OkButtonVisible);
end;

procedure TAskPasswordPresenterTest.InitializeTextMode_HidesCheckbox;
begin
	FPresenter.InitializeTextMode('Enter Text', 'Please enter value:');

	Assert.IsFalse(FMockView.CheckboxVisible);
end;

procedure TAskPasswordPresenterTest.InitializeTextMode_AddsOkAndCancelButtons;
begin
	FPresenter.InitializeTextMode('Enter Text', 'Please enter value:');

	Assert.AreEqual(Integer(2), Integer(FMockView.ActionButtons.Count));
end;

procedure TAskPasswordPresenterTest.InitializeTextMode_OkButtonHasCorrectCode;
begin
	FPresenter.InitializeTextMode('Enter Text', 'Please enter value:');

	Assert.AreEqual(Integer(1), FMockView.ActionButtons[0].Code, 'OK button should have code 1 (mrOk)');
	Assert.AreEqual(OK, FMockView.ActionButtons[0].Title, 'OK button should have correct title');
end;

procedure TAskPasswordPresenterTest.InitializeTextMode_CancelButtonHasCorrectCode;
begin
	FPresenter.InitializeTextMode('Enter Text', 'Please enter value:');

	Assert.AreEqual(Integer(2), FMockView.ActionButtons[1].Code, 'Cancel button should have code 2 (mrCancel)');
	Assert.AreEqual(CANCEL, FMockView.ActionButtons[1].Title, 'Cancel button should have correct title');
end;

{Action mode tests}

procedure TAskPasswordPresenterTest.InitializeActionMode_SetsCaption;
var
	Actions: TDictionary<Integer, WideString>;
begin
	Actions := TDictionary<Integer, WideString>.Create;
	try
		Actions.Add(1, 'Action1');
		FPresenter.InitializeActionMode('Choose Action', 'Select one:', Actions);

		Assert.AreEqual('Choose Action', FMockView.Caption);
	finally
		Actions.Free;
	end;
end;

procedure TAskPasswordPresenterTest.InitializeActionMode_SetsLabelText;
var
	Actions: TDictionary<Integer, WideString>;
begin
	Actions := TDictionary<Integer, WideString>.Create;
	try
		Actions.Add(1, 'Action1');
		FPresenter.InitializeActionMode('Choose Action', 'Select one:', Actions);

		Assert.AreEqual('Select one:', FMockView.LabelText);
	finally
		Actions.Free;
	end;
end;

procedure TAskPasswordPresenterTest.InitializeActionMode_HidesPasswordEdit;
var
	Actions: TDictionary<Integer, WideString>;
begin
	Actions := TDictionary<Integer, WideString>.Create;
	try
		Actions.Add(1, 'Action1');
		FPresenter.InitializeActionMode('Choose Action', 'Select one:', Actions);

		Assert.IsFalse(FMockView.PasswordVisible);
	finally
		Actions.Free;
	end;
end;

procedure TAskPasswordPresenterTest.InitializeActionMode_HidesOkButton;
var
	Actions: TDictionary<Integer, WideString>;
begin
	Actions := TDictionary<Integer, WideString>.Create;
	try
		Actions.Add(1, 'Action1');
		FPresenter.InitializeActionMode('Choose Action', 'Select one:', Actions);

		Assert.IsFalse(FMockView.OkButtonVisible);
	finally
		Actions.Free;
	end;
end;

procedure TAskPasswordPresenterTest.InitializeActionMode_HidesCheckbox;
var
	Actions: TDictionary<Integer, WideString>;
begin
	Actions := TDictionary<Integer, WideString>.Create;
	try
		Actions.Add(1, 'Action1');
		FPresenter.InitializeActionMode('Choose Action', 'Select one:', Actions);

		Assert.IsFalse(FMockView.CheckboxVisible);
	finally
		Actions.Free;
	end;
end;

procedure TAskPasswordPresenterTest.InitializeActionMode_AddsAllActionButtons;
var
	Actions: TDictionary<Integer, WideString>;
begin
	Actions := TDictionary<Integer, WideString>.Create;
	try
		Actions.Add(1, 'Action1');
		Actions.Add(2, 'Action2');
		Actions.Add(3, 'Action3');
		FPresenter.InitializeActionMode('Choose Action', 'Select one:', Actions);

		Assert.AreEqual(Integer(3), Integer(FMockView.ActionButtons.Count));
	finally
		Actions.Free;
	end;
end;

procedure TAskPasswordPresenterTest.InitializeActionMode_ButtonsHaveCorrectCodes;
var
	Actions: TDictionary<Integer, WideString>;
	i: Integer;
	FoundCode100, FoundCode200: Boolean;
begin
	Actions := TDictionary<Integer, WideString>.Create;
	try
		Actions.Add(100, 'First Action');
		Actions.Add(200, 'Second Action');
		FPresenter.InitializeActionMode('Choose', 'Pick:', Actions);

		{Verify buttons have correct codes (order is not guaranteed by dictionary)}
		FoundCode100 := False;
		FoundCode200 := False;
		for i := 0 to FMockView.ActionButtons.Count - 1 do
		begin
			if FMockView.ActionButtons[i].Code = 100 then
			begin
				FoundCode100 := True;
				Assert.AreEqual('First Action', FMockView.ActionButtons[i].Title);
			end;
			if FMockView.ActionButtons[i].Code = 200 then
			begin
				FoundCode200 := True;
				Assert.AreEqual('Second Action', FMockView.ActionButtons[i].Title);
			end;
		end;
		Assert.IsTrue(FoundCode100, 'Should have button with code 100');
		Assert.IsTrue(FoundCode200, 'Should have button with code 200');
	finally
		Actions.Free;
	end;
end;

procedure TAskPasswordPresenterTest.InitializeActionMode_EmptyActions_AddsNoButtons;
var
	Actions: TDictionary<Integer, WideString>;
begin
	Actions := TDictionary<Integer, WideString>.Create;
	try
		FPresenter.InitializeActionMode('Choose', 'Pick:', Actions);

		Assert.AreEqual(Integer(0), Integer(FMockView.ActionButtons.Count));
	finally
		Actions.Free;
	end;
end;

{Password change tests}

procedure TAskPasswordPresenterTest.OnPasswordChanged_EmptyPassword_DisablesOkButton;
begin
	FPresenter.InitializePasswordMode('Test', 'Enter:', False, False);

	FPresenter.OnPasswordChanged('');

	Assert.IsFalse(FMockView.OkButtonEnabled);
end;

procedure TAskPasswordPresenterTest.OnPasswordChanged_NonEmptyPassword_EnablesOkButton;
begin
	FPresenter.InitializePasswordMode('Test', 'Enter:', False, False);

	FPresenter.OnPasswordChanged('secret');

	Assert.IsTrue(FMockView.OkButtonEnabled);
end;

{Result tests}

procedure TAskPasswordPresenterTest.GetPassword_ReturnsViewPassword;
begin
	FMockView.Password := 'TestPassword123';

	Assert.AreEqual('TestPassword123', FPresenter.GetPassword);
end;

procedure TAskPasswordPresenterTest.GetUseTCPwdMngr_ReturnsCheckboxStateTrue;
begin
	FMockView.CheckboxChecked := True;

	Assert.IsTrue(FPresenter.GetUseTCPwdMngr);
end;

procedure TAskPasswordPresenterTest.GetUseTCPwdMngr_ReturnsCheckboxStateFalse;
begin
	FMockView.CheckboxChecked := False;

	Assert.IsFalse(FPresenter.GetUseTCPwdMngr);
end;

initialization
	TDUnitX.RegisterTestFixture(TAskPasswordPresenterTest);

end.
