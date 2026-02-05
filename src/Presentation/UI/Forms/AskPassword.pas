unit AskPassword;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.StdCtrls,
	PluginForm,
	System.Generics.Collections,
	AskPasswordPresenter,
	LanguageStrings;

type

	TAskPasswordForm = class(TPluginForm, IAskPasswordView)
		PasswordEditLabel: TLabel;
		PasswordEdit: TEdit;
		OkButton: TButton;
		UseTCPwdMngrCB: TCheckBox;

		procedure PasswordEditChange(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
	protected
		FPresenter: TAskPasswordPresenter;
		FNextButtonLeft: Integer;

		{IAskPasswordView implementation}
		procedure SetCaption(Caption: WideString);
		procedure SetLabelText(Text: WideString);
		procedure SetPasswordVisible(Visible: Boolean);
		procedure SetPasswordChar(Ch: Char);
		function GetPassword: WideString;
		procedure SetOkButtonVisible(Visible: Boolean);
		procedure SetOkButtonEnabled(Enabled: Boolean);
		procedure SetCheckboxVisible(Visible: Boolean);
		procedure SetCheckboxEnabled(Enabled: Boolean);
		procedure SetCheckboxChecked(Checked: Boolean);
		function GetCheckboxChecked: Boolean;
		procedure AddActionButton(Title: WideString; ResultCode: Integer);
		procedure UpdateFormCaptions;
	public
		destructor Destroy; override;
		class function AskPassword(CustomTitle, CustomText: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): integer;
		class function AskAction(CustomTitle, CustomText: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): integer;
		class function AskText(CustomTitle, CustomText: WideString; var Text: WideString; ParentWindow: HWND): Boolean;
	end;

implementation

{$R *.dfm}

{TAskPasswordForm - IAskPasswordView implementation}

procedure TAskPasswordForm.SetCaption(Caption: WideString);
begin
	Self.Caption := Caption;
end;

procedure TAskPasswordForm.SetLabelText(Text: WideString);
begin
	PasswordEditLabel.Caption := Text;
end;

procedure TAskPasswordForm.SetPasswordVisible(Visible: Boolean);
begin
	PasswordEdit.Visible := Visible;
end;

procedure TAskPasswordForm.SetPasswordChar(Ch: Char);
begin
	PasswordEdit.PasswordChar := Ch;
end;

function TAskPasswordForm.GetPassword: WideString;
begin
	Result := PasswordEdit.Text;
end;

procedure TAskPasswordForm.SetOkButtonVisible(Visible: Boolean);
begin
	OkButton.Visible := Visible;
end;

procedure TAskPasswordForm.SetOkButtonEnabled(Enabled: Boolean);
begin
	OkButton.Enabled := Enabled;
end;

procedure TAskPasswordForm.SetCheckboxVisible(Visible: Boolean);
begin
	UseTCPwdMngrCB.Visible := Visible;
end;

procedure TAskPasswordForm.SetCheckboxEnabled(Enabled: Boolean);
begin
	UseTCPwdMngrCB.Enabled := Enabled;
end;

procedure TAskPasswordForm.SetCheckboxChecked(Checked: Boolean);
begin
	UseTCPwdMngrCB.Checked := Checked;
end;

function TAskPasswordForm.GetCheckboxChecked: Boolean;
begin
	Result := UseTCPwdMngrCB.Checked;
end;

procedure TAskPasswordForm.AddActionButton(Title: WideString; ResultCode: Integer);
var
	Btn: TButton;
begin
	Btn := TButton.Create(Self);
	Btn.Caption := Title;
	Btn.top := 54;
	Btn.Left := FNextButtonLeft;
	Btn.ModalResult := ResultCode;
	Btn.Visible := True;
	Btn.Width := Self.Canvas.TextWidth(Title) + 16;
	Btn.Parent := Self;
	FNextButtonLeft := FNextButtonLeft + Btn.Width + 7;
end;

procedure TAskPasswordForm.UpdateFormCaptions;
begin
	OkButton.Caption := DFM_ASK_BTN_OK;
	UseTCPwdMngrCB.Caption := DFM_ASK_CB_STORE_PWD;
end;

{TAskPasswordForm}

destructor TAskPasswordForm.Destroy;
begin
	FreeAndNil(FPresenter);
	inherited;
end;

class function TAskPasswordForm.AskAction(CustomTitle, CustomText: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): integer;
var
	AskPasswordForm: TAskPasswordForm;
begin
	AskPasswordForm := TAskPasswordForm.Create(nil);
	try
		AskPasswordForm.ParentWindow := ParentWindow;
		AskPasswordForm.UpdateFormCaptions;
		AskPasswordForm.FNextButtonLeft := 7;
		AskPasswordForm.FPresenter := TAskPasswordPresenter.Create(AskPasswordForm);
		AskPasswordForm.FPresenter.InitializeActionMode(CustomTitle, CustomText, ActionsList);
		Result := AskPasswordForm.ShowModal;
	finally
		FreeAndNil(AskPasswordForm);
	end;
end;

class function TAskPasswordForm.AskPassword(CustomTitle, CustomText: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): integer;
var
	AskPasswordForm: TAskPasswordForm;
begin
	AskPasswordForm := TAskPasswordForm.Create(nil);
	try
		AskPasswordForm.ParentWindow := ParentWindow;
		AskPasswordForm.UpdateFormCaptions;
		AskPasswordForm.FPresenter := TAskPasswordPresenter.Create(AskPasswordForm);
		AskPasswordForm.FPresenter.InitializePasswordMode(CustomTitle, CustomText, UseTCPwdMngr, DisablePWDManagerCB);

		Result := AskPasswordForm.ShowModal;
		if Result = mrOk then
		begin
			Password := AskPasswordForm.FPresenter.GetPassword;
			UseTCPwdMngr := AskPasswordForm.FPresenter.GetUseTCPwdMngr;
		end;
	finally
		FreeAndNil(AskPasswordForm);
	end;
end;

class function TAskPasswordForm.AskText(CustomTitle, CustomText: WideString; var Text: WideString; ParentWindow: HWND): Boolean;
var
	AskPasswordForm: TAskPasswordForm;
begin
	Result := false;
	AskPasswordForm := TAskPasswordForm.Create(nil);
	try
		AskPasswordForm.ParentWindow := ParentWindow;
		AskPasswordForm.UpdateFormCaptions;
		AskPasswordForm.FNextButtonLeft := 7;
		AskPasswordForm.FPresenter := TAskPasswordPresenter.Create(AskPasswordForm);
		AskPasswordForm.FPresenter.InitializeTextMode(CustomTitle, CustomText);

		if mrOk = AskPasswordForm.ShowModal then
		begin
			Text := AskPasswordForm.FPresenter.GetPassword;
			Result := True;
		end;
	finally
		FreeAndNil(AskPasswordForm);
	end;
end;

procedure TAskPasswordForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	case Key of
		VK_ESCAPE:
			Close;
		VK_RETURN:
			if OkButton.Enabled then
				OkButton.Click;
	end;
end;

procedure TAskPasswordForm.FormShow(Sender: TObject);
begin
	if PasswordEdit.Visible then
		PasswordEdit.SetFocus;
end;

procedure TAskPasswordForm.PasswordEditChange(Sender: TObject);
begin
	FPresenter.OnPasswordChanged(PasswordEdit.Text);
end;

end.
