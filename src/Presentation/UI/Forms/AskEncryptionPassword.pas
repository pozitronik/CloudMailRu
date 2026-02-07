unit AskEncryptionPassword;

{Dedicated encryption password dialog. Unlike AskPassword, this dialog has no close
 button (X) and provides an explicit "No encryption this time" skip button.
 This makes the consequence of skipping encryption clearly visible to the user.}

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
	AskEncryptionPasswordPresenter,
	LanguageStrings;

type

	TAskEncryptionPasswordForm = class(TPluginForm, IAskEncryptionPasswordView)
		PasswordEditLabel: TLabel;
		PasswordEdit: TEdit;
		OkButton: TButton;
		SkipButton: TButton;

		procedure PasswordEditChange(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
	protected
		FPresenter: TAskEncryptionPasswordPresenter;

		{IAskEncryptionPasswordView implementation}
		procedure SetCaption(const Value: WideString);
		procedure SetLabelText(const Value: WideString);
		procedure SetPasswordChar(Value: Char);
		function GetPassword: WideString;
		procedure SetOkButtonEnabled(Value: Boolean);
		procedure SetSkipButtonCaption(const Value: WideString);
		procedure UpdateFormCaptions;
	public
		destructor Destroy; override;
		class function AskEncryptionPassword(Title, Text: WideString; var Password: WideString; ParentWindow: HWND): Integer;
	end;

implementation

{$R *.dfm}

{TAskEncryptionPasswordForm - IAskEncryptionPasswordView implementation}

procedure TAskEncryptionPasswordForm.SetCaption(const Value: WideString);
begin
	Self.Caption := Value;
end;

procedure TAskEncryptionPasswordForm.SetLabelText(const Value: WideString);
begin
	PasswordEditLabel.Caption := Value;
end;

procedure TAskEncryptionPasswordForm.SetPasswordChar(Value: Char);
begin
	PasswordEdit.PasswordChar := Value;
end;

function TAskEncryptionPasswordForm.GetPassword: WideString;
begin
	Result := PasswordEdit.Text;
end;

procedure TAskEncryptionPasswordForm.SetOkButtonEnabled(Value: Boolean);
begin
	OkButton.Enabled := Value;
end;

procedure TAskEncryptionPasswordForm.SetSkipButtonCaption(const Value: WideString);
begin
	SkipButton.Caption := Value;
end;

procedure TAskEncryptionPasswordForm.UpdateFormCaptions;
begin
	OkButton.Caption := DFM_ASK_BTN_OK;
	SkipButton.Caption := DFM_ASKENC_BTN_SKIP;
end;

{TAskEncryptionPasswordForm}

destructor TAskEncryptionPasswordForm.Destroy;
begin
	FreeAndNil(FPresenter);
	inherited;
end;

class function TAskEncryptionPasswordForm.AskEncryptionPassword(Title, Text: WideString; var Password: WideString; ParentWindow: HWND): Integer;
var
	Form: TAskEncryptionPasswordForm;
begin
	Form := TAskEncryptionPasswordForm.Create(nil);
	try
		Form.ParentWindow := ParentWindow;
		Form.UpdateFormCaptions;
		Form.FPresenter := TAskEncryptionPasswordPresenter.Create(Form);
		Form.FPresenter.Initialize(Title, Text);

		Result := Form.ShowModal;
		if Result = mrOk then
			Password := Form.FPresenter.GetPassword;
	finally
		FreeAndNil(Form);
	end;
end;

{Prevent closing via Alt+F4 -- only allow close through button clicks}
procedure TAskEncryptionPasswordForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	CanClose := (ModalResult <> mrNone);
end;

procedure TAskEncryptionPasswordForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	{VK_RETURN triggers OK if enabled; VK_ESCAPE does nothing (no close button)}
	if (Key = VK_RETURN) and OkButton.Enabled then
		OkButton.Click;
end;

procedure TAskEncryptionPasswordForm.FormShow(Sender: TObject);
begin
	PasswordEdit.SetFocus;
end;

procedure TAskEncryptionPasswordForm.PasswordEditChange(Sender: TObject);
begin
	FPresenter.OnPasswordChanged(PasswordEdit.Text);
end;

end.
