unit AskPassword;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
	TAskPasswordForm = class(TForm)
		PasswordEditLabel: TLabel;
		PasswordEdit: TEdit;
		OkButton: TButton;
		UseTCPwdMngrCB: TCheckBox;
		class function AskPassword(ParentWindow: HWND; AccountName: WideString; var Password: WideString; var UseTCPwdMngr: Boolean): integer;
		procedure PasswordEditChange(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure PasswordEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	AskPasswordForm: TAskPasswordForm;

implementation

{$R *.dfm}

{ TAskPasswordForm }
class function TAskPasswordForm.AskPassword(ParentWindow: HWND; AccountName: WideString; var Password: WideString; var UseTCPwdMngr: Boolean): integer;
var
	AskPasswordForm: TAskPasswordForm;
begin
	try
		AskPasswordForm := TAskPasswordForm.Create(nil);
		AskPasswordForm.ParentWindow := ParentWindow;
		AskPasswordForm.Caption := AccountName + ' password';
		result := AskPasswordForm.ShowModal;
		if result = mrOk then
		begin
			Password := AskPasswordForm.PasswordEdit.Text;
			UseTCPwdMngr := AskPasswordForm.UseTCPwdMngrCB.Checked;
		end;
	finally
		FreeAndNil(AskPasswordForm);
	end;
end;

procedure TAskPasswordForm.FormShow(Sender: TObject);
begin
	(Sender as TAskPasswordForm).PasswordEdit.SetFocus;
end;

procedure TAskPasswordForm.PasswordEditChange(Sender: TObject);
begin
	OkButton.Enabled := PasswordEdit.Text <> '';

end;

procedure TAskPasswordForm.PasswordEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_ESCAPE then
	begin
		(PasswordEdit.Parent as TAskPasswordForm).Close;
	end;
	if (Key = VK_RETURN) and OkButton.Enabled then
	begin
		OkButton.Click;
	end;
end;

end.
