unit AskPassword;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, MRC_Helper;

type
	TAskPasswordForm = class(TForm)
		PasswordEditLabel: TLabel;
		PasswordEdit: TEdit;
		OkButton: TButton;
		UseTCPwdMngrCB: TCheckBox;
		class function AskPassword(ParentWindow: HWND; AccountName: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean = false; HintLabelText: WideString = 'Enter account password:'): integer;
		procedure PasswordEditChange(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormActivate(Sender: TObject);
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
	private
		{Private declarations}
	public
		{Public declarations}
	end;

implementation

{$R *.dfm}

{TAskPasswordForm}
class function TAskPasswordForm.AskPassword(ParentWindow: HWND; AccountName: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean = false; HintLabelText: WideString = 'Enter account password:'): integer;
var
	AskPasswordForm: TAskPasswordForm;
begin
	try
		AskPasswordForm := TAskPasswordForm.Create(nil);
		AskPasswordForm.ParentWindow := ParentWindow;
		AskPasswordForm.PasswordEditLabel.Caption := HintLabelText;
		AskPasswordForm.Caption := AccountName + ' password';
		AskPasswordForm.UseTCPwdMngrCB.Enabled := not DisablePWDManagerCB;
		AskPasswordForm.UseTCPwdMngrCB.Checked := UseTCPwdMngr;

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

procedure TAskPasswordForm.FormActivate(Sender: TObject);
begin
	CenterWindow(Self.ParentWindow, Self.Handle);
end;

procedure TAskPasswordForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	begin
		case Key of
			VK_ESCAPE:
				Close;
			VK_RETURN:
				if OkButton.Enabled then
					OkButton.Click;
		end;
	end;
end;

procedure TAskPasswordForm.FormShow(Sender: TObject);
begin
	(Sender as TAskPasswordForm).PasswordEdit.SetFocus;
end;

procedure TAskPasswordForm.PasswordEditChange(Sender: TObject);
begin
	OkButton.Enabled := PasswordEdit.Text <> EmptyWideStr;
end;

end.
