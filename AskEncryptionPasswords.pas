unit AskEncryptionPasswords;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, MRC_Helper;

type
	TAskEncryptionPasswordsForm = class(TForm)
		OkButton: TButton;
		SkipEncryprionButton: TButton;
		EncryptFilesLabel: TLabel;
		EncryptFilesPasswordEdit: TEdit;
		EncryptFilenamesPasswordEdit: TEdit;
		EncryptFilenamesCB: TCheckBox;
		UseTCPwdMngrCB: TCheckBox;
		procedure FormActivate(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure EncryptFilesPasswordEditChange(Sender: TObject);
	private
		{Private declarations}
		procedure WMHotKey(var Message: TMessage); message WM_HOTKEY;
	public
		{Public declarations}
		class function AskPassword(ParentWindow: HWND; AccountName: WideString; var FilesPassword: WideString; var FileNamesPassword: WideString; var UseTCPwdMngr: Boolean): integer;
	end;

implementation

{$R *.dfm}
{TAskEncryptionPasswordsForm}

class function TAskEncryptionPasswordsForm.AskPassword(ParentWindow: HWND; AccountName: WideString; var FilesPassword, FileNamesPassword: WideString; var UseTCPwdMngr: Boolean): integer;
var
	AskEncryptionPasswordsForm: TAskEncryptionPasswordsForm;
begin
	try
		AskEncryptionPasswordsForm := TAskEncryptionPasswordsForm.Create(nil);
		AskEncryptionPasswordsForm.ParentWindow := ParentWindow;
		AskEncryptionPasswordsForm.Caption := AccountName + ' file encryption';
		AskEncryptionPasswordsForm.UseTCPwdMngrCB.Checked := UseTCPwdMngr;
		RegisterHotKey(AskEncryptionPasswordsForm.Handle, 1, 0, VK_ESCAPE);
		RegisterHotKey(AskEncryptionPasswordsForm.Handle, 2, 0, VK_RETURN);

		result := AskEncryptionPasswordsForm.ShowModal;
		if result = mrOk then
		begin
			FilesPassword := AskEncryptionPasswordsForm.EncryptFilesPasswordEdit.Text;
			if AskEncryptionPasswordsForm.EncryptFilenamesCB.Checked then
			begin
				FileNamesPassword := AskEncryptionPasswordsForm.EncryptFilenamesPasswordEdit.Text;
			end
			else
				FileNamesPassword := EmptyWideStr;

			UseTCPwdMngr := AskEncryptionPasswordsForm.UseTCPwdMngrCB.Checked;
		end;
	finally
		FreeAndNil(AskEncryptionPasswordsForm);
	end;
end;

procedure TAskEncryptionPasswordsForm.EncryptFilesPasswordEditChange(Sender: TObject);
begin
	OkButton.Enabled := EncryptFilesPasswordEdit.Text <> EmptyWideStr;
end;

procedure TAskEncryptionPasswordsForm.FormActivate(Sender: TObject);
begin
	CenterWindow(Self.ParentWindow, Self.Handle);
end;

procedure TAskEncryptionPasswordsForm.FormShow(Sender: TObject);
begin
	(Sender as TAskEncryptionPasswordsForm).EncryptFilesPasswordEdit.SetFocus;
end;

procedure TAskEncryptionPasswordsForm.WMHotKey(var Message: TMessage);
begin
	if Message.LParamHi = VK_ESCAPE then
		Close;
	if (Message.LParamHi = VK_RETURN) and OkButton.Enabled then
		OkButton.Click;
end;

end.
