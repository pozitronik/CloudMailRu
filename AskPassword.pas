unit AskPassword;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, MRC_Helper, System.Generics.Collections;

type

	TAskPasswordForm = class(TForm)
		PasswordEditLabel: TLabel;
		PasswordEdit: TEdit;
		OkButton: TButton;
		UseTCPwdMngrCB: TCheckBox;

		procedure PasswordEditChange(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormActivate(Sender: TObject);
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
	private
		{Private declarations}
	public
		{Public declarations}
		class function AskPassword(CustomTitle, CustomText: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean = false; ParentWindow: HWND = 0): integer;
		class function AskAction(CustomTitle, CustomText: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND = 0): integer;
		function AddButton(BtnTitle: WideString; BtnResultCode: integer; BtnLeft: integer): TButton;
	end;

implementation

{$R *.dfm}

{TAskPasswordForm}
function TAskPasswordForm.AddButton(BtnTitle: WideString; BtnResultCode, BtnLeft: integer): TButton;
begin
	Result := TButton.Create(Self);
	Result.Caption := BtnTitle;
	Result.top := 54;
	Result.Left := BtnLeft;
	Result.ModalResult := BtnResultCode;
	Result.Visible := True;
	Result.Width := Self.Canvas.TextWidth(BtnTitle) + 16;
	Result.Parent := Self;
end;

class function TAskPasswordForm.AskAction(CustomTitle, CustomText: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): integer;
var
	AskPasswordForm: TAskPasswordForm;
	ButtonCode, CurrentLeft: integer;
begin
	try
		AskPasswordForm := TAskPasswordForm.Create(nil);
		if (0 = ParentWindow) then
			AskPasswordForm.ParentWindow := FindTCWindow
		else
			AskPasswordForm.ParentWindow := ParentWindow;
		AskPasswordForm.PasswordEditLabel.Caption := CustomText;
		AskPasswordForm.Caption := CustomTitle;
		AskPasswordForm.UseTCPwdMngrCB.Visible := false;
		AskPasswordForm.PasswordEdit.Visible := false;
		AskPasswordForm.OkButton.Visible := false;
		CurrentLeft := 7;
		for ButtonCode in ActionsList.Keys do
		begin
			CurrentLeft := CurrentLeft + AskPasswordForm.AddButton(ActionsList.Items[ButtonCode], ButtonCode, CurrentLeft).Width + 7;
		end;
		Result := AskPasswordForm.ShowModal;

	finally
		FreeAndNil(AskPasswordForm);
	end;
end;

class function TAskPasswordForm.AskPassword(CustomTitle, CustomText: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean = false; ParentWindow: HWND = 0): integer;
var
	AskPasswordForm: TAskPasswordForm;
begin
	try
		AskPasswordForm := TAskPasswordForm.Create(nil);
		if (0 = ParentWindow) then
			AskPasswordForm.ParentWindow := FindTCWindow
		else
			AskPasswordForm.ParentWindow := ParentWindow;
		AskPasswordForm.PasswordEditLabel.Caption := CustomText;
		AskPasswordForm.Caption := CustomTitle;
		AskPasswordForm.UseTCPwdMngrCB.Enabled := not DisablePWDManagerCB;
		AskPasswordForm.UseTCPwdMngrCB.Checked := UseTCPwdMngr;

		Result := AskPasswordForm.ShowModal;
		if Result = mrOk then
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
	if (Sender as TAskPasswordForm).PasswordEdit.Visible then
		(Sender as TAskPasswordForm).PasswordEdit.SetFocus;
end;

procedure TAskPasswordForm.PasswordEditChange(Sender: TObject);
begin
	OkButton.Enabled := PasswordEdit.Text <> EmptyWideStr;
end;

end.
