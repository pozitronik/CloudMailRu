unit Accounts;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IniFiles, MRC_Helper, PLUGIN_Types;

type
	TAccountsForm = class(TForm)
		AccountsGroupBox: TGroupBox;
		AccountsList: TListBox;
		UsernameLabel: TLabel;
		EmailEdit: TEdit;
		AccountNameLabel: TLabel;
		AccountNameEdit: TEdit;
		PasswordEdit: TEdit;
		PasswordLabel: TLabel;
		UseTCPwdMngrCB: TCheckBox;
		ApplyButton: TButton;
		DeleteButton: TButton;
		procedure FormShow(Sender: TObject);
		procedure AccountsListClick(Sender: TObject);
		procedure ApplyButtonClick(Sender: TObject);
		procedure UpdateAccountsList();
		procedure DeleteButtonClick(Sender: TObject);
		procedure AccountsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		class procedure ShowAccounts(parentWindow: HWND; IniPath: WideString; CryptProc: TCryptProcW; PluginNum, CryptoNum: Integer);
		procedure AccountNameEditChange(Sender: TObject);
		procedure EmailEditChange(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
		IniPath: WideString;
		CryptProc: TCryptProcW;
		PluginNum: Integer;
		CryptoNum: Integer;

	end;

var
	AccountsForm: TAccountsForm;

implementation

{$R *.dfm}

procedure TAccountsForm.AccountNameEditChange(Sender: TObject);
begin
	if AccountsList.Items.IndexOf(AccountNameEdit.Text) = -1 then ApplyButton.Caption := 'Add'
	else ApplyButton.Caption := 'Save';
	ApplyButton.Enabled := (EmailEdit.Text <> '') and (AccountNameEdit.Text <> '');
end;

procedure TAccountsForm.AccountsListClick(Sender: TObject);
var
	CASettings: TAccountSettings;
begin
	if (AccountsList.Items.Count > 0) and (AccountsList.ItemIndex <> -1) then
	begin
		CASettings := GetAccountSettingsFromIniFile(IniPath, AccountsList.Items[AccountsList.ItemIndex]);
		AccountNameEdit.Text := CASettings.name;
		EmailEdit.Text := CASettings.email;
		PasswordEdit.Text := CASettings.password;
		UseTCPwdMngrCB.Checked := CASettings.use_tc_password_manager;

	end else begin
		AccountNameEdit.Text := '';
		EmailEdit.Text := '';
		PasswordEdit.Text := '';
		UseTCPwdMngrCB.Checked := false;
	end;

end;

procedure TAccountsForm.AccountsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_DELETE then DeleteButtonClick(nil);

end;

procedure TAccountsForm.ApplyButtonClick(Sender: TObject);
var
	CASettings: TAccountSettings;
begin
	CASettings.name := AccountNameEdit.Text;
	CASettings.email := EmailEdit.Text;
	CASettings.password := PasswordEdit.Text;
	CASettings.use_tc_password_manager := UseTCPwdMngrCB.Checked;
	if CASettings.use_tc_password_manager then // просим TC сохранить пароль
	begin
		case self.CryptProc(self.PluginNum, self.CryptoNum, FS_CRYPT_SAVE_PASSWORD, PWideChar(CASettings.name), PWideChar(CASettings.password), SizeOf(CASettings.password)) of
			FS_FILE_OK:
				begin // TC скушал пароль
					CASettings.password := '';
				end;
			FS_FILE_NOTSUPPORTED: // нажали отмену на вводе мастер-пароля
				begin //просто выйдем
					exit();
				end;
			FS_FILE_WRITEERROR: // Сохранение не получилось по другой причине. Сохранять не будем, выйдем
				begin
					exit();
				end;
		end;
	end;

	SetAccountSettingsToIniFile(IniPath, CASettings);

	UpdateAccountsList();

end;

procedure TAccountsForm.DeleteButtonClick(Sender: TObject);
begin
	if (AccountsList.Items.Count > 0) and (AccountsList.ItemIndex <> -1) then
	begin
		DeleteAccountFromIniFile(IniPath, AccountsList.Items[AccountsList.ItemIndex]);
		UpdateAccountsList();
	end;
end;

procedure TAccountsForm.EmailEditChange(Sender: TObject);
begin
	ApplyButton.Enabled := (EmailEdit.Text <> '') and (AccountNameEdit.Text <> '');
end;

procedure TAccountsForm.FormShow(Sender: TObject);
begin
	AccountsList.SetFocus;
	UpdateAccountsList();
	if AccountsList.Items.Count > 0 then
	begin
		AccountsList.Selected[0] := true;
		AccountsList.OnClick(self);
	end;

end;

class procedure TAccountsForm.ShowAccounts(parentWindow: HWND; IniPath: WideString; CryptProc: TCryptProcW; PluginNum, CryptoNum: Integer);
var
	AccountsForm: TAccountsForm;
begin
	try
		AccountsForm := TAccountsForm.Create(nil);
		AccountsForm.parentWindow := parentWindow;
		AccountsForm.IniPath := IniPath;
		AccountsForm.CryptProc := CryptProc;
		AccountsForm.PluginNum := PluginNum;
		AccountsForm.CryptoNum := CryptoNum;
		AccountsForm.ShowModal;
	finally
		FreeAndNil(AccountsForm);
	end;
end;

procedure TAccountsForm.UpdateAccountsList;
var
	TempList: TStringList;
begin
	TempList := TStringList.Create;
	GetAccountsListFromIniFile(IniPath, TempList);
	AccountsList.Items := TempList;
	TempList.Destroy;
	AccountsList.OnClick(self);
	ApplyButton.Enabled := (EmailEdit.Text <> '') and (AccountNameEdit.Text <> '');
end;

end.
