unit Accounts;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IniFiles, MRC_Helper, PLUGIN_Types, Vcl.ComCtrls;

type
	TAccountsForm = class(TForm)
		OptionPages: TPageControl;
		AccountsTab: TTabSheet;
		GlobalTab: TTabSheet;
		AccountsGroupBox: TGroupBox;
		AccountsList: TListBox;
		ApplyButton: TButton;
		DeleteButton: TButton;
		UnlimitedFileSizeCB: TCheckBox;
		UseTCPwdMngrCB: TCheckBox;
		PasswordEdit: TEdit;
		PasswordLabel: TLabel;
		EmailEdit: TEdit;
		UsernameLabel: TLabel;
		AccountNameEdit: TEdit;
		AccountNameLabel: TLabel;
		PreserveFileTimeCB: TCheckBox;
		UseDLLFromPluginDir: TCheckBox;
		SplitLargeFilesCB: TCheckBox;
		ProxyGB: TGroupBox;
		ProxyTypeLabel: TLabel;
		ProxyCB: TComboBox;
		ProxyServerEdit: TEdit;
		ProxyDivLabel: TLabel;
		ProxyPortEdit: TEdit;
		ProxyPortLabel: TLabel;
		ProxyUserLabel: TLabel;
		ProxyUserEdit: TEdit;
		ProxyPWDLabel: TLabel;
		ProxyPWDEdit: TEdit;
		ProxyServerLabel: TLabel;
		DescriptionEnabledCB: TCheckBox;
		procedure FormShow(Sender: TObject);
		procedure AccountsListClick(Sender: TObject);
		procedure ApplyButtonClick(Sender: TObject);
		procedure UpdateAccountsList();
		procedure DeleteButtonClick(Sender: TObject);
		procedure AccountsListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		class procedure ShowAccounts(parentWindow: HWND; IniPath, SettingsIniFilePath: WideString; CryptProc: TCryptProcW; PluginNum, CryptoNum: Integer; RemoteName: WideString);
		procedure AccountNameEditChange(Sender: TObject);
		procedure EmailEditChange(Sender: TObject);
		procedure FormActivate(Sender: TObject);
		procedure UseDLLFromPluginDirClick(Sender: TObject);
		procedure ProxyCBChange(Sender: TObject);
		procedure ProxyServerEditChange(Sender: TObject);
		procedure ProxyPortEditChange(Sender: TObject);
		procedure ProxyUserEditChange(Sender: TObject);
		procedure ProxyPWDEditChange(Sender: TObject);
		procedure PreserveFileTimeCBClick(Sender: TObject);
		procedure DescriptionEnabledCBClick(Sender: TObject);
	private
		{ Private declarations }
		procedure WMHotKey(var Message: TMessage); message WM_HOTKEY;
	public
		{ Public declarations }
		IniPath: WideString;
		SettingsIniFilePath: WideString;
		CryptProc: TCryptProcW;
		PluginNum: Integer;
		CryptoNum: Integer;
		SelectedAccount: WideString;

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
		UnlimitedFileSizeCB.Checked := CASettings.unlimited_filesize;
		SplitLargeFilesCB.Checked := CASettings.split_large_files;
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
	CASettings.unlimited_filesize := UnlimitedFileSizeCB.Checked;
	CASettings.split_large_files := SplitLargeFilesCB.Checked;
	if CASettings.use_tc_password_manager then // просим TC сохранить пароль
	begin
		case self.CryptProc(self.PluginNum, self.CryptoNum, FS_CRYPT_SAVE_PASSWORD, PWideChar(CASettings.name), PWideChar(CASettings.password), SizeOf(CASettings.password)) of
			FS_FILE_OK:
				begin // TC скушал пароль
					CASettings.password := '';
				end;
			FS_FILE_NOTSUPPORTED: // нажали отмену на вводе мастер-пароля
				begin // просто выйдем
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

procedure TAccountsForm.DescriptionEnabledCBClick(Sender: TObject);
begin
	SetPluginSettingsValue(SettingsIniFilePath, 'DescriptionEnabled', DescriptionEnabledCB.Checked);
end;

procedure TAccountsForm.EmailEditChange(Sender: TObject);
begin
	ApplyButton.Enabled := (EmailEdit.Text <> '') and (AccountNameEdit.Text <> '');
end;

procedure TAccountsForm.FormActivate(Sender: TObject);
begin
	CenterWindow(self.parentWindow, self.Handle);
end;

procedure TAccountsForm.FormShow(Sender: TObject);
begin
	UpdateAccountsList();
	AccountsList.SetFocus;
	if AccountsList.Items.Count > 0 then
	begin
		if (self.SelectedAccount <> '') and (AccountsList.Items.IndexOf(self.SelectedAccount) <> -1) then
		begin
			AccountsList.Selected[AccountsList.Items.IndexOf(self.SelectedAccount)] := true;
		end else begin
			AccountsList.Selected[0] := true;

		end;
		AccountsList.OnClick(self);
	end;
end;

procedure TAccountsForm.PreserveFileTimeCBClick(Sender: TObject);
begin
	SetPluginSettingsValue(SettingsIniFilePath, 'PreserveFileTime', PreserveFileTimeCB.Checked);
end;

procedure TAccountsForm.ProxyCBChange(Sender: TObject);
begin
	SetPluginSettingsValue(SettingsIniFilePath, 'ProxyType', ProxyCB.ItemIndex);
end;

procedure TAccountsForm.ProxyPortEditChange(Sender: TObject);
begin
	SetPluginSettingsValue(SettingsIniFilePath, 'ProxyPort', ProxyPortEdit.Text);
end;

procedure TAccountsForm.ProxyPWDEditChange(Sender: TObject);
begin
	SetPluginSettingsValue(SettingsIniFilePath, 'ProxyPassword', ProxyPWDEdit.Text);
end;

procedure TAccountsForm.ProxyServerEditChange(Sender: TObject);
begin
	SetPluginSettingsValue(SettingsIniFilePath, 'ProxyServer', ProxyServerEdit.Text);
end;

procedure TAccountsForm.ProxyUserEditChange(Sender: TObject);
begin
	SetPluginSettingsValue(SettingsIniFilePath, 'ProxyUser', ProxyUserEdit.Text);
end;

class procedure TAccountsForm.ShowAccounts(parentWindow: HWND; IniPath, SettingsIniFilePath: WideString; CryptProc: TCryptProcW; PluginNum, CryptoNum: Integer; RemoteName: WideString);
var
	AccountsForm: TAccountsForm;
begin
	try
		AccountsForm := TAccountsForm.Create(nil);
		AccountsForm.parentWindow := parentWindow;
		AccountsForm.IniPath := IniPath;
		AccountsForm.SettingsIniFilePath := SettingsIniFilePath;
		AccountsForm.CryptProc := CryptProc;
		AccountsForm.PluginNum := PluginNum;
		AccountsForm.CryptoNum := CryptoNum;
		AccountsForm.SelectedAccount := '';
		{ global settings }
		AccountsForm.UseDLLFromPluginDir.Checked := GetPluginSettings(SettingsIniFilePath).LoadSSLDLLOnlyFromPluginDir;
		AccountsForm.PreserveFileTimeCB.Checked := GetPluginSettings(SettingsIniFilePath).PreserveFileTime;
		AccountsForm.DescriptionEnabledCB.Checked := GetPluginSettings(SettingsIniFilePath).DescriptionEnabled;
		AccountsForm.ProxyCB.ItemIndex := GetPluginSettings(SettingsIniFilePath).Proxy.ProxyType;
		AccountsForm.ProxyServerEdit.Text := GetPluginSettings(SettingsIniFilePath).Proxy.Server;
		AccountsForm.ProxyPortEdit.Text := GetPluginSettings(SettingsIniFilePath).Proxy.Port.ToString;
		AccountsForm.ProxyUserEdit.Text := GetPluginSettings(SettingsIniFilePath).Proxy.User;
		AccountsForm.ProxyPWDEdit.Text := GetPluginSettings(SettingsIniFilePath).Proxy.password;
		{ global settings }
		if RemoteName <> '' then AccountsForm.SelectedAccount := Copy(RemoteName, 2, length(RemoteName) - 1);
		RegisterHotKey(AccountsForm.Handle, 1, 0, VK_ESCAPE);
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

procedure TAccountsForm.UseDLLFromPluginDirClick(Sender: TObject);
begin
	SetPluginSettingsValue(SettingsIniFilePath, 'LoadSSLDLLOnlyFromPluginDir', UseDLLFromPluginDir.Checked);
end;

procedure TAccountsForm.WMHotKey(var Message: TMessage);
begin
	if Message.LParamHi = VK_ESCAPE then Close;
end;

end.
