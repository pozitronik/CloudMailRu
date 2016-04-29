unit Accounts;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IniFiles, MRC_Helper;

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
		procedure FormShow(Sender: TObject);
		procedure AccountsListClick(Sender: TObject);
		procedure ApplyButtonClick(Sender: TObject);
		procedure FormHide(Sender: TObject);
	private
		{ Private declarations }
		IniFile: TIniFile;

	public
		{ Public declarations }
		IniPath: WideString;

	end;

var
	AccountsForm: TAccountsForm;

implementation

{$R *.dfm}

procedure TAccountsForm.AccountsListClick(Sender: TObject);
var
	CASettings: TAccountSettings;
begin
	CASettings.name := AccountsList.Items[AccountsList.ItemIndex];
	CASettings.email := IniFile.ReadString(CASettings.name, 'email', '');
	CASettings.password := IniFile.ReadString(CASettings.name, 'password', '');
	CASettings.use_tc_password_manager := IniFile.ReadBool(CASettings.name, 'tc_pwd_mngr', false);

	AccountNameEdit.Text := CASettings.name;
	EmailEdit.Text := CASettings.email;
	PasswordEdit.Text := CASettings.password;
	UseTCPwdMngrCB.Checked := CASettings.use_tc_password_manager;

end;

procedure TAccountsForm.ApplyButtonClick(Sender: TObject);
begin
	IniFile.WriteString(AccountNameEdit.Text, 'email', EmailEdit.Text);
	IniFile.WriteString(AccountNameEdit.Text, 'password', PasswordEdit.Text);
	IniFile.WriteBool(AccountNameEdit.Text, 'tc_pwd_mngr', UseTCPwdMngrCB.Checked);
  IniFile.ReadSections(AccountsList.Items);
end;

procedure TAccountsForm.FormHide(Sender: TObject);
begin
	IniFile.Destroy;
end;

procedure TAccountsForm.FormShow(Sender: TObject);
begin
	IniFile := TIniFile.Create(IniPath);
	IniFile.ReadSections(AccountsList.Items);
end;

end.
