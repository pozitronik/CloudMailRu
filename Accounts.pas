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
		procedure UpdateAccountsList();
	private
		{ Private declarations }
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
	CASettings := GetAccountSettingsFromIniFile(IniPath, AccountsList.Items[AccountsList.ItemIndex]);
	AccountNameEdit.Text := CASettings.name;
	EmailEdit.Text := CASettings.email;
	PasswordEdit.Text := CASettings.password;
	UseTCPwdMngrCB.Checked := CASettings.use_tc_password_manager;

end;

procedure TAccountsForm.ApplyButtonClick(Sender: TObject);
var
	CASettings: TAccountSettings;
begin
	CASettings.name := AccountNameEdit.Text;
	CASettings.email := EmailEdit.Text;
	CASettings.password := PasswordEdit.Text;
	CASettings.use_tc_password_manager := UseTCPwdMngrCB.Checked;
	SetAccountSettingsToIniFile(IniPath, CASettings);

	UpdateAccountsList();

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

procedure TAccountsForm.UpdateAccountsList;
var
	TempList: TStrings;
begin
	TempList := TStringList.Create;
	GetAccountsListFromIniFile(IniPath, TempList);
	AccountsList.Items := TempList;
	TempList.Destroy;
end;

end.
