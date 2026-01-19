unit AccountRegistrationHandler;

{Handles account registration: shows dialog, saves password, persists settings.}

interface

uses
	Windows,
	AccountSettings,
	ConnectionSettings,
	IPasswordManagerInterface,
	IAccountsManagerInterface,
	IAccountRegistrationHandlerInterface;

type
	TAccountRegistrationHandler = class(TInterfacedObject, IAccountRegistrationHandler)
	private
		FAccountsManager: IAccountsManager;
		FPasswordManager: IPasswordManager;
	public
		constructor Create(AccountsManager: IAccountsManager; PasswordManager: IPasswordManager);

		function Execute(ParentWindow: HWND; const AccountName: WideString;
			const ConnSettings: TConnectionSettings;
			ShowDialog: TShowRegistrationFunc): Boolean;
	end;

implementation

uses
	Controls,
	PLUGIN_TYPES;

constructor TAccountRegistrationHandler.Create(AccountsManager: IAccountsManager;
	PasswordManager: IPasswordManager);
begin
	inherited Create;
	FAccountsManager := AccountsManager;
	FPasswordManager := PasswordManager;
end;

function TAccountRegistrationHandler.Execute(ParentWindow: HWND;
	const AccountName: WideString; const ConnSettings: TConnectionSettings;
	ShowDialog: TShowRegistrationFunc): Boolean;
var
	RegisteredAccount: TAccountSettings;
begin
	RegisteredAccount := FAccountsManager.GetAccountSettings(AccountName);

	Result := mrOk = ShowDialog(ParentWindow, ConnSettings, RegisteredAccount);
	if Result then
	begin
		if RegisteredAccount.UseTCPasswordManager then //просим TC сохранить пароль
			Result := FS_FILE_OK = FPasswordManager.SetPassword(AccountName, RegisteredAccount.Password);
		if Result then
			FAccountsManager.SetAccountSettings(AccountName, RegisteredAccount);
	end;
end;

end.
