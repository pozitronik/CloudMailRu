unit AccountRegistrationHandler;

{Handles account registration in FsMkDir.
	Manages registration dialog, password manager, and account settings persistence.}

interface

uses
	Windows,
	AccountSettings,
	ConnectionSettings,
	TCPasswordManager,
	AccountsManager;

type
	{Callback type for showing registration dialog}
	TShowRegistrationFunc = reference to function(ParentWindow: HWND; ConnSettings: TConnectionSettings; var AccSettings: TAccountSettings): Integer;

	IAccountRegistrationHandler = interface
		['{B8C4D2E5-3F6A-7B8C-9D0E-1F2A3B4C5D6E}']

		{Registers a new account via dialog.
			@param ParentWindow Parent window handle for dialog
			@param AccountName The account name being registered
			@param ConnSettings Connection settings for registration
			@param ShowDialog Callback to show the registration dialog
			@return True if registration completed successfully}
		function Execute(ParentWindow: HWND; const AccountName: WideString; const ConnSettings: TConnectionSettings; ShowDialog: TShowRegistrationFunc): Boolean;
	end;

	TAccountRegistrationHandler = class(TInterfacedObject, IAccountRegistrationHandler)
	private
		FAccountsManager: IAccountsManager;
		FPasswordManager: IPasswordManager;
	public
		constructor Create(AccountsManager: IAccountsManager; PasswordManager: IPasswordManager);

		function Execute(ParentWindow: HWND; const AccountName: WideString; const ConnSettings: TConnectionSettings; ShowDialog: TShowRegistrationFunc): Boolean;
	end;

implementation

uses
	Controls,
	PLUGIN_TYPES;

constructor TAccountRegistrationHandler.Create(AccountsManager: IAccountsManager; PasswordManager: IPasswordManager);
begin
	inherited Create;
	FAccountsManager := AccountsManager;
	FPasswordManager := PasswordManager;
end;

function TAccountRegistrationHandler.Execute(ParentWindow: HWND; const AccountName: WideString; const ConnSettings: TConnectionSettings; ShowDialog: TShowRegistrationFunc): Boolean;
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
