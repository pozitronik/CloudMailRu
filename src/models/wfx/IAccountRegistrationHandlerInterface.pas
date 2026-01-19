unit IAccountRegistrationHandlerInterface;

{Interface for handling account registration in FsMkDir.
 Manages registration dialog, password manager, and account settings persistence.}

interface

uses
	Windows,
	AccountSettings,
	ConnectionSettings,
	IPasswordManagerInterface,
	IAccountsManagerInterface;

type
	{Callback type for showing registration dialog}
	TShowRegistrationFunc = reference to function(ParentWindow: HWND;
		ConnSettings: TConnectionSettings; var AccSettings: TAccountSettings): Integer;

	IAccountRegistrationHandler = interface
		['{B8C4D2E5-3F6A-7B8C-9D0E-1F2A3B4C5D6E}']

		{Registers a new account via dialog.
		 @param ParentWindow Parent window handle for dialog
		 @param AccountName The account name being registered
		 @param ConnSettings Connection settings for registration
		 @param ShowDialog Callback to show the registration dialog
		 @return True if registration completed successfully}
		function Execute(ParentWindow: HWND; const AccountName: WideString;
			const ConnSettings: TConnectionSettings;
			ShowDialog: TShowRegistrationFunc): Boolean;
	end;

implementation

end.
