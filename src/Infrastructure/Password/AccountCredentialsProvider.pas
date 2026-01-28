unit AccountCredentialsProvider;

{Account password retrieval service.
	Extracts password retrieval logic from ConnectionManager for better separation of concerns.
	Handles TC password storage, config file passwords, and user dialog.}

interface

uses
	AccountSettings,
	TCPasswordManager,
	PasswordUIProvider,
	TCLogger,
	TCHandler,
	AccountsManager;

type
	IAccountCredentialsProvider = interface
		['{8E7F3A2B-4C5D-6E8F-9A1B-2C3D4E5F6A7B}']

		{Retrieves password for account from TC storage, config, or user dialog.
			Handles storing password in TC if user chooses.
			@param AccountName Account identifier for TC storage key and dialog display
			@param Settings Account settings - password field will be updated
			@return True if password obtained, false if user cancelled}
		function GetPassword(const AccountName: WideString; var Settings: TAccountSettings): Boolean;
	end;

	TAccountCredentialsProvider = class(TInterfacedObject, IAccountCredentialsProvider)
	private
		FPasswordManager: IPasswordManager;
		FPasswordUI: IPasswordUIProvider;
		FLogger: ILogger;
		FTCHandler: ITCHandler;
		FAccountsManager: IAccountsManager;
	public
		constructor Create(PasswordManager: IPasswordManager; PasswordUI: IPasswordUIProvider; Logger: ILogger; TCHandler: ITCHandler; AccountsManager: IAccountsManager);
		function GetPassword(const AccountName: WideString; var Settings: TAccountSettings): Boolean;
	end;

	{Null implementation for testing - always returns false (simulates cancelled dialog)}
	TNullAccountCredentialsProvider = class(TInterfacedObject, IAccountCredentialsProvider)
	public
		function GetPassword(const AccountName: WideString; var Settings: TAccountSettings): Boolean;
	end;

implementation

uses
	SysUtils,
	WFXTypes,
	Vcl.Controls,
	CloudConstants,
	LanguageStrings,
	SettingsConstants;

constructor TAccountCredentialsProvider.Create(PasswordManager: IPasswordManager; PasswordUI: IPasswordUIProvider; Logger: ILogger; TCHandler: ITCHandler; AccountsManager: IAccountsManager);
begin
	inherited Create;
	FPasswordManager := PasswordManager;
	FPasswordUI := PasswordUI;
	FLogger := Logger;
	FTCHandler := TCHandler;
	FAccountsManager := AccountsManager;
end;

function TAccountCredentialsProvider.GetPassword(const AccountName: WideString; var Settings: TAccountSettings): Boolean;
begin
	{Try TC password manager first if enabled}
	if Settings.UseTCPasswordManager and (FPasswordManager.GetPassword(AccountName, Settings.password) = FS_FILE_OK) then
		exit(True);

	{If password is empty, ask user}
	if Settings.password = EmptyWideStr then
	begin
		if mrOk <> FPasswordUI.AskPassword(Format(ASK_PASSWORD, [AccountName]), PREFIX_ASK_PASSWORD, Settings.password, Settings.UseTCPasswordManager, False, FTCHandler.FindTCWindow) then
			exit(False);

		{Store in TC if user chose that option}
		if Settings.UseTCPasswordManager then
		begin
			if FS_FILE_OK = FPasswordManager.SetPassword(AccountName, Settings.password) then
			begin
				FLogger.Log(LOG_LEVEL_DEBUG, msgtype_details, PASSWORD_SAVED, [AccountName]);
				FAccountsManager.SwitchPasswordStorage(AccountName);
			end;
		end;
	end;

	Result := True;
end;

{TNullAccountCredentialsProvider}

function TNullAccountCredentialsProvider.GetPassword(const AccountName: WideString; var Settings: TAccountSettings): Boolean;
begin
	Result := False; {Simulates user cancelling the password dialog}
end;

end.
