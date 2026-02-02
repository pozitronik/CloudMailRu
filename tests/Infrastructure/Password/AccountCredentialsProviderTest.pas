unit AccountCredentialsProviderTest;

interface

uses
	Windows,
	AccountCredentialsProvider,
	AccountSettings,
	PasswordManager,
	PasswordUIProvider,
	Logger,
	TCHandler,
	AccountsManager,
	WSList,
	System.Generics.Collections,
	DUnitX.TestFramework;

type
	{Mock password manager that tracks calls and returns configured results}
	TMockPasswordManager = class(TInterfacedObject, IPasswordManager)
	private
		FGetPasswordResult: Integer;
		FStoredPassword: WideString;
		FSetPasswordResult: Integer;
		FGetPasswordCalled: Boolean;
		FSetPasswordCalled: Boolean;
		FLastAccountName: WideString;
	public
		constructor Create;
		function GetPassword(Key: WideString; var Password: WideString): Integer;
		function SetPassword(Key, Password: WideString): Integer;
		property GetPasswordResult: Integer read FGetPasswordResult write FGetPasswordResult;
		property StoredPassword: WideString read FStoredPassword write FStoredPassword;
		property SetPasswordResult: Integer read FSetPasswordResult write FSetPasswordResult;
		property GetPasswordCalled: Boolean read FGetPasswordCalled;
		property SetPasswordCalled: Boolean read FSetPasswordCalled;
		property LastAccountName: WideString read FLastAccountName;
	end;

	{Mock password UI that tracks calls and returns configured results}
	TMockPasswordUI = class(TInterfacedObject, IPasswordUIProvider)
	private
		FAskPasswordResult: Integer;
		FProvidedPassword: WideString;
		FAskPasswordCalled: Boolean;
	public
		constructor Create;
		function AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
		function AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
		property AskPasswordResult: Integer read FAskPasswordResult write FAskPasswordResult;
		property ProvidedPassword: WideString read FProvidedPassword write FProvidedPassword;
		property AskPasswordCalled: Boolean read FAskPasswordCalled;
	end;

	{Mock accounts manager for testing password storage switch}
	TMockAccountsManagerForCredentials = class(TInterfacedObject, IAccountsManager)
	private
		FSwitchPasswordStorageCalled: Boolean;
		FLastSwitchedAccount: WideString;
	public
		constructor Create;
		function GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
		function GetAccountSettings(Account: WideString): TAccountSettings;
		procedure SetAccountSettings(Account: WideString; Settings: TAccountSettings); overload;
		procedure SetAccountSettings(Settings: TAccountSettings); overload;
		procedure DeleteAccount(Account: WideString);
		procedure RenameAccount(const OldName, NewName: WideString);
		procedure SwitchPasswordStorage(Account: WideString);
		procedure SetCryptedGUID(Account: WideString; GUID: WideString);
		property SwitchPasswordStorageCalled: Boolean read FSwitchPasswordStorageCalled;
		property LastSwitchedAccount: WideString read FLastSwitchedAccount;
	end;

	[TestFixture]
	TAccountCredentialsProviderTest = class
	public
		[Test]
		{Verifies GetPassword returns true when password already exists in settings}
		procedure TestGetPasswordSucceedsWhenPasswordExists;

		[Test]
		{Verifies GetPassword returns true when TC password manager provides password}
		procedure TestGetPasswordSucceedsFromTCPasswordManager;

		[Test]
		{Verifies GetPassword returns false when user cancels dialog}
		procedure TestGetPasswordFailsWhenUserCancels;

		[Test]
		{Verifies GetPassword returns true when user provides password via dialog}
		procedure TestGetPasswordSucceedsFromUserDialog;

		[Test]
		{Verifies password is stored in TC when user chooses that option}
		procedure TestGetPasswordStoresInTCWhenRequested;
	end;

	[TestFixture]
	TNullAccountCredentialsProviderTest = class
	public
		[Test]
		{Verifies null provider always returns false}
		procedure TestNullProviderReturnsFalse;
	end;

implementation

uses
	SysUtils,
	WFXTypes,
	Vcl.Controls,
	SettingsConstants;

{TMockPasswordManager}

constructor TMockPasswordManager.Create;
begin
	inherited Create;
	FGetPasswordResult := FS_FILE_READERROR; {Default: password not found}
	FSetPasswordResult := FS_FILE_OK;
	FStoredPassword := '';
	FGetPasswordCalled := False;
	FSetPasswordCalled := False;
end;

function TMockPasswordManager.GetPassword(Key: WideString; var Password: WideString): Integer;
begin
	FGetPasswordCalled := True;
	FLastAccountName := Key;
	Result := FGetPasswordResult;
	if Result = FS_FILE_OK then
		Password := FStoredPassword;
end;

function TMockPasswordManager.SetPassword(Key, Password: WideString): Integer;
begin
	FSetPasswordCalled := True;
	FLastAccountName := Key;
	FStoredPassword := Password;
	Result := FSetPasswordResult;
end;

{TMockPasswordUI}

constructor TMockPasswordUI.Create;
begin
	inherited Create;
	FAskPasswordResult := mrCancel; {Default: user cancels}
	FProvidedPassword := '';
	FAskPasswordCalled := False;
end;

function TMockPasswordUI.AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
begin
	FAskPasswordCalled := True;
	Result := FAskPasswordResult;
	if Result = mrOk then
		Password := FProvidedPassword;
end;

function TMockPasswordUI.AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
begin
	Result := mrCancel;
end;

{TMockAccountsManagerForCredentials}

constructor TMockAccountsManagerForCredentials.Create;
begin
	inherited Create;
	FSwitchPasswordStorageCalled := False;
	FLastSwitchedAccount := '';
end;

function TMockAccountsManagerForCredentials.GetAccountsList(const AccountTypes: EAccountType; const VirtualTypes: EVirtualType): TWSList;
begin
	Result.Clear;
end;

function TMockAccountsManagerForCredentials.GetAccountSettings(Account: WideString): TAccountSettings;
begin
	Result := Default(TAccountSettings);
end;

procedure TMockAccountsManagerForCredentials.SetAccountSettings(Account: WideString; Settings: TAccountSettings);
begin
end;

procedure TMockAccountsManagerForCredentials.SetAccountSettings(Settings: TAccountSettings);
begin
end;

procedure TMockAccountsManagerForCredentials.DeleteAccount(Account: WideString);
begin
end;

procedure TMockAccountsManagerForCredentials.RenameAccount(const OldName, NewName: WideString);
begin
end;

procedure TMockAccountsManagerForCredentials.SwitchPasswordStorage(Account: WideString);
begin
	FSwitchPasswordStorageCalled := True;
	FLastSwitchedAccount := Account;
end;

procedure TMockAccountsManagerForCredentials.SetCryptedGUID(Account: WideString; GUID: WideString);
begin
end;

{TAccountCredentialsProviderTest}

procedure TAccountCredentialsProviderTest.TestGetPasswordSucceedsWhenPasswordExists;
var
	Provider: IAccountCredentialsProvider;
	Settings: TAccountSettings;
begin
	Provider := TAccountCredentialsProvider.Create(
		TMockPasswordManager.Create,
		TMockPasswordUI.Create,
		TNullLogger.Create,
		TNullTCHandler.Create,
		TMockAccountsManagerForCredentials.Create);

	Settings := Default(TAccountSettings);
	Settings.Password := 'existing_password';
	Settings.UseTCPasswordManager := False;

	Assert.IsTrue(Provider.GetPassword('test_account', Settings),
		'GetPassword should succeed when password already exists');
	Assert.AreEqual('existing_password', Settings.Password,
		'Password should remain unchanged');
end;

procedure TAccountCredentialsProviderTest.TestGetPasswordSucceedsFromTCPasswordManager;
var
	Provider: IAccountCredentialsProvider;
	MockPwdMgr: TMockPasswordManager;
	Settings: TAccountSettings;
begin
	MockPwdMgr := TMockPasswordManager.Create;
	MockPwdMgr.GetPasswordResult := FS_FILE_OK;
	MockPwdMgr.StoredPassword := 'tc_stored_password';

	Provider := TAccountCredentialsProvider.Create(
		MockPwdMgr,
		TMockPasswordUI.Create,
		TNullLogger.Create,
		TNullTCHandler.Create,
		TMockAccountsManagerForCredentials.Create);

	Settings := Default(TAccountSettings);
	Settings.Password := '';
	Settings.UseTCPasswordManager := True;

	Assert.IsTrue(Provider.GetPassword('test_account', Settings),
		'GetPassword should succeed when TC password manager provides password');
	Assert.AreEqual('tc_stored_password', Settings.Password,
		'Password should be retrieved from TC password manager');
	Assert.IsTrue(MockPwdMgr.GetPasswordCalled, 'TC password manager should be called');
end;

procedure TAccountCredentialsProviderTest.TestGetPasswordFailsWhenUserCancels;
var
	Provider: IAccountCredentialsProvider;
	MockUI: TMockPasswordUI;
	Settings: TAccountSettings;
begin
	MockUI := TMockPasswordUI.Create;
	MockUI.AskPasswordResult := mrCancel;

	Provider := TAccountCredentialsProvider.Create(
		TMockPasswordManager.Create,
		MockUI,
		TNullLogger.Create,
		TNullTCHandler.Create,
		TMockAccountsManagerForCredentials.Create);

	Settings := Default(TAccountSettings);
	Settings.Password := '';
	Settings.UseTCPasswordManager := False;

	Assert.IsFalse(Provider.GetPassword('test_account', Settings),
		'GetPassword should fail when user cancels dialog');
	Assert.IsTrue(MockUI.AskPasswordCalled, 'Password dialog should be shown');
end;

procedure TAccountCredentialsProviderTest.TestGetPasswordSucceedsFromUserDialog;
var
	Provider: IAccountCredentialsProvider;
	MockUI: TMockPasswordUI;
	Settings: TAccountSettings;
begin
	MockUI := TMockPasswordUI.Create;
	MockUI.AskPasswordResult := mrOk;
	MockUI.ProvidedPassword := 'user_entered_password';

	Provider := TAccountCredentialsProvider.Create(
		TMockPasswordManager.Create,
		MockUI,
		TNullLogger.Create,
		TNullTCHandler.Create,
		TMockAccountsManagerForCredentials.Create);

	Settings := Default(TAccountSettings);
	Settings.Password := '';
	Settings.UseTCPasswordManager := False;

	Assert.IsTrue(Provider.GetPassword('test_account', Settings),
		'GetPassword should succeed when user provides password');
	Assert.AreEqual('user_entered_password', Settings.Password,
		'Password should be set from user input');
end;

procedure TAccountCredentialsProviderTest.TestGetPasswordStoresInTCWhenRequested;
var
	Provider: IAccountCredentialsProvider;
	MockUI: TMockPasswordUI;
	MockPwdMgr: TMockPasswordManager;
	MockAccMgr: TMockAccountsManagerForCredentials;
	Settings: TAccountSettings;
begin
	MockUI := TMockPasswordUI.Create;
	MockUI.AskPasswordResult := mrOk;
	MockUI.ProvidedPassword := 'new_password';

	MockPwdMgr := TMockPasswordManager.Create;
	MockPwdMgr.GetPasswordResult := FS_FILE_READERROR; {Password not found}
	MockPwdMgr.SetPasswordResult := FS_FILE_OK;

	MockAccMgr := TMockAccountsManagerForCredentials.Create;

	Provider := TAccountCredentialsProvider.Create(
		MockPwdMgr,
		MockUI,
		TNullLogger.Create,
		TNullTCHandler.Create,
		MockAccMgr);

	Settings := Default(TAccountSettings);
	Settings.Password := '';
	Settings.UseTCPasswordManager := True; {User wants to store in TC}

	Assert.IsTrue(Provider.GetPassword('test_account', Settings),
		'GetPassword should succeed');
	Assert.IsTrue(MockPwdMgr.SetPasswordCalled,
		'Password should be stored in TC password manager');
	Assert.IsTrue(MockAccMgr.SwitchPasswordStorageCalled,
		'Account settings should be switched to TC storage');
	Assert.AreEqual('test_account', MockAccMgr.LastSwitchedAccount,
		'Correct account should be switched');
end;

{TNullAccountCredentialsProviderTest}

procedure TNullAccountCredentialsProviderTest.TestNullProviderReturnsFalse;
var
	Provider: IAccountCredentialsProvider;
	Settings: TAccountSettings;
begin
	Provider := TNullAccountCredentialsProvider.Create;
	Settings := Default(TAccountSettings);

	Assert.IsFalse(Provider.GetPassword('any_account', Settings),
		'Null provider should always return false');
end;

initialization

TDUnitX.RegisterTestFixture(TAccountCredentialsProviderTest);
TDUnitX.RegisterTestFixture(TNullAccountCredentialsProviderTest);

end.
