unit AccountRegistrationHandlerTest;

{Unit tests for TAccountRegistrationHandler.}

interface

uses
	DUnitX.TestFramework,
	Windows,
	AccountSettings,
	ConnectionSettings,
	TCPasswordManager,
	AccountsManager,
	AccountRegistrationHandler,
	WSList;

type
	{Mock accounts manager for testing}
	TMockRegAccountsManager = class(TInterfacedObject, IAccountsManager)
	private
		FStoredAccount: TAccountSettings;
		FSetAccountCalled: Boolean;
	public
		constructor Create;

		function GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
		function GetAccountSettings(Account: WideString): TAccountSettings;
		procedure SetAccountSettings(Account: WideString; Settings: TAccountSettings); overload;
		procedure SetAccountSettings(Settings: TAccountSettings); overload;
		procedure DeleteAccount(Account: WideString);
		procedure SwitchPasswordStorage(Account: WideString);
		procedure SetCryptedGUID(Account: WideString; GUID: WideString);

		property SetAccountCalled: Boolean read FSetAccountCalled;
		property StoredAccount: TAccountSettings read FStoredAccount;
	end;

	{Mock password manager for testing}
	TMockRegPasswordManager = class(TInterfacedObject, IPasswordManager)
	private
		FSetPasswordResult: Integer;
		FSetPasswordCalled: Boolean;
		FLastAccountName: WideString;
	public
		constructor Create(SetPasswordResult: Integer = 0);

		function GetPassword(AccountName: WideString; var Password: WideString): Integer;
		function SetPassword(AccountName: WideString; Password: WideString): Integer;
		function DeletePassword(AccountName: WideString): Integer;

		property SetPasswordCalled: Boolean read FSetPasswordCalled;
		property LastAccountName: WideString read FLastAccountName;
	end;

	[TestFixture]
	TAccountRegistrationHandlerTest = class
	private
		FHandler: IAccountRegistrationHandler;
		FAccountsManager: TMockRegAccountsManager;
		FPasswordManager: TMockRegPasswordManager;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Dialog cancelled tests}
		[Test]
		procedure TestExecute_DialogCancelled_ReturnsFalse;
		[Test]
		procedure TestExecute_DialogCancelled_DoesNotSaveSettings;

		{Dialog confirmed tests}
		[Test]
		procedure TestExecute_DialogConfirmed_ReturnsTrue;
		[Test]
		procedure TestExecute_DialogConfirmed_SavesSettings;

		{Password manager tests}
		[Test]
		procedure TestExecute_UseTCPasswordManager_SavesPassword;
		[Test]
		procedure TestExecute_PasswordManagerFails_ReturnsFalse;
		[Test]
		procedure TestExecute_NoTCPasswordManager_SkipsPasswordSave;
	end;

implementation

uses
	SysUtils,
	Controls,
	PLUGIN_TYPES;

{TMockRegAccountsManager}

constructor TMockRegAccountsManager.Create;
begin
	inherited Create;
	FSetAccountCalled := False;
	FillChar(FStoredAccount, SizeOf(FStoredAccount), 0);
end;

function TMockRegAccountsManager.GetAccountsList(const AccountTypes: EAccountType; const VirtualTypes: EVirtualType): TWSList;
begin
	Result.Clear;
end;

function TMockRegAccountsManager.GetAccountSettings(Account: WideString): TAccountSettings;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.Account := Account;
	Result.Email := Account + '@mail.ru';
end;

procedure TMockRegAccountsManager.SetAccountSettings(Account: WideString; Settings: TAccountSettings);
begin
	FSetAccountCalled := True;
	FStoredAccount := Settings;
end;

procedure TMockRegAccountsManager.SetAccountSettings(Settings: TAccountSettings);
begin
	FSetAccountCalled := True;
	FStoredAccount := Settings;
end;

procedure TMockRegAccountsManager.DeleteAccount(Account: WideString);
begin
	{No-op for test}
end;

procedure TMockRegAccountsManager.SwitchPasswordStorage(Account: WideString);
begin
	{No-op for test}
end;

procedure TMockRegAccountsManager.SetCryptedGUID(Account: WideString; GUID: WideString);
begin
	{No-op for test}
end;

{TMockRegPasswordManager}

constructor TMockRegPasswordManager.Create(SetPasswordResult: Integer);
begin
	inherited Create;
	FSetPasswordResult := SetPasswordResult;
	FSetPasswordCalled := False;
end;

function TMockRegPasswordManager.GetPassword(AccountName: WideString; var Password: WideString): Integer;
begin
	Result := FS_FILE_OK;
	Password := 'test';
end;

function TMockRegPasswordManager.SetPassword(AccountName: WideString; Password: WideString): Integer;
begin
	FSetPasswordCalled := True;
	FLastAccountName := AccountName;
	Result := FSetPasswordResult;
end;

function TMockRegPasswordManager.DeletePassword(AccountName: WideString): Integer;
begin
	Result := FS_FILE_OK;
end;

{TAccountRegistrationHandlerTest}

procedure TAccountRegistrationHandlerTest.Setup;
begin
	FAccountsManager := TMockRegAccountsManager.Create;
	FPasswordManager := TMockRegPasswordManager.Create;
	FHandler := TAccountRegistrationHandler.Create(FAccountsManager, FPasswordManager);
end;

procedure TAccountRegistrationHandlerTest.TearDown;
begin
	FHandler := nil;
	FPasswordManager := nil;
	FAccountsManager := nil;
end;

{Dialog cancelled tests}

procedure TAccountRegistrationHandlerTest.TestExecute_DialogCancelled_ReturnsFalse;
var
	Result: Boolean;
begin
	Result := FHandler.Execute(0, 'testaccount', Default(TConnectionSettings),
		function(ParentWindow: HWND; ConnSettings: TConnectionSettings;
			var AccSettings: TAccountSettings): Integer
		begin
			Result := mrCancel;
		end);

	Assert.IsFalse(Result, 'Should return False when dialog is cancelled');
end;

procedure TAccountRegistrationHandlerTest.TestExecute_DialogCancelled_DoesNotSaveSettings;
begin
	FHandler.Execute(0, 'testaccount', Default(TConnectionSettings),
		function(ParentWindow: HWND; ConnSettings: TConnectionSettings;
			var AccSettings: TAccountSettings): Integer
		begin
			Result := mrCancel;
		end);

	Assert.IsFalse(FAccountsManager.SetAccountCalled, 'Should not save settings when cancelled');
end;

{Dialog confirmed tests}

procedure TAccountRegistrationHandlerTest.TestExecute_DialogConfirmed_ReturnsTrue;
var
	Result: Boolean;
begin
	Result := FHandler.Execute(0, 'testaccount', Default(TConnectionSettings),
		function(ParentWindow: HWND; ConnSettings: TConnectionSettings;
			var AccSettings: TAccountSettings): Integer
		begin
			AccSettings.UseTCPasswordManager := False;
			Result := mrOk;
		end);

	Assert.IsTrue(Result, 'Should return True when dialog confirmed');
end;

procedure TAccountRegistrationHandlerTest.TestExecute_DialogConfirmed_SavesSettings;
begin
	FHandler.Execute(0, 'testaccount', Default(TConnectionSettings),
		function(ParentWindow: HWND; ConnSettings: TConnectionSettings;
			var AccSettings: TAccountSettings): Integer
		begin
			AccSettings.UseTCPasswordManager := False;
			AccSettings.Email := 'modified@mail.ru';
			Result := mrOk;
		end);

	Assert.IsTrue(FAccountsManager.SetAccountCalled, 'Should save settings when confirmed');
	Assert.AreEqual('modified@mail.ru', FAccountsManager.StoredAccount.Email);
end;

{Password manager tests}

procedure TAccountRegistrationHandlerTest.TestExecute_UseTCPasswordManager_SavesPassword;
begin
	FHandler.Execute(0, 'testaccount', Default(TConnectionSettings),
		function(ParentWindow: HWND; ConnSettings: TConnectionSettings;
			var AccSettings: TAccountSettings): Integer
		begin
			AccSettings.UseTCPasswordManager := True;
			AccSettings.Password := 'secret123';
			Result := mrOk;
		end);

	Assert.IsTrue(FPasswordManager.SetPasswordCalled, 'Should save password when UseTCPasswordManager is True');
	Assert.AreEqual('testaccount', FPasswordManager.LastAccountName);
end;

procedure TAccountRegistrationHandlerTest.TestExecute_PasswordManagerFails_ReturnsFalse;
var
	Result: Boolean;
	FailingPasswordManager: TMockRegPasswordManager;
	Handler: IAccountRegistrationHandler;
begin
	FailingPasswordManager := TMockRegPasswordManager.Create(FS_FILE_WRITEERROR);
	Handler := TAccountRegistrationHandler.Create(FAccountsManager, FailingPasswordManager);

	Result := Handler.Execute(0, 'testaccount', Default(TConnectionSettings),
		function(ParentWindow: HWND; ConnSettings: TConnectionSettings;
			var AccSettings: TAccountSettings): Integer
		begin
			AccSettings.UseTCPasswordManager := True;
			Result := mrOk;
		end);

	Assert.IsFalse(Result, 'Should return False when password manager fails');
	Assert.IsFalse(FAccountsManager.SetAccountCalled, 'Should not save settings when password save fails');
end;

procedure TAccountRegistrationHandlerTest.TestExecute_NoTCPasswordManager_SkipsPasswordSave;
begin
	FHandler.Execute(0, 'testaccount', Default(TConnectionSettings),
		function(ParentWindow: HWND; ConnSettings: TConnectionSettings;
			var AccSettings: TAccountSettings): Integer
		begin
			AccSettings.UseTCPasswordManager := False;
			Result := mrOk;
		end);

	Assert.IsFalse(FPasswordManager.SetPasswordCalled, 'Should not save password when UseTCPasswordManager is False');
	Assert.IsTrue(FAccountsManager.SetAccountCalled, 'Should still save settings');
end;

initialization
	TDUnitX.RegisterTestFixture(TAccountRegistrationHandlerTest);

end.
