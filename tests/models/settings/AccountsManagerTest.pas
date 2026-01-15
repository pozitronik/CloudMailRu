unit AccountsManagerTest;

interface

uses
	AccountsManager,
	AccountSettings,
	TestHelper,
	SysUtils,
	WSList,
	FileCipher,
	SETTINGS_CONSTANTS,
	DUnitX.TestFramework;

type

	[TestFixture]
	TAccountsManagerTest = class
		AppDir: WideString; //the current test binary directory
	private const
		FP_ACCOUNTS_INI = 'Accounts.ini';
	public
		[Setup]
		procedure Setup;
		[Test]
		procedure TestGetAccountSettings;
		[Test]
		procedure TestSetAccountSettings;
		[Test]
		procedure TestSetCryptedGUID;
		[Test]
		procedure TestClearPassword;
		[Test]
		procedure TestDeleteAccount;
		[Test]
		procedure TestListAccounts;
		[Test]
		procedure TestListAccountsEmpty;

	end;

implementation

{TAccountSettingsTest}

procedure TAccountsManagerTest.Setup;
begin
	AppDir := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(hInstance)));

	{cleans the previous run artefacts}
	if FileExists(self.AppDir + FP_ACCOUNTS_INI) then
		DeleteFile(self.AppDir + FP_ACCOUNTS_INI);
end;

procedure TAccountsManagerTest.TestClearPassword;
var
	TestAccountsManager: TAccountsManager;
	TestAccountSettings: TAccountSettings;
begin
	TestAccountsManager := TAccountsManager.Create(self.AppDir + FP_ACCOUNTS_INI); //Uses a new file in the test exe dir

	TestAccountSettings.Password := 'cjhjrnsczxj,tpmzyd;jgeceyekb,fyfy';
	TestAccountsManager.SetAccountSettings('NEW_ACCOUNT', TestAccountSettings);
	TestAccountsManager.Free;

	TestAccountsManager := TAccountsManager.Create(self.AppDir + FP_ACCOUNTS_INI);
	Assert.IsNotEmpty(TestAccountsManager.GetAccountSettings('NEW_ACCOUNT').Password);
	TestAccountsManager.SwitchPasswordStorage('NEW_ACCOUNT');
	Assert.IsEmpty(TestAccountsManager.GetAccountSettings('NEW_ACCOUNT').Password);
	TestAccountsManager.Free;
end;

procedure TAccountsManagerTest.TestDeleteAccount;
var
	TestAccountsManager: TAccountsManager;
	TestAccountSettings: TAccountSettings;
begin
	TestAccountsManager := TAccountsManager.Create(self.AppDir + FP_ACCOUNTS_INI); //Uses a new file in the test exe dir

	TestAccountSettings.Email := 'deleted_account@mail.ru';
	TestAccountsManager.SetAccountSettings('NEW_ACCOUNT', TestAccountSettings);
	TestAccountsManager.Free;

	TestAccountSettings.Email := EmptyWideStr;

	TestAccountsManager := TAccountsManager.Create(self.AppDir + FP_ACCOUNTS_INI);
	TestAccountSettings := TestAccountsManager.GetAccountSettings('NEW_ACCOUNT');
	Assert.AreEqual('deleted_account@mail.ru', TestAccountSettings.Email);
	TestAccountsManager.DeleteAccount('NEW_ACCOUNT');
	TestAccountsManager.Free;

	TestAccountsManager := TAccountsManager.Create(self.AppDir + FP_ACCOUNTS_INI);
	Assert.IsEmpty(TestAccountsManager.GetAccountSettings('NEW_ACCOUNT').Email);
	TestAccountsManager.Free;
end;

procedure TAccountsManagerTest.TestGetAccountSettings;
var
	TestAccountsManager: TAccountsManager;
	TestAccountSettings: TAccountSettings;
begin
	TestAccountsManager := TAccountsManager.Create(DataPath(FP_ACCOUNTS_INI)); //Uses a test file
	TestAccountSettings := TestAccountsManager.GetAccountSettings('TEST_ACCOUNT_TWO');

	Assert.AreEqual('test_fake_email_two@mail.ru', TestAccountSettings.Email);
	Assert.IsTrue(TestAccountSettings.UseTCPasswordManager);
	Assert.AreEqual('https://cloclo44.datacloudmail.ru/get/', TestAccountSettings.ShardOverride);
	Assert.AreEqual('1MXM1IaThcWdCXnOJuCJMMWHr1wzLJMYTpsMEdJixqomW0F6C7UaOqAJKFTNMzgKvKIlB8KoWZBRWraXhU9mwF+6tIKTRr0l', TestAccountSettings.CryptedGUIDFiles);

	TestAccountsManager.Free;
end;

procedure TAccountsManagerTest.TestListAccounts;
var
	TestAccountsManager: TAccountsManager;
	AccountsList: TWSList;
begin
	AccountsList := TWSList.Create();
	TestAccountsManager := TAccountsManager.Create(DataPath(FP_ACCOUNTS_INI)); //Uses a test file

	AccountsList := TestAccountsManager.GetAccountsList();
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_ONE'));
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_TWO'));
	Assert.IsFalse(AccountsList.Contains('TEST_ACCOUNT_THREE'));
	Assert.IsTrue(AccountsList.Contains('TEST_PUBLIC_ACCOUNT'));

	AccountsList := TestAccountsManager.GetAccountsList([ATPrivate]);
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_ONE'));
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_TWO'));
	Assert.IsFalse(AccountsList.Contains('TEST_ACCOUNT_THREE'));
	Assert.IsFalse(AccountsList.Contains('TEST_PUBLIC_ACCOUNT'));

	AccountsList := TestAccountsManager.GetAccountsList([ATPublic]);
	Assert.IsFalse(AccountsList.Contains('TEST_ACCOUNT_ONE'));
	Assert.IsFalse(AccountsList.Contains('TEST_ACCOUNT_TWO'));
	Assert.IsFalse(AccountsList.Contains('TEST_ACCOUNT_THREE'));
	Assert.IsTrue(AccountsList.Contains('TEST_PUBLIC_ACCOUNT'));

	AccountsList := TestAccountsManager.GetAccountsList([ATPrivate, ATPublic], [VTTrash, VTInvites]);
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_ONE'));
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_ONE' + TrashPostfix));
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_ONE' + InvitesPostfix));
	Assert.IsFalse(AccountsList.Contains('TEST_ACCOUNT_ONE' + SharedPostfix));
	Assert.IsTrue(AccountsList.Contains('TEST_PUBLIC_ACCOUNT'));
	Assert.IsFalse(AccountsList.Contains('TEST_PUBLIC_ACCOUNT' + TrashPostfix));
	Assert.IsFalse(AccountsList.Contains('TEST_PUBLIC_ACCOUNT' + InvitesPostfix));
	Assert.IsFalse(AccountsList.Contains('TEST_PUBLIC_ACCOUNT' + SharedPostfix));

	TestAccountsManager.Free;
end;

procedure TAccountsManagerTest.TestListAccountsEmpty;
var
	TestAccountsManager: TAccountsManager;
	AccountsList: TWSList;
begin
	AccountsList := TWSList.Create();
	TestAccountsManager := TAccountsManager.Create(self.AppDir + FP_ACCOUNTS_INI); //Uses a new file in the test exe dir
	AccountsList := TestAccountsManager.GetAccountsList();

	Assert.IsTrue(AccountsList.Count = 0);

	TestAccountsManager.Free;
end;

procedure TAccountsManagerTest.TestSetAccountSettings;
var
	TestAccountsManager: TAccountsManager;
	TestAccountSettings: TAccountSettings;
	TestAccountSettingsNew: TAccountSettings;
begin
	TestAccountsManager := TAccountsManager.Create(DataPath(FP_ACCOUNTS_INI)); //Uses a test file
	TestAccountSettings := TestAccountsManager.GetAccountSettings('TEST_ACCOUNT_TWO');
	TestAccountsManager.Free;

	TestAccountsManager := TAccountsManager.Create(self.AppDir + FP_ACCOUNTS_INI); //Uses a new file in the test exe dir
	TestAccountsManager.SetAccountSettings('NEW_ACCOUNT', TestAccountSettings);
	TestAccountsManager.Free;

	TestAccountsManager := TAccountsManager.Create(self.AppDir + FP_ACCOUNTS_INI);
	TestAccountSettingsNew := TestAccountsManager.GetAccountSettings('NEW_ACCOUNT');
	TestAccountsManager.Free;

	Assert.AreEqual(TestAccountSettings.Email, TestAccountSettingsNew.Email);
	Assert.AreEqual(TestAccountSettings.TwostepAuth, TestAccountSettingsNew.TwostepAuth);
	Assert.AreEqual(TestAccountSettings.UseTCPasswordManager, TestAccountSettingsNew.UseTCPasswordManager);
	Assert.AreEqual('', TestAccountSettingsNew.CryptedGUIDFiles);
end;

procedure TAccountsManagerTest.TestSetCryptedGUID;
var
	TestAccountsManager: TAccountsManager;
begin
	TestAccountsManager := TAccountsManager.Create(self.AppDir + FP_ACCOUNTS_INI); //Uses a new file in the test exe dir
	Assert.IsEmpty(TestAccountsManager.GetAccountSettings('NEW_ACCOUNT').CryptedGUIDFiles);
	TestAccountsManager.SetCryptedGUID('NEW_ACCOUNT', TFileCipher.GetCryptedGUID('cjhjrnsczxj,tpmzyd;jgeceyekb,fyfy'));

	Assert.AreEqual(TFileCipher.GetCryptedGUID('cjhjrnsczxj,tpmzyd;jgeceyekb,fyfy'), TestAccountsManager.GetAccountSettings('NEW_ACCOUNT').CryptedGUIDFiles);
	TestAccountsManager.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TAccountsManagerTest);

end.
