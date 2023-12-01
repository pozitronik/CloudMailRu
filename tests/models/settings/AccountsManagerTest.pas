unit AccountsManagerTest;

interface

uses
	AccountsManager,
	AccountSettings,
	TestHelper,
	SysUtils,
	WSList,
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

initialization

TDUnitX.RegisterTestFixture(TAccountsManagerTest);

end.
