unit AccountSettingsTest;

interface

uses
	NewAccountSettings,
	TestHelper,
	SysUtils,
	WSList,
	SETTINGS_CONSTANTS,
	DUnitX.TestFramework;

type

	[TestFixture]
	TAccountSettingsTest = class
		AppDir: WideString; //the current test binary directory
		AppDataSubDir: WideString; //the subdirectory in AppData
	private const
		FP_ACCOUNTS_INI = 'Accounts.ini';
	public
		[Setup]
		procedure Setup;

		[Test]
		procedure TestCreate;

		[Test]
		procedure TestSetValue;

		[Test]
		procedure TestSaveOnChange;

		[Test]
		procedure TestSaveOnChangeDisabled;

		[Test]
		procedure TestSetSettingValue;

		[Test]
		procedure TestNoAccount;

		[Test]
		procedure TestDeleteAccount;

		[Test]
		procedure TestListAccounts;

	end;

implementation

{TAccountSettingsTest}

procedure TAccountSettingsTest.Setup;
begin
	AppDir := IncludeTrailingBackslash(ExtractFilePath(GetModuleName(hInstance)));

	{cleans the previous run artefacts}
	if FileExists(self.AppDir + FP_ACCOUNTS_INI) then
		DeleteFile(self.AppDir + FP_ACCOUNTS_INI);
end;

procedure TAccountSettingsTest.TestCreate;
var
	TestAccountSettings: TNewAccountSettings;
begin
	TestAccountSettings := TNewAccountSettings.Create(DataPath(FP_ACCOUNTS_INI));

	Assert.IsFalse(TestAccountSettings.IsInAccount);
	Assert.IsEmpty(TestAccountSettings.Email);

	TestAccountSettings.Account := 'TEST_ACCOUNT_ONE';
	Assert.IsTrue(TestAccountSettings.IsInAccount);
	Assert.AreEqual('test_fake_email_one@mail.ru', TestAccountSettings.Email);

	TestAccountSettings.Free;
end;

procedure TAccountSettingsTest.TestDeleteAccount;
var
	TestAccountSettings: TNewAccountSettings;
begin
	TestAccountSettings := TNewAccountSettings.Create(self.AppDir + FP_ACCOUNTS_INI, 'NEW_ACCOUNT'); //creates a file in the test exe dir
	TestAccountSettings.Email := 'deleted_account@mail.ru';
	TestAccountSettings.Save;
	TestAccountSettings.Free;

	TestAccountSettings := TNewAccountSettings.Create(self.AppDir + FP_ACCOUNTS_INI, 'NEW_ACCOUNT');
	Assert.AreEqual('deleted_account@mail.ru', TestAccountSettings.Email);
	TestAccountSettings.DeleteAccount('NEW_ACCOUNT');
	TestAccountSettings.Free;

	TestAccountSettings := TNewAccountSettings.Create(self.AppDir + FP_ACCOUNTS_INI, 'NEW_ACCOUNT');
	Assert.IsEmpty(TestAccountSettings.Email);
	TestAccountSettings.Free;
end;

procedure TAccountSettingsTest.TestListAccounts;
var
	TestAccountSettings: TNewAccountSettings;
	AccountsList: TWSList;
begin
	AccountsList := TWSList.Create();
	TestAccountSettings := TNewAccountSettings.Create(DataPath(FP_ACCOUNTS_INI));

	AccountsList := TestAccountSettings.GetAccountsList();
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_ONE'));
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_TWO'));
	Assert.IsFalse(AccountsList.Contains('TEST_ACCOUNT_THREE'));
	Assert.IsTrue(AccountsList.Contains('TEST_PUBLIC_ACCOUNT'));

	AccountsList := TestAccountSettings.GetAccountsList([ATPrivate]);
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_ONE'));
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_TWO'));
	Assert.IsFalse(AccountsList.Contains('TEST_ACCOUNT_THREE'));
	Assert.IsFalse(AccountsList.Contains('TEST_PUBLIC_ACCOUNT'));

	AccountsList := TestAccountSettings.GetAccountsList([ATPublic]);
	Assert.IsFalse(AccountsList.Contains('TEST_ACCOUNT_ONE'));
	Assert.IsFalse(AccountsList.Contains('TEST_ACCOUNT_TWO'));
	Assert.IsFalse(AccountsList.Contains('TEST_ACCOUNT_THREE'));
	Assert.IsTrue(AccountsList.Contains('TEST_PUBLIC_ACCOUNT'));

	AccountsList := TestAccountSettings.GetAccountsList([ATPrivate, ATPublic], [VTTrash, VTInvites]);
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_ONE'));
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_ONE' + TrashPostfix));
	Assert.IsTrue(AccountsList.Contains('TEST_ACCOUNT_ONE' + InvitesPostfix));
	Assert.IsFalse(AccountsList.Contains('TEST_ACCOUNT_ONE' + SharedPostfix));
	Assert.IsTrue(AccountsList.Contains('TEST_PUBLIC_ACCOUNT'));
	Assert.IsFalse(AccountsList.Contains('TEST_PUBLIC_ACCOUNT' + TrashPostfix));
	Assert.IsFalse(AccountsList.Contains('TEST_PUBLIC_ACCOUNT' + InvitesPostfix));
	Assert.IsFalse(AccountsList.Contains('TEST_PUBLIC_ACCOUNT' + SharedPostfix));

	TestAccountSettings.Free;
end;

procedure TAccountSettingsTest.TestNoAccount;
var
	TestAccountSettings: TNewAccountSettings;
begin
	TestAccountSettings := TNewAccountSettings.Create(self.AppDir + FP_ACCOUNTS_INI); //creates a file in the test exe dir

	TestAccountSettings.Email := 'no_acc_email@mail.test';
	TestAccountSettings.Save();
	TestAccountSettings.Free;

	Assert.IsFalse(FileExists(self.AppDir + FP_ACCOUNTS_INI));
end;

procedure TAccountSettingsTest.TestSaveOnChange;
var
	TestAccountSettings: TNewAccountSettings;
begin
	TestAccountSettings := TNewAccountSettings.Create(self.AppDir + FP_ACCOUNTS_INI); //creates a file in the test exe dir
	Assert.IsFalse(TestAccountSettings.SaveOnChange);
	TestAccountSettings.SaveOnChange := True;

	Assert.IsFalse(TestAccountSettings.IsInAccount);
	Assert.IsEmpty(TestAccountSettings.Email);

	TestAccountSettings.Account := 'NEW_ACCOUNT';
	Assert.IsTrue(TestAccountSettings.IsInAccount);
	Assert.IsEmpty(TestAccountSettings.Email);

	TestAccountSettings.Email := 'new_acc_email@mail.test';

	TestAccountSettings.Free;

	Assert.IsTrue(FileExists(self.AppDir + FP_ACCOUNTS_INI));

	TestAccountSettings := TNewAccountSettings.Create(self.AppDir + FP_ACCOUNTS_INI, 'NEW_ACCOUNT');
	Assert.IsTrue(TestAccountSettings.IsInAccount);
	Assert.AreEqual('new_acc_email@mail.test', TestAccountSettings.Email);
	TestAccountSettings.Free;
end;

procedure TAccountSettingsTest.TestSaveOnChangeDisabled;
var
	TestAccountSettings: TNewAccountSettings;
begin
	TestAccountSettings := TNewAccountSettings.Create(self.AppDir + FP_ACCOUNTS_INI); //creates a file in the test exe dir
	Assert.IsFalse(TestAccountSettings.SaveOnChange);

	Assert.IsFalse(TestAccountSettings.IsInAccount);
	Assert.IsEmpty(TestAccountSettings.Email);

	TestAccountSettings.Account := 'NEW_ACCOUNT';
	Assert.IsTrue(TestAccountSettings.IsInAccount);
	Assert.AreEqual('', TestAccountSettings.Email);
	TestAccountSettings.Email := 'new_acc_email@mail.test';

	{The values aren't saved}
	TestAccountSettings.Free;

	Assert.IsFalse(FileExists(self.AppDir + FP_ACCOUNTS_INI));
end;

procedure TAccountSettingsTest.TestSetSettingValue;
var
	TestAccountSettings: TNewAccountSettings;
begin
	Assert.IsFalse(FileExists(self.AppDir + FP_ACCOUNTS_INI));
	TestAccountSettings := TNewAccountSettings.Create(self.AppDir + FP_ACCOUNTS_INI, 'NEW_ACCOUNT'); //creates a file in the test exe dir

	TestAccountSettings.SetSettingValue('email', 'new_acc_email@mail.test');

	Assert.IsTrue(FileExists(self.AppDir + FP_ACCOUNTS_INI));
	TestAccountSettings.Free;

	TestAccountSettings := TNewAccountSettings.Create(self.AppDir + FP_ACCOUNTS_INI, 'NEW_ACCOUNT');
	Assert.AreEqual('new_acc_email@mail.test', TestAccountSettings.Email);
	TestAccountSettings.Free;
end;

procedure TAccountSettingsTest.TestSetValue;
var
	TestAccountSettings: TNewAccountSettings;
begin
	TestAccountSettings := TNewAccountSettings.Create(self.AppDir + FP_ACCOUNTS_INI); //creates a file in the test exe dir

	Assert.IsFalse(TestAccountSettings.IsInAccount);
	Assert.IsEmpty(TestAccountSettings.Email);

	TestAccountSettings.Account := 'NEW_ACCOUNT';
	Assert.IsTrue(TestAccountSettings.IsInAccount);
	Assert.IsEmpty(TestAccountSettings.Email);
	TestAccountSettings.Email := 'new_acc_email@mail.test';
	TestAccountSettings.Save;

	TestAccountSettings.Free;

	TestAccountSettings := TNewAccountSettings.Create(self.AppDir + FP_ACCOUNTS_INI, 'NEW_ACCOUNT');
	Assert.IsTrue(TestAccountSettings.IsInAccount);
	Assert.AreEqual('new_acc_email@mail.test', TestAccountSettings.Email);
	TestAccountSettings.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TAccountSettingsTest);

end.
