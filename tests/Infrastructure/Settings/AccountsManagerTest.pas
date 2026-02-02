unit AccountsManagerTest;

interface

uses
	AccountsManager,
	AccountSettings,
	CloudConstants,
	TestHelper,
	SysUtils,
	Classes,
	WSList,
	Cipher,
	SettingsConstants,
	ConfigFile,
	Logger,
	DUnitX.TestFramework;

type

	[TestFixture]
	TAccountsManagerTest = class
		AppDir: WideString; {The current test binary directory}
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
		[Test]
		procedure TestAuthMethodPersistence;
		[Test]
		{Verifies CipherProfileId is correctly saved and restored from INI file}
		procedure TestCipherProfileIdPersistence;
		[Test]
		{Verifies CipherProfileId defaults to empty string for accounts without it}
		procedure TestCipherProfileIdEmptyByDefault;
		[Test]
		{Verifies CipherProfileId survives write-read-write-read cycle across different accounts}
		procedure TestCipherProfileIdRoundtrip;

		[Test]
		{Verifies all settings are accessible under the new name after rename}
		procedure TestRenameAccount_PreservesSettings;
		[Test]
		{Verifies old section no longer exists after rename}
		procedure TestRenameAccount_DeletesOldSection;
		[Test]
		{Verifies renaming to the same name does not crash or lose data}
		procedure TestRenameAccount_SameName_NoError;

		{IsValidAccountName tests}
		[Test]
		{Verifies normal account names pass validation}
		procedure TestIsValidAccountName_ValidNames;
		[Test]
		{Verifies backslash in account name fails validation}
		procedure TestIsValidAccountName_Backslash;
		[Test]
		{Verifies forward slash in account name fails validation}
		procedure TestIsValidAccountName_ForwardSlash;
		[Test]
		{Verifies square brackets in account name fail validation}
		procedure TestIsValidAccountName_Brackets;
		[Test]
		{Verifies reserved virtual directory postfixes fail validation}
		procedure TestIsValidAccountName_ReservedPostfix;
		[Test]
		{Verifies empty account name fails validation}
		procedure TestIsValidAccountName_Empty;

		[Test]
		{Verifies GetAccountsList silently skips accounts with invalid names}
		procedure TestGetAccountsList_SkipsInvalidNames;

	end;

	[TestFixture]
	TNullAccountsManagerTest = class
	public
		[Test]
		{Verifies TNullAccountsManager can be assigned to IAccountsManager variable}
		procedure TestImplementsIAccountsManager;

		[Test]
		{Verifies GetAccountSettings returns default settings with account name}
		procedure TestGetAccountSettingsReturnsDefault;

		[Test]
		{Verifies GetAccountSettings preserves account name}
		procedure TestGetAccountSettingsPreservesAccountName;

		[Test]
		{Verifies SwitchPasswordStorage completes without exception}
		procedure TestSwitchPasswordStorageNoOp;

		[Test]
		{Verifies SetCryptedGUID completes without exception}
		procedure TestSetCryptedGUIDNoOp;

		[Test]
		{Verifies multiple calls work correctly}
		procedure TestMultipleCalls;

		[Test]
		{Verifies GetAccountsList returns empty list}
		procedure TestGetAccountsList_ReturnsEmpty;
		[Test]
		{Verifies SetAccountSettings completes without exception}
		procedure TestSetAccountSettings_NoOp;
		[Test]
		{Verifies DeleteAccount completes without exception}
		procedure TestDeleteAccount_NoOp;
		[Test]
		{Verifies RenameAccount completes without exception}
		procedure TestRenameAccount_NoOp;
	end;

implementation

{TAccountsManagerTest}

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
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create); {Uses a new file in the test exe dir}

	TestAccountSettings.Password := 'cjhjrnsczxj,tpmzyd;jgeceyekb,fyfy';
	TestAccountsManager.SetAccountSettings('NEW_ACCOUNT', TestAccountSettings);
	TestAccountsManager.Free;

	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
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
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create); {Uses a new file in the test exe dir}

	TestAccountSettings.Email := 'deleted_account@mail.ru';
	TestAccountsManager.SetAccountSettings('NEW_ACCOUNT', TestAccountSettings);
	TestAccountsManager.Free;

	TestAccountSettings.Email := EmptyWideStr;

	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	TestAccountSettings := TestAccountsManager.GetAccountSettings('NEW_ACCOUNT');
	Assert.AreEqual('deleted_account@mail.ru', TestAccountSettings.Email);
	TestAccountsManager.DeleteAccount('NEW_ACCOUNT');
	TestAccountsManager.Free;

	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	Assert.IsEmpty(TestAccountsManager.GetAccountSettings('NEW_ACCOUNT').Email);
	TestAccountsManager.Free;
end;

procedure TAccountsManagerTest.TestGetAccountSettings;
var
	TestAccountsManager: TAccountsManager;
	TestAccountSettings: TAccountSettings;
begin
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(DataPath(FP_ACCOUNTS_INI)), TNullLogger.Create); {Uses a test file}
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
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(DataPath(FP_ACCOUNTS_INI)), TNullLogger.Create); {Uses a test file}

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
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create); {Uses a new file in the test exe dir}
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
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(DataPath(FP_ACCOUNTS_INI)), TNullLogger.Create); {Uses a test file}
	TestAccountSettings := TestAccountsManager.GetAccountSettings('TEST_ACCOUNT_TWO');
	TestAccountsManager.Free;

	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create); {Uses a new file in the test exe dir}
	TestAccountsManager.SetAccountSettings('NEW_ACCOUNT', TestAccountSettings);
	TestAccountsManager.Free;

	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	TestAccountSettingsNew := TestAccountsManager.GetAccountSettings('NEW_ACCOUNT');
	TestAccountsManager.Free;

	Assert.AreEqual(TestAccountSettings.Email, TestAccountSettingsNew.Email);
	Assert.AreEqual(TestAccountSettings.UseTCPasswordManager, TestAccountSettingsNew.UseTCPasswordManager);
	Assert.AreEqual('', TestAccountSettingsNew.CryptedGUIDFiles);
end;

procedure TAccountsManagerTest.TestSetCryptedGUID;
var
	TestAccountsManager: TAccountsManager;
begin
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create); {Uses a new file in the test exe dir}
	Assert.IsEmpty(TestAccountsManager.GetAccountSettings('NEW_ACCOUNT').CryptedGUIDFiles);
	TestAccountsManager.SetCryptedGUID('NEW_ACCOUNT', TFileCipher.GetCryptedGUID('cjhjrnsczxj,tpmzyd;jgeceyekb,fyfy'));

	Assert.AreEqual(TFileCipher.GetCryptedGUID('cjhjrnsczxj,tpmzyd;jgeceyekb,fyfy'), TestAccountsManager.GetAccountSettings('NEW_ACCOUNT').CryptedGUIDFiles);
	TestAccountsManager.Free;
end;

procedure TAccountsManagerTest.TestAuthMethodPersistence;
var
	TestAccountsManager: TAccountsManager;
	TestAccountSettings: TAccountSettings;
	LoadedSettings: TAccountSettings;
begin
	{ Test that AuthMethod and UseAppPassword are properly saved and loaded }
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	try
		TestAccountSettings := Default(TAccountSettings);
		TestAccountSettings.Account := 'OAUTH_TEST_ACCOUNT';
		TestAccountSettings.Email := 'oauth_test@mail.ru';
		TestAccountSettings.AuthMethod := CLOUD_AUTH_METHOD_OAUTH_APP;
		TestAccountSettings.UseAppPassword := True;

		TestAccountsManager.SetAccountSettings(TestAccountSettings);
	finally
		TestAccountsManager.Free;
	end;

	{ Reload and verify }
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	try
		LoadedSettings := TestAccountsManager.GetAccountSettings('OAUTH_TEST_ACCOUNT');

		Assert.AreEqual('oauth_test@mail.ru', LoadedSettings.Email);
		Assert.AreEqual(CLOUD_AUTH_METHOD_OAUTH_APP, LoadedSettings.AuthMethod);
		Assert.IsTrue(LoadedSettings.UseAppPassword);
	finally
		TestAccountsManager.Free;
	end;
end;

procedure TAccountsManagerTest.TestCipherProfileIdPersistence;
var
	TestAccountsManager: TAccountsManager;
	TestAccountSettings: TAccountSettings;
	LoadedSettings: TAccountSettings;
begin
	{ Save account with CipherProfileId set }
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	try
		TestAccountSettings := Default(TAccountSettings);
		TestAccountSettings.Account := 'CIPHER_PROFILE_ACCOUNT';
		TestAccountSettings.Email := 'cipher_test@mail.ru';
		TestAccountSettings.CipherProfileId := 'dcpcrypt-twofish256-cfb8-sha256';

		TestAccountsManager.SetAccountSettings(TestAccountSettings);
	finally
		TestAccountsManager.Free;
	end;

	{ Reload from file and verify CipherProfileId was preserved }
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	try
		LoadedSettings := TestAccountsManager.GetAccountSettings('CIPHER_PROFILE_ACCOUNT');

		Assert.AreEqual('cipher_test@mail.ru', LoadedSettings.Email);
		Assert.AreEqual('dcpcrypt-twofish256-cfb8-sha256', LoadedSettings.CipherProfileId);
	finally
		TestAccountsManager.Free;
	end;
end;

procedure TAccountsManagerTest.TestCipherProfileIdEmptyByDefault;
var
	TestAccountsManager: TAccountsManager;
	TestAccountSettings: TAccountSettings;
	LoadedSettings: TAccountSettings;
begin
	{ Save account without setting CipherProfileId }
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	try
		TestAccountSettings := Default(TAccountSettings);
		TestAccountSettings.Account := 'NO_CIPHER_PROFILE_ACCOUNT';
		TestAccountSettings.Email := 'no_cipher@mail.ru';
		{ CipherProfileId intentionally not set - should remain default empty }

		TestAccountsManager.SetAccountSettings(TestAccountSettings);
	finally
		TestAccountsManager.Free;
	end;

	{ Reload and verify CipherProfileId defaults to empty string }
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	try
		LoadedSettings := TestAccountsManager.GetAccountSettings('NO_CIPHER_PROFILE_ACCOUNT');

		Assert.AreEqual('no_cipher@mail.ru', LoadedSettings.Email);
		Assert.IsEmpty(LoadedSettings.CipherProfileId);
	finally
		TestAccountsManager.Free;
	end;
end;

procedure TAccountsManagerTest.TestCipherProfileIdRoundtrip;
var
	TestAccountsManager: TAccountsManager;
	TestAccountSettings: TAccountSettings;
	FirstLoaded, SecondLoaded: TAccountSettings;
begin
	{ Write first account with CipherProfileId }
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	try
		TestAccountSettings := Default(TAccountSettings);
		TestAccountSettings.Account := 'ROUNDTRIP_FIRST';
		TestAccountSettings.Email := 'roundtrip_first@mail.ru';
		TestAccountSettings.CipherProfileId := 'dcpcrypt-twofish256-cfb8-sha256';

		TestAccountsManager.SetAccountSettings(TestAccountSettings);
	finally
		TestAccountsManager.Free;
	end;

	{ Read first account back, then write its CipherProfileId to a second account }
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	try
		FirstLoaded := TestAccountsManager.GetAccountSettings('ROUNDTRIP_FIRST');

		TestAccountSettings := Default(TAccountSettings);
		TestAccountSettings.Account := 'ROUNDTRIP_SECOND';
		TestAccountSettings.Email := 'roundtrip_second@mail.ru';
		TestAccountSettings.CipherProfileId := FirstLoaded.CipherProfileId;

		TestAccountsManager.SetAccountSettings(TestAccountSettings);
	finally
		TestAccountsManager.Free;
	end;

	{ Reload both accounts and verify CipherProfileId matches }
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	try
		FirstLoaded := TestAccountsManager.GetAccountSettings('ROUNDTRIP_FIRST');
		SecondLoaded := TestAccountsManager.GetAccountSettings('ROUNDTRIP_SECOND');

		Assert.AreEqual('dcpcrypt-twofish256-cfb8-sha256', FirstLoaded.CipherProfileId);
		Assert.AreEqual('dcpcrypt-twofish256-cfb8-sha256', SecondLoaded.CipherProfileId);
		Assert.AreEqual(FirstLoaded.CipherProfileId, SecondLoaded.CipherProfileId);
	finally
		TestAccountsManager.Free;
	end;
end;

procedure TAccountsManagerTest.TestRenameAccount_PreservesSettings;
var
	TestAccountsManager: TAccountsManager;
	Original, Loaded: TAccountSettings;
begin
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	try
		Original := Default(TAccountSettings);
		Original.Account := 'OLD_NAME';
		Original.Email := 'rename@mail.ru';
		Original.Password := 'secret';
		Original.UseTCPasswordManager := True;
		Original.UnlimitedFileSize := True;
		Original.SplitLargeFiles := True;
		Original.EncryptFilesMode := EncryptModeAlways;

		TestAccountsManager.SetAccountSettings(Original);
		TestAccountsManager.SetCryptedGUID('OLD_NAME', 'test-guid-value');

		TestAccountsManager.RenameAccount('OLD_NAME', 'NEW_NAME');

		Loaded := TestAccountsManager.GetAccountSettings('NEW_NAME');
		Assert.AreEqual('rename@mail.ru', Loaded.Email, 'Email should be preserved');
		Assert.AreEqual('secret', Loaded.Password, 'Password should be preserved');
		Assert.IsTrue(Loaded.UseTCPasswordManager, 'UseTCPasswordManager should be preserved');
		Assert.IsTrue(Loaded.UnlimitedFileSize, 'UnlimitedFileSize should be preserved');
		Assert.IsTrue(Loaded.SplitLargeFiles, 'SplitLargeFiles should be preserved');
		Assert.AreEqual(EncryptModeAlways, Loaded.EncryptFilesMode, 'EncryptFilesMode should be preserved');
		Assert.AreEqual('test-guid-value', Loaded.CryptedGUIDFiles, 'CryptedGUID should be preserved');
	finally
		TestAccountsManager.Free;
	end;
end;

procedure TAccountsManagerTest.TestRenameAccount_DeletesOldSection;
var
	TestAccountsManager: TAccountsManager;
	Original, Loaded: TAccountSettings;
	AccountsList: TWSList;
begin
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	try
		Original := Default(TAccountSettings);
		Original.Account := 'BEFORE_RENAME';
		Original.Email := 'before@mail.ru';
		TestAccountsManager.SetAccountSettings(Original);

		TestAccountsManager.RenameAccount('BEFORE_RENAME', 'AFTER_RENAME');

		{Old name should return empty defaults}
		Loaded := TestAccountsManager.GetAccountSettings('BEFORE_RENAME');
		Assert.IsEmpty(Loaded.Email, 'Old section should be deleted');

		{Old name should not appear in accounts list}
		AccountsList := TestAccountsManager.GetAccountsList;
		Assert.IsFalse(AccountsList.Contains('BEFORE_RENAME'), 'Old account should not be in list');
		Assert.IsTrue(AccountsList.Contains('AFTER_RENAME'), 'New account should be in list');
	finally
		TestAccountsManager.Free;
	end;
end;

procedure TAccountsManagerTest.TestRenameAccount_SameName_NoError;
var
	TestAccountsManager: TAccountsManager;
	Original, Loaded: TAccountSettings;
begin
	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(self.AppDir + FP_ACCOUNTS_INI), TNullLogger.Create);
	try
		Original := Default(TAccountSettings);
		Original.Account := 'SAME_NAME';
		Original.Email := 'same@mail.ru';
		TestAccountsManager.SetAccountSettings(Original);

		{Renaming to same name should be a no-op}
		TestAccountsManager.RenameAccount('SAME_NAME', 'SAME_NAME');

		Loaded := TestAccountsManager.GetAccountSettings('SAME_NAME');
		Assert.AreEqual('same@mail.ru', Loaded.Email, 'Data should be intact after identity rename');
	finally
		TestAccountsManager.Free;
	end;
end;

procedure TAccountsManagerTest.TestIsValidAccountName_ValidNames;
begin
	Assert.IsTrue(TAccountsManager.IsValidAccountName('MyAccount'), 'Simple name should be valid');
	Assert.IsTrue(TAccountsManager.IsValidAccountName('Account With Spaces'), 'Name with spaces should be valid');
	Assert.IsTrue(TAccountsManager.IsValidAccountName('user@mail.ru'), 'Name with @ and dots should be valid');
	Assert.IsTrue(TAccountsManager.IsValidAccountName('Account-123_test'), 'Name with dashes and underscores should be valid');
	Assert.IsTrue(TAccountsManager.IsValidAccountName('trashcan'), 'Name containing "trash" substring should be valid');
end;

procedure TAccountsManagerTest.TestIsValidAccountName_Backslash;
begin
	Assert.IsFalse(TAccountsManager.IsValidAccountName('TEST\TEST'), 'Backslash should be rejected');
	Assert.IsFalse(TAccountsManager.IsValidAccountName('\leading'), 'Leading backslash should be rejected');
	Assert.IsFalse(TAccountsManager.IsValidAccountName('trailing\'), 'Trailing backslash should be rejected');
end;

procedure TAccountsManagerTest.TestIsValidAccountName_ForwardSlash;
begin
	Assert.IsFalse(TAccountsManager.IsValidAccountName('TEST/TEST'), 'Forward slash should be rejected');
	Assert.IsFalse(TAccountsManager.IsValidAccountName('/leading'), 'Leading slash should be rejected');
	Assert.IsFalse(TAccountsManager.IsValidAccountName('trailing/'), 'Trailing slash should be rejected');
end;

procedure TAccountsManagerTest.TestIsValidAccountName_Brackets;
begin
	Assert.IsFalse(TAccountsManager.IsValidAccountName('TEST[1]'), 'Square brackets should be rejected');
	Assert.IsFalse(TAccountsManager.IsValidAccountName('[section]'), 'INI section format should be rejected');
	Assert.IsFalse(TAccountsManager.IsValidAccountName('only[open'), 'Opening bracket should be rejected');
	Assert.IsFalse(TAccountsManager.IsValidAccountName('only]close'), 'Closing bracket should be rejected');
end;

procedure TAccountsManagerTest.TestIsValidAccountName_ReservedPostfix;
begin
	Assert.IsFalse(TAccountsManager.IsValidAccountName('MyAccount.trash'), '.trash postfix should be rejected');
	Assert.IsFalse(TAccountsManager.IsValidAccountName('MyAccount.shared'), '.shared postfix should be rejected');
	Assert.IsFalse(TAccountsManager.IsValidAccountName('MyAccount.invites'), '.invites postfix should be rejected');
	Assert.IsFalse(TAccountsManager.IsValidAccountName('MyAccount.TRASH'), '.TRASH (case-insensitive) should be rejected');
	Assert.IsFalse(TAccountsManager.IsValidAccountName('MyAccount.Shared'), '.Shared (case-insensitive) should be rejected');
end;

procedure TAccountsManagerTest.TestIsValidAccountName_Empty;
begin
	Assert.IsFalse(TAccountsManager.IsValidAccountName(''), 'Empty name should be rejected');
end;

procedure TAccountsManagerTest.TestGetAccountsList_SkipsInvalidNames;
var
	TestAccountsManager: TAccountsManager;
	AccountsList: TWSList;
	IniContent: TStringList;
	IniPath: WideString;
begin
	IniPath := self.AppDir + FP_ACCOUNTS_INI;

	{Write INI file directly to simulate manual edit with invalid section names.
		TIniConfigFile validates section names, so we bypass it for the bad entries.}
	IniContent := TStringList.Create;
	try
		IniContent.Add('[VALID_ACCOUNT]');
		IniContent.Add('email=valid@mail.ru');
		IniContent.Add('');
		IniContent.Add('[TEST\SLASH]');
		IniContent.Add('email=slash@mail.ru');
		IniContent.Add('');
		IniContent.Add('[MyAccount.trash]');
		IniContent.Add('email=trash@mail.ru');
		IniContent.SaveToFile(IniPath);
	finally
		IniContent.Free;
	end;

	TestAccountsManager := TAccountsManager.Create(TIniConfigFile.Create(IniPath), TNullLogger.Create);
	try
		AccountsList := TestAccountsManager.GetAccountsList;
		Assert.IsTrue(AccountsList.Contains('VALID_ACCOUNT'), 'Valid account should be listed');
		Assert.IsFalse(AccountsList.Contains('TEST\SLASH'), 'Account with backslash should be skipped');
		Assert.IsFalse(AccountsList.Contains('MyAccount.trash'), 'Account with reserved postfix should be skipped');
	finally
		TestAccountsManager.Free;
	end;
end;

{TNullAccountsManagerTest}

procedure TNullAccountsManagerTest.TestImplementsIAccountsManager;
var
	AccountsManager: IAccountsManager;
begin
	AccountsManager := TNullAccountsManager.Create;
	Assert.IsNotNull(AccountsManager);
end;

procedure TNullAccountsManagerTest.TestGetAccountSettingsReturnsDefault;
var
	AccountsManager: IAccountsManager;
	Settings: TAccountSettings;
begin
	AccountsManager := TNullAccountsManager.Create;
	Settings := AccountsManager.GetAccountSettings('test_account');
	{Default values should be empty/false/zero}
	Assert.AreEqual('', Settings.Email);
	Assert.AreEqual('', Settings.Password);
	Assert.IsFalse(Settings.UseTCPasswordManager);
end;

procedure TNullAccountsManagerTest.TestGetAccountSettingsPreservesAccountName;
var
	AccountsManager: IAccountsManager;
	Settings: TAccountSettings;
begin
	AccountsManager := TNullAccountsManager.Create;
	Settings := AccountsManager.GetAccountSettings('my_test_account');
	Assert.AreEqual('my_test_account', Settings.Account);
end;

procedure TNullAccountsManagerTest.TestSwitchPasswordStorageNoOp;
var
	AccountsManager: IAccountsManager;
begin
	AccountsManager := TNullAccountsManager.Create;
	{Should complete without exception}
	AccountsManager.SwitchPasswordStorage('test_account');
	Assert.Pass('SwitchPasswordStorage completed without exception');
end;

procedure TNullAccountsManagerTest.TestSetCryptedGUIDNoOp;
var
	AccountsManager: IAccountsManager;
begin
	AccountsManager := TNullAccountsManager.Create;
	{Should complete without exception}
	AccountsManager.SetCryptedGUID('test_account', 'some-guid-value');
	Assert.Pass('SetCryptedGUID completed without exception');
end;

procedure TNullAccountsManagerTest.TestMultipleCalls;
var
	AccountsManager: IAccountsManager;
	Settings1, Settings2: TAccountSettings;
begin
	AccountsManager := TNullAccountsManager.Create;

	Settings1 := AccountsManager.GetAccountSettings('account1');
	Settings2 := AccountsManager.GetAccountSettings('account2');

	Assert.AreEqual('account1', Settings1.Account);
	Assert.AreEqual('account2', Settings2.Account);

	AccountsManager.SwitchPasswordStorage('account1');
	AccountsManager.SetCryptedGUID('account2', 'guid');

	Assert.Pass('Multiple calls completed without exception');
end;

procedure TNullAccountsManagerTest.TestGetAccountsList_ReturnsEmpty;
var
	AccountsManager: IAccountsManager;
	List: TWSList;
begin
	AccountsManager := TNullAccountsManager.Create;
	List := AccountsManager.GetAccountsList;
	Assert.AreEqual(Integer(0), Integer(List.Count));
end;

procedure TNullAccountsManagerTest.TestSetAccountSettings_NoOp;
var
	AccountsManager: IAccountsManager;
	Settings: TAccountSettings;
begin
	AccountsManager := TNullAccountsManager.Create;
	Settings := Default(TAccountSettings);
	Settings.Account := 'test';
	AccountsManager.SetAccountSettings(Settings);
	Assert.Pass;
end;

procedure TNullAccountsManagerTest.TestDeleteAccount_NoOp;
var
	AccountsManager: IAccountsManager;
begin
	AccountsManager := TNullAccountsManager.Create;
	AccountsManager.DeleteAccount('test');
	Assert.Pass;
end;

procedure TNullAccountsManagerTest.TestRenameAccount_NoOp;
var
	AccountsManager: IAccountsManager;
begin
	AccountsManager := TNullAccountsManager.Create;
	AccountsManager.RenameAccount('old', 'new');
	Assert.Pass;
end;

initialization

TDUnitX.RegisterTestFixture(TAccountsManagerTest);
TDUnitX.RegisterTestFixture(TNullAccountsManagerTest);

end.
