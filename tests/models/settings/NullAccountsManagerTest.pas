unit NullAccountsManagerTest;

interface

uses
	IAccountsManagerInterface,
	AccountSettings,
	SysUtils,
	DUnitX.TestFramework;

type
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
	end;

implementation

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

initialization

TDUnitX.RegisterTestFixture(TNullAccountsManagerTest);

end.
