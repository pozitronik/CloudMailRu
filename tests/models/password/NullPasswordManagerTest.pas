unit NullPasswordManagerTest;

interface

uses
	IPasswordManagerInterface,
	PLUGIN_TYPES,
	SysUtils,
	DUnitX.TestFramework;

type
	[TestFixture]
	TNullPasswordManagerTest = class
	public
		[Test]
		{Verifies TNullPasswordManager can be assigned to IPasswordManager variable}
		procedure TestImplementsIPasswordManager;

		[Test]
		{Verifies GetPassword returns FS_FILE_NOTFOUND (no password stored)}
		procedure TestGetPasswordReturnsNotFound;

		[Test]
		{Verifies SetPassword returns FS_FILE_OK (pretends to save)}
		procedure TestSetPasswordReturnsOK;

		[Test]
		{Verifies GetPassword does not modify the password variable}
		procedure TestGetPasswordDoesNotModifyPassword;

		[Test]
		{Verifies multiple sequential calls work correctly}
		procedure TestMultipleCalls;

		[Test]
		{Verifies different keys return same result}
		procedure TestDifferentKeysReturnSameResult;
	end;

implementation

procedure TNullPasswordManagerTest.TestImplementsIPasswordManager;
var
	PasswordManager: IPasswordManager;
begin
	PasswordManager := TNullPasswordManager.Create;
	Assert.IsNotNull(PasswordManager);
end;

procedure TNullPasswordManagerTest.TestGetPasswordReturnsNotFound;
var
	PasswordManager: IPasswordManager;
	Password: WideString;
begin
	PasswordManager := TNullPasswordManager.Create;
	Password := '';
	Assert.AreEqual(FS_FILE_NOTFOUND, PasswordManager.GetPassword('test_key', Password));
end;

procedure TNullPasswordManagerTest.TestSetPasswordReturnsOK;
var
	PasswordManager: IPasswordManager;
begin
	PasswordManager := TNullPasswordManager.Create;
	Assert.AreEqual(FS_FILE_OK, PasswordManager.SetPassword('test_key', 'test_password'));
end;

procedure TNullPasswordManagerTest.TestGetPasswordDoesNotModifyPassword;
var
	PasswordManager: IPasswordManager;
	Password: WideString;
begin
	PasswordManager := TNullPasswordManager.Create;
	Password := 'original_value';
	PasswordManager.GetPassword('test_key', Password);
	Assert.AreEqual('original_value', Password, 'Password should not be modified by GetPassword');
end;

procedure TNullPasswordManagerTest.TestMultipleCalls;
var
	PasswordManager: IPasswordManager;
	Password: WideString;
	i: Integer;
begin
	PasswordManager := TNullPasswordManager.Create;
	for i := 1 to 10 do
	begin
		Password := '';
		Assert.AreEqual(FS_FILE_NOTFOUND, PasswordManager.GetPassword('key' + IntToStr(i), Password));
		Assert.AreEqual(FS_FILE_OK, PasswordManager.SetPassword('key' + IntToStr(i), 'password' + IntToStr(i)));
	end;
	Assert.Pass('Multiple calls completed without exception');
end;

procedure TNullPasswordManagerTest.TestDifferentKeysReturnSameResult;
var
	PasswordManager: IPasswordManager;
	Password: WideString;
begin
	PasswordManager := TNullPasswordManager.Create;
	Password := '';
	Assert.AreEqual(FS_FILE_NOTFOUND, PasswordManager.GetPassword('account1', Password));
	Assert.AreEqual(FS_FILE_NOTFOUND, PasswordManager.GetPassword('account2', Password));
	Assert.AreEqual(FS_FILE_NOTFOUND, PasswordManager.GetPassword('proxy_user', Password));
end;

initialization

TDUnitX.RegisterTestFixture(TNullPasswordManagerTest);

end.
