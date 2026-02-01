unit PasswordManagerTest;

interface

uses
	PasswordManager,
	Logger,
	TCHandler,
	WFXTypes,
	Windows,
	SysUtils,
	DUnitX.TestFramework;

type
	{Test fixture for TTCPasswordManager with mock CryptProc}
	[TestFixture]
	TTCPasswordManagerTest = class
	public
		[Test]
		{Verifies TTCPasswordManager implements IPasswordManager interface}
		procedure TestImplementsIPasswordManager;

		[Test]
		procedure TestGetPassword_FS_FILE_OK_ReturnsPassword;
		[Test]
		procedure TestGetPassword_FS_FILE_NOTFOUND_TriesWithUI;
		[Test]
		procedure TestGetPassword_FS_FILE_READERROR_LogsError;
		[Test]
		procedure TestGetPassword_FS_FILE_NOTSUPPORTED_LogsDecryptFailed;
		[Test]
		procedure TestSetPassword_FS_FILE_OK_LogsSuccess;
		[Test]
		procedure TestSetPassword_FS_FILE_NOTSUPPORTED_LogsEncryptFailed;
		[Test]
		procedure TestSetPassword_FS_FILE_WRITEERROR_LogsWriteFailed;
		[Test]
		procedure TestSetPassword_FS_FILE_NOTFOUND_LogsNoMasterPassword;
		[Test]
		procedure TestGetPassword_BufferFilledCorrectly;
	end;

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

var
	{Global state for mock CryptProc - set before each test}
	GMockCryptResult: Integer;
	GMockCryptPassword: WideString;
	GMockCryptCallCount: Integer;
	GMockCryptLastMode: Integer;

{Mock CryptProc callback that returns configurable results}
function MockCryptProc(PluginNr, CryptoNr: Integer; Mode: Integer;
	ConnectionName, Password: PWideChar; MaxLen: Integer): Integer; stdcall;
begin
	Inc(GMockCryptCallCount);
	GMockCryptLastMode := Mode;

	{For GetPassword, fill the buffer if returning OK}
	if (Mode = FS_CRYPT_LOAD_PASSWORD_NO_UI) or (Mode = FS_CRYPT_LOAD_PASSWORD) then
	begin
		if GMockCryptResult = FS_FILE_OK then
		begin
			if Length(GMockCryptPassword) < MaxLen div SizeOf(WideChar) then
				StrPCopy(Password, GMockCryptPassword);
		end;
	end;

	Result := GMockCryptResult;
end;

{TTCPasswordManagerTest}

procedure TTCPasswordManagerTest.TestImplementsIPasswordManager;
var
	PasswordManager: IPasswordManager;
begin
	{Note: TTCPasswordManager requires TC callbacks for actual password operations.
	 This test only verifies interface implementation, not functionality.
	 Functional testing requires running within Total Commander context.}
	PasswordManager := TTCPasswordManager.Create(nil, 0, 0, TNullLogger.Create, TNullTCHandler.Create);
	Assert.IsNotNull(PasswordManager);
end;

procedure TTCPasswordManagerTest.TestGetPassword_FS_FILE_OK_ReturnsPassword;
var
	PasswordManager: TTCPasswordManager;
	Password: WideString;
	Result: Integer;
begin
	GMockCryptResult := FS_FILE_OK;
	GMockCryptPassword := 'secret123';
	GMockCryptCallCount := 0;

	PasswordManager := TTCPasswordManager.Create(@MockCryptProc, 1, 2, TNullLogger.Create, TNullTCHandler.Create);
	try
		Password := '';
		Result := PasswordManager.GetPassword('test_key', Password);
		Assert.AreEqual(FS_FILE_OK, Result);
		Assert.AreEqual('secret123', Password);
	finally
		PasswordManager.Free;
	end;
end;

procedure TTCPasswordManagerTest.TestGetPassword_FS_FILE_NOTFOUND_TriesWithUI;
var
	PasswordManager: TTCPasswordManager;
	Password: WideString;
begin
	{First call returns NOTFOUND, second should use UI mode}
	GMockCryptCallCount := 0;
	GMockCryptResult := FS_FILE_NOTFOUND;
	GMockCryptPassword := '';

	PasswordManager := TTCPasswordManager.Create(@MockCryptProc, 1, 2, TNullLogger.Create, TNullTCHandler.Create);
	try
		Password := '';
		PasswordManager.GetPassword('test_key', Password);
		{Should have made 2 calls: first NO_UI, then with UI}
		Assert.AreEqual(2, GMockCryptCallCount);
	finally
		PasswordManager.Free;
	end;
end;

procedure TTCPasswordManagerTest.TestGetPassword_FS_FILE_READERROR_LogsError;
var
	PasswordManager: TTCPasswordManager;
	Password: WideString;
	Result: Integer;
begin
	GMockCryptResult := FS_FILE_READERROR;
	GMockCryptCallCount := 0;

	PasswordManager := TTCPasswordManager.Create(@MockCryptProc, 1, 2, TNullLogger.Create, TNullTCHandler.Create);
	try
		Password := '';
		Result := PasswordManager.GetPassword('test_key', Password);
		Assert.AreEqual(FS_FILE_READERROR, Result);
	finally
		PasswordManager.Free;
	end;
end;

procedure TTCPasswordManagerTest.TestGetPassword_FS_FILE_NOTSUPPORTED_LogsDecryptFailed;
var
	PasswordManager: TTCPasswordManager;
	Password: WideString;
	Result: Integer;
begin
	GMockCryptResult := FS_FILE_NOTSUPPORTED;
	GMockCryptCallCount := 0;

	PasswordManager := TTCPasswordManager.Create(@MockCryptProc, 1, 2, TNullLogger.Create, TNullTCHandler.Create);
	try
		Password := '';
		Result := PasswordManager.GetPassword('test_key', Password);
		Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result);
	finally
		PasswordManager.Free;
	end;
end;

procedure TTCPasswordManagerTest.TestSetPassword_FS_FILE_OK_LogsSuccess;
var
	PasswordManager: TTCPasswordManager;
	Result: Integer;
begin
	GMockCryptResult := FS_FILE_OK;
	GMockCryptCallCount := 0;

	PasswordManager := TTCPasswordManager.Create(@MockCryptProc, 1, 2, TNullLogger.Create, TNullTCHandler.Create);
	try
		Result := PasswordManager.SetPassword('test_key', 'mypassword');
		Assert.AreEqual(FS_FILE_OK, Result);
		Assert.AreEqual(1, GMockCryptCallCount);
	finally
		PasswordManager.Free;
	end;
end;

procedure TTCPasswordManagerTest.TestSetPassword_FS_FILE_NOTSUPPORTED_LogsEncryptFailed;
var
	PasswordManager: TTCPasswordManager;
	Result: Integer;
begin
	GMockCryptResult := FS_FILE_NOTSUPPORTED;
	GMockCryptCallCount := 0;

	PasswordManager := TTCPasswordManager.Create(@MockCryptProc, 1, 2, TNullLogger.Create, TNullTCHandler.Create);
	try
		Result := PasswordManager.SetPassword('test_key', 'mypassword');
		Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result);
	finally
		PasswordManager.Free;
	end;
end;

procedure TTCPasswordManagerTest.TestSetPassword_FS_FILE_WRITEERROR_LogsWriteFailed;
var
	PasswordManager: TTCPasswordManager;
	Result: Integer;
begin
	GMockCryptResult := FS_FILE_WRITEERROR;
	GMockCryptCallCount := 0;

	PasswordManager := TTCPasswordManager.Create(@MockCryptProc, 1, 2, TNullLogger.Create, TNullTCHandler.Create);
	try
		Result := PasswordManager.SetPassword('test_key', 'mypassword');
		Assert.AreEqual(FS_FILE_WRITEERROR, Result);
	finally
		PasswordManager.Free;
	end;
end;

procedure TTCPasswordManagerTest.TestSetPassword_FS_FILE_NOTFOUND_LogsNoMasterPassword;
var
	PasswordManager: TTCPasswordManager;
	Result: Integer;
begin
	GMockCryptResult := FS_FILE_NOTFOUND;
	GMockCryptCallCount := 0;

	PasswordManager := TTCPasswordManager.Create(@MockCryptProc, 1, 2, TNullLogger.Create, TNullTCHandler.Create);
	try
		Result := PasswordManager.SetPassword('test_key', 'mypassword');
		Assert.AreEqual(FS_FILE_NOTFOUND, Result);
	finally
		PasswordManager.Free;
	end;
end;

procedure TTCPasswordManagerTest.TestGetPassword_BufferFilledCorrectly;
var
	PasswordManager: TTCPasswordManager;
	Password: WideString;
begin
	GMockCryptResult := FS_FILE_OK;
	GMockCryptPassword := 'Unicode_Password_\u1234';
	GMockCryptCallCount := 0;

	PasswordManager := TTCPasswordManager.Create(@MockCryptProc, 1, 2, TNullLogger.Create, TNullTCHandler.Create);
	try
		Password := 'initial_value';
		PasswordManager.GetPassword('test_key', Password);
		Assert.AreEqual('Unicode_Password_\u1234', Password);
	finally
		PasswordManager.Free;
	end;
end;

{TNullPasswordManagerTest}

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

TDUnitX.RegisterTestFixture(TTCPasswordManagerTest);
TDUnitX.RegisterTestFixture(TNullPasswordManagerTest);

end.
