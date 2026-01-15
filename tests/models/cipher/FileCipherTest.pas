unit FileCipherTest;

interface

uses
	FileCipher,
	DUnitX.TestFramework;

type
	{ Test helper class to expose protected methods }
	TFileCipherTestHelper = class(TFileCipher)
	public
		function TestBase64ToSafe(const Base64: WideString): WideString;
		function TestBase64FromSafe(const Safe: WideString): WideString;
	end;

	[TestFixture]
	TFileCipherTest = class
	public
		{ Base64ToSafe tests }
		[Test]
		procedure TestBase64ToSafePlusToMinus;
		[Test]
		procedure TestBase64ToSafeSlashToUnderscore;
		[Test]
		procedure TestBase64ToSafeMixedCharacters;
		[Test]
		procedure TestBase64ToSafeNoSpecialChars;
		[Test]
		procedure TestBase64ToSafeEmptyString;

		{ Base64FromSafe tests }
		[Test]
		procedure TestBase64FromSafeMinusToPlus;
		[Test]
		procedure TestBase64FromSafeUnderscoreToSlash;
		[Test]
		procedure TestBase64FromSafeMixedCharacters;
		[Test]
		procedure TestBase64FromSafeNoSpecialChars;
		[Test]
		procedure TestBase64FromSafeEmptyString;

		{ Roundtrip tests }
		[Test]
		procedure TestBase64RoundtripToSafeAndBack;
		[Test]
		procedure TestBase64RoundtripFromSafeAndBack;

		{ GetCryptedGUID tests }
		[Test]
		procedure TestGetCryptedGUIDNotEmpty;
		[Test]
		procedure TestGetCryptedGUIDDeterministic;
		[Test]
		procedure TestGetCryptedGUIDDifferentPasswords;

		{ CheckPasswordGUID tests }
		[Test]
		procedure TestCheckPasswordGUIDCorrectPassword;
		[Test]
		procedure TestCheckPasswordGUIDWrongPassword;
		[Test]
		procedure TestCheckPasswordGUIDEmptyPassword;
	end;

implementation

{ TFileCipherTestHelper }

function TFileCipherTestHelper.TestBase64ToSafe(const Base64: WideString): WideString;
begin
	Result := Self.Base64ToSafe(Base64);
end;

function TFileCipherTestHelper.TestBase64FromSafe(const Safe: WideString): WideString;
begin
	Result := Self.Base64FromSafe(Safe);
end;

{ Base64ToSafe tests - converts + to - and / to _ for URL/filename safety }

procedure TFileCipherTest.TestBase64ToSafePlusToMinus;
var
	Cipher: TFileCipherTestHelper;
begin
	Cipher := TFileCipherTestHelper.Create('testpassword');
	try
		Assert.AreEqual('abc-def', Cipher.TestBase64ToSafe('abc+def'));
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestBase64ToSafeSlashToUnderscore;
var
	Cipher: TFileCipherTestHelper;
begin
	Cipher := TFileCipherTestHelper.Create('testpassword');
	try
		Assert.AreEqual('abc_def', Cipher.TestBase64ToSafe('abc/def'));
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestBase64ToSafeMixedCharacters;
var
	Cipher: TFileCipherTestHelper;
begin
	Cipher := TFileCipherTestHelper.Create('testpassword');
	try
		Assert.AreEqual('a-b_c-d_e', Cipher.TestBase64ToSafe('a+b/c+d/e'));
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestBase64ToSafeNoSpecialChars;
var
	Cipher: TFileCipherTestHelper;
begin
	Cipher := TFileCipherTestHelper.Create('testpassword');
	try
		{ String without + or / should remain unchanged }
		Assert.AreEqual('abcdefgh', Cipher.TestBase64ToSafe('abcdefgh'));
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestBase64ToSafeEmptyString;
var
	Cipher: TFileCipherTestHelper;
begin
	Cipher := TFileCipherTestHelper.Create('testpassword');
	try
		Assert.AreEqual('', Cipher.TestBase64ToSafe(''));
	finally
		Cipher.Free;
	end;
end;

{ Base64FromSafe tests - converts - to + and _ to / }

procedure TFileCipherTest.TestBase64FromSafeMinusToPlus;
var
	Cipher: TFileCipherTestHelper;
begin
	Cipher := TFileCipherTestHelper.Create('testpassword');
	try
		Assert.AreEqual('abc+def', Cipher.TestBase64FromSafe('abc-def'));
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestBase64FromSafeUnderscoreToSlash;
var
	Cipher: TFileCipherTestHelper;
begin
	Cipher := TFileCipherTestHelper.Create('testpassword');
	try
		Assert.AreEqual('abc/def', Cipher.TestBase64FromSafe('abc_def'));
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestBase64FromSafeMixedCharacters;
var
	Cipher: TFileCipherTestHelper;
begin
	Cipher := TFileCipherTestHelper.Create('testpassword');
	try
		Assert.AreEqual('a+b/c+d/e', Cipher.TestBase64FromSafe('a-b_c-d_e'));
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestBase64FromSafeNoSpecialChars;
var
	Cipher: TFileCipherTestHelper;
begin
	Cipher := TFileCipherTestHelper.Create('testpassword');
	try
		{ String without - or _ should remain unchanged }
		Assert.AreEqual('abcdefgh', Cipher.TestBase64FromSafe('abcdefgh'));
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestBase64FromSafeEmptyString;
var
	Cipher: TFileCipherTestHelper;
begin
	Cipher := TFileCipherTestHelper.Create('testpassword');
	try
		Assert.AreEqual('', Cipher.TestBase64FromSafe(''));
	finally
		Cipher.Free;
	end;
end;

{ Roundtrip tests }

procedure TFileCipherTest.TestBase64RoundtripToSafeAndBack;
var
	Cipher: TFileCipherTestHelper;
	Original, Safe, Restored: WideString;
begin
	Cipher := TFileCipherTestHelper.Create('testpassword');
	try
		Original := 'abc+def/ghi+jkl/mno';
		Safe := Cipher.TestBase64ToSafe(Original);
		Restored := Cipher.TestBase64FromSafe(Safe);
		Assert.AreEqual(Original, Restored);
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestBase64RoundtripFromSafeAndBack;
var
	Cipher: TFileCipherTestHelper;
	Original, Base64, Restored: WideString;
begin
	Cipher := TFileCipherTestHelper.Create('testpassword');
	try
		Original := 'abc-def_ghi-jkl_mno';
		Base64 := Cipher.TestBase64FromSafe(Original);
		Restored := Cipher.TestBase64ToSafe(Base64);
		Assert.AreEqual(Original, Restored);
	finally
		Cipher.Free;
	end;
end;

{ GetCryptedGUID tests }

procedure TFileCipherTest.TestGetCryptedGUIDNotEmpty;
var
	GUID: WideString;
begin
	GUID := TFileCipher.GetCryptedGUID('testpassword');
	Assert.IsNotEmpty(GUID);
end;

procedure TFileCipherTest.TestGetCryptedGUIDDeterministic;
var
	GUID1, GUID2: WideString;
begin
	{ Same password should always produce same GUID }
	GUID1 := TFileCipher.GetCryptedGUID('mypassword');
	GUID2 := TFileCipher.GetCryptedGUID('mypassword');
	Assert.AreEqual(GUID1, GUID2);
end;

procedure TFileCipherTest.TestGetCryptedGUIDDifferentPasswords;
var
	GUID1, GUID2: WideString;
begin
	{ Different passwords should produce different GUIDs }
	GUID1 := TFileCipher.GetCryptedGUID('password1');
	GUID2 := TFileCipher.GetCryptedGUID('password2');
	Assert.AreNotEqual(GUID1, GUID2);
end;

{ CheckPasswordGUID tests }

procedure TFileCipherTest.TestCheckPasswordGUIDCorrectPassword;
var
	StoredGUID: WideString;
begin
	{ Generate GUID with password, then verify same password }
	StoredGUID := TFileCipher.GetCryptedGUID('correctpassword');
	Assert.IsTrue(TFileCipher.CheckPasswordGUID('correctpassword', StoredGUID));
end;

procedure TFileCipherTest.TestCheckPasswordGUIDWrongPassword;
var
	StoredGUID: WideString;
begin
	{ Generate GUID with one password, try to verify with different password }
	StoredGUID := TFileCipher.GetCryptedGUID('originalpassword');
	Assert.IsFalse(TFileCipher.CheckPasswordGUID('wrongpassword', StoredGUID));
end;

procedure TFileCipherTest.TestCheckPasswordGUIDEmptyPassword;
var
	StoredGUID: WideString;
begin
	{ Empty password should still work consistently }
	StoredGUID := TFileCipher.GetCryptedGUID('');
	Assert.IsTrue(TFileCipher.CheckPasswordGUID('', StoredGUID));
end;

initialization

TDUnitX.RegisterTestFixture(TFileCipherTest);

end.
