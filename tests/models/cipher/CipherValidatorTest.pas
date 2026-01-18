unit CipherValidatorTest;

interface

uses
	ICipherValidatorInterface,
	CipherValidator,
	DUnitX.TestFramework;

type
	[TestFixture]
	TNullCipherValidatorTest = class
	public
		[Test]
		{Verifies TNullCipherValidator can be assigned to ICipherValidator variable}
		procedure TestImplementsICipherValidator;

		[Test]
		{Verifies CheckPasswordGUID always returns True}
		procedure TestCheckPasswordGUIDReturnsTrue;

		[Test]
		{Verifies CheckPasswordGUID returns True regardless of input}
		procedure TestCheckPasswordGUIDIgnoresInput;

		[Test]
		{Verifies GetCryptedGUID returns empty string}
		procedure TestGetCryptedGUIDReturnsEmpty;

		[Test]
		{Verifies multiple calls work correctly}
		procedure TestMultipleCalls;
	end;

	[TestFixture]
	TCipherValidatorTest = class
	public
		[Test]
		{Verifies TCipherValidator can be assigned to ICipherValidator variable}
		procedure TestImplementsICipherValidator;

		[Test]
		{Verifies GetCryptedGUID returns non-empty string for non-empty password}
		procedure TestGetCryptedGUIDReturnsNonEmpty;

		[Test]
		{Verifies GetCryptedGUID returns same result for same password}
		procedure TestGetCryptedGUIDConsistent;

		[Test]
		{Verifies GetCryptedGUID returns different results for different passwords}
		procedure TestGetCryptedGUIDDifferentPasswords;

		[Test]
		{Verifies CheckPasswordGUID validates correctly generated GUID}
		procedure TestCheckPasswordGUIDValidatesCorrectly;

		[Test]
		{Verifies CheckPasswordGUID rejects wrong GUID}
		procedure TestCheckPasswordGUIDRejectsWrong;
	end;

implementation

{TNullCipherValidatorTest}

procedure TNullCipherValidatorTest.TestImplementsICipherValidator;
var
	Validator: ICipherValidator;
begin
	Validator := TNullCipherValidator.Create;
	Assert.IsNotNull(Validator);
end;

procedure TNullCipherValidatorTest.TestCheckPasswordGUIDReturnsTrue;
var
	Validator: ICipherValidator;
begin
	Validator := TNullCipherValidator.Create;
	Assert.IsTrue(Validator.CheckPasswordGUID('password', 'guid'));
end;

procedure TNullCipherValidatorTest.TestCheckPasswordGUIDIgnoresInput;
var
	Validator: ICipherValidator;
begin
	Validator := TNullCipherValidator.Create;
	Assert.IsTrue(Validator.CheckPasswordGUID('', ''));
	Assert.IsTrue(Validator.CheckPasswordGUID('wrong', 'different'));
	Assert.IsTrue(Validator.CheckPasswordGUID('any', 'values'));
end;

procedure TNullCipherValidatorTest.TestGetCryptedGUIDReturnsEmpty;
var
	Validator: ICipherValidator;
begin
	Validator := TNullCipherValidator.Create;
	Assert.AreEqual('', Validator.GetCryptedGUID('password'));
end;

procedure TNullCipherValidatorTest.TestMultipleCalls;
var
	Validator: ICipherValidator;
begin
	Validator := TNullCipherValidator.Create;

	Assert.AreEqual('', Validator.GetCryptedGUID('pwd1'));
	Assert.AreEqual('', Validator.GetCryptedGUID('pwd2'));
	Assert.IsTrue(Validator.CheckPasswordGUID('a', 'b'));
	Assert.IsTrue(Validator.CheckPasswordGUID('c', 'd'));

	Assert.Pass('Multiple calls completed without exception');
end;

{TCipherValidatorTest}

procedure TCipherValidatorTest.TestImplementsICipherValidator;
var
	Validator: ICipherValidator;
begin
	Validator := TCipherValidator.Create;
	Assert.IsNotNull(Validator);
end;

procedure TCipherValidatorTest.TestGetCryptedGUIDReturnsNonEmpty;
var
	Validator: ICipherValidator;
	Result: WideString;
begin
	Validator := TCipherValidator.Create;
	Result := Validator.GetCryptedGUID('test_password');
	Assert.IsNotEmpty(Result);
end;

procedure TCipherValidatorTest.TestGetCryptedGUIDConsistent;
var
	Validator: ICipherValidator;
	Result1, Result2: WideString;
begin
	Validator := TCipherValidator.Create;
	Result1 := Validator.GetCryptedGUID('consistent_password');
	Result2 := Validator.GetCryptedGUID('consistent_password');
	Assert.AreEqual(Result1, Result2, 'Same password should produce same GUID');
end;

procedure TCipherValidatorTest.TestGetCryptedGUIDDifferentPasswords;
var
	Validator: ICipherValidator;
	Result1, Result2: WideString;
begin
	Validator := TCipherValidator.Create;
	Result1 := Validator.GetCryptedGUID('password_one');
	Result2 := Validator.GetCryptedGUID('password_two');
	Assert.AreNotEqual(Result1, Result2, 'Different passwords should produce different GUIDs');
end;

procedure TCipherValidatorTest.TestCheckPasswordGUIDValidatesCorrectly;
var
	Validator: ICipherValidator;
	Password: WideString;
	GUID: WideString;
begin
	Validator := TCipherValidator.Create;
	Password := 'my_secret_password';
	GUID := Validator.GetCryptedGUID(Password);

	Assert.IsTrue(Validator.CheckPasswordGUID(Password, GUID),
		'Password should validate against its own generated GUID');
end;

procedure TCipherValidatorTest.TestCheckPasswordGUIDRejectsWrong;
var
	Validator: ICipherValidator;
	Password: WideString;
	WrongGUID: WideString;
begin
	Validator := TCipherValidator.Create;
	Password := 'correct_password';
	WrongGUID := Validator.GetCryptedGUID('wrong_password');

	Assert.IsFalse(Validator.CheckPasswordGUID(Password, WrongGUID),
		'Password should not validate against a different password''s GUID');
end;

initialization

TDUnitX.RegisterTestFixture(TNullCipherValidatorTest);
TDUnitX.RegisterTestFixture(TCipherValidatorTest);

end.
