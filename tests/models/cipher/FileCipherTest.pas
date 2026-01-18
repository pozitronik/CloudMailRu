unit FileCipherTest;

interface

uses
	FileCipher,
	CipherInterface,
	System.SysUtils,
	System.IOUtils,
	System.Classes,
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
		{ Interface implementation test }
		[Test]
		procedure TestImplementsICipher;

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

		{ CryptFileName/DecryptFileName empty input tests - verify early exit cleanup }
		[Test]
		procedure TestCryptFileNameEmptyInput;
		[Test]
		procedure TestDecryptFileNameEmptyInput;
		[Test]
		procedure TestCryptFileNameValidInput;
		[Test]
		procedure TestDecryptFileNameValidInput;

		{ CryptFile/DecryptFile tests - verify stream cleanup on exceptions }
		[Test]
		procedure TestCryptFileNonExistentSource;
		[Test]
		procedure TestCryptFileInvalidDestination;
		[Test]
		procedure TestDecryptFileNonExistentSource;
		[Test]
		procedure TestDecryptFileInvalidDestination;
		[Test]
		procedure TestCryptDecryptFileRoundtrip;
		[Test]
		procedure TestCryptDecryptFileEmptyFile;

		{ CryptStream/DecryptStream tests - verify stream operations }
		[Test]
		procedure TestCryptDecryptStreamRoundtrip;
		[Test]
		procedure TestCryptDecryptStreamEmptyStream;
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

{ Interface implementation test }

procedure TFileCipherTest.TestImplementsICipher;
var
	Cipher: ICipher;
begin
	Cipher := TFileCipher.Create('testpassword');
	Assert.IsNotNull(Cipher);
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

{ CryptFileName/DecryptFileName tests - verify early exit cleanup }

procedure TFileCipherTest.TestCryptFileNameEmptyInput;
var
	Cipher: TFileCipher;
	Result: WideString;
begin
	{ Empty filename should return empty string without leaking cipher resources }
	Cipher := TFileCipher.Create('testpassword', '', true);
	try
		Result := Cipher.CryptFileName('');
		Assert.AreEqual('', Result);
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestDecryptFileNameEmptyInput;
var
	Cipher: TFileCipher;
	Result: WideString;
begin
	{ Empty filename should return empty string without leaking cipher resources }
	Cipher := TFileCipher.Create('testpassword', '', true);
	try
		Result := Cipher.DecryptFileName('');
		Assert.AreEqual('', Result);
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestCryptFileNameValidInput;
var
	Cipher: TFileCipher;
	Encrypted: WideString;
begin
	{ Valid filename should be encrypted when DoFilenameCipher is true }
	Cipher := TFileCipher.Create('testpassword', '', true);
	try
		Encrypted := Cipher.CryptFileName('testfile.txt');
		Assert.IsNotEmpty(Encrypted);
		Assert.AreNotEqual('testfile.txt', Encrypted, 'Filename should be encrypted');
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestDecryptFileNameValidInput;
var
	Cipher: TFileCipher;
	Encrypted, Decrypted: WideString;
begin
	{ Encrypted filename should decrypt back to original }
	Cipher := TFileCipher.Create('testpassword', '', true);
	try
		Encrypted := Cipher.CryptFileName('myfile.dat');
		Decrypted := Cipher.DecryptFileName(Encrypted);
		Assert.AreEqual('myfile.dat', Decrypted);
	finally
		Cipher.Free;
	end;
end;

{ CryptFile/DecryptFile tests - these prove stream leak fix works }

procedure TFileCipherTest.TestCryptFileNonExistentSource;
var
	Cipher: TFileCipher;
	ResultCode: Integer;
begin
	{ Non-existent source file should return CIPHER_IO_ERROR without leaking streams.
	  Before the fix, streams would leak on exception because Free was not in finally block. }
	Cipher := TFileCipher.Create('testpassword');
	try
		ResultCode := Cipher.CryptFile('C:\NonExistent\File\That\Does\Not\Exist.txt', TPath.GetTempFileName);
		Assert.AreEqual(CIPHER_IO_ERROR, ResultCode);
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestCryptFileInvalidDestination;
var
	Cipher: TFileCipher;
	SourceFile: string;
	ResultCode: Integer;
begin
	{ Invalid destination path should return CIPHER_IO_ERROR without leaking streams.
	  Before the fix, SourceStream would leak if DestinationStream creation failed. }
	SourceFile := TPath.GetTempFileName;
	try
		TFile.WriteAllText(SourceFile, 'test content');
		Cipher := TFileCipher.Create('testpassword');
		try
			ResultCode := Cipher.CryptFile(SourceFile, 'Z:\Invalid\Path\That\Cannot\Be\Created\output.enc');
			Assert.AreEqual(CIPHER_IO_ERROR, ResultCode);
		finally
			Cipher.Free;
		end;
	finally
		if TFile.Exists(SourceFile) then
			TFile.Delete(SourceFile);
	end;
end;

procedure TFileCipherTest.TestDecryptFileNonExistentSource;
var
	Cipher: TFileCipher;
	ResultCode: Integer;
begin
	{ Non-existent source file should return CIPHER_IO_ERROR without leaking streams.
	  Before the fix, streams would leak on exception because Free was not in finally block. }
	Cipher := TFileCipher.Create('testpassword');
	try
		ResultCode := Cipher.DecryptFile('C:\NonExistent\File\That\Does\Not\Exist.enc', TPath.GetTempFileName);
		Assert.AreEqual(CIPHER_IO_ERROR, ResultCode);
	finally
		Cipher.Free;
	end;
end;

procedure TFileCipherTest.TestDecryptFileInvalidDestination;
var
	Cipher: TFileCipher;
	SourceFile: string;
	ResultCode: Integer;
begin
	{ Invalid destination path should return CIPHER_IO_ERROR without leaking streams.
	  Before the fix, SourceStream would leak if DestinationStream creation failed. }
	SourceFile := TPath.GetTempFileName;
	try
		TFile.WriteAllText(SourceFile, 'encrypted content placeholder');
		Cipher := TFileCipher.Create('testpassword');
		try
			ResultCode := Cipher.DecryptFile(SourceFile, 'Z:\Invalid\Path\That\Cannot\Be\Created\output.txt');
			Assert.AreEqual(CIPHER_IO_ERROR, ResultCode);
		finally
			Cipher.Free;
		end;
	finally
		if TFile.Exists(SourceFile) then
			TFile.Delete(SourceFile);
	end;
end;

procedure TFileCipherTest.TestCryptDecryptFileRoundtrip;
var
	Cipher: TFileCipher;
	SourceFile, EncryptedFile, DecryptedFile: string;
	OriginalContent, DecryptedContent: string;
	CryptResult, DecryptResult: Integer;
begin
	{ Verify file encryption/decryption roundtrip works correctly }
	SourceFile := TPath.GetTempFileName;
	EncryptedFile := TPath.GetTempFileName;
	DecryptedFile := TPath.GetTempFileName;
	try
		OriginalContent := 'This is test content for encryption roundtrip testing. 1234567890!@#$%';
		TFile.WriteAllText(SourceFile, OriginalContent);

		Cipher := TFileCipher.Create('testpassword');
		try
			CryptResult := Cipher.CryptFile(SourceFile, EncryptedFile);
			Assert.AreEqual(CIPHER_OK, CryptResult, 'Encryption should succeed');

			DecryptResult := Cipher.DecryptFile(EncryptedFile, DecryptedFile);
			Assert.AreEqual(CIPHER_OK, DecryptResult, 'Decryption should succeed');

			DecryptedContent := TFile.ReadAllText(DecryptedFile);
			Assert.AreEqual(OriginalContent, DecryptedContent, 'Decrypted content should match original');
		finally
			Cipher.Free;
		end;
	finally
		if TFile.Exists(SourceFile) then TFile.Delete(SourceFile);
		if TFile.Exists(EncryptedFile) then TFile.Delete(EncryptedFile);
		if TFile.Exists(DecryptedFile) then TFile.Delete(DecryptedFile);
	end;
end;

procedure TFileCipherTest.TestCryptDecryptFileEmptyFile;
var
	Cipher: TFileCipher;
	SourceFile, EncryptedFile, DecryptedFile: string;
	CryptResult, DecryptResult: Integer;
begin
	{ Verify empty file handling - edge case where Size = 0 skips encryption }
	SourceFile := TPath.GetTempFileName;
	EncryptedFile := TPath.GetTempFileName;
	DecryptedFile := TPath.GetTempFileName;
	try
		TFile.WriteAllText(SourceFile, ''); { Empty file }

		Cipher := TFileCipher.Create('testpassword');
		try
			CryptResult := Cipher.CryptFile(SourceFile, EncryptedFile);
			Assert.AreEqual(CIPHER_OK, CryptResult, 'Empty file encryption should succeed');

			DecryptResult := Cipher.DecryptFile(EncryptedFile, DecryptedFile);
			Assert.AreEqual(CIPHER_OK, DecryptResult, 'Empty file decryption should succeed');

			Assert.AreEqual(Int64(0), TFile.GetSize(DecryptedFile), 'Decrypted file should be empty');
		finally
			Cipher.Free;
		end;
	finally
		if TFile.Exists(SourceFile) then TFile.Delete(SourceFile);
		if TFile.Exists(EncryptedFile) then TFile.Delete(EncryptedFile);
		if TFile.Exists(DecryptedFile) then TFile.Delete(DecryptedFile);
	end;
end;

{ CryptStream/DecryptStream tests }

procedure TFileCipherTest.TestCryptDecryptStreamRoundtrip;
var
	Cipher: TFileCipher;
	SourceStream, EncryptedStream, DecryptedStream: TMemoryStream;
	OriginalBytes, DecryptedBytes: TBytes;
begin
	{ Verify stream encryption/decryption roundtrip works correctly }
	SourceStream := TMemoryStream.Create;
	EncryptedStream := TMemoryStream.Create;
	DecryptedStream := TMemoryStream.Create;
	try
		OriginalBytes := TEncoding.UTF8.GetBytes('Stream test content for roundtrip verification');
		SourceStream.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		SourceStream.Position := 0;

		Cipher := TFileCipher.Create('testpassword');
		try
			Cipher.CryptStream(SourceStream, EncryptedStream);
			Assert.IsTrue(EncryptedStream.Size > 0, 'Encrypted stream should have content');

			EncryptedStream.Position := 0;
			Cipher.DecryptStream(EncryptedStream, DecryptedStream);

			SetLength(DecryptedBytes, DecryptedStream.Size);
			DecryptedStream.Position := 0;
			DecryptedStream.ReadBuffer(DecryptedBytes[0], DecryptedStream.Size);

			Assert.AreEqual(
				TEncoding.UTF8.GetString(OriginalBytes),
				TEncoding.UTF8.GetString(DecryptedBytes),
				'Decrypted stream content should match original'
			);
		finally
			Cipher.Free;
		end;
	finally
		SourceStream.Free;
		EncryptedStream.Free;
		DecryptedStream.Free;
	end;
end;

procedure TFileCipherTest.TestCryptDecryptStreamEmptyStream;
var
	Cipher: TFileCipher;
	SourceStream, EncryptedStream: TMemoryStream;
	BytesEncrypted: Integer;
begin
	{ Verify empty stream handling - Size = 0 should return 0 bytes encrypted }
	SourceStream := TMemoryStream.Create;
	EncryptedStream := TMemoryStream.Create;
	try
		{ SourceStream is empty (Size = 0) }
		Cipher := TFileCipher.Create('testpassword');
		try
			BytesEncrypted := Cipher.CryptStream(SourceStream, EncryptedStream);
			Assert.AreEqual(0, BytesEncrypted, 'Empty stream should encrypt 0 bytes');
			Assert.AreEqual(Int64(0), EncryptedStream.Size, 'Encrypted stream should be empty');
		finally
			Cipher.Free;
		end;
	finally
		SourceStream.Free;
		EncryptedStream.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TFileCipherTest);

end.
