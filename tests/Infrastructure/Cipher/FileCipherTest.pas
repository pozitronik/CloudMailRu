unit FileCipherTest;

interface

uses
	FileCipher,
	DCPcrypt2,
	DCPblockciphers,
	DCPrijndael,
	DCPsha1,
	DCPsha256,
	DCPtwofish,
	System.SysUtils,
	System.IOUtils,
	System.Classes,
	DUnitX.TestFramework;

type
	[TestFixture]
	TFileCipherTest = class
	public
		{ Interface implementation test }
		[Test]
		procedure TestImplementsICipher;

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

		{ Cipher algorithm tests }
		[Test]
		procedure TestConstructWithTwofishProfile;
		[Test]
		procedure TestConstructWithSHA256Profile;
		[Test]
		procedure TestDifferentProfilesProduceDifferentCiphertext;
		[Test]
		procedure TestLegacyProfileBackwardCompatibility;
		[Test]
		procedure TestGUIDValidationStableAcrossProfiles;
	end;

implementation

{ Interface implementation test }

procedure TFileCipherTest.TestImplementsICipher;
var
	Cipher: ICipher;
begin
	Cipher := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
	Assert.IsNotNull(Cipher);
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

{ CryptFile/DecryptFile tests - these prove stream leak fix works }

procedure TFileCipherTest.TestCryptFileNonExistentSource;
var
	Cipher: TFileCipher;
	ResultCode: Integer;
begin
	{ Non-existent source file should return CIPHER_IO_ERROR without leaking streams.
	  Before the fix, streams would leak on exception because Free was not in finally block. }
	Cipher := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
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
		Cipher := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
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
	Cipher := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
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
		Cipher := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
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

		Cipher := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
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

		Cipher := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
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

		Cipher := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
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
		Cipher := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
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

{ Cipher algorithm tests }

procedure TFileCipherTest.TestConstructWithTwofishProfile;
var
	Cipher: TFileCipher;
	SourceFile, EncryptedFile, DecryptedFile: string;
	OriginalContent, DecryptedContent: string;
	CryptResult, DecryptResult: Integer;
begin
	{ Twofish-256 profile should encrypt and decrypt a file correctly }
	SourceFile := TPath.GetTempFileName;
	EncryptedFile := TPath.GetTempFileName;
	DecryptedFile := TPath.GetTempFileName;
	try
		OriginalContent := 'Twofish profile roundtrip test content 1234567890';
		TFile.WriteAllText(SourceFile, OriginalContent);

		Cipher := TFileCipher.Create('testpassword', TDCP_twofish, TDCP_sha256);
		try
			CryptResult := Cipher.CryptFile(SourceFile, EncryptedFile);
			Assert.AreEqual(CIPHER_OK, CryptResult, 'Twofish encryption should succeed');

			DecryptResult := Cipher.DecryptFile(EncryptedFile, DecryptedFile);
			Assert.AreEqual(CIPHER_OK, DecryptResult, 'Twofish decryption should succeed');

			DecryptedContent := TFile.ReadAllText(DecryptedFile);
			Assert.AreEqual(OriginalContent, DecryptedContent, 'Twofish decrypted content should match original');
		finally
			Cipher.Free;
		end;
	finally
		if TFile.Exists(SourceFile) then TFile.Delete(SourceFile);
		if TFile.Exists(EncryptedFile) then TFile.Delete(EncryptedFile);
		if TFile.Exists(DecryptedFile) then TFile.Delete(DecryptedFile);
	end;
end;

procedure TFileCipherTest.TestConstructWithSHA256Profile;
var
	Cipher: TFileCipher;
	SourceFile, EncryptedFile, DecryptedFile: string;
	OriginalContent, DecryptedContent: string;
	CryptResult, DecryptResult: Integer;
begin
	{ AES-256 with SHA-256 KDF should encrypt and decrypt a file correctly }
	SourceFile := TPath.GetTempFileName;
	EncryptedFile := TPath.GetTempFileName;
	DecryptedFile := TPath.GetTempFileName;
	try
		OriginalContent := 'AES-256/SHA-256 profile roundtrip test content 1234567890';
		TFile.WriteAllText(SourceFile, OriginalContent);

		Cipher := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha256);
		try
			CryptResult := Cipher.CryptFile(SourceFile, EncryptedFile);
			Assert.AreEqual(CIPHER_OK, CryptResult, 'AES-256/SHA-256 encryption should succeed');

			DecryptResult := Cipher.DecryptFile(EncryptedFile, DecryptedFile);
			Assert.AreEqual(CIPHER_OK, DecryptResult, 'AES-256/SHA-256 decryption should succeed');

			DecryptedContent := TFile.ReadAllText(DecryptedFile);
			Assert.AreEqual(OriginalContent, DecryptedContent, 'AES-256/SHA-256 decrypted content should match original');
		finally
			Cipher.Free;
		end;
	finally
		if TFile.Exists(SourceFile) then TFile.Delete(SourceFile);
		if TFile.Exists(EncryptedFile) then TFile.Delete(EncryptedFile);
		if TFile.Exists(DecryptedFile) then TFile.Delete(DecryptedFile);
	end;
end;

procedure TFileCipherTest.TestDifferentProfilesProduceDifferentCiphertext;
var
	CipherAES, CipherTwofish: TFileCipher;
	SourceFile, EncryptedAES, EncryptedTwofish: string;
	OriginalContent: string;
	AESBytes, TwofishBytes: TBytes;
begin
	{ Same plaintext encrypted with different profiles must produce different ciphertext }
	SourceFile := TPath.GetTempFileName;
	EncryptedAES := TPath.GetTempFileName;
	EncryptedTwofish := TPath.GetTempFileName;
	try
		OriginalContent := 'Content to verify different profiles produce different ciphertext';
		TFile.WriteAllText(SourceFile, OriginalContent);

		CipherAES := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha256);
		try
			Assert.AreEqual(CIPHER_OK, CipherAES.CryptFile(SourceFile, EncryptedAES), 'AES encryption should succeed');
		finally
			CipherAES.Free;
		end;

		CipherTwofish := TFileCipher.Create('testpassword', TDCP_twofish, TDCP_sha256);
		try
			Assert.AreEqual(CIPHER_OK, CipherTwofish.CryptFile(SourceFile, EncryptedTwofish), 'Twofish encryption should succeed');
		finally
			CipherTwofish.Free;
		end;

		AESBytes := TFile.ReadAllBytes(EncryptedAES);
		TwofishBytes := TFile.ReadAllBytes(EncryptedTwofish);

		Assert.AreNotEqual(Integer(0), Integer(Length(AESBytes)), 'AES encrypted file should not be empty');
		Assert.AreNotEqual(Integer(0), Integer(Length(TwofishBytes)), 'Twofish encrypted file should not be empty');
		Assert.AreNotEqual(TEncoding.ANSI.GetString(AESBytes), TEncoding.ANSI.GetString(TwofishBytes),
			'Different profiles must produce different ciphertext');
	finally
		if TFile.Exists(SourceFile) then TFile.Delete(SourceFile);
		if TFile.Exists(EncryptedAES) then TFile.Delete(EncryptedAES);
		if TFile.Exists(EncryptedTwofish) then TFile.Delete(EncryptedTwofish);
	end;
end;

procedure TFileCipherTest.TestLegacyProfileBackwardCompatibility;
var
	CipherFirst, CipherSecond: TFileCipher;
	SourceFile, EncryptedFirst, EncryptedSecond: string;
	OriginalContent: string;
	FirstBytes, SecondBytes: TBytes;
begin
	{ Two separate AES/SHA-1 instances with same password must produce byte-identical output,
		ensuring deterministic encryption behavior for backward compatibility }
	SourceFile := TPath.GetTempFileName;
	EncryptedFirst := TPath.GetTempFileName;
	EncryptedSecond := TPath.GetTempFileName;
	try
		OriginalContent := 'Legacy backward compatibility test content';
		TFile.WriteAllText(SourceFile, OriginalContent);

		CipherFirst := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
		try
			Assert.AreEqual(CIPHER_OK, CipherFirst.CryptFile(SourceFile, EncryptedFirst),
				'First instance encryption should succeed');
		finally
			CipherFirst.Free;
		end;

		CipherSecond := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
		try
			Assert.AreEqual(CIPHER_OK, CipherSecond.CryptFile(SourceFile, EncryptedSecond),
				'Second instance encryption should succeed');
		finally
			CipherSecond.Free;
		end;

		FirstBytes := TFile.ReadAllBytes(EncryptedFirst);
		SecondBytes := TFile.ReadAllBytes(EncryptedSecond);

		Assert.AreEqual(Length(FirstBytes), Length(SecondBytes),
			'Encrypted files should have identical length');
		Assert.AreEqual(TEncoding.ANSI.GetString(FirstBytes), TEncoding.ANSI.GetString(SecondBytes),
			'Same cipher class refs must produce byte-identical output across instances');
	finally
		if TFile.Exists(SourceFile) then TFile.Delete(SourceFile);
		if TFile.Exists(EncryptedFirst) then TFile.Delete(EncryptedFirst);
		if TFile.Exists(EncryptedSecond) then TFile.Delete(EncryptedSecond);
	end;
end;

procedure TFileCipherTest.TestGUIDValidationStableAcrossProfiles;
var
	Cipher: TFileCipher;
	StoredGUID: WideString;
begin
	{ GetCryptedGUID uses hardcoded AES/SHA-1 regardless of profile.
		A GUID generated the standard way must validate even when the cipher
		instance was created with a different profile (Twofish). }
	StoredGUID := TFileCipher.GetCryptedGUID('testpassword');

	Cipher := TFileCipher.Create('testpassword', TDCP_twofish, TDCP_sha256, StoredGUID);
	try
		Assert.IsFalse(Cipher.IsWrongPassword,
			'GUID validation should succeed regardless of cipher profile');
	finally
		Cipher.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TFileCipherTest);

end.
