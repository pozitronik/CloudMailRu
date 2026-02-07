unit CipherTest;

interface

uses
	Cipher,
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

		{ Cipher algorithm tests via GetEncryptingStream/GetDecryptingStream }
		[Test]
		procedure TestConstructWithTwofishProfile;
		[Test]
		procedure TestConstructWithSHA256Profile;
		[Test]
		procedure TestDifferentProfilesProduceDifferentCiphertext;
		[Test]
		procedure TestLegacyProfileBackwardCompatibility;
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

{ Cipher algorithm tests via GetEncryptingStream/GetDecryptingStream }

procedure TFileCipherTest.TestConstructWithTwofishProfile;
var
	Cipher: ICipher;
	SourceStream, EncryptedBuf: TMemoryStream;
	EncStream, DecStream: TStream;
	OriginalBytes, DecryptedBytes: TBytes;
	BytesRead: Integer;
begin
	{ Twofish-256 profile should encrypt and decrypt via streams correctly }
	SourceStream := TMemoryStream.Create;
	EncryptedBuf := TMemoryStream.Create;
	try
		OriginalBytes := TEncoding.UTF8.GetBytes('Twofish profile roundtrip test content 1234567890');
		SourceStream.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		SourceStream.Position := 0;

		Cipher := TFileCipher.Create('testpassword', TDCP_twofish, TDCP_sha256);

		{Encrypt}
		EncStream := Cipher.GetEncryptingStream(SourceStream);
		try
			SetLength(DecryptedBytes, EncStream.Size);
			BytesRead := EncStream.Read(DecryptedBytes[0], Length(DecryptedBytes));
			Assert.IsTrue(BytesRead > 0, 'Twofish encryption should produce output');
			EncryptedBuf.WriteBuffer(DecryptedBytes[0], BytesRead);
		finally
			EncStream.Free;
		end;

		{Decrypt with fresh cipher instance}
		Cipher := TFileCipher.Create('testpassword', TDCP_twofish, TDCP_sha256);
		EncryptedBuf.Position := 0;
		DecStream := Cipher.GetDecryptingStream(EncryptedBuf);
		try
			SetLength(DecryptedBytes, DecStream.Size);
			BytesRead := DecStream.Read(DecryptedBytes[0], Length(DecryptedBytes));

			Assert.AreEqual(
				TEncoding.UTF8.GetString(OriginalBytes),
				TEncoding.UTF8.GetString(Copy(DecryptedBytes, 0, BytesRead)),
				'Twofish decrypted content should match original'
			);
		finally
			DecStream.Free;
		end;
	finally
		SourceStream.Free;
		EncryptedBuf.Free;
	end;
end;

procedure TFileCipherTest.TestConstructWithSHA256Profile;
var
	Cipher: ICipher;
	SourceStream, EncryptedBuf: TMemoryStream;
	EncStream, DecStream: TStream;
	OriginalBytes, DecryptedBytes: TBytes;
	BytesRead: Integer;
begin
	{ AES-256 with SHA-256 KDF should encrypt and decrypt via streams correctly }
	SourceStream := TMemoryStream.Create;
	EncryptedBuf := TMemoryStream.Create;
	try
		OriginalBytes := TEncoding.UTF8.GetBytes('AES-256/SHA-256 profile roundtrip test content 1234567890');
		SourceStream.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		SourceStream.Position := 0;

		Cipher := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha256);

		{Encrypt}
		EncStream := Cipher.GetEncryptingStream(SourceStream);
		try
			SetLength(DecryptedBytes, EncStream.Size);
			BytesRead := EncStream.Read(DecryptedBytes[0], Length(DecryptedBytes));
			Assert.IsTrue(BytesRead > 0, 'AES-256/SHA-256 encryption should produce output');
			EncryptedBuf.WriteBuffer(DecryptedBytes[0], BytesRead);
		finally
			EncStream.Free;
		end;

		{Decrypt with fresh cipher instance}
		Cipher := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha256);
		EncryptedBuf.Position := 0;
		DecStream := Cipher.GetDecryptingStream(EncryptedBuf);
		try
			SetLength(DecryptedBytes, DecStream.Size);
			BytesRead := DecStream.Read(DecryptedBytes[0], Length(DecryptedBytes));

			Assert.AreEqual(
				TEncoding.UTF8.GetString(OriginalBytes),
				TEncoding.UTF8.GetString(Copy(DecryptedBytes, 0, BytesRead)),
				'AES-256/SHA-256 decrypted content should match original'
			);
		finally
			DecStream.Free;
		end;
	finally
		SourceStream.Free;
		EncryptedBuf.Free;
	end;
end;

procedure TFileCipherTest.TestDifferentProfilesProduceDifferentCiphertext;
var
	CipherAES, CipherTwofish: ICipher;
	Source1, Source2: TMemoryStream;
	Enc1, Enc2: TStream;
	OriginalBytes, AESBytes, TwofishBytes: TBytes;
	BytesRead: Integer;
begin
	{ Same plaintext encrypted with different profiles must produce different ciphertext }
	OriginalBytes := TEncoding.UTF8.GetBytes('Content to verify different profiles produce different ciphertext');

	Source1 := TMemoryStream.Create;
	Source2 := TMemoryStream.Create;
	try
		Source1.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		Source1.Position := 0;
		Source2.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		Source2.Position := 0;

		CipherAES := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha256);
		Enc1 := CipherAES.GetEncryptingStream(Source1);
		try
			SetLength(AESBytes, Enc1.Size);
			BytesRead := Enc1.Read(AESBytes[0], Length(AESBytes));
			SetLength(AESBytes, BytesRead);
		finally
			Enc1.Free;
		end;

		CipherTwofish := TFileCipher.Create('testpassword', TDCP_twofish, TDCP_sha256);
		Enc2 := CipherTwofish.GetEncryptingStream(Source2);
		try
			SetLength(TwofishBytes, Enc2.Size);
			BytesRead := Enc2.Read(TwofishBytes[0], Length(TwofishBytes));
			SetLength(TwofishBytes, BytesRead);
		finally
			Enc2.Free;
		end;

		Assert.AreNotEqual(Integer(0), Integer(Length(AESBytes)), 'AES encrypted data should not be empty');
		Assert.AreNotEqual(Integer(0), Integer(Length(TwofishBytes)), 'Twofish encrypted data should not be empty');
		Assert.AreNotEqual(TEncoding.ANSI.GetString(AESBytes), TEncoding.ANSI.GetString(TwofishBytes),
			'Different profiles must produce different ciphertext');
	finally
		Source1.Free;
		Source2.Free;
	end;
end;

procedure TFileCipherTest.TestLegacyProfileBackwardCompatibility;
var
	CipherFirst, CipherSecond: ICipher;
	Source1, Source2: TMemoryStream;
	Enc1, Enc2: TStream;
	OriginalBytes, FirstBytes, SecondBytes: TBytes;
	BytesRead: Integer;
begin
	{ Two separate AES/SHA-1 instances with same password must produce byte-identical output,
		ensuring deterministic encryption behavior for backward compatibility }
	OriginalBytes := TEncoding.UTF8.GetBytes('Legacy backward compatibility test content');

	Source1 := TMemoryStream.Create;
	Source2 := TMemoryStream.Create;
	try
		Source1.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		Source1.Position := 0;
		Source2.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		Source2.Position := 0;

		CipherFirst := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
		Enc1 := CipherFirst.GetEncryptingStream(Source1);
		try
			SetLength(FirstBytes, Enc1.Size);
			BytesRead := Enc1.Read(FirstBytes[0], Length(FirstBytes));
			SetLength(FirstBytes, BytesRead);
		finally
			Enc1.Free;
		end;

		CipherSecond := TFileCipher.Create('testpassword', TDCP_rijndael, TDCP_sha1);
		Enc2 := CipherSecond.GetEncryptingStream(Source2);
		try
			SetLength(SecondBytes, Enc2.Size);
			BytesRead := Enc2.Read(SecondBytes[0], Length(SecondBytes));
			SetLength(SecondBytes, BytesRead);
		finally
			Enc2.Free;
		end;

		Assert.AreEqual(Length(FirstBytes), Length(SecondBytes),
			'Encrypted data should have identical length');
		Assert.AreEqual(TEncoding.ANSI.GetString(FirstBytes), TEncoding.ANSI.GetString(SecondBytes),
			'Same cipher class refs must produce byte-identical output across instances');
	finally
		Source1.Free;
		Source2.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TFileCipherTest);

end.
