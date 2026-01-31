unit OpenSSLCipherTest;

{Tests for OpenSSL AES-256 CFB-8 cipher backend.
	Tests conditionally skip if OpenSSL cipher functions are not available.}

interface

uses
	OpenSSLCipher,
	OpenSSLProvider,
	BlockCipher,
	FileCipher,
	System.SysUtils,
	System.IOUtils,
	System.Classes,
	DUnitX.TestFramework;

type
	[TestFixture]
	TOpenSSLBlockCipherTest = class
	private
		FProvider: IOpenSSLProvider;
		function CipherAvailable: Boolean;
		function CreateBlockCipher: IBlockCipher;
	public
		[Setup]
		procedure Setup;

		{Basic operations}
		[Test]
		procedure TestEncryptCFB8bit_ProducesOutput;
		[Test]
		procedure TestDecryptCFB8bit_ProducesOutput;
		[Test]
		procedure TestEncryptDecrypt_Roundtrip;

		{Reset restores cipher state}
		[Test]
		procedure TestReset_ProducesSameOutput;

		{Burn clears state}
		[Test]
		procedure TestBurn_CompletesWithoutError;
	end;

	[TestFixture]
	TOpenSSLCipherTest = class
	private
		FProvider: IOpenSSLProvider;
		function CipherAvailable: Boolean;
	public
		[Setup]
		procedure Setup;

		{Stream roundtrip}
		[Test]
		procedure TestCryptDecryptStreamRoundtrip;
		[Test]
		procedure TestCryptDecryptStreamEmptyStream;

		{File roundtrip}
		[Test]
		procedure TestCryptDecryptFileRoundtrip;
		[Test]
		procedure TestCryptFileNonExistentSource;

		{Different passwords produce different output}
		[Test]
		procedure TestDifferentPasswords_DifferentCiphertext;

		{GUID validation uses legacy AES/SHA-1}
		[Test]
		procedure TestGUIDValidation_UsesLegacyAlgorithm;

		{Interface compliance}
		[Test]
		procedure TestImplementsICipher;
	end;

implementation

{TOpenSSLBlockCipherTest}

procedure TOpenSSLBlockCipherTest.Setup;
begin
	FProvider := TOpenSSLProvider.Create('', False);
end;

function TOpenSSLBlockCipherTest.CipherAvailable: Boolean;
begin
	Result := FProvider.IsAvailable and FProvider.GetFunctions.CipherLoaded;
end;

function TOpenSSLBlockCipherTest.CreateBlockCipher: IBlockCipher;
var
	Key, IV: TBytes;
begin
	SetLength(Key, OPENSSL_AES256_KEY_SIZE);
	SetLength(IV, OPENSSL_AES256_IV_SIZE);
	FillChar(Key[0], OPENSSL_AES256_KEY_SIZE, $42);
	FillChar(IV[0], OPENSSL_AES256_IV_SIZE, 0);
	Result := TOpenSSLBlockCipher.Create(FProvider.GetFunctions, Key, IV);
end;

procedure TOpenSSLBlockCipherTest.TestEncryptCFB8bit_ProducesOutput;
var
	Adapter: IBlockCipher;
	Data, Original: TBytes;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	Adapter := CreateBlockCipher;

	SetLength(Data, 16);
	SetLength(Original, 16);
	FillChar(Data[0], 16, $55);
	Move(Data[0], Original[0], 16);

	Adapter.EncryptCFB8bit(Data[0], Data[0], 16);

	Assert.AreNotEqual(Original, Data, 'EncryptCFB8bit should transform data');
end;

procedure TOpenSSLBlockCipherTest.TestDecryptCFB8bit_ProducesOutput;
var
	Adapter: IBlockCipher;
	Data, Original: TBytes;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	Adapter := CreateBlockCipher;

	SetLength(Data, 16);
	SetLength(Original, 16);
	FillChar(Data[0], 16, $55);
	Move(Data[0], Original[0], 16);

	Adapter.DecryptCFB8bit(Data[0], Data[0], 16);

	Assert.AreNotEqual(Original, Data, 'DecryptCFB8bit should transform data');
end;

procedure TOpenSSLBlockCipherTest.TestEncryptDecrypt_Roundtrip;
var
	EncAdapter, DecAdapter: IBlockCipher;
	Original, Encrypted, Decrypted: TBytes;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	SetLength(Original, 64);
	SetLength(Encrypted, 64);
	SetLength(Decrypted, 64);
	FillChar(Original[0], 64, $AB);

	{Encrypt with one instance}
	EncAdapter := CreateBlockCipher;
	Move(Original[0], Encrypted[0], 64);
	EncAdapter.EncryptCFB8bit(Encrypted[0], Encrypted[0], 64);

	{Decrypt with a fresh instance (same key/IV)}
	DecAdapter := CreateBlockCipher;
	Move(Encrypted[0], Decrypted[0], 64);
	DecAdapter.DecryptCFB8bit(Decrypted[0], Decrypted[0], 64);

	Assert.AreEqual(Original, Decrypted, 'Roundtrip encrypt/decrypt should restore original data');
end;

procedure TOpenSSLBlockCipherTest.TestReset_ProducesSameOutput;
var
	Adapter: IBlockCipher;
	Data1, Data2, Source: TBytes;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	Adapter := CreateBlockCipher;

	SetLength(Source, 32);
	SetLength(Data1, 32);
	SetLength(Data2, 32);
	FillChar(Source[0], 32, $55);

	{First pass}
	Move(Source[0], Data1[0], 32);
	Adapter.EncryptCFB8bit(Data1[0], Data1[0], 32);

	{Reset and encrypt same data}
	Adapter.Reset;
	Move(Source[0], Data2[0], 32);
	Adapter.EncryptCFB8bit(Data2[0], Data2[0], 32);

	Assert.AreEqual(Data1, Data2, 'After Reset, encrypting same data should produce same output');
end;

procedure TOpenSSLBlockCipherTest.TestBurn_CompletesWithoutError;
var
	Adapter: IBlockCipher;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	Adapter := CreateBlockCipher;
	Adapter.Burn;
	Assert.Pass('Burn completed without error');
end;

{TOpenSSLCipherTest}

procedure TOpenSSLCipherTest.Setup;
begin
	FProvider := TOpenSSLProvider.Create('', False);
end;

function TOpenSSLCipherTest.CipherAvailable: Boolean;
begin
	Result := FProvider.IsAvailable and FProvider.GetFunctions.CipherLoaded;
end;

procedure TOpenSSLCipherTest.TestCryptDecryptStreamRoundtrip;
var
	Cipher: TOpenSSLCipher;
	SourceStream, EncryptedStream, DecryptedStream: TMemoryStream;
	OriginalBytes, DecryptedBytes: TBytes;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	SourceStream := TMemoryStream.Create;
	EncryptedStream := TMemoryStream.Create;
	DecryptedStream := TMemoryStream.Create;
	try
		OriginalBytes := TEncoding.UTF8.GetBytes('OpenSSL stream roundtrip test content for verification');
		SourceStream.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		SourceStream.Position := 0;

		Cipher := TOpenSSLCipher.Create('testpassword', FProvider.GetFunctions);
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

procedure TOpenSSLCipherTest.TestCryptDecryptStreamEmptyStream;
var
	Cipher: TOpenSSLCipher;
	SourceStream, EncryptedStream: TMemoryStream;
	BytesEncrypted: Integer;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	SourceStream := TMemoryStream.Create;
	EncryptedStream := TMemoryStream.Create;
	try
		Cipher := TOpenSSLCipher.Create('testpassword', FProvider.GetFunctions);
		try
			BytesEncrypted := Cipher.CryptStream(SourceStream, EncryptedStream);
			Assert.AreEqual(0, BytesEncrypted, 'Empty stream should encrypt 0 bytes');
		finally
			Cipher.Free;
		end;
	finally
		SourceStream.Free;
		EncryptedStream.Free;
	end;
end;

procedure TOpenSSLCipherTest.TestCryptDecryptFileRoundtrip;
var
	Cipher: TOpenSSLCipher;
	SourceFile, EncryptedFile, DecryptedFile: string;
	OriginalContent, DecryptedContent: string;
	CryptResult, DecryptResult: Integer;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	SourceFile := TPath.GetTempFileName;
	EncryptedFile := TPath.GetTempFileName;
	DecryptedFile := TPath.GetTempFileName;
	try
		OriginalContent := 'OpenSSL file encryption roundtrip test 1234567890!@#$%';
		TFile.WriteAllText(SourceFile, OriginalContent);

		Cipher := TOpenSSLCipher.Create('testpassword', FProvider.GetFunctions);
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

procedure TOpenSSLCipherTest.TestCryptFileNonExistentSource;
var
	Cipher: TOpenSSLCipher;
	ResultCode: Integer;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	Cipher := TOpenSSLCipher.Create('testpassword', FProvider.GetFunctions);
	try
		ResultCode := Cipher.CryptFile('C:\NonExistent\File\That\Does\Not\Exist.txt', TPath.GetTempFileName);
		Assert.AreEqual(CIPHER_IO_ERROR, ResultCode);
	finally
		Cipher.Free;
	end;
end;

procedure TOpenSSLCipherTest.TestDifferentPasswords_DifferentCiphertext;
var
	Cipher1, Cipher2: TOpenSSLCipher;
	Source1, Source2, Enc1, Enc2: TMemoryStream;
	TestData: TBytes;
	EncBytes1, EncBytes2: TBytes;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	TestData := TEncoding.UTF8.GetBytes('Same plaintext for both ciphers');

	Source1 := TMemoryStream.Create;
	Source2 := TMemoryStream.Create;
	Enc1 := TMemoryStream.Create;
	Enc2 := TMemoryStream.Create;
	try
		Source1.WriteBuffer(TestData[0], Length(TestData));
		Source2.WriteBuffer(TestData[0], Length(TestData));

		Cipher1 := TOpenSSLCipher.Create('password1', FProvider.GetFunctions);
		try
			Source1.Position := 0;
			Cipher1.CryptStream(Source1, Enc1);
		finally
			Cipher1.Free;
		end;

		Cipher2 := TOpenSSLCipher.Create('password2', FProvider.GetFunctions);
		try
			Source2.Position := 0;
			Cipher2.CryptStream(Source2, Enc2);
		finally
			Cipher2.Free;
		end;

		SetLength(EncBytes1, Enc1.Size);
		Enc1.Position := 0;
		Enc1.ReadBuffer(EncBytes1[0], Enc1.Size);

		SetLength(EncBytes2, Enc2.Size);
		Enc2.Position := 0;
		Enc2.ReadBuffer(EncBytes2[0], Enc2.Size);

		Assert.AreNotEqual(EncBytes1, EncBytes2, 'Different passwords must produce different ciphertext');
	finally
		Source1.Free;
		Source2.Free;
		Enc1.Free;
		Enc2.Free;
	end;
end;

procedure TOpenSSLCipherTest.TestGUIDValidation_UsesLegacyAlgorithm;
var
	Cipher: TOpenSSLCipher;
	StoredGUID: WideString;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	{GUID generated with legacy AES/SHA-1 must validate in OpenSSL backend}
	StoredGUID := TFileCipher.GetCryptedGUID('testpassword');

	Cipher := TOpenSSLCipher.Create('testpassword', FProvider.GetFunctions, StoredGUID);
	try
		Assert.IsFalse(Cipher.IsWrongPassword, 'GUID validation should succeed with correct password');
	finally
		Cipher.Free;
	end;
end;

procedure TOpenSSLCipherTest.TestImplementsICipher;
var
	Cipher: ICipher;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	Cipher := TOpenSSLCipher.Create('testpassword', FProvider.GetFunctions);
	Assert.IsNotNull(Cipher, 'TOpenSSLCipher should implement ICipher');
end;

initialization

TDUnitX.RegisterTestFixture(TOpenSSLBlockCipherTest);
TDUnitX.RegisterTestFixture(TOpenSSLCipherTest);

end.
