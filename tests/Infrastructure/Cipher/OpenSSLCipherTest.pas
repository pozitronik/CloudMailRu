unit OpenSSLCipherTest;

{Tests for OpenSSL AES-256 CFB-8 cipher backend.
	Tests conditionally skip if OpenSSL cipher functions are not available.}

interface

uses
	OpenSSLCipher,
	OpenSSLProvider,
	BlockCipher,
	Cipher,
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

		{Different passwords produce different output}
		[Test]
		procedure TestDifferentPasswords_DifferentCiphertext;

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

procedure TOpenSSLCipherTest.TestDifferentPasswords_DifferentCiphertext;
var
	Cipher1, Cipher2: ICipher;
	Source1, Source2: TMemoryStream;
	Enc1, Enc2: TStream;
	TestData, EncBytes1, EncBytes2: TBytes;
	BytesRead: Integer;
begin
	if not CipherAvailable then
	begin
		Assert.Pass('Skipped - OpenSSL cipher functions not available');
		Exit;
	end;

	TestData := TEncoding.UTF8.GetBytes('Same plaintext for both ciphers');

	Source1 := TMemoryStream.Create;
	Source2 := TMemoryStream.Create;
	try
		Source1.WriteBuffer(TestData[0], Length(TestData));
		Source1.Position := 0;
		Source2.WriteBuffer(TestData[0], Length(TestData));
		Source2.Position := 0;

		Cipher1 := TOpenSSLCipher.Create('password1', FProvider.GetFunctions);
		Enc1 := Cipher1.GetEncryptingStream(Source1);
		try
			SetLength(EncBytes1, Enc1.Size);
			BytesRead := Enc1.Read(EncBytes1[0], Length(EncBytes1));
			SetLength(EncBytes1, BytesRead);
		finally
			Enc1.Free;
		end;

		Cipher2 := TOpenSSLCipher.Create('password2', FProvider.GetFunctions);
		Enc2 := Cipher2.GetEncryptingStream(Source2);
		try
			SetLength(EncBytes2, Enc2.Size);
			BytesRead := Enc2.Read(EncBytes2[0], Length(EncBytes2));
			SetLength(EncBytes2, BytesRead);
		finally
			Enc2.Free;
		end;

		Assert.AreNotEqual(EncBytes1, EncBytes2, 'Different passwords must produce different ciphertext');
	finally
		Source1.Free;
		Source2.Free;
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
