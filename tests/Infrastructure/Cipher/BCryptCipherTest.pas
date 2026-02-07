unit BCryptCipherTest;

{Tests for BCrypt AES-256 CFB-8 cipher backend.
	BCrypt should be available on all modern Windows systems.}

interface

uses
	BCryptCipher,
	BCryptProvider,
	BlockCipher,
	Cipher,
	System.SysUtils,
	System.IOUtils,
	System.Classes,
	DUnitX.TestFramework;

type
	[TestFixture]
	TBCryptBlockCipherTest = class
	private
		FProvider: IBCryptProvider;
		function ProviderAvailable: Boolean;
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

		{CFB-8 correctness: encrypt then decrypt restores original for various sizes}
		[Test]
		procedure TestRoundtrip_SingleByte;
		[Test]
		procedure TestRoundtrip_LargerThanBlockSize;

		{Reset restores cipher state}
		[Test]
		procedure TestReset_ProducesSameOutput;

		{Burn clears state}
		[Test]
		procedure TestBurn_CompletesWithoutError;
	end;

	[TestFixture]
	TBCryptCipherTest = class
	private
		FProvider: IBCryptProvider;
		function ProviderAvailable: Boolean;
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

{TBCryptBlockCipherTest}

procedure TBCryptBlockCipherTest.Setup;
begin
	FProvider := TBCryptProvider.Create;
end;

function TBCryptBlockCipherTest.ProviderAvailable: Boolean;
begin
	Result := FProvider.IsAvailable;
end;

function TBCryptBlockCipherTest.CreateBlockCipher: IBlockCipher;
var
	Key, IV: TBytes;
begin
	Key := FProvider.DeriveKey('testkey');
	SetLength(IV, BCRYPT_AES_BLOCK_SIZE);
	FillChar(IV[0], BCRYPT_AES_BLOCK_SIZE, 0);
	Result := TBCryptBlockCipher.Create(FProvider, Key, IV);
end;

procedure TBCryptBlockCipherTest.TestEncryptCFB8bit_ProducesOutput;
var
	Cipher: IBlockCipher;
	Data, Original: TBytes;
begin
	if not ProviderAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	Cipher := CreateBlockCipher;
	SetLength(Data, 16);
	SetLength(Original, 16);
	FillChar(Data[0], 16, $55);
	Move(Data[0], Original[0], 16);

	Cipher.EncryptCFB8bit(Data[0], Data[0], 16);

	Assert.AreNotEqual(Original, Data, 'EncryptCFB8bit should transform data');
end;

procedure TBCryptBlockCipherTest.TestDecryptCFB8bit_ProducesOutput;
var
	Cipher: IBlockCipher;
	Data, Original: TBytes;
begin
	if not ProviderAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	Cipher := CreateBlockCipher;
	SetLength(Data, 16);
	SetLength(Original, 16);
	FillChar(Data[0], 16, $55);
	Move(Data[0], Original[0], 16);

	Cipher.DecryptCFB8bit(Data[0], Data[0], 16);

	Assert.AreNotEqual(Original, Data, 'DecryptCFB8bit should transform data');
end;

procedure TBCryptBlockCipherTest.TestEncryptDecrypt_Roundtrip;
var
	EncCipher, DecCipher: IBlockCipher;
	Original, Encrypted, Decrypted: TBytes;
begin
	if not ProviderAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	SetLength(Original, 64);
	SetLength(Encrypted, 64);
	SetLength(Decrypted, 64);
	FillChar(Original[0], 64, $AB);

	EncCipher := CreateBlockCipher;
	Move(Original[0], Encrypted[0], 64);
	EncCipher.EncryptCFB8bit(Encrypted[0], Encrypted[0], 64);

	DecCipher := CreateBlockCipher;
	Move(Encrypted[0], Decrypted[0], 64);
	DecCipher.DecryptCFB8bit(Decrypted[0], Decrypted[0], 64);

	Assert.AreEqual(Original, Decrypted, 'Roundtrip encrypt/decrypt should restore original data');
end;

procedure TBCryptBlockCipherTest.TestRoundtrip_SingleByte;
var
	EncCipher, DecCipher: IBlockCipher;
	Original, Encrypted, Decrypted: Byte;
begin
	if not ProviderAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	Original := $42;

	EncCipher := CreateBlockCipher;
	Encrypted := Original;
	EncCipher.EncryptCFB8bit(Encrypted, Encrypted, 1);

	DecCipher := CreateBlockCipher;
	Decrypted := Encrypted;
	DecCipher.DecryptCFB8bit(Decrypted, Decrypted, 1);

	Assert.AreEqual(Original, Decrypted, 'Single byte roundtrip should restore original');
end;

procedure TBCryptBlockCipherTest.TestRoundtrip_LargerThanBlockSize;
var
	EncCipher, DecCipher: IBlockCipher;
	Original, Encrypted, Decrypted: TBytes;
	I: Integer;
begin
	if not ProviderAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	{Test with 37 bytes (not a multiple of 16, larger than block size)}
	SetLength(Original, 37);
	SetLength(Encrypted, 37);
	SetLength(Decrypted, 37);
	for I := 0 to 36 do
		Original[I] := Byte(I * 7 + 13);

	EncCipher := CreateBlockCipher;
	Move(Original[0], Encrypted[0], 37);
	EncCipher.EncryptCFB8bit(Encrypted[0], Encrypted[0], 37);

	DecCipher := CreateBlockCipher;
	Move(Encrypted[0], Decrypted[0], 37);
	DecCipher.DecryptCFB8bit(Decrypted[0], Decrypted[0], 37);

	Assert.AreEqual(Original, Decrypted, 'Non-aligned size roundtrip should restore original');
end;

procedure TBCryptBlockCipherTest.TestReset_ProducesSameOutput;
var
	Cipher: IBlockCipher;
	Data1, Data2, Source: TBytes;
begin
	if not ProviderAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	Cipher := CreateBlockCipher;
	SetLength(Source, 32);
	SetLength(Data1, 32);
	SetLength(Data2, 32);
	FillChar(Source[0], 32, $55);

	Move(Source[0], Data1[0], 32);
	Cipher.EncryptCFB8bit(Data1[0], Data1[0], 32);

	Cipher.Reset;
	Move(Source[0], Data2[0], 32);
	Cipher.EncryptCFB8bit(Data2[0], Data2[0], 32);

	Assert.AreEqual(Data1, Data2, 'After Reset, encrypting same data should produce same output');
end;

procedure TBCryptBlockCipherTest.TestBurn_CompletesWithoutError;
var
	Cipher: IBlockCipher;
begin
	if not ProviderAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	Cipher := CreateBlockCipher;
	Cipher.Burn;
	Assert.Pass('Burn completed without error');
end;

{TBCryptCipherTest}

procedure TBCryptCipherTest.Setup;
begin
	FProvider := TBCryptProvider.Create;
end;

function TBCryptCipherTest.ProviderAvailable: Boolean;
begin
	Result := FProvider.IsAvailable;
end;

procedure TBCryptCipherTest.TestDifferentPasswords_DifferentCiphertext;
var
	Cipher1, Cipher2: ICipher;
	Source1, Source2: TMemoryStream;
	Enc1, Enc2: TStream;
	TestData, EncBytes1, EncBytes2: TBytes;
	BytesRead: Integer;
begin
	if not ProviderAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
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

		Cipher1 := TBCryptCipher.Create('password1', FProvider);
		Enc1 := Cipher1.GetEncryptingStream(Source1);
		try
			SetLength(EncBytes1, Enc1.Size);
			BytesRead := Enc1.Read(EncBytes1[0], Length(EncBytes1));
			SetLength(EncBytes1, BytesRead);
		finally
			Enc1.Free;
		end;

		Cipher2 := TBCryptCipher.Create('password2', FProvider);
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

procedure TBCryptCipherTest.TestImplementsICipher;
var
	Cipher: ICipher;
begin
	if not ProviderAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	Cipher := TBCryptCipher.Create('testpassword', FProvider);
	Assert.IsNotNull(Cipher, 'TBCryptCipher should implement ICipher');
end;

initialization

TDUnitX.RegisterTestFixture(TBCryptBlockCipherTest);
TDUnitX.RegisterTestFixture(TBCryptCipherTest);

end.
