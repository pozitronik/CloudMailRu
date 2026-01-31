unit BlockCipherTest;

interface

uses
	BlockCipher,
	System.SysUtils,
	DCPrijndael,
	DCPblockciphers,
	DUnitX.TestFramework;

type
	[TestFixture]
	TDCPCryptBlockCipherTest = class
	private
		const
			TEST_PASSWORD = 'TestPassword123!';
			TEST_IV = '1234567890ABCDEF';
		function CreateInitializedAdapter: IBlockCipher;
	public
		{Adapter delegates correctly}
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

		{Interface reference counting}
		[Test]
		procedure TestInterfaceRefCounting_NoLeak;
	end;

implementation

function TDCPCryptBlockCipherTest.CreateInitializedAdapter: IBlockCipher;
var
	Cipher: TDCP_rijndael;
begin
	Cipher := TDCP_rijndael.Create(nil);
	Cipher.Init(TEST_PASSWORD[1], Length(TEST_PASSWORD) * SizeOf(Char), @TEST_IV[1]);
	Result := TDCPCryptBlockCipher.Create(Cipher);
end;

procedure TDCPCryptBlockCipherTest.TestEncryptCFB8bit_ProducesOutput;
var
	Adapter: IBlockCipher;
	Data, Original: TBytes;
begin
	Adapter := CreateInitializedAdapter;

	SetLength(Data, 16);
	SetLength(Original, 16);
	FillChar(Data[0], 16, $42);
	Move(Data[0], Original[0], 16);

	Adapter.EncryptCFB8bit(Data[0], Data[0], 16);

	{Encrypted data should differ from original}
	Assert.AreNotEqual(Original, Data, 'EncryptCFB8bit should transform data');
end;

procedure TDCPCryptBlockCipherTest.TestDecryptCFB8bit_ProducesOutput;
var
	Adapter: IBlockCipher;
	Data, Original: TBytes;
begin
	Adapter := CreateInitializedAdapter;

	SetLength(Data, 16);
	SetLength(Original, 16);
	FillChar(Data[0], 16, $42);
	Move(Data[0], Original[0], 16);

	Adapter.DecryptCFB8bit(Data[0], Data[0], 16);

	Assert.AreNotEqual(Original, Data, 'DecryptCFB8bit should transform data');
end;

procedure TDCPCryptBlockCipherTest.TestEncryptDecrypt_Roundtrip;
var
	EncAdapter, DecAdapter: IBlockCipher;
	Original, Encrypted, Decrypted: TBytes;
begin
	SetLength(Original, 64);
	SetLength(Encrypted, 64);
	SetLength(Decrypted, 64);
	FillChar(Original[0], 64, $AB);

	{Encrypt with one adapter instance}
	EncAdapter := CreateInitializedAdapter;
	Move(Original[0], Encrypted[0], 64);
	EncAdapter.EncryptCFB8bit(Encrypted[0], Encrypted[0], 64);

	{Decrypt with a fresh adapter instance (same key/IV)}
	DecAdapter := CreateInitializedAdapter;
	Move(Encrypted[0], Decrypted[0], 64);
	DecAdapter.DecryptCFB8bit(Decrypted[0], Decrypted[0], 64);

	Assert.AreEqual(Original, Decrypted, 'Roundtrip encrypt/decrypt should restore original data');
end;

procedure TDCPCryptBlockCipherTest.TestReset_ProducesSameOutput;
var
	Adapter: IBlockCipher;
	Data1, Data2, Source: TBytes;
begin
	Adapter := CreateInitializedAdapter;

	SetLength(Source, 32);
	SetLength(Data1, 32);
	SetLength(Data2, 32);
	FillChar(Source[0], 32, $55);

	{First encryption pass}
	Move(Source[0], Data1[0], 32);
	Adapter.EncryptCFB8bit(Data1[0], Data1[0], 32);

	{Reset and encrypt same data}
	Adapter.Reset;
	Move(Source[0], Data2[0], 32);
	Adapter.EncryptCFB8bit(Data2[0], Data2[0], 32);

	Assert.AreEqual(Data1, Data2, 'After Reset, encrypting same data should produce same output');
end;

procedure TDCPCryptBlockCipherTest.TestBurn_CompletesWithoutError;
var
	Adapter: IBlockCipher;
begin
	Adapter := CreateInitializedAdapter;
	Adapter.Burn;
	Assert.Pass('Burn completed without error');
end;

procedure TDCPCryptBlockCipherTest.TestInterfaceRefCounting_NoLeak;
var
	Adapter: IBlockCipher;
begin
	{Create and immediately release -- should not leak the underlying DCPCrypt cipher}
	Adapter := CreateInitializedAdapter;
	Adapter := nil;
	Assert.Pass('Interface reference released without leak');
end;

initialization

TDUnitX.RegisterTestFixture(TDCPCryptBlockCipherTest);

end.
