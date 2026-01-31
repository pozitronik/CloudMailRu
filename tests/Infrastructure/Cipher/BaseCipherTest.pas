unit BaseCipherTest;

{Tests TBaseCipher in isolation using a trivial XOR-based IBlockCipher.
	Verifies that the Template Method pattern correctly delegates to CreateBlockCipher
	and that all shared file/stream logic works regardless of backend.}

interface

uses
	BlockCipher,
	FileCipher,
	System.SysUtils,
	System.IOUtils,
	System.Classes,
	DUnitX.TestFramework;

type
	{Trivial XOR cipher for testing TBaseCipher without real crypto backends.
		XOR with a fixed key byte - deterministic, self-inverse (encrypt = decrypt).}
	TXORBlockCipher = class(TInterfacedObject, IBlockCipher)
	private
		FKeyByte: Byte;
	public
		constructor Create(KeyByte: Byte);
		procedure EncryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
		procedure DecryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
		procedure Reset;
		procedure Burn;
	end;

	{Concrete TBaseCipher subclass for testing, using TXORBlockCipher}
	TTestCipher = class(TBaseCipher)
	private
		FKeyByte: Byte;
	protected
		function CreateBlockCipher: IBlockCipher; override;
	public
		constructor Create(KeyByte: Byte);
	end;

	[TestFixture]
	TBaseCipherTest = class
	public
		{Stream roundtrip: encrypt then decrypt restores original}
		[Test]
		procedure TestCryptDecryptStreamRoundtrip;

		{Empty stream handling}
		[Test]
		procedure TestCryptStreamEmpty;

		{File roundtrip: encrypt then decrypt restores original}
		[Test]
		procedure TestCryptDecryptFileRoundtrip;

		{IO error handling: non-existent source returns CIPHER_IO_ERROR}
		[Test]
		procedure TestCryptFileNonExistentSource;

		{IO error handling: non-existent source for decrypt}
		[Test]
		procedure TestDecryptFileNonExistentSource;

		{GetEncryptingStream/GetDecryptingStream roundtrip}
		[Test]
		procedure TestGetEncryptingDecryptingStreamRoundtrip;

		{CryptStream transforms data (output differs from input)}
		[Test]
		procedure TestCryptStreamTransformsData;
	end;

implementation

uses
	CipherStreams;

{TXORBlockCipher}

constructor TXORBlockCipher.Create(KeyByte: Byte);
begin
	inherited Create;
	FKeyByte := KeyByte;
end;

procedure TXORBlockCipher.EncryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
var
	I: Cardinal;
	InPtr, OutPtr: PByte;
begin
	InPtr := @Indata;
	OutPtr := @Outdata;
	for I := 0 to Size - 1 do
	begin
		OutPtr^ := InPtr^ xor FKeyByte;
		Inc(InPtr);
		Inc(OutPtr);
	end;
end;

procedure TXORBlockCipher.DecryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
begin
	{XOR is self-inverse}
	EncryptCFB8bit(Indata, Outdata, Size);
end;

procedure TXORBlockCipher.Reset;
begin
	{No state to reset for simple XOR}
end;

procedure TXORBlockCipher.Burn;
begin
	FKeyByte := 0;
end;

{TTestCipher}

constructor TTestCipher.Create(KeyByte: Byte);
begin
	inherited Create;
	FKeyByte := KeyByte;
end;

function TTestCipher.CreateBlockCipher: IBlockCipher;
begin
	Result := TXORBlockCipher.Create(FKeyByte);
end;

{TBaseCipherTest}

procedure TBaseCipherTest.TestCryptDecryptStreamRoundtrip;
var
	Cipher: TTestCipher;
	SourceStream, EncryptedStream, DecryptedStream: TMemoryStream;
	OriginalBytes, DecryptedBytes: TBytes;
begin
	SourceStream := TMemoryStream.Create;
	EncryptedStream := TMemoryStream.Create;
	DecryptedStream := TMemoryStream.Create;
	try
		OriginalBytes := TEncoding.UTF8.GetBytes('TBaseCipher stream roundtrip test content');
		SourceStream.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		SourceStream.Position := 0;

		Cipher := TTestCipher.Create($AB);
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
				'Decrypted content should match original'
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

procedure TBaseCipherTest.TestCryptStreamEmpty;
var
	Cipher: TTestCipher;
	SourceStream, DestStream: TMemoryStream;
	BytesEncrypted: Integer;
begin
	SourceStream := TMemoryStream.Create;
	DestStream := TMemoryStream.Create;
	try
		Cipher := TTestCipher.Create($AB);
		try
			BytesEncrypted := Cipher.CryptStream(SourceStream, DestStream);
			Assert.AreEqual(0, BytesEncrypted, 'Empty stream should return 0 bytes');
			Assert.AreEqual(Int64(0), DestStream.Size, 'Destination should remain empty');
		finally
			Cipher.Free;
		end;
	finally
		SourceStream.Free;
		DestStream.Free;
	end;
end;

procedure TBaseCipherTest.TestCryptDecryptFileRoundtrip;
var
	Cipher: TTestCipher;
	SourceFile, EncryptedFile, DecryptedFile: string;
	OriginalContent, DecryptedContent: string;
	CryptResult, DecryptResult: Integer;
begin
	SourceFile := TPath.GetTempFileName;
	EncryptedFile := TPath.GetTempFileName;
	DecryptedFile := TPath.GetTempFileName;
	try
		OriginalContent := 'TBaseCipher file roundtrip test 1234567890!@#$%';
		TFile.WriteAllText(SourceFile, OriginalContent);

		Cipher := TTestCipher.Create($AB);
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

procedure TBaseCipherTest.TestCryptFileNonExistentSource;
var
	Cipher: TTestCipher;
	ResultCode: Integer;
begin
	Cipher := TTestCipher.Create($AB);
	try
		ResultCode := Cipher.CryptFile('C:\NonExistent\File\That\Does\Not\Exist.txt', TPath.GetTempFileName);
		Assert.AreEqual(CIPHER_IO_ERROR, ResultCode, 'Non-existent source should return IO error');
	finally
		Cipher.Free;
	end;
end;

procedure TBaseCipherTest.TestDecryptFileNonExistentSource;
var
	Cipher: TTestCipher;
	ResultCode: Integer;
begin
	Cipher := TTestCipher.Create($AB);
	try
		ResultCode := Cipher.DecryptFile('C:\NonExistent\File\That\Does\Not\Exist.enc', TPath.GetTempFileName);
		Assert.AreEqual(CIPHER_IO_ERROR, ResultCode, 'Non-existent source should return IO error');
	finally
		Cipher.Free;
	end;
end;

procedure TBaseCipherTest.TestGetEncryptingDecryptingStreamRoundtrip;
var
	Cipher: TTestCipher;
	SourceStream: TMemoryStream;
	EncStream, DecStream: TStream;
	OriginalBytes, DecryptedBytes: TBytes;
	BytesRead: Integer;
begin
	SourceStream := TMemoryStream.Create;
	try
		OriginalBytes := TEncoding.UTF8.GetBytes('GetEncryptingStream/GetDecryptingStream roundtrip');
		SourceStream.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		SourceStream.Position := 0;

		Cipher := TTestCipher.Create($CD);
		try
			{Encrypt via wrapping stream}
			EncStream := Cipher.GetEncryptingStream(SourceStream);
			try
				SetLength(DecryptedBytes, EncStream.Size);
				BytesRead := EncStream.Read(DecryptedBytes[0], Length(DecryptedBytes));
				Assert.AreEqual(Integer(Length(OriginalBytes)), BytesRead, 'Should read all encrypted bytes');
			finally
				EncStream.Free;
			end;

			{DecryptedBytes now holds encrypted data; wrap in memory stream for decryption}
			SourceStream.Clear;
			SourceStream.WriteBuffer(DecryptedBytes[0], BytesRead);
			SourceStream.Position := 0;

			DecStream := Cipher.GetDecryptingStream(SourceStream);
			try
				SetLength(DecryptedBytes, DecStream.Size);
				BytesRead := DecStream.Read(DecryptedBytes[0], Length(DecryptedBytes));
				Assert.AreEqual(Integer(Length(OriginalBytes)), BytesRead, 'Should read all decrypted bytes');

				Assert.AreEqual(
					TEncoding.UTF8.GetString(OriginalBytes),
					TEncoding.UTF8.GetString(DecryptedBytes),
					'Roundtrip via encrypting/decrypting streams should restore original'
				);
			finally
				DecStream.Free;
			end;
		finally
			Cipher.Free;
		end;
	finally
		SourceStream.Free;
	end;
end;

procedure TBaseCipherTest.TestCryptStreamTransformsData;
var
	Cipher: TTestCipher;
	SourceStream, EncryptedStream: TMemoryStream;
	OriginalBytes, EncryptedBytes: TBytes;
begin
	SourceStream := TMemoryStream.Create;
	EncryptedStream := TMemoryStream.Create;
	try
		OriginalBytes := TEncoding.UTF8.GetBytes('Data that should be transformed');
		SourceStream.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		SourceStream.Position := 0;

		Cipher := TTestCipher.Create($AB);
		try
			Cipher.CryptStream(SourceStream, EncryptedStream);

			SetLength(EncryptedBytes, EncryptedStream.Size);
			EncryptedStream.Position := 0;
			EncryptedStream.ReadBuffer(EncryptedBytes[0], EncryptedStream.Size);

			Assert.AreNotEqual(OriginalBytes, EncryptedBytes, 'Encrypted data should differ from original');
		finally
			Cipher.Free;
		end;
	finally
		SourceStream.Free;
		EncryptedStream.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TBaseCipherTest);

end.
