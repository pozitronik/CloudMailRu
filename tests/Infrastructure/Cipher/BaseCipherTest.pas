unit BaseCipherTest;

{Tests TBaseCipher in isolation using a trivial XOR-based IBlockCipher.
	Verifies that the Template Method pattern correctly delegates to CreateBlockCipher
	and that all shared file/stream logic works regardless of backend.}

interface

uses
	Windows,
	BlockCipher,
	Cipher,
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
		{GetEncryptingStream/GetDecryptingStream}
		[Test]
		procedure TestGetEncryptingDecryptingStreamRoundtrip;
		[Test]
		procedure TestGetEncryptingStreamTransformsData;

		{CryptStream / DecryptStream}
		[Test]
		procedure TestCryptStreamRoundtrip;
		[Test]
		procedure TestCryptStreamEmptySourceReturnsZero;
		[Test]
		procedure TestDecryptStreamEmptySourceReturnsZero;
		[Test]
		procedure TestCryptStreamReturnsByteCount;

		{CryptFile / DecryptFile}
		[Test]
		procedure TestCryptFileDecryptFileRoundtrip;
		[Test]
		procedure TestCryptFileNonexistentSourceReturnsIOError;
		[Test]
		procedure TestDecryptFileNonexistentSourceReturnsIOError;
		[Test]
		procedure TestCryptFileEmptySourceCreatesEmptyDest;
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

{TBaseCipherTest - GetEncryptingStream/GetDecryptingStream}

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

procedure TBaseCipherTest.TestGetEncryptingStreamTransformsData;
var
	Cipher: TTestCipher;
	SourceStream: TMemoryStream;
	EncStream: TStream;
	OriginalBytes, EncryptedBytes: TBytes;
begin
	SourceStream := TMemoryStream.Create;
	try
		OriginalBytes := TEncoding.UTF8.GetBytes('Data that should be transformed');
		SourceStream.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));
		SourceStream.Position := 0;

		Cipher := TTestCipher.Create($AB);
		try
			EncStream := Cipher.GetEncryptingStream(SourceStream);
			try
				SetLength(EncryptedBytes, EncStream.Size);
				EncStream.Read(EncryptedBytes[0], Length(EncryptedBytes));

				Assert.AreNotEqual(OriginalBytes, EncryptedBytes, 'Encrypted data should differ from original');
			finally
				EncStream.Free;
			end;
		finally
			Cipher.Free;
		end;
	finally
		SourceStream.Free;
	end;
end;

{TBaseCipherTest - CryptStream / DecryptStream}

procedure TBaseCipherTest.TestCryptStreamRoundtrip;
var
	Cipher: IFileCipher;
	Source, Encrypted, Decrypted: TMemoryStream;
	OriginalBytes, DecryptedBytes: TBytes;
begin
	Source := TMemoryStream.Create;
	Encrypted := TMemoryStream.Create;
	Decrypted := TMemoryStream.Create;
	try
		OriginalBytes := TEncoding.UTF8.GetBytes('CryptStream/DecryptStream roundtrip test content');
		Source.WriteBuffer(OriginalBytes[0], Length(OriginalBytes));

		Cipher := TTestCipher.Create($5A);
		Cipher.CryptStream(Source, Encrypted);

		{Encrypted data should differ from original}
		Assert.IsTrue(Encrypted.Size > 0, 'Encrypted stream should not be empty');

		{Decrypt}
		Cipher := TTestCipher.Create($5A);
		Cipher.DecryptStream(Encrypted, Decrypted);

		Decrypted.Position := 0;
		SetLength(DecryptedBytes, Decrypted.Size);
		Decrypted.ReadBuffer(DecryptedBytes[0], Decrypted.Size);

		Assert.AreEqual(
			TEncoding.UTF8.GetString(OriginalBytes),
			TEncoding.UTF8.GetString(DecryptedBytes),
			'Decrypted content should match original'
		);
	finally
		Source.Free;
		Encrypted.Free;
		Decrypted.Free;
	end;
end;

procedure TBaseCipherTest.TestCryptStreamEmptySourceReturnsZero;
var
	Cipher: IFileCipher;
	Source, Dest: TMemoryStream;
	BytesWritten: Integer;
begin
	Cipher := TTestCipher.Create($FF);
	Source := TMemoryStream.Create;
	Dest := TMemoryStream.Create;
	try
		BytesWritten := Cipher.CryptStream(Source, Dest);

		Assert.AreEqual(0, BytesWritten, 'Empty source should return 0');
		Assert.AreEqual(Int64(0), Dest.Size, 'Destination should remain empty');
	finally
		Source.Free;
		Dest.Free;
	end;
end;

procedure TBaseCipherTest.TestDecryptStreamEmptySourceReturnsZero;
var
	Cipher: IFileCipher;
	Source, Dest: TMemoryStream;
	BytesWritten: Integer;
begin
	Cipher := TTestCipher.Create($FF);
	Source := TMemoryStream.Create;
	Dest := TMemoryStream.Create;
	try
		BytesWritten := Cipher.DecryptStream(Source, Dest);

		Assert.AreEqual(0, BytesWritten, 'Empty source should return 0');
		Assert.AreEqual(Int64(0), Dest.Size, 'Destination should remain empty');
	finally
		Source.Free;
		Dest.Free;
	end;
end;

procedure TBaseCipherTest.TestCryptStreamReturnsByteCount;
var
	Cipher: IFileCipher;
	Source, Dest: TMemoryStream;
	Data: TBytes;
	BytesWritten: Integer;
begin
	Cipher := TTestCipher.Create($33);
	Source := TMemoryStream.Create;
	Dest := TMemoryStream.Create;
	try
		Data := TEncoding.UTF8.GetBytes('Count these bytes precisely');
		Source.WriteBuffer(Data[0], Length(Data));

		BytesWritten := Cipher.CryptStream(Source, Dest);

		Assert.AreEqual(Integer(Length(Data)), BytesWritten, 'Should return exact byte count');
	finally
		Source.Free;
		Dest.Free;
	end;
end;

{TBaseCipherTest - CryptFile / DecryptFile}

procedure TBaseCipherTest.TestCryptFileDecryptFileRoundtrip;
var
	TempDir, SourceFile, EncryptedFile, DecryptedFile: string;
	OriginalContent, DecryptedContent: string;
	Cipher: IFileCipher;
begin
	TempDir := TPath.GetTempPath;
	SourceFile := TPath.Combine(TempDir, 'BaseCipherTest_src_' + IntToStr(GetTickCount) + '.tmp');
	EncryptedFile := TPath.Combine(TempDir, 'BaseCipherTest_enc_' + IntToStr(GetTickCount) + '.tmp');
	DecryptedFile := TPath.Combine(TempDir, 'BaseCipherTest_dec_' + IntToStr(GetTickCount) + '.tmp');
	try
		OriginalContent := 'CryptFile/DecryptFile roundtrip test content for BaseCipher';
		TFile.WriteAllText(SourceFile, OriginalContent);

		Cipher := TTestCipher.Create($7B);
		Assert.AreEqual(CIPHER_OK, Cipher.CryptFile(SourceFile, EncryptedFile), 'CryptFile should succeed');
		Assert.IsTrue(TFile.Exists(EncryptedFile), 'Encrypted file should exist');

		Cipher := TTestCipher.Create($7B);
		Assert.AreEqual(CIPHER_OK, Cipher.DecryptFile(EncryptedFile, DecryptedFile), 'DecryptFile should succeed');

		DecryptedContent := TFile.ReadAllText(DecryptedFile);
		Assert.AreEqual(OriginalContent, DecryptedContent, 'Decrypted file content should match original');
	finally
		if TFile.Exists(SourceFile) then TFile.Delete(SourceFile);
		if TFile.Exists(EncryptedFile) then TFile.Delete(EncryptedFile);
		if TFile.Exists(DecryptedFile) then TFile.Delete(DecryptedFile);
	end;
end;

procedure TBaseCipherTest.TestCryptFileNonexistentSourceReturnsIOError;
var
	Cipher: IFileCipher;
begin
	Cipher := TTestCipher.Create($AA);
	Assert.AreEqual(CIPHER_IO_ERROR, Cipher.CryptFile('C:\nonexistent\no_file.tmp', 'C:\output.tmp'));
end;

procedure TBaseCipherTest.TestDecryptFileNonexistentSourceReturnsIOError;
var
	Cipher: IFileCipher;
begin
	Cipher := TTestCipher.Create($AA);
	Assert.AreEqual(CIPHER_IO_ERROR, Cipher.DecryptFile('C:\nonexistent\no_file.tmp', 'C:\output.tmp'));
end;

procedure TBaseCipherTest.TestCryptFileEmptySourceCreatesEmptyDest;
var
	TempDir, SourceFile, DestFile: string;
	Cipher: IFileCipher;
	DestStream: TFileStream;
begin
	TempDir := TPath.GetTempPath;
	SourceFile := TPath.Combine(TempDir, 'BaseCipherEmpty_src_' + IntToStr(GetTickCount) + '.tmp');
	DestFile := TPath.Combine(TempDir, 'BaseCipherEmpty_dst_' + IntToStr(GetTickCount) + '.tmp');
	try
		{Create empty source file}
		TFile.WriteAllText(SourceFile, '');

		Cipher := TTestCipher.Create($42);
		Assert.AreEqual(CIPHER_OK, Cipher.CryptFile(SourceFile, DestFile), 'Should handle empty file');
		Assert.IsTrue(TFile.Exists(DestFile), 'Destination should exist');

		DestStream := TFile.OpenRead(DestFile);
		try
			Assert.AreEqual(Int64(0), DestStream.Size, 'Destination should be empty');
		finally
			DestStream.Free;
		end;
	finally
		if TFile.Exists(SourceFile) then TFile.Delete(SourceFile);
		if TFile.Exists(DestFile) then TFile.Delete(DestFile);
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TBaseCipherTest);

end.
