unit BaseCipherTest;

{Tests TBaseCipher in isolation using a trivial XOR-based IBlockCipher.
	Verifies that the Template Method pattern correctly delegates to CreateBlockCipher
	and that all shared stream logic works regardless of backend.}

interface

uses
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
		{GetEncryptingStream/GetDecryptingStream roundtrip}
		[Test]
		procedure TestGetEncryptingDecryptingStreamRoundtrip;

		{GetEncryptingStream transforms data (output differs from input)}
		[Test]
		procedure TestGetEncryptingStreamTransformsData;
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

initialization

TDUnitX.RegisterTestFixture(TBaseCipherTest);

end.
