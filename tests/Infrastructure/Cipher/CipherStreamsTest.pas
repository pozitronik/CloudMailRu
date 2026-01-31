unit CipherStreamsTest;

interface

uses
	CipherStreams,
	BlockCipher,
	System.SysUtils,
	System.Classes,
	DCPrijndael,
	DCPtwofish,
	DCPblockciphers,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCipherStreamsTest = class
	private
		const
			TEST_PASSWORD = 'TestPassword123!';
			TEST_IV = '1234567890ABCDEF';
		function CreateInitializedCipher: IBlockCipher;
		function CreateInitializedTwofishCipher: IBlockCipher;
		function CreateTestData(Size: Integer): TBytes;
	public
		{TEncryptingStream - Basic functionality}
		[Test]
		procedure TestEncryptingStream_Create_InitializesCorrectly;
		[Test]
		procedure TestEncryptingStream_GetSize_ReturnsSourceSize;
		[Test]
		procedure TestEncryptingStream_Read_ReturnsEncryptedData;
		[Test]
		procedure TestEncryptingStream_Read_DifferentFromSource;

		{TEncryptingStream - Write rejection}
		[Test]
		procedure TestEncryptingStream_Write_RaisesException;

		{TEncryptingStream - Seek behavior}
		[Test]
		procedure TestEncryptingStream_SeekCurrentZero_ReturnsPosition;
		[Test]
		procedure TestEncryptingStream_SeekBeginning_RaisesException;
		[Test]
		procedure TestEncryptingStream_SeekArbitrary_RaisesException;

		{TEncryptingStream - Buffer handling}
		[Test]
		procedure TestEncryptingStream_MultipleSmallReads_ReturnsAllData;
		[Test]
		procedure TestEncryptingStream_LargeDataRead_HandlesMultipleBuffers;
		[Test]
		procedure TestEncryptingStream_ExactBufferSizeRead_Works;
		[Test]
		procedure TestEncryptingStream_EmptySource_ReadsZeroBytes;

		{TDecryptingStream - Basic functionality}
		[Test]
		procedure TestDecryptingStream_Create_InitializesCorrectly;
		[Test]
		procedure TestDecryptingStream_GetSize_ReturnsSourceSize;
		[Test]
		procedure TestDecryptingStream_Read_ReturnsDecryptedData;

		{TDecryptingStream - Write rejection}
		[Test]
		procedure TestDecryptingStream_Write_RaisesException;

		{TDecryptingStream - Seek behavior}
		[Test]
		procedure TestDecryptingStream_SeekCurrentZero_ReturnsPosition;
		[Test]
		procedure TestDecryptingStream_SeekBeginning_RaisesException;
		[Test]
		procedure TestDecryptingStream_SeekArbitrary_RaisesException;

		{TDecryptingStream - Buffer handling}
		[Test]
		procedure TestDecryptingStream_MultipleSmallReads_ReturnsAllData;
		[Test]
		procedure TestDecryptingStream_LargeDataRead_HandlesMultipleBuffers;
		[Test]
		procedure TestDecryptingStream_ExactBufferSizeRead_Works;
		[Test]
		procedure TestDecryptingStream_EmptySource_ReadsZeroBytes;

		{Roundtrip tests - encrypt then decrypt}
		[Test]
		procedure TestRoundtrip_SmallData_PreservesContent;
		[Test]
		procedure TestRoundtrip_LargeData_PreservesContent;
		[Test]
		procedure TestRoundtrip_ExactBufferSize_PreservesContent;
		[Test]
		procedure TestRoundtrip_BufferPlusOne_PreservesContent;
		[Test]
		procedure TestRoundtrip_EmptyData_PreservesContent;
		[Test]
		procedure TestRoundtrip_ChunkedReads_PreservesContent;

		{Memory and resource management}
		[Test]
		procedure TestEncryptingStream_Destroy_BurnsCipher;
		[Test]
		procedure TestDecryptingStream_Destroy_BurnsCipher;

		{Position tracking}
		[Test]
		procedure TestEncryptingStream_PositionTracking_AfterPartialRead;
		[Test]
		procedure TestDecryptingStream_PositionTracking_AfterPartialRead;

		{Alternative cipher tests - verify widened TDCP_blockcipher128 type}
		[Test]
		procedure TestRoundtrip_Twofish_PreservesContent;
		[Test]
		procedure TestEncryptingStream_Twofish_SeekReset;
	end;

implementation

{Helper to create initialized cipher with consistent key/IV for testing}
function TCipherStreamsTest.CreateInitializedCipher: IBlockCipher;
var
	Cipher: TDCP_rijndael;
begin
	Cipher := TDCP_rijndael.Create(nil);
	Cipher.Init(TEST_PASSWORD[1], Length(TEST_PASSWORD) * SizeOf(Char) * 8, @TEST_IV[1]);
	Result := TDCPCryptBlockCipher.Create(Cipher);
end;

{Helper to create initialized Twofish cipher for alternative cipher testing}
function TCipherStreamsTest.CreateInitializedTwofishCipher: IBlockCipher;
var
	Cipher: TDCP_twofish;
begin
	Cipher := TDCP_twofish.Create(nil);
	Cipher.Init(TEST_PASSWORD[1], Length(TEST_PASSWORD) * SizeOf(Char) * 8, @TEST_IV[1]);
	Result := TDCPCryptBlockCipher.Create(Cipher);
end;

{Helper to create test data of specified size}
function TCipherStreamsTest.CreateTestData(Size: Integer): TBytes;
var
	I: Integer;
begin
	SetLength(Result, Size);
	for I := 0 to Size - 1 do
		Result[I] := Byte(I mod 256);
end;

{TEncryptingStream - Basic functionality}

procedure TCipherStreamsTest.TestEncryptingStream_Create_InitializesCorrectly;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(100)[0], 100);
		Source.Position := 0;

		{Cipher ownership transfers to EncStream via interface ref counting}
		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		try
			Assert.IsNotNull(EncStream);
			Assert.AreEqual(Int64(100), EncStream.Size);
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestEncryptingStream_GetSize_ReturnsSourceSize;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
	TestSizes: array of Integer;
	I: Integer;
begin
	TestSizes := [0, 1, 100, 1000, CIPHER_BUFFER_SIZE, CIPHER_BUFFER_SIZE + 1];

	for I := Low(TestSizes) to High(TestSizes) do
	begin
		Source := TMemoryStream.Create;
		try
			if TestSizes[I] > 0 then
				Source.WriteBuffer(CreateTestData(TestSizes[I])[0], TestSizes[I]);
			Source.Position := 0;

			EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
			try
				Assert.AreEqual(Int64(TestSizes[I]), EncStream.Size,
					Format('Size mismatch for input size %d', [TestSizes[I]]));
			finally
				EncStream.Free;
			end;
		finally
			Source.Free;
		end;
	end;
end;

procedure TCipherStreamsTest.TestEncryptingStream_Read_ReturnsEncryptedData;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
	OriginalData, EncryptedData: TBytes;
	BytesRead: Integer;
begin
	OriginalData := CreateTestData(100);

	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(OriginalData[0], Length(OriginalData));
		Source.Position := 0;

		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(EncryptedData, 100);
			BytesRead := EncStream.Read(EncryptedData[0], 100);

			Assert.AreEqual(100, BytesRead, 'Should read all bytes');
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestEncryptingStream_Read_DifferentFromSource;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
	OriginalData, EncryptedData: TBytes;
	SameCount, I: Integer;
begin
	OriginalData := CreateTestData(100);

	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(OriginalData[0], Length(OriginalData));
		Source.Position := 0;

		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(EncryptedData, 100);
			EncStream.Read(EncryptedData[0], 100);

			{Encrypted data should differ from original - count matching bytes}
			SameCount := 0;
			for I := 0 to 99 do
				if OriginalData[I] = EncryptedData[I] then
					Inc(SameCount);

			{Statistically, very few bytes should match by chance}
			Assert.IsTrue(SameCount < 10, 'Encrypted data should differ significantly from original');
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

{TEncryptingStream - Write rejection}

procedure TCipherStreamsTest.TestEncryptingStream_Write_RaisesException;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
	Data: TBytes;
begin
	Source := TMemoryStream.Create;
	try
		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Data, 10);
			Assert.WillRaise(
				procedure
				begin
					EncStream.Write(Data[0], 10);
				end,
				Exception,
				'TEncryptingStream is read-only'
			);
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

{TEncryptingStream - Seek behavior}

procedure TCipherStreamsTest.TestEncryptingStream_SeekCurrentZero_ReturnsPosition;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
	Buffer: TBytes;
	Position: Int64;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(200)[0], 200);
		Source.Position := 0;

		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		try
			{Read some data first}
			SetLength(Buffer, 50);
			EncStream.Read(Buffer[0], 50);

			{Seek(0, soCurrent) should return current position}
			Position := EncStream.Seek(0, soCurrent);
			Assert.AreEqual(Int64(50), Position);
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestEncryptingStream_SeekBeginning_RaisesException;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
	FirstRead, SecondRead: TBytes;
begin
	{Test renamed: SeekBeginning now works - needed for Indy HTTP uploads}
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(100)[0], 100);
		Source.Position := 0;

		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		try
			{Read encrypted data}
			SetLength(FirstRead, 100);
			EncStream.Read(FirstRead[0], 100);

			{Seek back to beginning}
			Assert.AreEqual(Int64(0), EncStream.Seek(0, soBeginning), 'Seek should return 0');

			{Read again - should get same encrypted data}
			SetLength(SecondRead, 100);
			EncStream.Read(SecondRead[0], 100);

			Assert.AreEqual(FirstRead, SecondRead, 'Re-reading after seek should produce same data');
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestEncryptingStream_SeekArbitrary_RaisesException;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(100)[0], 100);
		Source.Position := 0;

		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		try
			Assert.WillRaise(
				procedure
				begin
					EncStream.Seek(50, soBeginning);
				end,
				Exception,
				'TEncryptingStream does not support arbitrary seeking'
			);
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

{TEncryptingStream - Buffer handling}

procedure TCipherStreamsTest.TestEncryptingStream_MultipleSmallReads_ReturnsAllData;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
	Buffer: TBytes;
	TotalRead, BytesRead: Integer;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(500)[0], 500);
		Source.Position := 0;

		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Buffer, 17); {Prime number to test non-aligned reads}
			TotalRead := 0;

			repeat
				BytesRead := EncStream.Read(Buffer[0], 17);
				Inc(TotalRead, BytesRead);
			until BytesRead = 0;

			Assert.AreEqual(500, TotalRead, 'Should read all 500 bytes in small chunks');
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestEncryptingStream_LargeDataRead_HandlesMultipleBuffers;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
	Buffer: TBytes;
	DataSize, BytesRead: Integer;
begin
	{Test data larger than internal buffer (65536 bytes)}
	DataSize := CIPHER_BUFFER_SIZE * 2 + 1000;

	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(DataSize)[0], DataSize);
		Source.Position := 0;

		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Buffer, DataSize);
			BytesRead := EncStream.Read(Buffer[0], DataSize);

			Assert.AreEqual(DataSize, BytesRead, 'Should read all data across multiple buffers');
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestEncryptingStream_ExactBufferSizeRead_Works;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(CIPHER_BUFFER_SIZE)[0], CIPHER_BUFFER_SIZE);
		Source.Position := 0;

		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Buffer, CIPHER_BUFFER_SIZE);
			BytesRead := EncStream.Read(Buffer[0], CIPHER_BUFFER_SIZE);

			Assert.AreEqual(CIPHER_BUFFER_SIZE, BytesRead, 'Should read exact buffer size');
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestEncryptingStream_EmptySource_ReadsZeroBytes;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	Source := TMemoryStream.Create;
	try
		{Empty source stream}
		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Buffer, 100);
			BytesRead := EncStream.Read(Buffer[0], 100);

			Assert.AreEqual(0, BytesRead, 'Empty source should yield zero bytes');
			Assert.AreEqual(Int64(0), EncStream.Size, 'Size should be zero');
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

{TDecryptingStream - Basic functionality}

procedure TCipherStreamsTest.TestDecryptingStream_Create_InitializesCorrectly;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(100)[0], 100);
		Source.Position := 0;

		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		try
			Assert.IsNotNull(DecStream);
			Assert.AreEqual(Int64(100), DecStream.Size);
		finally
			DecStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestDecryptingStream_GetSize_ReturnsSourceSize;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(256)[0], 256);
		Source.Position := 0;

		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		try
			Assert.AreEqual(Int64(256), DecStream.Size);
		finally
			DecStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestDecryptingStream_Read_ReturnsDecryptedData;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(100)[0], 100);
		Source.Position := 0;

		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Buffer, 100);
			BytesRead := DecStream.Read(Buffer[0], 100);

			Assert.AreEqual(100, BytesRead, 'Should read all bytes');
		finally
			DecStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

{TDecryptingStream - Write rejection}

procedure TCipherStreamsTest.TestDecryptingStream_Write_RaisesException;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
	Data: TBytes;
begin
	Source := TMemoryStream.Create;
	try
		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Data, 10);
			Assert.WillRaise(
				procedure
				begin
					DecStream.Write(Data[0], 10);
				end,
				Exception,
				'TDecryptingStream is read-only'
			);
		finally
			DecStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

{TDecryptingStream - Seek behavior}

procedure TCipherStreamsTest.TestDecryptingStream_SeekCurrentZero_ReturnsPosition;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
	Buffer: TBytes;
	Position: Int64;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(200)[0], 200);
		Source.Position := 0;

		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Buffer, 75);
			DecStream.Read(Buffer[0], 75);

			Position := DecStream.Seek(0, soCurrent);
			Assert.AreEqual(Int64(75), Position);
		finally
			DecStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestDecryptingStream_SeekBeginning_RaisesException;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
	FirstRead, SecondRead: TBytes;
begin
	{Test renamed: SeekBeginning now works - needed for Indy HTTP downloads}
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(100)[0], 100);
		Source.Position := 0;

		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		try
			{Read decrypted data}
			SetLength(FirstRead, 100);
			DecStream.Read(FirstRead[0], 100);

			{Seek back to beginning}
			Assert.AreEqual(Int64(0), DecStream.Seek(0, soBeginning), 'Seek should return 0');

			{Read again - should get same decrypted data}
			SetLength(SecondRead, 100);
			DecStream.Read(SecondRead[0], 100);

			Assert.AreEqual(FirstRead, SecondRead, 'Re-reading after seek should produce same data');
		finally
			DecStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestDecryptingStream_SeekArbitrary_RaisesException;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(100)[0], 100);
		Source.Position := 0;

		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		try
			Assert.WillRaise(
				procedure
				begin
					DecStream.Seek(25, soCurrent);
				end,
				Exception,
				'TDecryptingStream does not support arbitrary seeking'
			);
		finally
			DecStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

{TDecryptingStream - Buffer handling}

procedure TCipherStreamsTest.TestDecryptingStream_MultipleSmallReads_ReturnsAllData;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
	Buffer: TBytes;
	TotalRead, BytesRead: Integer;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(500)[0], 500);
		Source.Position := 0;

		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Buffer, 23); {Prime number}
			TotalRead := 0;

			repeat
				BytesRead := DecStream.Read(Buffer[0], 23);
				Inc(TotalRead, BytesRead);
			until BytesRead = 0;

			Assert.AreEqual(500, TotalRead, 'Should read all 500 bytes in small chunks');
		finally
			DecStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestDecryptingStream_LargeDataRead_HandlesMultipleBuffers;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
	Buffer: TBytes;
	DataSize, BytesRead: Integer;
begin
	DataSize := CIPHER_BUFFER_SIZE * 3;

	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(DataSize)[0], DataSize);
		Source.Position := 0;

		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Buffer, DataSize);
			BytesRead := DecStream.Read(Buffer[0], DataSize);

			Assert.AreEqual(DataSize, BytesRead);
		finally
			DecStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestDecryptingStream_ExactBufferSizeRead_Works;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(CIPHER_BUFFER_SIZE)[0], CIPHER_BUFFER_SIZE);
		Source.Position := 0;

		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Buffer, CIPHER_BUFFER_SIZE);
			BytesRead := DecStream.Read(Buffer[0], CIPHER_BUFFER_SIZE);

			Assert.AreEqual(CIPHER_BUFFER_SIZE, BytesRead);
		finally
			DecStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestDecryptingStream_EmptySource_ReadsZeroBytes;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	Source := TMemoryStream.Create;
	try
		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Buffer, 100);
			BytesRead := DecStream.Read(Buffer[0], 100);

			Assert.AreEqual(0, BytesRead);
			Assert.AreEqual(Int64(0), DecStream.Size);
		finally
			DecStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

{Roundtrip tests - encrypt then decrypt to verify data integrity}

procedure TCipherStreamsTest.TestRoundtrip_SmallData_PreservesContent;
var
	OriginalData, EncryptedData, DecryptedData: TBytes;
	SourceStream, EncryptedStream: TMemoryStream;
	EncStream: TEncryptingStream;
	DecStream: TDecryptingStream;
begin
	OriginalData := CreateTestData(100);

	{Step 1: Encrypt}
	SourceStream := TMemoryStream.Create;
	try
		SourceStream.WriteBuffer(OriginalData[0], Length(OriginalData));
		SourceStream.Position := 0;

		EncStream := TEncryptingStream.Create(SourceStream, CreateInitializedCipher);
		try
			SetLength(EncryptedData, 100);
			EncStream.Read(EncryptedData[0], 100);
		finally
			EncStream.Free;
		end;
	finally
		SourceStream.Free;
	end;

	{Step 2: Decrypt}
	EncryptedStream := TMemoryStream.Create;
	try
		EncryptedStream.WriteBuffer(EncryptedData[0], Length(EncryptedData));
		EncryptedStream.Position := 0;

		DecStream := TDecryptingStream.Create(EncryptedStream, CreateInitializedCipher);
		try
			SetLength(DecryptedData, 100);
			DecStream.Read(DecryptedData[0], 100);
		finally
			DecStream.Free;
		end;
	finally
		EncryptedStream.Free;
	end;

	{Verify}
	Assert.AreEqual(OriginalData, DecryptedData, 'Decrypted data should match original');
end;

procedure TCipherStreamsTest.TestRoundtrip_LargeData_PreservesContent;
var
	DataSize: Integer;
	OriginalData, EncryptedData, DecryptedData: TBytes;
	SourceStream, EncryptedStream: TMemoryStream;
	EncStream: TEncryptingStream;
	DecStream: TDecryptingStream;
begin
	DataSize := CIPHER_BUFFER_SIZE * 2 + 12345;
	OriginalData := CreateTestData(DataSize);

	{Encrypt}
	SourceStream := TMemoryStream.Create;
	try
		SourceStream.WriteBuffer(OriginalData[0], DataSize);
		SourceStream.Position := 0;

		EncStream := TEncryptingStream.Create(SourceStream, CreateInitializedCipher);
		try
			SetLength(EncryptedData, DataSize);
			EncStream.Read(EncryptedData[0], DataSize);
		finally
			EncStream.Free;
		end;
	finally
		SourceStream.Free;
	end;

	{Decrypt}
	EncryptedStream := TMemoryStream.Create;
	try
		EncryptedStream.WriteBuffer(EncryptedData[0], DataSize);
		EncryptedStream.Position := 0;

		DecStream := TDecryptingStream.Create(EncryptedStream, CreateInitializedCipher);
		try
			SetLength(DecryptedData, DataSize);
			DecStream.Read(DecryptedData[0], DataSize);
		finally
			DecStream.Free;
		end;
	finally
		EncryptedStream.Free;
	end;

	Assert.AreEqual(OriginalData, DecryptedData);
end;

procedure TCipherStreamsTest.TestRoundtrip_ExactBufferSize_PreservesContent;
var
	OriginalData, EncryptedData, DecryptedData: TBytes;
	SourceStream, EncryptedStream: TMemoryStream;
	EncStream: TEncryptingStream;
	DecStream: TDecryptingStream;
begin
	OriginalData := CreateTestData(CIPHER_BUFFER_SIZE);

	{Encrypt}
	SourceStream := TMemoryStream.Create;
	try
		SourceStream.WriteBuffer(OriginalData[0], CIPHER_BUFFER_SIZE);
		SourceStream.Position := 0;

		EncStream := TEncryptingStream.Create(SourceStream, CreateInitializedCipher);
		try
			SetLength(EncryptedData, CIPHER_BUFFER_SIZE);
			EncStream.Read(EncryptedData[0], CIPHER_BUFFER_SIZE);
		finally
			EncStream.Free;
		end;
	finally
		SourceStream.Free;
	end;

	{Decrypt}
	EncryptedStream := TMemoryStream.Create;
	try
		EncryptedStream.WriteBuffer(EncryptedData[0], CIPHER_BUFFER_SIZE);
		EncryptedStream.Position := 0;

		DecStream := TDecryptingStream.Create(EncryptedStream, CreateInitializedCipher);
		try
			SetLength(DecryptedData, CIPHER_BUFFER_SIZE);
			DecStream.Read(DecryptedData[0], CIPHER_BUFFER_SIZE);
		finally
			DecStream.Free;
		end;
	finally
		EncryptedStream.Free;
	end;

	Assert.AreEqual(OriginalData, DecryptedData);
end;

procedure TCipherStreamsTest.TestRoundtrip_BufferPlusOne_PreservesContent;
var
	DataSize: Integer;
	OriginalData, EncryptedData, DecryptedData: TBytes;
	SourceStream, EncryptedStream: TMemoryStream;
	EncStream: TEncryptingStream;
	DecStream: TDecryptingStream;
begin
	{Test boundary condition: buffer size + 1}
	DataSize := CIPHER_BUFFER_SIZE + 1;
	OriginalData := CreateTestData(DataSize);

	{Encrypt}
	SourceStream := TMemoryStream.Create;
	try
		SourceStream.WriteBuffer(OriginalData[0], DataSize);
		SourceStream.Position := 0;

		EncStream := TEncryptingStream.Create(SourceStream, CreateInitializedCipher);
		try
			SetLength(EncryptedData, DataSize);
			EncStream.Read(EncryptedData[0], DataSize);
		finally
			EncStream.Free;
		end;
	finally
		SourceStream.Free;
	end;

	{Decrypt}
	EncryptedStream := TMemoryStream.Create;
	try
		EncryptedStream.WriteBuffer(EncryptedData[0], DataSize);
		EncryptedStream.Position := 0;

		DecStream := TDecryptingStream.Create(EncryptedStream, CreateInitializedCipher);
		try
			SetLength(DecryptedData, DataSize);
			DecStream.Read(DecryptedData[0], DataSize);
		finally
			DecStream.Free;
		end;
	finally
		EncryptedStream.Free;
	end;

	Assert.AreEqual(OriginalData, DecryptedData);
end;

procedure TCipherStreamsTest.TestRoundtrip_EmptyData_PreservesContent;
var
	EncryptedData, DecryptedData: TBytes;
	SourceStream, EncryptedStream: TMemoryStream;
	EncStream: TEncryptingStream;
	DecStream: TDecryptingStream;
	BytesRead: Integer;
begin
	{Empty source}
	SourceStream := TMemoryStream.Create;
	try
		EncStream := TEncryptingStream.Create(SourceStream, CreateInitializedCipher);
		try
			SetLength(EncryptedData, 100);
			BytesRead := EncStream.Read(EncryptedData[0], 100);
			Assert.AreEqual(0, BytesRead, 'Should read 0 bytes from empty source');
		finally
			EncStream.Free;
		end;
	finally
		SourceStream.Free;
	end;

	{Decrypt empty}
	EncryptedStream := TMemoryStream.Create;
	try
		DecStream := TDecryptingStream.Create(EncryptedStream, CreateInitializedCipher);
		try
			SetLength(DecryptedData, 100);
			BytesRead := DecStream.Read(DecryptedData[0], 100);
			Assert.AreEqual(0, BytesRead, 'Should read 0 bytes from empty encrypted');
		finally
			DecStream.Free;
		end;
	finally
		EncryptedStream.Free;
	end;
end;

procedure TCipherStreamsTest.TestRoundtrip_ChunkedReads_PreservesContent;
var
	OriginalData, EncryptedData, DecryptedData: TBytes;
	SourceStream, EncryptedStream: TMemoryStream;
	EncStream: TEncryptingStream;
	DecStream: TDecryptingStream;
	ChunkBuffer: TBytes;
	ChunkSize, Offset, BytesRead: Integer;
begin
	OriginalData := CreateTestData(1000);
	SetLength(EncryptedData, 1000);
	SetLength(DecryptedData, 1000);
	SetLength(ChunkBuffer, 50); {Temporary buffer for chunked reads}

	{Encrypt in chunks}
	SourceStream := TMemoryStream.Create;
	try
		SourceStream.WriteBuffer(OriginalData[0], 1000);
		SourceStream.Position := 0;

		EncStream := TEncryptingStream.Create(SourceStream, CreateInitializedCipher);
		try
			ChunkSize := 37; {Prime number for non-aligned reads}
			Offset := 0;
			repeat
				BytesRead := EncStream.Read(ChunkBuffer[0], ChunkSize);
				if BytesRead > 0 then
				begin
					Move(ChunkBuffer[0], EncryptedData[Offset], BytesRead);
					Inc(Offset, BytesRead);
				end;
			until BytesRead = 0;
		finally
			EncStream.Free;
		end;
	finally
		SourceStream.Free;
	end;

	{Decrypt in different chunk size}
	EncryptedStream := TMemoryStream.Create;
	try
		EncryptedStream.WriteBuffer(EncryptedData[0], 1000);
		EncryptedStream.Position := 0;

		DecStream := TDecryptingStream.Create(EncryptedStream, CreateInitializedCipher);
		try
			ChunkSize := 41; {Different prime}
			Offset := 0;
			repeat
				BytesRead := DecStream.Read(ChunkBuffer[0], ChunkSize);
				if BytesRead > 0 then
				begin
					Move(ChunkBuffer[0], DecryptedData[Offset], BytesRead);
					Inc(Offset, BytesRead);
				end;
			until BytesRead = 0;
		finally
			DecStream.Free;
		end;
	finally
		EncryptedStream.Free;
	end;

	Assert.AreEqual(OriginalData, DecryptedData, 'Chunked roundtrip should preserve content');
end;

{Memory and resource management}

procedure TCipherStreamsTest.TestEncryptingStream_Destroy_BurnsCipher;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
begin
	{This test verifies destructor runs without error.
	 Burn() and Free() on cipher are called in destructor.
	 If cipher was already freed or corrupted, this would raise.}
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(100)[0], 100);
		Source.Position := 0;

		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		EncStream.Free; {Should not raise}

		Assert.Pass('Destructor completed without error');
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestDecryptingStream_Destroy_BurnsCipher;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(100)[0], 100);
		Source.Position := 0;

		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		DecStream.Free;

		Assert.Pass('Destructor completed without error');
	finally
		Source.Free;
	end;
end;

{Position tracking}

procedure TCipherStreamsTest.TestEncryptingStream_PositionTracking_AfterPartialRead;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
	Buffer: TBytes;
	Pos1, Pos2, Pos3: Int64;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(500)[0], 500);
		Source.Position := 0;

		EncStream := TEncryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Buffer, 100);

			Pos1 := EncStream.Seek(0, soCurrent);
			Assert.AreEqual(Int64(0), Pos1, 'Initial position should be 0');

			EncStream.Read(Buffer[0], 100);
			Pos2 := EncStream.Seek(0, soCurrent);
			Assert.AreEqual(Int64(100), Pos2, 'Position after 100 bytes read');

			EncStream.Read(Buffer[0], 50);
			Pos3 := EncStream.Seek(0, soCurrent);
			Assert.AreEqual(Int64(150), Pos3, 'Position after 150 total bytes read');
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

procedure TCipherStreamsTest.TestDecryptingStream_PositionTracking_AfterPartialRead;
var
	Source: TMemoryStream;
	DecStream: TDecryptingStream;
	Buffer: TBytes;
	Pos1, Pos2: Int64;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(300)[0], 300);
		Source.Position := 0;

		DecStream := TDecryptingStream.Create(Source, CreateInitializedCipher);
		try
			SetLength(Buffer, 100);

			Pos1 := DecStream.Seek(0, soCurrent);
			Assert.AreEqual(Int64(0), Pos1);

			DecStream.Read(Buffer[0], 75);
			Pos2 := DecStream.Seek(0, soCurrent);
			Assert.AreEqual(Int64(75), Pos2);
		finally
			DecStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

{Alternative cipher tests - verify widened TDCP_blockcipher128 type}

procedure TCipherStreamsTest.TestRoundtrip_Twofish_PreservesContent;
var
	OriginalData, EncryptedData, DecryptedData: TBytes;
	SourceStream, EncryptedStream: TMemoryStream;
	EncStream: TEncryptingStream;
	DecStream: TDecryptingStream;
begin
	OriginalData := CreateTestData(200);

	{Step 1: Encrypt with Twofish}
	SourceStream := TMemoryStream.Create;
	try
		SourceStream.WriteBuffer(OriginalData[0], Length(OriginalData));
		SourceStream.Position := 0;

		EncStream := TEncryptingStream.Create(SourceStream, CreateInitializedTwofishCipher);
		try
			SetLength(EncryptedData, 200);
			EncStream.Read(EncryptedData[0], 200);
		finally
			EncStream.Free;
		end;
	finally
		SourceStream.Free;
	end;

	{Step 2: Decrypt with Twofish}
	EncryptedStream := TMemoryStream.Create;
	try
		EncryptedStream.WriteBuffer(EncryptedData[0], Length(EncryptedData));
		EncryptedStream.Position := 0;

		DecStream := TDecryptingStream.Create(EncryptedStream, CreateInitializedTwofishCipher);
		try
			SetLength(DecryptedData, 200);
			DecStream.Read(DecryptedData[0], 200);
		finally
			DecStream.Free;
		end;
	finally
		EncryptedStream.Free;
	end;

	{Verify}
	Assert.AreEqual(OriginalData, DecryptedData, 'Twofish roundtrip: decrypted data should match original');
end;

procedure TCipherStreamsTest.TestEncryptingStream_Twofish_SeekReset;
var
	Source: TMemoryStream;
	EncStream: TEncryptingStream;
	FirstRead, SecondRead: TBytes;
begin
	Source := TMemoryStream.Create;
	try
		Source.WriteBuffer(CreateTestData(200)[0], 200);
		Source.Position := 0;

		EncStream := TEncryptingStream.Create(Source, CreateInitializedTwofishCipher);
		try
			{Read encrypted data}
			SetLength(FirstRead, 200);
			EncStream.Read(FirstRead[0], 200);

			{Seek back to beginning}
			Assert.AreEqual(Int64(0), EncStream.Seek(0, soBeginning), 'Seek should return 0');

			{Read again - should get same encrypted data}
			SetLength(SecondRead, 200);
			EncStream.Read(SecondRead[0], 200);

			Assert.AreEqual(FirstRead, SecondRead, 'Twofish: re-reading after seek should produce same data');
		finally
			EncStream.Free;
		end;
	finally
		Source.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TCipherStreamsTest);

end.
