unit FileSplitInfoTest;

interface

uses
	FileSplitInfo,
	System.Classes,
	Windows,
	DUnitX.TestFramework;

type

	[TestFixture]
	TFileSplitInfoTest = class
	private
		FTempFile: string;
		procedure CreateTestFile(Size: Integer);
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ Constructor tests }
		[Test]
		procedure TestCreateFileNotExists;
		[Test]
		procedure TestCreateSingleChunk;
		[Test]
		procedure TestCreateMultipleChunksEven;
		[Test]
		procedure TestCreateMultipleChunksRemainder;

		{ ChunksCount property tests }
		[Test]
		procedure TestChunksCountSingleChunk;
		[Test]
		procedure TestChunksCountMultipleEven;
		[Test]
		procedure TestChunksCountMultipleRemainder;

		{ Chunk naming tests }
		[Test]
		procedure TestChunkNamingFormat;
		[Test]
		procedure TestChunkNamingLeadingZeros;

		{ Chunk start/size tests }
		[Test]
		procedure TestChunkStartOffsets;
		[Test]
		procedure TestChunkSizesEvenDivision;
		[Test]
		procedure TestChunkSizesWithRemainder;
		[Test]
		procedure TestLastChunkSizeRemainder;

		{ CRCFileName property tests }
		[Test]
		procedure TestCRCFileNameExtension;
		[Test]
		procedure TestCRCFileNameBaseName;

		{ GetCRCData tests }
		[Test]
		procedure TestGetCRCDataFormat;
		[Test]
		procedure TestGetCRCDataContainsFilename;
		[Test]
		procedure TestGetCRCDataContainsSize;
		[Test]
		procedure TestGetCRCDataContainsCRC;

		{ CRC32 calculation tests }
		[Test]
		procedure TestCRC32KnownValue;
		[Test]
		procedure TestCRC32EmptyFile;
	end;

implementation

uses
	System.SysUtils;

const
	{ Known CRC32 values for testing }
	CRC32_EMPTY = '00000000'; { CRC32 of empty data }
	CRC32_ABC = '352441C2'; { CRC32 of 'abc' }

{ Setup and TearDown }

procedure TFileSplitInfoTest.Setup;
begin
	FTempFile := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
		'FileSplitInfoTest_' + IntToStr(GetCurrentThreadId) + '.tmp';
end;

procedure TFileSplitInfoTest.TearDown;
begin
	if FileExists(FTempFile) then
		System.SysUtils.DeleteFile(FTempFile);
end;

procedure TFileSplitInfoTest.CreateTestFile(Size: Integer);
var
	F: TFileStream;
	Buffer: TBytes;
	i: Integer;
begin
	F := TFileStream.Create(FTempFile, fmCreate);
	try
		if Size > 0 then
		begin
			SetLength(Buffer, Size);
			{ Fill with sequential bytes for predictable content }
			for i := 0 to Size - 1 do
				Buffer[i] := Byte(i mod 256);
			F.WriteBuffer(Buffer[0], Size);
		end;
	finally
		F.Free;
	end;
end;

{ Constructor tests }

procedure TFileSplitInfoTest.TestCreateFileNotExists;
var
	RaisedException: Boolean;
begin
	RaisedException := False;
	try
		TFileSplitInfo.Create('Z:\NonExistent\File.dat', 1000).Free;
	except
		on E: Exception do
			RaisedException := True;
	end;
	Assert.IsTrue(RaisedException, 'Expected exception for non-existent file');
end;

procedure TFileSplitInfoTest.TestCreateSingleChunk;
var
	SplitInfo: TFileSplitInfo;
begin
	{ File size 500 with chunk size 1000 = 1 chunk }
	CreateTestFile(500);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 1000);
	try
		Assert.AreEqual(Int64(1), SplitInfo.ChunksCount);
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestCreateMultipleChunksEven;
var
	SplitInfo: TFileSplitInfo;
begin
	{ File size 1000 with chunk size 250 = 4 chunks exactly }
	CreateTestFile(1000);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 250);
	try
		Assert.AreEqual(Int64(4), SplitInfo.ChunksCount);
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestCreateMultipleChunksRemainder;
var
	SplitInfo: TFileSplitInfo;
begin
	{ File size 1000 with chunk size 300 = 4 chunks (300+300+300+100) }
	CreateTestFile(1000);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 300);
	try
		Assert.AreEqual(Int64(4), SplitInfo.ChunksCount);
	finally
		SplitInfo.Free;
	end;
end;

{ ChunksCount property tests }

procedure TFileSplitInfoTest.TestChunksCountSingleChunk;
var
	SplitInfo: TFileSplitInfo;
begin
	CreateTestFile(100);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 1000);
	try
		Assert.AreEqual(Int64(1), SplitInfo.ChunksCount);
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestChunksCountMultipleEven;
var
	SplitInfo: TFileSplitInfo;
begin
	{ 900 bytes / 300 = 3 chunks }
	CreateTestFile(900);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 300);
	try
		Assert.AreEqual(Int64(3), SplitInfo.ChunksCount);
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestChunksCountMultipleRemainder;
var
	SplitInfo: TFileSplitInfo;
begin
	{ 1000 bytes / 400 = 2.5, so 3 chunks }
	CreateTestFile(1000);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 400);
	try
		Assert.AreEqual(Int64(3), SplitInfo.ChunksCount);
	finally
		SplitInfo.Free;
	end;
end;

{ Chunk naming tests }

procedure TFileSplitInfoTest.TestChunkNamingFormat;
var
	SplitInfo: TFileSplitInfo;
	Chunks: AFileChunkInfo;
	BaseName: string;
begin
	CreateTestFile(500);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 200);
	try
		Chunks := SplitInfo.GetChunks;
		BaseName := ChangeFileExt(ExtractFileName(FTempFile), '');
		{ First chunk should be basename.001 }
		Assert.AreEqual(BaseName + '.001', Chunks[0].name);
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestChunkNamingLeadingZeros;
var
	SplitInfo: TFileSplitInfo;
	Chunks: AFileChunkInfo;
	BaseName: string;
begin
	{ Create enough chunks to verify leading zeros }
	CreateTestFile(1000);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 100);
	try
		Chunks := SplitInfo.GetChunks;
		BaseName := ChangeFileExt(ExtractFileName(FTempFile), '');
		Assert.AreEqual(BaseName + '.001', Chunks[0].name);
		Assert.AreEqual(BaseName + '.002', Chunks[1].name);
		Assert.AreEqual(BaseName + '.010', Chunks[9].name);
	finally
		SplitInfo.Free;
	end;
end;

{ Chunk start/size tests }

procedure TFileSplitInfoTest.TestChunkStartOffsets;
var
	SplitInfo: TFileSplitInfo;
	Chunks: AFileChunkInfo;
begin
	CreateTestFile(900);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 300);
	try
		Chunks := SplitInfo.GetChunks;
		Assert.AreEqual(Int64(0), Chunks[0].start);
		Assert.AreEqual(Int64(300), Chunks[1].start);
		Assert.AreEqual(Int64(600), Chunks[2].start);
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestChunkSizesEvenDivision;
var
	SplitInfo: TFileSplitInfo;
	Chunks: AFileChunkInfo;
begin
	{ 900 bytes / 300 = 3 equal chunks }
	CreateTestFile(900);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 300);
	try
		Chunks := SplitInfo.GetChunks;
		Assert.AreEqual(Int64(300), Chunks[0].size);
		Assert.AreEqual(Int64(300), Chunks[1].size);
		Assert.AreEqual(Int64(300), Chunks[2].size);
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestChunkSizesWithRemainder;
var
	SplitInfo: TFileSplitInfo;
	Chunks: AFileChunkInfo;
begin
	{ 1000 bytes / 400 = 2 full + 1 partial (200 bytes) }
	CreateTestFile(1000);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 400);
	try
		Chunks := SplitInfo.GetChunks;
		Assert.AreEqual(Int64(400), Chunks[0].size);
		Assert.AreEqual(Int64(400), Chunks[1].size);
		Assert.AreEqual(Int64(200), Chunks[2].size);
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestLastChunkSizeRemainder;
var
	SplitInfo: TFileSplitInfo;
	Chunks: AFileChunkInfo;
begin
	{ 1001 bytes / 500 = 500 + 500 + 1 }
	CreateTestFile(1001);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 500);
	try
		Chunks := SplitInfo.GetChunks;
		Assert.AreEqual(Int64(3), SplitInfo.ChunksCount);
		Assert.AreEqual(Int64(1), Chunks[2].size);
	finally
		SplitInfo.Free;
	end;
end;

{ CRCFileName property tests }

procedure TFileSplitInfoTest.TestCRCFileNameExtension;
var
	SplitInfo: TFileSplitInfo;
begin
	CreateTestFile(100);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 1000);
	try
		Assert.AreEqual('.crc', ExtractFileExt(SplitInfo.CRCFileName));
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestCRCFileNameBaseName;
var
	SplitInfo: TFileSplitInfo;
	ExpectedBaseName: string;
begin
	CreateTestFile(100);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 1000);
	try
		ExpectedBaseName := ChangeFileExt(ExtractFileName(FTempFile), '');
		Assert.AreEqual(ExpectedBaseName + '.crc', SplitInfo.CRCFileName);
	finally
		SplitInfo.Free;
	end;
end;

{ GetCRCData tests }

procedure TFileSplitInfoTest.TestGetCRCDataFormat;
var
	SplitInfo: TFileSplitInfo;
	DataStream: TStringStream;
	Data: string;
begin
	CreateTestFile(100);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 1000);
	try
		DataStream := TStringStream.Create;
		try
			SplitInfo.GetCRCData(DataStream);
			Data := DataStream.DataString;
			{ Should contain filename=, size=, crc32= }
			Assert.IsTrue(Pos('filename=', Data) > 0, 'Missing filename= in CRC data');
			Assert.IsTrue(Pos('size=', Data) > 0, 'Missing size= in CRC data');
			Assert.IsTrue(Pos('crc32=', Data) > 0, 'Missing crc32= in CRC data');
		finally
			DataStream.Free;
		end;
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestGetCRCDataContainsFilename;
var
	SplitInfo: TFileSplitInfo;
	DataStream: TStringStream;
	Data: string;
begin
	CreateTestFile(100);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 1000);
	try
		DataStream := TStringStream.Create;
		try
			SplitInfo.GetCRCData(DataStream);
			Data := DataStream.DataString;
			Assert.IsTrue(Pos('filename=' + ExtractFileName(FTempFile), Data) > 0);
		finally
			DataStream.Free;
		end;
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestGetCRCDataContainsSize;
var
	SplitInfo: TFileSplitInfo;
	DataStream: TStringStream;
	Data: string;
begin
	CreateTestFile(12345);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 10000);
	try
		DataStream := TStringStream.Create;
		try
			SplitInfo.GetCRCData(DataStream);
			Data := DataStream.DataString;
			Assert.IsTrue(Pos('size=12345', Data) > 0);
		finally
			DataStream.Free;
		end;
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestGetCRCDataContainsCRC;
var
	SplitInfo: TFileSplitInfo;
	DataStream: TStringStream;
	Data: string;
begin
	CreateTestFile(100);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 1000);
	try
		DataStream := TStringStream.Create;
		try
			SplitInfo.GetCRCData(DataStream);
			Data := DataStream.DataString;
			{ CRC32 should be 8 hex chars }
			Assert.IsTrue(Pos('crc32=', Data) > 0);
			{ Extract CRC value and verify length }
			Assert.AreEqual(8, Length(Copy(Data, Pos('crc32=', Data) + 6, 8)));
		finally
			DataStream.Free;
		end;
	finally
		SplitInfo.Free;
	end;
end;

{ CRC32 calculation tests }

procedure TFileSplitInfoTest.TestCRC32KnownValue;
var
	SplitInfo: TFileSplitInfo;
	DataStream: TStringStream;
	Data: string;
	F: TFileStream;
	Content: AnsiString;
begin
	{ Create file with 'abc' content }
	Content := 'abc';
	F := TFileStream.Create(FTempFile, fmCreate);
	try
		F.WriteBuffer(Content[1], Length(Content));
	finally
		F.Free;
	end;

	SplitInfo := TFileSplitInfo.Create(FTempFile, 1000);
	try
		DataStream := TStringStream.Create;
		try
			SplitInfo.GetCRCData(DataStream);
			Data := DataStream.DataString;
			{ CRC32 of 'abc' is 352441C2 }
			Assert.IsTrue(Pos('crc32=' + CRC32_ABC, Data) > 0,
				'Expected CRC32 of abc to be ' + CRC32_ABC + ', got: ' + Data);
		finally
			DataStream.Free;
		end;
	finally
		SplitInfo.Free;
	end;
end;

procedure TFileSplitInfoTest.TestCRC32EmptyFile;
var
	SplitInfo: TFileSplitInfo;
	DataStream: TStringStream;
	Data: string;
begin
	CreateTestFile(0);
	SplitInfo := TFileSplitInfo.Create(FTempFile, 1000);
	try
		DataStream := TStringStream.Create;
		try
			SplitInfo.GetCRCData(DataStream);
			Data := DataStream.DataString;
			{ CRC32 of empty data with seed FFFFFFFF XOR FFFFFFFF = 00000000 }
			Assert.IsTrue(Pos('crc32=' + CRC32_EMPTY, Data) > 0,
				'Expected CRC32 of empty file to be ' + CRC32_EMPTY + ', got: ' + Data);
		finally
			DataStream.Free;
		end;
	finally
		SplitInfo.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TFileSplitInfoTest);

end.
