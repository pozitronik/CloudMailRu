unit ChunkedFileStreamTest;

interface

uses
	ChunkedFileStream,
	System.Classes,
	Windows,
	DUnitX.TestFramework;

type

	[TestFixture]
	TChunkedFileStreamTest = class
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
		procedure TestCreateValidChunk;
		[Test]
		procedure TestCreateChunkSizeZeroMeansRest;
		[Test]
		procedure TestCreateInvalidChunkStartNegative;
		[Test]
		procedure TestCreateInvalidChunkStartBeyondFile;
		[Test]
		procedure TestCreateInvalidChunkSizeNegative;
		[Test]
		procedure TestCreateInvalidChunkSizeBeyondFile;

		{ Size property tests }
		[Test]
		procedure TestSizeReturnsChunkSize;
		[Test]
		procedure TestSizeWithZeroChunkSizeReturnsRemainder;

		{ Read tests }
		[Test]
		procedure TestReadWithinChunk;
		[Test]
		procedure TestReadAtChunkBoundary;
		[Test]
		procedure TestReadBeyondChunkBoundary;
		[Test]
		procedure TestReadFromMiddleOfChunk;

		{ Seek tests }
		[Test]
		procedure TestSeekFromBeginning;
		[Test]
		procedure TestSeekFromCurrent;
		[Test]
		procedure TestSeekFromEnd;
		[Test]
		procedure TestSeekBeyondChunkClamps;
	end;

implementation

uses
	System.SysUtils;

{ Setup and TearDown }

procedure TChunkedFileStreamTest.Setup;
begin
	FTempFile := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
		'ChunkedFileStreamTest_' + IntToStr(GetCurrentThreadId) + '.tmp';
end;

procedure TChunkedFileStreamTest.TearDown;
begin
	if FileExists(FTempFile) then
		System.SysUtils.DeleteFile(FTempFile);
end;

procedure TChunkedFileStreamTest.CreateTestFile(Size: Integer);
var
	F: TFileStream;
	Buffer: TBytes;
	i: Integer;
begin
	F := TFileStream.Create(FTempFile, fmCreate);
	try
		SetLength(Buffer, Size);
		{ Fill with sequential bytes for easy verification }
		for i := 0 to Size - 1 do
			Buffer[i] := Byte(i mod 256);
		if Size > 0 then
			F.WriteBuffer(Buffer[0], Size);
	finally
		F.Free;
	end;
end;

{ Constructor tests }

procedure TChunkedFileStreamTest.TestCreateValidChunk;
var
	Stream: TChunkedFileStream;
begin
	CreateTestFile(1000);
	Stream := TChunkedFileStream.Create(FTempFile, fmOpenRead, 100, 200);
	try
		Assert.AreEqual(Int64(200), Stream.Size);
	finally
		Stream.Free;
	end;
end;

procedure TChunkedFileStreamTest.TestCreateChunkSizeZeroMeansRest;
var
	Stream: TChunkedFileStream;
begin
	{ ChunkSize = 0 means "rest of file from ChunkStart" }
	CreateTestFile(1000);
	Stream := TChunkedFileStream.Create(FTempFile, fmOpenRead, 300, 0);
	try
		Assert.AreEqual(Int64(700), Stream.Size); { 1000 - 300 = 700 }
	finally
		Stream.Free;
	end;
end;

procedure TChunkedFileStreamTest.TestCreateInvalidChunkStartNegative;
var
	RaisedException: Boolean;
begin
	CreateTestFile(1000);
	RaisedException := False;
	try
		TChunkedFileStream.Create(FTempFile, fmOpenRead, -1, 100).Free;
	except
		on E: EReadError do
			RaisedException := True;
	end;
	Assert.IsTrue(RaisedException, 'Expected EReadError for negative ChunkStart');
end;

procedure TChunkedFileStreamTest.TestCreateInvalidChunkStartBeyondFile;
var
	RaisedException: Boolean;
begin
	CreateTestFile(1000);
	RaisedException := False;
	try
		TChunkedFileStream.Create(FTempFile, fmOpenRead, 1001, 100).Free;
	except
		on E: EReadError do
			RaisedException := True;
	end;
	Assert.IsTrue(RaisedException, 'Expected EReadError for ChunkStart beyond file');
end;

procedure TChunkedFileStreamTest.TestCreateInvalidChunkSizeNegative;
var
	RaisedException: Boolean;
begin
	CreateTestFile(1000);
	RaisedException := False;
	try
		TChunkedFileStream.Create(FTempFile, fmOpenRead, 0, -1).Free;
	except
		on E: EReadError do
			RaisedException := True;
	end;
	Assert.IsTrue(RaisedException, 'Expected EReadError for negative ChunkSize');
end;

procedure TChunkedFileStreamTest.TestCreateInvalidChunkSizeBeyondFile;
var
	RaisedException: Boolean;
begin
	CreateTestFile(1000);
	RaisedException := False;
	try
		TChunkedFileStream.Create(FTempFile, fmOpenRead, 0, 1001).Free;
	except
		on E: EReadError do
			RaisedException := True;
	end;
	Assert.IsTrue(RaisedException, 'Expected EReadError for ChunkSize beyond file');
end;

{ Size property tests }

procedure TChunkedFileStreamTest.TestSizeReturnsChunkSize;
var
	Stream: TChunkedFileStream;
begin
	CreateTestFile(1000);
	Stream := TChunkedFileStream.Create(FTempFile, fmOpenRead, 0, 500);
	try
		Assert.AreEqual(Int64(500), Stream.Size);
	finally
		Stream.Free;
	end;
end;

procedure TChunkedFileStreamTest.TestSizeWithZeroChunkSizeReturnsRemainder;
var
	Stream: TChunkedFileStream;
begin
	CreateTestFile(1000);
	Stream := TChunkedFileStream.Create(FTempFile, fmOpenRead, 250, 0);
	try
		Assert.AreEqual(Int64(750), Stream.Size);
	finally
		Stream.Free;
	end;
end;

{ Read tests }

procedure TChunkedFileStreamTest.TestReadWithinChunk;
var
	Stream: TChunkedFileStream;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	CreateTestFile(1000);
	{ Chunk starts at byte 100, size 200 }
	Stream := TChunkedFileStream.Create(FTempFile, fmOpenRead, 100, 200);
	try
		SetLength(Buffer, 50);
		BytesRead := Stream.Read(Buffer[0], 50);
		Assert.AreEqual(50, BytesRead);
		{ First byte should be 100 (sequential fill) }
		Assert.AreEqual(Byte(100), Buffer[0]);
		Assert.AreEqual(Byte(149), Buffer[49]);
	finally
		Stream.Free;
	end;
end;

procedure TChunkedFileStreamTest.TestReadAtChunkBoundary;
var
	Stream: TChunkedFileStream;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	CreateTestFile(1000);
	{ Chunk starts at 100, size 200 (ends at 300) }
	Stream := TChunkedFileStream.Create(FTempFile, fmOpenRead, 100, 200);
	try
		SetLength(Buffer, 200);
		{ Read exactly the chunk size }
		BytesRead := Stream.Read(Buffer[0], 200);
		Assert.AreEqual(200, BytesRead);
		{ First byte = 100, last byte = 299 mod 256 = 43 }
		Assert.AreEqual(Byte(100), Buffer[0]);
		Assert.AreEqual(Byte(299 mod 256), Buffer[199]);
	finally
		Stream.Free;
	end;
end;

procedure TChunkedFileStreamTest.TestReadBeyondChunkBoundary;
var
	Stream: TChunkedFileStream;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	CreateTestFile(1000);
	{ Chunk starts at 100, size 200 }
	Stream := TChunkedFileStream.Create(FTempFile, fmOpenRead, 100, 200);
	try
		SetLength(Buffer, 300);
		{ Try to read 300 bytes, but chunk is only 200 }
		BytesRead := Stream.Read(Buffer[0], 300);
		Assert.AreEqual(200, BytesRead); { Should be clamped to chunk size }
	finally
		Stream.Free;
	end;
end;

procedure TChunkedFileStreamTest.TestReadFromMiddleOfChunk;
var
	Stream: TChunkedFileStream;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	CreateTestFile(1000);
	{ Chunk starts at 100, size 200 }
	Stream := TChunkedFileStream.Create(FTempFile, fmOpenRead, 100, 200);
	try
		{ Seek to position 50 within chunk (actual file position 150) }
		Stream.Seek(50, soBeginning);
		SetLength(Buffer, 50);
		BytesRead := Stream.Read(Buffer[0], 50);
		Assert.AreEqual(50, BytesRead);
		{ First byte should be 150 }
		Assert.AreEqual(Byte(150), Buffer[0]);
	finally
		Stream.Free;
	end;
end;

{ Seek tests }

procedure TChunkedFileStreamTest.TestSeekFromBeginning;
var
	Stream: TChunkedFileStream;
	NewPos: Int64;
begin
	CreateTestFile(1000);
	Stream := TChunkedFileStream.Create(FTempFile, fmOpenRead, 100, 200);
	try
		NewPos := Stream.Seek(50, soBeginning);
		Assert.AreEqual(Int64(50), NewPos);
	finally
		Stream.Free;
	end;
end;

procedure TChunkedFileStreamTest.TestSeekFromCurrent;
var
	Stream: TChunkedFileStream;
	NewPos: Int64;
begin
	CreateTestFile(1000);
	Stream := TChunkedFileStream.Create(FTempFile, fmOpenRead, 100, 200);
	try
		Stream.Seek(50, soBeginning);
		NewPos := Stream.Seek(25, soCurrent);
		Assert.AreEqual(Int64(75), NewPos);
	finally
		Stream.Free;
	end;
end;

procedure TChunkedFileStreamTest.TestSeekFromEnd;
var
	Stream: TChunkedFileStream;
	NewPos: Int64;
begin
	CreateTestFile(1000);
	Stream := TChunkedFileStream.Create(FTempFile, fmOpenRead, 100, 200);
	try
		{ Seek to 10 bytes before end of chunk }
		NewPos := Stream.Seek(-10, soEnd);
		Assert.AreEqual(Int64(190), NewPos);
	finally
		Stream.Free;
	end;
end;

procedure TChunkedFileStreamTest.TestSeekBeyondChunkClamps;
var
	Stream: TChunkedFileStream;
	NewPos: Int64;
begin
	CreateTestFile(1000);
	Stream := TChunkedFileStream.Create(FTempFile, fmOpenRead, 100, 200);
	try
		{ Seek 1000 bytes from current (0) - should clamp to chunk size }
		NewPos := Stream.Seek(1000, soCurrent);
		Assert.AreEqual(Int64(200), NewPos);
	finally
		Stream.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TChunkedFileStreamTest);

end.
