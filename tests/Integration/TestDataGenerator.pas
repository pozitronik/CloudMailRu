unit TestDataGenerator;

{Programmatic test data generation for integration tests.
	Creates test files of various sizes without external dependencies.}

interface

uses
	System.SysUtils,
	System.Classes,
	System.Hash;

type
	{Generates test data for integration tests}
	TTestDataGenerator = class
	public
		{Create a small test file with pseudo-random repeatable content.
			@param SizeBytes Size in bytes (default 1KB)
			@param Seed Random seed for reproducible content (default 42)
			@return Memory stream with test data (caller owns the stream)}
		class function CreateSmallTestFile(SizeBytes: Integer = 1024; Seed: Integer = 42): TMemoryStream; static;

		{Create a large test file suitable for chunked upload testing.
			Uses efficient block-based generation.
			@param SizeBytes Size in bytes
			@param Seed Random seed for reproducible content (default 42)
			@return Memory stream with test data (caller owns the stream)}
		class function CreateLargeTestFile(SizeBytes: Integer; Seed: Integer = 42): TMemoryStream; static;

		{Create test content as byte array.
			@param SizeBytes Size in bytes
			@param Seed Random seed for reproducible content (default 42)
			@return Byte array with test data}
		class function CreateTestContent(SizeBytes: Integer; Seed: Integer = 42): TBytes; static;

		{Calculate SHA1 hash of a stream (cloud.mail.ru uses SHA1).
			Stream position is reset to beginning after calculation.
			@param Stream Stream to hash
			@return Lowercase hex string of SHA1 hash}
		class function CalculateSHA1Hash(Stream: TStream): WideString; static;

		{Calculate SHA1 hash of byte array.
			@param Data Byte array to hash
			@return Lowercase hex string of SHA1 hash}
		class function CalculateSHA1HashBytes(const Data: TBytes): WideString; static;

		{Generate a unique filename for test isolation.
			@param Prefix Optional prefix (default 'test')
			@param Extension Optional extension including dot (default '.txt')
			@return Unique filename like 'test_20240115_143052_abc123.txt'}
		class function GenerateUniqueFilename(const Prefix: WideString = 'test'; const Extension: WideString = '.txt'): WideString; static;

		{Generate a unique folder name for test isolation.
			@param Prefix Optional prefix (default 'TestRun')
			@return Unique folder name like 'TestRun_20240115_143052'}
		class function GenerateUniqueFolderName(const Prefix: WideString = 'TestRun'): WideString; static;

		{Create test text content with identifiable pattern.
			@param LineCount Number of lines
			@param LineLength Approximate length per line
			@return String content with numbered lines}
		class function CreateTextContent(LineCount: Integer; LineLength: Integer = 80): WideString; static;
	end;

implementation

{TTestDataGenerator}

class function TTestDataGenerator.CreateSmallTestFile(SizeBytes: Integer; Seed: Integer): TMemoryStream;
var
	Data: TBytes;
begin
	Data := CreateTestContent(SizeBytes, Seed);
	Result := TMemoryStream.Create;
	Result.WriteBuffer(Data[0], Length(Data));
	Result.Position := 0;
end;

class function TTestDataGenerator.CreateLargeTestFile(SizeBytes: Integer; Seed: Integer): TMemoryStream;
const
	BLOCK_SIZE = 65536; {64KB blocks for efficient generation}
var
	BlockData: TBytes;
	RemainingBytes: Integer;
	CurrentBlock: Integer;
	BlockBytes: Integer;
	I: Integer;
	RandSeed: Integer;
begin
	Result := TMemoryStream.Create;
	Result.Size := SizeBytes; {Pre-allocate for efficiency}
	Result.Position := 0;

	SetLength(BlockData, BLOCK_SIZE);
	RemainingBytes := SizeBytes;
	CurrentBlock := 0;

	while RemainingBytes > 0 do
	begin
		{Each block uses a derived seed for reproducibility}
		RandSeed := Seed + CurrentBlock;
		RandSeed := ((RandSeed * 1103515245) + 12345) and $7FFFFFFF;

		BlockBytes := BLOCK_SIZE;
		if RemainingBytes < BLOCK_SIZE then
			BlockBytes := RemainingBytes;

		{Fill block with pseudo-random bytes}
		for I := 0 to BlockBytes - 1 do
		begin
			RandSeed := ((RandSeed * 1103515245) + 12345) and $7FFFFFFF;
			BlockData[I] := Byte(RandSeed and $FF);
		end;

		Result.WriteBuffer(BlockData[0], BlockBytes);
		Dec(RemainingBytes, BlockBytes);
		Inc(CurrentBlock);
	end;

	Result.Position := 0;
end;

class function TTestDataGenerator.CreateTestContent(SizeBytes: Integer; Seed: Integer): TBytes;
var
	I: Integer;
	RandSeed: Integer;
begin
	SetLength(Result, SizeBytes);
	RandSeed := Seed;

	for I := 0 to SizeBytes - 1 do
	begin
		{Linear congruential generator for reproducible pseudo-random bytes}
		RandSeed := ((RandSeed * 1103515245) + 12345) and $7FFFFFFF;
		Result[I] := Byte(RandSeed and $FF);
	end;
end;

class function TTestDataGenerator.CalculateSHA1Hash(Stream: TStream): WideString;
var
	Hasher: THashSHA1;
	Buffer: TBytes;
	BytesRead: Integer;
	OriginalPosition: Int64;
begin
	OriginalPosition := Stream.Position;
	Stream.Position := 0;

	Hasher := THashSHA1.Create;
	SetLength(Buffer, 65536); {64KB buffer}

	repeat
		BytesRead := Stream.Read(Buffer[0], Length(Buffer));
		if BytesRead > 0 then
			Hasher.Update(Buffer, BytesRead);
	until BytesRead = 0;

	Result := LowerCase(Hasher.HashAsString);
	Stream.Position := OriginalPosition;
end;

class function TTestDataGenerator.CalculateSHA1HashBytes(const Data: TBytes): WideString;
var
	Stream: TBytesStream;
begin
	Stream := TBytesStream.Create(Data);
	try
		Result := CalculateSHA1Hash(Stream);
	finally
		Stream.Free;
	end;
end;

class function TTestDataGenerator.GenerateUniqueFilename(const Prefix: WideString; const Extension: WideString): WideString;
var
	Timestamp: WideString;
	RandomPart: WideString;
begin
	Timestamp := FormatDateTime('yyyymmdd_hhnnss', Now);
	RandomPart := IntToHex(Random($FFFFFF), 6);
	Result := Format('%s_%s_%s%s', [Prefix, Timestamp, RandomPart, Extension]);
end;

class function TTestDataGenerator.GenerateUniqueFolderName(const Prefix: WideString): WideString;
var
	Timestamp: WideString;
begin
	Timestamp := FormatDateTime('yyyymmdd_hhnnss', Now);
	Result := Format('%s_%s', [Prefix, Timestamp]);
end;

class function TTestDataGenerator.CreateTextContent(LineCount: Integer; LineLength: Integer): WideString;
const
	CHARS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ';
var
	Builder: TStringBuilder;
	I, J: Integer;
	LinePrefix: WideString;
begin
	Builder := TStringBuilder.Create;
	try
		for I := 1 to LineCount do
		begin
			LinePrefix := Format('Line %5d: ', [I]);
			Builder.Append(LinePrefix);

			{Fill remaining line length with pattern}
			for J := 1 to LineLength - Length(LinePrefix) - 2 do {-2 for CRLF}
				Builder.Append(CHARS[(J mod Length(CHARS)) + 1]);

			Builder.AppendLine;
		end;
		Result := Builder.ToString;
	finally
		Builder.Free;
	end;
end;

initialization
	Randomize;

end.
