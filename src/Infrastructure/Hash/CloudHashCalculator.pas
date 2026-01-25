unit CloudHashCalculator;

{TODO: Performance optimizations for hash calculation:
 1. Increase BUFFER_SIZE to 64-128KB (trivial, 10-30% speedup)
 2. Use Windows BCrypt API for hardware-accelerated SHA1 (SHA-NI instructions)
 3. Use OpenSSL EVP functions (already loaded for HTTPS)
 4. Memory-mapped files for very large files
 Current bottleneck: 8KB buffer causes excessive syscalls and iterations.}

interface

uses
	System.Classes,
	TCProgress,
	WindowsFileSystem;

type
	{Interface for cloud hash calculation - allows dependency injection and testability}
	ICloudHashCalculator = interface
		['{45039BC6-7501-41B6-8C82-620E45895E41}']
		function CalculateHash(Path: WideString): WideString; overload;
		function CalculateHash(Stream: TStream; ProgressPath: WideString): WideString; overload;
	end;

	{Cloud Mail.ru proprietary hash calculator.
		Hash algorithm:
		- Files < 21 bytes: pad to 20 bytes, return hex digest of raw bytes
		- Files >= 21 bytes: SHA1('mrCloud' + content + size_string)}
	TCloudHashCalculator = class(TInterfacedObject, ICloudHashCalculator)
	private
		FProgress: IProgress;
		FFileSystem: IFileSystem;
	public
		constructor Create(Progress: IProgress; FileSystem: IFileSystem);
		{Calculate hash for file at given path}
		function CalculateHash(Path: WideString): WideString; overload;
		{Calculate hash for stream content with progress reporting}
		function CalculateHash(Stream: TStream; ProgressPath: WideString): WideString; overload;
	end;

	{Null implementation for testing or when hash calculation is not needed}
	TNullHashCalculator = class(TInterfacedObject, ICloudHashCalculator)
	public
		function CalculateHash(Path: WideString): WideString; overload;
		function CalculateHash(Stream: TStream; ProgressPath: WideString): WideString; overload;
	end;

implementation

uses
	System.Hash,
	System.SysUtils,
	FileHelper,
	PathHelper,
	LANGUAGE_STRINGS;

const
	{Cloud Mail.ru hash algorithm constants}
	HASH_SEED = 'mrCloud';
	SMALL_FILE_THRESHOLD = 21;
	SMALL_FILE_BUFFER = 20;
	BUFFER_SIZE = 8192;

	{TCloudHashCalculator}

constructor TCloudHashCalculator.Create(Progress: IProgress; FileSystem: IFileSystem);
begin
	inherited Create;
	FProgress := Progress;
	FFileSystem := FileSystem;
end;

function TCloudHashCalculator.CalculateHash(Path: WideString): WideString;
var
	Stream: TStream;
begin
	Result := EmptyWideStr;
	if not FFileSystem.FileExists(Path) then
		Exit;

	try
		Stream := TBufferedFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
	except
		Exit;
	end;
	try
		Result := CalculateHash(Stream, GetLFCFilePath(Path));
	finally
		Stream.Free;
	end;
end;

function TCloudHashCalculator.CalculateHash(Stream: TStream; ProgressPath: WideString): WideString;
var
	sha1: THashSHA1;
	buffer: array [0 .. BUFFER_SIZE - 1] of byte;
	read, iteration, processedBytes: Int64;
	initBuffer, finalBuffer: TBytes;
	Percent: Integer;
	Aborted: Boolean;
begin
	Stream.Position := 0;
	Result := EmptyWideStr;
	if Stream.Size < SMALL_FILE_THRESHOLD then
	begin
		SetLength(initBuffer, SMALL_FILE_BUFFER);
		Stream.read(initBuffer, Stream.Size);
		Result := UpperCase(THash.DigestAsString(initBuffer));
		Exit;
	end;

	FillChar(buffer, sizeof(buffer), 0);
	initBuffer := TEncoding.UTF8.GetBytes(HASH_SEED);

	sha1 := THashSHA1.Create;
	sha1.Update(initBuffer, length(initBuffer));
	iteration := 0;
	repeat
		iteration := iteration + 1;
		processedBytes := BUFFER_SIZE * iteration;
		Percent := Round((processedBytes / Stream.Size) * 100);
		if Percent > 100 then
			Percent := 100;

		read := Stream.read(buffer, BUFFER_SIZE);
		sha1.Update(buffer, read);
		Aborted := FProgress.Progress(ProgressPath, CALCULATING_HASH, Percent);
	until (read < sizeof(buffer)) or Aborted;

	finalBuffer := TEncoding.UTF8.GetBytes(Stream.Size.ToString);
	sha1.Update(finalBuffer, length(finalBuffer));
	if (not Aborted) then
		Result := UpperCase(sha1.HashAsString);
	sha1.Reset;
end;

{TNullHashCalculator}

function TNullHashCalculator.CalculateHash(Path: WideString): WideString;
begin
	Result := EmptyWideStr;
end;

function TNullHashCalculator.CalculateHash(Stream: TStream; ProgressPath: WideString): WideString;
begin
	Result := EmptyWideStr;
end;

end.
