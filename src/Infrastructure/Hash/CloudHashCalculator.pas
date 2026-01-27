unit CloudHashCalculator;

{Hash calculator implementations for Cloud Mail.ru proprietary hash algorithm.

	Strategy selection:
	- HashStrategyAuto: Auto-select best available (BCrypt > OpenSSL > Delphi)
	- HashStrategyDelphi: Use Delphi's System.Hash.THashSHA1 with 64KB buffer
	- HashStrategyBCrypt: Use Windows BCrypt/CNG API (hardware-accelerated SHA-NI)
	- HashStrategyOpenSSL: Use OpenSSL EVP functions via centralized provider

	BCrypt typically provides 2-3x speedup over Delphi implementation on modern CPUs
	with SHA-NI instructions (Intel Goldmont+, Ice Lake+; AMD Zen+).}

interface

uses
	System.Classes,
	TCProgress,
	WindowsFileSystem,
	OpenSSLProvider;

type
	{Interface for cloud hash calculation - allows dependency injection and testability}
	ICloudHashCalculator = interface
		['{45039BC6-7501-41B6-8C82-620E45895E41}']
		function CalculateHash(Path: WideString): WideString; overload;
		function CalculateHash(Stream: TStream; ProgressPath: WideString): WideString; overload;
	end;

	{Base class with shared CloudHash algorithm logic.
		Hash algorithm:
		- Files < 21 bytes: pad to 20 bytes, return hex digest of raw bytes
		- Files >= 21 bytes: SHA1('mrCloud' + content + size_string)}
	TCloudHashCalculatorBase = class(TInterfacedObject, ICloudHashCalculator)
	protected
		FProgress: IProgress;
		FFileSystem: IFileSystem;
		{Override in subclasses to provide SHA1 implementation}
		function CalculateSHA1(Stream: TStream; ProgressPath: WideString): WideString; virtual; abstract;
	public
		constructor Create(Progress: IProgress; FileSystem: IFileSystem);
		function CalculateHash(Path: WideString): WideString; overload;
		function CalculateHash(Stream: TStream; ProgressPath: WideString): WideString; overload;
	end;

	{Delphi implementation using System.Hash.THashSHA1 with optimized 64KB buffer.
		This is the baseline implementation with ~3x speedup over original 8KB buffer.}
	TCloudHashCalculator = class(TCloudHashCalculatorBase)
	protected
		function CalculateSHA1(Stream: TStream; ProgressPath: WideString): WideString; override;
	end;

	{Windows BCrypt/CNG implementation with hardware acceleration.
		Uses SHA-NI instructions when available for 2-3x speedup over Delphi.}
	TCloudHashCalculatorBCrypt = class(TCloudHashCalculatorBase)
	protected
		function CalculateSHA1(Stream: TStream; ProgressPath: WideString): WideString; override;
	end;

	{OpenSSL EVP implementation using centralized OpenSSL provider.
		Uses the same library instance as Indy (HTTPS), avoiding duplicate loading.}
	TCloudHashCalculatorOpenSSL = class(TCloudHashCalculatorBase)
	private
		FOpenSSLProvider: IOpenSSLProvider;
	protected
		function CalculateSHA1(Stream: TStream; ProgressPath: WideString): WideString; override;
	public
		constructor Create(Progress: IProgress; FileSystem: IFileSystem; OpenSSLProvider: IOpenSSLProvider);
	end;

	{Null implementation for testing or when hash calculation is not needed}
	TNullHashCalculator = class(TInterfacedObject, ICloudHashCalculator)
	public
		function CalculateHash(Path: WideString): WideString; overload;
		function CalculateHash(Stream: TStream; ProgressPath: WideString): WideString; overload;
	end;

	{Factory function to create hash calculator based on strategy setting.
		@param Strategy Hash strategy constant (HashStrategyAuto, HashStrategyDelphi, etc.)
		@param Progress Progress reporter for long operations
		@param FileSystem File system abstraction for file access
		@param OpenSSLProvider Centralized OpenSSL provider for OpenSSL strategy
		@return Appropriate ICloudHashCalculator implementation}
function CreateHashCalculator(Strategy: Integer; Progress: IProgress; FileSystem: IFileSystem; OpenSSLProvider: IOpenSSLProvider): ICloudHashCalculator;

{Check if BCrypt SHA1 is available on this system}
function IsBCryptAvailable: Boolean;

{Check if OpenSSL EVP functions are available via provider
	@param OpenSSLProvider Provider to check; returns False if nil}
function IsOpenSSLAvailable(OpenSSLProvider: IOpenSSLProvider): Boolean;

implementation

uses
	System.Hash,
	System.SysUtils,
	Windows,
	FileHelper,
	PathHelper,
	LanguageStrings,
	SettingsConstants;

const
	{Cloud Mail.ru hash algorithm constants}
	HASH_SEED = 'mrCloud';
	SMALL_FILE_THRESHOLD = 21;
	SMALL_FILE_BUFFER = 20;
	{Optimized buffer size - 64KB reduces syscalls and improves throughput}
	BUFFER_SIZE = 65536;

	{BCrypt API declarations (Windows Vista+)}
const
	BCRYPT_SHA1_ALGORITHM: PWideChar = 'SHA1';
	BCRYPT_HASH_LENGTH: PWideChar = 'HashDigestLength';

type
	BCRYPT_ALG_HANDLE = THandle;
	BCRYPT_HASH_HANDLE = THandle;
	NTSTATUS = LongInt;

const
	STATUS_SUCCESS = 0;

function BCryptOpenAlgorithmProvider(out phAlgorithm: BCRYPT_ALG_HANDLE; pszAlgId: PWideChar; pszImplementation: PWideChar; dwFlags: ULONG): NTSTATUS; stdcall; external 'bcrypt.dll';

function BCryptCloseAlgorithmProvider(hAlgorithm: BCRYPT_ALG_HANDLE; dwFlags: ULONG): NTSTATUS; stdcall; external 'bcrypt.dll';

function BCryptCreateHash(hAlgorithm: BCRYPT_ALG_HANDLE; out phHash: BCRYPT_HASH_HANDLE; pbHashObject: Pointer; cbHashObject: ULONG; pbSecret: Pointer; cbSecret: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external 'bcrypt.dll';

function BCryptDestroyHash(hHash: BCRYPT_HASH_HANDLE): NTSTATUS; stdcall; external 'bcrypt.dll';

function BCryptHashData(hHash: BCRYPT_HASH_HANDLE; pbInput: Pointer; cbInput: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external 'bcrypt.dll';

function BCryptFinishHash(hHash: BCRYPT_HASH_HANDLE; pbOutput: Pointer; cbOutput: ULONG; dwFlags: ULONG): NTSTATUS; stdcall; external 'bcrypt.dll';


function IsBCryptAvailable: Boolean;
var
	hAlg: BCRYPT_ALG_HANDLE;
begin
	{BCrypt is available on Windows Vista+ which is always true for supported Windows versions}
	Result := BCryptOpenAlgorithmProvider(hAlg, BCRYPT_SHA1_ALGORITHM, nil, 0) = STATUS_SUCCESS;
	if Result then
		BCryptCloseAlgorithmProvider(hAlg, 0);
end;

function IsOpenSSLAvailable(OpenSSLProvider: IOpenSSLProvider): Boolean;
begin
	if OpenSSLProvider = nil then
		Result := False
	else
		Result := OpenSSLProvider.IsAvailable;
end;

function CreateHashCalculator(Strategy: Integer; Progress: IProgress; FileSystem: IFileSystem; OpenSSLProvider: IOpenSSLProvider): ICloudHashCalculator;
begin
	case Strategy of
		HashStrategyDelphi:
			Result := TCloudHashCalculator.Create(Progress, FileSystem);
		HashStrategyBCrypt:
			if IsBCryptAvailable then
				Result := TCloudHashCalculatorBCrypt.Create(Progress, FileSystem)
			else
				Result := TCloudHashCalculator.Create(Progress, FileSystem);
		HashStrategyOpenSSL:
			if IsOpenSSLAvailable(OpenSSLProvider) then
				Result := TCloudHashCalculatorOpenSSL.Create(Progress, FileSystem, OpenSSLProvider)
			else
				Result := TCloudHashCalculator.Create(Progress, FileSystem);
		else {HashStrategyAuto}
			begin
				{Prefer BCrypt for hardware acceleration, fallback to OpenSSL, then Delphi}
				if IsBCryptAvailable then
					Result := TCloudHashCalculatorBCrypt.Create(Progress, FileSystem)
				else if IsOpenSSLAvailable(OpenSSLProvider) then
					Result := TCloudHashCalculatorOpenSSL.Create(Progress, FileSystem, OpenSSLProvider)
				else
					Result := TCloudHashCalculator.Create(Progress, FileSystem);
			end;
	end;
end;

{TCloudHashCalculatorBase}

constructor TCloudHashCalculatorBase.Create(Progress: IProgress; FileSystem: IFileSystem);
begin
	inherited Create;
	FProgress := Progress;
	FFileSystem := FileSystem;
end;

function TCloudHashCalculatorBase.CalculateHash(Path: WideString): WideString;
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

function TCloudHashCalculatorBase.CalculateHash(Stream: TStream; ProgressPath: WideString): WideString;
var
	initBuffer: TBytes;
begin
	Stream.Position := 0;
	Result := EmptyWideStr;

	{Small files: pad to 20 bytes and return hex digest directly}
	if Stream.Size < SMALL_FILE_THRESHOLD then
	begin
		SetLength(initBuffer, SMALL_FILE_BUFFER);
		Stream.Read(initBuffer, Stream.Size);
		Result := UpperCase(THash.DigestAsString(initBuffer));
		Exit;
	end;

	{Large files: use SHA1('mrCloud' + content + size_string)}
	Result := CalculateSHA1(Stream, ProgressPath);
end;

{TCloudHashCalculator - Delphi implementation}

function TCloudHashCalculator.CalculateSHA1(Stream: TStream; ProgressPath: WideString): WideString;
var
	sha1: THashSHA1;
	buffer: array [0 .. BUFFER_SIZE - 1] of byte;
	bytesRead: Int64;
	processedBytes: Int64;
	initBuffer, finalBuffer: TBytes;
	Percent: Integer;
	Aborted: Boolean;
begin
	Result := EmptyWideStr;
	Stream.Position := 0;
	FillChar(buffer, SizeOf(buffer), 0);
	initBuffer := TEncoding.UTF8.GetBytes(HASH_SEED);

	sha1 := THashSHA1.Create;
	sha1.Update(initBuffer, Length(initBuffer));
	processedBytes := 0;
	Aborted := False;

	repeat
		bytesRead := Stream.Read(buffer, BUFFER_SIZE);
		if bytesRead > 0 then
		begin
			sha1.Update(buffer, bytesRead);
			Inc(processedBytes, bytesRead);
			Percent := Round((processedBytes / Stream.Size) * 100);
			if Percent > 100 then
				Percent := 100;
			Aborted := FProgress.Progress(ProgressPath, Format(CALCULATING_HASH_FOR, [ProgressPath]), Percent);
		end;
	until (bytesRead < BUFFER_SIZE) or Aborted;

	if not Aborted then
	begin
		finalBuffer := TEncoding.UTF8.GetBytes(Stream.Size.ToString);
		sha1.Update(finalBuffer, Length(finalBuffer));
		Result := UpperCase(sha1.HashAsString);
	end;
	sha1.Reset;
end;

{TCloudHashCalculatorBCrypt - Windows CNG implementation}

function TCloudHashCalculatorBCrypt.CalculateSHA1(Stream: TStream; ProgressPath: WideString): WideString;
var
	hAlg: BCRYPT_ALG_HANDLE;
	hHash: BCRYPT_HASH_HANDLE;
	buffer: array [0 .. BUFFER_SIZE - 1] of byte;
	hashValue: array [0 .. 19] of byte; {SHA1 = 20 bytes}
	bytesRead: Int64;
	processedBytes: Int64;
	initBuffer, finalBuffer: TBytes;
	Percent: Integer;
	Aborted: Boolean;
	i: Integer;
begin
	Result := EmptyWideStr;
	Stream.Position := 0;
	Aborted := False;

	if BCryptOpenAlgorithmProvider(hAlg, BCRYPT_SHA1_ALGORITHM, nil, 0) <> STATUS_SUCCESS then
	begin
		{Fallback to Delphi implementation if BCrypt fails to initialize}
		Result := TCloudHashCalculator.Create(FProgress, FFileSystem).CalculateSHA1(Stream, ProgressPath);
		Exit;
	end;

	try
		if BCryptCreateHash(hAlg, hHash, nil, 0, nil, 0, 0) <> STATUS_SUCCESS then
		begin
			Result := TCloudHashCalculator.Create(FProgress, FFileSystem).CalculateSHA1(Stream, ProgressPath);
			Exit;
		end;

		try
			{Hash the seed prefix}
			initBuffer := TEncoding.UTF8.GetBytes(HASH_SEED);
			BCryptHashData(hHash, @initBuffer[0], Length(initBuffer), 0);

			{Hash the file content}
			processedBytes := 0;
			repeat
				bytesRead := Stream.Read(buffer, BUFFER_SIZE);
				if bytesRead > 0 then
				begin
					BCryptHashData(hHash, @buffer[0], bytesRead, 0);
					Inc(processedBytes, bytesRead);
					Percent := Round((processedBytes / Stream.Size) * 100);
					if Percent > 100 then
						Percent := 100;
					Aborted := FProgress.Progress(ProgressPath, Format(CALCULATING_HASH_FOR, [ProgressPath]), Percent);
				end;
			until (bytesRead < BUFFER_SIZE) or Aborted;

			if not Aborted then
			begin
				{Hash the size suffix}
				finalBuffer := TEncoding.UTF8.GetBytes(Stream.Size.ToString);
				BCryptHashData(hHash, @finalBuffer[0], Length(finalBuffer), 0);

				{Get final hash}
				BCryptFinishHash(hHash, @hashValue[0], SizeOf(hashValue), 0);

				{Convert to hex string}
				Result := '';
				for i := 0 to High(hashValue) do
					Result := Result + IntToHex(hashValue[i], 2);
			end;
		finally
			BCryptDestroyHash(hHash);
		end;
	finally
		BCryptCloseAlgorithmProvider(hAlg, 0);
	end;
end;

{TCloudHashCalculatorOpenSSL - OpenSSL EVP implementation}

constructor TCloudHashCalculatorOpenSSL.Create(Progress: IProgress; FileSystem: IFileSystem; OpenSSLProvider: IOpenSSLProvider);
begin
	inherited Create(Progress, FileSystem);
	FOpenSSLProvider := OpenSSLProvider;
end;

function TCloudHashCalculatorOpenSSL.CalculateSHA1(Stream: TStream; ProgressPath: WideString): WideString;
var
	Funcs: TOpenSSLFunctions;
	ctx: Pointer;
	buffer: array [0 .. BUFFER_SIZE - 1] of byte;
	hashValue: array [0 .. 19] of byte; {SHA1 = 20 bytes}
	hashLen: Cardinal;
	bytesRead: Int64;
	processedBytes: Int64;
	initBuffer, finalBuffer: TBytes;
	Percent: Integer;
	Aborted: Boolean;
	i: Integer;
begin
	Result := EmptyWideStr;
	Stream.Position := 0;
	Aborted := False;

	{Get functions from provider}
	if (FOpenSSLProvider = nil) or not FOpenSSLProvider.IsAvailable then
	begin
		{Fallback to Delphi implementation if OpenSSL is not available}
		Result := TCloudHashCalculator.Create(FProgress, FFileSystem).CalculateSHA1(Stream, ProgressPath);
		Exit;
	end;

	Funcs := FOpenSSLProvider.GetFunctions;

	ctx := Funcs.EVP_MD_CTX_new();
	if ctx = nil then
	begin
		Result := TCloudHashCalculator.Create(FProgress, FFileSystem).CalculateSHA1(Stream, ProgressPath);
		Exit;
	end;

	try
		if Funcs.EVP_DigestInit_ex(ctx, Funcs.EVP_sha1(), nil) <> 1 then
		begin
			Result := TCloudHashCalculator.Create(FProgress, FFileSystem).CalculateSHA1(Stream, ProgressPath);
			Exit;
		end;

		{Hash the seed prefix}
		initBuffer := TEncoding.UTF8.GetBytes(HASH_SEED);
		Funcs.EVP_DigestUpdate(ctx, @initBuffer[0], Length(initBuffer));

		{Hash the file content}
		processedBytes := 0;
		repeat
			bytesRead := Stream.Read(buffer, BUFFER_SIZE);
			if bytesRead > 0 then
			begin
				Funcs.EVP_DigestUpdate(ctx, @buffer[0], bytesRead);
				Inc(processedBytes, bytesRead);
				Percent := Round((processedBytes / Stream.Size) * 100);
				if Percent > 100 then
					Percent := 100;
				Aborted := FProgress.Progress(ProgressPath, Format(CALCULATING_HASH_FOR, [ProgressPath]), Percent);
			end;
		until (bytesRead < BUFFER_SIZE) or Aborted;

		if not Aborted then
		begin
			{Hash the size suffix}
			finalBuffer := TEncoding.UTF8.GetBytes(Stream.Size.ToString);
			Funcs.EVP_DigestUpdate(ctx, @finalBuffer[0], Length(finalBuffer));

			{Get final hash}
			hashLen := SizeOf(hashValue);
			Funcs.EVP_DigestFinal_ex(ctx, @hashValue[0], hashLen);

			{Convert to hex string}
			Result := '';
			for i := 0 to High(hashValue) do
				Result := Result + IntToHex(hashValue[i], 2);
		end;
	finally
		Funcs.EVP_MD_CTX_free(ctx);
	end;
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
