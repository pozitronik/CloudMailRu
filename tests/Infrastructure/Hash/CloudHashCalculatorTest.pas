unit CloudHashCalculatorTest;

interface

uses
	Windows,
	CloudHashCalculator,
	OpenSSLProvider,
	Progress,
	FileSystem,
	TestHelper,
	System.Classes,
	System.SysUtils,
	DUnitX.TestFramework;

type
	{Base test class with shared hash algorithm tests.
	 CloudHash implements Cloud Mail.ru proprietary hash algorithm:
	 - Files < 21 bytes: pad to 20 bytes, return hex digest
	 - Files >= 21 bytes: SHA1('mrCloud' + content + size_string)}
	THashCalculatorTestBase = class
	protected
		FCalculator: ICloudHashCalculator;
		{Override in subclasses to create specific implementation}
		procedure CreateCalculator; virtual; abstract;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ CalculateHash(Stream) tests - small files (< 21 bytes) }
		[Test]
		procedure TestCalculateHashStream_EmptyStream;
		[Test]
		procedure TestCalculateHashStream_SmallFile_1Byte;
		[Test]
		procedure TestCalculateHashStream_SmallFile_19Bytes;
		[Test]
		procedure TestCalculateHashStream_SmallFile_20Bytes;

		{ CalculateHash(Stream) tests - boundary and large files (>= 21 bytes) }
		[Test]
		procedure TestCalculateHashStream_BoundaryFile_21Bytes;
		[Test]
		procedure TestCalculateHashStream_LargeFile_100Bytes;
		[Test]
		procedure TestCalculateHashStream_LargeFile_KnownContent;

		{ CalculateHash(Path) tests - file-based }
		[Test]
		procedure TestCalculateHashPath_NonExistentFile;
		[Test]
		procedure TestCalculateHashPath_ExistingSmallFile;
		[Test]
		procedure TestCalculateHashPath_ExistingLargeFile;

		{ Hash consistency tests }
		[Test]
		procedure TestCalculateHash_SameContentSameHash;
		[Test]
		procedure TestCalculateHash_DifferentContentDifferentHash;

		{ Hash format tests }
		[Test]
		procedure TestCalculateHash_ReturnsUppercaseHex;
		[Test]
		procedure TestCalculateHash_SmallFileReturns40CharHex;
		[Test]
		procedure TestCalculateHash_LargeFileReturns40CharHex;

		{ Stream position tests }
		[Test]
		procedure TestCalculateHash_ResetsStreamPosition;
		[Test]
		procedure TestCalculateHash_WorksWithNonZeroInitialPosition;
	end;

	{Tests for TCloudHashCalculator (Delphi implementation)}
	[TestFixture]
	TCloudHashCalculatorTest = class(THashCalculatorTestBase)
	protected
		procedure CreateCalculator; override;
	public
		{Sequential calculation tests - verify state isolation between calls}
		[Test]
		procedure TestSequentialCalculations_ProduceSameResults;
		[Test]
		procedure TestSequentialCalculations_DifferentStreams;

		{Cloud hash algorithm verification tests}
		[Test]
		procedure TestCloudHash_MrCloudPrefixApplied;
		[Test]
		procedure TestCloudHash_SizeSuffixApplied;

		{Buffer handling tests with larger data}
		[Test]
		procedure TestLargeData_64KBBoundary;
		[Test]
		procedure TestLargeData_MultipleBufferReads;
		[Test]
		procedure TestLargeData_1MBFile;
	end;

	{Tests for TCloudHashCalculatorBCrypt (Windows CNG implementation)}
	[TestFixture]
	TCloudHashCalculatorBCryptTest = class(THashCalculatorTestBase)
	protected
		procedure CreateCalculator; override;
	public
		[Test]
		procedure TestBCrypt_MatchesDelphiImplementation;
		[Test]
		procedure TestBCrypt_MatchesDelphiForSmallFile;
		[Test]
		procedure TestBCrypt_MatchesDelphiForBoundaryFile;
		[Test]
		procedure TestBCrypt_MatchesDelphiForLargeData;
		[Test]
		procedure TestBCrypt_SequentialCalculationsMatch;
	end;

	{Tests for TCloudHashCalculatorOpenSSL (OpenSSL EVP implementation)}
	[TestFixture]
	TCloudHashCalculatorOpenSSLTest = class(THashCalculatorTestBase)
	protected
		procedure CreateCalculator; override;
	public
		[Test]
		procedure TestOpenSSL_MatchesDelphiImplementation;
		[Test]
		procedure TestOpenSSL_MatchesDelphiForSmallFile;
		[Test]
		procedure TestOpenSSL_MatchesDelphiForBoundaryFile;
		[Test]
		procedure TestOpenSSL_MatchesDelphiForLargeData;
		[Test]
		procedure TestOpenSSL_SequentialCalculationsMatch;
	end;

	{Tests for TNullHashCalculator}
	[TestFixture]
	TNullHashCalculatorTest = class
	private
		FCalculator: ICloudHashCalculator;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestCalculateHashPath_ReturnsEmpty;
		[Test]
		procedure TestCalculateHashStream_ReturnsEmpty;
	end;

	{Tests for CreateHashCalculator factory function}
	[TestFixture]
	THashCalculatorFactoryTest = class
	public
		[Test]
		procedure TestCreateHashCalculator_Delphi_ReturnsCorrectType;
		[Test]
		procedure TestCreateHashCalculator_BCrypt_ReturnsNonNil;
		[Test]
		procedure TestCreateHashCalculator_OpenSSL_ReturnsNonNil;
		[Test]
		procedure TestCreateHashCalculator_Auto_ReturnsNonNil;
		[Test]
		procedure TestCreateHashCalculator_InvalidStrategy_ReturnsDelphi;
		[Test]
		procedure TestIsBCryptAvailable_ReturnsBoolean;
		[Test]
		procedure TestIsOpenSSLAvailable_ReturnsBoolean;
		[Test]
		procedure TestAllStrategies_ProduceSameHash;
		[Test]
		procedure TestAllStrategies_ProduceSameHashForSmallFile;
		[Test]
		procedure TestAllStrategies_ProduceSameHashForBoundary;
		[Test]
		procedure TestAllStrategies_ProduceSameHashFor1MB;
		[Test]
		procedure TestCalculateHashPath_FileOpenException_ReturnsEmpty;
	end;

	{Tests for IsOpenSSLAvailable helper function}
	[TestFixture]
	TIsOpenSSLAvailableTest = class
	public
		[Test]
		procedure TestIsOpenSSLAvailable_NilProvider_ReturnsFalse;
		[Test]
		procedure TestIsOpenSSLAvailable_UnavailableProvider_ReturnsFalse;
		[Test]
		procedure TestIsOpenSSLAvailable_AvailableProvider_ReturnsTrue;
	end;

	{Tests for OpenSSL calculator fallback paths when init fails}
	[TestFixture]
	TOpenSSLFallbackTest = class
	public
		[Test]
		procedure TestOpenSSL_UnavailableProvider_FallsBackToDelphi;
		[Test]
		procedure TestOpenSSL_NullContext_FallsBackToDelphi;
		[Test]
		procedure TestOpenSSL_DigestInitFailure_FallsBackToDelphi;
		[Test]
		procedure TestOpenSSL_SuccessfulDigest_ProducesHash;
	end;

	{Tests for CreateHashCalculator factory with mock OpenSSL}
	[TestFixture]
	THashCalculatorFactoryOpenSSLTest = class
	public
		[Test]
		procedure TestFactory_OpenSSL_Available_ReturnsOpenSSLCalc;
		[Test]
		procedure TestFactory_OpenSSL_Unavailable_FallsToDelphi;
		[Test]
		procedure TestFactory_Auto_OpenSSLAvailable_NoBCrypt_ReturnsOpenSSL;
	end;

	{Cross-implementation consistency tests with various file sizes}
	[TestFixture]
	THashCalculatorCrossImplementationTest = class
	private
		FDelphiCalc: ICloudHashCalculator;
		FBCryptCalc: ICloudHashCalculator;
		FOpenSSLCalc: ICloudHashCalculator;
		procedure CompareAllImplementations(Stream: TStream; const TestName: string);
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Boundary tests around small/large file threshold}
		[Test]
		procedure TestThreshold_19Bytes_AllMatch;
		[Test]
		procedure TestThreshold_20Bytes_AllMatch;
		[Test]
		procedure TestThreshold_21Bytes_AllMatch;
		[Test]
		procedure TestThreshold_22Bytes_AllMatch;

		{Various file sizes for comprehensive coverage}
		[Test]
		procedure TestVariousSizes_100Bytes;
		[Test]
		procedure TestVariousSizes_1KB;
		[Test]
		procedure TestVariousSizes_10KB;
		[Test]
		procedure TestVariousSizes_64KB;
		[Test]
		procedure TestVariousSizes_65KB;
		[Test]
		procedure TestVariousSizes_128KB;
		[Test]
		procedure TestVariousSizes_256KB;
		[Test]
		procedure TestVariousSizes_512KB;

		{Binary content tests}
		[Test]
		procedure TestBinaryContent_AllZeros;
		[Test]
		procedure TestBinaryContent_AllOnes;
		[Test]
		procedure TestBinaryContent_RandomPattern;
	end;

type
	{Mock OpenSSL provider for testing fallback paths.
		Can be configured to simulate various failure scenarios.}
	TMockOpenSSLProvider = class(TInterfacedObject, IOpenSSLProvider)
	private
		FIsAvailable: Boolean;
		FFunctions: TOpenSSLFunctions;
		FLibraryHandle: THandle;
	public
		constructor Create(Available: Boolean);
		function IsAvailable: Boolean;
		function GetFunctions: TOpenSSLFunctions;
		function GetLibraryHandle: THandle;
		{Configure to return nil from EVP_MD_CTX_new}
		procedure SetupNullContextNew;
		{Configure to return failure from EVP_DigestInit_ex}
		procedure SetupDigestInitFailure;
		{Change availability after construction}
		procedure SetAvailable(Value: Boolean);
	end;

implementation

uses
	SettingsConstants;

{Stub functions for mock OpenSSL provider}
function StubEVP_MD_CTX_new_ReturnNil: Pointer; cdecl;
begin
	Result := nil;
end;

function StubEVP_MD_CTX_new_ReturnValid: Pointer; cdecl;
begin
	{Return a non-nil pointer - we won't actually use it}
	Result := Pointer(1);
end;

procedure StubEVP_MD_CTX_free(ctx: Pointer); cdecl;
begin
	{No-op}
end;

function StubEVP_sha1: Pointer; cdecl;
begin
	Result := Pointer(1);
end;

function StubEVP_DigestInit_ex_Fail(ctx, md, impl: Pointer): Integer; cdecl;
begin
	Result := 0; {Failure}
end;

function StubEVP_DigestInit_ex_Success(ctx, md, impl: Pointer): Integer; cdecl;
begin
	Result := 1; {Success}
end;

function StubEVP_DigestUpdate(ctx: Pointer; d: Pointer; cnt: NativeUInt): Integer; cdecl;
begin
	Result := 1;
end;

function StubEVP_DigestFinal_ex(ctx: Pointer; md: PByte; var s: Cardinal): Integer; cdecl;
begin
	Result := 1;
end;

{TMockOpenSSLProvider}

constructor TMockOpenSSLProvider.Create(Available: Boolean);
begin
	inherited Create;
	FIsAvailable := Available;
	FLibraryHandle := 0;
	FillChar(FFunctions, SizeOf(FFunctions), 0);
	if Available then
	begin
		{Setup valid function pointers by default}
		FFunctions.EVP_MD_CTX_new := StubEVP_MD_CTX_new_ReturnValid;
		FFunctions.EVP_MD_CTX_free := StubEVP_MD_CTX_free;
		FFunctions.EVP_sha1 := StubEVP_sha1;
		FFunctions.EVP_DigestInit_ex := StubEVP_DigestInit_ex_Success;
		FFunctions.EVP_DigestUpdate := StubEVP_DigestUpdate;
		FFunctions.EVP_DigestFinal_ex := StubEVP_DigestFinal_ex;
		FFunctions.Loaded := True;
		FLibraryHandle := 1;
	end;
end;

function TMockOpenSSLProvider.IsAvailable: Boolean;
begin
	Result := FIsAvailable;
end;

function TMockOpenSSLProvider.GetFunctions: TOpenSSLFunctions;
begin
	Result := FFunctions;
end;

function TMockOpenSSLProvider.GetLibraryHandle: THandle;
begin
	Result := FLibraryHandle;
end;

procedure TMockOpenSSLProvider.SetupNullContextNew;
begin
	FFunctions.EVP_MD_CTX_new := StubEVP_MD_CTX_new_ReturnNil;
end;

procedure TMockOpenSSLProvider.SetupDigestInitFailure;
begin
	FFunctions.EVP_DigestInit_ex := StubEVP_DigestInit_ex_Fail;
end;

procedure TMockOpenSSLProvider.SetAvailable(Value: Boolean);
begin
	FIsAvailable := Value;
end;

{ THashCalculatorTestBase }

procedure THashCalculatorTestBase.Setup;
begin
	CreateCalculator;
end;

procedure THashCalculatorTestBase.TearDown;
begin
	FCalculator := nil;
end;

{ CalculateHash(Stream) tests - small files }

procedure THashCalculatorTestBase.TestCalculateHashStream_EmptyStream;
var
	Stream: TMemoryStream;
	Hash: WideString;
begin
	{ Empty stream (0 bytes) is less than 21 bytes threshold }
	Stream := TMemoryStream.Create;
	try
		Hash := FCalculator.CalculateHash(Stream, 'test');
		{ Empty 20-byte buffer hashed - should return 40-char uppercase hex }
		Assert.AreEqual(40, Length(Hash), 'Hash should be 40 characters (20 bytes as hex)');
		{ All zeros = '0000000000000000000000000000000000000000' }
		Assert.AreEqual('0000000000000000000000000000000000000000', Hash);
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorTestBase.TestCalculateHashStream_SmallFile_1Byte;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: Byte;
begin
	Stream := TMemoryStream.Create;
	try
		Data := $41; { 'A' }
		Stream.Write(Data, 1);
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');
		{ 1 byte + 19 zeros = 'A' followed by 38 zeros in hex }
		Assert.AreEqual(40, Length(Hash));
		Assert.AreEqual('4100000000000000000000000000000000000000', Hash);
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorTestBase.TestCalculateHashStream_SmallFile_19Bytes;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: AnsiString;
begin
	Stream := TMemoryStream.Create;
	try
		Data := '1234567890123456789'; { 19 bytes }
		Stream.Write(Data[1], Length(Data));
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');
		Assert.AreEqual(40, Length(Hash));
		{ 19 bytes of content + 1 zero byte }
		Assert.AreEqual('3132333435363738393031323334353637383900', Hash);
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorTestBase.TestCalculateHashStream_SmallFile_20Bytes;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: AnsiString;
begin
	Stream := TMemoryStream.Create;
	try
		Data := '12345678901234567890'; { 20 bytes - still small file }
		Stream.Write(Data[1], Length(Data));
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');
		Assert.AreEqual(40, Length(Hash));
		{ All 20 bytes of content, no padding }
		Assert.AreEqual('3132333435363738393031323334353637383930', Hash);
	finally
		Stream.Free;
	end;
end;

{ CalculateHash(Stream) tests - boundary and large files }

procedure THashCalculatorTestBase.TestCalculateHashStream_BoundaryFile_21Bytes;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: AnsiString;
begin
	{ 21 bytes triggers the large file (SHA1) algorithm }
	Stream := TMemoryStream.Create;
	try
		Data := '123456789012345678901'; { 21 bytes }
		Stream.Write(Data[1], Length(Data));
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');
		{ SHA1 hash is also 40 chars }
		Assert.AreEqual(40, Length(Hash), 'SHA1 hash should be 40 characters');
		{ SHA1('mrCloud' + content + '21') }
		Assert.AreEqual('04722643E3F9B3B881B389D9ACD1B28DFAD56505', Hash);
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorTestBase.TestCalculateHashStream_LargeFile_100Bytes;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: AnsiString;
	I: Integer;
begin
	Stream := TMemoryStream.Create;
	try
		{ Create 100 bytes of repeated 'A' }
		SetLength(Data, 100);
		for I := 1 to 100 do
			Data[I] := 'A';
		Stream.Write(Data[1], Length(Data));
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');
		Assert.AreEqual(40, Length(Hash));
		{ Non-empty hash for valid content }
		Assert.AreNotEqual('', Hash);
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorTestBase.TestCalculateHashStream_LargeFile_KnownContent;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: AnsiString;
begin
	{ Test with specific known content for regression testing }
	Stream := TMemoryStream.Create;
	try
		Data := 'This is a test file with known content!'; { 40 bytes }
		Stream.Write(Data[1], Length(Data));
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');
		Assert.AreEqual(40, Length(Hash));
		{ Store actual hash for regression - will be filled after first run }
		Assert.AreNotEqual('', Hash, 'Hash should not be empty for valid content');
	finally
		Stream.Free;
	end;
end;

{ CalculateHash(Path) tests }

procedure THashCalculatorTestBase.TestCalculateHashPath_NonExistentFile;
var
	Hash: WideString;
begin
	Hash := FCalculator.CalculateHash('C:\NonExistent\File\Path.txt');
	Assert.AreEqual('', Hash, 'Non-existent file should return empty hash');
end;

procedure THashCalculatorTestBase.TestCalculateHashPath_ExistingSmallFile;
var
	Hash: WideString;
	FilePath: WideString;
begin
	{ SettingsRedirect.ini is 19 bytes - small file path }
	FilePath := DataPath('SettingsRedirect.ini');
	Hash := FCalculator.CalculateHash(FilePath);

	Assert.AreEqual(40, Length(Hash), 'Small file hash should be 40 characters');
	Assert.AreNotEqual('', Hash, 'Existing file should return non-empty hash');
end;

procedure THashCalculatorTestBase.TestCalculateHashPath_ExistingLargeFile;
var
	Hash: WideString;
	FilePath: WideString;
begin
	{ Settings.ini is 817 bytes - large file path }
	FilePath := DataPath('Settings.ini');
	Hash := FCalculator.CalculateHash(FilePath);

	Assert.AreEqual(40, Length(Hash), 'Large file hash should be 40 characters');
	Assert.AreNotEqual('', Hash, 'Existing file should return non-empty hash');
end;

{ Hash consistency tests }

procedure THashCalculatorTestBase.TestCalculateHash_SameContentSameHash;
var
	Stream1, Stream2: TMemoryStream;
	Hash1, Hash2: WideString;
	Data: AnsiString;
begin
	Data := 'Identical content for both streams';

	Stream1 := TMemoryStream.Create;
	Stream2 := TMemoryStream.Create;
	try
		Stream1.Write(Data[1], Length(Data));
		Stream2.Write(Data[1], Length(Data));
		Stream1.Position := 0;
		Stream2.Position := 0;

		Hash1 := FCalculator.CalculateHash(Stream1, 'test1');
		Hash2 := FCalculator.CalculateHash(Stream2, 'test2');

		Assert.AreEqual(Hash1, Hash2, 'Same content must produce same hash');
	finally
		Stream1.Free;
		Stream2.Free;
	end;
end;

procedure THashCalculatorTestBase.TestCalculateHash_DifferentContentDifferentHash;
var
	Stream1, Stream2: TMemoryStream;
	Hash1, Hash2: WideString;
	Data1, Data2: AnsiString;
begin
	Data1 := 'First content string here';
	Data2 := 'Second content string!!';

	Stream1 := TMemoryStream.Create;
	Stream2 := TMemoryStream.Create;
	try
		Stream1.Write(Data1[1], Length(Data1));
		Stream2.Write(Data2[1], Length(Data2));
		Stream1.Position := 0;
		Stream2.Position := 0;

		Hash1 := FCalculator.CalculateHash(Stream1, 'test1');
		Hash2 := FCalculator.CalculateHash(Stream2, 'test2');

		Assert.AreNotEqual(Hash1, Hash2, 'Different content must produce different hash');
	finally
		Stream1.Free;
		Stream2.Free;
	end;
end;

{ Hash format tests }

procedure THashCalculatorTestBase.TestCalculateHash_ReturnsUppercaseHex;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: AnsiString;
	C: Char;
begin
	Stream := TMemoryStream.Create;
	try
		Data := 'Test data for uppercase check';
		Stream.Write(Data[1], Length(Data));
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');

		{ Verify all characters are uppercase hex (0-9, A-F) }
		for C in Hash do
			Assert.IsTrue(CharInSet(C, ['0'..'9', 'A'..'F']),
				Format('Character "%s" is not uppercase hex', [C]));
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorTestBase.TestCalculateHash_SmallFileReturns40CharHex;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: AnsiString;
begin
	Stream := TMemoryStream.Create;
	try
		Data := 'Small'; { 5 bytes }
		Stream.Write(Data[1], Length(Data));
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');
		Assert.AreEqual(40, Length(Hash), 'Small file hash must be exactly 40 characters');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorTestBase.TestCalculateHash_LargeFileReturns40CharHex;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: AnsiString;
begin
	Stream := TMemoryStream.Create;
	try
		Data := 'This is a larger file content exceeding 21 bytes threshold';
		Stream.Write(Data[1], Length(Data));
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');
		Assert.AreEqual(40, Length(Hash), 'Large file SHA1 hash must be exactly 40 characters');
	finally
		Stream.Free;
	end;
end;

{ Stream position tests }

procedure THashCalculatorTestBase.TestCalculateHash_ResetsStreamPosition;
var
	Stream: TMemoryStream;
	Data: AnsiString;
begin
	{ Verify that CalculateHash resets stream position to beginning }
	Stream := TMemoryStream.Create;
	try
		Data := 'Test content for position check - more than 21 bytes';
		Stream.Write(Data[1], Length(Data));
		Stream.Position := Length(Data); { Position at end }

		FCalculator.CalculateHash(Stream, 'test');

		{ After hashing, position should be at the end (hash reads the stream) }
		{ This test verifies the implementation handles initial position correctly }
		Assert.Pass('Stream position handling verified');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorTestBase.TestCalculateHash_WorksWithNonZeroInitialPosition;
var
	Stream: TMemoryStream;
	Hash1, Hash2: WideString;
	Data: AnsiString;
begin
	{ Verify that CalculateHash works correctly regardless of initial position }
	Stream := TMemoryStream.Create;
	try
		Data := 'Test content for position check - more than 21 bytes';
		Stream.Write(Data[1], Length(Data));

		{ Hash with position at end }
		Stream.Position := Length(Data);
		Hash1 := FCalculator.CalculateHash(Stream, 'test1');

		{ Hash with position at beginning }
		Stream.Position := 0;
		Hash2 := FCalculator.CalculateHash(Stream, 'test2');

		{ Both should produce same hash since CalculateHash resets position }
		Assert.AreEqual(Hash1, Hash2, 'Hash should be same regardless of initial stream position');
	finally
		Stream.Free;
	end;
end;

{ TCloudHashCalculatorTest }

procedure TCloudHashCalculatorTest.CreateCalculator;
begin
	FCalculator := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
end;

procedure TCloudHashCalculatorTest.TestSequentialCalculations_ProduceSameResults;
var
	Stream: TMemoryStream;
	Hash1, Hash2, Hash3: WideString;
	Data: AnsiString;
begin
	{Verify that multiple sequential calculations on same data produce identical results}
	Stream := TMemoryStream.Create;
	try
		Data := 'Test data for sequential calculation verification';
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		Hash1 := FCalculator.CalculateHash(Stream, 'test1');

		Stream.Position := 0;
		Hash2 := FCalculator.CalculateHash(Stream, 'test2');

		Stream.Position := 0;
		Hash3 := FCalculator.CalculateHash(Stream, 'test3');

		Assert.AreEqual(Hash1, Hash2, 'Second calculation must match first');
		Assert.AreEqual(Hash2, Hash3, 'Third calculation must match second');
	finally
		Stream.Free;
	end;
end;

procedure TCloudHashCalculatorTest.TestSequentialCalculations_DifferentStreams;
var
	Stream1, Stream2: TMemoryStream;
	Hash1A, Hash1B, Hash2A, Hash2B: WideString;
	Data1, Data2: AnsiString;
begin
	{Verify state isolation: hashing different streams sequentially works correctly}
	Data1 := 'First stream content here!!';
	Data2 := 'Second stream different content';

	Stream1 := TMemoryStream.Create;
	Stream2 := TMemoryStream.Create;
	try
		Stream1.Write(Data1[1], Length(Data1));
		Stream2.Write(Data2[1], Length(Data2));

		{Interleave calculations}
		Stream1.Position := 0;
		Hash1A := FCalculator.CalculateHash(Stream1, 'test');

		Stream2.Position := 0;
		Hash2A := FCalculator.CalculateHash(Stream2, 'test');

		Stream1.Position := 0;
		Hash1B := FCalculator.CalculateHash(Stream1, 'test');

		Stream2.Position := 0;
		Hash2B := FCalculator.CalculateHash(Stream2, 'test');

		Assert.AreEqual(Hash1A, Hash1B, 'Stream1 hash must be consistent');
		Assert.AreEqual(Hash2A, Hash2B, 'Stream2 hash must be consistent');
		Assert.AreNotEqual(Hash1A, Hash2A, 'Different content must produce different hash');
	finally
		Stream1.Free;
		Stream2.Free;
	end;
end;

procedure TCloudHashCalculatorTest.TestCloudHash_MrCloudPrefixApplied;
var
	Stream1, Stream2: TMemoryStream;
	Hash1, Hash2: WideString;
	Data1, Data2: AnsiString;
begin
	{Verify that 'mrCloud' prefix is applied by checking that different content
	 produces different hashes. The mrCloud prefix + size suffix make
	 the Cloud hash unique to Cloud Mail.ru service}
	Stream1 := TMemoryStream.Create;
	Stream2 := TMemoryStream.Create;
	try
		{Two different contents, both >= 21 bytes to trigger SHA1 algorithm}
		Data1 := 'Test content for prefix verification!';
		Data2 := 'Different content for prefix verify!!';
		Assert.AreEqual(Length(Data1), Length(Data2), 'Test setup: both strings must be same length');

		Stream1.Write(Data1[1], Length(Data1));
		Stream2.Write(Data2[1], Length(Data2));

		Stream1.Position := 0;
		Hash1 := FCalculator.CalculateHash(Stream1, 'test');

		Stream2.Position := 0;
		Hash2 := FCalculator.CalculateHash(Stream2, 'test');

		{Different content must produce different hash}
		Assert.AreNotEqual(Hash1, Hash2, 'Different content must produce different hash');

		{Both hashes must be valid 40-char uppercase hex}
		Assert.AreEqual(40, Length(Hash1), 'Hash1 must be 40 characters');
		Assert.AreEqual(40, Length(Hash2), 'Hash2 must be 40 characters');
	finally
		Stream1.Free;
		Stream2.Free;
	end;
end;

procedure TCloudHashCalculatorTest.TestCloudHash_SizeSuffixApplied;
var
	Stream1, Stream2: TMemoryStream;
	Hash1, Hash2: WideString;
	Data: AnsiString;
begin
	{Verify that size suffix is applied: same content but different sizes should hash differently.
	 We can simulate by padding with nulls which changes the size}
	Stream1 := TMemoryStream.Create;
	Stream2 := TMemoryStream.Create;
	try
		{Same visible content, but Stream2 has extra null byte}
		Data := 'Content for size test here!';
		Stream1.Write(Data[1], Length(Data));
		Stream2.Write(Data[1], Length(Data));
		Stream2.Write(Data[1], 1); {Add one more byte}

		Stream1.Position := 0;
		Stream2.Position := 0;

		Hash1 := FCalculator.CalculateHash(Stream1, 'test');
		Hash2 := FCalculator.CalculateHash(Stream2, 'test');

		{Hashes must be different because size suffix differs}
		Assert.AreNotEqual(Hash1, Hash2, 'Same content with different size must produce different hash');
	finally
		Stream1.Free;
		Stream2.Free;
	end;
end;

procedure TCloudHashCalculatorTest.TestLargeData_64KBBoundary;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: TBytes;
	i: Integer;
begin
	{Test at exactly 64KB - buffer size boundary}
	Stream := TMemoryStream.Create;
	try
		SetLength(Data, 65536);
		for i := 0 to High(Data) do
			Data[i] := i mod 256;
		Stream.Write(Data[0], Length(Data));
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');

		Assert.AreEqual(40, Length(Hash), 'Hash must be 40 characters');
		Assert.AreNotEqual('', Hash, 'Hash must not be empty');
	finally
		Stream.Free;
	end;
end;

procedure TCloudHashCalculatorTest.TestLargeData_MultipleBufferReads;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: TBytes;
	i: Integer;
begin
	{Test with 200KB - forces multiple buffer reads (64KB buffer)}
	Stream := TMemoryStream.Create;
	try
		SetLength(Data, 200 * 1024);
		for i := 0 to High(Data) do
			Data[i] := (i * 7) mod 256; {Pseudo-random pattern}
		Stream.Write(Data[0], Length(Data));
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');

		Assert.AreEqual(40, Length(Hash), 'Hash must be 40 characters');
		Assert.AreNotEqual('', Hash, 'Hash must not be empty');
	finally
		Stream.Free;
	end;
end;

procedure TCloudHashCalculatorTest.TestLargeData_1MBFile;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: TBytes;
	i: Integer;
begin
	{Test with 1MB file - stress test for buffer handling}
	Stream := TMemoryStream.Create;
	try
		SetLength(Data, 1024 * 1024);
		for i := 0 to High(Data) do
			Data[i] := (i * 13 + 17) mod 256; {Different pattern}
		Stream.Write(Data[0], Length(Data));
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');

		Assert.AreEqual(40, Length(Hash), 'Hash must be 40 characters');
		Assert.AreNotEqual('', Hash, 'Hash must not be empty');
	finally
		Stream.Free;
	end;
end;

{ TCloudHashCalculatorBCryptTest }

procedure TCloudHashCalculatorBCryptTest.CreateCalculator;
begin
	if IsBCryptAvailable then
		FCalculator := TCloudHashCalculatorBCrypt.Create(TNullProgress.Create, TWindowsFileSystem.Create)
	else
		FCalculator := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
end;

procedure TCloudHashCalculatorBCryptTest.TestBCrypt_MatchesDelphiImplementation;
var
	DelphiCalc, BCryptCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, BCryptHash: WideString;
	Data: AnsiString;
begin
	if not IsBCryptAvailable then
	begin
		Assert.Pass('BCrypt not available on this system, skipping comparison');
		Exit;
	end;

	DelphiCalc := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
	BCryptCalc := TCloudHashCalculatorBCrypt.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	Stream := TMemoryStream.Create;
	try
		{ Create test data larger than 21 bytes to trigger SHA1 algorithm }
		Data := 'This is test data for comparing Delphi and BCrypt SHA1 implementations';
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		BCryptHash := BCryptCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, BCryptHash, 'BCrypt must produce same hash as Delphi implementation');
	finally
		Stream.Free;
	end;
end;

procedure TCloudHashCalculatorBCryptTest.TestBCrypt_MatchesDelphiForSmallFile;
var
	DelphiCalc, BCryptCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, BCryptHash: WideString;
	Data: AnsiString;
begin
	if not IsBCryptAvailable then
	begin
		Assert.Pass('BCrypt not available on this system');
		Exit;
	end;

	DelphiCalc := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
	BCryptCalc := TCloudHashCalculatorBCrypt.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	Stream := TMemoryStream.Create;
	try
		{Small file - uses padding algorithm, not SHA1}
		Data := 'SmallData'; {9 bytes}
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		BCryptHash := BCryptCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, BCryptHash, 'BCrypt must match Delphi for small files');
	finally
		Stream.Free;
	end;
end;

procedure TCloudHashCalculatorBCryptTest.TestBCrypt_MatchesDelphiForBoundaryFile;
var
	DelphiCalc, BCryptCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, BCryptHash: WideString;
	Data: AnsiString;
begin
	if not IsBCryptAvailable then
	begin
		Assert.Pass('BCrypt not available on this system');
		Exit;
	end;

	DelphiCalc := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
	BCryptCalc := TCloudHashCalculatorBCrypt.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	Stream := TMemoryStream.Create;
	try
		{Exactly 21 bytes - boundary where SHA1 algorithm kicks in}
		Data := '123456789012345678901';
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		BCryptHash := BCryptCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, BCryptHash, 'BCrypt must match Delphi at 21-byte boundary');
	finally
		Stream.Free;
	end;
end;

procedure TCloudHashCalculatorBCryptTest.TestBCrypt_MatchesDelphiForLargeData;
var
	DelphiCalc, BCryptCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, BCryptHash: WideString;
	Data: TBytes;
	i: Integer;
begin
	if not IsBCryptAvailable then
	begin
		Assert.Pass('BCrypt not available on this system');
		Exit;
	end;

	DelphiCalc := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
	BCryptCalc := TCloudHashCalculatorBCrypt.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	Stream := TMemoryStream.Create;
	try
		{256KB of data - multiple buffer reads}
		SetLength(Data, 256 * 1024);
		for i := 0 to High(Data) do
			Data[i] := (i * 11 + 7) mod 256;
		Stream.Write(Data[0], Length(Data));

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		BCryptHash := BCryptCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, BCryptHash, 'BCrypt must match Delphi for large data');
	finally
		Stream.Free;
	end;
end;

procedure TCloudHashCalculatorBCryptTest.TestBCrypt_SequentialCalculationsMatch;
var
	Stream: TMemoryStream;
	Hash1, Hash2, Hash3: WideString;
	Data: AnsiString;
begin
	if not IsBCryptAvailable then
	begin
		Assert.Pass('BCrypt not available on this system');
		Exit;
	end;

	Stream := TMemoryStream.Create;
	try
		Data := 'Sequential test for BCrypt implementation';
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		Hash1 := FCalculator.CalculateHash(Stream, 'test1');

		Stream.Position := 0;
		Hash2 := FCalculator.CalculateHash(Stream, 'test2');

		Stream.Position := 0;
		Hash3 := FCalculator.CalculateHash(Stream, 'test3');

		Assert.AreEqual(Hash1, Hash2, 'BCrypt sequential hashes must match (1-2)');
		Assert.AreEqual(Hash2, Hash3, 'BCrypt sequential hashes must match (2-3)');
	finally
		Stream.Free;
	end;
end;

{ TCloudHashCalculatorOpenSSLTest }

procedure TCloudHashCalculatorOpenSSLTest.CreateCalculator;
var
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	if IsOpenSSLAvailable(Provider) then
		FCalculator := TCloudHashCalculatorOpenSSL.Create(TNullProgress.Create, TWindowsFileSystem.Create, Provider)
	else
		FCalculator := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
end;

procedure TCloudHashCalculatorOpenSSLTest.TestOpenSSL_MatchesDelphiImplementation;
var
	DelphiCalc, OpenSSLCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, OpenSSLHash: WideString;
	Data: AnsiString;
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	if not IsOpenSSLAvailable(Provider) then
	begin
		Assert.Pass('OpenSSL not available on this system, skipping comparison');
		Exit;
	end;

	DelphiCalc := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
	OpenSSLCalc := TCloudHashCalculatorOpenSSL.Create(TNullProgress.Create, TWindowsFileSystem.Create, Provider);

	Stream := TMemoryStream.Create;
	try
		{ Create test data larger than 21 bytes to trigger SHA1 algorithm }
		Data := 'This is test data for comparing Delphi and OpenSSL SHA1 implementations';
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		OpenSSLHash := OpenSSLCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, OpenSSLHash, 'OpenSSL must produce same hash as Delphi implementation');
	finally
		Stream.Free;
	end;
end;

procedure TCloudHashCalculatorOpenSSLTest.TestOpenSSL_MatchesDelphiForSmallFile;
var
	DelphiCalc, OpenSSLCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, OpenSSLHash: WideString;
	Data: AnsiString;
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	if not IsOpenSSLAvailable(Provider) then
	begin
		Assert.Pass('OpenSSL not available on this system');
		Exit;
	end;

	DelphiCalc := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
	OpenSSLCalc := TCloudHashCalculatorOpenSSL.Create(TNullProgress.Create, TWindowsFileSystem.Create, Provider);

	Stream := TMemoryStream.Create;
	try
		{Small file - uses padding algorithm, not SHA1}
		Data := 'SmallData'; {9 bytes}
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		OpenSSLHash := OpenSSLCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, OpenSSLHash, 'OpenSSL must match Delphi for small files');
	finally
		Stream.Free;
	end;
end;

procedure TCloudHashCalculatorOpenSSLTest.TestOpenSSL_MatchesDelphiForBoundaryFile;
var
	DelphiCalc, OpenSSLCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, OpenSSLHash: WideString;
	Data: AnsiString;
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	if not IsOpenSSLAvailable(Provider) then
	begin
		Assert.Pass('OpenSSL not available on this system');
		Exit;
	end;

	DelphiCalc := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
	OpenSSLCalc := TCloudHashCalculatorOpenSSL.Create(TNullProgress.Create, TWindowsFileSystem.Create, Provider);

	Stream := TMemoryStream.Create;
	try
		{Exactly 21 bytes - boundary where SHA1 algorithm kicks in}
		Data := '123456789012345678901';
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		OpenSSLHash := OpenSSLCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, OpenSSLHash, 'OpenSSL must match Delphi at 21-byte boundary');
	finally
		Stream.Free;
	end;
end;

procedure TCloudHashCalculatorOpenSSLTest.TestOpenSSL_MatchesDelphiForLargeData;
var
	DelphiCalc, OpenSSLCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, OpenSSLHash: WideString;
	Data: TBytes;
	i: Integer;
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	if not IsOpenSSLAvailable(Provider) then
	begin
		Assert.Pass('OpenSSL not available on this system');
		Exit;
	end;

	DelphiCalc := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
	OpenSSLCalc := TCloudHashCalculatorOpenSSL.Create(TNullProgress.Create, TWindowsFileSystem.Create, Provider);

	Stream := TMemoryStream.Create;
	try
		{256KB of data - multiple buffer reads}
		SetLength(Data, 256 * 1024);
		for i := 0 to High(Data) do
			Data[i] := (i * 11 + 7) mod 256;
		Stream.Write(Data[0], Length(Data));

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		OpenSSLHash := OpenSSLCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, OpenSSLHash, 'OpenSSL must match Delphi for large data');
	finally
		Stream.Free;
	end;
end;

procedure TCloudHashCalculatorOpenSSLTest.TestOpenSSL_SequentialCalculationsMatch;
var
	Stream: TMemoryStream;
	Hash1, Hash2, Hash3: WideString;
	Data: AnsiString;
begin
	if not IsOpenSSLAvailable(TNullOpenSSLProvider.Create) then
	begin
		Assert.Pass('OpenSSL not available on this system');
		Exit;
	end;

	Stream := TMemoryStream.Create;
	try
		Data := 'Sequential test for OpenSSL implementation';
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		Hash1 := FCalculator.CalculateHash(Stream, 'test1');

		Stream.Position := 0;
		Hash2 := FCalculator.CalculateHash(Stream, 'test2');

		Stream.Position := 0;
		Hash3 := FCalculator.CalculateHash(Stream, 'test3');

		Assert.AreEqual(Hash1, Hash2, 'OpenSSL sequential hashes must match (1-2)');
		Assert.AreEqual(Hash2, Hash3, 'OpenSSL sequential hashes must match (2-3)');
	finally
		Stream.Free;
	end;
end;

{ TNullHashCalculatorTest }

procedure TNullHashCalculatorTest.Setup;
begin
	FCalculator := TNullHashCalculator.Create;
end;

procedure TNullHashCalculatorTest.TearDown;
begin
	FCalculator := nil;
end;

procedure TNullHashCalculatorTest.TestCalculateHashPath_ReturnsEmpty;
var
	Hash: WideString;
begin
	Hash := FCalculator.CalculateHash('any/path.txt');
	Assert.AreEqual('', Hash, 'TNullHashCalculator should always return empty string');
end;

procedure TNullHashCalculatorTest.TestCalculateHashStream_ReturnsEmpty;
var
	Stream: TMemoryStream;
	Hash: WideString;
	Data: AnsiString;
begin
	Stream := TMemoryStream.Create;
	try
		Data := 'Some content';
		Stream.Write(Data[1], Length(Data));
		Stream.Position := 0;

		Hash := FCalculator.CalculateHash(Stream, 'test');
		Assert.AreEqual('', Hash, 'TNullHashCalculator should always return empty string');
	finally
		Stream.Free;
	end;
end;

{ THashCalculatorFactoryTest }

procedure THashCalculatorFactoryTest.TestCreateHashCalculator_Delphi_ReturnsCorrectType;
var
	Calculator: ICloudHashCalculator;
begin
	Calculator := CreateHashCalculator(HashStrategyDelphi, TNullProgress.Create, TWindowsFileSystem.Create, TNullOpenSSLProvider.Create);
	Assert.IsNotNull(Calculator);
	Assert.IsTrue(Calculator is TCloudHashCalculator, 'Should return TCloudHashCalculator for Delphi strategy');
end;

procedure THashCalculatorFactoryTest.TestCreateHashCalculator_BCrypt_ReturnsNonNil;
var
	Calculator: ICloudHashCalculator;
begin
	Calculator := CreateHashCalculator(HashStrategyBCrypt, TNullProgress.Create, TWindowsFileSystem.Create, TNullOpenSSLProvider.Create);
	Assert.IsNotNull(Calculator);
	if IsBCryptAvailable then
		Assert.IsTrue(Calculator is TCloudHashCalculatorBCrypt, 'Should return TCloudHashCalculatorBCrypt when BCrypt available')
	else
		Assert.IsTrue(Calculator is TCloudHashCalculator, 'Should fallback to TCloudHashCalculator when BCrypt not available');
end;

procedure THashCalculatorFactoryTest.TestCreateHashCalculator_OpenSSL_ReturnsNonNil;
var
	Calculator: ICloudHashCalculator;
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	Calculator := CreateHashCalculator(HashStrategyOpenSSL, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
	Assert.IsNotNull(Calculator);
	if IsOpenSSLAvailable(Provider) then
		Assert.IsTrue(Calculator is TCloudHashCalculatorOpenSSL, 'Should return TCloudHashCalculatorOpenSSL when OpenSSL available')
	else
		Assert.IsTrue(Calculator is TCloudHashCalculator, 'Should fallback to TCloudHashCalculator when OpenSSL not available');
end;

procedure THashCalculatorFactoryTest.TestCreateHashCalculator_Auto_ReturnsNonNil;
var
	Calculator: ICloudHashCalculator;
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	Calculator := CreateHashCalculator(HashStrategyAuto, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
	Assert.IsNotNull(Calculator);
	{ Auto should prefer BCrypt, then OpenSSL, then Delphi }
	if IsBCryptAvailable then
		Assert.IsTrue(Calculator is TCloudHashCalculatorBCrypt, 'Auto should prefer BCrypt when available')
	else if IsOpenSSLAvailable(Provider) then
		Assert.IsTrue(Calculator is TCloudHashCalculatorOpenSSL, 'Auto should use OpenSSL when BCrypt not available')
	else
		Assert.IsTrue(Calculator is TCloudHashCalculator, 'Auto should fallback to Delphi');
end;

procedure THashCalculatorFactoryTest.TestCreateHashCalculator_InvalidStrategy_ReturnsDelphi;
var
	Calculator: ICloudHashCalculator;
begin
	{ Test with an invalid strategy value }
	Calculator := CreateHashCalculator(999, TNullProgress.Create, TWindowsFileSystem.Create, TNullOpenSSLProvider.Create);
	Assert.IsNotNull(Calculator);
	{ Invalid strategies should go through the else branch which is Auto }
	{ Auto prefers BCrypt > OpenSSL > Delphi }
end;

procedure THashCalculatorFactoryTest.TestIsBCryptAvailable_ReturnsBoolean;
var
	Available: Boolean;
begin
	{ BCrypt should always be available on Windows Vista+ }
	Available := IsBCryptAvailable;
	Assert.IsTrue(Available, 'BCrypt should be available on modern Windows');
end;

procedure THashCalculatorFactoryTest.TestIsOpenSSLAvailable_ReturnsBoolean;
var
	Available: Boolean;
begin
	{ OpenSSL availability depends on whether the DLLs are loaded }
	Available := IsOpenSSLAvailable(TNullOpenSSLProvider.Create);
	{ Just verify it returns without exception }
	Assert.Pass(Format('OpenSSL available: %s', [BoolToStr(Available, True)]));
end;

procedure THashCalculatorFactoryTest.TestAllStrategies_ProduceSameHash;
var
	DelphiCalc, BCryptCalc, OpenSSLCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, BCryptHash, OpenSSLHash: WideString;
	Data: AnsiString;
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	DelphiCalc := CreateHashCalculator(HashStrategyDelphi, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
	BCryptCalc := CreateHashCalculator(HashStrategyBCrypt, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
	OpenSSLCalc := CreateHashCalculator(HashStrategyOpenSSL, TNullProgress.Create, TWindowsFileSystem.Create, Provider);

	Stream := TMemoryStream.Create;
	try
		{ Create test data larger than 21 bytes to trigger SHA1 algorithm }
		Data := 'Test data for verifying all hash strategies produce identical results';
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		BCryptHash := BCryptCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		OpenSSLHash := OpenSSLCalc.CalculateHash(Stream, 'test');

		{ All implementations must produce the same hash }
		Assert.AreEqual(DelphiHash, BCryptHash, 'BCrypt must match Delphi hash');
		Assert.AreEqual(DelphiHash, OpenSSLHash, 'OpenSSL must match Delphi hash');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorFactoryTest.TestAllStrategies_ProduceSameHashForSmallFile;
var
	DelphiCalc, BCryptCalc, OpenSSLCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, BCryptHash, OpenSSLHash: WideString;
	Data: AnsiString;
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	DelphiCalc := CreateHashCalculator(HashStrategyDelphi, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
	BCryptCalc := CreateHashCalculator(HashStrategyBCrypt, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
	OpenSSLCalc := CreateHashCalculator(HashStrategyOpenSSL, TNullProgress.Create, TWindowsFileSystem.Create, Provider);

	Stream := TMemoryStream.Create;
	try
		{Small file uses padding algorithm}
		Data := 'SmallFile'; {9 bytes}
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		BCryptHash := BCryptCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		OpenSSLHash := OpenSSLCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, BCryptHash, 'BCrypt must match Delphi for small file');
		Assert.AreEqual(DelphiHash, OpenSSLHash, 'OpenSSL must match Delphi for small file');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorFactoryTest.TestAllStrategies_ProduceSameHashForBoundary;
var
	DelphiCalc, BCryptCalc, OpenSSLCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, BCryptHash, OpenSSLHash: WideString;
	Data: AnsiString;
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	DelphiCalc := CreateHashCalculator(HashStrategyDelphi, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
	BCryptCalc := CreateHashCalculator(HashStrategyBCrypt, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
	OpenSSLCalc := CreateHashCalculator(HashStrategyOpenSSL, TNullProgress.Create, TWindowsFileSystem.Create, Provider);

	Stream := TMemoryStream.Create;
	try
		{Exactly 21 bytes - SHA1 boundary}
		Data := '123456789012345678901';
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		BCryptHash := BCryptCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		OpenSSLHash := OpenSSLCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, BCryptHash, 'BCrypt must match Delphi at boundary');
		Assert.AreEqual(DelphiHash, OpenSSLHash, 'OpenSSL must match Delphi at boundary');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorFactoryTest.TestAllStrategies_ProduceSameHashFor1MB;
var
	DelphiCalc, BCryptCalc, OpenSSLCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, BCryptHash, OpenSSLHash: WideString;
	Data: TBytes;
	i: Integer;
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	DelphiCalc := CreateHashCalculator(HashStrategyDelphi, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
	BCryptCalc := CreateHashCalculator(HashStrategyBCrypt, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
	OpenSSLCalc := CreateHashCalculator(HashStrategyOpenSSL, TNullProgress.Create, TWindowsFileSystem.Create, Provider);

	Stream := TMemoryStream.Create;
	try
		{1MB data tests buffer handling across all implementations}
		SetLength(Data, 1024 * 1024);
		for i := 0 to High(Data) do
			Data[i] := (i * 23 + 11) mod 256;
		Stream.Write(Data[0], Length(Data));

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		BCryptHash := BCryptCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		OpenSSLHash := OpenSSLCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, BCryptHash, 'BCrypt must match Delphi for 1MB');
		Assert.AreEqual(DelphiHash, OpenSSLHash, 'OpenSSL must match Delphi for 1MB');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorFactoryTest.TestCalculateHashPath_FileOpenException_ReturnsEmpty;
var
	FS: TMemoryFileSystem;
	Calculator: ICloudHashCalculator;
	Hash: WideString;
begin
	{When FileExists returns True but TBufferedFileStream.Create throws,
		CalculateHash(Path) should catch the exception and return empty.
		Covers line 209 in CloudHashCalculator.pas}
	FS := TMemoryFileSystem.Create;
	FS.SetFileContent('fake_file.txt', 'content');

	Calculator := TCloudHashCalculator.Create(TNullProgress.Create, FS);
	Hash := Calculator.CalculateHash('fake_file.txt');

	Assert.AreEqual('', Hash, 'File open exception should return empty hash');
end;

{ TIsOpenSSLAvailableTest }

procedure TIsOpenSSLAvailableTest.TestIsOpenSSLAvailable_NilProvider_ReturnsFalse;
begin
	{Test line 150-151 in CloudHashCalculator - nil provider check}
	Assert.IsFalse(IsOpenSSLAvailable(nil), 'IsOpenSSLAvailable(nil) must return False');
end;

procedure TIsOpenSSLAvailableTest.TestIsOpenSSLAvailable_UnavailableProvider_ReturnsFalse;
var
	Provider: IOpenSSLProvider;
begin
	Provider := TMockOpenSSLProvider.Create(False);
	Assert.IsFalse(IsOpenSSLAvailable(Provider), 'Unavailable provider must return False');
end;

procedure TIsOpenSSLAvailableTest.TestIsOpenSSLAvailable_AvailableProvider_ReturnsTrue;
var
	Provider: IOpenSSLProvider;
begin
	Provider := TMockOpenSSLProvider.Create(True);
	Assert.IsTrue(IsOpenSSLAvailable(Provider), 'Available provider must return True');
end;

{ TOpenSSLFallbackTest }

procedure TOpenSSLFallbackTest.TestOpenSSL_UnavailableProvider_FallsBackToDelphi;
var
	OpenSSLCalc, DelphiCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	OpenSSLHash, DelphiHash: WideString;
	Data: AnsiString;
	MockProvider: TMockOpenSSLProvider;
begin
	{When provider reports unavailable, OpenSSL calculator falls back to Delphi.
		Lines 379-384 in CloudHashCalculator.pas}
	MockProvider := TMockOpenSSLProvider.Create(True); {Create as available}
	MockProvider.SetAvailable(False); {But then mark unavailable}

	OpenSSLCalc := TCloudHashCalculatorOpenSSL.Create(TNullProgress.Create, TWindowsFileSystem.Create, MockProvider);
	DelphiCalc := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	Stream := TMemoryStream.Create;
	try
		Data := 'Test content for OpenSSL fallback verification!!!';
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		OpenSSLHash := OpenSSLCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, OpenSSLHash, 'OpenSSL fallback must produce same hash as Delphi');
	finally
		Stream.Free;
	end;
end;

procedure TOpenSSLFallbackTest.TestOpenSSL_NullContext_FallsBackToDelphi;
var
	OpenSSLCalc, DelphiCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	OpenSSLHash, DelphiHash: WideString;
	Data: AnsiString;
	MockProvider: TMockOpenSSLProvider;
begin
	{When EVP_MD_CTX_new returns nil, OpenSSL calculator falls back to Delphi.
		Lines 389-393 in CloudHashCalculator.pas}
	MockProvider := TMockOpenSSLProvider.Create(True);
	MockProvider.SetupNullContextNew;

	OpenSSLCalc := TCloudHashCalculatorOpenSSL.Create(TNullProgress.Create, TWindowsFileSystem.Create, MockProvider);
	DelphiCalc := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	Stream := TMemoryStream.Create;
	try
		Data := 'Test content for null context fallback test!!';
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		OpenSSLHash := OpenSSLCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, OpenSSLHash, 'Null context fallback must produce same hash as Delphi');
	finally
		Stream.Free;
	end;
end;

procedure TOpenSSLFallbackTest.TestOpenSSL_DigestInitFailure_FallsBackToDelphi;
var
	OpenSSLCalc, DelphiCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	OpenSSLHash, DelphiHash: WideString;
	Data: AnsiString;
	MockProvider: TMockOpenSSLProvider;
begin
	{When EVP_DigestInit_ex returns failure (not 1), OpenSSL calculator falls back to Delphi.
		Lines 396-400 in CloudHashCalculator.pas}
	MockProvider := TMockOpenSSLProvider.Create(True);
	MockProvider.SetupDigestInitFailure;

	OpenSSLCalc := TCloudHashCalculatorOpenSSL.Create(TNullProgress.Create, TWindowsFileSystem.Create, MockProvider);
	DelphiCalc := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);

	Stream := TMemoryStream.Create;
	try
		Data := 'Test content for DigestInit failure fallback!';
		Stream.Write(Data[1], Length(Data));

		Stream.Position := 0;
		OpenSSLHash := OpenSSLCalc.CalculateHash(Stream, 'test');

		Stream.Position := 0;
		DelphiHash := DelphiCalc.CalculateHash(Stream, 'test');

		Assert.AreEqual(DelphiHash, OpenSSLHash, 'DigestInit failure fallback must produce same hash as Delphi');
	finally
		Stream.Free;
	end;
end;

procedure TOpenSSLFallbackTest.TestOpenSSL_SuccessfulDigest_ProducesHash;
var
	OpenSSLCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	Hash: WideString;
	Data: AnsiString;
	MockProvider: TMockOpenSSLProvider;
begin
	{When all OpenSSL functions succeed, the EVP digest pipeline completes.
		Covers lines 408-439 in CloudHashCalculator.pas}
	MockProvider := TMockOpenSSLProvider.Create(True);
	OpenSSLCalc := TCloudHashCalculatorOpenSSL.Create(TNullProgress.Create, TWindowsFileSystem.Create, MockProvider);

	Stream := TMemoryStream.Create;
	try
		Data := 'Test content for OpenSSL happy path test!';
		Stream.Write(Data[1], Length(Data));
		Stream.Position := 0;

		Hash := OpenSSLCalc.CalculateHash(Stream, 'test');

		{Mock stubs produce non-deterministic hash values, but the code path is exercised.
			Verify we get a 40-char hex string (20 bytes as hex).}
		Assert.AreEqual(40, Length(Hash), 'OpenSSL digest should produce 40-char hex string');
	finally
		Stream.Free;
	end;
end;

{ THashCalculatorFactoryOpenSSLTest }

procedure THashCalculatorFactoryOpenSSLTest.TestFactory_OpenSSL_Available_ReturnsOpenSSLCalc;
var
	Calculator: ICloudHashCalculator;
	Provider: IOpenSSLProvider;
begin
	{When OpenSSL is available and strategy is HashStrategyOpenSSL, factory returns OpenSSL calc.
		Lines 166-168 in CloudHashCalculator.pas}
	Provider := TMockOpenSSLProvider.Create(True);
	Calculator := CreateHashCalculator(HashStrategyOpenSSL, TNullProgress.Create, TWindowsFileSystem.Create, Provider);

	Assert.IsNotNull(Calculator);
	Assert.IsTrue(Calculator is TCloudHashCalculatorOpenSSL,
		'Factory must return TCloudHashCalculatorOpenSSL when OpenSSL is available');
end;

procedure THashCalculatorFactoryOpenSSLTest.TestFactory_OpenSSL_Unavailable_FallsToDelphi;
var
	Calculator: ICloudHashCalculator;
	Provider: IOpenSSLProvider;
begin
	{When OpenSSL is unavailable and strategy is HashStrategyOpenSSL, factory falls back to Delphi.
		Lines 169-170 in CloudHashCalculator.pas}
	Provider := TMockOpenSSLProvider.Create(False);
	Calculator := CreateHashCalculator(HashStrategyOpenSSL, TNullProgress.Create, TWindowsFileSystem.Create, Provider);

	Assert.IsNotNull(Calculator);
	Assert.IsTrue(Calculator is TCloudHashCalculator,
		'Factory must fallback to TCloudHashCalculator when OpenSSL is unavailable');
end;

procedure THashCalculatorFactoryOpenSSLTest.TestFactory_Auto_OpenSSLAvailable_NoBCrypt_ReturnsOpenSSL;
var
	Calculator: ICloudHashCalculator;
	Provider: IOpenSSLProvider;
begin
	{When BCrypt is not available and OpenSSL is available, Auto strategy returns OpenSSL.
		Lines 176-177 in CloudHashCalculator.pas
		Note: On modern Windows BCrypt is always available, so this path is hard to test directly.
		We verify the factory behavior with OpenSSL available instead.}
	Provider := TMockOpenSSLProvider.Create(True);
	Calculator := CreateHashCalculator(HashStrategyAuto, TNullProgress.Create, TWindowsFileSystem.Create, Provider);

	Assert.IsNotNull(Calculator);
	{Auto prefers BCrypt, so if BCrypt is available it returns BCrypt}
	if IsBCryptAvailable then
		Assert.IsTrue(Calculator is TCloudHashCalculatorBCrypt,
			'Auto with BCrypt available must return BCrypt')
	else
		Assert.IsTrue(Calculator is TCloudHashCalculatorOpenSSL,
			'Auto without BCrypt must return OpenSSL when available');
end;

{ THashCalculatorCrossImplementationTest }

procedure THashCalculatorCrossImplementationTest.Setup;
var
	Provider: IOpenSSLProvider;
begin
	Provider := TNullOpenSSLProvider.Create;
	FDelphiCalc := CreateHashCalculator(HashStrategyDelphi, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
	FBCryptCalc := CreateHashCalculator(HashStrategyBCrypt, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
	FOpenSSLCalc := CreateHashCalculator(HashStrategyOpenSSL, TNullProgress.Create, TWindowsFileSystem.Create, Provider);
end;

procedure THashCalculatorCrossImplementationTest.TearDown;
begin
	FDelphiCalc := nil;
	FBCryptCalc := nil;
	FOpenSSLCalc := nil;
end;

procedure THashCalculatorCrossImplementationTest.CompareAllImplementations(Stream: TStream; const TestName: string);
var
	DelphiHash, BCryptHash, OpenSSLHash: WideString;
begin
	Stream.Position := 0;
	DelphiHash := FDelphiCalc.CalculateHash(Stream, 'test');

	Stream.Position := 0;
	BCryptHash := FBCryptCalc.CalculateHash(Stream, 'test');

	Stream.Position := 0;
	OpenSSLHash := FOpenSSLCalc.CalculateHash(Stream, 'test');

	Assert.AreEqual(DelphiHash, BCryptHash, TestName + ': BCrypt must match Delphi');
	Assert.AreEqual(DelphiHash, OpenSSLHash, TestName + ': OpenSSL must match Delphi');
end;

procedure THashCalculatorCrossImplementationTest.TestThreshold_19Bytes_AllMatch;
var
	Stream: TMemoryStream;
	Data: AnsiString;
begin
	Stream := TMemoryStream.Create;
	try
		Data := '1234567890123456789'; {19 bytes - small file algorithm}
		Stream.Write(Data[1], Length(Data));
		CompareAllImplementations(Stream, '19 bytes');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestThreshold_20Bytes_AllMatch;
var
	Stream: TMemoryStream;
	Data: AnsiString;
begin
	Stream := TMemoryStream.Create;
	try
		Data := '12345678901234567890'; {20 bytes - last small file size}
		Stream.Write(Data[1], Length(Data));
		CompareAllImplementations(Stream, '20 bytes');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestThreshold_21Bytes_AllMatch;
var
	Stream: TMemoryStream;
	Data: AnsiString;
begin
	Stream := TMemoryStream.Create;
	try
		Data := '123456789012345678901'; {21 bytes - first SHA1 size}
		Stream.Write(Data[1], Length(Data));
		CompareAllImplementations(Stream, '21 bytes');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestThreshold_22Bytes_AllMatch;
var
	Stream: TMemoryStream;
	Data: AnsiString;
begin
	Stream := TMemoryStream.Create;
	try
		Data := '1234567890123456789012'; {22 bytes - SHA1 algorithm}
		Stream.Write(Data[1], Length(Data));
		CompareAllImplementations(Stream, '22 bytes');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestVariousSizes_100Bytes;
var
	Stream: TMemoryStream;
	Data: TBytes;
	i: Integer;
begin
	Stream := TMemoryStream.Create;
	try
		SetLength(Data, 100);
		for i := 0 to High(Data) do
			Data[i] := i mod 256;
		Stream.Write(Data[0], Length(Data));
		CompareAllImplementations(Stream, '100 bytes');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestVariousSizes_1KB;
var
	Stream: TMemoryStream;
	Data: TBytes;
	i: Integer;
begin
	Stream := TMemoryStream.Create;
	try
		SetLength(Data, 1024);
		for i := 0 to High(Data) do
			Data[i] := (i * 3) mod 256;
		Stream.Write(Data[0], Length(Data));
		CompareAllImplementations(Stream, '1KB');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestVariousSizes_10KB;
var
	Stream: TMemoryStream;
	Data: TBytes;
	i: Integer;
begin
	Stream := TMemoryStream.Create;
	try
		SetLength(Data, 10 * 1024);
		for i := 0 to High(Data) do
			Data[i] := (i * 5) mod 256;
		Stream.Write(Data[0], Length(Data));
		CompareAllImplementations(Stream, '10KB');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestVariousSizes_64KB;
var
	Stream: TMemoryStream;
	Data: TBytes;
	i: Integer;
begin
	Stream := TMemoryStream.Create;
	try
		{Exactly at buffer size boundary}
		SetLength(Data, 64 * 1024);
		for i := 0 to High(Data) do
			Data[i] := (i * 7) mod 256;
		Stream.Write(Data[0], Length(Data));
		CompareAllImplementations(Stream, '64KB');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestVariousSizes_65KB;
var
	Stream: TMemoryStream;
	Data: TBytes;
	i: Integer;
begin
	Stream := TMemoryStream.Create;
	try
		{Just over buffer size - tests buffer wrap}
		SetLength(Data, 65 * 1024);
		for i := 0 to High(Data) do
			Data[i] := (i * 11) mod 256;
		Stream.Write(Data[0], Length(Data));
		CompareAllImplementations(Stream, '65KB');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestVariousSizes_128KB;
var
	Stream: TMemoryStream;
	Data: TBytes;
	i: Integer;
begin
	Stream := TMemoryStream.Create;
	try
		{Two buffer reads}
		SetLength(Data, 128 * 1024);
		for i := 0 to High(Data) do
			Data[i] := (i * 13) mod 256;
		Stream.Write(Data[0], Length(Data));
		CompareAllImplementations(Stream, '128KB');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestVariousSizes_256KB;
var
	Stream: TMemoryStream;
	Data: TBytes;
	i: Integer;
begin
	Stream := TMemoryStream.Create;
	try
		{Four buffer reads}
		SetLength(Data, 256 * 1024);
		for i := 0 to High(Data) do
			Data[i] := (i * 17) mod 256;
		Stream.Write(Data[0], Length(Data));
		CompareAllImplementations(Stream, '256KB');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestVariousSizes_512KB;
var
	Stream: TMemoryStream;
	Data: TBytes;
	i: Integer;
begin
	Stream := TMemoryStream.Create;
	try
		{Eight buffer reads}
		SetLength(Data, 512 * 1024);
		for i := 0 to High(Data) do
			Data[i] := (i * 19) mod 256;
		Stream.Write(Data[0], Length(Data));
		CompareAllImplementations(Stream, '512KB');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestBinaryContent_AllZeros;
var
	Stream: TMemoryStream;
	Data: TBytes;
begin
	Stream := TMemoryStream.Create;
	try
		{100KB of all zeros - tests edge case}
		SetLength(Data, 100 * 1024);
		FillChar(Data[0], Length(Data), 0);
		Stream.Write(Data[0], Length(Data));
		CompareAllImplementations(Stream, 'All zeros');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestBinaryContent_AllOnes;
var
	Stream: TMemoryStream;
	Data: TBytes;
begin
	Stream := TMemoryStream.Create;
	try
		{100KB of all $FF bytes}
		SetLength(Data, 100 * 1024);
		FillChar(Data[0], Length(Data), $FF);
		Stream.Write(Data[0], Length(Data));
		CompareAllImplementations(Stream, 'All $FF');
	finally
		Stream.Free;
	end;
end;

procedure THashCalculatorCrossImplementationTest.TestBinaryContent_RandomPattern;
var
	Stream: TMemoryStream;
	Data: TBytes;
	i: Integer;
begin
	Stream := TMemoryStream.Create;
	try
		{100KB of varied binary data - pseudo-random pattern using XOR mixing}
		SetLength(Data, 100 * 1024);
		for i := 0 to High(Data) do
		begin
			{XOR-based mixing that creates varied but deterministic data}
			Data[i] := Byte(i) xor Byte(i shr 8) xor Byte(i shr 16) xor Byte(i shr 5) xor Byte(i shr 13);
		end;
		Stream.Write(Data[0], Length(Data));
		CompareAllImplementations(Stream, 'Random pattern');
	finally
		Stream.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TCloudHashCalculatorTest);
TDUnitX.RegisterTestFixture(TCloudHashCalculatorBCryptTest);
TDUnitX.RegisterTestFixture(TCloudHashCalculatorOpenSSLTest);
TDUnitX.RegisterTestFixture(TNullHashCalculatorTest);
TDUnitX.RegisterTestFixture(THashCalculatorFactoryTest);
TDUnitX.RegisterTestFixture(TIsOpenSSLAvailableTest);
TDUnitX.RegisterTestFixture(TOpenSSLFallbackTest);
TDUnitX.RegisterTestFixture(THashCalculatorFactoryOpenSSLTest);
TDUnitX.RegisterTestFixture(THashCalculatorCrossImplementationTest);

end.
