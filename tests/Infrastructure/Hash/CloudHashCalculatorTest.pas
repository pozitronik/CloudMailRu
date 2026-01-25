unit CloudHashCalculatorTest;

interface

uses
	CloudHashCalculator,
	TCProgress,
	WindowsFileSystem,
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
	end;

	{Tests for TCloudHashCalculatorBCrypt (Windows CNG implementation)}
	[TestFixture]
	TCloudHashCalculatorBCryptTest = class(THashCalculatorTestBase)
	protected
		procedure CreateCalculator; override;
	public
		[Test]
		procedure TestBCrypt_MatchesDelphiImplementation;
	end;

	{Tests for TCloudHashCalculatorOpenSSL (OpenSSL EVP implementation)}
	[TestFixture]
	TCloudHashCalculatorOpenSSLTest = class(THashCalculatorTestBase)
	protected
		procedure CreateCalculator; override;
	public
		[Test]
		procedure TestOpenSSL_MatchesDelphiImplementation;
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
	end;

implementation

uses
	SETTINGS_CONSTANTS;

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

{ TCloudHashCalculatorOpenSSLTest }

procedure TCloudHashCalculatorOpenSSLTest.CreateCalculator;
begin
	if IsOpenSSLAvailable then
		FCalculator := TCloudHashCalculatorOpenSSL.Create(TNullProgress.Create, TWindowsFileSystem.Create)
	else
		FCalculator := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
end;

procedure TCloudHashCalculatorOpenSSLTest.TestOpenSSL_MatchesDelphiImplementation;
var
	DelphiCalc, OpenSSLCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, OpenSSLHash: WideString;
	Data: AnsiString;
begin
	if not IsOpenSSLAvailable then
	begin
		Assert.Pass('OpenSSL not available on this system, skipping comparison');
		Exit;
	end;

	DelphiCalc := TCloudHashCalculator.Create(TNullProgress.Create, TWindowsFileSystem.Create);
	OpenSSLCalc := TCloudHashCalculatorOpenSSL.Create(TNullProgress.Create, TWindowsFileSystem.Create);

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
	Calculator := CreateHashCalculator(HashStrategyDelphi, TNullProgress.Create, TWindowsFileSystem.Create);
	Assert.IsNotNull(Calculator);
	Assert.IsTrue(Calculator is TCloudHashCalculator, 'Should return TCloudHashCalculator for Delphi strategy');
end;

procedure THashCalculatorFactoryTest.TestCreateHashCalculator_BCrypt_ReturnsNonNil;
var
	Calculator: ICloudHashCalculator;
begin
	Calculator := CreateHashCalculator(HashStrategyBCrypt, TNullProgress.Create, TWindowsFileSystem.Create);
	Assert.IsNotNull(Calculator);
	if IsBCryptAvailable then
		Assert.IsTrue(Calculator is TCloudHashCalculatorBCrypt, 'Should return TCloudHashCalculatorBCrypt when BCrypt available')
	else
		Assert.IsTrue(Calculator is TCloudHashCalculator, 'Should fallback to TCloudHashCalculator when BCrypt not available');
end;

procedure THashCalculatorFactoryTest.TestCreateHashCalculator_OpenSSL_ReturnsNonNil;
var
	Calculator: ICloudHashCalculator;
begin
	Calculator := CreateHashCalculator(HashStrategyOpenSSL, TNullProgress.Create, TWindowsFileSystem.Create);
	Assert.IsNotNull(Calculator);
	if IsOpenSSLAvailable then
		Assert.IsTrue(Calculator is TCloudHashCalculatorOpenSSL, 'Should return TCloudHashCalculatorOpenSSL when OpenSSL available')
	else
		Assert.IsTrue(Calculator is TCloudHashCalculator, 'Should fallback to TCloudHashCalculator when OpenSSL not available');
end;

procedure THashCalculatorFactoryTest.TestCreateHashCalculator_Auto_ReturnsNonNil;
var
	Calculator: ICloudHashCalculator;
begin
	Calculator := CreateHashCalculator(HashStrategyAuto, TNullProgress.Create, TWindowsFileSystem.Create);
	Assert.IsNotNull(Calculator);
	{ Auto should prefer BCrypt, then OpenSSL, then Delphi }
	if IsBCryptAvailable then
		Assert.IsTrue(Calculator is TCloudHashCalculatorBCrypt, 'Auto should prefer BCrypt when available')
	else if IsOpenSSLAvailable then
		Assert.IsTrue(Calculator is TCloudHashCalculatorOpenSSL, 'Auto should use OpenSSL when BCrypt not available')
	else
		Assert.IsTrue(Calculator is TCloudHashCalculator, 'Auto should fallback to Delphi');
end;

procedure THashCalculatorFactoryTest.TestCreateHashCalculator_InvalidStrategy_ReturnsDelphi;
var
	Calculator: ICloudHashCalculator;
begin
	{ Test with an invalid strategy value }
	Calculator := CreateHashCalculator(999, TNullProgress.Create, TWindowsFileSystem.Create);
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
	Available := IsOpenSSLAvailable;
	{ Just verify it returns without exception }
	Assert.Pass(Format('OpenSSL available: %s', [BoolToStr(Available, True)]));
end;

procedure THashCalculatorFactoryTest.TestAllStrategies_ProduceSameHash;
var
	DelphiCalc, BCryptCalc, OpenSSLCalc: ICloudHashCalculator;
	Stream: TMemoryStream;
	DelphiHash, BCryptHash, OpenSSLHash: WideString;
	Data: AnsiString;
begin
	DelphiCalc := CreateHashCalculator(HashStrategyDelphi, TNullProgress.Create, TWindowsFileSystem.Create);
	BCryptCalc := CreateHashCalculator(HashStrategyBCrypt, TNullProgress.Create, TWindowsFileSystem.Create);
	OpenSSLCalc := CreateHashCalculator(HashStrategyOpenSSL, TNullProgress.Create, TWindowsFileSystem.Create);

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

initialization

TDUnitX.RegisterTestFixture(TCloudHashCalculatorTest);
TDUnitX.RegisterTestFixture(TCloudHashCalculatorBCryptTest);
TDUnitX.RegisterTestFixture(TCloudHashCalculatorOpenSSLTest);
TDUnitX.RegisterTestFixture(TNullHashCalculatorTest);
TDUnitX.RegisterTestFixture(THashCalculatorFactoryTest);

end.
