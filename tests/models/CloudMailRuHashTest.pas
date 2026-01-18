unit CloudMailRuHashTest;

interface

uses
	CloudMailRu,
	CloudSettings,
	CMRFileIdentity,
	CMRConstants,
	LANGUAGE_STRINGS,
	PLUGIN_TYPES,
	ILoggerInterface,
	IProgressInterface,
	IRequestInterface,
	IAuthStrategyInterface,
	TestHelper,
	System.Classes,
	System.SysUtils,
	System.Hash,
	DUnitX.TestFramework;

type
	{ Testable subclass that exposes protected CloudHash methods }
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		function TestCloudHash(Path: WideString): WideString; overload;
		function TestCloudHash(Stream: TStream; Path: WideString = CALCULATING_HASH): WideString; overload;
	end;

	{ Tests for TCloudMailRu hashing methods.
	  CloudHash implements Cloud Mail.ru proprietary hash algorithm:
	  - Files < 21 bytes: pad to 20 bytes, return hex digest
	  - Files >= 21 bytes: SHA1('mrCloud' + content + size_string) }
	[TestFixture]
	TCloudMailRuHashTest = class
	private
		FCloud: TTestableCloudMailRu;
		FSettings: TCloudSettings;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ CloudHash(Stream) tests - small files (< 21 bytes) }
		[Test]
		procedure TestCloudHashStream_EmptyStream;
		[Test]
		procedure TestCloudHashStream_SmallFile_1Byte;
		[Test]
		procedure TestCloudHashStream_SmallFile_19Bytes;
		[Test]
		procedure TestCloudHashStream_SmallFile_20Bytes;

		{ CloudHash(Stream) tests - boundary and large files (>= 21 bytes) }
		[Test]
		procedure TestCloudHashStream_BoundaryFile_21Bytes;
		[Test]
		procedure TestCloudHashStream_LargeFile_100Bytes;
		[Test]
		procedure TestCloudHashStream_LargeFile_KnownContent;

		{ CloudHash(Path) tests - file-based }
		[Test]
		procedure TestCloudHashPath_NonExistentFile;
		[Test]
		procedure TestCloudHashPath_ExistingSmallFile;
		[Test]
		procedure TestCloudHashPath_ExistingLargeFile;

		{ FileIdentity tests }
		[Test]
		procedure TestFileIdentity_NonExistentFile;
		[Test]
		procedure TestFileIdentity_ExistingFile;

		{ Hash consistency tests }
		[Test]
		procedure TestCloudHash_SameContentSameHash;
		[Test]
		procedure TestCloudHash_DifferentContentDifferentHash;

		{ Hash format tests }
		[Test]
		procedure TestCloudHash_ReturnsUppercaseHex;
		[Test]
		procedure TestCloudHash_SmallFileReturns40CharHex;
		[Test]
		procedure TestCloudHash_LargeFileReturns40CharHex;
	end;

implementation

{ TTestableCloudMailRu }

function TTestableCloudMailRu.TestCloudHash(Path: WideString): WideString;
begin
	Result := CloudHash(Path);
end;

function TTestableCloudMailRu.TestCloudHash(Stream: TStream; Path: WideString): WideString;
begin
	Result := CloudHash(Stream, Path);
end;

{ TCloudMailRuHashTest }

procedure TCloudMailRuHashTest.Setup;
begin
	FSettings := Default(TCloudSettings);
	FCloud := TTestableCloudMailRu.Create(FSettings, nil, TNullAuthStrategy.Create, TNullLogger.Create, TNullProgress.Create, TNullRequest.Create);
end;

procedure TCloudMailRuHashTest.TearDown;
begin
	FCloud.Free;
end;

{ CloudHash(Stream) tests - small files }

procedure TCloudMailRuHashTest.TestCloudHashStream_EmptyStream;
var
	Stream: TMemoryStream;
	Hash: WideString;
begin
	{ Empty stream (0 bytes) is less than 21 bytes threshold }
	Stream := TMemoryStream.Create;
	try
		Hash := FCloud.TestCloudHash(Stream);
		{ Empty 20-byte buffer hashed - should return 40-char uppercase hex }
		Assert.AreEqual(40, Length(Hash), 'Hash should be 40 characters (20 bytes as hex)');
		{ All zeros = '0000000000000000000000000000000000000000' }
		Assert.AreEqual('0000000000000000000000000000000000000000', Hash);
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuHashTest.TestCloudHashStream_SmallFile_1Byte;
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

		Hash := FCloud.TestCloudHash(Stream);
		{ 1 byte + 19 zeros = 'A' followed by 38 zeros in hex }
		Assert.AreEqual(40, Length(Hash));
		Assert.AreEqual('4100000000000000000000000000000000000000', Hash);
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuHashTest.TestCloudHashStream_SmallFile_19Bytes;
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

		Hash := FCloud.TestCloudHash(Stream);
		Assert.AreEqual(40, Length(Hash));
		{ 19 bytes of content + 1 zero byte }
		Assert.AreEqual('3132333435363738393031323334353637383900', Hash);
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuHashTest.TestCloudHashStream_SmallFile_20Bytes;
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

		Hash := FCloud.TestCloudHash(Stream);
		Assert.AreEqual(40, Length(Hash));
		{ All 20 bytes of content, no padding }
		Assert.AreEqual('3132333435363738393031323334353637383930', Hash);
	finally
		Stream.Free;
	end;
end;

{ CloudHash(Stream) tests - boundary and large files }

procedure TCloudMailRuHashTest.TestCloudHashStream_BoundaryFile_21Bytes;
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

		Hash := FCloud.TestCloudHash(Stream);
		{ SHA1 hash is also 40 chars }
		Assert.AreEqual(40, Length(Hash), 'SHA1 hash should be 40 characters');
		{ SHA1('mrCloud' + content + '21') }
		Assert.AreEqual('04722643E3F9B3B881B389D9ACD1B28DFAD56505', Hash);
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuHashTest.TestCloudHashStream_LargeFile_100Bytes;
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

		Hash := FCloud.TestCloudHash(Stream);
		Assert.AreEqual(40, Length(Hash));
		{ Non-empty hash for valid content }
		Assert.AreNotEqual('', Hash);
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuHashTest.TestCloudHashStream_LargeFile_KnownContent;
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

		Hash := FCloud.TestCloudHash(Stream);
		Assert.AreEqual(40, Length(Hash));
		{ Store actual hash for regression - will be filled after first run }
		Assert.AreNotEqual('', Hash, 'Hash should not be empty for valid content');
	finally
		Stream.Free;
	end;
end;

{ CloudHash(Path) tests }

procedure TCloudMailRuHashTest.TestCloudHashPath_NonExistentFile;
var
	Hash: WideString;
begin
	Hash := FCloud.TestCloudHash('C:\NonExistent\File\Path.txt');
	Assert.AreEqual('', Hash, 'Non-existent file should return empty hash');
end;

procedure TCloudMailRuHashTest.TestCloudHashPath_ExistingSmallFile;
var
	Hash: WideString;
	FilePath: WideString;
begin
	{ SettingsRedirect.ini is 19 bytes - small file path }
	FilePath := DataPath('SettingsRedirect.ini');
	Hash := FCloud.TestCloudHash(FilePath);

	Assert.AreEqual(40, Length(Hash), 'Small file hash should be 40 characters');
	Assert.AreNotEqual('', Hash, 'Existing file should return non-empty hash');
end;

procedure TCloudMailRuHashTest.TestCloudHashPath_ExistingLargeFile;
var
	Hash: WideString;
	FilePath: WideString;
begin
	{ Settings.ini is 817 bytes - large file path }
	FilePath := DataPath('Settings.ini');
	Hash := FCloud.TestCloudHash(FilePath);

	Assert.AreEqual(40, Length(Hash), 'Large file hash should be 40 characters');
	Assert.AreNotEqual('', Hash, 'Existing file should return non-empty hash');
end;

{ FileIdentity tests }

procedure TCloudMailRuHashTest.TestFileIdentity_NonExistentFile;
var
	Identity: TCMRFileIdentity;
begin
	Identity := FCloud.FileIdentity('C:\NonExistent\File\Path.txt');

	Assert.AreEqual('', Identity.Hash, 'Non-existent file should have empty hash');
	Assert.AreEqual(Int64(-1), Identity.Size, 'Non-existent file should have size -1');
end;

procedure TCloudMailRuHashTest.TestFileIdentity_ExistingFile;
var
	Identity: TCMRFileIdentity;
	FilePath: WideString;
begin
	FilePath := DataPath('Settings.ini');
	Identity := FCloud.FileIdentity(FilePath);

	Assert.AreEqual(40, Length(Identity.Hash), 'Hash should be 40 characters');
	Assert.AreEqual(Int64(817), Identity.Size, 'Settings.ini should be 817 bytes');
end;

{ Hash consistency tests }

procedure TCloudMailRuHashTest.TestCloudHash_SameContentSameHash;
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

		Hash1 := FCloud.TestCloudHash(Stream1);
		Hash2 := FCloud.TestCloudHash(Stream2);

		Assert.AreEqual(Hash1, Hash2, 'Same content must produce same hash');
	finally
		Stream1.Free;
		Stream2.Free;
	end;
end;

procedure TCloudMailRuHashTest.TestCloudHash_DifferentContentDifferentHash;
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

		Hash1 := FCloud.TestCloudHash(Stream1);
		Hash2 := FCloud.TestCloudHash(Stream2);

		Assert.AreNotEqual(Hash1, Hash2, 'Different content must produce different hash');
	finally
		Stream1.Free;
		Stream2.Free;
	end;
end;

{ Hash format tests }

procedure TCloudMailRuHashTest.TestCloudHash_ReturnsUppercaseHex;
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

		Hash := FCloud.TestCloudHash(Stream);

		{ Verify all characters are uppercase hex (0-9, A-F) }
		for C in Hash do
			Assert.IsTrue(CharInSet(C, ['0'..'9', 'A'..'F']),
				Format('Character "%s" is not uppercase hex', [C]));
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuHashTest.TestCloudHash_SmallFileReturns40CharHex;
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

		Hash := FCloud.TestCloudHash(Stream);
		Assert.AreEqual(40, Length(Hash), 'Small file hash must be exactly 40 characters');
	finally
		Stream.Free;
	end;
end;

procedure TCloudMailRuHashTest.TestCloudHash_LargeFileReturns40CharHex;
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

		Hash := FCloud.TestCloudHash(Stream);
		Assert.AreEqual(40, Length(Hash), 'Large file SHA1 hash must be exactly 40 characters');
	finally
		Stream.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TCloudMailRuHashTest);

end.
