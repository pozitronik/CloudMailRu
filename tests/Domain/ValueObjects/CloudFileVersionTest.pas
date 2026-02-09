unit CloudFileVersionTest;

{Unit tests for TCloudFileVersion value object}

interface

uses
	DUnitX.TestFramework,
	CloudFileVersion;

type
	[TestFixture]
	TCloudFileVersionTest = class
	public
		[Test]
		procedure TestHasHash_WhenHashSet_ReturnsTrue;
		[Test]
		procedure TestHasHash_WhenHashEmpty_ReturnsFalse;
		[Test]
		procedure TestDefaultValues_AllFieldsEmpty;
		[Test]
		procedure TestFieldAccess_StoresValues;
	end;

implementation

procedure TCloudFileVersionTest.TestHasHash_WhenHashSet_ReturnsTrue;
var
	Version: TCloudFileVersion;
begin
	Version := Default(TCloudFileVersion);
	Version.Hash := 'DEADBEEF';

	Assert.IsTrue(Version.HasHash, 'HasHash should return True when hash is set');
end;

procedure TCloudFileVersionTest.TestHasHash_WhenHashEmpty_ReturnsFalse;
var
	Version: TCloudFileVersion;
begin
	Version := Default(TCloudFileVersion);

	Assert.IsFalse(Version.HasHash, 'HasHash should return False when hash is empty');
end;

procedure TCloudFileVersionTest.TestDefaultValues_AllFieldsEmpty;
var
	Version: TCloudFileVersion;
begin
	Version := Default(TCloudFileVersion);

	Assert.AreEqual('', Version.Hash, 'Hash should be empty by default');
	Assert.AreEqual('', Version.Name, 'Name should be empty by default');
	Assert.AreEqual('', Version.Path, 'Path should be empty by default');
	Assert.AreEqual(Int64(0), Version.Size, 'Size should be 0 by default');
	Assert.AreEqual(Int64(0), Version.Time, 'Time should be 0 by default');
	Assert.AreEqual(0, Version.Rev, 'Rev should be 0 by default');
	Assert.AreEqual(0, Version.UID, 'UID should be 0 by default');
end;

procedure TCloudFileVersionTest.TestFieldAccess_StoresValues;
var
	Version: TCloudFileVersion;
begin
	Version := Default(TCloudFileVersion);
	Version.Hash := 'ABC123';
	Version.Name := 'test.txt';
	Version.Path := '/test.txt';
	Version.Size := 12345;
	Version.Time := 1700000000;
	Version.Rev := 3;
	Version.UID := 42;

	Assert.AreEqual('ABC123', Version.Hash);
	Assert.AreEqual('test.txt', Version.Name);
	Assert.AreEqual('/test.txt', Version.Path);
	Assert.AreEqual(Int64(12345), Version.Size);
	Assert.AreEqual(Int64(1700000000), Version.Time);
	Assert.AreEqual(3, Version.Rev);
	Assert.AreEqual(42, Version.UID);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileVersionTest);

end.
