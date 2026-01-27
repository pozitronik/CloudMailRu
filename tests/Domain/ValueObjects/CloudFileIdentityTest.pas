unit CloudFileIdentityTest;

interface

uses
	CloudFileIdentity,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCloudFileIdentityTest = class
	public
		[Test]
		procedure TestIsEqualToIdentical;
		[Test]
		procedure TestIsEqualToDifferentHash;
		[Test]
		procedure TestIsEqualToDifferentSize;
		[Test]
		procedure TestIsEqualToBothDifferent;
		[Test]
		procedure TestIsEqualToEmptyStrings;
		[Test]
		procedure TestIsEqualToZeroSize;
		[Test]
		procedure TestIsEqualToCaseSensitiveHash;
	end;

implementation

procedure TCloudFileIdentityTest.TestIsEqualToIdentical;
var
	Identity1, Identity2: TCloudFileIdentity;
begin
	Identity1.Hash := 'ABC123DEF456';
	Identity1.Size := 1024;

	Identity2.Hash := 'ABC123DEF456';
	Identity2.Size := 1024;

	Assert.IsTrue(Identity1.IsEqualTo(Identity2));
end;

procedure TCloudFileIdentityTest.TestIsEqualToDifferentHash;
var
	Identity1, Identity2: TCloudFileIdentity;
begin
	Identity1.Hash := 'ABC123DEF456';
	Identity1.Size := 1024;

	Identity2.Hash := 'DIFFERENT_HASH';
	Identity2.Size := 1024;

	Assert.IsFalse(Identity1.IsEqualTo(Identity2));
end;

procedure TCloudFileIdentityTest.TestIsEqualToDifferentSize;
var
	Identity1, Identity2: TCloudFileIdentity;
begin
	Identity1.Hash := 'ABC123DEF456';
	Identity1.Size := 1024;

	Identity2.Hash := 'ABC123DEF456';
	Identity2.Size := 2048;

	Assert.IsFalse(Identity1.IsEqualTo(Identity2));
end;

procedure TCloudFileIdentityTest.TestIsEqualToBothDifferent;
var
	Identity1, Identity2: TCloudFileIdentity;
begin
	Identity1.Hash := 'ABC123DEF456';
	Identity1.Size := 1024;

	Identity2.Hash := 'DIFFERENT_HASH';
	Identity2.Size := 2048;

	Assert.IsFalse(Identity1.IsEqualTo(Identity2));
end;

procedure TCloudFileIdentityTest.TestIsEqualToEmptyStrings;
var
	Identity1, Identity2: TCloudFileIdentity;
begin
	Identity1.Hash := '';
	Identity1.Size := 0;

	Identity2.Hash := '';
	Identity2.Size := 0;

	Assert.IsTrue(Identity1.IsEqualTo(Identity2));
end;

procedure TCloudFileIdentityTest.TestIsEqualToZeroSize;
var
	Identity1, Identity2: TCloudFileIdentity;
begin
	{ Two empty files with same hash should be equal }
	Identity1.Hash := 'EMPTY_FILE_HASH';
	Identity1.Size := 0;

	Identity2.Hash := 'EMPTY_FILE_HASH';
	Identity2.Size := 0;

	Assert.IsTrue(Identity1.IsEqualTo(Identity2));
end;

procedure TCloudFileIdentityTest.TestIsEqualToCaseSensitiveHash;
var
	Identity1, Identity2: TCloudFileIdentity;
begin
	{ Hash comparison should be case-sensitive }
	Identity1.Hash := 'ABC123';
	Identity1.Size := 100;

	Identity2.Hash := 'abc123';
	Identity2.Size := 100;

	Assert.IsFalse(Identity1.IsEqualTo(Identity2));
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileIdentityTest);

end.
