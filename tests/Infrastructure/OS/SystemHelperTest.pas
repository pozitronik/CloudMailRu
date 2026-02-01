unit SystemHelperTest;

interface

uses
	SystemHelper,
	Windows,
	SysUtils,
	DUnitX.TestFramework;

type

	[TestFixture]
	TSystemHelperTest = class
	public
		{ CheckFlag tests }
		[Test]
		procedure TestCheckFlagSingleBitSet;
		[Test]
		procedure TestCheckFlagSingleBitNotSet;
		[Test]
		procedure TestCheckFlagMultipleBits;
		[Test]
		procedure TestCheckFlagZeroFlags;

	end;

implementation

{TSystemHelperTest}

{ DateTimeToUnix tests }

{ CheckFlag tests }

procedure TSystemHelperTest.TestCheckFlagSingleBitSet;
begin
	{ Bit 0 (value 1) is set in flags 1 }
	Assert.IsTrue(CheckFlag(1, 1));
	{ Bit 1 (value 2) is set in flags 2 }
	Assert.IsTrue(CheckFlag(2, 2));
	{ Bit 2 (value 4) is set in flags 4 }
	Assert.IsTrue(CheckFlag(4, 4));
end;

procedure TSystemHelperTest.TestCheckFlagSingleBitNotSet;
begin
	{ Bit 0 (value 1) is NOT set in flags 2 }
	Assert.IsFalse(CheckFlag(1, 2));
	{ Bit 1 (value 2) is NOT set in flags 1 }
	Assert.IsFalse(CheckFlag(2, 1));
	{ Bit 2 (value 4) is NOT set in flags 3 }
	Assert.IsFalse(CheckFlag(4, 3));
end;

procedure TSystemHelperTest.TestCheckFlagMultipleBits;
begin
	{ Multiple bits set: flags = 7 (binary 111) }
	Assert.IsTrue(CheckFlag(1, 7));
	Assert.IsTrue(CheckFlag(2, 7));
	Assert.IsTrue(CheckFlag(4, 7));
	{ Bit 3 (value 8) is NOT set in flags 7 }
	Assert.IsFalse(CheckFlag(8, 7));
end;

procedure TSystemHelperTest.TestCheckFlagZeroFlags;
begin
	{ No bits set when flags = 0 }
	Assert.IsFalse(CheckFlag(1, 0));
	Assert.IsFalse(CheckFlag(2, 0));
	Assert.IsFalse(CheckFlag(255, 0));
end;

initialization

TDUnitX.RegisterTestFixture(TSystemHelperTest);

end.
