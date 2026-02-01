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

		{ DateTimeToFileTime tests }
		[Test]
		procedure TestDateTimeToFileTimeNotZero;
		[Test]
		procedure TestDateTimeToFileTimeKnownValue;
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

{ DateTimeToFileTime tests }

procedure TSystemHelperTest.TestDateTimeToFileTimeNotZero;
var
	TestDate: TDateTime;
	FileTime: TFileTime;
begin
	{ Valid date should produce non-zero file time }
	TestDate := EncodeDate(2020, 6, 15) + EncodeTime(12, 30, 0, 0);
	FileTime := DateTimeToFileTime(TestDate);
	Assert.IsTrue((FileTime.dwLowDateTime <> 0) or (FileTime.dwHighDateTime <> 0),
		'DateTimeToFileTime should return non-zero value for valid date');
end;

procedure TSystemHelperTest.TestDateTimeToFileTimeKnownValue;
var
	TestDate: TDateTime;
	FileTime: TFileTime;
	SystemTime: TSystemTime;
begin
	{ Convert file time back to system time to verify }
	TestDate := EncodeDate(2020, 1, 1) + EncodeTime(0, 0, 0, 0);
	FileTime := DateTimeToFileTime(TestDate);

	{ Convert back and verify the date components }
	FileTimeToSystemTime(FileTime, SystemTime);
	Assert.AreEqual(Word(2020), SystemTime.wYear);
	Assert.AreEqual(Word(1), SystemTime.wMonth);
	Assert.AreEqual(Word(1), SystemTime.wDay);
end;

initialization

TDUnitX.RegisterTestFixture(TSystemHelperTest);

end.
