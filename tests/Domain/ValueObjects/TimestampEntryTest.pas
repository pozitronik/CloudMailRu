unit TimestampEntryTest;

{Unit tests for TTimestampEntry value object}

interface

uses
	DUnitX.TestFramework,
	TimestampEntry;

type
	[TestFixture]
	TTimestampEntryTest = class
	public
		[Test]
		procedure TestEmpty_ReturnsZeroFields;
		[Test]
		procedure TestIsEmpty_WhenBothZero_ReturnsTrue;
		[Test]
		procedure TestIsEmpty_WhenLocalMTimeSet_ReturnsFalse;
		[Test]
		procedure TestIsEmpty_WhenCloudMTimeSet_ReturnsFalse;
		[Test]
		procedure TestIsEmpty_WhenBothSet_ReturnsFalse;
		[Test]
		procedure TestFieldAccess_StoresValues;
	end;

implementation

procedure TTimestampEntryTest.TestEmpty_ReturnsZeroFields;
var
	Entry: TTimestampEntry;
begin
	Entry := TTimestampEntry.Empty;

	Assert.AreEqual(Int64(0), Entry.LocalMTime);
	Assert.AreEqual(Int64(0), Entry.CloudMTime);
end;

procedure TTimestampEntryTest.TestIsEmpty_WhenBothZero_ReturnsTrue;
var
	Entry: TTimestampEntry;
begin
	Entry := TTimestampEntry.Empty;

	Assert.IsTrue(Entry.IsEmpty);
end;

procedure TTimestampEntryTest.TestIsEmpty_WhenLocalMTimeSet_ReturnsFalse;
var
	Entry: TTimestampEntry;
begin
	Entry := TTimestampEntry.Empty;
	Entry.LocalMTime := 1704067200;

	Assert.IsFalse(Entry.IsEmpty);
end;

procedure TTimestampEntryTest.TestIsEmpty_WhenCloudMTimeSet_ReturnsFalse;
var
	Entry: TTimestampEntry;
begin
	Entry := TTimestampEntry.Empty;
	Entry.CloudMTime := 1704067200;

	Assert.IsFalse(Entry.IsEmpty);
end;

procedure TTimestampEntryTest.TestIsEmpty_WhenBothSet_ReturnsFalse;
var
	Entry: TTimestampEntry;
begin
	Entry.LocalMTime := 1704067200;
	Entry.CloudMTime := 1704153600;

	Assert.IsFalse(Entry.IsEmpty);
end;

procedure TTimestampEntryTest.TestFieldAccess_StoresValues;
var
	Entry: TTimestampEntry;
begin
	Entry.LocalMTime := 1704067200;
	Entry.CloudMTime := 1704153600;

	Assert.AreEqual(Int64(1704067200), Entry.LocalMTime);
	Assert.AreEqual(Int64(1704153600), Entry.CloudMTime);
end;

initialization
	TDUnitX.RegisterTestFixture(TTimestampEntryTest);

end.
