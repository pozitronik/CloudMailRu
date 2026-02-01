unit DateTimeUtilsTest;

interface

uses
	DateTimeUtils,
	Windows,
	SysUtils,
	DUnitX.TestFramework;

type

	[TestFixture]
	TDateTimeUtilsTest = class
	public
		[Test]
		procedure TestDateTimeToFileTimeNotZero;
		[Test]
		procedure TestDateTimeToFileTimeKnownValue;
	end;

implementation

{ TDateTimeUtilsTest }

procedure TDateTimeUtilsTest.TestDateTimeToFileTimeNotZero;
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

procedure TDateTimeUtilsTest.TestDateTimeToFileTimeKnownValue;
var
	TestDate: TDateTime;
	FileTime, LocalFileTime: TFileTime;
	SystemTime: TSystemTime;
begin
	{ Convert file time back to local system time to verify round-trip.
		Use noon to avoid timezone offsets crossing day boundaries. }
	TestDate := EncodeDate(2020, 6, 15) + EncodeTime(12, 0, 0, 0);
	FileTime := DateTimeToFileTime(TestDate);

	{ Convert UTC back to local for round-trip verification }
	FileTimeToLocalFileTime(FileTime, LocalFileTime);
	FileTimeToSystemTime(LocalFileTime, SystemTime);
	Assert.AreEqual(Word(2020), SystemTime.wYear);
	Assert.AreEqual(Word(6), SystemTime.wMonth);
	Assert.AreEqual(Word(15), SystemTime.wDay);
end;

initialization

TDUnitX.RegisterTestFixture(TDateTimeUtilsTest);

end.
