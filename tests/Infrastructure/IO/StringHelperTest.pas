unit StringHelperTest;

interface

uses
	StringHelper,
	CloudConstants,
	SysUtils,
	DUnitX.TestFramework;

type

	[TestFixture]
	TStringHelperTest = class
	public
		{ SizeOf vs Length verification - proves WideString is pointer-based }
		[Test]
		procedure TestWideStringSizeOfReturnsPointerSize;

		[Test]
		procedure TestUrlEncodeSimple;
		[Test]
		procedure TestUrlEncodeSpecialChars;
		[Test]
		procedure TestUrlEncodeCyrillic;
		{ FormatSize tests - TYPE_AUTO mode }
		[Test]
		procedure TestFormatSizeAutoBytes;
		[Test]
		procedure TestFormatSizeAutoKilobytes;
		[Test]
		procedure TestFormatSizeAutoMegabytes;
		[Test]
		procedure TestFormatSizeAutoGigabytes;
		[Test]
		procedure TestFormatSizeAutoTerabytes;
		[Test]
		procedure TestFormatSizeAutoPetabytes;
		[Test]
		procedure TestFormatSizeAutoZero;
		[Test]
		procedure TestFormatSizeAutoExactBoundary;
		[Test]
		procedure TestFormatSizeAutoBelowBoundary;
		[Test]
		procedure TestFormatSizeAutoLargeValue;

		{ FormatSize tests - explicit SizeType mode }
		[Test]
		procedure TestFormatSizeExplicitBytes;
		[Test]
		procedure TestFormatSizeExplicitKilobytes;
		[Test]
		procedure TestFormatSizeExplicitMegabytes;

		{ IfEmpty tests }
		[Test]
		procedure TestIfEmpty_NonEmptyValue_ReturnsValue;
		[Test]
		procedure TestIfEmpty_EmptyValue_ReturnsDefault;
		[Test]
		procedure TestIfEmpty_BothEmpty_ReturnsEmpty;
		[Test]
		procedure TestIfEmpty_EmptyDefault_ReturnsValue;
	end;

implementation

{ Verifies that SizeOf(WideString) returns pointer size, not string content size.
  This proves why PasswordManager.SetPassword needed fixing. }
procedure TStringHelperTest.TestWideStringSizeOfReturnsPointerSize;
var
	ShortStr: WideString;
	LongStr: WideString;
begin
	ShortStr := 'A';                                  // 1 character
	LongStr := '12345678901234567890123456789012';    // 32 characters

	{ SizeOf returns the same value regardless of string content length }
	Assert.AreEqual(SizeOf(ShortStr), SizeOf(LongStr),
		'SizeOf must be identical for 1-char and 32-char strings');

	{ Both equal pointer size }
	Assert.AreEqual(SizeOf(Pointer), SizeOf(ShortStr),
		'SizeOf(WideString) equals pointer size');

	{ But actual content sizes are different }
	Assert.AreEqual(4, (Length(ShortStr) + 1) * SizeOf(WideChar),
		'1-char string = 4 bytes with null terminator');
	Assert.AreEqual(66, (Length(LongStr) + 1) * SizeOf(WideChar),
		'32-char string = 66 bytes with null terminator');
end;

procedure TStringHelperTest.TestUrlEncodeSimple;
begin
	{ Alphanumeric and safe chars should not be encoded }
	Assert.AreEqual('hello/world', UrlEncode('hello/world'));
end;

procedure TStringHelperTest.TestUrlEncodeSpecialChars;
begin
	{ Spaces and special characters should be percent-encoded }
	Assert.AreEqual('hello%20world', UrlEncode('hello world'));
end;

procedure TStringHelperTest.TestUrlEncodeCyrillic;
var
	Encoded: WideString;
begin
	{ Cyrillic characters should be UTF-8 percent-encoded }
	Encoded := UrlEncode('Test');
	Assert.AreEqual('Test', Encoded);
end;

{ FormatSize TYPE_AUTO tests - automatic unit detection }

procedure TStringHelperTest.TestFormatSizeAutoBytes;
begin
	{ Values under 1024 stay in bytes }
	Assert.AreEqual('0 b', FormatSize(0));
	Assert.AreEqual('1 b', FormatSize(1));
	Assert.AreEqual('100 b', FormatSize(100));
	Assert.AreEqual('1023 b', FormatSize(1023));
end;

procedure TStringHelperTest.TestFormatSizeAutoKilobytes;
begin
	{ Values at or above 1024 convert to next unit }
	Assert.AreEqual('1 kb', FormatSize(1024)); { exactly 1024 converts }
	Assert.AreEqual('1 kb', FormatSize(1025)); { just over boundary: converts }
	Assert.AreEqual('10 kb', FormatSize(10240)); { 10 * 1024 }
end;

procedure TStringHelperTest.TestFormatSizeAutoMegabytes;
begin
	{ Values at or above 1024 kb convert to Mb }
	Assert.AreEqual('1 Mb', FormatSize(1048576)); { exactly 1024^2 = 1 Mb }
	Assert.AreEqual('50 Mb', FormatSize(52428800)); { 50 * 1024^2 }
	Assert.AreEqual('100 Mb', FormatSize(104857600)); { 100 * 1024^2 }
end;

procedure TStringHelperTest.TestFormatSizeAutoGigabytes;
begin
	{ Values at or above 1024 Mb convert to Gb }
	Assert.AreEqual('1 Gb', FormatSize(1073741824)); { exactly 1024^3 = 1 Gb }
	Assert.AreEqual('2 Gb', FormatSize(2147483648)); { 2 * 1024^3 }
	Assert.AreEqual('10 Gb', FormatSize(10737418240)); { 10 * 1024^3 }
end;

procedure TStringHelperTest.TestFormatSizeAutoTerabytes;
begin
	{ Values at or above 1024 Gb convert to Tb }
	Assert.AreEqual('1 Tb', FormatSize(1099511627776)); { exactly 1024^4 = 1 Tb }
	Assert.AreEqual('2 Tb', FormatSize(2199023255552)); { 2 * 1024^4 }
end;

procedure TStringHelperTest.TestFormatSizeAutoPetabytes;
begin
	{ Values at or above 1024 Tb convert to Pb }
	Assert.AreEqual('1 Pb', FormatSize(1125899906842624)); { exactly 1024^5 = 1 Pb }
	Assert.AreEqual('2 Pb', FormatSize(2251799813685248)); { 2 * 1024^5 }
end;

procedure TStringHelperTest.TestFormatSizeAutoZero;
begin
	{ Zero should return 0 bytes }
	Assert.AreEqual('0 b', FormatSize(0));
end;

procedure TStringHelperTest.TestFormatSizeAutoExactBoundary;
begin
	{ Exactly at 1024 boundary converts to next unit (uses < 1024 check) }
	Assert.AreEqual('1 kb', FormatSize(1024));
	Assert.AreEqual('1 Mb', FormatSize(1048576));
end;

procedure TStringHelperTest.TestFormatSizeAutoBelowBoundary;
begin
	{ Just below boundary stays in current unit }
	Assert.AreEqual('1023 b', FormatSize(1023));
	Assert.AreEqual('1023 kb', FormatSize(1047552));
end;

procedure TStringHelperTest.TestFormatSizeAutoLargeValue;
begin
	{ Large values should be handled correctly }
	Assert.AreEqual('2 Tb', FormatSize(2199023255552)); { 2 * 1024^4 }
end;

{ FormatSize explicit SizeType tests }

procedure TStringHelperTest.TestFormatSizeExplicitBytes;
begin
	{ TYPE_BYTES (0) - no conversion, just append 'b' suffix }
	Assert.AreEqual('1024 b', FormatSize(1024, TYPE_BYTES));
	Assert.AreEqual('5000 b', FormatSize(5000, TYPE_BYTES));
end;

procedure TStringHelperTest.TestFormatSizeExplicitKilobytes;
const
	SIZE_TYPE_KILOBYTES = 1;
begin
	{ SizeType=1 (kilobytes) - divide by 1024 once, append 'kb' suffix }
	{ 2048 bytes = 2 kb }
	Assert.AreEqual('2 kb', FormatSize(2048, SIZE_TYPE_KILOBYTES), 'SIZE_TYPE_KILOBYTES should show kb suffix');
	{ 10240 bytes = 10 kb }
	Assert.AreEqual('10 kb', FormatSize(10240, SIZE_TYPE_KILOBYTES), 'SIZE_TYPE_KILOBYTES should divide by 1024 once');
end;

procedure TStringHelperTest.TestFormatSizeExplicitMegabytes;
const
	SIZE_TYPE_MEGABYTES = 2;
begin
	{ SizeType=2 (megabytes) - divide by 1024 twice, append 'Mb' suffix }
	{ 2097152 bytes = 2 Mb }
	Assert.AreEqual('2 Mb', FormatSize(2097152, SIZE_TYPE_MEGABYTES), 'SIZE_TYPE_MEGABYTES should show Mb suffix');
	{ 10485760 bytes = 10 Mb }
	Assert.AreEqual('10 Mb', FormatSize(10485760, SIZE_TYPE_MEGABYTES), 'SIZE_TYPE_MEGABYTES should divide by 1024 twice');
end;

{ IfEmpty tests }

procedure TStringHelperTest.TestIfEmpty_NonEmptyValue_ReturnsValue;
begin
	Assert.AreEqual('custom', IfEmpty('custom', 'default'));
end;

procedure TStringHelperTest.TestIfEmpty_EmptyValue_ReturnsDefault;
begin
	Assert.AreEqual('default', IfEmpty('', 'default'));
end;

procedure TStringHelperTest.TestIfEmpty_BothEmpty_ReturnsEmpty;
begin
	Assert.AreEqual('', IfEmpty('', ''));
end;

procedure TStringHelperTest.TestIfEmpty_EmptyDefault_ReturnsValue;
begin
	Assert.AreEqual('value', IfEmpty('value', ''));
end;

initialization

TDUnitX.RegisterTestFixture(TStringHelperTest);

end.
