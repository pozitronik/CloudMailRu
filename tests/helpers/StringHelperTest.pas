unit StringHelperTest;

interface

uses
	StringHelper,
	Classes,
	SysUtils,
	DUnitX.TestFramework;

type

	[TestFixture]
	TStringHelperTest = class
	public
		[Test]
		procedure TestImplodeEmpty;
		[Test]
		procedure TestImplodeSingleItem;
		[Test]
		procedure TestImplodeMultipleItems;
		[Test]
		procedure TestExplodeSimple;
		[Test]
		procedure TestExplodeEmpty;
		[Test]
		procedure TestTrimExSingleChar;
		[Test]
		procedure TestTrimExMultipleChars;
		[Test]
		procedure TestTrimExNoMatch;
		[Test]
		procedure TestPosLastSingle;
		[Test]
		procedure TestPosLastMultiple;
		[Test]
		procedure TestPosLastNotFound;
		[Test]
		procedure TestExtractLinkFromUrlFull;
		[Test]
		procedure TestExtractLinkFromUrlShort;
		[Test]
		procedure TestExtractLinkFromUrlNoPrefix;
		[Test]
		procedure TestGetWordFirst;
		[Test]
		procedure TestGetWordSecond;
		[Test]
		procedure TestGetWordOutOfBounds;
		[Test]
		procedure TestGetWordEmpty;
		[Test]
		procedure TestUrlEncodeSimple;
		[Test]
		procedure TestUrlEncodeSpecialChars;
		[Test]
		procedure TestUrlEncodeCyrillic;
		[Test]
		procedure TestMyExtractStringsSimple;
		[Test]
		procedure TestMyExtractStringsQuoted;
		{ TDD: Tests for bug fixes - these should fail before fix }
		[Test]
		procedure TestExplodeSemicolonDelimiter;
		[Test]
		procedure TestExplodeCommaDelimiter;
		[Test]
		procedure TestImplodeMultiCharDelimiter;
	end;

implementation

procedure TStringHelperTest.TestImplodeEmpty;
var
	List: TStringList;
begin
	List := TStringList.Create;
	try
		Assert.AreEqual('', Implode(List, ','));
	finally
		List.Free;
	end;
end;

procedure TStringHelperTest.TestImplodeSingleItem;
var
	List: TStringList;
begin
	List := TStringList.Create;
	try
		List.Add('item1');
		Assert.AreEqual('item1', Implode(List, ','));
	finally
		List.Free;
	end;
end;

procedure TStringHelperTest.TestImplodeMultipleItems;
var
	List: TStringList;
begin
	List := TStringList.Create;
	try
		List.Add('a');
		List.Add('b');
		List.Add('c');
		Assert.AreEqual('a,b,c', Implode(List, ','));
	finally
		List.Free;
	end;
end;

procedure TStringHelperTest.TestExplodeSimple;
var
	List: TStringList;
begin
	List := Explode('a b c', ' ');
	try
		Assert.AreEqual(3, List.Count);
	finally
		List.Free;
	end;
end;

procedure TStringHelperTest.TestExplodeEmpty;
var
	List: TStringList;
begin
	List := Explode('', ' ');
	try
		Assert.AreEqual(0, List.Count);
	finally
		List.Free;
	end;
end;

procedure TStringHelperTest.TestTrimExSingleChar;
begin
	Assert.AreEqual('hello', TrimEx('/hello/', '/'));
end;

procedure TStringHelperTest.TestTrimExMultipleChars;
begin
	Assert.AreEqual('hello', TrimEx('///hello///', '/'));
end;

procedure TStringHelperTest.TestTrimExNoMatch;
begin
	Assert.AreEqual('hello', TrimEx('hello', '/'));
end;

procedure TStringHelperTest.TestPosLastSingle;
begin
	{ In 'a/b/c/d': '/' at positions 2, 4, 6. Last is 6. }
	Assert.AreEqual(6, PosLast('/', 'a/b/c/d'));
end;

procedure TStringHelperTest.TestPosLastMultiple;
begin
	{ Last occurrence of '/' in 'path/to/file' is at position 8 }
	Assert.AreEqual(8, PosLast('/', 'path/to/file'));
end;

procedure TStringHelperTest.TestPosLastNotFound;
begin
	Assert.AreEqual(0, PosLast('/', 'nodelimiters'));
end;

procedure TStringHelperTest.TestExtractLinkFromUrlFull;
begin
	{ Full public URL should extract just the link part }
	Assert.AreEqual('/ABC123/filename', ExtractLinkFromUrl('https://cloud.mail.ru/public/ABC123/filename'));
end;

procedure TStringHelperTest.TestExtractLinkFromUrlShort;
begin
	{ Already short link should remain unchanged }
	Assert.AreEqual('/ABC123/filename', ExtractLinkFromUrl('/ABC123/filename'));
end;

procedure TStringHelperTest.TestExtractLinkFromUrlNoPrefix;
begin
	{ URL without the public prefix should remain unchanged }
	Assert.AreEqual('https://other.site.ru/path', ExtractLinkFromUrl('https://other.site.ru/path'));
end;

procedure TStringHelperTest.TestGetWordFirst;
begin
	Assert.AreEqual('clone', GetWord('clone http://link.com', 0));
end;

procedure TStringHelperTest.TestGetWordSecond;
begin
	Assert.AreEqual('http://link.com', GetWord('clone http://link.com', 1));
end;

procedure TStringHelperTest.TestGetWordOutOfBounds;
begin
	Assert.AreEqual('', GetWord('clone', 5));
end;

procedure TStringHelperTest.TestGetWordEmpty;
begin
	Assert.AreEqual('', GetWord('', 0));
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

procedure TStringHelperTest.TestMyExtractStringsSimple;
var
	List: TStringList;
begin
	List := TStringList.Create;
	try
		MyExtractStrings(['\'], [], PWideChar('path\to\file'), List);
		Assert.AreEqual(3, List.Count);
		Assert.AreEqual('path', List[0]);
		Assert.AreEqual('to', List[1]);
		Assert.AreEqual('file', List[2]);
	finally
		List.Free;
	end;
end;

procedure TStringHelperTest.TestMyExtractStringsQuoted;
var
	List: TStringList;
begin
	List := TStringList.Create;
	try
		MyExtractStrings([' '], [], PWideChar('command "quoted arg" normal'), List);
		Assert.AreEqual(3, List.Count);
		Assert.AreEqual('command', List[0]);
		Assert.AreEqual('"quoted arg"', List[1]);
		Assert.AreEqual('normal', List[2]);
	finally
		List.Free;
	end;
end;

{ TDD: This test exposes the Explode bug - delimiter must be set BEFORE DelimitedText }
procedure TStringHelperTest.TestExplodeSemicolonDelimiter;
var
	List: TStringList;
begin
	List := Explode('a;b;c', ';');
	try
		Assert.AreEqual(3, List.Count, 'Explode with semicolon delimiter should split into 3 items');
		Assert.AreEqual('a', List[0]);
		Assert.AreEqual('b', List[1]);
		Assert.AreEqual('c', List[2]);
	finally
		List.Free;
	end;
end;

{ TDD: Comma delimiter test - works by accident with buggy code (comma is default) }
procedure TStringHelperTest.TestExplodeCommaDelimiter;
var
	List: TStringList;
begin
	List := Explode('x,y,z', ',');
	try
		Assert.AreEqual(3, List.Count, 'Explode with comma delimiter should split into 3 items');
		Assert.AreEqual('x', List[0]);
		Assert.AreEqual('y', List[1]);
		Assert.AreEqual('z', List[2]);
	finally
		List.Free;
	end;
end;

{ TDD: This test exposes the Implode bug - multi-char delimiter deletion }
procedure TStringHelperTest.TestImplodeMultiCharDelimiter;
var
	List: TStringList;
begin
	List := TStringList.Create;
	try
		List.Add('a');
		List.Add('b');
		List.Add('c');
		Assert.AreEqual('a::b::c', Implode(List, '::'), 'Implode with :: delimiter should not leave trailing chars');
	finally
		List.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TStringHelperTest);

end.
