unit DescriptionTest;

interface

uses
	Description,
	System.Classes,
	Windows,
	DUnitX.TestFramework;

type

	[TestFixture]
	TDescriptionTest = class
	private
		FTempFile: string;
		FDescription: TDescription;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ SetValue / GetValue tests }
		[Test]
		procedure TestSetAndGetValue;
		[Test]
		procedure TestGetValueNotFound;
		[Test]
		procedure TestSetValueEmpty;
		[Test]
		procedure TestSetValueOverwrite;

		{ DeleteValue tests }
		[Test]
		procedure TestDeleteValue;
		[Test]
		procedure TestDeleteValueNotFound;

		{ RenameItem tests }
		[Test]
		procedure TestRenameItemSuccess;
		[Test]
		procedure TestRenameItemNotFound;

		{ Clear tests }
		[Test]
		procedure TestClear;

		{ FormatValue tests via GetValue }
		[Test]
		procedure TestFormatValueAsIs;
		[Test]
		procedure TestFormatValueClear;
		[Test]
		procedure TestFormatValueOneline;

		{ Read / Write file tests }
		[Test]
		procedure TestWriteAndRead;
		[Test]
		procedure TestWriteQuotedFilename;
		[Test]
		procedure TestReadEmptyFile;

		{ Encoding tests }
		[Test]
		procedure TestDetermineEncodingUTF8;
		[Test]
		procedure TestDetermineEncodingUnicode;
	end;

implementation

uses
	System.SysUtils;

{ Setup and TearDown }

procedure TDescriptionTest.Setup;
begin
	FTempFile := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
		'DescriptionTest_' + IntToStr(GetCurrentThreadId) + '.ion';
	FDescription := TDescription.Create(FTempFile, ENCODING_UTF8);
end;

procedure TDescriptionTest.TearDown;
begin
	FDescription.Free;
	if FileExists(FTempFile) then
		System.SysUtils.DeleteFile(FTempFile);
end;

{ SetValue / GetValue tests }

procedure TDescriptionTest.TestSetAndGetValue;
begin
	FDescription.SetValue('testkey', 'testvalue');
	Assert.AreEqual('testvalue', FDescription.GetValue('testkey'));
end;

procedure TDescriptionTest.TestGetValueNotFound;
begin
	Assert.AreEqual('', FDescription.GetValue('nonexistent'));
end;

procedure TDescriptionTest.TestSetValueEmpty;
begin
	{ Setting empty value should delete the key }
	FDescription.SetValue('testkey', 'testvalue');
	Assert.AreEqual('testvalue', FDescription.GetValue('testkey'));

	FDescription.SetValue('testkey', '');
	Assert.AreEqual('', FDescription.GetValue('testkey'));
end;

procedure TDescriptionTest.TestSetValueOverwrite;
begin
	FDescription.SetValue('testkey', 'oldvalue');
	FDescription.SetValue('testkey', 'newvalue');
	Assert.AreEqual('newvalue', FDescription.GetValue('testkey'));
end;

{ DeleteValue tests }

procedure TDescriptionTest.TestDeleteValue;
begin
	FDescription.SetValue('testkey', 'testvalue');
	Assert.AreEqual('testvalue', FDescription.GetValue('testkey'));

	FDescription.DeleteValue('testkey');
	Assert.AreEqual('', FDescription.GetValue('testkey'));
end;

procedure TDescriptionTest.TestDeleteValueNotFound;
begin
	{ Deleting non-existent key should not raise error }
	Assert.IsTrue(FDescription.DeleteValue('nonexistent'));
end;

{ RenameItem tests }

procedure TDescriptionTest.TestRenameItemSuccess;
begin
	FDescription.SetValue('oldkey', 'testvalue');
	Assert.IsTrue(FDescription.RenameItem('oldkey', 'newkey'));
	Assert.AreEqual('', FDescription.GetValue('oldkey'));
	Assert.AreEqual('testvalue', FDescription.GetValue('newkey'));
end;

procedure TDescriptionTest.TestRenameItemNotFound;
begin
	Assert.IsFalse(FDescription.RenameItem('nonexistent', 'newkey'));
end;

{ Clear tests }

procedure TDescriptionTest.TestClear;
begin
	FDescription.SetValue('key1', 'value1');
	FDescription.SetValue('key2', 'value2');
	FDescription.Clear;
	Assert.AreEqual('', FDescription.GetValue('key1'));
	Assert.AreEqual('', FDescription.GetValue('key2'));
end;

{ FormatValue tests }

procedure TDescriptionTest.TestFormatValueAsIs;
begin
	{ FORMAT_AS_IS returns raw value with control characters }
	FDescription.SetValue('testkey', 'line1\nline2');
	Assert.AreEqual('line1\nline2', FDescription.GetValue('testkey', FORMAT_AS_IS));
end;

procedure TDescriptionTest.TestFormatValueClear;
begin
	{ FORMAT_CLEAR replaces \n with line breaks }
	FDescription.SetValue('testkey', 'line1\nline2');
	Assert.AreEqual('line1' + sLineBreak + 'line2', FDescription.GetValue('testkey', FORMAT_CLEAR));
end;

procedure TDescriptionTest.TestFormatValueOneline;
begin
	{ FORMAT_ONELINE replaces \n with double space }
	FDescription.SetValue('testkey', 'line1\nline2');
	Assert.AreEqual('line1  line2', FDescription.GetValue('testkey', FORMAT_ONELINE));
end;

{ Read / Write file tests }

procedure TDescriptionTest.TestWriteAndRead;
var
	Description2: TDescription;
begin
	FDescription.SetValue('key1', 'value1');
	FDescription.SetValue('key2', 'value2');
	FDescription.Write;

	{ Read with new instance }
	Description2 := TDescription.Create(FTempFile, ENCODING_UTF8);
	try
		Description2.Read;
		Assert.AreEqual('value1', Description2.GetValue('key1'));
		Assert.AreEqual('value2', Description2.GetValue('key2'));
	finally
		Description2.Free;
	end;
end;

procedure TDescriptionTest.TestWriteQuotedFilename;
var
	Description2: TDescription;
begin
	{ Filenames with spaces should be quoted }
	FDescription.SetValue('file with spaces.txt', 'description text');
	FDescription.Write;

	Description2 := TDescription.Create(FTempFile, ENCODING_UTF8);
	try
		Description2.Read;
		Assert.AreEqual('description text', Description2.GetValue('file with spaces.txt'));
	finally
		Description2.Free;
	end;
end;

procedure TDescriptionTest.TestReadEmptyFile;
var
	Description2: TDescription;
begin
	{ Empty file should be readable without errors }
	FDescription.Clear;
	FDescription.Write;

	Description2 := TDescription.Create(FTempFile, ENCODING_UTF8);
	try
		Description2.Read;
		Assert.AreEqual('', Description2.GetValue('anykey'));
	finally
		Description2.Free;
	end;
end;

{ Encoding tests }

procedure TDescriptionTest.TestDetermineEncodingUTF8;
var
	F: TFileStream;
	BOM: array[0..2] of Byte;
	Encoding: TEncoding;
begin
	{ Write UTF-8 BOM }
	BOM[0] := $EF;
	BOM[1] := $BB;
	BOM[2] := $BF;
	F := TFileStream.Create(FTempFile, fmCreate);
	try
		F.WriteBuffer(BOM[0], 3);
		F.WriteBuffer(AnsiString('test'), 4);
	finally
		F.Free;
	end;

	Encoding := FDescription.DetermineEncoding;
	Assert.AreSame(TEncoding.UTF8, Encoding);
end;

procedure TDescriptionTest.TestDetermineEncodingUnicode;
var
	F: TFileStream;
	BOM: array[0..1] of Byte;
	Encoding: TEncoding;
	Description2: TDescription;
begin
	{ Write Unicode (UTF-16 LE) BOM }
	BOM[0] := $FF;
	BOM[1] := $FE;
	F := TFileStream.Create(FTempFile, fmCreate);
	try
		F.WriteBuffer(BOM[0], 2);
	finally
		F.Free;
	end;

	Description2 := TDescription.Create(FTempFile, ENCODING_UNICODE);
	try
		Encoding := Description2.DetermineEncoding;
		Assert.AreSame(TEncoding.Unicode, Encoding);
	finally
		Description2.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TDescriptionTest);

end.
