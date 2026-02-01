unit IniFilesHelperTest;

interface

uses
	IniFilesHelper,
	IniFiles,
	SysUtils,
	IOUtils,
	DUnitX.TestFramework;

type

	[TestFixture]
	TIniFilesHelperTest = class
	private
		FTestIniPath: string;
		FIniFile: TIniFile;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;
		[Test]
		procedure TestValidateSectionNameValid;
		[Test]
		procedure TestValidateSectionNameWithBrackets;
		[Test]
		procedure TestValidateSectionNameWithNewline;
		[Test]
		procedure TestValidateIdentNameValid;
		[Test]
		procedure TestValidateIdentNameStartsWithLetter;
		[Test]
		procedure TestValidateIdentNameStartsWithNumber;
		[Test]
		procedure TestValidateIdentNameEmpty;
		[Test]
		procedure TestValidateIdentNameSingleChar;
		[Test]
		procedure TestReadInt64Valid;
		[Test]
		procedure TestReadInt64Hex;
		[Test]
		procedure TestReadInt64Default;
		[Test]
		procedure TestWriteInt64;
		[Test]
		procedure TestWriteIntegerIfNotDefaultWritesValue;
		[Test]
		procedure TestWriteIntegerIfNotDefaultDeletesKey;
		[Test]
		procedure TestWriteStringIfNotDefaultWritesValue;
		[Test]
		procedure TestWriteStringIfNotDefaultDeletesKey;
		[Test]
		procedure TestWriteStringInvalidSectionRaises;
		[Test]
		procedure TestWriteStringInvalidIdentRaises;
	end;

implementation

procedure TIniFilesHelperTest.Setup;
begin
	FTestIniPath := TPath.Combine(TPath.GetTempPath, 'test_ini_helper_' + IntToStr(Random(100000)) + '.ini');
	FIniFile := TIniFile.Create(FTestIniPath);
end;

procedure TIniFilesHelperTest.TearDown;
begin
	FIniFile.Free;
	if TFile.Exists(FTestIniPath) then
		TFile.Delete(FTestIniPath);
end;

procedure TIniFilesHelperTest.TestValidateSectionNameValid;
begin
	Assert.IsTrue(FIniFile.ValidateSectionName('ValidSection'));
	Assert.IsTrue(FIniFile.ValidateSectionName('Section With Spaces'));
	Assert.IsTrue(FIniFile.ValidateSectionName('Section123'));
end;

procedure TIniFilesHelperTest.TestValidateSectionNameWithBrackets;
begin
	{ Section names cannot contain [ or ] }
	Assert.IsFalse(FIniFile.ValidateSectionName('Invalid[Section'));
	Assert.IsFalse(FIniFile.ValidateSectionName('Invalid]Section'));
	Assert.IsFalse(FIniFile.ValidateSectionName('[Invalid]'));
end;

procedure TIniFilesHelperTest.TestValidateSectionNameWithNewline;
begin
	{ Section names cannot contain newlines }
	Assert.IsFalse(FIniFile.ValidateSectionName('Invalid' + #10 + 'Section'));
end;

procedure TIniFilesHelperTest.TestValidateIdentNameValid;
begin
	Assert.IsTrue(FIniFile.ValidateIdentName('ValidIdent'));
	Assert.IsTrue(FIniFile.ValidateIdentName('valid_ident'));
	Assert.IsTrue(FIniFile.ValidateIdentName('Ident123'));
	Assert.IsTrue(FIniFile.ValidateIdentName('$special'));
	Assert.IsTrue(FIniFile.ValidateIdentName('.dotstart'));
end;

procedure TIniFilesHelperTest.TestValidateIdentNameStartsWithLetter;
begin
	Assert.IsTrue(FIniFile.ValidateIdentName('aIdent'));
	Assert.IsTrue(FIniFile.ValidateIdentName('ZIdent'));
end;

procedure TIniFilesHelperTest.TestValidateIdentNameStartsWithNumber;
begin
	{ Identifiers cannot start with a number }
	Assert.IsFalse(FIniFile.ValidateIdentName('1InvalidIdent'));
	Assert.IsFalse(FIniFile.ValidateIdentName('9test'));
end;

procedure TIniFilesHelperTest.TestValidateIdentNameEmpty;
begin
	Assert.IsFalse(FIniFile.ValidateIdentName(''));
end;

procedure TIniFilesHelperTest.TestValidateIdentNameSingleChar;
begin
	{Single-character identifiers should be valid}
	Assert.IsTrue(FIniFile.ValidateIdentName('x'), 'Single lowercase letter should be valid');
	Assert.IsTrue(FIniFile.ValidateIdentName('Z'), 'Single uppercase letter should be valid');
	Assert.IsTrue(FIniFile.ValidateIdentName('$'), 'Single $ should be valid');
end;

procedure TIniFilesHelperTest.TestReadInt64Valid;
begin
	FIniFile.WriteString('TestSection', 'Int64Value', '9223372036854775807');

	Assert.AreEqual(Int64(9223372036854775807), FIniFile.ReadInt64('TestSection', 'Int64Value', 0));
end;

procedure TIniFilesHelperTest.TestReadInt64Hex;
begin
	{ ReadInt64 supports 0x hex notation }
	FIniFile.WriteString('TestSection', 'HexValue', '0xFF');

	Assert.AreEqual(Int64(255), FIniFile.ReadInt64('TestSection', 'HexValue', 0));
end;

procedure TIniFilesHelperTest.TestReadInt64Default;
begin
	Assert.AreEqual(Int64(42), FIniFile.ReadInt64('TestSection', 'NonExistent', 42));
end;

procedure TIniFilesHelperTest.TestWriteInt64;
begin
	FIniFile.WriteInt64('TestSection', 'TestKey', 123456789012345);

	Assert.AreEqual(Int64(123456789012345), FIniFile.ReadInt64('TestSection', 'TestKey', 0));
end;

procedure TIniFilesHelperTest.TestWriteIntegerIfNotDefaultWritesValue;
begin
	FIniFile.WriteIntegerIfNotDefault('TestSection', 'TestKey', 100, 0);

	Assert.AreEqual(100, FIniFile.ReadInteger('TestSection', 'TestKey', 0));
end;

procedure TIniFilesHelperTest.TestWriteIntegerIfNotDefaultDeletesKey;
begin
	{ First write a value }
	FIniFile.WriteInteger('TestSection', 'TestKey', 100);
	Assert.AreEqual(100, FIniFile.ReadInteger('TestSection', 'TestKey', 0));

	{ Now write default - should delete }
	FIniFile.WriteIntegerIfNotDefault('TestSection', 'TestKey', 0, 0);

	{ Key should be gone, return default }
	Assert.AreEqual(999, FIniFile.ReadInteger('TestSection', 'TestKey', 999));
end;

procedure TIniFilesHelperTest.TestWriteStringIfNotDefaultWritesValue;
begin
	FIniFile.WriteStringIfNotDefault('TestSection', 'TestKey', 'custom', 'default');

	Assert.AreEqual('custom', FIniFile.ReadString('TestSection', 'TestKey', ''));
end;

procedure TIniFilesHelperTest.TestWriteStringIfNotDefaultDeletesKey;
begin
	{ First write a value }
	FIniFile.WriteString('TestSection', 'TestKey', 'custom');

	{ Now write default - should delete }
	FIniFile.WriteStringIfNotDefault('TestSection', 'TestKey', 'default', 'default');

	{ Key should be gone }
	Assert.AreEqual('fallback', FIniFile.ReadString('TestSection', 'TestKey', 'fallback'));
end;

procedure TIniFilesHelperTest.TestWriteStringInvalidSectionRaises;
begin
	Assert.WillRaise(
		procedure
		begin
			FIniFile.WriteString('Invalid[Section', 'key', 'value');
		end,
		EIniFileException
	);
end;

procedure TIniFilesHelperTest.TestWriteStringInvalidIdentRaises;
begin
	Assert.WillRaise(
		procedure
		begin
			FIniFile.WriteString('Section', '1invalid', 'value');
		end,
		EIniFileException
	);
end;

initialization

TDUnitX.RegisterTestFixture(TIniFilesHelperTest);

end.
