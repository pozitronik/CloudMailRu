unit HashInfoTest;

interface

uses
	HashInfo,
	LanguageStrings,
	DUnitX.TestFramework;

type

	[TestFixture]
	THashInfoTest = class
	public
		[Test]
		procedure TestValidHashSizeName;
		[Test]
		procedure TestValidHashSizeOnly;
		[Test]
		procedure TestValidWithHashCommand;
		[Test]
		procedure TestValidWithQuotedParameter;
		[Test]
		procedure TestInvalidNoDivisor;
		[Test]
		procedure TestInvalidHashTooShort;
		[Test]
		procedure TestInvalidHashTooLong;
		[Test]
		procedure TestInvalidSizeNotNumeric;
		[Test]
		procedure TestCloudFileIdentityProperty;
		[Test]
		procedure TestNameDefaultsToHash;
		[Test]
		procedure TestDoCleanFalse;

		{Inline comment support (#315)}
		[Test]
		procedure TestValidWithInlineComment;
		[Test]
		procedure TestValidWithInlineCommentNoSpace;
		[Test]
		procedure TestValidWithInlineCommentPreservesName;
		[Test]
		procedure TestCommentOnlyLine;
		[Test]
		procedure TestEmptyLine;

		{Zero-based Copy behavior verification - Delphi supports zero-based strings}
		[Test]
		procedure TestCopyWithZeroPosition_WorksCorrectly;
	end;

implementation

const
	{ Valid 40-char SHA1 hash for testing }
	VALID_HASH = '0123456789abcdef0123456789abcdef01234567';
	SHORT_HASH = '0123456789abcdef';
	LONG_HASH = '0123456789abcdef0123456789abcdef0123456789extra';

procedure THashInfoTest.TestValidHashSizeName;
var
	HI: THashInfo;
begin
	HI := THashInfo.Create(VALID_HASH + ':12345:myfile.txt');
	try
		Assert.IsTrue(HI.valid);
		Assert.AreEqual(VALID_HASH, HI.hash);
		Assert.AreEqual(Int64(12345), HI.size);
		Assert.AreEqual('myfile.txt', HI.name);
		Assert.IsEmpty(HI.errorString);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestValidHashSizeOnly;
var
	HI: THashInfo;
begin
	{ When name is not provided, it should default to hash }
	HI := THashInfo.Create(VALID_HASH + ':67890');
	try
		Assert.IsTrue(HI.valid);
		Assert.AreEqual(VALID_HASH, HI.hash);
		Assert.AreEqual(Int64(67890), HI.size);
		Assert.AreEqual(VALID_HASH, HI.name);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestValidWithHashCommand;
var
	HI: THashInfo;
begin
	{ Parameter with 'hash ' command prefix should be cleaned }
	HI := THashInfo.Create('hash ' + VALID_HASH + ':1000:file.txt');
	try
		Assert.IsTrue(HI.valid);
		Assert.AreEqual(VALID_HASH, HI.hash);
		Assert.AreEqual(Int64(1000), HI.size);
		Assert.AreEqual('file.txt', HI.name);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestValidWithQuotedParameter;
var
	HI: THashInfo;
begin
	{ Quoted parameter should have quotes trimmed }
	HI := THashInfo.Create('hash "' + VALID_HASH + ':2000:quoted.txt"');
	try
		Assert.IsTrue(HI.valid);
		Assert.AreEqual('quoted.txt', HI.name);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestInvalidNoDivisor;
var
	HI: THashInfo;
begin
	HI := THashInfo.Create('invalidparameter');
	try
		Assert.IsFalse(HI.valid);
		Assert.AreEqual(ERR_WRONG_FORMAT, HI.errorString);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestInvalidHashTooShort;
var
	HI: THashInfo;
begin
	HI := THashInfo.Create(SHORT_HASH + ':1000:file.txt');
	try
		Assert.IsFalse(HI.valid);
		Assert.AreEqual(ERR_WRONG_HASH_LENGTH, HI.errorString);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestInvalidHashTooLong;
var
	HI: THashInfo;
begin
	HI := THashInfo.Create(LONG_HASH + ':1000:file.txt');
	try
		Assert.IsFalse(HI.valid);
		Assert.AreEqual(ERR_WRONG_HASH_LENGTH, HI.errorString);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestInvalidSizeNotNumeric;
var
	HI: THashInfo;
begin
	HI := THashInfo.Create(VALID_HASH + ':notanumber:file.txt');
	try
		Assert.IsFalse(HI.valid);
		Assert.AreEqual(ERR_WRONG_SIZE_FORMAT, HI.errorString);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestCloudFileIdentityProperty;
var
	HI: THashInfo;
begin
	HI := THashInfo.Create(VALID_HASH + ':99999:test.bin');
	try
		Assert.IsTrue(HI.valid);
		Assert.AreEqual(VALID_HASH, HI.CloudFileIdentity.hash);
		Assert.AreEqual(Int64(99999), HI.CloudFileIdentity.size);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestNameDefaultsToHash;
var
	HI: THashInfo;
begin
	{ Without name part, name should default to hash value }
	HI := THashInfo.Create(VALID_HASH + ':500');
	try
		Assert.IsTrue(HI.valid);
		Assert.AreEqual(HI.hash, HI.name);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestDoCleanFalse;
var
	HI: THashInfo;
begin
	{ With doClean=false, 'hash ' prefix should NOT be stripped }
	HI := THashInfo.Create('hash ' + VALID_HASH + ':1000', false);
	try
		{ Should fail because 'hash ' is not stripped }
		Assert.IsFalse(HI.valid);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestValidWithInlineComment;
var
	HI: THashInfo;
begin
	{Inline # comment after closing quote should be ignored}
	HI := THashInfo.Create('hash "' + VALID_HASH + ':2000:image.jpg" #uploaded 2024-01-15');
	try
		Assert.IsTrue(HI.valid);
		Assert.AreEqual(VALID_HASH, HI.hash);
		Assert.AreEqual(Int64(2000), HI.size);
		Assert.AreEqual('image.jpg', HI.name);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestValidWithInlineCommentNoSpace;
var
	HI: THashInfo;
begin
	{Comment immediately after closing quote (no space) should also work}
	HI := THashInfo.Create('hash "' + VALID_HASH + ':500:doc.pdf"#my note');
	try
		Assert.IsTrue(HI.valid);
		Assert.AreEqual(VALID_HASH, HI.hash);
		Assert.AreEqual(Int64(500), HI.size);
		Assert.AreEqual('doc.pdf', HI.name);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestValidWithInlineCommentPreservesName;
var
	HI: THashInfo;
begin
	{Filename containing # inside quotes should be preserved as-is}
	HI := THashInfo.Create('hash "' + VALID_HASH + ':100:file#1.txt"');
	try
		Assert.IsTrue(HI.valid);
		Assert.AreEqual('file#1.txt', HI.name, 'Hash inside quotes is part of filename');
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestCommentOnlyLine;
var
	HI: THashInfo;
begin
	{A line starting with # has no valid hash data}
	HI := THashInfo.Create('# this is a block comment');
	try
		Assert.IsFalse(HI.valid);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestEmptyLine;
var
	HI: THashInfo;
begin
	{An empty string has no valid hash data}
	HI := THashInfo.Create('');
	try
		Assert.IsFalse(HI.valid);
	finally
		HI.Free;
	end;
end;

procedure THashInfoTest.TestCopyWithZeroPosition_WorksCorrectly;
var
	HI: THashInfo;
begin
	{This test verifies that Copy(str, 0, n) works in Delphi with zero-based strings.
	 HashInfo.pas uses Copy(parameter, 0, divisor_position - 1) which relies on this.
	 If this test passes, the zero-based Copy usage is correct for this Delphi version.
	 Delphi supports ZEROBASEDSTRINGS compiler directive.}
	HI := THashInfo.Create(VALID_HASH + ':12345:testfile.txt');
	try
		Assert.IsTrue(HI.valid, 'HashInfo should parse successfully');
		Assert.AreEqual(VALID_HASH, HI.hash, 'Hash should be extracted correctly via Copy(str, 0, n)');
		Assert.AreEqual(Int64(12345), HI.size, 'Size should be extracted correctly');
		Assert.AreEqual('testfile.txt', HI.name, 'Name should be extracted correctly');
	finally
		HI.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(THashInfoTest);

end.
