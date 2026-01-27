unit CloudAccessMapperTest;

{Tests for TCloudAccessMapper - access level conversion utilities}

interface

uses
	CloudAccessMapper,
	CloudConstants,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCloudAccessMapperTest = class
	public
		{AccessToString tests - input normalization}
		[Test]
		procedure TestAccessToString_ReadOnlyHuman_NoInvert;
		[Test]
		procedure TestAccessToString_ReadWriteHuman_NoInvert;
		[Test]
		procedure TestAccessToString_ReadOnlyConstant_NoInvert;
		[Test]
		procedure TestAccessToString_ReadWriteConstant_NoInvert;

		{AccessToString tests - invert behavior}
		[Test]
		procedure TestAccessToString_ReadOnlyHuman_Invert;
		[Test]
		procedure TestAccessToString_ReadWriteHuman_Invert;
		[Test]
		procedure TestAccessToString_ReadOnlyConstant_Invert;
		[Test]
		procedure TestAccessToString_ReadWriteConstant_Invert;

		{AccessToString edge cases}
		[Test]
		procedure TestAccessToString_EmptyString_NoInvert;
		[Test]
		procedure TestAccessToString_EmptyString_Invert;
		[Test]
		procedure TestAccessToString_UnknownString_NoInvert;
		[Test]
		procedure TestAccessToString_UnknownString_Invert;

		{StringToAccess tests - input normalization}
		[Test]
		procedure TestStringToAccess_ReadOnlyHuman_NoInvert;
		[Test]
		procedure TestStringToAccess_ReadWriteHuman_NoInvert;
		[Test]
		procedure TestStringToAccess_ReadOnlyConstant_NoInvert;
		[Test]
		procedure TestStringToAccess_ReadWriteConstant_NoInvert;

		{StringToAccess tests - invert behavior}
		[Test]
		procedure TestStringToAccess_ReadOnlyHuman_Invert;
		[Test]
		procedure TestStringToAccess_ReadWriteHuman_Invert;
		[Test]
		procedure TestStringToAccess_ReadOnlyConstant_Invert;
		[Test]
		procedure TestStringToAccess_ReadWriteConstant_Invert;

		{StringToAccess edge cases}
		[Test]
		procedure TestStringToAccess_EmptyString_NoInvert;
		[Test]
		procedure TestStringToAccess_EmptyString_Invert;
		[Test]
		procedure TestStringToAccess_UnknownString_NoInvert;
		[Test]
		procedure TestStringToAccess_UnknownString_Invert;

		{Symmetry tests}
		[Test]
		procedure TestSymmetry_ReadOnly;
		[Test]
		procedure TestSymmetry_ReadWrite;
	end;

implementation

{AccessToString tests - input normalization without Invert}

procedure TCloudAccessMapperTest.TestAccessToString_ReadOnlyHuman_NoInvert;
begin
	Assert.AreEqual('read only', TCloudAccessMapper.AccessToString('read only', False));
end;

procedure TCloudAccessMapperTest.TestAccessToString_ReadWriteHuman_NoInvert;
begin
	Assert.AreEqual('read and write', TCloudAccessMapper.AccessToString('read and write', False));
end;

procedure TCloudAccessMapperTest.TestAccessToString_ReadOnlyConstant_NoInvert;
begin
	Assert.AreEqual('read only', TCloudAccessMapper.AccessToString(CLOUD_SHARE_ACCESS_READ_ONLY, False));
end;

procedure TCloudAccessMapperTest.TestAccessToString_ReadWriteConstant_NoInvert;
begin
	Assert.AreEqual('read and write', TCloudAccessMapper.AccessToString(CLOUD_SHARE_ACCESS_READ_WRITE, False));
end;

{AccessToString tests - invert behavior}

procedure TCloudAccessMapperTest.TestAccessToString_ReadOnlyHuman_Invert;
begin
	Assert.AreEqual('read and write', TCloudAccessMapper.AccessToString('read only', True));
end;

procedure TCloudAccessMapperTest.TestAccessToString_ReadWriteHuman_Invert;
begin
	Assert.AreEqual('read only', TCloudAccessMapper.AccessToString('read and write', True));
end;

procedure TCloudAccessMapperTest.TestAccessToString_ReadOnlyConstant_Invert;
begin
	Assert.AreEqual('read and write', TCloudAccessMapper.AccessToString(CLOUD_SHARE_ACCESS_READ_ONLY, True));
end;

procedure TCloudAccessMapperTest.TestAccessToString_ReadWriteConstant_Invert;
begin
	Assert.AreEqual('read only', TCloudAccessMapper.AccessToString(CLOUD_SHARE_ACCESS_READ_WRITE, True));
end;

{AccessToString edge cases}

procedure TCloudAccessMapperTest.TestAccessToString_EmptyString_NoInvert;
begin
	{Empty/unknown defaults to read-write}
	Assert.AreEqual('read and write', TCloudAccessMapper.AccessToString('', False));
end;

procedure TCloudAccessMapperTest.TestAccessToString_EmptyString_Invert;
begin
	{Empty/unknown defaults to read-write, inverted = read-only}
	Assert.AreEqual('read only', TCloudAccessMapper.AccessToString('', True));
end;

procedure TCloudAccessMapperTest.TestAccessToString_UnknownString_NoInvert;
begin
	{Unknown defaults to read-write}
	Assert.AreEqual('read and write', TCloudAccessMapper.AccessToString('garbage', False));
end;

procedure TCloudAccessMapperTest.TestAccessToString_UnknownString_Invert;
begin
	{Unknown defaults to read-write, inverted = read-only}
	Assert.AreEqual('read only', TCloudAccessMapper.AccessToString('garbage', True));
end;

{StringToAccess tests - input normalization without Invert}

procedure TCloudAccessMapperTest.TestStringToAccess_ReadOnlyHuman_NoInvert;
begin
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudAccessMapper.StringToAccess('read only', False));
end;

procedure TCloudAccessMapperTest.TestStringToAccess_ReadWriteHuman_NoInvert;
begin
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudAccessMapper.StringToAccess('read and write', False));
end;

procedure TCloudAccessMapperTest.TestStringToAccess_ReadOnlyConstant_NoInvert;
begin
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudAccessMapper.StringToAccess(CLOUD_SHARE_ACCESS_READ_ONLY, False));
end;

procedure TCloudAccessMapperTest.TestStringToAccess_ReadWriteConstant_NoInvert;
begin
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudAccessMapper.StringToAccess(CLOUD_SHARE_ACCESS_READ_WRITE, False));
end;

{StringToAccess tests - invert behavior}

procedure TCloudAccessMapperTest.TestStringToAccess_ReadOnlyHuman_Invert;
begin
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudAccessMapper.StringToAccess('read only', True));
end;

procedure TCloudAccessMapperTest.TestStringToAccess_ReadWriteHuman_Invert;
begin
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudAccessMapper.StringToAccess('read and write', True));
end;

procedure TCloudAccessMapperTest.TestStringToAccess_ReadOnlyConstant_Invert;
begin
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudAccessMapper.StringToAccess(CLOUD_SHARE_ACCESS_READ_ONLY, True));
end;

procedure TCloudAccessMapperTest.TestStringToAccess_ReadWriteConstant_Invert;
begin
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudAccessMapper.StringToAccess(CLOUD_SHARE_ACCESS_READ_WRITE, True));
end;

{StringToAccess edge cases}

procedure TCloudAccessMapperTest.TestStringToAccess_EmptyString_NoInvert;
begin
	{Empty/unknown defaults to read-write}
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudAccessMapper.StringToAccess('', False));
end;

procedure TCloudAccessMapperTest.TestStringToAccess_EmptyString_Invert;
begin
	{Empty/unknown defaults to read-write, inverted = read-only}
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudAccessMapper.StringToAccess('', True));
end;

procedure TCloudAccessMapperTest.TestStringToAccess_UnknownString_NoInvert;
begin
	{Unknown defaults to read-write}
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudAccessMapper.StringToAccess('garbage', False));
end;

procedure TCloudAccessMapperTest.TestStringToAccess_UnknownString_Invert;
begin
	{Unknown defaults to read-write, inverted = read-only}
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudAccessMapper.StringToAccess('garbage', True));
end;

{Symmetry tests - verify AccessToString and StringToAccess are consistent}

procedure TCloudAccessMapperTest.TestSymmetry_ReadOnly;
var
	StringResult: WideString;
	IntResult: Integer;
begin
	StringResult := TCloudAccessMapper.AccessToString(CLOUD_SHARE_ACCESS_READ_ONLY, False);
	IntResult := TCloudAccessMapper.StringToAccess(StringResult, False);
	Assert.AreEqual(CLOUD_SHARE_RO, IntResult, 'Read-only should round-trip correctly');
end;

procedure TCloudAccessMapperTest.TestSymmetry_ReadWrite;
var
	StringResult: WideString;
	IntResult: Integer;
begin
	StringResult := TCloudAccessMapper.AccessToString(CLOUD_SHARE_ACCESS_READ_WRITE, False);
	IntResult := TCloudAccessMapper.StringToAccess(StringResult, False);
	Assert.AreEqual(CLOUD_SHARE_RW, IntResult, 'Read-write should round-trip correctly');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudAccessMapperTest);

end.
