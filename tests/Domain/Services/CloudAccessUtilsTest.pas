unit CloudAccessUtilsTest;

{Tests for TCloudAccessUtils - access level conversion utilities}

interface

uses
	CloudAccessUtils,
	CMRConstants,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCloudAccessUtilsTest = class
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

procedure TCloudAccessUtilsTest.TestAccessToString_ReadOnlyHuman_NoInvert;
begin
	Assert.AreEqual('read only', TCloudAccessUtils.AccessToString('read only', False));
end;

procedure TCloudAccessUtilsTest.TestAccessToString_ReadWriteHuman_NoInvert;
begin
	Assert.AreEqual('read and write', TCloudAccessUtils.AccessToString('read and write', False));
end;

procedure TCloudAccessUtilsTest.TestAccessToString_ReadOnlyConstant_NoInvert;
begin
	Assert.AreEqual('read only', TCloudAccessUtils.AccessToString(CLOUD_SHARE_ACCESS_READ_ONLY, False));
end;

procedure TCloudAccessUtilsTest.TestAccessToString_ReadWriteConstant_NoInvert;
begin
	Assert.AreEqual('read and write', TCloudAccessUtils.AccessToString(CLOUD_SHARE_ACCESS_READ_WRITE, False));
end;

{AccessToString tests - invert behavior}

procedure TCloudAccessUtilsTest.TestAccessToString_ReadOnlyHuman_Invert;
begin
	Assert.AreEqual('read and write', TCloudAccessUtils.AccessToString('read only', True));
end;

procedure TCloudAccessUtilsTest.TestAccessToString_ReadWriteHuman_Invert;
begin
	Assert.AreEqual('read only', TCloudAccessUtils.AccessToString('read and write', True));
end;

procedure TCloudAccessUtilsTest.TestAccessToString_ReadOnlyConstant_Invert;
begin
	Assert.AreEqual('read and write', TCloudAccessUtils.AccessToString(CLOUD_SHARE_ACCESS_READ_ONLY, True));
end;

procedure TCloudAccessUtilsTest.TestAccessToString_ReadWriteConstant_Invert;
begin
	Assert.AreEqual('read only', TCloudAccessUtils.AccessToString(CLOUD_SHARE_ACCESS_READ_WRITE, True));
end;

{AccessToString edge cases}

procedure TCloudAccessUtilsTest.TestAccessToString_EmptyString_NoInvert;
begin
	{Empty/unknown defaults to read-write}
	Assert.AreEqual('read and write', TCloudAccessUtils.AccessToString('', False));
end;

procedure TCloudAccessUtilsTest.TestAccessToString_EmptyString_Invert;
begin
	{Empty/unknown defaults to read-write, inverted = read-only}
	Assert.AreEqual('read only', TCloudAccessUtils.AccessToString('', True));
end;

procedure TCloudAccessUtilsTest.TestAccessToString_UnknownString_NoInvert;
begin
	{Unknown defaults to read-write}
	Assert.AreEqual('read and write', TCloudAccessUtils.AccessToString('garbage', False));
end;

procedure TCloudAccessUtilsTest.TestAccessToString_UnknownString_Invert;
begin
	{Unknown defaults to read-write, inverted = read-only}
	Assert.AreEqual('read only', TCloudAccessUtils.AccessToString('garbage', True));
end;

{StringToAccess tests - input normalization without Invert}

procedure TCloudAccessUtilsTest.TestStringToAccess_ReadOnlyHuman_NoInvert;
begin
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudAccessUtils.StringToAccess('read only', False));
end;

procedure TCloudAccessUtilsTest.TestStringToAccess_ReadWriteHuman_NoInvert;
begin
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudAccessUtils.StringToAccess('read and write', False));
end;

procedure TCloudAccessUtilsTest.TestStringToAccess_ReadOnlyConstant_NoInvert;
begin
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudAccessUtils.StringToAccess(CLOUD_SHARE_ACCESS_READ_ONLY, False));
end;

procedure TCloudAccessUtilsTest.TestStringToAccess_ReadWriteConstant_NoInvert;
begin
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudAccessUtils.StringToAccess(CLOUD_SHARE_ACCESS_READ_WRITE, False));
end;

{StringToAccess tests - invert behavior}

procedure TCloudAccessUtilsTest.TestStringToAccess_ReadOnlyHuman_Invert;
begin
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudAccessUtils.StringToAccess('read only', True));
end;

procedure TCloudAccessUtilsTest.TestStringToAccess_ReadWriteHuman_Invert;
begin
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudAccessUtils.StringToAccess('read and write', True));
end;

procedure TCloudAccessUtilsTest.TestStringToAccess_ReadOnlyConstant_Invert;
begin
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudAccessUtils.StringToAccess(CLOUD_SHARE_ACCESS_READ_ONLY, True));
end;

procedure TCloudAccessUtilsTest.TestStringToAccess_ReadWriteConstant_Invert;
begin
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudAccessUtils.StringToAccess(CLOUD_SHARE_ACCESS_READ_WRITE, True));
end;

{StringToAccess edge cases}

procedure TCloudAccessUtilsTest.TestStringToAccess_EmptyString_NoInvert;
begin
	{Empty/unknown defaults to read-write}
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudAccessUtils.StringToAccess('', False));
end;

procedure TCloudAccessUtilsTest.TestStringToAccess_EmptyString_Invert;
begin
	{Empty/unknown defaults to read-write, inverted = read-only}
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudAccessUtils.StringToAccess('', True));
end;

procedure TCloudAccessUtilsTest.TestStringToAccess_UnknownString_NoInvert;
begin
	{Unknown defaults to read-write}
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudAccessUtils.StringToAccess('garbage', False));
end;

procedure TCloudAccessUtilsTest.TestStringToAccess_UnknownString_Invert;
begin
	{Unknown defaults to read-write, inverted = read-only}
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudAccessUtils.StringToAccess('garbage', True));
end;

{Symmetry tests - verify AccessToString and StringToAccess are consistent}

procedure TCloudAccessUtilsTest.TestSymmetry_ReadOnly;
var
	StringResult: WideString;
	IntResult: Integer;
begin
	StringResult := TCloudAccessUtils.AccessToString(CLOUD_SHARE_ACCESS_READ_ONLY, False);
	IntResult := TCloudAccessUtils.StringToAccess(StringResult, False);
	Assert.AreEqual(CLOUD_SHARE_RO, IntResult, 'Read-only should round-trip correctly');
end;

procedure TCloudAccessUtilsTest.TestSymmetry_ReadWrite;
var
	StringResult: WideString;
	IntResult: Integer;
begin
	StringResult := TCloudAccessUtils.AccessToString(CLOUD_SHARE_ACCESS_READ_WRITE, False);
	IntResult := TCloudAccessUtils.StringToAccess(StringResult, False);
	Assert.AreEqual(CLOUD_SHARE_RW, IntResult, 'Read-write should round-trip correctly');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudAccessUtilsTest);

end.
