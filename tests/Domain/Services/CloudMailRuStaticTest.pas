unit CloudMailRuStaticTest;

interface

uses
	CloudMailRu,
	CloudMailRuFactory,
	CMRConstants,
	LANGUAGE_STRINGS,
	SysUtils,
	DUnitX.TestFramework;

type
	{ Tests for TCloudMailRu static class functions.
	  Tests wrappers that delegate to extracted utility classes.
	  ErrorCodeText tests moved to CloudErrorMapperTest. }
	[TestFixture]
	TCloudMailRuStaticTest = class
	public
		{ CloudAccessToString tests - input normalization }
		[Test]
		procedure TestCloudAccessToString_ReadOnlyHuman_NoInvert;
		[Test]
		procedure TestCloudAccessToString_ReadWriteHuman_NoInvert;
		[Test]
		procedure TestCloudAccessToString_ReadOnlyConstant_NoInvert;
		[Test]
		procedure TestCloudAccessToString_ReadWriteConstant_NoInvert;

		{ CloudAccessToString tests - invert behavior }
		[Test]
		procedure TestCloudAccessToString_ReadOnlyHuman_Invert;
		[Test]
		procedure TestCloudAccessToString_ReadWriteHuman_Invert;
		[Test]
		procedure TestCloudAccessToString_ReadOnlyConstant_Invert;
		[Test]
		procedure TestCloudAccessToString_ReadWriteConstant_Invert;

		{ CloudAccessToString edge cases }
		[Test]
		procedure TestCloudAccessToString_EmptyString_NoInvert;
		[Test]
		procedure TestCloudAccessToString_EmptyString_Invert;
		[Test]
		procedure TestCloudAccessToString_UnknownString_NoInvert;
		[Test]
		procedure TestCloudAccessToString_UnknownString_Invert;

		{ StringToCloudAccess tests - input normalization }
		[Test]
		procedure TestStringToCloudAccess_ReadOnlyHuman_NoInvert;
		[Test]
		procedure TestStringToCloudAccess_ReadWriteHuman_NoInvert;
		[Test]
		procedure TestStringToCloudAccess_ReadOnlyConstant_NoInvert;
		[Test]
		procedure TestStringToCloudAccess_ReadWriteConstant_NoInvert;

		{ StringToCloudAccess tests - invert behavior }
		[Test]
		procedure TestStringToCloudAccess_ReadOnlyHuman_Invert;
		[Test]
		procedure TestStringToCloudAccess_ReadWriteHuman_Invert;
		[Test]
		procedure TestStringToCloudAccess_ReadOnlyConstant_Invert;
		[Test]
		procedure TestStringToCloudAccess_ReadWriteConstant_Invert;

		{ StringToCloudAccess edge cases }
		[Test]
		procedure TestStringToCloudAccess_EmptyString_NoInvert;
		[Test]
		procedure TestStringToCloudAccess_EmptyString_Invert;
		[Test]
		procedure TestStringToCloudAccess_UnknownString_NoInvert;
		[Test]
		procedure TestStringToCloudAccess_UnknownString_Invert;

		{ Symmetry tests - verify CloudAccessToString and StringToCloudAccess are consistent }
		[Test]
		procedure TestAccessConversion_RoundTrip_ReadOnly;
		[Test]
		procedure TestAccessConversion_RoundTrip_ReadWrite;

		{ TempPublicCloudInit tests }
		[Test]
		procedure TestTempPublicCloudInit_CreatesCloudInstance;
		[Test]
		procedure TestTempPublicCloudInit_SetsPublicAccountFlag;
		[Test]
		procedure TestTempPublicCloudInit_CloudMustBeFreedByCaller;
	end;

implementation

{ CloudAccessToString tests - input normalization without Invert }

procedure TCloudMailRuStaticTest.TestCloudAccessToString_ReadOnlyHuman_NoInvert;
begin
	{ Human-readable 'read only' input, no invert -> 'read only' output }
	Assert.AreEqual('read only', TCloudMailRu.CloudAccessToString('read only', False));
end;

procedure TCloudMailRuStaticTest.TestCloudAccessToString_ReadWriteHuman_NoInvert;
begin
	{ Human-readable 'read and write' input, no invert -> 'read and write' output }
	Assert.AreEqual('read and write', TCloudMailRu.CloudAccessToString('read and write', False));
end;

procedure TCloudMailRuStaticTest.TestCloudAccessToString_ReadOnlyConstant_NoInvert;
begin
	{ API constant 'read_only' input, no invert -> 'read only' output }
	Assert.AreEqual('read only', TCloudMailRu.CloudAccessToString(CLOUD_SHARE_ACCESS_READ_ONLY, False));
end;

procedure TCloudMailRuStaticTest.TestCloudAccessToString_ReadWriteConstant_NoInvert;
begin
	{ API constant 'read_write' input, no invert -> 'read and write' output }
	Assert.AreEqual('read and write', TCloudMailRu.CloudAccessToString(CLOUD_SHARE_ACCESS_READ_WRITE, False));
end;

{ CloudAccessToString tests - invert behavior }

procedure TCloudMailRuStaticTest.TestCloudAccessToString_ReadOnlyHuman_Invert;
begin
	{ Human-readable 'read only' input, invert -> 'read and write' output }
	Assert.AreEqual('read and write', TCloudMailRu.CloudAccessToString('read only', True));
end;

procedure TCloudMailRuStaticTest.TestCloudAccessToString_ReadWriteHuman_Invert;
begin
	{ Human-readable 'read and write' input, invert -> 'read only' output }
	Assert.AreEqual('read only', TCloudMailRu.CloudAccessToString('read and write', True));
end;

procedure TCloudMailRuStaticTest.TestCloudAccessToString_ReadOnlyConstant_Invert;
begin
	{ API constant 'read_only' input, invert -> 'read and write' output }
	Assert.AreEqual('read and write', TCloudMailRu.CloudAccessToString(CLOUD_SHARE_ACCESS_READ_ONLY, True));
end;

procedure TCloudMailRuStaticTest.TestCloudAccessToString_ReadWriteConstant_Invert;
begin
	{ API constant 'read_write' input, invert -> 'read only' output }
	Assert.AreEqual('read only', TCloudMailRu.CloudAccessToString(CLOUD_SHARE_ACCESS_READ_WRITE, True));
end;

{ CloudAccessToString edge cases }

procedure TCloudMailRuStaticTest.TestCloudAccessToString_EmptyString_NoInvert;
begin
	{ Empty string is not 'read_only', so defaults to 'read and write' }
	Assert.AreEqual('read and write', TCloudMailRu.CloudAccessToString('', False));
end;

procedure TCloudMailRuStaticTest.TestCloudAccessToString_EmptyString_Invert;
begin
	{ Empty string defaults to 'read and write', invert -> 'read only' }
	Assert.AreEqual('read only', TCloudMailRu.CloudAccessToString('', True));
end;

procedure TCloudMailRuStaticTest.TestCloudAccessToString_UnknownString_NoInvert;
begin
	{ Unknown string is not 'read_only', so defaults to 'read and write' }
	Assert.AreEqual('read and write', TCloudMailRu.CloudAccessToString('garbage', False));
end;

procedure TCloudMailRuStaticTest.TestCloudAccessToString_UnknownString_Invert;
begin
	{ Unknown string defaults to 'read and write', invert -> 'read only' }
	Assert.AreEqual('read only', TCloudMailRu.CloudAccessToString('garbage', True));
end;

{ StringToCloudAccess tests - input normalization without Invert }

procedure TCloudMailRuStaticTest.TestStringToCloudAccess_ReadOnlyHuman_NoInvert;
begin
	{ Human-readable 'read only' input, no invert -> CLOUD_SHARE_RO }
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudMailRu.StringToCloudAccess('read only', False));
end;

procedure TCloudMailRuStaticTest.TestStringToCloudAccess_ReadWriteHuman_NoInvert;
begin
	{ Human-readable 'read and write' input, no invert -> CLOUD_SHARE_RW }
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudMailRu.StringToCloudAccess('read and write', False));
end;

procedure TCloudMailRuStaticTest.TestStringToCloudAccess_ReadOnlyConstant_NoInvert;
begin
	{ API constant 'read_only' input, no invert -> CLOUD_SHARE_RO }
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudMailRu.StringToCloudAccess(CLOUD_SHARE_ACCESS_READ_ONLY, False));
end;

procedure TCloudMailRuStaticTest.TestStringToCloudAccess_ReadWriteConstant_NoInvert;
begin
	{ API constant 'read_write' input, no invert -> CLOUD_SHARE_RW }
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudMailRu.StringToCloudAccess(CLOUD_SHARE_ACCESS_READ_WRITE, False));
end;

{ StringToCloudAccess tests - invert behavior }

procedure TCloudMailRuStaticTest.TestStringToCloudAccess_ReadOnlyHuman_Invert;
begin
	{ Human-readable 'read only' input, invert -> CLOUD_SHARE_RW }
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudMailRu.StringToCloudAccess('read only', True));
end;

procedure TCloudMailRuStaticTest.TestStringToCloudAccess_ReadWriteHuman_Invert;
begin
	{ Human-readable 'read and write' input, invert -> CLOUD_SHARE_RO }
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudMailRu.StringToCloudAccess('read and write', True));
end;

procedure TCloudMailRuStaticTest.TestStringToCloudAccess_ReadOnlyConstant_Invert;
begin
	{ API constant 'read_only' input, invert -> CLOUD_SHARE_RW }
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudMailRu.StringToCloudAccess(CLOUD_SHARE_ACCESS_READ_ONLY, True));
end;

procedure TCloudMailRuStaticTest.TestStringToCloudAccess_ReadWriteConstant_Invert;
begin
	{ API constant 'read_write' input, invert -> CLOUD_SHARE_RO }
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudMailRu.StringToCloudAccess(CLOUD_SHARE_ACCESS_READ_WRITE, True));
end;

{ StringToCloudAccess edge cases }

procedure TCloudMailRuStaticTest.TestStringToCloudAccess_EmptyString_NoInvert;
begin
	{ Empty string is not 'read_only', so defaults to CLOUD_SHARE_RW }
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudMailRu.StringToCloudAccess('', False));
end;

procedure TCloudMailRuStaticTest.TestStringToCloudAccess_EmptyString_Invert;
begin
	{ Empty string defaults to RW, invert -> CLOUD_SHARE_RO }
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudMailRu.StringToCloudAccess('', True));
end;

procedure TCloudMailRuStaticTest.TestStringToCloudAccess_UnknownString_NoInvert;
begin
	{ Unknown string is not 'read_only', so defaults to CLOUD_SHARE_RW }
	Assert.AreEqual(CLOUD_SHARE_RW, TCloudMailRu.StringToCloudAccess('garbage', False));
end;

procedure TCloudMailRuStaticTest.TestStringToCloudAccess_UnknownString_Invert;
begin
	{ Unknown string defaults to RW, invert -> CLOUD_SHARE_RO }
	Assert.AreEqual(CLOUD_SHARE_RO, TCloudMailRu.StringToCloudAccess('garbage', True));
end;

{ Symmetry tests - verify conversion functions are consistent }

procedure TCloudMailRuStaticTest.TestAccessConversion_RoundTrip_ReadOnly;
var
	StringResult: WideString;
	IntResult: Integer;
begin
	{ Verify that CloudAccessToString and StringToCloudAccess are consistent for read-only.
	  CloudAccessToString('read_only') -> 'read only'
	  StringToCloudAccess('read only') -> CLOUD_SHARE_RO (1) }
	StringResult := TCloudMailRu.CloudAccessToString(CLOUD_SHARE_ACCESS_READ_ONLY, False);
	IntResult := TCloudMailRu.StringToCloudAccess(StringResult, False);
	Assert.AreEqual(CLOUD_SHARE_RO, IntResult, 'Round-trip conversion should preserve read-only semantics');
end;

procedure TCloudMailRuStaticTest.TestAccessConversion_RoundTrip_ReadWrite;
var
	StringResult: WideString;
	IntResult: Integer;
begin
	{ Verify that CloudAccessToString and StringToCloudAccess are consistent for read-write.
	  CloudAccessToString('read_write') -> 'read and write'
	  StringToCloudAccess('read and write') -> CLOUD_SHARE_RW (0) }
	StringResult := TCloudMailRu.CloudAccessToString(CLOUD_SHARE_ACCESS_READ_WRITE, False);
	IntResult := TCloudMailRu.StringToCloudAccess(StringResult, False);
	Assert.AreEqual(CLOUD_SHARE_RW, IntResult, 'Round-trip conversion should preserve read-write semantics');
end;

{ TempPublicCloudInit tests }

procedure TCloudMailRuStaticTest.TestTempPublicCloudInit_CreatesCloudInstance;
var
	TempCloud: TCloudMailRu;
begin
	{ CreatePublicCloud creates a TCloudMailRu instance via the factory.
	  Note: Login will fail without network, but instance should still be created.
	  We don't test Login result since it requires network. }
	TempCloud := nil;
	try
		TCloudMailRuFactory.CreatePublicCloud(TempCloud, 'https://cloud.mail.ru/public/test123');
		Assert.IsNotNull(TempCloud, 'CreatePublicCloud should create a cloud instance');
	finally
		TempCloud.Free;
	end;
end;

procedure TCloudMailRuStaticTest.TestTempPublicCloudInit_SetsPublicAccountFlag;
var
	TempCloud: TCloudMailRu;
begin
	{ Verify the created instance is configured as public account }
	TempCloud := nil;
	try
		TCloudMailRuFactory.CreatePublicCloud(TempCloud, 'https://cloud.mail.ru/public/abc123');
		Assert.IsTrue(TempCloud.IsPublicAccount, 'Created cloud should be marked as public account');
	finally
		TempCloud.Free;
	end;
end;

procedure TCloudMailRuStaticTest.TestTempPublicCloudInit_CloudMustBeFreedByCaller;
var
	TempCloud: TCloudMailRu;
begin
	{ Verify that after CreatePublicCloud, the caller owns the instance and must free it.
	  This test ensures no memory leaks by properly freeing. FastMM will catch leaks. }
	TempCloud := nil;
	TCloudMailRuFactory.CreatePublicCloud(TempCloud, 'https://cloud.mail.ru/public/xyz789');
	Assert.IsNotNull(TempCloud, 'Instance should be created');
	TempCloud.Free; { Caller is responsible for freeing }
	Assert.Pass('Instance freed successfully - caller owns the instance');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudMailRuStaticTest);

end.
