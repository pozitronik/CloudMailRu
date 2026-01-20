unit CloudMailRuStaticTest;

interface

uses
	CloudMailRu,
	CMRConstants,
	LANGUAGE_STRINGS,
	SysUtils,
	DUnitX.TestFramework;

type
	{ Tests for TCloudMailRu static class functions.
	  These are pure functions with no dependencies - ideal for unit testing. }
	[TestFixture]
	TCloudMailRuStaticTest = class
	public
		{ ErrorCodeText tests - all 18 known error codes }
		[Test]
		procedure TestErrorCodeText_Exists;
		[Test]
		procedure TestErrorCodeText_Required;
		[Test]
		procedure TestErrorCodeText_Invalid;
		[Test]
		procedure TestErrorCodeText_ReadOnly;
		[Test]
		procedure TestErrorCodeText_NameLengthExceeded;
		[Test]
		procedure TestErrorCodeText_Overquota;
		[Test]
		procedure TestErrorCodeText_NotExists;
		[Test]
		procedure TestErrorCodeText_Own;
		[Test]
		procedure TestErrorCodeText_NameTooLong;
		[Test]
		procedure TestErrorCodeText_VirusScanFail;
		[Test]
		procedure TestErrorCodeText_Owner;
		[Test]
		procedure TestErrorCodeText_Fahrenheit;
		[Test]
		procedure TestErrorCodeText_BadRequest;
		[Test]
		procedure TestErrorCodeText_TreesConflict;
		[Test]
		procedure TestErrorCodeText_UnprocessableEntry;
		[Test]
		procedure TestErrorCodeText_UserLimitExceeded;
		[Test]
		procedure TestErrorCodeText_ExportLimitExceeded;
		[Test]
		procedure TestErrorCodeText_NotAcceptable;

		{ ErrorCodeText edge cases - unknown error codes }
		[Test]
		procedure TestErrorCodeText_UnknownPositive;
		[Test]
		procedure TestErrorCodeText_UnknownNegative;
		[Test]
		procedure TestErrorCodeText_Zero;
		[Test]
		procedure TestErrorCodeText_MaxInt;

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

{ ErrorCodeText tests - known error codes }

procedure TCloudMailRuStaticTest.TestErrorCodeText_Exists;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_EXISTS, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_EXISTS));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_Required;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_REQUIRED, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_REQUIRED));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_Invalid;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_INVALID, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_INVALID));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_ReadOnly;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_READONLY, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_READONLY));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_NameLengthExceeded;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_NAME_LENGTH_EXCEEDED, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_NAME_LENGTH_EXCEEDED));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_Overquota;
begin
	{ Note: CLOUD_ERROR_OVERQUOTA = 7, same as CLOUD_ERROR_QUOTA_EXCEEDED }
	Assert.AreEqual(ERR_CLOUD_ERROR_OVERQUOTA, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_OVERQUOTA));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_NotExists;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_NOT_EXISTS, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_NOT_EXISTS));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_Own;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_OWN, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_OWN));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_NameTooLong;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_NAME_TOO_LONG, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_NAME_TOO_LONG));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_VirusScanFail;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_VIRUS_SCAN_FAIL, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_VIRUS_SCAN_FAIL));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_Owner;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_OWNER, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_OWNER));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_Fahrenheit;
begin
	{ Code 451 - content blocked by rights holder }
	Assert.AreEqual(ERR_CLOUD_ERROR_FAHRENHEIT, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_FAHRENHEIT));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_BadRequest;
begin
	{ HTTP 400 error }
	Assert.AreEqual(ERR_CLOUD_ERROR_BAD_REQUEST, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_BAD_REQUEST));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_TreesConflict;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_TREES_CONFLICT, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_TREES_CONFLICT));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_UnprocessableEntry;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_UNPROCESSABLE_ENTRY, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_UNPROCESSABLE_ENTRY));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_UserLimitExceeded;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_USER_LIMIT_EXCEEDED, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_USER_LIMIT_EXCEEDED));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_ExportLimitExceeded;
begin
	Assert.AreEqual(ERR_CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_NotAcceptable;
begin
	{ HTTP 406 error }
	Assert.AreEqual(ERR_CLOUD_ERROR_NOT_ACCEPTABLE, TCloudMailRu.ErrorCodeText(CLOUD_ERROR_NOT_ACCEPTABLE));
end;

{ ErrorCodeText edge cases }

procedure TCloudMailRuStaticTest.TestErrorCodeText_UnknownPositive;
begin
	{ Unknown error code 999 should format with ERR_CLOUD_ERROR_UNKNOWN }
	Assert.AreEqual(Format(ERR_CLOUD_ERROR_UNKNOWN, [999]), TCloudMailRu.ErrorCodeText(999));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_UnknownNegative;
begin
	{ Negative error codes (except internal ones) should be unknown }
	Assert.AreEqual(Format(ERR_CLOUD_ERROR_UNKNOWN, [-999]), TCloudMailRu.ErrorCodeText(-999));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_Zero;
begin
	{ Zero is CLOUD_OPERATION_OK, not in ErrorCodeText case - should be unknown }
	Assert.AreEqual(Format(ERR_CLOUD_ERROR_UNKNOWN, [0]), TCloudMailRu.ErrorCodeText(0));
end;

procedure TCloudMailRuStaticTest.TestErrorCodeText_MaxInt;
begin
	{ Boundary test with MaxInt }
	Assert.AreEqual(Format(ERR_CLOUD_ERROR_UNKNOWN, [MaxInt]), TCloudMailRu.ErrorCodeText(MaxInt));
end;

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
	{ TempPublicCloudInit creates a TCloudMailRu instance.
	  Note: Login will fail without network, but instance should still be created.
	  We don't test Login result since it requires network. }
	TempCloud := nil;
	try
		TCloudMailRu.TempPublicCloudInit(TempCloud, 'https://cloud.mail.ru/public/test123');
		Assert.IsNotNull(TempCloud, 'TempPublicCloudInit should create a cloud instance');
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
		TCloudMailRu.TempPublicCloudInit(TempCloud, 'https://cloud.mail.ru/public/abc123');
		Assert.IsTrue(TempCloud.IsPublicAccount, 'Created cloud should be marked as public account');
	finally
		TempCloud.Free;
	end;
end;

procedure TCloudMailRuStaticTest.TestTempPublicCloudInit_CloudMustBeFreedByCaller;
var
	TempCloud: TCloudMailRu;
begin
	{ Verify that after TempPublicCloudInit, the caller owns the instance and must free it.
	  This test ensures no memory leaks by properly freeing. FastMM will catch leaks. }
	TempCloud := nil;
	TCloudMailRu.TempPublicCloudInit(TempCloud, 'https://cloud.mail.ru/public/xyz789');
	Assert.IsNotNull(TempCloud, 'Instance should be created');
	TempCloud.Free; { Caller is responsible for freeing }
	Assert.Pass('Instance freed successfully - caller owns the instance');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudMailRuStaticTest);

end.
