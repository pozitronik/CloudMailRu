unit CloudMailRuInstanceTest;

interface

uses
	CloudMailRu,
	CloudSettings,
	CMRConstants,
	CMROperationResult,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	IAuthStrategyInterface,
	WindowsFileSystem,
	PLUGIN_TYPES,
	SysUtils,
	DUnitX.TestFramework;

type
	{ Tests for TCloudMailRu instance methods that don't require network access.
	  These tests verify error code mapping and result conversion logic. }
	[TestFixture]
	TCloudMailRuInstanceTest = class
	private
		FCloud: TCloudMailRu;
		FSettings: TCloudSettings;

		{ Helper to create TCMROperationResult with specific error code }
		function CreateResult(OperationResult: Integer; OperationStatus: Integer = 0): TCMROperationResult;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ CloudResultToFsResult tests - record-based overload }
		[Test]
		procedure TestCloudResultToFsResult_OK;
		[Test]
		procedure TestCloudResultToFsResult_Exists;
		[Test]
		procedure TestCloudResultToFsResult_Required;
		[Test]
		procedure TestCloudResultToFsResult_Invalid;
		[Test]
		procedure TestCloudResultToFsResult_ReadOnly;
		[Test]
		procedure TestCloudResultToFsResult_NameLengthExceeded;
		[Test]
		procedure TestCloudResultToFsResult_Unknown;
		[Test]
		procedure TestCloudResultToFsResult_Overquota;
		[Test]
		procedure TestCloudResultToFsResult_NameTooLong;
		[Test]
		procedure TestCloudResultToFsResult_OtherError;

		{ CloudResultToFsResult tests - JSON-based overload }
		[Test]
		procedure TestCloudResultToFsResult_JSON_Success;
		[Test]
		procedure TestCloudResultToFsResult_JSON_Error;
		[Test]
		procedure TestCloudResultToFsResult_JSON_Invalid;

		{ CloudResultToBoolean tests - record-based overload }
		[Test]
		procedure TestCloudResultToBoolean_OK;
		[Test]
		procedure TestCloudResultToBoolean_Exists;
		[Test]
		procedure TestCloudResultToBoolean_Unknown;
		[Test]
		procedure TestCloudResultToBoolean_OtherError;

		{ CloudResultToBoolean tests - JSON-based overload }
		[Test]
		procedure TestCloudResultToBoolean_JSON_Success;
		[Test]
		procedure TestCloudResultToBoolean_JSON_Error;
		[Test]
		procedure TestCloudResultToBoolean_JSON_Invalid;

		{ Edge cases }
		[Test]
		procedure TestCloudResultToFsResult_NegativeErrorCode;
		[Test]
		procedure TestCloudResultToFsResult_WithErrorPrefix;
		[Test]
		procedure TestCloudResultToFsResult_WithoutErrorPrefix;
	end;

implementation

{ Test JSON responses - reusing patterns from CMROperationResultTest }
const
	JSON_SUCCESS = '{"status":200,"body":{"home":"/test"}}';
	JSON_ERROR_EXISTS = '{"status":400,"body":{"home":{"error":"exists"}}}';
	JSON_ERROR_INVALID = '{"status":400,"body":{"home":{"error":"invalid"}}}';
	JSON_INVALID = 'not valid json';

{ TCloudMailRuInstanceTest }

procedure TCloudMailRuInstanceTest.Setup;
begin
	{ Create minimal TCloudMailRu instance with null implementations }
	FSettings := Default(TCloudSettings);
	FCloud := TCloudMailRu.Create(FSettings, nil, TNullAuthStrategy.Create, TNullFileSystem.Create, TNullLogger.Create, TNullProgress.Create, TNullRequest.Create, TNullTCHandler.Create);
end;

procedure TCloudMailRuInstanceTest.TearDown;
begin
	FCloud.Free;
end;

function TCloudMailRuInstanceTest.CreateResult(OperationResult, OperationStatus: Integer): TCMROperationResult;
begin
	Result.OperationResult := OperationResult;
	Result.OperationStatus := OperationStatus;
end;

{ CloudResultToFsResult tests - record-based }

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_OK;
begin
	Assert.AreEqual(FS_FILE_OK, FCloud.CloudResultToFsResult(CreateResult(CLOUD_OPERATION_OK), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_Exists;
begin
	Assert.AreEqual(FS_FILE_EXISTS, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_EXISTS), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_Required;
begin
	{ CLOUD_ERROR_REQUIRED maps to FS_FILE_WRITEERROR }
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_REQUIRED), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_Invalid;
begin
	{ CLOUD_ERROR_INVALID maps to FS_FILE_WRITEERROR }
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_INVALID), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_ReadOnly;
begin
	{ CLOUD_ERROR_READONLY maps to FS_FILE_WRITEERROR }
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_READONLY), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_NameLengthExceeded;
begin
	{ CLOUD_ERROR_NAME_LENGTH_EXCEEDED maps to FS_FILE_WRITEERROR }
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_NAME_LENGTH_EXCEEDED), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_Unknown;
begin
	{ CLOUD_ERROR_UNKNOWN maps to FS_FILE_NOTSUPPORTED }
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_UNKNOWN), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_Overquota;
begin
	{ CLOUD_ERROR_OVERQUOTA maps to FS_FILE_WRITEERROR (with logging) }
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_OVERQUOTA), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_NameTooLong;
begin
	{ CLOUD_ERROR_NAME_TOO_LONG maps to FS_FILE_WRITEERROR (with logging) }
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_NAME_TOO_LONG), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_OtherError;
begin
	{ Unhandled error codes (e.g., CLOUD_ERROR_FAHRENHEIT) map to FS_FILE_WRITEERROR }
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_FAHRENHEIT), ''));
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_VIRUS_SCAN_FAIL), ''));
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_OWNER), ''));
end;

{ CloudResultToFsResult tests - JSON-based }

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_JSON_Success;
begin
	Assert.AreEqual(FS_FILE_OK, FCloud.CloudResultToFsResult(JSON_SUCCESS, ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_JSON_Error;
begin
	{ JSON with "exists" error should map to FS_FILE_EXISTS }
	Assert.AreEqual(FS_FILE_EXISTS, FCloud.CloudResultToFsResult(JSON_ERROR_EXISTS, ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_JSON_Invalid;
begin
	{ Invalid JSON results in CLOUD_ERROR_UNKNOWN -> FS_FILE_NOTSUPPORTED }
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, FCloud.CloudResultToFsResult(JSON_INVALID, ''));
end;

{ CloudResultToBoolean tests - record-based }

procedure TCloudMailRuInstanceTest.TestCloudResultToBoolean_OK;
begin
	Assert.IsTrue(FCloud.CloudResultToBoolean(CreateResult(CLOUD_OPERATION_OK), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToBoolean_Exists;
begin
	Assert.IsFalse(FCloud.CloudResultToBoolean(CreateResult(CLOUD_ERROR_EXISTS), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToBoolean_Unknown;
begin
	Assert.IsFalse(FCloud.CloudResultToBoolean(CreateResult(CLOUD_ERROR_UNKNOWN), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToBoolean_OtherError;
begin
	{ Any non-OK result should return false }
	Assert.IsFalse(FCloud.CloudResultToBoolean(CreateResult(CLOUD_ERROR_OVERQUOTA), ''));
	Assert.IsFalse(FCloud.CloudResultToBoolean(CreateResult(CLOUD_ERROR_FAHRENHEIT), ''));
	Assert.IsFalse(FCloud.CloudResultToBoolean(CreateResult(CLOUD_OPERATION_FAILED), ''));
end;

{ CloudResultToBoolean tests - JSON-based }

procedure TCloudMailRuInstanceTest.TestCloudResultToBoolean_JSON_Success;
begin
	Assert.IsTrue(FCloud.CloudResultToBoolean(JSON_SUCCESS, ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToBoolean_JSON_Error;
begin
	Assert.IsFalse(FCloud.CloudResultToBoolean(JSON_ERROR_EXISTS, ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToBoolean_JSON_Invalid;
begin
	{ Invalid JSON results in CLOUD_ERROR_UNKNOWN -> false }
	Assert.IsFalse(FCloud.CloudResultToBoolean(JSON_INVALID, ''));
end;

{ Edge cases }

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_NegativeErrorCode;
begin
	{ Negative error codes (internal codes) should fall through to else branch }
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_TOKEN_OUTDATED), ''));
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN), ''));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_WithErrorPrefix;
begin
	{ With error prefix, logging occurs but result is the same }
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_FAHRENHEIT), 'Test error: '));
end;

procedure TCloudMailRuInstanceTest.TestCloudResultToFsResult_WithoutErrorPrefix;
begin
	{ Without error prefix (empty string), no logging but result is the same }
	Assert.AreEqual(FS_FILE_WRITEERROR, FCloud.CloudResultToFsResult(CreateResult(CLOUD_ERROR_FAHRENHEIT), ''));
end;

initialization

TDUnitX.RegisterTestFixture(TCloudMailRuInstanceTest);

end.
