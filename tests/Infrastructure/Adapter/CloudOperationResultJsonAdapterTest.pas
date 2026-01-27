unit CloudOperationResultJsonAdapterTest;

interface

uses
	CloudOperationResult,
	CloudOperationResultJsonAdapter,
	CloudConstants,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCloudOperationResultJsonAdapterTest = class
	private
		const
			{Successful operation}
			JSON_SUCCESS = '{"status":200,"body":{}}';

			{Error: exists}
			JSON_ERROR_EXISTS = '{"status":400,"body":{"home":{"error":"exists"}}}';

			{Error: readonly}
			JSON_ERROR_READONLY = '{"status":400,"body":{"home":{"error":"readonly"}}}';

			{Error: not_exists}
			JSON_ERROR_NOT_EXISTS = '{"status":400,"body":{"home":{"error":"not_exists"}}}';

			{HTTP 451 - blocked content}
			JSON_STATUS_451 = '{"status":451,"body":{}}';

			{HTTP 507 - overquota}
			JSON_STATUS_507 = '{"status":507,"body":{}}';

			{Invalid JSON}
			JSON_INVALID = 'not valid json';

			{Empty string}
			JSON_EMPTY = '';

			{Error via weblink path instead of home}
			JSON_ERROR_WEBLINK = '{"status":400,"body":{"weblink":{"error":"not_exists"}}}';

			{Error via invite_email path}
			JSON_ERROR_INVITE = '{"status":400,"body":{"invite_email":{"error":"invalid"}}}';

			{Missing body}
			JSON_NO_BODY = '{"status":400}';

			{Body is not an object}
			JSON_BODY_STRING = '{"status":400,"body":"error message"}';

			{Unknown body structure}
			JSON_UNKNOWN_BODY = '{"status":400,"body":{"unknown_field":{"error":"something"}}}';

			{HTTP 406 - Not Acceptable}
			JSON_STATUS_406 = '{"status":406,"body":{}}';

			{All known error types}
			JSON_ERROR_REQUIRED = '{"status":400,"body":{"home":{"error":"required"}}}';
			JSON_ERROR_READ_ONLY = '{"status":400,"body":{"home":{"error":"read_only"}}}';
			JSON_ERROR_NAME_LENGTH = '{"status":400,"body":{"home":{"error":"name_length_exceeded"}}}';
			JSON_ERROR_OVERQUOTA = '{"status":400,"body":{"home":{"error":"overquota"}}}';
			JSON_ERROR_QUOTA_EXCEEDED = '{"status":400,"body":{"home":{"error":"quota_exceeded"}}}';
			JSON_ERROR_INVALID = '{"status":400,"body":{"home":{"error":"invalid"}}}';
			JSON_ERROR_OWN = '{"status":400,"body":{"home":{"error":"own"}}}';
			JSON_ERROR_NAME_TOO_LONG = '{"status":400,"body":{"home":{"error":"name_too_long"}}}';
			JSON_ERROR_VIRUS_SCAN = '{"status":400,"body":{"home":{"error":"virus_scan_fail"}}}';
			JSON_ERROR_OWNER = '{"status":400,"body":{"home":{"error":"owner"}}}';
			JSON_ERROR_TREES_CONFLICT = '{"status":400,"body":{"home":{"error":"trees_conflict"}}}';
			JSON_ERROR_USER_LIMIT = '{"status":400,"body":{"home":{"error":"user_limit_exceeded"}}}';
			JSON_ERROR_EXPORT_LIMIT = '{"status":400,"body":{"home":{"error":"export_limit_exceeded"}}}';
			JSON_ERROR_UNPROCESSABLE = '{"status":400,"body":{"home":{"error":"unprocessable_entry"}}}';
			JSON_ERROR_UNKNOWN_TYPE = '{"status":400,"body":{"home":{"error":"some_future_error"}}}';
	public
		[Test]
		procedure TestParse_Success_SetsOperationOK;
		[Test]
		procedure TestParse_Success_SetsStatus200;
		[Test]
		procedure TestParse_ErrorExists_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorReadonly_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorNotExists_SetsCorrectResult;
		[Test]
		procedure TestParse_Status451_SetsFahrenheitError;
		[Test]
		procedure TestParse_Status507_SetsOverquotaError;
		[Test]
		procedure TestParse_InvalidJSON_SetsUnknownError;
		[Test]
		procedure TestParse_EmptyString_SetsUnknownError;
		[Test]
		procedure TestToBoolean_Success_ReturnsTrue;
		[Test]
		procedure TestToBoolean_Error_ReturnsFalse;
		{Alternative body structures}
		[Test]
		procedure TestParse_ErrorViaWeblink_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorViaInviteEmail_SetsCorrectResult;
		[Test]
		procedure TestParse_NoBody_SetsUnknownError;
		[Test]
		procedure TestParse_BodyString_SetsUnknownError;
		[Test]
		procedure TestParse_UnknownBodyStructure_SetsUnknownError;
		[Test]
		procedure TestParse_Status406_SetsNotAcceptableError;
		{All error types coverage}
		[Test]
		procedure TestParse_ErrorRequired_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorReadOnlyUnderscore_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorNameLength_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorOverquota_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorQuotaExceeded_SetsOverquotaResult;
		[Test]
		procedure TestParse_ErrorInvalid_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorOwn_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorNameTooLong_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorVirusScan_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorOwner_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorTreesConflict_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorUserLimit_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorExportLimit_SetsCorrectResult;
		[Test]
		procedure TestParse_ErrorUnprocessable_SetsCorrectResult;
		[Test]
		procedure TestParse_UnknownErrorType_SetsUnknownResult;
	end;

implementation

procedure TCloudOperationResultJsonAdapterTest.TestParse_Success_SetsOperationOK;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_SUCCESS, Result);
	Assert.AreEqual(CLOUD_OPERATION_OK, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_Success_SetsStatus200;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_SUCCESS, Result);
	Assert.AreEqual(200, Result.OperationStatus);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorExists_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_EXISTS, Result);
	Assert.AreEqual(CLOUD_ERROR_EXISTS, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorReadonly_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_READONLY, Result);
	Assert.AreEqual(CLOUD_ERROR_READONLY, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorNotExists_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_NOT_EXISTS, Result);
	Assert.AreEqual(CLOUD_ERROR_NOT_EXISTS, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_Status451_SetsFahrenheitError;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_STATUS_451, Result);
	Assert.AreEqual(CLOUD_ERROR_FAHRENHEIT, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_Status507_SetsOverquotaError;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_STATUS_507, Result);
	Assert.AreEqual(CLOUD_ERROR_OVERQUOTA, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_InvalidJSON_SetsUnknownError;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_INVALID, Result);
	Assert.AreEqual(CLOUD_ERROR_UNKNOWN, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_EmptyString_SetsUnknownError;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_EMPTY, Result);
	Assert.AreEqual(CLOUD_ERROR_UNKNOWN, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestToBoolean_Success_ReturnsTrue;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_SUCCESS, Result);
	Assert.IsTrue(Result.ToBoolean);
end;

procedure TCloudOperationResultJsonAdapterTest.TestToBoolean_Error_ReturnsFalse;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_EXISTS, Result);
	Assert.IsFalse(Result.ToBoolean);
end;

{Alternative body structures}

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorViaWeblink_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_WEBLINK, Result);
	Assert.AreEqual(CLOUD_ERROR_NOT_EXISTS, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorViaInviteEmail_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_INVITE, Result);
	Assert.AreEqual(CLOUD_ERROR_INVALID, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_NoBody_SetsUnknownError;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_NO_BODY, Result);
	Assert.AreEqual(CLOUD_ERROR_UNKNOWN, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_BodyString_SetsUnknownError;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_BODY_STRING, Result);
	Assert.AreEqual(CLOUD_ERROR_UNKNOWN, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_UnknownBodyStructure_SetsUnknownError;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_UNKNOWN_BODY, Result);
	Assert.AreEqual(CLOUD_ERROR_UNKNOWN, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_Status406_SetsNotAcceptableError;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_STATUS_406, Result);
	Assert.AreEqual(CLOUD_ERROR_NOT_ACCEPTABLE, Result.OperationResult);
end;

{All error types coverage}

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorRequired_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_REQUIRED, Result);
	Assert.AreEqual(CLOUD_ERROR_REQUIRED, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorReadOnlyUnderscore_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	{Test read_only variant (with underscore, different from readonly)}
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_READ_ONLY, Result);
	Assert.AreEqual(CLOUD_ERROR_READONLY, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorNameLength_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_NAME_LENGTH, Result);
	Assert.AreEqual(CLOUD_ERROR_NAME_LENGTH_EXCEEDED, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorOverquota_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_OVERQUOTA, Result);
	Assert.AreEqual(CLOUD_ERROR_OVERQUOTA, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorQuotaExceeded_SetsOverquotaResult;
var
	Result: TCloudOperationResult;
begin
	{quota_exceeded should map to same error as overquota}
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_QUOTA_EXCEEDED, Result);
	Assert.AreEqual(CLOUD_ERROR_OVERQUOTA, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorInvalid_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_INVALID, Result);
	Assert.AreEqual(CLOUD_ERROR_INVALID, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorOwn_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_OWN, Result);
	Assert.AreEqual(CLOUD_ERROR_OWN, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorNameTooLong_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_NAME_TOO_LONG, Result);
	Assert.AreEqual(CLOUD_ERROR_NAME_TOO_LONG, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorVirusScan_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_VIRUS_SCAN, Result);
	Assert.AreEqual(CLOUD_ERROR_VIRUS_SCAN_FAIL, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorOwner_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_OWNER, Result);
	Assert.AreEqual(CLOUD_ERROR_OWNER, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorTreesConflict_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_TREES_CONFLICT, Result);
	Assert.AreEqual(CLOUD_ERROR_TREES_CONFLICT, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorUserLimit_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_USER_LIMIT, Result);
	Assert.AreEqual(CLOUD_ERROR_USER_LIMIT_EXCEEDED, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorExportLimit_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_EXPORT_LIMIT, Result);
	Assert.AreEqual(CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_ErrorUnprocessable_SetsCorrectResult;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_UNPROCESSABLE, Result);
	Assert.AreEqual(CLOUD_ERROR_UNPROCESSABLE_ENTRY, Result.OperationResult);
end;

procedure TCloudOperationResultJsonAdapterTest.TestParse_UnknownErrorType_SetsUnknownResult;
var
	Result: TCloudOperationResult;
begin
	{Future/unknown error types should map to CLOUD_ERROR_UNKNOWN}
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_UNKNOWN_TYPE, Result);
	Assert.AreEqual(CLOUD_ERROR_UNKNOWN, Result.OperationResult);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudOperationResultJsonAdapterTest);

end.
