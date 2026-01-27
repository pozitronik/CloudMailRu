unit CloudOperationResultTest;

interface

uses
	CloudOperationResult,
	CloudOperationResultJsonAdapter,
	CloudConstants,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCloudOperationResultTest = class
	public
		[Test]
		procedure TestFromJSONSuccess;
		[Test]
		procedure TestFromJSONErrorExists;
		[Test]
		procedure TestFromJSONErrorReadonly;
		[Test]
		procedure TestFromJSONErrorOverquota;
		[Test]
		procedure TestFromJSONErrorNotExists;
		[Test]
		procedure TestFromJSONErrorNameTooLong;
		[Test]
		procedure TestFromJSONErrorInvalid;
		[Test]
		procedure TestFromJSONStatus451Fahrenheit;
		[Test]
		procedure TestFromJSONStatus507Overquota;
		[Test]
		procedure TestFromJSONInvalidJSON;
		[Test]
		procedure TestFromJSONEmptyString;
		[Test]
		procedure TestToBooleanTrue;
		[Test]
		procedure TestToBooleanFalse;
		[Test]
		procedure TestParseRegistration200;
		[Test]
		procedure TestParseRegistration400;
	end;

implementation

const
	{ Test JSON responses }
	JSON_SUCCESS = '{"status":200,"body":{"home":"/test"}}';
	JSON_ERROR_EXISTS = '{"status":400,"body":{"home":{"error":"exists"}}}';
	JSON_ERROR_READONLY = '{"status":400,"body":{"home":{"error":"readonly"}}}';
	JSON_ERROR_READ_ONLY = '{"status":400,"body":{"home":{"error":"read_only"}}}';
	JSON_ERROR_OVERQUOTA = '{"status":400,"body":{"home":{"error":"overquota"}}}';
	JSON_ERROR_NOT_EXISTS = '{"status":400,"body":{"home":{"error":"not_exists"}}}';
	JSON_ERROR_NAME_TOO_LONG = '{"status":400,"body":{"home":{"error":"name_too_long"}}}';
	JSON_ERROR_INVALID = '{"status":400,"body":{"home":{"error":"invalid"}}}';
	JSON_STATUS_451 = '{"status":451,"body":{"home":{"error":"blocked"}}}';
	JSON_STATUS_507 = '{"status":507,"body":{"home":{"error":"storage_full"}}}';
	JSON_REGISTRATION_200 = '{"status":200,"body":"ok"}';
	JSON_REGISTRATION_400 = '{"status":400,"body":"error"}';

procedure TCloudOperationResultTest.TestFromJSONSuccess;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_SUCCESS, Result);

	Assert.AreEqual(200, Result.OperationStatus);
	Assert.AreEqual(CLOUD_OPERATION_OK, Result.OperationResult);
end;

procedure TCloudOperationResultTest.TestFromJSONErrorExists;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_EXISTS, Result);

	Assert.AreEqual(400, Result.OperationStatus);
	Assert.AreEqual(CLOUD_ERROR_EXISTS, Result.OperationResult);
end;

procedure TCloudOperationResultTest.TestFromJSONErrorReadonly;
var
	Result: TCloudOperationResult;
begin
	{ Test both 'readonly' and 'read_only' variants }
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_READONLY, Result);
	Assert.AreEqual(CLOUD_ERROR_READONLY, Result.OperationResult);

	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_READ_ONLY, Result);
	Assert.AreEqual(CLOUD_ERROR_READONLY, Result.OperationResult);
end;

procedure TCloudOperationResultTest.TestFromJSONErrorOverquota;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_OVERQUOTA, Result);

	Assert.AreEqual(CLOUD_ERROR_OVERQUOTA, Result.OperationResult);
end;

procedure TCloudOperationResultTest.TestFromJSONErrorNotExists;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_NOT_EXISTS, Result);

	Assert.AreEqual(CLOUD_ERROR_NOT_EXISTS, Result.OperationResult);
end;

procedure TCloudOperationResultTest.TestFromJSONErrorNameTooLong;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_NAME_TOO_LONG, Result);

	Assert.AreEqual(CLOUD_ERROR_NAME_TOO_LONG, Result.OperationResult);
end;

procedure TCloudOperationResultTest.TestFromJSONErrorInvalid;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_INVALID, Result);

	Assert.AreEqual(CLOUD_ERROR_INVALID, Result.OperationResult);
end;

procedure TCloudOperationResultTest.TestFromJSONStatus451Fahrenheit;
var
	Result: TCloudOperationResult;
begin
	{ HTTP 451 = content blocked (copyright) }
	TCloudOperationResultJsonAdapter.Parse(JSON_STATUS_451, Result);

	Assert.AreEqual(451, Result.OperationStatus);
	Assert.AreEqual(CLOUD_ERROR_FAHRENHEIT, Result.OperationResult);
end;

procedure TCloudOperationResultTest.TestFromJSONStatus507Overquota;
var
	Result: TCloudOperationResult;
begin
	{ HTTP 507 = storage quota exceeded }
	TCloudOperationResultJsonAdapter.Parse(JSON_STATUS_507, Result);

	Assert.AreEqual(507, Result.OperationStatus);
	Assert.AreEqual(CLOUD_ERROR_OVERQUOTA, Result.OperationResult);
end;

procedure TCloudOperationResultTest.TestFromJSONInvalidJSON;
var
	Result: TCloudOperationResult;
begin
	{ Invalid JSON should result in CLOUD_ERROR_UNKNOWN }
	TCloudOperationResultJsonAdapter.Parse('not valid json', Result);

	Assert.AreEqual(0, Result.OperationStatus);
	Assert.AreEqual(CLOUD_ERROR_UNKNOWN, Result.OperationResult);
end;

procedure TCloudOperationResultTest.TestFromJSONEmptyString;
var
	Result: TCloudOperationResult;
begin
	{ Empty string should result in CLOUD_ERROR_UNKNOWN }
	TCloudOperationResultJsonAdapter.Parse('', Result);

	Assert.AreEqual(0, Result.OperationStatus);
	Assert.AreEqual(CLOUD_ERROR_UNKNOWN, Result.OperationResult);
end;

procedure TCloudOperationResultTest.TestToBooleanTrue;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_SUCCESS, Result);

	Assert.IsTrue(Result.ToBoolean);
end;

procedure TCloudOperationResultTest.TestToBooleanFalse;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON_ERROR_EXISTS, Result);

	Assert.IsFalse(Result.ToBoolean);
end;

procedure TCloudOperationResultTest.TestParseRegistration200;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.ParseRegistration(JSON_REGISTRATION_200, Result);

	Assert.AreEqual(200, Result.OperationStatus);
	Assert.AreEqual(CLOUD_OPERATION_OK, Result.OperationResult);
end;

procedure TCloudOperationResultTest.TestParseRegistration400;
var
	Result: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.ParseRegistration(JSON_REGISTRATION_400, Result);

	Assert.AreEqual(400, Result.OperationStatus);
	Assert.AreEqual(CLOUD_ERROR_BAD_REQUEST, Result.OperationResult);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudOperationResultTest);

end.
