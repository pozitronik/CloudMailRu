unit CMROperationResultTest;

interface

uses
	CMROperationResult,
	CMROperationResultJsonAdapter,
	CMRConstants,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCMROperationResultTest = class
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

procedure TCMROperationResultTest.TestFromJSONSuccess;
var
	Result: TCMROperationResult;
begin
	TCMROperationResultJsonAdapter.Parse(JSON_SUCCESS, Result);

	Assert.AreEqual(200, Result.OperationStatus);
	Assert.AreEqual(CLOUD_OPERATION_OK, Result.OperationResult);
end;

procedure TCMROperationResultTest.TestFromJSONErrorExists;
var
	Result: TCMROperationResult;
begin
	TCMROperationResultJsonAdapter.Parse(JSON_ERROR_EXISTS, Result);

	Assert.AreEqual(400, Result.OperationStatus);
	Assert.AreEqual(CLOUD_ERROR_EXISTS, Result.OperationResult);
end;

procedure TCMROperationResultTest.TestFromJSONErrorReadonly;
var
	Result: TCMROperationResult;
begin
	{ Test both 'readonly' and 'read_only' variants }
	TCMROperationResultJsonAdapter.Parse(JSON_ERROR_READONLY, Result);
	Assert.AreEqual(CLOUD_ERROR_READONLY, Result.OperationResult);

	TCMROperationResultJsonAdapter.Parse(JSON_ERROR_READ_ONLY, Result);
	Assert.AreEqual(CLOUD_ERROR_READONLY, Result.OperationResult);
end;

procedure TCMROperationResultTest.TestFromJSONErrorOverquota;
var
	Result: TCMROperationResult;
begin
	TCMROperationResultJsonAdapter.Parse(JSON_ERROR_OVERQUOTA, Result);

	Assert.AreEqual(CLOUD_ERROR_OVERQUOTA, Result.OperationResult);
end;

procedure TCMROperationResultTest.TestFromJSONErrorNotExists;
var
	Result: TCMROperationResult;
begin
	TCMROperationResultJsonAdapter.Parse(JSON_ERROR_NOT_EXISTS, Result);

	Assert.AreEqual(CLOUD_ERROR_NOT_EXISTS, Result.OperationResult);
end;

procedure TCMROperationResultTest.TestFromJSONErrorNameTooLong;
var
	Result: TCMROperationResult;
begin
	TCMROperationResultJsonAdapter.Parse(JSON_ERROR_NAME_TOO_LONG, Result);

	Assert.AreEqual(CLOUD_ERROR_NAME_TOO_LONG, Result.OperationResult);
end;

procedure TCMROperationResultTest.TestFromJSONErrorInvalid;
var
	Result: TCMROperationResult;
begin
	TCMROperationResultJsonAdapter.Parse(JSON_ERROR_INVALID, Result);

	Assert.AreEqual(CLOUD_ERROR_INVALID, Result.OperationResult);
end;

procedure TCMROperationResultTest.TestFromJSONStatus451Fahrenheit;
var
	Result: TCMROperationResult;
begin
	{ HTTP 451 = content blocked (copyright) }
	TCMROperationResultJsonAdapter.Parse(JSON_STATUS_451, Result);

	Assert.AreEqual(451, Result.OperationStatus);
	Assert.AreEqual(CLOUD_ERROR_FAHRENHEIT, Result.OperationResult);
end;

procedure TCMROperationResultTest.TestFromJSONStatus507Overquota;
var
	Result: TCMROperationResult;
begin
	{ HTTP 507 = storage quota exceeded }
	TCMROperationResultJsonAdapter.Parse(JSON_STATUS_507, Result);

	Assert.AreEqual(507, Result.OperationStatus);
	Assert.AreEqual(CLOUD_ERROR_OVERQUOTA, Result.OperationResult);
end;

procedure TCMROperationResultTest.TestFromJSONInvalidJSON;
var
	Result: TCMROperationResult;
begin
	{ Invalid JSON should result in CLOUD_ERROR_UNKNOWN }
	TCMROperationResultJsonAdapter.Parse('not valid json', Result);

	Assert.AreEqual(0, Result.OperationStatus);
	Assert.AreEqual(CLOUD_ERROR_UNKNOWN, Result.OperationResult);
end;

procedure TCMROperationResultTest.TestFromJSONEmptyString;
var
	Result: TCMROperationResult;
begin
	{ Empty string should result in CLOUD_ERROR_UNKNOWN }
	TCMROperationResultJsonAdapter.Parse('', Result);

	Assert.AreEqual(0, Result.OperationStatus);
	Assert.AreEqual(CLOUD_ERROR_UNKNOWN, Result.OperationResult);
end;

procedure TCMROperationResultTest.TestToBooleanTrue;
var
	Result: TCMROperationResult;
begin
	TCMROperationResultJsonAdapter.Parse(JSON_SUCCESS, Result);

	Assert.IsTrue(Result.ToBoolean);
end;

procedure TCMROperationResultTest.TestToBooleanFalse;
var
	Result: TCMROperationResult;
begin
	TCMROperationResultJsonAdapter.Parse(JSON_ERROR_EXISTS, Result);

	Assert.IsFalse(Result.ToBoolean);
end;

procedure TCMROperationResultTest.TestParseRegistration200;
var
	Result: TCMROperationResult;
begin
	TCMROperationResultJsonAdapter.ParseRegistration(JSON_REGISTRATION_200, Result);

	Assert.AreEqual(200, Result.OperationStatus);
	Assert.AreEqual(CLOUD_OPERATION_OK, Result.OperationResult);
end;

procedure TCMROperationResultTest.TestParseRegistration400;
var
	Result: TCMROperationResult;
begin
	TCMROperationResultJsonAdapter.ParseRegistration(JSON_REGISTRATION_400, Result);

	Assert.AreEqual(400, Result.OperationStatus);
	Assert.AreEqual(CLOUD_ERROR_BAD_REQUEST, Result.OperationResult);
end;

initialization

TDUnitX.RegisterTestFixture(TCMROperationResultTest);

end.
