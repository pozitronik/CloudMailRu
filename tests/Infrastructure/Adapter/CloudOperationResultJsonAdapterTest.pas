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

initialization

TDUnitX.RegisterTestFixture(TCloudOperationResultJsonAdapterTest);

end.
