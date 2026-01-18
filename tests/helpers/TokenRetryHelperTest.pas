unit TokenRetryHelperTest;

{Tests for TokenRetryHelper - token refresh retry logic.}

interface

uses
	TokenRetryHelper,
	DUnitX.TestFramework;

type
	{Tests for IsTokenExpiredInJSON}
	[TestFixture]
	TIsTokenExpiredInJSONTest = class
	public
		[Test]
		procedure TestTokenError_ReturnsTrue;
		[Test]
		procedure TestOtherError_ReturnsFalse;
		[Test]
		procedure TestSuccessResponse_ReturnsFalse;
		[Test]
		procedure TestEmptyJSON_ReturnsFalse;
		[Test]
		procedure TestInvalidJSON_ReturnsFalse;
	end;

	{Tests for IsTokenExpiredResult}
	[TestFixture]
	TIsTokenExpiredResultTest = class
	public
		[Test]
		procedure TestTokenOutdated_ReturnsTrue;
		[Test]
		procedure TestOK_ReturnsFalse;
		[Test]
		procedure TestOtherError_ReturnsFalse;
		[Test]
		procedure TestZero_ReturnsFalse;
	end;

	{Tests for TAPICallResult}
	[TestFixture]
	TAPICallResultTest = class
	public
		{FromBoolean tests}
		[Test]
		procedure TestFromBoolean_Success;
		[Test]
		procedure TestFromBoolean_Failure;

		{FromInteger tests}
		[Test]
		procedure TestFromInteger_OK;
		[Test]
		procedure TestFromInteger_FileOK;
		[Test]
		procedure TestFromInteger_Error;
		[Test]
		procedure TestFromInteger_TokenOutdated;

		{NeedsTokenRefresh tests}
		[Test]
		procedure TestNeedsTokenRefresh_BooleanWithTokenError;
		[Test]
		procedure TestNeedsTokenRefresh_BooleanSuccess;
		[Test]
		procedure TestNeedsTokenRefresh_BooleanOtherError;
		[Test]
		procedure TestNeedsTokenRefresh_IntegerTokenOutdated;
		[Test]
		procedure TestNeedsTokenRefresh_IntegerOK;
	end;

	{Tests for ExecuteWithTokenRetry}
	[TestFixture]
	TExecuteWithTokenRetryTest = class
	private
		FCallCount: Integer;
		FRefreshCount: Integer;
		FRefreshShouldSucceed: Boolean;
		FFirstCallShouldFail: Boolean;

		function MockOperation: TAPICallResult;
		function MockRefreshToken: Boolean;
	public
		[Setup]
		procedure Setup;

		[Test]
		procedure TestSuccessOnFirstTry_NoRetry;
		[Test]
		procedure TestTokenError_RetriesOnce;
		[Test]
		procedure TestTokenError_RefreshFails_NoRetry;
		[Test]
		procedure TestTokenError_RespectsMaxRetries;
		[Test]
		procedure TestOtherError_NoRetry;
	end;

implementation

uses
	CMRConstants,
	PLUGIN_TYPES;

const
	{Test JSON responses - body is a string for errors, not an object}
	JSON_SUCCESS = '{"status":200,"body":{"home":"/test"}}';
	JSON_TOKEN_ERROR = '{"status":400,"body":"token"}';
	JSON_OTHER_ERROR = '{"status":400,"body":"exists"}';

{ TIsTokenExpiredInJSONTest }

procedure TIsTokenExpiredInJSONTest.TestTokenError_ReturnsTrue;
begin
	Assert.IsTrue(IsTokenExpiredInJSON(JSON_TOKEN_ERROR));
end;

procedure TIsTokenExpiredInJSONTest.TestOtherError_ReturnsFalse;
begin
	Assert.IsFalse(IsTokenExpiredInJSON(JSON_OTHER_ERROR));
end;

procedure TIsTokenExpiredInJSONTest.TestSuccessResponse_ReturnsFalse;
begin
	Assert.IsFalse(IsTokenExpiredInJSON(JSON_SUCCESS));
end;

procedure TIsTokenExpiredInJSONTest.TestEmptyJSON_ReturnsFalse;
begin
	Assert.IsFalse(IsTokenExpiredInJSON(''));
end;

procedure TIsTokenExpiredInJSONTest.TestInvalidJSON_ReturnsFalse;
begin
	Assert.IsFalse(IsTokenExpiredInJSON('not valid json'));
end;

{ TIsTokenExpiredResultTest }

procedure TIsTokenExpiredResultTest.TestTokenOutdated_ReturnsTrue;
begin
	Assert.IsTrue(IsTokenExpiredResult(CLOUD_ERROR_TOKEN_OUTDATED));
end;

procedure TIsTokenExpiredResultTest.TestOK_ReturnsFalse;
begin
	Assert.IsFalse(IsTokenExpiredResult(CLOUD_OPERATION_OK));
end;

procedure TIsTokenExpiredResultTest.TestOtherError_ReturnsFalse;
begin
	Assert.IsFalse(IsTokenExpiredResult(CLOUD_ERROR_EXISTS));
	Assert.IsFalse(IsTokenExpiredResult(CLOUD_OPERATION_FAILED));
end;

procedure TIsTokenExpiredResultTest.TestZero_ReturnsFalse;
begin
	Assert.IsFalse(IsTokenExpiredResult(0));
end;

{ TAPICallResultTest }

procedure TAPICallResultTest.TestFromBoolean_Success;
var
	R: TAPICallResult;
begin
	R := TAPICallResult.FromBoolean(True, JSON_SUCCESS);
	Assert.IsTrue(R.Success);
	Assert.AreEqual(JSON_SUCCESS, R.JSON);
	Assert.AreEqual(0, R.ResultCode);
end;

procedure TAPICallResultTest.TestFromBoolean_Failure;
var
	R: TAPICallResult;
begin
	R := TAPICallResult.FromBoolean(False, JSON_OTHER_ERROR);
	Assert.IsFalse(R.Success);
	Assert.AreEqual(JSON_OTHER_ERROR, R.JSON);
end;

procedure TAPICallResultTest.TestFromInteger_OK;
var
	R: TAPICallResult;
begin
	R := TAPICallResult.FromInteger(CLOUD_OPERATION_OK, JSON_SUCCESS);
	Assert.IsTrue(R.Success);
	Assert.AreEqual(CLOUD_OPERATION_OK, R.ResultCode);
end;

procedure TAPICallResultTest.TestFromInteger_FileOK;
var
	R: TAPICallResult;
begin
	R := TAPICallResult.FromInteger(FS_FILE_OK, '');
	Assert.IsTrue(R.Success);
	Assert.AreEqual(FS_FILE_OK, R.ResultCode);
end;

procedure TAPICallResultTest.TestFromInteger_Error;
var
	R: TAPICallResult;
begin
	R := TAPICallResult.FromInteger(CLOUD_ERROR_EXISTS, JSON_OTHER_ERROR);
	Assert.IsFalse(R.Success);
	Assert.AreEqual(CLOUD_ERROR_EXISTS, R.ResultCode);
end;

procedure TAPICallResultTest.TestFromInteger_TokenOutdated;
var
	R: TAPICallResult;
begin
	R := TAPICallResult.FromInteger(CLOUD_ERROR_TOKEN_OUTDATED, '');
	Assert.IsFalse(R.Success);
	Assert.AreEqual(CLOUD_ERROR_TOKEN_OUTDATED, R.ResultCode);
end;

procedure TAPICallResultTest.TestNeedsTokenRefresh_BooleanWithTokenError;
var
	R: TAPICallResult;
begin
	R := TAPICallResult.FromBoolean(False, JSON_TOKEN_ERROR);
	Assert.IsTrue(R.NeedsTokenRefresh, 'Failed Boolean with token error should need refresh');
end;

procedure TAPICallResultTest.TestNeedsTokenRefresh_BooleanSuccess;
var
	R: TAPICallResult;
begin
	R := TAPICallResult.FromBoolean(True, JSON_SUCCESS);
	Assert.IsFalse(R.NeedsTokenRefresh, 'Successful Boolean should not need refresh');
end;

procedure TAPICallResultTest.TestNeedsTokenRefresh_BooleanOtherError;
var
	R: TAPICallResult;
begin
	R := TAPICallResult.FromBoolean(False, JSON_OTHER_ERROR);
	Assert.IsFalse(R.NeedsTokenRefresh, 'Failed Boolean with other error should not need refresh');
end;

procedure TAPICallResultTest.TestNeedsTokenRefresh_IntegerTokenOutdated;
var
	R: TAPICallResult;
begin
	R := TAPICallResult.FromInteger(CLOUD_ERROR_TOKEN_OUTDATED, '');
	Assert.IsTrue(R.NeedsTokenRefresh, 'Token outdated code should need refresh');
end;

procedure TAPICallResultTest.TestNeedsTokenRefresh_IntegerOK;
var
	R: TAPICallResult;
begin
	R := TAPICallResult.FromInteger(CLOUD_OPERATION_OK, JSON_SUCCESS);
	Assert.IsFalse(R.NeedsTokenRefresh, 'OK result should not need refresh');
end;

{ TExecuteWithTokenRetryTest }

procedure TExecuteWithTokenRetryTest.Setup;
begin
	FCallCount := 0;
	FRefreshCount := 0;
	FRefreshShouldSucceed := True;
	FFirstCallShouldFail := False;
end;

function TExecuteWithTokenRetryTest.MockOperation: TAPICallResult;
begin
	Inc(FCallCount);
	if FFirstCallShouldFail and (FCallCount = 1) then
		Result := TAPICallResult.FromBoolean(False, JSON_TOKEN_ERROR)
	else
		Result := TAPICallResult.FromBoolean(True, JSON_SUCCESS);
end;

function TExecuteWithTokenRetryTest.MockRefreshToken: Boolean;
begin
	Inc(FRefreshCount);
	Result := FRefreshShouldSucceed;
end;

procedure TExecuteWithTokenRetryTest.TestSuccessOnFirstTry_NoRetry;
var
	R: TAPICallResult;
begin
	FFirstCallShouldFail := False;

	R := ExecuteWithTokenRetry(MockOperation, MockRefreshToken);

	Assert.IsTrue(R.Success);
	Assert.AreEqual(1, FCallCount, 'Should call operation once');
	Assert.AreEqual(0, FRefreshCount, 'Should not refresh token');
end;

procedure TExecuteWithTokenRetryTest.TestTokenError_RetriesOnce;
var
	R: TAPICallResult;
begin
	FFirstCallShouldFail := True;
	FRefreshShouldSucceed := True;

	R := ExecuteWithTokenRetry(MockOperation, MockRefreshToken);

	Assert.IsTrue(R.Success);
	Assert.AreEqual(2, FCallCount, 'Should call operation twice');
	Assert.AreEqual(1, FRefreshCount, 'Should refresh token once');
end;

procedure TExecuteWithTokenRetryTest.TestTokenError_RefreshFails_NoRetry;
var
	R: TAPICallResult;
begin
	FFirstCallShouldFail := True;
	FRefreshShouldSucceed := False;

	R := ExecuteWithTokenRetry(MockOperation, MockRefreshToken);

	Assert.IsFalse(R.Success);
	Assert.AreEqual(1, FCallCount, 'Should call operation once');
	Assert.AreEqual(1, FRefreshCount, 'Should attempt refresh once');
end;

procedure TExecuteWithTokenRetryTest.TestTokenError_RespectsMaxRetries;
var
	R: TAPICallResult;
	AlwaysFailCount: Integer;
begin
	AlwaysFailCount := 0;

	R := ExecuteWithTokenRetry(
		function: TAPICallResult
		begin
			Inc(AlwaysFailCount);
			Result := TAPICallResult.FromBoolean(False, JSON_TOKEN_ERROR);
		end,
		function: Boolean
		begin
			Result := True; {Refresh always succeeds}
		end,
		3); {Allow 3 retries}

	Assert.IsFalse(R.Success);
	Assert.AreEqual(4, AlwaysFailCount, 'Should call operation 4 times (1 initial + 3 retries)');
end;

procedure TExecuteWithTokenRetryTest.TestOtherError_NoRetry;
var
	R: TAPICallResult;
	CallCount: Integer;
begin
	CallCount := 0;

	R := ExecuteWithTokenRetry(
		function: TAPICallResult
		begin
			Inc(CallCount);
			Result := TAPICallResult.FromBoolean(False, JSON_OTHER_ERROR);
		end,
		MockRefreshToken);

	Assert.IsFalse(R.Success);
	Assert.AreEqual(1, CallCount, 'Should call operation once for non-token error');
	Assert.AreEqual(0, FRefreshCount, 'Should not refresh for non-token error');
end;

initialization

TDUnitX.RegisterTestFixture(TIsTokenExpiredInJSONTest);
TDUnitX.RegisterTestFixture(TIsTokenExpiredResultTest);
TDUnitX.RegisterTestFixture(TAPICallResultTest);
TDUnitX.RegisterTestFixture(TExecuteWithTokenRetryTest);

end.
