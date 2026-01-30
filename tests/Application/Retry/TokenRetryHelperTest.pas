unit TokenRetryHelperTest;

{Tests for TokenRetryHelper - token refresh retry logic.}

interface

uses
	TokenRetryHelper,
	CloudContext,
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
		[Test]
		procedure TestNotAuthorizedError_ReturnsTrue;
		[Test]
		procedure TestNotAuthorizedError_DifferentValue_ReturnsFalse;
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
		[Test]
		procedure TestNeedsTokenRefresh_BooleanWithNotAuthorizedError;
	end;

	{Mock implementation of IRetryContext for testing TRetryOperation}
	TMockRetryContext = class(TInterfacedObject, IRetryContext)
	private
		FRefreshCount: Integer;
		FPostFormCount: Integer;
		FGetPageCount: Integer;
		FRefreshShouldSucceed: Boolean;
		FPostFormShouldSucceed: Boolean;
		FGetPageShouldSucceed: Boolean;
		FReturnTokenError: Boolean;
		FLastPostedURL: WideString;
		FLastPostedParams: WideString;
	public
		constructor Create;

		{IRetryContext implementation}
		function RefreshToken: Boolean;
		function PostForm(const URL, Params: WideString; var JSON: WideString): Boolean;
		function GetPage(const URL: WideString; var JSON: WideString; var ShowProgress: Boolean): Boolean;
		function ResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean;
		function ResultToInteger(const JSON, ErrorPrefix: WideString): Integer;

		{Test configuration}
		property RefreshShouldSucceed: Boolean read FRefreshShouldSucceed write FRefreshShouldSucceed;
		property PostFormShouldSucceed: Boolean read FPostFormShouldSucceed write FPostFormShouldSucceed;
		property GetPageShouldSucceed: Boolean read FGetPageShouldSucceed write FGetPageShouldSucceed;
		property ReturnTokenError: Boolean read FReturnTokenError write FReturnTokenError;

		{Test inspection}
		property RefreshCount: Integer read FRefreshCount;
		property PostFormCount: Integer read FPostFormCount;
		property GetPageCount: Integer read FGetPageCount;
		property LastPostedURL: WideString read FLastPostedURL;
		property LastPostedParams: WideString read FLastPostedParams;
	end;

	{Tests for TRetryOperation}
	[TestFixture]
	TRetryOperationTest = class
	private
		FRetryOp: TRetryOperation;
		FMockContext: TMockRetryContext;
		FMockContextRef: IRetryContext;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Execute tests}
		[Test]
		procedure TestExecute_SuccessOnFirstTry;
		[Test]
		procedure TestExecute_RetriesOnTokenError;
		[Test]
		procedure TestExecute_StopsWhenRefreshFails;

		{PostFormBoolean tests}
		[Test]
		procedure TestPostFormBoolean_Success;
		[Test]
		procedure TestPostFormBoolean_Failure;
		[Test]
		procedure TestPostFormBoolean_RetriesOnTokenError;

		{PostFormInteger tests}
		[Test]
		procedure TestPostFormInteger_Success;
		[Test]
		procedure TestPostFormInteger_Failure;
		[Test]
		procedure TestPostFormInteger_RetriesOnTokenError;

		{GetPageBoolean tests}
		[Test]
		procedure TestGetPageBoolean_Success;
		[Test]
		procedure TestGetPageBoolean_Failure;
		[Test]
		procedure TestGetPageBoolean_RetriesOnTokenError;
		[Test]
		procedure TestGetPageBoolean_ReturnsJSON;

		{GetPage tests}
		[Test]
		procedure TestGetPage_Success;
		[Test]
		procedure TestGetPage_ReturnsJSON;
	end;

implementation

uses
	CloudConstants,
	WFXTypes;

const
	{Test JSON responses - body is a string for errors, not an object}
	JSON_SUCCESS = '{"status":200,"body":{"home":"/test"}}';
	JSON_TOKEN_ERROR = '{"status":400,"body":"token"}';
	JSON_OTHER_ERROR = '{"status":400,"body":"exists"}';
	JSON_NOT_AUTHORIZED = '{"error":"NOT/AUTHORIZED"}';

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

procedure TIsTokenExpiredInJSONTest.TestNotAuthorizedError_ReturnsTrue;
begin
	Assert.IsTrue(IsTokenExpiredInJSON(JSON_NOT_AUTHORIZED));
end;

procedure TIsTokenExpiredInJSONTest.TestNotAuthorizedError_DifferentValue_ReturnsFalse;
begin
	Assert.IsFalse(IsTokenExpiredInJSON('{"error":"SOME_OTHER_ERROR"}'));
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

procedure TAPICallResultTest.TestNeedsTokenRefresh_BooleanWithNotAuthorizedError;
var
	R: TAPICallResult;
begin
	R := TAPICallResult.FromBoolean(False, JSON_NOT_AUTHORIZED);
	Assert.IsTrue(R.NeedsTokenRefresh, 'Failed Boolean with NOT/AUTHORIZED should need refresh');
end;

{ TMockRetryContext }

constructor TMockRetryContext.Create;
begin
	inherited Create;
	FRefreshCount := 0;
	FPostFormCount := 0;
	FGetPageCount := 0;
	FRefreshShouldSucceed := True;
	FPostFormShouldSucceed := True;
	FGetPageShouldSucceed := True;
	FReturnTokenError := False;
	FLastPostedURL := '';
	FLastPostedParams := '';
end;

function TMockRetryContext.RefreshToken: Boolean;
begin
	Inc(FRefreshCount);
	Result := FRefreshShouldSucceed;
end;

function TMockRetryContext.PostForm(const URL, Params: WideString; var JSON: WideString): Boolean;
begin
	Inc(FPostFormCount);
	FLastPostedURL := URL;
	FLastPostedParams := Params;

	if FReturnTokenError and (FPostFormCount = 1) then
	begin
		JSON := JSON_TOKEN_ERROR;
		Result := True; {HTTP succeeded but API returned token error}
	end
	else if FPostFormShouldSucceed then
	begin
		JSON := JSON_SUCCESS;
		Result := True;
	end
	else
	begin
		JSON := JSON_OTHER_ERROR;
		Result := False;
	end;
end;

function TMockRetryContext.GetPage(const URL: WideString; var JSON: WideString; var ShowProgress: Boolean): Boolean;
begin
	Inc(FGetPageCount);

	if FReturnTokenError and (FGetPageCount = 1) then
	begin
		JSON := JSON_TOKEN_ERROR;
		Result := True;
	end
	else if FGetPageShouldSucceed then
	begin
		JSON := JSON_SUCCESS;
		Result := True;
	end
	else
	begin
		JSON := JSON_OTHER_ERROR;
		Result := False;
	end;
end;

function TMockRetryContext.ResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean;
begin
	Result := (JSON = JSON_SUCCESS);
end;

function TMockRetryContext.ResultToInteger(const JSON, ErrorPrefix: WideString): Integer;
begin
	if JSON = JSON_SUCCESS then
		Result := FS_FILE_OK
	else
		Result := FS_FILE_WRITEERROR;
end;

{ TRetryOperationTest }

procedure TRetryOperationTest.Setup;
begin
	FMockContext := TMockRetryContext.Create;
	FMockContextRef := FMockContext;
	FRetryOp := TRetryOperation.Create(FMockContext);
end;

procedure TRetryOperationTest.TearDown;
begin
	FRetryOp.Free;
	FMockContextRef := nil;
end;

{ Execute tests }

procedure TRetryOperationTest.TestExecute_SuccessOnFirstTry;
var
	CallCount: Integer;
	R: TAPICallResult;
begin
	CallCount := 0;
	R := FRetryOp.Execute(
		function: TAPICallResult
		begin
			Inc(CallCount);
			Result := TAPICallResult.FromBoolean(True, JSON_SUCCESS);
		end);

	Assert.IsTrue(R.Success);
	Assert.AreEqual(1, CallCount, 'Should call operation once');
	Assert.AreEqual(0, FMockContext.RefreshCount, 'Should not refresh token');
end;

procedure TRetryOperationTest.TestExecute_RetriesOnTokenError;
var
	CallCount: Integer;
	R: TAPICallResult;
begin
	CallCount := 0;
	R := FRetryOp.Execute(
		function: TAPICallResult
		begin
			Inc(CallCount);
			if CallCount = 1 then
				Result := TAPICallResult.FromBoolean(False, JSON_TOKEN_ERROR)
			else
				Result := TAPICallResult.FromBoolean(True, JSON_SUCCESS);
		end);

	Assert.IsTrue(R.Success);
	Assert.AreEqual(2, CallCount, 'Should call operation twice');
	Assert.AreEqual(1, FMockContext.RefreshCount, 'Should refresh token once');
end;

procedure TRetryOperationTest.TestExecute_StopsWhenRefreshFails;
var
	CallCount: Integer;
	R: TAPICallResult;
begin
	FMockContext.RefreshShouldSucceed := False;
	CallCount := 0;
	R := FRetryOp.Execute(
		function: TAPICallResult
		begin
			Inc(CallCount);
			Result := TAPICallResult.FromBoolean(False, JSON_TOKEN_ERROR);
		end);

	Assert.IsFalse(R.Success);
	Assert.AreEqual(1, CallCount, 'Should call operation once');
	Assert.AreEqual(1, FMockContext.RefreshCount, 'Should attempt refresh once');
end;

{ PostFormBoolean tests }

procedure TRetryOperationTest.TestPostFormBoolean_Success;
var
	R: Boolean;
begin
	R := FRetryOp.PostFormBoolean('http://test/api', 'param=value', 'Error: ');

	Assert.IsTrue(R);
	Assert.AreEqual(1, FMockContext.PostFormCount);
	Assert.AreEqual(WideString('http://test/api'), FMockContext.LastPostedURL);
	Assert.AreEqual(WideString('param=value'), FMockContext.LastPostedParams);
end;

procedure TRetryOperationTest.TestPostFormBoolean_Failure;
var
	R: Boolean;
begin
	FMockContext.PostFormShouldSucceed := False;
	R := FRetryOp.PostFormBoolean('http://test/api', 'param=value', 'Error: ');

	Assert.IsFalse(R);
end;

procedure TRetryOperationTest.TestPostFormBoolean_RetriesOnTokenError;
var
	R: Boolean;
begin
	FMockContext.ReturnTokenError := True;
	R := FRetryOp.PostFormBoolean('http://test/api', 'param=value', 'Error: ');

	Assert.IsTrue(R);
	Assert.AreEqual(2, FMockContext.PostFormCount, 'Should call PostForm twice');
	Assert.AreEqual(1, FMockContext.RefreshCount, 'Should refresh token once');
end;

{ PostFormInteger tests }

procedure TRetryOperationTest.TestPostFormInteger_Success;
var
	R: Integer;
begin
	R := FRetryOp.PostFormInteger('http://test/api', 'param=value', 'Error: ');

	Assert.AreEqual(FS_FILE_OK, R);
end;

procedure TRetryOperationTest.TestPostFormInteger_Failure;
var
	R: Integer;
begin
	FMockContext.PostFormShouldSucceed := False;
	R := FRetryOp.PostFormInteger('http://test/api', 'param=value', 'Error: ');

	Assert.AreEqual(FS_FILE_WRITEERROR, R);
end;

procedure TRetryOperationTest.TestPostFormInteger_RetriesOnTokenError;
var
	R: Integer;
begin
	FMockContext.ReturnTokenError := True;
	R := FRetryOp.PostFormInteger('http://test/api', 'param=value', 'Error: ');

	Assert.AreEqual(FS_FILE_OK, R);
	Assert.AreEqual(2, FMockContext.PostFormCount, 'Should call PostForm twice');
	Assert.AreEqual(1, FMockContext.RefreshCount, 'Should refresh token once');
end;

{ GetPageBoolean tests }

procedure TRetryOperationTest.TestGetPageBoolean_Success;
var
	R: Boolean;
	JSON: WideString;
begin
	R := FRetryOp.GetPageBoolean('http://test/api', 'Error: ', JSON);

	Assert.IsTrue(R);
	Assert.AreEqual(1, FMockContext.GetPageCount);
end;

procedure TRetryOperationTest.TestGetPageBoolean_Failure;
var
	R: Boolean;
	JSON: WideString;
begin
	FMockContext.GetPageShouldSucceed := False;
	R := FRetryOp.GetPageBoolean('http://test/api', 'Error: ', JSON);

	Assert.IsFalse(R);
end;

procedure TRetryOperationTest.TestGetPageBoolean_RetriesOnTokenError;
var
	R: Boolean;
	JSON: WideString;
begin
	FMockContext.ReturnTokenError := True;
	R := FRetryOp.GetPageBoolean('http://test/api', 'Error: ', JSON);

	Assert.IsTrue(R);
	Assert.AreEqual(2, FMockContext.GetPageCount, 'Should call GetPage twice');
	Assert.AreEqual(1, FMockContext.RefreshCount, 'Should refresh token once');
end;

procedure TRetryOperationTest.TestGetPageBoolean_ReturnsJSON;
var
	R: Boolean;
	JSON: WideString;
begin
	R := FRetryOp.GetPageBoolean('http://test/api', 'Error: ', JSON);

	Assert.IsTrue(R);
	Assert.AreEqual(JSON_SUCCESS, JSON);
end;

{ GetPage tests }

procedure TRetryOperationTest.TestGetPage_Success;
var
	R: Boolean;
	JSON: WideString;
begin
	R := FRetryOp.GetPage('http://test/api', JSON);

	Assert.IsTrue(R);
	Assert.AreEqual(1, FMockContext.GetPageCount);
end;

procedure TRetryOperationTest.TestGetPage_ReturnsJSON;
var
	R: Boolean;
	JSON: WideString;
begin
	R := FRetryOp.GetPage('http://test/api', JSON);

	Assert.IsTrue(R);
	Assert.AreEqual(JSON_SUCCESS, JSON);
end;

initialization

TDUnitX.RegisterTestFixture(TIsTokenExpiredInJSONTest);
TDUnitX.RegisterTestFixture(TIsTokenExpiredResultTest);
TDUnitX.RegisterTestFixture(TAPICallResultTest);
TDUnitX.RegisterTestFixture(TRetryOperationTest);

end.
