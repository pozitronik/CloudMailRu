unit TokenRetryHelper;

{Helper for automatic token refresh retry logic.
	TRetryOperation centralizes the duplicated pattern of checking for token
	expiration and retrying API operations after refreshing the CSRF token.}

interface

uses
	System.SysUtils;

type
	{Result of an API operation that may need token refresh}
	TAPICallResult = record
		Success: Boolean;
		JSON: WideString;
		ResultCode: Integer; {For methods returning Integer, 0 means not applicable}

		{Check if this result indicates token expiration}
		function NeedsTokenRefresh: Boolean;

		{Factory methods}
		class function FromBoolean(ASuccess: Boolean; const AJSON: WideString): TAPICallResult; static;
		class function FromInteger(AResultCode: Integer; const AJSON: WideString = ''): TAPICallResult; static;
	end;

	{Callback types}
	TAPIOperation = reference to function: TAPICallResult;
	TTokenRefreshFunc = reference to function: Boolean;

	{HTTP operation callbacks - abstracts HTTP layer from retry logic}
	THTTPPostFormFunc = reference to function(const URL, Params: WideString; var JSON: WideString): Boolean;
	THTTPGetPageFunc = reference to function(const URL: WideString; var JSON: WideString; var ShowProgress: Boolean): Boolean;

	{Result conversion callbacks}
	TResultToBooleanFunc = reference to function(const JSON, ErrorPrefix: WideString): Boolean;
	TResultToIntegerFunc = reference to function(const JSON, ErrorPrefix: WideString): Integer;

	{Handles API operations with automatic token refresh retry.
		Initialize once with callbacks, then use specialized methods for common patterns
		or Execute() for complex operations.}
	TRetryOperation = class
	private
		FRefreshToken: TTokenRefreshFunc;
		FPostForm: THTTPPostFormFunc;
		FGetPage: THTTPGetPageFunc;
		FToBoolean: TResultToBooleanFunc;
		FToInteger: TResultToIntegerFunc;
		FMaxRetries: Integer;
	public
		constructor Create(RefreshToken: TTokenRefreshFunc; PostForm: THTTPPostFormFunc; GetPage: THTTPGetPageFunc; ToBoolean: TResultToBooleanFunc; ToInteger: TResultToIntegerFunc; MaxRetries: Integer = 1);

		{Generic execute for complex operations that need custom logic}
		function Execute(Operation: TAPIOperation): TAPICallResult;

		{POST form and return Boolean success}
		function PostFormBoolean(const URL, Params, ErrorPrefix: WideString): Boolean;

		{POST form and return FS result code}
		function PostFormInteger(const URL, Params, ErrorPrefix: WideString): Integer;

		{GET page and return Boolean success. Returns JSON for further processing.}
		function GetPageBoolean(const URL, ErrorPrefix: WideString; out JSON: WideString; ShowProgress: Boolean = False): Boolean;

		{GET page without error prefix - for operations that handle errors themselves}
		function GetPage(const URL: WideString; out JSON: WideString; ShowProgress: Boolean = False): Boolean;
	end;

	{Check if JSON response indicates token expiration}
function IsTokenExpiredInJSON(const JSON: WideString): Boolean;

{Check if result code indicates token expiration}
function IsTokenExpiredResult(ResultCode: Integer): Boolean;

implementation

uses
	CloudConstants,
	WFXTypes,
	JSONHelper;

function IsTokenExpiredInJSON(const JSON: WideString): Boolean;
begin
	Result := NAME_TOKEN = getBodyError(JSON);
end;

function IsTokenExpiredResult(ResultCode: Integer): Boolean;
begin
	Result := ResultCode = CLOUD_ERROR_TOKEN_OUTDATED;
end;

{TAPICallResult}

function TAPICallResult.NeedsTokenRefresh: Boolean;
begin
	{Check both ResultCode (for file transfer operations that return CLOUD_ERROR_TOKEN_OUTDATED)
		and JSON error (for API calls that return token error in JSON body)}
	Result := IsTokenExpiredResult(ResultCode) or ((not Success) and IsTokenExpiredInJSON(JSON));
end;

class function TAPICallResult.FromBoolean(ASuccess: Boolean; const AJSON: WideString): TAPICallResult;
begin
	Result.Success := ASuccess;
	Result.JSON := AJSON;
	Result.ResultCode := 0;
end;

class function TAPICallResult.FromInteger(AResultCode: Integer; const AJSON: WideString): TAPICallResult;
begin
	Result.Success := AResultCode in [FS_FILE_OK, CLOUD_OPERATION_OK];
	Result.JSON := AJSON;
	Result.ResultCode := AResultCode;
end;

{TRetryOperation}

constructor TRetryOperation.Create(RefreshToken: TTokenRefreshFunc; PostForm: THTTPPostFormFunc; GetPage: THTTPGetPageFunc; ToBoolean: TResultToBooleanFunc; ToInteger: TResultToIntegerFunc; MaxRetries: Integer);
begin
	inherited Create;
	FRefreshToken := RefreshToken;
	FPostForm := PostForm;
	FGetPage := GetPage;
	FToBoolean := ToBoolean;
	FToInteger := ToInteger;
	FMaxRetries := MaxRetries;
end;

function TRetryOperation.Execute(Operation: TAPIOperation): TAPICallResult;
var
	RetryCount: Integer;
begin
	RetryCount := 0;
	repeat
		Result := Operation();
		if Result.NeedsTokenRefresh and (RetryCount < FMaxRetries) and FRefreshToken() then
			Inc(RetryCount)
		else
			Break;
	until False;
end;

function TRetryOperation.PostFormBoolean(const URL, Params, ErrorPrefix: WideString): Boolean;
var
	CallResult: TAPICallResult;
begin
	CallResult := Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := FPostForm(URL, Params, JSON) and FToBoolean(JSON, ErrorPrefix);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);
	Result := CallResult.Success;
end;

function TRetryOperation.PostFormInteger(const URL, Params, ErrorPrefix: WideString): Integer;
var
	CallResult: TAPICallResult;
begin
	CallResult := Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			ResultCode: Integer;
		begin
			if FPostForm(URL, Params, JSON) then
				ResultCode := FToInteger(JSON, ErrorPrefix)
			else
				ResultCode := FS_FILE_WRITEERROR;
			Result := TAPICallResult.FromInteger(ResultCode, JSON);
		end);
	Result := CallResult.ResultCode;
end;

function TRetryOperation.GetPageBoolean(const URL, ErrorPrefix: WideString; out JSON: WideString; ShowProgress: Boolean): Boolean;
var
	CallResult: TAPICallResult;
	LocalJSON: WideString;
	Progress: Boolean;
begin
	Progress := ShowProgress;
	LocalJSON := '';
	CallResult := Execute(
		function: TAPICallResult
		var
			Success: Boolean;
		begin
			Success := FGetPage(URL, LocalJSON, Progress);
			if Success then
				Success := FToBoolean(LocalJSON, ErrorPrefix);
			Result := TAPICallResult.FromBoolean(Success, LocalJSON);
		end);
	JSON := LocalJSON;
	Result := CallResult.Success;
end;

function TRetryOperation.GetPage(const URL: WideString; out JSON: WideString; ShowProgress: Boolean): Boolean;
var
	CallResult: TAPICallResult;
	LocalJSON: WideString;
	Progress: Boolean;
begin
	Progress := ShowProgress;
	LocalJSON := '';
	CallResult := Execute(
		function: TAPICallResult
		var
			Success: Boolean;
		begin
			Success := FGetPage(URL, LocalJSON, Progress);
			Result := TAPICallResult.FromBoolean(Success, LocalJSON);
		end);
	JSON := LocalJSON;
	Result := CallResult.Success;
end;

end.
