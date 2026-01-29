unit TokenRetryHelper;

{Helper for automatic token refresh retry logic.
	TRetryOperation centralizes the duplicated pattern of checking for token
	expiration and retrying API operations after refreshing the CSRF token.}

interface

uses
	System.SysUtils,
	CloudContext;

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

	{Callback type for custom API operations}
	TAPIOperation = reference to function: TAPICallResult;

	{Interface for API operations with automatic token refresh retry}
	IRetryOperation = interface
		['{A7B3C8D1-E2F4-4A5B-9C6D-7E8F90A1B2C3}']

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

	{Handles API operations with automatic token refresh retry.
		Uses IRetryContext for HTTP operations and result mapping.}
	TRetryOperation = class(TInterfacedObject, IRetryOperation)
	private
		FContext: IRetryContext;
		FMaxRetries: Integer;
	public
		constructor Create(Context: IRetryContext; MaxRetries: Integer = 1);

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

constructor TRetryOperation.Create(Context: IRetryContext; MaxRetries: Integer);
begin
	inherited Create;
	FContext := Context;
	FMaxRetries := MaxRetries;
end;

function TRetryOperation.Execute(Operation: TAPIOperation): TAPICallResult;
var
	RetryCount: Integer;
begin
	RetryCount := 0;
	repeat
		Result := Operation();
		if Result.NeedsTokenRefresh and (RetryCount < FMaxRetries) and FContext.RefreshToken then
			Inc(RetryCount)
		else
			Break;
	until False;
end;

function TRetryOperation.PostFormBoolean(const URL, Params, ErrorPrefix: WideString): Boolean;
var
	CallResult: TAPICallResult;
	Context: IRetryContext;
begin
	Context := FContext;
	CallResult := Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			Success: Boolean;
		begin
			Success := Context.PostForm(URL, Params, JSON) and Context.ResultToBoolean(JSON, ErrorPrefix);
			Result := TAPICallResult.FromBoolean(Success, JSON);
		end);
	Result := CallResult.Success;
end;

function TRetryOperation.PostFormInteger(const URL, Params, ErrorPrefix: WideString): Integer;
var
	CallResult: TAPICallResult;
	Context: IRetryContext;
begin
	Context := FContext;
	CallResult := Execute(
		function: TAPICallResult
		var
			JSON: WideString;
			ResultCode: Integer;
		begin
			if Context.PostForm(URL, Params, JSON) then
				ResultCode := Context.ResultToInteger(JSON, ErrorPrefix)
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
	Context: IRetryContext;
begin
	Progress := ShowProgress;
	LocalJSON := '';
	Context := FContext;
	CallResult := Execute(
		function: TAPICallResult
		var
			Success: Boolean;
		begin
			Success := Context.GetPage(URL, LocalJSON, Progress);
			if Success then
				Success := Context.ResultToBoolean(LocalJSON, ErrorPrefix);
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
	Context: IRetryContext;
begin
	Progress := ShowProgress;
	LocalJSON := '';
	Context := FContext;
	CallResult := Execute(
		function: TAPICallResult
		var
			Success: Boolean;
		begin
			Success := Context.GetPage(URL, LocalJSON, Progress);
			Result := TAPICallResult.FromBoolean(Success, LocalJSON);
		end);
	JSON := LocalJSON;
	Result := CallResult.Success;
end;

end.
