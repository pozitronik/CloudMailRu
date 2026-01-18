unit TokenRetryHelper;

{Helper functions for automatic token refresh retry logic.
 Centralizes the duplicated pattern of checking for token expiration
 and retrying API operations after refreshing the CSRF token.}

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

	{Callback type for API operations}
	TAPIOperation = reference to function: TAPICallResult;

	{Callback type for token refresh}
	TTokenRefreshFunc = reference to function: Boolean;

{Execute an API operation with automatic token refresh retry.
 If the operation fails due to token expiration, refreshes the token
 and retries up to MaxRetries times.
 @param Operation The API operation to execute
 @param RefreshToken Function that refreshes the token, returns True on success
 @param MaxRetries Maximum number of retry attempts (default 1)
 @return Final result after retries}
function ExecuteWithTokenRetry(
	Operation: TAPIOperation;
	RefreshToken: TTokenRefreshFunc;
	MaxRetries: Integer = 1): TAPICallResult;

{Check if JSON response indicates token expiration}
function IsTokenExpiredInJSON(const JSON: WideString): Boolean;

{Check if result code indicates token expiration}
function IsTokenExpiredResult(ResultCode: Integer): Boolean;

implementation

uses
	CMRConstants,
	PLUGIN_TYPES,
	JSONHelper;

function IsTokenExpiredInJSON(const JSON: WideString): Boolean;
begin
	Result := NAME_TOKEN = getBodyError(JSON);
end;

function IsTokenExpiredResult(ResultCode: Integer): Boolean;
begin
	Result := ResultCode = CLOUD_ERROR_TOKEN_OUTDATED;
end;

{ TAPICallResult }

function TAPICallResult.NeedsTokenRefresh: Boolean;
begin
	{Check error code first (for file transfer operations)}
	if ResultCode <> 0 then
		Result := IsTokenExpiredResult(ResultCode)
	else
		{For Boolean operations, check JSON error when operation failed}
		Result := (not Success) and IsTokenExpiredInJSON(JSON);
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

function ExecuteWithTokenRetry(Operation: TAPIOperation; RefreshToken: TTokenRefreshFunc; MaxRetries: Integer): TAPICallResult;
var
	RetryCount: Integer;
begin
	RetryCount := 0;
	repeat
		Result := Operation();
		if Result.NeedsTokenRefresh and (RetryCount < MaxRetries) and RefreshToken() then
			Inc(RetryCount)
		else
			Break;
	until False;
end;

end.
