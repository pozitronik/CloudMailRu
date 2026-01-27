unit CloudErrorMapper;

interface

uses
	CloudOperationResult,
	CloudOperationResultJsonAdapter,
	TCLogger;

type
	{Static helper class for mapping cloud API results to file system results.
		Centralizes error code mapping and error message generation.}
	TCloudErrorMapper = class
	public
		{Map cloud operation result to WFX file system result code.
			Logs errors if Logger is provided and ErrorPrefix is non-empty.}
		class function ToFsResult(CloudResult: TCloudOperationResult; Logger: ILogger; ErrorPrefix: WideString = ''): Integer; overload; static;
		{Parse JSON and map to WFX file system result code.}
		class function ToFsResult(JSON: WideString; Logger: ILogger; ErrorPrefix: WideString = ''): Integer; overload; static;
		{Map cloud operation result to boolean success/failure.
			Logs errors if Logger is provided and ErrorPrefix is non-empty.}
		class function ToBoolean(CloudResult: TCloudOperationResult; Logger: ILogger; ErrorPrefix: WideString = ''): Boolean; overload; static;
		{Parse JSON and map to boolean success/failure.}
		class function ToBoolean(JSON: WideString; Logger: ILogger; ErrorPrefix: WideString = ''): Boolean; overload; static;
		{Get human-readable error text for cloud error code.}
		class function ErrorCodeText(ErrorCode: Integer): WideString; static;
	end;

implementation

uses
	System.SysUtils,
	CloudConstants,
	WFXTypes,
	LanguageStrings;

{TCloudErrorMapper}

class function TCloudErrorMapper.ToFsResult(JSON: WideString; Logger: ILogger; ErrorPrefix: WideString): Integer;
var
	OperationResult: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON, OperationResult);
	Result := ToFsResult(OperationResult, Logger, ErrorPrefix);
end;

class function TCloudErrorMapper.ToFsResult(CloudResult: TCloudOperationResult; Logger: ILogger; ErrorPrefix: WideString): Integer;
begin
	case CloudResult.OperationResult of
		CLOUD_OPERATION_OK:
			Exit(FS_FILE_OK);
		CLOUD_ERROR_EXISTS:
			Exit(FS_FILE_EXISTS);
		CLOUD_ERROR_REQUIRED, CLOUD_ERROR_INVALID, CLOUD_ERROR_READONLY, CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
			Exit(FS_FILE_WRITEERROR);
		CLOUD_ERROR_UNKNOWN:
			Exit(FS_FILE_NOTSUPPORTED);
		CLOUD_ERROR_OVERQUOTA:
			begin
				if Assigned(Logger) then
					Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_INSUFFICIENT_STORAGE);
				Exit(FS_FILE_WRITEERROR);
			end;
		CLOUD_ERROR_NAME_TOO_LONG:
			begin
				if Assigned(Logger) then
					Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_NAME_TOO_LONG);
				Exit(FS_FILE_WRITEERROR);
			end;
		else
			begin {Unknown error code}
				if Assigned(Logger) and (ErrorPrefix <> EmptyWideStr) then
					Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s%s%d', [ErrorPrefix, ErrorCodeText(CloudResult.OperationResult), PREFIX_STATUS, CloudResult.OperationStatus]);
				Exit(FS_FILE_WRITEERROR);
			end;
	end;
end;

class function TCloudErrorMapper.ToBoolean(JSON: WideString; Logger: ILogger; ErrorPrefix: WideString): Boolean;
var
	OperationResult: TCloudOperationResult;
begin
	TCloudOperationResultJsonAdapter.Parse(JSON, OperationResult);
	Result := ToBoolean(OperationResult, Logger, ErrorPrefix);
end;

class function TCloudErrorMapper.ToBoolean(CloudResult: TCloudOperationResult; Logger: ILogger; ErrorPrefix: WideString): Boolean;
begin
	Result := CloudResult.ToBoolean;
	if not(Result) and (ErrorPrefix <> EmptyWideStr) and Assigned(Logger) then
		Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s%s%d', [ErrorPrefix, ErrorCodeText(CloudResult.OperationResult), PREFIX_STATUS, CloudResult.OperationStatus]);
end;

class function TCloudErrorMapper.ErrorCodeText(ErrorCode: Integer): WideString;
begin
	case ErrorCode of
		CLOUD_ERROR_EXISTS:
			Exit(ERR_CLOUD_ERROR_EXISTS);
		CLOUD_ERROR_REQUIRED:
			Exit(ERR_CLOUD_ERROR_REQUIRED);
		CLOUD_ERROR_INVALID:
			Exit(ERR_CLOUD_ERROR_INVALID);
		CLOUD_ERROR_READONLY:
			Exit(ERR_CLOUD_ERROR_READONLY);
		CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
			Exit(ERR_CLOUD_ERROR_NAME_LENGTH_EXCEEDED);
		CLOUD_ERROR_OVERQUOTA:
			Exit(ERR_CLOUD_ERROR_OVERQUOTA);
		CLOUD_ERROR_NOT_EXISTS:
			Exit(ERR_CLOUD_ERROR_NOT_EXISTS);
		CLOUD_ERROR_OWN:
			Exit(ERR_CLOUD_ERROR_OWN);
		CLOUD_ERROR_NAME_TOO_LONG:
			Exit(ERR_CLOUD_ERROR_NAME_TOO_LONG);
		CLOUD_ERROR_VIRUS_SCAN_FAIL:
			Exit(ERR_CLOUD_ERROR_VIRUS_SCAN_FAIL);
		CLOUD_ERROR_OWNER:
			Exit(ERR_CLOUD_ERROR_OWNER);
		CLOUD_ERROR_FAHRENHEIT:
			Exit(ERR_CLOUD_ERROR_FAHRENHEIT);
		CLOUD_ERROR_BAD_REQUEST:
			Exit(ERR_CLOUD_ERROR_BAD_REQUEST);
		CLOUD_ERROR_TREES_CONFLICT:
			Exit(ERR_CLOUD_ERROR_TREES_CONFLICT);
		CLOUD_ERROR_UNPROCESSABLE_ENTRY:
			Exit(ERR_CLOUD_ERROR_UNPROCESSABLE_ENTRY);
		CLOUD_ERROR_USER_LIMIT_EXCEEDED:
			Exit(ERR_CLOUD_ERROR_USER_LIMIT_EXCEEDED);
		CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED:
			Exit(ERR_CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED);
		CLOUD_ERROR_NOT_ACCEPTABLE:
			Exit(ERR_CLOUD_ERROR_NOT_ACCEPTABLE);
		else
			Exit(Format(ERR_CLOUD_ERROR_UNKNOWN, [ErrorCode]));
	end;
end;

end.
