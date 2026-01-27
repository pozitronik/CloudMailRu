unit CloudOperationResultJsonAdapter;

{Adapter to parse JSON into TCloudOperationResult records.
	Separates JSON parsing (infrastructure concern) from the domain object.}

interface

uses
	CloudOperationResult;

type
	TCloudOperationResultJsonAdapter = class
	public
		class procedure Parse(const JSON: WideString; out OperationResult: TCloudOperationResult); static;
		class function ParseRegistration(const JSON: WideString; out OperationResult: TCloudOperationResult): Boolean; static;
	end;

implementation

uses
	SysUtils,
	CloudConstants,
	LanguageStrings,
	JSONHelper,
	JSON;

class procedure TCloudOperationResultJsonAdapter.Parse(const JSON: WideString; out OperationResult: TCloudOperationResult);
var
	JSONVal: TJSONObject;
	error, nodename: WideString;
begin
	{Initialize with safe defaults}
	OperationResult.OperationStatus := 0;
	OperationResult.OperationResult := CLOUD_ERROR_UNKNOWN;

	JSONVal := nil;
	try
		try
			if not init(JSON, JSONVal) then
				Exit;
			OperationResult.OperationStatus := JSONVal.Values[NAME_STATUS].Value.ToInteger;
			if OperationResult.OperationStatus = 200 then
			begin
				OperationResult.OperationResult := CLOUD_OPERATION_OK;
				Exit;
			end;

			{Handle HTTP status codes that have defined meanings regardless of body content}
			if OperationResult.OperationStatus = 451 then
			begin
				OperationResult.OperationResult := CLOUD_ERROR_FAHRENHEIT;
				Exit;
			end;
			if OperationResult.OperationStatus = 507 then
			begin
				OperationResult.OperationResult := CLOUD_ERROR_OVERQUOTA;
				Exit;
			end;
			if OperationResult.OperationStatus = 406 then
			begin
				OperationResult.OperationResult := CLOUD_ERROR_NOT_ACCEPTABLE;
				Exit;
			end;

			{Parse error from response body for other status codes}
			error := EmptyWideStr;
			if Assigned((JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_HOME]) then
				nodename := 'home'
			else if Assigned((JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_WEBLINK]) then
				nodename := 'weblink'
			else if Assigned((JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITE_EMAIL]) then
			begin
				error := (((JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITE_EMAIL]) as TJSONObject).Values[NAME_ERROR].Value;
			end else begin
				{Unknown body structure - keep CLOUD_ERROR_UNKNOWN from initialization}
				Exit;
			end;

			if error = EmptyWideStr then
				error := ((JSONVal.Values[NAME_BODY] as TJSONObject).Values[nodename] as TJSONObject).Values[NAME_ERROR].Value;

			if error = 'exists' then
				OperationResult.OperationResult := CLOUD_ERROR_EXISTS
			else if error = 'required' then
				OperationResult.OperationResult := CLOUD_ERROR_REQUIRED
			else if error = 'readonly' then
				OperationResult.OperationResult := CLOUD_ERROR_READONLY
			else if error = 'read_only' then
				OperationResult.OperationResult := CLOUD_ERROR_READONLY
			else if error = 'name_length_exceeded' then
				OperationResult.OperationResult := CLOUD_ERROR_NAME_LENGTH_EXCEEDED
			else if error = 'unknown' then
				OperationResult.OperationResult := CLOUD_ERROR_UNKNOWN
			else if error = 'overquota' then
				OperationResult.OperationResult := CLOUD_ERROR_OVERQUOTA
			else if error = 'quota_exceeded' then
				OperationResult.OperationResult := CLOUD_ERROR_OVERQUOTA
			else if error = 'invalid' then
				OperationResult.OperationResult := CLOUD_ERROR_INVALID
			else if error = 'not_exists' then
				OperationResult.OperationResult := CLOUD_ERROR_NOT_EXISTS
			else if error = 'own' then
				OperationResult.OperationResult := CLOUD_ERROR_OWN
			else if error = 'name_too_long' then
				OperationResult.OperationResult := CLOUD_ERROR_NAME_TOO_LONG
			else if error = 'virus_scan_fail' then
				OperationResult.OperationResult := CLOUD_ERROR_VIRUS_SCAN_FAIL
			else if error = 'owner' then
				OperationResult.OperationResult := CLOUD_ERROR_OWNER
			else if error = 'trees_conflict' then
				OperationResult.OperationResult := CLOUD_ERROR_TREES_CONFLICT
			else if error = 'user_limit_exceeded' then
				OperationResult.OperationResult := CLOUD_ERROR_USER_LIMIT_EXCEEDED
			else if error = 'export_limit_exceeded' then
				OperationResult.OperationResult := CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED
			else if error = 'unprocessable_entry' then
				OperationResult.OperationResult := CLOUD_ERROR_UNPROCESSABLE_ENTRY;
			{Unknown errors keep CLOUD_ERROR_UNKNOWN from initialization}

		except
			on E: Exception do
			begin
				{Keep CLOUD_ERROR_UNKNOWN from initialization}
			end;
		end;
	finally
		JSONVal.Free;
	end;
end;

class function TCloudOperationResultJsonAdapter.ParseRegistration(const JSON: WideString; out OperationResult: TCloudOperationResult): Boolean;
var
	JSONVal: TJSONObject;
begin
	Result := False;
	OperationResult.OperationResult := CLOUD_ERROR_UNKNOWN;
	JSONVal := nil;
	try
		try
			if not init(JSON, JSONVal) then
				Exit;
			OperationResult.OperationStatus := JSONVal.Values[NAME_STATUS].Value.ToInteger;
			case OperationResult.OperationStatus of
				200:
					begin
						OperationResult.OperationResult := CLOUD_OPERATION_OK;
						Result := True;
					end;
				400:
					begin
						OperationResult.OperationResult := CLOUD_ERROR_BAD_REQUEST;
					end;
				else
					begin
						OperationResult.OperationResult := CLOUD_ERROR_UNKNOWN;
					end;
			end;
		except
			OperationResult.OperationResult := CLOUD_ERROR_UNKNOWN;
		end;
	finally
		JSONVal.Free;
	end;
end;

end.
