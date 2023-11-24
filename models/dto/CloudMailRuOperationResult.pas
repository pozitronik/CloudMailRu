unit CloudMailRuOperationResult;

interface

uses
	SysUtils,
	CMRConstants,
	JSONHelper,
	JSON;

type
	TCloudMailRuOperationResult = record
		OperationStatus: integer; //HTTP Code
		OperationResult: integer; //error code (mostly)
	end;

function getOperationResult(JSON: WideString): TCloudMailRuOperationResult;
function getRegistrationOperationResult(JSON: WideString): TCloudMailRuOperationResult;
function getRegistrationBody(JSON: WideString; var Body: WideString): Boolean;

implementation

function getOperationResult(JSON: WideString): TCloudMailRuOperationResult;
var
	JSONVal: TJSONObject;
	error, nodename: WideString;
begin
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		result.OperationStatus := JSONVal.Values[NAME_STATUS].Value.ToInteger;
		if result.OperationStatus <> 200 then
		begin
			//if result.OperationStatus = 400 then
			//result.OperationResult := CLOUD_ERROR_BAD_REQUEST;
			if result.OperationStatus = 451 then
				result.OperationResult := CLOUD_ERROR_FAHRENHEIT;
			if result.OperationStatus = 507 then
				result.OperationResult := CLOUD_ERROR_OVERQUOTA;
			if result.OperationStatus = 406 then
				result.OperationResult := (CLOUD_ERROR_NOT_ACCEPTABLE);

			if (Assigned((JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_HOME])) then
				nodename := 'home'
			else if (Assigned((JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_WEBLINK])) then
				nodename := 'weblink'
			else if (Assigned((JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITE_EMAIL])) then
			begin //invite errors
				error := (((JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITE_EMAIL]) as TJSONObject).Values[NAME_ERROR].Value;
			end else begin
				//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
				result.OperationResult := (CLOUD_ERROR_UNKNOWN);
			end;
			if error = EmptyWideStr then
				error := ((JSONVal.Values[NAME_BODY] as TJSONObject).Values[nodename] as TJSONObject).Values[NAME_ERROR].Value;
			if error = 'exists' then
				result.OperationResult := CLOUD_ERROR_EXISTS
			else if error = 'required' then
				result.OperationResult := CLOUD_ERROR_REQUIRED
			else if error = 'readonly' then
				result.OperationResult := CLOUD_ERROR_READONLY
			else if error = 'read_only' then
				result.OperationResult := CLOUD_ERROR_READONLY
			else if error = 'name_length_exceeded' then
				result.OperationResult := CLOUD_ERROR_NAME_LENGTH_EXCEEDED
			else if error = 'unknown' then
				result.OperationResult := CLOUD_ERROR_UNKNOWN
			else if error = 'overquota' then
				result.OperationResult := CLOUD_ERROR_OVERQUOTA
			else if error = 'quota_exceeded' then
				result.OperationResult := CLOUD_ERROR_OVERQUOTA
			else if error = 'invalid' then
				result.OperationResult := CLOUD_ERROR_INVALID
			else if error = 'not_exists' then
				result.OperationResult := CLOUD_ERROR_NOT_EXISTS
			else if error = 'own' then
				result.OperationResult := CLOUD_ERROR_OWN
			else if error = 'name_too_long' then
				result.OperationResult := CLOUD_ERROR_NAME_TOO_LONG
			else if error = 'virus_scan_fail' then
				result.OperationResult := CLOUD_ERROR_VIRUS_SCAN_FAIL
			else if error = 'owner' then
				result.OperationResult := CLOUD_ERROR_OWNER
			else if error = 'trees_conflict' then
				result.OperationResult := CLOUD_ERROR_TREES_CONFLICT
			else if error = 'user_limit_exceeded' then
				result.OperationResult := CLOUD_ERROR_USER_LIMIT_EXCEEDED
			else if error = 'export_limit_exceeded' then
				result.OperationResult := CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED
			else if error = 'unprocessable_entry' then
				result.OperationResult := CLOUD_ERROR_UNPROCESSABLE_ENTRY
			else
				result.OperationResult := CLOUD_ERROR_UNKNOWN; //Эту ошибку мы пока не встречали
		end
		else
			result.OperationResult := CLOUD_OPERATION_OK;

	except
		on E: {EJSON}Exception do
		begin
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			result.OperationResult := CLOUD_ERROR_UNKNOWN;
		end;
	end;
	JSONVal.free;
end;

function getRegistrationOperationResult(JSON: WideString): TCloudMailRuOperationResult;
var
	JSONVal: TJSONObject;
begin
	result.OperationResult := CLOUD_ERROR_UNKNOWN;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		result.OperationStatus := JSONVal.Values[NAME_STATUS].Value.ToInteger;
		case result.OperationStatus of
			200:
				begin
					result.OperationResult := CLOUD_OPERATION_OK;
				end;
			400:
				begin
					result.OperationResult := CLOUD_ERROR_BAD_REQUEST;
				end;
			else
				begin
					result.OperationResult := CLOUD_ERROR_UNKNOWN; //Эту ошибку мы пока не встречали
				end;
		end;

	except
		on E: {EJSON}Exception do
		begin
			result.OperationResult := CLOUD_ERROR_UNKNOWN;
		end;
	end;
	JSONVal.free;
end;

function getRegistrationBody(JSON: WideString; var Body: WideString): Boolean;
var
	JSONVal: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		Body := JSONVal.Values[NAME_BODY].Value;
	except
		Exit;
	end;
	result := true;
	JSONVal.free;
end;

end.
