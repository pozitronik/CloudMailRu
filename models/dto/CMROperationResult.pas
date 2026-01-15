unit CMROperationResult;

interface

uses
	SysUtils,
	CMRConstants,
	LANGUAGE_STRINGS,
	TCLogger,
	PLUGIN_TYPES,
	JSONHelper,
	JSON;

type
	TCMROperationResult = record
		OperationStatus: Integer; //HTTP Code
		OperationResult: Integer; //error code (mostly)

		procedure FromJSON(JSON: WideString); //this always prepare a record, even if JSON is not valid
		function ToBoolean: Boolean; overload;
		class function GetOperationResult(JSON: WideString): TCMROperationResult; static;
		class function GetRegistrationOperationResult(JSON: WideString): TCMROperationResult; static;
		class function ToBoolean(JSON: WideString): Boolean; overload; static;

	end;

implementation

{TCloudMailRuOperationResult}

procedure TCMROperationResult.FromJSON(JSON: WideString);
var
	JSONVal: TJSONObject;
	error, nodename: WideString;
begin
	{ Initialize with safe defaults }
	self.OperationStatus := 0;
	self.OperationResult := CLOUD_ERROR_UNKNOWN;

	try
		if (not init(JSON, JSONVal)) then
			Exit;
		self.OperationStatus := JSONVal.Values[NAME_STATUS].Value.ToInteger;
		if self.OperationStatus = 200 then
		begin
			self.OperationResult := CLOUD_OPERATION_OK;
			Exit;
		end;

		{ Handle HTTP status codes that have defined meanings regardless of body content }
		if self.OperationStatus = 451 then
		begin
			self.OperationResult := CLOUD_ERROR_FAHRENHEIT;
			Exit;
		end;
		if self.OperationStatus = 507 then
		begin
			self.OperationResult := CLOUD_ERROR_OVERQUOTA;
			Exit;
		end;
		if self.OperationStatus = 406 then
		begin
			self.OperationResult := CLOUD_ERROR_NOT_ACCEPTABLE;
			Exit;
		end;

		{ Parse error from response body for other status codes }
		error := EmptyWideStr;
		if (Assigned((JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_HOME])) then
			nodename := 'home'
		else if (Assigned((JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_WEBLINK])) then
			nodename := 'weblink'
		else if (Assigned((JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITE_EMAIL])) then
		begin
			error := (((JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITE_EMAIL]) as TJSONObject).Values[NAME_ERROR].Value;
		end else begin
			{ Unknown body structure - keep CLOUD_ERROR_UNKNOWN from initialization }
			Exit;
		end;

		if error = EmptyWideStr then
			error := ((JSONVal.Values[NAME_BODY] as TJSONObject).Values[nodename] as TJSONObject).Values[NAME_ERROR].Value;

		if error = 'exists' then
			self.OperationResult := CLOUD_ERROR_EXISTS
		else if error = 'required' then
			self.OperationResult := CLOUD_ERROR_REQUIRED
		else if error = 'readonly' then
			self.OperationResult := CLOUD_ERROR_READONLY
		else if error = 'read_only' then
			self.OperationResult := CLOUD_ERROR_READONLY
		else if error = 'name_length_exceeded' then
			self.OperationResult := CLOUD_ERROR_NAME_LENGTH_EXCEEDED
		else if error = 'unknown' then
			self.OperationResult := CLOUD_ERROR_UNKNOWN
		else if error = 'overquota' then
			self.OperationResult := CLOUD_ERROR_OVERQUOTA
		else if error = 'quota_exceeded' then
			self.OperationResult := CLOUD_ERROR_OVERQUOTA
		else if error = 'invalid' then
			self.OperationResult := CLOUD_ERROR_INVALID
		else if error = 'not_exists' then
			self.OperationResult := CLOUD_ERROR_NOT_EXISTS
		else if error = 'own' then
			self.OperationResult := CLOUD_ERROR_OWN
		else if error = 'name_too_long' then
			self.OperationResult := CLOUD_ERROR_NAME_TOO_LONG
		else if error = 'virus_scan_fail' then
			self.OperationResult := CLOUD_ERROR_VIRUS_SCAN_FAIL
		else if error = 'owner' then
			self.OperationResult := CLOUD_ERROR_OWNER
		else if error = 'trees_conflict' then
			self.OperationResult := CLOUD_ERROR_TREES_CONFLICT
		else if error = 'user_limit_exceeded' then
			self.OperationResult := CLOUD_ERROR_USER_LIMIT_EXCEEDED
		else if error = 'export_limit_exceeded' then
			self.OperationResult := CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED
		else if error = 'unprocessable_entry' then
			self.OperationResult := CLOUD_ERROR_UNPROCESSABLE_ENTRY;
		{ Unknown errors keep CLOUD_ERROR_UNKNOWN from initialization }

	except
		on E: Exception do
		begin
			{ Keep CLOUD_ERROR_UNKNOWN from initialization }
		end;
	end;
	JSONVal.free;
end;

class function TCMROperationResult.GetOperationResult(JSON: WideString): TCMROperationResult;
begin
	result.FromJSON(JSON);
end;

class function TCMROperationResult.GetRegistrationOperationResult(JSON: WideString): TCMROperationResult;
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

function TCMROperationResult.ToBoolean: Boolean;
begin
	result := self.OperationResult = CLOUD_OPERATION_OK
end;

class function TCMROperationResult.ToBoolean(JSON: WideString): Boolean;
begin
	result := TCMROperationResult.GetOperationResult(JSON).ToBoolean;
end;

end.
