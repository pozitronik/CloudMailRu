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
	private
		class function MapErrorToResult(const Error: WideString): Integer; static;
	end;

implementation

uses
	CloudConstants,
	SafeJSON;

class function TCloudOperationResultJsonAdapter.MapErrorToResult(const Error: WideString): Integer;
begin
	if Error = 'exists' then
		Result := CLOUD_ERROR_EXISTS
	else if Error = 'required' then
		Result := CLOUD_ERROR_REQUIRED
	else if Error = 'readonly' then
		Result := CLOUD_ERROR_READONLY
	else if Error = 'read_only' then
		Result := CLOUD_ERROR_READONLY
	else if Error = 'name_length_exceeded' then
		Result := CLOUD_ERROR_NAME_LENGTH_EXCEEDED
	else if Error = 'unknown' then
		Result := CLOUD_ERROR_UNKNOWN
	else if Error = 'overquota' then
		Result := CLOUD_ERROR_OVERQUOTA
	else if Error = 'quota_exceeded' then
		Result := CLOUD_ERROR_OVERQUOTA
	else if Error = 'invalid' then
		Result := CLOUD_ERROR_INVALID
	else if Error = 'not_exists' then
		Result := CLOUD_ERROR_NOT_EXISTS
	else if Error = 'own' then
		Result := CLOUD_ERROR_OWN
	else if Error = 'name_too_long' then
		Result := CLOUD_ERROR_NAME_TOO_LONG
	else if Error = 'virus_scan_fail' then
		Result := CLOUD_ERROR_VIRUS_SCAN_FAIL
	else if Error = 'owner' then
		Result := CLOUD_ERROR_OWNER
	else if Error = 'trees_conflict' then
		Result := CLOUD_ERROR_TREES_CONFLICT
	else if Error = 'user_limit_exceeded' then
		Result := CLOUD_ERROR_USER_LIMIT_EXCEEDED
	else if Error = 'export_limit_exceeded' then
		Result := CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED
	else if Error = 'unprocessable_entry' then
		Result := CLOUD_ERROR_UNPROCESSABLE_ENTRY
	else
		Result := CLOUD_ERROR_UNKNOWN;
end;

class procedure TCloudOperationResultJsonAdapter.Parse(const JSON: WideString; out OperationResult: TCloudOperationResult);
var
	Root, Body, ErrorNode: TSafeJSON;
	Error: WideString;
begin
	OperationResult := Default(TCloudOperationResult);
	OperationResult.OperationResult := CLOUD_ERROR_UNKNOWN;

	Root := TSafeJSON.Parse(JSON);
	try
		if Root.IsNull then
			Exit;

		OperationResult.OperationStatus := Root.Get(NAME_STATUS).AsInt;

		{Success}
		if OperationResult.OperationStatus = 200 then
		begin
			OperationResult.OperationResult := CLOUD_OPERATION_OK;
			Exit;
		end;

		{HTTP status codes with defined meanings}
		case OperationResult.OperationStatus of
			451:
				begin
					OperationResult.OperationResult := CLOUD_ERROR_FAHRENHEIT;
					Exit;
				end;
			507:
				begin
					OperationResult.OperationResult := CLOUD_ERROR_OVERQUOTA;
					Exit;
				end;
			406:
				begin
					OperationResult.OperationResult := CLOUD_ERROR_NOT_ACCEPTABLE;
					Exit;
				end;
		end;

		{Parse error from response body - try different body structures}
		Body := Root.Get(NAME_BODY);
		if Body.IsNull or not Body.IsObject then
			Exit;

		{Try body.home.error}
		ErrorNode := Body.Get(NAME_HOME);
		if not ErrorNode.IsNull then
		begin
			Error := ErrorNode.Get(NAME_ERROR).AsString;
			if Error <> '' then
			begin
				OperationResult.OperationResult := MapErrorToResult(Error);
				Exit;
			end;
		end;

		{Try body.weblink.error}
		ErrorNode := Body.Get(NAME_WEBLINK);
		if not ErrorNode.IsNull then
		begin
			Error := ErrorNode.Get(NAME_ERROR).AsString;
			if Error <> '' then
			begin
				OperationResult.OperationResult := MapErrorToResult(Error);
				Exit;
			end;
		end;

		{Try body.invite_email.error}
		ErrorNode := Body.Get(NAME_INVITE_EMAIL);
		if not ErrorNode.IsNull then
		begin
			Error := ErrorNode.Get(NAME_ERROR).AsString;
			if Error <> '' then
			begin
				OperationResult.OperationResult := MapErrorToResult(Error);
				Exit;
			end;
		end;

		{Unknown body structure - keep CLOUD_ERROR_UNKNOWN}
	finally
		Root.Free;
	end;
end;

class function TCloudOperationResultJsonAdapter.ParseRegistration(const JSON: WideString; out OperationResult: TCloudOperationResult): Boolean;
var
	Root: TSafeJSON;
begin
	Result := False;
	OperationResult := Default(TCloudOperationResult);
	OperationResult.OperationResult := CLOUD_ERROR_UNKNOWN;

	Root := TSafeJSON.Parse(JSON);
	try
		if Root.IsNull then
			Exit;

		OperationResult.OperationStatus := Root.Get(NAME_STATUS).AsInt;

		case OperationResult.OperationStatus of
			200:
				begin
					OperationResult.OperationResult := CLOUD_OPERATION_OK;
					Result := True;
				end;
			400:
				OperationResult.OperationResult := CLOUD_ERROR_BAD_REQUEST;
		else
			OperationResult.OperationResult := CLOUD_ERROR_UNKNOWN;
		end;
	finally
		Root.Free;
	end;
end;

end.
