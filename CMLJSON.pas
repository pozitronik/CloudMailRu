unit CMLJSON;

interface

uses JSON, CMLTypes, System.SysUtils;

function initJSONValue(JSON: WideString; var JSONVal: TJSONValue): Boolean;
function fromJSON_DirListing(JSON: WideString; var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean;
function fromJSON_UserSpace(JSON: WideString; var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean;
function fromJSON_FileStatus(JSON: WideString; var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean;
function fromJSON_Shard(JSON: WideString; var Shard: WideString): Boolean;
function fromJSON_OAuthTokenInfo(JSON: WideString; var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean;
function fromJSON_PublicLink(JSON: WideString; var PublicLink: WideString): Boolean;
function fromJSON_OperationResult(JSON: WideString; var OperationStatus: integer): integer;
function fromJSON_InviteListing(JSON: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
function fromJSON_IncomingInviteListing(JSON: WideString; var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean;
function fromJSON_TwostepData(JSON: WideString; var TwostepData: TCloudMailRuTwostepData): Boolean;

implementation

function initJSONValue(JSON: WideString; var JSONVal: TJSONValue): Boolean;
begin
	Result := false;
	JSONVal := nil;
	try
		JSONVal := TJSONObject.ParseJSONValue(JSON);
	except
		exit;
	end;
	Result := true;
end;

function fromJSON_DirListing(JSON: WideString; var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean;
var
	JSONVal: TJSONValue;
	ParserObj: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	Result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;

	try
		A := ((JSONVal as TJSONObject).Values['body'] as TJSONObject).Values['list'] as TJSONArray;
		SetLength(CloudMailRuDirListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with CloudMailRuDirListing[J] do
			begin
				if Assigned(ParserObj.Values['size']) then
					size := ParserObj.Values['size'].Value.ToInt64;
				if Assigned(ParserObj.Values['kind']) then
					kind := ParserObj.Values['kind'].Value;
				if Assigned(ParserObj.Values['weblink']) then
					weblink := ParserObj.Values['weblink'].Value;
				if Assigned(ParserObj.Values['type']) then
					type_ := ParserObj.Values['type'].Value;
				if Assigned(ParserObj.Values['home']) then
					home := ParserObj.Values['home'].Value;
				if Assigned(ParserObj.Values['name']) then
					name := ParserObj.Values['name'].Value;
				if Assigned(ParserObj.Values['deleted_at']) then
					deleted_at := ParserObj.Values['deleted_at'].Value.ToInteger;
				if Assigned(ParserObj.Values['deleted_from']) then
					deleted_from := ParserObj.Values['deleted_from'].Value;
				if Assigned(ParserObj.Values['deleted_by']) then
					deleted_by := ParserObj.Values['deleted_by'].Value.ToInteger;
				if Assigned(ParserObj.Values['grev']) then
					grev := ParserObj.Values['grev'].Value.ToInteger;
				if Assigned(ParserObj.Values['rev']) then
					rev := ParserObj.Values['rev'].Value.ToInteger;
				if (type_ = TYPE_FILE) then
				begin
					if Assigned(ParserObj.Values['mtime']) then
						mtime := ParserObj.Values['mtime'].Value.ToInt64;
					if Assigned(ParserObj.Values['virus_scan']) then
						virus_scan := ParserObj.Values['virus_scan'].Value;
					if Assigned(ParserObj.Values['hash']) then
						hash := ParserObj.Values['hash'].Value;
				end else begin
					if Assigned(ParserObj.Values['tree']) then
						tree := ParserObj.Values['tree'].Value;

					if Assigned(ParserObj.Values['count']) then
					begin
						folders_count := (ParserObj.Values['count'] as TJSONObject).Values['folders'].Value.ToInteger();
						files_count := (ParserObj.Values['count'] as TJSONObject).Values['files'].Value.ToInteger();
					end;
					mtime := 0;
				end;
			end;
		end;
		JSONVal.free;
	except
		exit;
	end;
	Result := true;
end;

function fromJSON_FileStatus(JSON: WideString; var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean;
var
	ParserObj: TJSONObject;
	JSONVal: TJSONValue;
begin
	Result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		ParserObj := (JSONVal as TJSONObject).Values['body'] as TJSONObject;
		with CloudMailRuDirListingItem do
		begin
			if Assigned(ParserObj.Values['size']) then
				size := ParserObj.Values['size'].Value.ToInt64;
			if Assigned(ParserObj.Values['kind']) then
				kind := ParserObj.Values['kind'].Value;
			if Assigned(ParserObj.Values['weblink']) then
				weblink := ParserObj.Values['weblink'].Value;
			if Assigned(ParserObj.Values['type']) then
				type_ := ParserObj.Values['type'].Value;
			if Assigned(ParserObj.Values['home']) then
				home := ParserObj.Values['home'].Value;
			if Assigned(ParserObj.Values['name']) then
				name := ParserObj.Values['name'].Value;
			if (type_ = TYPE_FILE) then
			begin
				if Assigned(ParserObj.Values['mtime']) then
					mtime := ParserObj.Values['mtime'].Value.ToInteger;
				if Assigned(ParserObj.Values['virus_scan']) then
					virus_scan := ParserObj.Values['virus_scan'].Value;
				if Assigned(ParserObj.Values['hash']) then
					hash := ParserObj.Values['hash'].Value;
			end else begin
				if Assigned(ParserObj.Values['tree']) then
					tree := ParserObj.Values['tree'].Value;
				if Assigned(ParserObj.Values['grev']) then
					grev := ParserObj.Values['grev'].Value.ToInteger;
				if Assigned(ParserObj.Values['rev']) then
					rev := ParserObj.Values['rev'].Value.ToInteger;
				if Assigned((ParserObj.Values['count'] as TJSONObject).Values['folders']) then
					folders_count := (ParserObj.Values['count'] as TJSONObject).Values['folders'].Value.ToInteger();
				if Assigned((ParserObj.Values['count'] as TJSONObject).Values['files']) then
					files_count := (ParserObj.Values['count'] as TJSONObject).Values['files'].Value.ToInteger();
				mtime := 0;
			end;
		end;
		JSONVal.free;
	except
		exit;
	end;
	Result := true;
end;

function fromJSON_InviteListing(JSON: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
var
	J: integer;
	A: TJSONArray;
	ParserObj: TJSONObject;
	JSONVal: TJSONValue;
begin
	Result := false;
	SetLength(InviteListing, 0);
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		A := ((JSONVal as TJSONObject).Values['body'] as TJSONObject).Values['invited'] as TJSONArray;
		if not Assigned(A) then
			exit(true); //no invites
		SetLength(InviteListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with InviteListing[J] do
			begin
				if Assigned(ParserObj.Values['email']) then
					email := ParserObj.Values['email'].Value;
				if Assigned(ParserObj.Values['status']) then
					status := ParserObj.Values['status'].Value;
				if Assigned(ParserObj.Values['access']) then
					access := ParserObj.Values['access'].Value;
				if Assigned(ParserObj.Values['name']) then
					name := ParserObj.Values['name'].Value;
			end;
		end;
		JSONVal.free;
	except
		on E: {EJSON}Exception do
		begin
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			exit;
		end;
	end;
	Result := true;
end;

function fromJSON_IncomingInviteListing(JSON: WideString; var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean;
var
	ParserObj: TJSONObject;
	JSONVal: TJSONValue;
	OwnerObj: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	Result := false;
	SetLength(IncomingInviteListing, 0);
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		A := ((JSONVal as TJSONObject).Values['body'] as TJSONObject).Values['list'] as TJSONArray;
		if not Assigned(A) then
			exit; //no invites
		SetLength(IncomingInviteListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with IncomingInviteListing[J] do
			begin
				if Assigned(ParserObj.Values['owner']) then
				begin
					OwnerObj := ParserObj.Values['owner'] as TJSONObject;
					if Assigned(OwnerObj.Values['email']) then
						owner.email := OwnerObj.Values['email'].Value;
					if Assigned(OwnerObj.Values['name']) then
						owner.name := OwnerObj.Values['name'].Value;
				end;

				if Assigned(ParserObj.Values['tree']) then
					tree := ParserObj.Values['tree'].Value;
				if Assigned(ParserObj.Values['access']) then
					access := ParserObj.Values['access'].Value;
				if Assigned(ParserObj.Values['name']) then
					name := ParserObj.Values['name'].Value;
				if Assigned(ParserObj.Values['home']) then
					home := ParserObj.Values['home'].Value;
				if Assigned(ParserObj.Values['size']) then
					size := ParserObj.Values['size'].Value.ToInt64;
				if Assigned(ParserObj.Values['invite_token']) then
					invite_token := ParserObj.Values['invite_token'].Value;
			end;
		end;
		JSONVal.free;
	except
		on E: {EJSON}Exception do
		begin
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			exit;
		end;
	end;
	Result := true;
end;

function fromJSON_OAuthTokenInfo(JSON: WideString; var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean;
var
	JSONVal: TJSONValue;
	ParserObj: TJSONObject;
begin
	Result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		ParserObj := (JSONVal as TJSONObject);
		with CloudMailRuOAuthInfo do
		begin
			if Assigned(ParserObj.Values['error']) then
				error := ParserObj.Values['error'].Value;
			if Assigned(ParserObj.Values['error_code']) then
				error_code := ParserObj.Values['error_code'].Value.ToInteger;
			if Assigned(ParserObj.Values['error_description']) then
				error_description := ParserObj.Values['error_description'].Value;
			if Assigned(ParserObj.Values['expires_in']) then
				expires_in := ParserObj.Values['expires_in'].Value.ToInteger;
			if Assigned(ParserObj.Values['refresh_token']) then
				refresh_token := ParserObj.Values['refresh_token'].Value;
			if Assigned(ParserObj.Values['access_token']) then
				access_token := ParserObj.Values['access_token'].Value;
		end;
		JSONVal.free;
	except
		on E: {EJSON}Exception do
		begin
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			CloudMailRuOAuthInfo.error_code := CLOUD_ERROR_UNKNOWN;
			CloudMailRuOAuthInfo.error := 'Answer parsing';
			CloudMailRuOAuthInfo.error_description := 'JSON parsing error: at ' + JSON;
			exit;
		end;
	end;
	Result := true;
end;

function fromJSON_OperationResult(JSON: WideString; var OperationStatus: integer): integer;
var
	error, nodename: WideString;
	ParserObj: TJSONObject;
	JSONVal: TJSONValue;
begin
	//Result:=CLOUD_ERROR_BAD_REQUEST;
	if not initJSONValue(JSON, JSONVal) then
		exit(CLOUD_ERROR_UNKNOWN);
	try
		ParserObj := JSONVal as TJSONObject;
		OperationStatus := ParserObj.Values['status'].Value.ToInteger;
		if OperationStatus <> 200 then
		begin
			//if OperationStatus = 400 then exit(CLOUD_ERROR_BAD_REQUEST);
			if OperationStatus = 451 then
				exit(CLOUD_ERROR_FAHRENHEIT);
			if OperationStatus = 507 then
				exit(CLOUD_ERROR_OVERQUOTA);
			if OperationStatus = 406 then
				exit(CLOUD_ERROR_NOT_ACCEPTABLE);

			if (Assigned((ParserObj.Values['body'] as TJSONObject).Values['home'])) then
				nodename := 'home'
			else if (Assigned((ParserObj.Values['body'] as TJSONObject).Values['weblink'])) then
				nodename := 'weblink'
			else if (Assigned((ParserObj.Values['body'] as TJSONObject).Values['invite.email'])) then
			begin //invite errors
				error := (((ParserObj.Values['body'] as TJSONObject).Values['invite.email']) as TJSONObject).Values['error'].Value;
			end else begin
				//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
				exit(CLOUD_ERROR_UNKNOWN);
			end;
			if error = '' then
				error := ((ParserObj.Values['body'] as TJSONObject).Values[nodename] as TJSONObject).Values['error'].Value;
			if error = 'exists' then
				exit(CLOUD_ERROR_EXISTS);
			if error = 'required' then
				exit(CLOUD_ERROR_REQUIRED);
			if error = 'readonly' then
				exit(CLOUD_ERROR_READONLY);
			if error = 'read_only' then
				exit(CLOUD_ERROR_READONLY);
			if error = 'name_length_exceeded' then
				exit(CLOUD_ERROR_NAME_LENGTH_EXCEEDED);
			if error = 'unknown' then
				exit(CLOUD_ERROR_UNKNOWN);
			if error = 'overquota' then
				exit(CLOUD_ERROR_OVERQUOTA);
			if error = 'quota_exceeded' then
				exit(CLOUD_ERROR_OVERQUOTA);
			if error = 'invalid' then
				exit(CLOUD_ERROR_INVALID);
			if error = 'not_exists' then
				exit(CLOUD_ERROR_NOT_EXISTS);
			if error = 'own' then
				exit(CLOUD_ERROR_OWN);
			if error = 'name_too_long' then
				exit(CLOUD_ERROR_NAME_TOO_LONG);
			if error = 'virus_scan_fail' then
				exit(CLOUD_ERROR_VIRUS_SCAN_FAIL);
			if error = 'owner' then
				exit(CLOUD_ERROR_OWNER);
			if error = 'trees_conflict' then
				exit(CLOUD_ERROR_TREES_CONFLICT);
			if error = 'user_limit_exceeded' then
				exit(CLOUD_ERROR_USER_LIMIT_EXCEEDED);
			if error = 'export_limit_exceeded' then
				exit(CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED);
			if error = 'unprocessable_entry' then
				exit(CLOUD_ERROR_UNPROCESSABLE_ENTRY);

			exit(CLOUD_ERROR_UNKNOWN); //Эту ошибку мы пока не встречали

		end;
		JSONVal.free;
	except
		on E: {EJSON}Exception do
		begin
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			exit(CLOUD_ERROR_UNKNOWN);
		end;
	end;
	Result := CLOUD_OPERATION_OK;
end;

function fromJSON_PublicLink(JSON: WideString; var PublicLink: WideString): Boolean;
var
	JSONVal: TJSONValue;
begin
	Result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		PublicLink := (JSONVal as TJSONObject).Values['body'].Value;
		JSONVal.free;
	except
		exit;
	end;
	Result := true;
end;

function fromJSON_Shard(JSON: WideString; var Shard: WideString): Boolean;
var
	JSONVal: TJSONValue;
begin
	Result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		Shard := ((((JSONVal as TJSONObject).Values['body'] as TJSONObject).Values['get'] as TJSONArray).Items[0] as TJSONObject).Values['url'].Value;
		JSONVal.free;
	except
		exit;
	end;
	Result := true;
end;

function fromJSON_TwostepData(JSON: WideString; var TwostepData: TCloudMailRuTwostepData): Boolean;
var
	ParserObj: TJSONObject;
	JSONVal: TJSONValue;
begin
	Result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		ParserObj := (JSONVal as TJSONObject) as TJSONObject;
		with TwostepData do
		begin
			if Assigned(ParserObj.Values['form_name']) then
				form_name := ParserObj.Values['form_name'].Value;
			if Assigned(ParserObj.Values['auth_host']) then
				auth_host := ParserObj.Values['auth_host'].Value;
			if Assigned(ParserObj.Values['secstep_phone']) then
				secstep_phone := ParserObj.Values['secstep_phone'].Value;
			if Assigned(ParserObj.Values['secstep_page']) then
				secstep_page := ParserObj.Values['secstep_page'].Value;
			if Assigned(ParserObj.Values['secstep_code_fail']) then
				secstep_code_fail := ParserObj.Values['secstep_code_fail'].Value;
			if Assigned(ParserObj.Values['secstep_resend_fail']) then
				secstep_resend_fail := ParserObj.Values['secstep_resend_fail'].Value;
			if Assigned(ParserObj.Values['secstep_resend_success']) then
				secstep_resend_success := ParserObj.Values['secstep_resend_success'].Value;
			if Assigned(ParserObj.Values['secstep_timeout']) then
			begin
				if ParserObj.Values['secstep_timeout'].Value <> '' then
					secstep_timeout := ParserObj.Values['secstep_timeout'].Value.ToInt64
				else
					secstep_timeout := AUTH_APP_USED;
			end;
			if Assigned(ParserObj.Values['secstep_login']) then
				secstep_login := ParserObj.Values['secstep_login'].Value;
			if Assigned(ParserObj.Values['secstep_disposable_fail']) then
				secstep_disposable_fail := ParserObj.Values['secstep_disposable_fail'].Value;
			if Assigned(ParserObj.Values['secstep_smsapi_error']) then
				secstep_smsapi_error := ParserObj.Values['secstep_smsapi_error'].Value;
			if Assigned(ParserObj.Values['secstep_captcha']) then
				secstep_captcha := ParserObj.Values['secstep_captcha'].Value;
			if Assigned(ParserObj.Values['totp_enabled']) then
				totp_enabled := ParserObj.Values['totp_enabled'].Value;
			if Assigned(ParserObj.Values['locale']) then
				locale := ParserObj.Values['locale'].Value;
			if Assigned(ParserObj.Values['client']) then
				client := ParserObj.Values['client'].Value;
			if Assigned(ParserObj.Values['csrf']) then
				csrf := ParserObj.Values['csrf'].Value;
			if Assigned(ParserObj.Values['device']) then
				device := ParserObj.Values['device'].Value;
		end;
		JSONVal.free;
	except
		exit;
	end;
	Result := true;
end;

function fromJSON_UserSpace(JSON: WideString; var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean;
var
	ParserObj: TJSONObject;
	JSONVal: TJSONValue;
begin
	Result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		ParserObj := (JSONVal as TJSONObject).Values['body'] as TJSONObject;
		with CloudMailRuSpaceInfo do
		begin
			if Assigned(ParserObj.Values['overquota']) then
				overquota := ParserObj.Values['overquota'].Value.ToBoolean;
			if Assigned(ParserObj.Values['total']) then
				total := ParserObj.Values['total'].Value.ToInt64;
			if Assigned(ParserObj.Values['used']) then
				used := ParserObj.Values['used'].Value.ToInt64;
		end;
		JSONVal.free;
	except
		exit;
	end;
	Result := true;
end;

end.
