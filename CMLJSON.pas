unit CMLJSON;

interface

uses JSON, CMLTypes, System.SysUtils;

const
	NAME_BODY = 'body';
	NAME_LIST = 'list';
	NAME_SIZE = 'size';
	NAME_KIND = 'kind';
	NAME_WEBLINK = 'weblink';
	NAME_TYPE = 'type';
	NAME_HOME = 'home';
	NAME_NAME = 'name'; //funny
	NAME_DELETED_AT = 'deleted_at';
	NAME_DELETED_FROM = 'deleted_from';
	NAME_DELETED_BY = 'deleted_by';
	NAME_GREV = 'grev';
	NAME_REV = 'rev';
	NAME_MTIME = 'mtime';
	NAME_VIRUS_SCAN = 'virus_scan';
	NAME_HASH = 'hash';
	NAME_TREE = 'tree';
	NAME_COUNT = 'count';
	NAME_EMAIL = 'email';
	NAME_STATUS = 'status';
	NAME_ACCESS = 'access';
	NAME_OVERQUOTA = 'overquota';
	NAME_TOTAL = 'total';
	NAME_USED = 'used';
	NAME_FOLDERS = 'folders';
	NAME_FILES = 'files';
	NAME_INVITED = 'invited';
	NAME_OWNER = 'owner';
	NAME_INVITE_TOKEN = 'invite_token';
	NAME_ERROR = 'error';
	NAME_ERROR_CODE = 'error_code';
	NAME_ERROR_DESCRIPTION = 'error_description';
	NAME_EXPIRES_IN = 'expires_in';
	NAME_REFRESH_TOKEN = 'refresh_token';
	NAME_ACCESS_TOKEN = 'access_token';
	NAME_INVITE = 'invite';
	NAME_INVITE_EMAIL = 'invite_email';
	NAME_GET = 'get';
	NAME_URL = 'url';
	NAME_FORM_NAME = 'form_name';
	NAME_AUTH_HOST = 'auth_host';
	NAME_SECSTEP_PHONE = 'secstep_phone';
	NAME_SECSTEP_PAGE = 'secstep_page';
	NAME_SECSTEP_CODE_FAIL = 'secstep_code_fail';
	NAME_SECSTEP_RESEND_FAIL = 'secstep_resend_fail';
	NAME_SECSTEP_RESEND_SUCCESS = 'secstep_resend_success';
	NAME_SECSTEP_TIMEOUT = 'secstep_timeout';
	NAME_SECSTEP_LOGIN = 'secstep_login';
	NAME_SECSTEP_DISPOSABLE_FAIL = 'secstep_disposable_fail';
	NAME_SECSTEP_SMSAPI_ERROR = 'secstep_smsapi_error';
	NAME_SECSTEP_CAPTCHA = 'secstep_captcha';
	NAME_TOTP_ENABLED = 'totp_enabled';
	NAME_LOCALE = 'locale';
	NAME_CLIENT = 'client';
	NAME_CSRF = 'csrf';
	NAME_DEVICE = 'device';

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
//function assignFromName(var Item: Variant; Value: TJSONValue; Name: string): Boolean;

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
		A := ((JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject).Values[NAME_LIST] as TJSONArray;
		SetLength(CloudMailRuDirListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with CloudMailRuDirListing[J] do
			begin
				if Assigned(ParserObj.Values[NAME_SIZE]) then
					size := ParserObj.Values[NAME_SIZE].Value.ToInt64;
				if Assigned(ParserObj.Values[NAME_KIND]) then
					kind := ParserObj.Values[NAME_KIND].Value;
				if Assigned(ParserObj.Values[NAME_WEBLINK]) then
					weblink := ParserObj.Values[NAME_WEBLINK].Value;
				if Assigned(ParserObj.Values[NAME_TYPE]) then
					type_ := ParserObj.Values[NAME_TYPE].Value;
				if Assigned(ParserObj.Values[NAME_HOME]) then
					home := ParserObj.Values[NAME_HOME].Value;
				if Assigned(ParserObj.Values[NAME_NAME]) then
					name := ParserObj.Values[NAME_NAME].Value;
				visible_name := name;
				if Assigned(ParserObj.Values[NAME_DELETED_AT]) then
					deleted_at := ParserObj.Values[NAME_DELETED_AT].Value.ToInteger;
				if Assigned(ParserObj.Values[NAME_DELETED_FROM]) then
					deleted_from := ParserObj.Values[NAME_DELETED_FROM].Value;
				if Assigned(ParserObj.Values[NAME_DELETED_BY]) then
					deleted_by := ParserObj.Values[NAME_DELETED_BY].Value.ToInteger;
				if Assigned(ParserObj.Values[NAME_GREV]) then
					grev := ParserObj.Values[NAME_GREV].Value.ToInteger;
				if Assigned(ParserObj.Values[NAME_REV]) then
					rev := ParserObj.Values[NAME_REV].Value.ToInteger;
				if (type_ = TYPE_FILE) then
				begin
					if Assigned(ParserObj.Values[NAME_MTIME]) then
						mtime := ParserObj.Values[NAME_MTIME].Value.ToInt64;
					if Assigned(ParserObj.Values[NAME_VIRUS_SCAN]) then
						virus_scan := ParserObj.Values[NAME_VIRUS_SCAN].Value;
					if Assigned(ParserObj.Values[NAME_HASH]) then
						hash := ParserObj.Values[NAME_HASH].Value;
				end else begin
					if Assigned(ParserObj.Values[NAME_TREE]) then
						tree := ParserObj.Values[NAME_TREE].Value;

					if Assigned(ParserObj.Values[NAME_COUNT]) then
					begin
						folders_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FOLDERS].Value.ToInteger();
						files_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FILES].Value.ToInteger();
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
		ParserObj := (JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject;
		with CloudMailRuDirListingItem do
		begin
			if Assigned(ParserObj.Values[NAME_SIZE]) then
				size := ParserObj.Values[NAME_SIZE].Value.ToInt64;
			if Assigned(ParserObj.Values[NAME_KIND]) then
				kind := ParserObj.Values[NAME_KIND].Value;
			if Assigned(ParserObj.Values[NAME_WEBLINK]) then
				weblink := ParserObj.Values[NAME_WEBLINK].Value;
			if Assigned(ParserObj.Values[NAME_TYPE]) then
				type_ := ParserObj.Values[NAME_TYPE].Value;
			if Assigned(ParserObj.Values[NAME_HOME]) then
				home := ParserObj.Values[NAME_HOME].Value;
			if Assigned(ParserObj.Values[NAME_NAME]) then
				name := ParserObj.Values[NAME_NAME].Value;
			if (type_ = TYPE_FILE) then
			begin
				if Assigned(ParserObj.Values[NAME_MTIME]) then
					mtime := ParserObj.Values[NAME_MTIME].Value.ToInteger;
				if Assigned(ParserObj.Values[NAME_VIRUS_SCAN]) then
					virus_scan := ParserObj.Values[NAME_VIRUS_SCAN].Value;
				if Assigned(ParserObj.Values[NAME_HASH]) then
					hash := ParserObj.Values[NAME_HASH].Value;
			end else begin
				if Assigned(ParserObj.Values[NAME_TREE]) then
					tree := ParserObj.Values[NAME_TREE].Value;
				if Assigned(ParserObj.Values[NAME_GREV]) then
					grev := ParserObj.Values[NAME_GREV].Value.ToInteger;
				if Assigned(ParserObj.Values[NAME_REV]) then
					rev := ParserObj.Values[NAME_REV].Value.ToInteger;
				if Assigned((ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FOLDERS]) then
					folders_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FOLDERS].Value.ToInteger();
				if Assigned((ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FILES]) then
					files_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FILES].Value.ToInteger();
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
		A := ((JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject).Values[NAME_INVITED] as TJSONArray;
		if not Assigned(A) then
			exit(true); //no invites
		SetLength(InviteListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with InviteListing[J] do
			begin
				if Assigned(ParserObj.Values[NAME_EMAIL]) then
					email := ParserObj.Values[NAME_EMAIL].Value;
				if Assigned(ParserObj.Values[NAME_STATUS]) then
					status := ParserObj.Values[NAME_STATUS].Value;
				if Assigned(ParserObj.Values[NAME_ACCESS]) then
					access := ParserObj.Values[NAME_ACCESS].Value;
				if Assigned(ParserObj.Values[NAME_NAME]) then
					name := ParserObj.Values[NAME_NAME].Value;
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
		A := ((JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject).Values[NAME_LIST] as TJSONArray;
		if not Assigned(A) then
			exit; //no invites
		SetLength(IncomingInviteListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with IncomingInviteListing[J] do
			begin
				if Assigned(ParserObj.Values[NAME_OWNER]) then
				begin
					OwnerObj := ParserObj.Values[NAME_OWNER] as TJSONObject;
					if Assigned(OwnerObj.Values[NAME_EMAIL]) then
						owner.email := OwnerObj.Values[NAME_EMAIL].Value;
					if Assigned(OwnerObj.Values[NAME_NAME]) then
						owner.Name := OwnerObj.Values[NAME_NAME].Value;
				end;

				if Assigned(ParserObj.Values[NAME_TREE]) then
					tree := ParserObj.Values[NAME_TREE].Value;
				if Assigned(ParserObj.Values[NAME_ACCESS]) then
					access := ParserObj.Values[NAME_ACCESS].Value;
				if Assigned(ParserObj.Values[NAME_NAME]) then
					name := ParserObj.Values[NAME_NAME].Value;
				if Assigned(ParserObj.Values[NAME_HOME]) then
					home := ParserObj.Values[NAME_HOME].Value;
				if Assigned(ParserObj.Values[NAME_SIZE]) then
					size := ParserObj.Values[NAME_SIZE].Value.ToInt64;
				if Assigned(ParserObj.Values[NAME_INVITE_TOKEN]) then
					invite_token := ParserObj.Values[NAME_INVITE_TOKEN].Value;
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
			if Assigned(ParserObj.Values[NAME_ERROR]) then
				error := ParserObj.Values[NAME_ERROR].Value;
			if Assigned(ParserObj.Values[NAME_ERROR_CODE]) then
				error_code := ParserObj.Values[NAME_ERROR_CODE].Value.ToInteger;
			if Assigned(ParserObj.Values[NAME_ERROR_DESCRIPTION]) then
				error_description := ParserObj.Values[NAME_ERROR_DESCRIPTION].Value;
			if Assigned(ParserObj.Values[NAME_EXPIRES_IN]) then
				expires_in := ParserObj.Values[NAME_EXPIRES_IN].Value.ToInteger;
			if Assigned(ParserObj.Values[NAME_REFRESH_TOKEN]) then
				refresh_token := ParserObj.Values[NAME_REFRESH_TOKEN].Value;
			if Assigned(ParserObj.Values[NAME_ACCESS_TOKEN]) then
				access_token := ParserObj.Values[NAME_ACCESS_TOKEN].Value;
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
		OperationStatus := ParserObj.Values[NAME_STATUS].Value.ToInteger;
		if OperationStatus <> 200 then
		begin
			//if OperationStatus = 400 then exit(CLOUD_ERROR_BAD_REQUEST);
			if OperationStatus = 451 then
				exit(CLOUD_ERROR_FAHRENHEIT);
			if OperationStatus = 507 then
				exit(CLOUD_ERROR_OVERQUOTA);
			if OperationStatus = 406 then
				exit(CLOUD_ERROR_NOT_ACCEPTABLE);

			if (Assigned((ParserObj.Values[NAME_BODY] as TJSONObject).Values[NAME_HOME])) then
				nodename := 'home'
			else if (Assigned((ParserObj.Values[NAME_BODY] as TJSONObject).Values[NAME_WEBLINK])) then
				nodename := 'weblink'
			else if (Assigned((ParserObj.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITE_EMAIL])) then
			begin //invite errors
				error := (((ParserObj.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITE_EMAIL]) as TJSONObject).Values[NAME_ERROR].Value;
			end else begin
				//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
				exit(CLOUD_ERROR_UNKNOWN);
			end;
			if error = EmptyWideStr then
				error := ((ParserObj.Values[NAME_BODY] as TJSONObject).Values[nodename] as TJSONObject).Values[NAME_ERROR].Value;
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
		PublicLink := (JSONVal as TJSONObject).Values[NAME_BODY].Value;
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
		Shard := ((((JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject).Values[NAME_GET] as TJSONArray).Items[0] as TJSONObject).Values[NAME_URL].Value;
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
			if Assigned(ParserObj.Values[NAME_FORM_NAME]) then
				form_name := ParserObj.Values[NAME_FORM_NAME].Value;
			if Assigned(ParserObj.Values[NAME_AUTH_HOST]) then
				auth_host := ParserObj.Values[NAME_AUTH_HOST].Value;
			if Assigned(ParserObj.Values[NAME_SECSTEP_PHONE]) then
				secstep_phone := ParserObj.Values[NAME_SECSTEP_PHONE].Value;
			if Assigned(ParserObj.Values[NAME_SECSTEP_PAGE]) then
				secstep_page := ParserObj.Values[NAME_SECSTEP_PAGE].Value;
			if Assigned(ParserObj.Values[NAME_SECSTEP_CODE_FAIL]) then
				secstep_code_fail := ParserObj.Values[NAME_SECSTEP_CODE_FAIL].Value;
			if Assigned(ParserObj.Values[NAME_SECSTEP_RESEND_FAIL]) then
				secstep_resend_fail := ParserObj.Values[NAME_SECSTEP_RESEND_FAIL].Value;
			if Assigned(ParserObj.Values[NAME_SECSTEP_RESEND_SUCCESS]) then
				secstep_resend_success := ParserObj.Values[NAME_SECSTEP_RESEND_SUCCESS].Value;
			if Assigned(ParserObj.Values[NAME_SECSTEP_TIMEOUT]) then
			begin
				if ParserObj.Values[NAME_SECSTEP_TIMEOUT].Value <> '' then
					secstep_timeout := ParserObj.Values[NAME_SECSTEP_TIMEOUT].Value.ToInt64
				else
					secstep_timeout := AUTH_APP_USED;
			end;
			if Assigned(ParserObj.Values[NAME_SECSTEP_LOGIN]) then
				secstep_login := ParserObj.Values[NAME_SECSTEP_LOGIN].Value;
			if Assigned(ParserObj.Values[NAME_SECSTEP_DISPOSABLE_FAIL]) then
				secstep_disposable_fail := ParserObj.Values[NAME_SECSTEP_DISPOSABLE_FAIL].Value;
			if Assigned(ParserObj.Values[NAME_SECSTEP_SMSAPI_ERROR]) then
				secstep_smsapi_error := ParserObj.Values[NAME_SECSTEP_SMSAPI_ERROR].Value;
			if Assigned(ParserObj.Values[NAME_SECSTEP_CAPTCHA]) then
				secstep_captcha := ParserObj.Values[NAME_SECSTEP_CAPTCHA].Value;
			if Assigned(ParserObj.Values[NAME_TOTP_ENABLED]) then
				totp_enabled := ParserObj.Values[NAME_TOTP_ENABLED].Value;
			if Assigned(ParserObj.Values[NAME_LOCALE]) then
				locale := ParserObj.Values[NAME_LOCALE].Value;
			if Assigned(ParserObj.Values[NAME_CLIENT]) then
				client := ParserObj.Values[NAME_CLIENT].Value;
			if Assigned(ParserObj.Values[NAME_CSRF]) then
				csrf := ParserObj.Values[NAME_CSRF].Value;
			if Assigned(ParserObj.Values[NAME_DEVICE]) then
				device := ParserObj.Values[NAME_DEVICE].Value;
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
		ParserObj := (JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject;
		with CloudMailRuSpaceInfo do
		begin
			if Assigned(ParserObj.Values[NAME_OVERQUOTA]) then
				overquota := ParserObj.Values[NAME_OVERQUOTA].Value.ToBoolean;
			if Assigned(ParserObj.Values[NAME_TOTAL]) then
				total := ParserObj.Values[NAME_TOTAL].Value.ToInt64;
			if Assigned(ParserObj.Values[NAME_USED]) then
				used := ParserObj.Values[NAME_USED].Value.ToInt64;
		end;
		JSONVal.free;
	except
		exit;
	end;
	Result := true;
end;

end.
