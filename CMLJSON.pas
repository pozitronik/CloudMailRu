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
	NAME_TOTAL = 'bytes_total';
	NAME_USED = 'bytes_used';
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

implementation

function assignFromName(ParserObject: TJSONObject; Name: WideString): Variant;
begin
	if Assigned(ParserObject.Values[Name]) then
		result := ParserObject.Values[Name].Value;
end;

function initJSONValue(JSON: WideString; var JSONVal: TJSONValue): Boolean;
begin
	result := false;
	JSONVal := nil;
	try
		JSONVal := TJSONObject.ParseJSONValue(JSON);
	except
		exit;
	end;
	result := true;
end;

function fromJSON_DirListing(JSON: WideString; var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean;
var
	JSONVal: TJSONValue;
	ParserObj: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	result := false;
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
				size := assignFromName(ParserObj, NAME_SIZE);
				kind := assignFromName(ParserObj, NAME_KIND);
				weblink := assignFromName(ParserObj, NAME_WEBLINK);
				type_ := assignFromName(ParserObj, NAME_TYPE);
				home := assignFromName(ParserObj, NAME_HOME);
				name := assignFromName(ParserObj, NAME_NAME);
				visible_name := name;
				deleted_at := assignFromName(ParserObj, NAME_DELETED_AT);
				deleted_from := assignFromName(ParserObj, NAME_DELETED_FROM);
				deleted_by := assignFromName(ParserObj, NAME_DELETED_BY);
				grev := assignFromName(ParserObj, NAME_GREV);
				rev := assignFromName(ParserObj, NAME_REV);
				if (type_ = TYPE_FILE) then
				begin
					mtime := assignFromName(ParserObj, NAME_MTIME);
					virus_scan := assignFromName(ParserObj, NAME_VIRUS_SCAN);
					hash := assignFromName(ParserObj, NAME_HASH);
				end else begin
					tree := assignFromName(ParserObj, NAME_TREE);

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
	result := true;
end;

function fromJSON_FileStatus(JSON: WideString; var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean;
var
	ParserObj: TJSONObject;
	JSONVal: TJSONValue;
begin
	result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		ParserObj := (JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject;
		with CloudMailRuDirListingItem do
		begin
			size := assignFromName(ParserObj, NAME_SIZE);
			kind := assignFromName(ParserObj, NAME_KIND);
			weblink := assignFromName(ParserObj, NAME_WEBLINK);
			type_ := assignFromName(ParserObj, NAME_TYPE);
			home := assignFromName(ParserObj, NAME_HOME);
			name := assignFromName(ParserObj, NAME_NAME);
			if (type_ = TYPE_FILE) then
			begin
				mtime := assignFromName(ParserObj, NAME_MTIME);
				virus_scan := assignFromName(ParserObj, NAME_VIRUS_SCAN);
				hash := assignFromName(ParserObj, NAME_HASH);
			end else begin
				tree := assignFromName(ParserObj, NAME_TREE);
				grev := assignFromName(ParserObj, NAME_GREV);
				rev := assignFromName(ParserObj, NAME_REV);
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
	result := true;
end;

function fromJSON_InviteListing(JSON: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
var
	J: integer;
	A: TJSONArray;
	ParserObj: TJSONObject;
	JSONVal: TJSONValue;
begin
	result := false;
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
				email := assignFromName(ParserObj, NAME_EMAIL);
				status := assignFromName(ParserObj, NAME_STATUS);
				access := assignFromName(ParserObj, NAME_ACCESS);
				name := assignFromName(ParserObj, NAME_NAME);
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
	result := true;
end;

function fromJSON_IncomingInviteListing(JSON: WideString; var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean;
var
	ParserObj: TJSONObject;
	JSONVal: TJSONValue;
	OwnerObj: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	result := false;
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

				tree := assignFromName(ParserObj, NAME_TREE);
				access := assignFromName(ParserObj, NAME_ACCESS);
				name := assignFromName(ParserObj, NAME_NAME);
				home := assignFromName(ParserObj, NAME_HOME);
				size := assignFromName(ParserObj, NAME_SIZE);
				invite_token := assignFromName(ParserObj, NAME_INVITE_TOKEN);
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
	result := true;
end;

function fromJSON_OAuthTokenInfo(JSON: WideString; var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean;
var
	JSONVal: TJSONValue;
	ParserObj: TJSONObject;
begin
	result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		ParserObj := (JSONVal as TJSONObject);
		with CloudMailRuOAuthInfo do
		begin
			error := assignFromName(ParserObj, NAME_ERROR);
			error_code := assignFromName(ParserObj, NAME_ERROR_CODE);
			error_description := assignFromName(ParserObj, NAME_ERROR_DESCRIPTION);
			expires_in := assignFromName(ParserObj, NAME_EXPIRES_IN);
			refresh_token := assignFromName(ParserObj, NAME_REFRESH_TOKEN);
			access_token := assignFromName(ParserObj, NAME_ACCESS_TOKEN);
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
	result := true;
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
	result := CLOUD_OPERATION_OK;
end;

function fromJSON_PublicLink(JSON: WideString; var PublicLink: WideString): Boolean;
var
	JSONVal: TJSONValue;
begin
	result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		PublicLink := (JSONVal as TJSONObject).Values[NAME_BODY].Value;
		JSONVal.free;
	except
		exit;
	end;
	result := true;
end;

function fromJSON_Shard(JSON: WideString; var Shard: WideString): Boolean;
var
	JSONVal: TJSONValue;
begin
	result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		Shard := ((((JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject).Values[NAME_GET] as TJSONArray).Items[0] as TJSONObject).Values[NAME_URL].Value;
		JSONVal.free;
	except
		exit;
	end;
	result := true;
end;

function fromJSON_TwostepData(JSON: WideString; var TwostepData: TCloudMailRuTwostepData): Boolean;
var
	ParserObj: TJSONObject;
	JSONVal: TJSONValue;
begin
	result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		ParserObj := (JSONVal as TJSONObject) as TJSONObject;
		with TwostepData do
		begin
			form_name := assignFromName(ParserObj, NAME_FORM_NAME);
			auth_host := assignFromName(ParserObj, NAME_AUTH_HOST);
			secstep_phone := assignFromName(ParserObj, NAME_SECSTEP_PHONE);
			secstep_page := assignFromName(ParserObj, NAME_SECSTEP_PAGE);
			secstep_code_fail := assignFromName(ParserObj, NAME_SECSTEP_CODE_FAIL);
			secstep_resend_fail := assignFromName(ParserObj, NAME_SECSTEP_RESEND_FAIL);
			secstep_resend_success := assignFromName(ParserObj, NAME_SECSTEP_RESEND_SUCCESS);
			if Assigned(ParserObj.Values[NAME_SECSTEP_TIMEOUT]) then
			begin
				if ParserObj.Values[NAME_SECSTEP_TIMEOUT].Value <> '' then
					secstep_timeout := ParserObj.Values[NAME_SECSTEP_TIMEOUT].Value.ToInt64
				else
					secstep_timeout := AUTH_APP_USED;
			end;
			secstep_login := assignFromName(ParserObj, NAME_SECSTEP_LOGIN);
			secstep_disposable_fail := assignFromName(ParserObj, NAME_SECSTEP_DISPOSABLE_FAIL);
			secstep_smsapi_error := assignFromName(ParserObj, NAME_SECSTEP_SMSAPI_ERROR);
			secstep_captcha := assignFromName(ParserObj, NAME_SECSTEP_CAPTCHA);
			totp_enabled := assignFromName(ParserObj, NAME_TOTP_ENABLED);
			locale := assignFromName(ParserObj, NAME_LOCALE);
			client := assignFromName(ParserObj, NAME_CLIENT);
			csrf := assignFromName(ParserObj, NAME_CSRF);
			device := assignFromName(ParserObj, NAME_DEVICE);
		end;
		JSONVal.free;
	except
		exit;
	end;
	result := true;
end;

function fromJSON_UserSpace(JSON: WideString; var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean;
var
	ParserObj: TJSONObject;
	JSONVal: TJSONValue;
begin
	result := false;
	if not initJSONValue(JSON, JSONVal) then
		exit;
	try
		ParserObj := (JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject;
		with CloudMailRuSpaceInfo do
		begin
			overquota := assignFromName(ParserObj, NAME_OVERQUOTA);
			total := assignFromName(ParserObj, NAME_TOTAL);

			overquota := assignFromName(ParserObj, NAME_OVERQUOTA);
			total := assignFromName(ParserObj, NAME_TOTAL);
			used := assignFromName(ParserObj, NAME_USED);
		end;
		JSONVal.free;
	except
		exit;
	end;
	result := true;
end;

end.
