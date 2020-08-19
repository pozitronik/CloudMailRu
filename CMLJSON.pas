unit CMLJSON;

interface

uses
	JSON, CMLTypes, System.SysUtils, System.Generics.Collections;

type
	CMLJSONParser = class
	private
		class procedure assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: WideString); overload;
		class procedure assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: Int64); overload;
		class procedure assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: integer); overload;
		class procedure assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: Boolean); overload;
		class function init(JSON: WideString; var JSONVal: TJSONObject): Boolean;
	public
		{parser functions}
		class function getDirListing(JSON: WideString; var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean; overload;
		class function getFileStatus(JSON: WideString; var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean; overload;
		class function getInviteListing(JSON: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean; overload;
		class function getIncomingInviteListing(JSON: WideString; var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean; overload;
		class function getOAuthTokenInfo(JSON: WideString; var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean; overload;
		class function getOperationResult(JSON: WideString): TCloudMailRuOperationResult; overload;
		class function getPublicLink(JSON: WideString; var PublicLink: WideString): Boolean; overload;
		class function getShard(JSON: WideString; var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean; overload;
		class function getTwostepData(JSON: WideString; var TwostepData: TCloudMailRuTwostepData): Boolean; overload;
		class function getUserSpace(JSON: WideString; var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean; overload;
		class function getRegistrationBody(JSON: WideString; var Body: WideString): Boolean; overload;
		class function getRegistrationOperationResult(JSON: WideString): TCloudMailRuOperationResult; overload;
		class function getBodyError(JSON: WideString): WideString;
	end;

implementation

{TCloudMailRuJSONParser}

class procedure CMLJSONParser.assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: WideString);
begin
	if Assigned(ParserObj.Values[Name]) then
		Item := ParserObj.Values[Name].Value;
end;

class procedure CMLJSONParser.assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: Int64);
begin
	if Assigned(ParserObj.Values[Name]) then
		Item := ParserObj.Values[Name].Value.ToInt64;
end;

class procedure CMLJSONParser.assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: integer);
begin
	if Assigned(ParserObj.Values[Name]) then
		Item := ParserObj.Values[Name].Value.ToInteger;
end;

class procedure CMLJSONParser.assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: Boolean);
begin
	if Assigned(ParserObj.Values[Name]) then
		Item := ParserObj.Values[Name].Value.ToBoolean;
end;

class function CMLJSONParser.init(JSON: WideString; var JSONVal: TJSONObject): Boolean;
begin
	result := False;
	try
		JSONVal := nil;
		JSONVal := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
	except
		Exit;
	end;
	result := true;
end;

class function CMLJSONParser.getDirListing(JSON: WideString; var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean;
var
	J: integer;
	A: TJSONArray;
	JSONVal: TJSONObject;
	ParserObj: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		A := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_LIST] as TJSONArray;
		SetLength(CloudMailRuDirListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with CloudMailRuDirListing[J] do
			begin
				assignFromName(NAME_SIZE, ParserObj, size);
				assignFromName(NAME_KIND, ParserObj, kind);
				assignFromName(NAME_WEBLINK, ParserObj, weblink);
				assignFromName(NAME_TYPE, ParserObj, type_);
				assignFromName(NAME_HOME, ParserObj, home);
				assignFromName(NAME_NAME, ParserObj, name);
				visible_name := name;
				assignFromName(NAME_DELETED_AT, ParserObj, deleted_at);
				assignFromName(NAME_DELETED_FROM, ParserObj, deleted_from);
				assignFromName(NAME_DELETED_BY, ParserObj, deleted_by);
				assignFromName(NAME_GREV, ParserObj, grev);
				assignFromName(NAME_REV, ParserObj, rev);
				if (type_ = TYPE_FILE) then
				begin
					assignFromName(NAME_MTIME, ParserObj, mtime);
					assignFromName(NAME_VIRUS_SCAN, ParserObj, virus_scan);
					assignFromName(NAME_HASH, ParserObj, hash);
				end else begin
					assignFromName(NAME_TREE, ParserObj, tree);

					if Assigned(ParserObj.Values[NAME_COUNT]) then
					begin
						folders_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FOLDERS].Value.ToInteger();
						files_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FILES].Value.ToInteger();
					end;
					mtime := 0;
				end;
			end;
		end;
	except
		Exit;
	end;
	result := true;
	JSONVal.free;
end;

class function CMLJSONParser.getFileStatus(JSON: WideString; var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		ParserObj := JSONVal.Values[NAME_BODY] as TJSONObject;
		with CloudMailRuDirListingItem do
		begin
			assignFromName(NAME_SIZE, ParserObj, size);
			assignFromName(NAME_KIND, ParserObj, kind);
			assignFromName(NAME_WEBLINK, ParserObj, weblink);
			assignFromName(NAME_TYPE, ParserObj, type_);
			assignFromName(NAME_HOME, ParserObj, home);
			assignFromName(NAME_NAME, ParserObj, name);
			if (type_ = TYPE_FILE) then
			begin
				assignFromName(NAME_MTIME, ParserObj, mtime);
				assignFromName(NAME_VIRUS_SCAN, ParserObj, virus_scan);
				assignFromName(NAME_HASH, ParserObj, hash);
			end else begin
				assignFromName(NAME_TREE, ParserObj, tree);
				assignFromName(NAME_GREV, ParserObj, grev);
				assignFromName(NAME_REV, ParserObj, rev);
				if Assigned((ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FOLDERS]) then
					folders_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FOLDERS].Value.ToInteger();
				if Assigned((ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FILES]) then
					files_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FILES].Value.ToInteger();
				mtime := 0;
			end;
		end;
	except
		Exit;
	end;
	result := true;
	JSONVal.free;
end;

class function CMLJSONParser.getIncomingInviteListing(JSON: WideString; var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean;
var
	JSONVal: TJSONObject;
	OwnerObj: TJSONObject;
	ParserObj: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	result := False;

	SetLength(IncomingInviteListing, 0);
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		A := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_LIST] as TJSONArray;
		if not Assigned(A) then
			Exit; //no invites
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

				assignFromName(NAME_TREE, ParserObj, tree);
				assignFromName(NAME_ACCESS, ParserObj, access);
				assignFromName(NAME_NAME, ParserObj, name);
				assignFromName(NAME_HOME, ParserObj, home);
				assignFromName(NAME_SIZE, ParserObj, size);
				assignFromName(NAME_INVITE_TOKEN, ParserObj, invite_token);
			end;
		end;
	except
		on E: {EJSON}Exception do
		begin
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			Exit;
		end;
	end;
	result := true;
	JSONVal.free;
end;

class function CMLJSONParser.getInviteListing(JSON: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	result := False;
	SetLength(InviteListing, 0);
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		A := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITED] as TJSONArray;
		if not Assigned(A) then
			Exit(true); //no invites
		SetLength(InviteListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with InviteListing[J] do
			begin
				assignFromName(NAME_EMAIL, ParserObj, email);
				assignFromName(NAME_STATUS, ParserObj, status);
				assignFromName(NAME_ACCESS, ParserObj, access);
				assignFromName(NAME_NAME, ParserObj, name);
			end;
		end;
	except
		on E: {EJSON}Exception do
		begin
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			Exit;
		end;
	end;
	result := true;
	JSONVal.free;
end;

class function CMLJSONParser.getOAuthTokenInfo(JSON: WideString; var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean;
var
	JSONVal: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		with CloudMailRuOAuthInfo do
		begin
			assignFromName(NAME_ERROR, JSONVal, error);
			assignFromName(NAME_ERROR_CODE, JSONVal, error_code);
			assignFromName(NAME_ERROR_DESCRIPTION, JSONVal, error_description);
			assignFromName(NAME_EXPIRES_IN, JSONVal, expires_in);
			assignFromName(NAME_REFRESH_TOKEN, JSONVal, refresh_token);
			assignFromName(NAME_ACCESS_TOKEN, JSONVal, access_token);
		end;
	except
		on E: {EJSON}Exception do
		begin
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			CloudMailRuOAuthInfo.error_code := CLOUD_ERROR_UNKNOWN;
			CloudMailRuOAuthInfo.error := 'Answer parsing';
			CloudMailRuOAuthInfo.error_description := 'JSON parsing error: at ' + JSON;
			Exit;
		end;
	end;
	result := true;
	JSONVal.free;
end;

class function CMLJSONParser.getOperationResult(JSON: WideString): TCloudMailRuOperationResult;
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

class function CMLJSONParser.getPublicLink(JSON: WideString; var PublicLink: WideString): Boolean;
var
	JSONVal: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		PublicLink := JSONVal.Values[NAME_BODY].Value;
	except
		Exit;
	end;
	result := true;
	JSONVal.free;
end;

class function CMLJSONParser.getShard(JSON: WideString; var Shard: WideString; ShardType: WideString = NAME_GET): Boolean; //Некрасиво получается, подумать над переделкой, например вызывать методы только статикой
var
	JSONVal: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		Shard := (((JSONVal.Values[NAME_BODY] as TJSONObject).Values[ShardType] as TJSONArray).Items[0] as TJSONObject).Values[NAME_URL].Value;
	except
		Exit;
	end;
	result := true;
	JSONVal.free;
end;

class function CMLJSONParser.getTwostepData(JSON: WideString; var TwostepData: TCloudMailRuTwostepData): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		ParserObj := JSONVal as TJSONObject; //не менять
		with TwostepData do
		begin
			assignFromName(NAME_FORM_NAME, ParserObj, form_name);
			assignFromName(NAME_AUTH_HOST, ParserObj, auth_host);
			assignFromName(NAME_SECSTEP_PHONE, ParserObj, secstep_phone);
			assignFromName(NAME_SECSTEP_PAGE, ParserObj, secstep_page);
			assignFromName(NAME_SECSTEP_CODE_FAIL, ParserObj, secstep_code_fail);
			assignFromName(NAME_SECSTEP_RESEND_FAIL, ParserObj, secstep_resend_fail);
			assignFromName(NAME_SECSTEP_RESEND_SUCCESS, ParserObj, secstep_resend_success);
			if Assigned(ParserObj.Values[NAME_SECSTEP_TIMEOUT]) then
			begin
				if ParserObj.Values[NAME_SECSTEP_TIMEOUT].Value <> '' then
					secstep_timeout := ParserObj.Values[NAME_SECSTEP_TIMEOUT].Value.ToInt64
				else
					secstep_timeout := AUTH_APP_USED;
			end;
			assignFromName(NAME_SECSTEP_LOGIN, ParserObj, secstep_login);
			assignFromName(NAME_SECSTEP_DISPOSABLE_FAIL, ParserObj, secstep_disposable_fail);
			assignFromName(NAME_SECSTEP_SMSAPI_ERROR, ParserObj, secstep_smsapi_error);
			assignFromName(NAME_SECSTEP_CAPTCHA, ParserObj, secstep_captcha);
			assignFromName(NAME_TOTP_ENABLED, ParserObj, totp_enabled);
			assignFromName(NAME_LOCALE, ParserObj, locale);
			assignFromName(NAME_CLIENT, ParserObj, client);
			assignFromName(NAME_CSRF, ParserObj, csrf);
			assignFromName(NAME_DEVICE, ParserObj, device);
		end;
	except
		Exit;
	end;
	result := true;
	JSONVal.free;
end;

class function CMLJSONParser.getUserSpace(JSON: WideString; var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		ParserObj := JSONVal.Values[NAME_BODY] as TJSONObject;
		with CloudMailRuSpaceInfo do
		begin
			assignFromName(NAME_OVERQUOTA, ParserObj, overquota);
			assignFromName(NAME_TOTAL, ParserObj, total);
			assignFromName(NAME_USED, ParserObj, used);
		end;
	except
		Exit;
	end;
	result := true;
	JSONVal.free;
end;

class function CMLJSONParser.getBodyError(JSON: WideString): WideString;
var
	JSONVal: TJSONObject;
begin
	result := '';
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		result := JSONVal.Values[NAME_BODY].Value;
	except
		Exit;
	end;

	JSONVal.free;
end;

class function CMLJSONParser.getRegistrationBody(JSON: WideString; var Body: WideString): Boolean;
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

class function CMLJSONParser.getRegistrationOperationResult(JSON: WideString): TCloudMailRuOperationResult;
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

end.
