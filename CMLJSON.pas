unit CMLJSON;

interface

uses JSON, CMLTypes, System.SysUtils, System.Generics.Collections;

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

type
	TCloudMailRuJSONParser = class
	private
		JSON: WideString; //initial JSON string
		JSONVal: TJSONValue; //Main JSON
		ParserObj: TJSONObject; //Currently parsed JSON branch
		procedure assignFromName(Name: WideString; var Item: WideString); overload;
		procedure assignFromName(Name: WideString; var Item: Int64); overload;
		procedure assignFromName(Name: WideString; var Item: integer); overload;
		procedure assignFromName(Name: WideString; var Item: Boolean); overload;
	public
		constructor Create(JSON: WideString = '');
		destructor Destroy; override;
		{parser functions}
		function init(JSON: WideString): Boolean;
		function getDirListing(var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean; overload;
		function getDirListing(JSON: WideString; var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean; overload;
		function getFileStatus(var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean; overload;
		function getFileStatus(JSON: WideString; var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean; overload;
		function getInviteListing(var InviteListing: TCloudMailRuInviteInfoListing): Boolean; overload;
		function getInviteListing(JSON: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean; overload;
		function getIncomingInviteListing(var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean; overload;
		function getIncomingInviteListing(JSON: WideString; var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean; overload;
		function getOAuthTokenInfo(var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean; overload;
		function getOAuthTokenInfo(JSON: WideString; var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean; overload;
		function getOperationResult(): TCloudMailRuOperationResult; overload;
		function getOperationResult(JSON: WideString): TCloudMailRuOperationResult; overload;
		function getPublicLink(var PublicLink: WideString): Boolean; overload;
		function getPublicLink(JSON: WideString; var PublicLink: WideString): Boolean; overload;
		function getShard(var Shard: WideString): Boolean; overload;
		function getShard(JSON: WideString; var Shard: WideString): Boolean; overload;
		function getTwostepData(var TwostepData: TCloudMailRuTwostepData): Boolean; overload;
		function getTwostepData(JSON: WideString; var TwostepData: TCloudMailRuTwostepData): Boolean; overload;
		function getUserSpace(var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean; overload;
		function getUserSpace(JSON: WideString; var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean; overload;
		function getRegistrationBody(var Body: WideString): Boolean; overload;
		function getRegistrationBody(JSON: WideString; var Body: WideString): Boolean; overload;
		function getRegistrationOperationResult(): TCloudMailRuOperationResult; overload;
		function getRegistrationOperationResult(JSON: WideString): TCloudMailRuOperationResult; overload;

	end;

implementation

{TCloudMailRuJSONParser}

procedure TCloudMailRuJSONParser.assignFromName(Name: WideString; var Item: WideString);
begin
	if Assigned(self.ParserObj.Values[Name]) then
		Item := self.ParserObj.Values[Name].Value;
end;

procedure TCloudMailRuJSONParser.assignFromName(Name: WideString; var Item: Int64);
begin
	if Assigned(self.ParserObj.Values[Name]) then
		Item := self.ParserObj.Values[Name].Value.ToInt64;
end;

procedure TCloudMailRuJSONParser.assignFromName(Name: WideString; var Item: integer);
begin
	if Assigned(self.ParserObj.Values[Name]) then
		Item := self.ParserObj.Values[Name].Value.ToInteger;
end;

procedure TCloudMailRuJSONParser.assignFromName(Name: WideString; var Item: Boolean);
begin
	if Assigned(self.ParserObj.Values[Name]) then
		Item := self.ParserObj.Values[Name].Value.ToBoolean;
end;

function TCloudMailRuJSONParser.init(JSON: WideString): Boolean;
begin
	result := false;
	self.JSON := JSON;
	try
		self.JSONVal := TJSONObject.ParseJSONValue(self.JSON);
	except
		exit;
	end;
	result := true;
end;

constructor TCloudMailRuJSONParser.Create(JSON: WideString = '');
begin
	if (EmptyWideStr <> JSON) then
		self.init(JSON);

end;

destructor TCloudMailRuJSONParser.Destroy;
begin
	JSONVal.free;
end;

function TCloudMailRuJSONParser.getDirListing(var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean;
var
	J: integer;
	A: TJSONArray;
begin
	result := false;
	try
		A := ((JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject).Values[NAME_LIST] as TJSONArray;
		SetLength(CloudMailRuDirListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with CloudMailRuDirListing[J] do
			begin
				assignFromName(NAME_SIZE, size);
				assignFromName(NAME_KIND, kind);
				assignFromName(NAME_WEBLINK, weblink);
				assignFromName(NAME_TYPE, type_);
				assignFromName(NAME_HOME, home);
				assignFromName(NAME_NAME, name);
				visible_name := name;
				assignFromName(NAME_DELETED_AT, deleted_at);
				assignFromName(NAME_DELETED_FROM, deleted_from);
				assignFromName(NAME_DELETED_BY, deleted_by);
				assignFromName(NAME_GREV, grev);
				assignFromName(NAME_REV, rev);
				if (type_ = TYPE_FILE) then
				begin
					assignFromName(NAME_MTIME, mtime);
					assignFromName(NAME_VIRUS_SCAN, virus_scan);
					assignFromName(NAME_HASH, hash);
				end else begin
					assignFromName(NAME_TREE, tree);

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
		exit;
	end;
	result := true;
end;

function TCloudMailRuJSONParser.getFileStatus(var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean;
begin
	result := false;
	try
		ParserObj := (JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject;
		with CloudMailRuDirListingItem do
		begin
			assignFromName(NAME_SIZE, size);
			assignFromName(NAME_KIND, kind);
			assignFromName(NAME_WEBLINK, weblink);
			assignFromName(NAME_TYPE, type_);
			assignFromName(NAME_HOME, home);
			assignFromName(NAME_NAME, name);
			if (type_ = TYPE_FILE) then
			begin
				assignFromName(NAME_MTIME, mtime);
				assignFromName(NAME_VIRUS_SCAN, virus_scan);
				assignFromName(NAME_HASH, hash);
			end else begin
				assignFromName(NAME_TREE, tree);
				assignFromName(NAME_GREV, grev);
				assignFromName(NAME_REV, rev);
				if Assigned((ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FOLDERS]) then
					folders_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FOLDERS].Value.ToInteger();
				if Assigned((ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FILES]) then
					files_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FILES].Value.ToInteger();
				mtime := 0;
			end;
		end;
	except
		exit;
	end;
	result := true;
end;

function TCloudMailRuJSONParser.getIncomingInviteListing(var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean;
var
	OwnerObj: TJSONObject;
	J: integer;
	A: TJSONArray;
begin
	result := false;
	SetLength(IncomingInviteListing, 0);
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

				assignFromName(NAME_TREE, tree);
				assignFromName(NAME_ACCESS, access);
				assignFromName(NAME_NAME, name);
				assignFromName(NAME_HOME, home);
				assignFromName(NAME_SIZE, size);
				assignFromName(NAME_INVITE_TOKEN, invite_token);
			end;
		end;
	except
		on E: {EJSON}Exception do
		begin
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			exit;
		end;
	end;
	result := true;
end;

function TCloudMailRuJSONParser.getInviteListing(var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
var
	J: integer;
	A: TJSONArray;
begin
	result := false;
	SetLength(InviteListing, 0);
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
				assignFromName(NAME_EMAIL, email);
				assignFromName(NAME_STATUS, status);
				assignFromName(NAME_ACCESS, access);
				assignFromName(NAME_NAME, name);
			end;
		end;
	except
		on E: {EJSON}Exception do
		begin
			//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
			exit;
		end;
	end;
	result := true;
end;

function TCloudMailRuJSONParser.getOAuthTokenInfo(var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean;
begin
	result := false;
	try
		ParserObj := (JSONVal as TJSONObject);
		with CloudMailRuOAuthInfo do
		begin
			assignFromName(NAME_ERROR, error);
			assignFromName(NAME_ERROR_CODE, error_code);
			assignFromName(NAME_ERROR_DESCRIPTION, error_description);
			assignFromName(NAME_EXPIRES_IN, expires_in);
			assignFromName(NAME_REFRESH_TOKEN, refresh_token);
			assignFromName(NAME_ACCESS_TOKEN, access_token);
		end;
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

function TCloudMailRuJSONParser.getOperationResult(): TCloudMailRuOperationResult;
var
	error, nodename: WideString;
begin
	try
		ParserObj := JSONVal as TJSONObject;
		result.OperationStatus := ParserObj.Values[NAME_STATUS].Value.ToInteger;
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

			if (Assigned((ParserObj.Values[NAME_BODY] as TJSONObject).Values[NAME_HOME])) then
				nodename := 'home'
			else if (Assigned((ParserObj.Values[NAME_BODY] as TJSONObject).Values[NAME_WEBLINK])) then
				nodename := 'weblink'
			else if (Assigned((ParserObj.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITE_EMAIL])) then
			begin //invite errors
				error := (((ParserObj.Values[NAME_BODY] as TJSONObject).Values[NAME_INVITE_EMAIL]) as TJSONObject).Values[NAME_ERROR].Value;
			end else begin
				//Log(MSGTYPE_IMPORTANTERROR, 'Can''t parse server answer: ' + JSON); todo
				result.OperationResult := (CLOUD_ERROR_UNKNOWN);
			end;
			if error = EmptyWideStr then
				error := ((ParserObj.Values[NAME_BODY] as TJSONObject).Values[nodename] as TJSONObject).Values[NAME_ERROR].Value;
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

end;

function TCloudMailRuJSONParser.getPublicLink(var PublicLink: WideString): Boolean;
begin
	result := false;
	try
		PublicLink := (JSONVal as TJSONObject).Values[NAME_BODY].Value;
	except
		exit;
	end;
	result := true;
end;

function TCloudMailRuJSONParser.getShard(var Shard: WideString): Boolean;
begin
	result := false;
	try
		Shard := ((((JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject).Values[NAME_GET] as TJSONArray).Items[0] as TJSONObject).Values[NAME_URL].Value;
	except
		exit;
	end;
	result := true;
end;

function TCloudMailRuJSONParser.getTwostepData(var TwostepData: TCloudMailRuTwostepData): Boolean;
begin
	result := false;
	try
		ParserObj := (JSONVal as TJSONObject) as TJSONObject;
		with TwostepData do
		begin
			assignFromName(NAME_FORM_NAME, form_name);
			assignFromName(NAME_AUTH_HOST, auth_host);
			assignFromName(NAME_SECSTEP_PHONE, secstep_phone);
			assignFromName(NAME_SECSTEP_PAGE, secstep_page);
			assignFromName(NAME_SECSTEP_CODE_FAIL, secstep_code_fail);
			assignFromName(NAME_SECSTEP_RESEND_FAIL, secstep_resend_fail);
			assignFromName(NAME_SECSTEP_RESEND_SUCCESS, secstep_resend_success);
			if Assigned(ParserObj.Values[NAME_SECSTEP_TIMEOUT]) then
			begin
				if ParserObj.Values[NAME_SECSTEP_TIMEOUT].Value <> '' then
					secstep_timeout := ParserObj.Values[NAME_SECSTEP_TIMEOUT].Value.ToInt64
				else
					secstep_timeout := AUTH_APP_USED;
			end;
			assignFromName(NAME_SECSTEP_LOGIN, secstep_login);
			assignFromName(NAME_SECSTEP_DISPOSABLE_FAIL, secstep_disposable_fail);
			assignFromName(NAME_SECSTEP_SMSAPI_ERROR, secstep_smsapi_error);
			assignFromName(NAME_SECSTEP_CAPTCHA, secstep_captcha);
			assignFromName(NAME_TOTP_ENABLED, totp_enabled);
			assignFromName(NAME_LOCALE, locale);
			assignFromName(NAME_CLIENT, client);
			assignFromName(NAME_CSRF, csrf);
			assignFromName(NAME_DEVICE, device);
		end;
	except
		exit;
	end;
	result := true;
end;

function TCloudMailRuJSONParser.getUserSpace(var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean;
begin
	result := false;
	try
		ParserObj := (JSONVal as TJSONObject).Values[NAME_BODY] as TJSONObject;
		with CloudMailRuSpaceInfo do
		begin
			assignFromName(NAME_OVERQUOTA, overquota);
			assignFromName(NAME_TOTAL, total);
			assignFromName(NAME_USED, used);
		end;
	except
		exit;
	end;
	result := true;
end;

function TCloudMailRuJSONParser.getRegistrationBody(var Body: WideString): Boolean;
begin
	result := false;
	try
		Body := (JSONVal as TJSONObject).Values[NAME_BODY].Value;
	except
		exit;
	end;
	result := true;
end;

function TCloudMailRuJSONParser.getDirListing(JSON: WideString; var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean;
begin
	init(JSON);
	exit(getDirListing(CloudMailRuDirListing));
end;

function TCloudMailRuJSONParser.getFileStatus(JSON: WideString; var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean;
begin
	init(JSON);
	exit(getFileStatus(CloudMailRuDirListingItem));
end;

function TCloudMailRuJSONParser.getIncomingInviteListing(JSON: WideString; var IncomingInviteListing: TCloudMailRuIncomingInviteInfoListing): Boolean;
begin
	init(JSON);
	exit(getIncomingInviteListing(IncomingInviteListing));
end;

function TCloudMailRuJSONParser.getInviteListing(JSON: WideString; var InviteListing: TCloudMailRuInviteInfoListing): Boolean;
begin
	init(JSON);
	exit(getInviteListing(InviteListing));
end;

function TCloudMailRuJSONParser.getOAuthTokenInfo(JSON: WideString; var CloudMailRuOAuthInfo: TCloudMailRuOAuthInfo): Boolean;
begin
	init(JSON);
	exit(getOAuthTokenInfo(CloudMailRuOAuthInfo));
end;

function TCloudMailRuJSONParser.getOperationResult(JSON: WideString): TCloudMailRuOperationResult;
begin
	init(JSON);
	exit(getOperationResult());
end;

function TCloudMailRuJSONParser.getPublicLink(JSON: WideString; var PublicLink: WideString): Boolean;
begin
	init(JSON);
	exit(getPublicLink(PublicLink));
end;

function TCloudMailRuJSONParser.getShard(JSON: WideString; var Shard: WideString): Boolean;
begin
	init(JSON);
	exit(getShard(Shard));
end;

function TCloudMailRuJSONParser.getTwostepData(JSON: WideString; var TwostepData: TCloudMailRuTwostepData): Boolean;
begin
	init(JSON);
	exit(getTwostepData(TwostepData));
end;

function TCloudMailRuJSONParser.getUserSpace(JSON: WideString; var CloudMailRuSpaceInfo: TCloudMailRuSpaceInfo): Boolean;
begin
	init(JSON);
	exit(getUserSpace(CloudMailRuSpaceInfo));
end;

function TCloudMailRuJSONParser.getRegistrationBody(JSON: WideString; var Body: WideString): Boolean;
begin
	init(JSON);
	exit(getRegistrationBody(Body));
end;

function TCloudMailRuJSONParser.getRegistrationOperationResult(JSON: WideString): TCloudMailRuOperationResult;
begin
	init(JSON);
	exit(getRegistrationOperationResult());
end;

{registration api}
function TCloudMailRuJSONParser.getRegistrationOperationResult(): TCloudMailRuOperationResult;
begin
	result.OperationResult := CLOUD_ERROR_UNKNOWN;
	try
		ParserObj := JSONVal as TJSONObject;
		result.OperationStatus := ParserObj.Values[NAME_STATUS].Value.ToInteger;
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

end;

end.
