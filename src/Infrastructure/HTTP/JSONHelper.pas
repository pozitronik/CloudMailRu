unit JSONHelper;

{Refactored to use TSafeJSON for null-safe JSON navigation.
 Removed assignFromName and init - use TSafeJSON directly for such operations.}

interface

uses
	CloudConstants;

function getPublicLink(JSON: WideString; var PublicLink: WideString): Boolean;
function getShard(JSON: WideString; var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean;
function getBodyError(JSON: WideString): WideString;
function getBodyToken(JSON: WideString; var Token: WideString): Boolean;
function getRegistrationBody(JSON: WideString; var Body: WideString): Boolean;

implementation

uses
	SafeJSON;

function getPublicLink(JSON: WideString; var PublicLink: WideString): Boolean;
var
	SafeVal: TSafeJSON;
	ExtractedLink: WideString;
begin
	Result := False;
	SafeVal := TSafeJSON.Parse(JSON);
	try
		ExtractedLink := SafeVal.Get(NAME_BODY).AsString;
		if ExtractedLink <> '' then
		begin
			PublicLink := ExtractedLink;
			Result := True;
		end;
	finally
		SafeVal.Free;
	end;
end;

function getShard(JSON: WideString; var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean;
var
	SafeVal: TSafeJSON;
	ExtractedShard: WideString;
begin
	Result := False;
	SafeVal := TSafeJSON.Parse(JSON);
	try
		ExtractedShard := SafeVal.Get(NAME_BODY).Get(ShardType).Item(0).Get(NAME_URL).AsString;
		if ExtractedShard <> '' then
		begin
			Shard := ExtractedShard;
			Result := True;
		end;
	finally
		SafeVal.Free;
	end;
end;

function getBodyError(JSON: WideString): WideString;
var
	SafeVal: TSafeJSON;
begin
	SafeVal := TSafeJSON.Parse(JSON);
	try
		Result := SafeVal.Get(NAME_BODY).AsString;
	finally
		SafeVal.Free;
	end;
end;

function getBodyToken(JSON: WideString; var Token: WideString): Boolean;
var
	SafeVal: TSafeJSON;
	ExtractedToken: WideString;
begin
	Result := False;
	SafeVal := TSafeJSON.Parse(JSON);
	try
		ExtractedToken := SafeVal.Get(NAME_BODY).Get(NAME_TOKEN).AsString;
		if ExtractedToken <> '' then
		begin
			Token := ExtractedToken;
			Result := True;
		end;
	finally
		SafeVal.Free;
	end;
end;

function getRegistrationBody(JSON: WideString; var Body: WideString): Boolean;
var
	SafeVal: TSafeJSON;
	ExtractedBody: WideString;
begin
	Result := False;
	SafeVal := TSafeJSON.Parse(JSON);
	try
		ExtractedBody := SafeVal.Get(NAME_BODY).AsString;
		if ExtractedBody <> '' then
		begin
			Body := ExtractedBody;
			Result := True;
		end;
	finally
		SafeVal.Free;
	end;
end;

end.
