unit JSONHelper;

interface

uses
	System.Generics.Collections,
	JSON,
	SysUtils,
	CMRConstants;

procedure assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: WideString); overload;
procedure assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: Int64); overload;
procedure assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: integer); overload;
procedure assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: Boolean); overload;
function init(JSON: WideString; var JSONVal: TJSONObject): Boolean;

function getPublicLink(JSON: WideString; var PublicLink: WideString): Boolean;
function getShard(JSON: WideString; var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean;
function getBodyError(JSON: WideString): WideString;
function getBodyToken(JSON: WideString; var Token: WideString): Boolean;
function getRegistrationBody(JSON: WideString; var Body: WideString): Boolean;

implementation

procedure assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: WideString);
begin
	if Assigned(ParserObj.Values[Name]) then
		Item := ParserObj.Values[Name].Value;
end;

procedure assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: Int64);
begin
	if Assigned(ParserObj.Values[Name]) then
		Item := ParserObj.Values[Name].Value.ToInt64;
end;

procedure assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: integer);
begin
	if Assigned(ParserObj.Values[Name]) then
		Item := ParserObj.Values[Name].Value.ToInteger;
end;

procedure assignFromName(Name: WideString; var ParserObj: TJSONObject; var Item: Boolean);
begin
	if Assigned(ParserObj.Values[Name]) then
		Item := ParserObj.Values[Name].Value.ToBoolean;
end;

function init(JSON: WideString; var JSONVal: TJSONObject): Boolean;
begin
	result := False;
	try
		JSONVal := nil;
		JSONVal := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
	except
		Exit;
	end;
	result := JSONVal <> nil;
end;

function getPublicLink(JSON: WideString; var PublicLink: WideString): Boolean;
var
	JSONVal: TJSONObject;
begin
	result := False;
	if (not init(JSON, JSONVal)) then
		Exit;
	try
		try
			PublicLink := JSONVal.Values[NAME_BODY].Value;
			result := true;
		except
			Exit;
		end;
	finally
		JSONVal.Free;
	end;
end;

function getShard(JSON: WideString; var Shard: WideString; ShardType: WideString = NAME_GET): Boolean;
var
	JSONVal: TJSONObject;
begin
	result := False;
	if (not init(JSON, JSONVal)) then
		Exit;
	try
		try
			Shard := (((JSONVal.Values[NAME_BODY] as TJSONObject).Values[ShardType] as TJSONArray).Items[0] as TJSONObject).Values[NAME_URL].Value;
			result := true;
		except
			Exit;
		end;
	finally
		JSONVal.Free;
	end;
end;

function getBodyError(JSON: WideString): WideString;
var
	JSONVal: TJSONObject;
begin
	result := '';
	if (not init(JSON, JSONVal)) then
		Exit;
	try
		try
			result := JSONVal.Values[NAME_BODY].Value;
		except
			Exit;
		end;
	finally
		JSONVal.Free;
	end;
end;

function getBodyToken(JSON: WideString; var Token: WideString): Boolean;
var
	JSONVal: TJSONObject;
begin
	result := False;
	if (not init(JSON, JSONVal)) then
		Exit;
	try
		try
			Token := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_TOKEN].Value;
			result := true;
		except
			Exit;
		end;
	finally
		JSONVal.Free;
	end;
end;

function getRegistrationBody(JSON: WideString; var Body: WideString): Boolean;
var
	JSONVal: TJSONObject;
begin
	result := False;
	if (not init(JSON, JSONVal)) then
		Exit;
	try
		try
			Body := JSONVal.Values[NAME_BODY].Value;
			result := true;
		except
			Exit;
		end;
	finally
		JSONVal.Free;
	end;
end;

end.
