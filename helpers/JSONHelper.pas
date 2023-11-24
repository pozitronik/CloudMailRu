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
	result := true;
end;

function getPublicLink(JSON: WideString; var PublicLink: WideString): Boolean;
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

function getShard(JSON: WideString; var Shard: WideString; ShardType: WideString = NAME_GET): Boolean; //Некрасиво получается, подумать над переделкой, например вызывать методы только статикой
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

function getBodyError(JSON: WideString): WideString;
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

function getBodyToken(JSON: WideString; var Token: WideString): Boolean;
var
	JSONVal: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		Token := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_TOKEN].Value;
	except
		Exit;
	end;
	result := true;
	JSONVal.free;
end;

end.
