unit SafeJSON;

{Null-safe JSON accessor wrapper for parsing dynamic JSON structures.
 Provides fluent, chainable navigation without explicit Assigned() checks.
 Handles null values and missing fields gracefully with default values.}

interface

uses
	System.Generics.Collections,
	JSON,
	SysUtils;

type
	{Null-safe wrapper around TJSONValue for fluent JSON navigation.
	 All operations are safe on null/missing values - they return appropriate defaults.}
	TSafeJSON = record
	private
		FValue: TJSONValue;
		FOwnsValue: Boolean;

		{Internal constructor for wrapping existing JSON values}
		class function CreateInternal(AValue: TJSONValue; AOwnsValue: Boolean): TSafeJSON; static;
	public
		{Parse JSON string and create owned wrapper.
		 @param JSON The JSON string to parse
		 @return TSafeJSON wrapper (check IsNull if parsing failed)}
		class function Parse(const JSON: WideString): TSafeJSON; static;

		{Wrap existing TJSONValue without taking ownership.
		 @param AValue The JSON value to wrap (can be nil)
		 @return TSafeJSON wrapper}
		class function Wrap(AValue: TJSONValue): TSafeJSON; static;

		{Create a null/empty wrapper.
		 @return TSafeJSON with IsNull = True}
		class function Null: TSafeJSON; static;

		{Free owned JSON value. Call this only if you used Parse().
		 Safe to call multiple times or on non-owned wrappers.}
		procedure Free;

		{Navigate to child object property by name.
		 @param Name The property name to access
		 @return TSafeJSON wrapper for child (IsNull if not found or not object)}
		function Get(const Name: WideString): TSafeJSON;

		{Access array element by index.
		 @param Index Zero-based array index
		 @return TSafeJSON wrapper for element (IsNull if out of bounds or not array)}
		function Item(Index: Integer): TSafeJSON;

		{Get string value with default.
		 Returns empty string for null JSON values.
		 @param Default Value to return if null/missing
		 @return String value or default}
		function AsString(const Default: WideString = ''): WideString;

		{Get integer value with default.
		 @param Default Value to return if null/missing/invalid
		 @return Integer value or default}
		function AsInt(const Default: Integer = 0): Integer;

		{Get Int64 value with default.
		 @param Default Value to return if null/missing/invalid
		 @return Int64 value or default}
		function AsInt64(const Default: Int64 = 0): Int64;

		{Get boolean value with default.
		 @param Default Value to return if null/missing/invalid
		 @return Boolean value or default}
		function AsBool(const Default: Boolean = False): Boolean;

		{Check if this wrapper represents a null/missing value.
		 @return True if null or missing}
		function IsNull: Boolean;

		{Check if this wrapper represents a JSON object.
		 @return True if JSON object}
		function IsObject: Boolean;

		{Check if this wrapper represents a JSON array.
		 @return True if JSON array}
		function IsArray: Boolean;

		{Get array length.
		 @return Number of elements (0 if not an array)}
		function Count: Integer;

		{Get underlying TJSONValue (can be nil).
		 Use for advanced scenarios or compatibility with existing code.
		 @return Raw TJSONValue pointer}
		function Raw: TJSONValue;
	end;

implementation

class function TSafeJSON.CreateInternal(AValue: TJSONValue; AOwnsValue: Boolean): TSafeJSON;
begin
	Result.FValue := AValue;
	Result.FOwnsValue := AOwnsValue;
end;

class function TSafeJSON.Parse(const JSON: WideString): TSafeJSON;
var
	ParsedValue: TJSONValue;
begin
	Result.FValue := nil;
	Result.FOwnsValue := True;
	if JSON = '' then
		Exit;
	try
		ParsedValue := TJSONObject.ParseJSONValue(JSON);
		Result.FValue := ParsedValue;
	except
		{Parsing failed - return null wrapper}
	end;
end;

class function TSafeJSON.Wrap(AValue: TJSONValue): TSafeJSON;
begin
	Result := CreateInternal(AValue, False);
end;

class function TSafeJSON.Null: TSafeJSON;
begin
	Result := CreateInternal(nil, False);
end;

procedure TSafeJSON.Free;
begin
	if FOwnsValue and Assigned(FValue) then
	begin
		FValue.Free;
		FValue := nil;
	end;
	FOwnsValue := False;
end;

function TSafeJSON.Get(const Name: WideString): TSafeJSON;
var
	Obj: TJSONObject;
	ChildValue: TJSONValue;
begin
	Result := TSafeJSON.Null;
	if not Assigned(FValue) then
		Exit;
	if not (FValue is TJSONObject) then
		Exit;

	Obj := FValue as TJSONObject;
	ChildValue := Obj.Values[Name];
	if Assigned(ChildValue) then
		Result := TSafeJSON.Wrap(ChildValue);
end;

function TSafeJSON.Item(Index: Integer): TSafeJSON;
var
	Arr: TJSONArray;
begin
	Result := TSafeJSON.Null;
	if not Assigned(FValue) then
		Exit;
	if not (FValue is TJSONArray) then
		Exit;

	Arr := FValue as TJSONArray;
	if (Index < 0) or (Index >= Arr.Count) then
		Exit;

	Result := TSafeJSON.Wrap(Arr.Items[Index]);
end;

function TSafeJSON.AsString(const Default: WideString): WideString;
begin
	Result := Default;
	if not Assigned(FValue) then
		Exit;

	{Handle explicit null values - return default (usually empty string)}
	if FValue is TJSONNull then
		Exit;

	{Get value - will be empty string for non-string types}
	try
		Result := FValue.Value;
	except
		Result := Default;
	end;
end;

function TSafeJSON.AsInt(const Default: Integer): Integer;
var
	StrVal: string;
begin
	Result := Default;
	if not Assigned(FValue) then
		Exit;
	if FValue is TJSONNull then
		Exit;

	try
		{Try to get value and convert to integer}
		StrVal := FValue.Value;
		if StrVal <> '' then
			Result := StrToIntDef(StrVal, Default);
	except
		Result := Default;
	end;
end;

function TSafeJSON.AsInt64(const Default: Int64): Int64;
var
	StrVal: string;
begin
	Result := Default;
	if not Assigned(FValue) then
		Exit;
	if FValue is TJSONNull then
		Exit;

	try
		StrVal := FValue.Value;
		if StrVal <> '' then
			Result := StrToInt64Def(StrVal, Default);
	except
		Result := Default;
	end;
end;

function TSafeJSON.AsBool(const Default: Boolean): Boolean;
var
	StrVal: string;
begin
	Result := Default;
	if not Assigned(FValue) then
		Exit;
	if FValue is TJSONNull then
		Exit;

	try
		{TJSONBool.Value returns 'true' or 'false'}
		StrVal := LowerCase(FValue.Value);
		if (StrVal = 'true') or (StrVal = '1') then
			Result := True
		else if (StrVal = 'false') or (StrVal = '0') then
			Result := False;
		{Other values leave default unchanged}
	except
		Result := Default;
	end;
end;

function TSafeJSON.IsNull: Boolean;
begin
	Result := not Assigned(FValue) or (FValue is TJSONNull);
end;

function TSafeJSON.IsObject: Boolean;
begin
	Result := Assigned(FValue) and (FValue is TJSONObject);
end;

function TSafeJSON.IsArray: Boolean;
begin
	Result := Assigned(FValue) and (FValue is TJSONArray);
end;

function TSafeJSON.Count: Integer;
begin
	Result := 0;
	if not Assigned(FValue) then
		Exit;
	if FValue is TJSONArray then
		Result := TJSONArray(FValue).Count;
end;

function TSafeJSON.Raw: TJSONValue;
begin
	Result := FValue;
end;

end.
