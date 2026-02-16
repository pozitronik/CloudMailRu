unit CloudDirItemListJsonAdapter;

{Infrastructure adapter for parsing JSON responses into TCloudDirItemList.
 Separates JSON parsing infrastructure concern from domain value object.}

interface

uses
	CloudDirItemList;

type
	TCloudDirItemListJsonAdapter = class
	public
		{Parses JSON response into TCloudDirItemList array, returning expected total from body.count.
		 @param JSON The JSON string from API response (expects body.list structure)
		 @param List Output parameter that receives parsed array
		 @param ExpectedCount Output: expected total items (files + folders) from body.count; 0 if absent
		 @return True if parsing succeeded, False otherwise}
		class function Parse(const JSON: WideString; var List: TCloudDirItemList; out ExpectedCount: Integer): Boolean; overload; static;

		{Parses JSON response into TCloudDirItemList array (ignores expected count).
		 @param JSON The JSON string from API response (expects body.list structure)
		 @param List Output parameter that receives parsed array
		 @return True if parsing succeeded, False otherwise}
		class function Parse(const JSON: WideString; var List: TCloudDirItemList): Boolean; overload; static;

		{Serializes a TCloudDirItemList to compact JSON string.
		 Produces a "list":[...] structure compatible with Parse.
		 @param Items The item list to serialize
		 @return Compact JSON string}
		class function ToJSON(const Items: TCloudDirItemList): WideString; static;
	end;

implementation

uses
	SysUtils,
	CloudConstants,
	SafeJSON,
	CloudDirItemJsonAdapter;

class function TCloudDirItemListJsonAdapter.Parse(const JSON: WideString; var List: TCloudDirItemList; out ExpectedCount: Integer): Boolean;
var
	Root, Body, ListArray, CountNode: TSafeJSON;
	I: Integer;
begin
	Result := False;
	ExpectedCount := 0;
	SetLength(List, 0);

	Root := TSafeJSON.Parse(JSON);
	try
		if Root.IsNull then
			Exit;

		Body := Root.Get(NAME_BODY);
		ListArray := Body.Get(NAME_LIST);
		if ListArray.IsNull or not ListArray.IsArray then
			Exit;

		{Extract expected total from body.count (files + folders)}
		CountNode := Body.Get(NAME_COUNT);
		ExpectedCount := CountNode.Get(NAME_FILES).AsInt + CountNode.Get(NAME_FOLDERS).AsInt;

		SetLength(List, ListArray.Count);
		for I := 0 to ListArray.Count - 1 do
			TCloudDirItemJsonAdapter.ParseFromNode(ListArray.Item(I), List[I]);

		Result := True;
	finally
		Root.Free;
	end;
end;

class function TCloudDirItemListJsonAdapter.Parse(const JSON: WideString; var List: TCloudDirItemList): Boolean;
var
	Ignored: Integer;
begin
	Result := Parse(JSON, List, Ignored);
end;

class function TCloudDirItemListJsonAdapter.ToJSON(const Items: TCloudDirItemList): WideString;
var
	I: Integer;
	SB: TStringBuilder;
begin
	SB := TStringBuilder.Create;
	try
		SB.Append('{"list":[');
		for I := 0 to High(Items) do
		begin
			if I > 0 then
				SB.Append(',');
			SB.Append(TCloudDirItemJsonAdapter.ItemToJSON(Items[I]));
		end;
		SB.Append(']}');
		Result := SB.ToString;
	finally
		SB.Free;
	end;
end;

end.
