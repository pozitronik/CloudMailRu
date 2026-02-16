unit CloudDirItemJsonAdapter;

{Infrastructure adapter for parsing JSON responses into TCloudDirItem.
 Separates JSON parsing infrastructure concern from domain value object.}

interface

uses
	CloudDirItem,
	SafeJSON;

type
	TCloudDirItemJsonAdapter = class
	public
		{Parses JSON response into TCloudDirItem record.
		 @param JSON The JSON string from API response (expects body.* structure)
		 @param Item Output parameter that receives parsed data
		 @return True if parsing succeeded, False otherwise}
		class function Parse(const JSON: WideString; out Item: TCloudDirItem): Boolean; static;

		{Parses item fields from a JSON node into TCloudDirItem record.
		 Used internally and by CloudDirItemListJsonAdapter to avoid duplication.
		 @param Node The TSafeJSON node containing item data
		 @param Item Output parameter that receives parsed data}
		class procedure ParseFromNode(const Node: TSafeJSON; out Item: TCloudDirItem); static;

		{Serializes a TCloudDirItem to compact JSON string.
		 Uses the same NAME_* constants as parsing to ensure roundtrip fidelity.
		 @param Item The item to serialize
		 @return Compact JSON string}
		class function ItemToJSON(const Item: TCloudDirItem): WideString; static;
	end;

implementation

uses
	SysUtils, JSON,
	CloudConstants;

class procedure TCloudDirItemJsonAdapter.ParseFromNode(const Node: TSafeJSON; out Item: TCloudDirItem);
var
	CountNode: TSafeJSON;
begin
	Item := Default(TCloudDirItem);

	Item.size := Node.Get(NAME_SIZE).AsInt64;
	Item.kind := Node.Get(NAME_KIND).AsString;
	Item.weblink := Node.Get(NAME_WEBLINK).AsString;
	Item.type_ := Node.Get(NAME_TYPE).AsString;
	Item.home := Node.Get(NAME_HOME).AsString;
	Item.name := Node.Get(NAME_NAME).AsString;
	Item.visible_name := Item.name;

	{Trash item fields - zero/empty by default from Default(), parsed if present}
	Item.deleted_at := Node.Get(NAME_DELETED_AT).AsInt;
	Item.deleted_from := Node.Get(NAME_DELETED_FROM).AsString;
	Item.deleted_by := Node.Get(NAME_DELETED_BY).AsInt;

	Item.grev := Node.Get(NAME_GREV).AsInt;
	Item.rev := Node.Get(NAME_REV).AsInt;

	if Item.type_ = TYPE_FILE then
	begin
		Item.mtime := Node.Get(NAME_MTIME).AsInt64;
		Item.virus_scan := Node.Get(NAME_VIRUS_SCAN).AsString;
		Item.hash := Node.Get(NAME_HASH).AsString;
	end
	else
	begin
		Item.tree := Node.Get(NAME_TREE).AsString;

		{Parse nested count object - TSafeJSON handles missing gracefully}
		CountNode := Node.Get(NAME_COUNT);
		Item.folders_count := CountNode.Get(NAME_FOLDERS).AsInt;
		Item.files_count := CountNode.Get(NAME_FILES).AsInt;

		Item.mtime := 0;
	end;
end;

class function TCloudDirItemJsonAdapter.Parse(const JSON: WideString; out Item: TCloudDirItem): Boolean;
var
	Root, Body: TSafeJSON;
begin
	Result := False;
	Item := Default(TCloudDirItem);

	Root := TSafeJSON.Parse(JSON);
	try
		if Root.IsNull then
			Exit;

		Body := Root.Get(NAME_BODY);
		if Body.IsNull then
			Exit;

		ParseFromNode(Body, Item);
		Result := True;
	finally
		Root.Free;
	end;
end;

class function TCloudDirItemJsonAdapter.ItemToJSON(const Item: TCloudDirItem): WideString;
var
	Obj, CountObj: TJSONObject;
begin
	Obj := TJSONObject.Create;
	try
		Obj.AddPair(NAME_SIZE, TJSONNumber.Create(Item.size));
		Obj.AddPair(NAME_KIND, Item.kind);
		Obj.AddPair(NAME_WEBLINK, Item.weblink);
		Obj.AddPair(NAME_TYPE, Item.type_);
		Obj.AddPair(NAME_HOME, Item.home);
		Obj.AddPair(NAME_NAME, Item.name);
		Obj.AddPair(NAME_GREV, TJSONNumber.Create(Item.grev));
		Obj.AddPair(NAME_REV, TJSONNumber.Create(Item.rev));

		if Item.deleted_at <> 0 then
			Obj.AddPair(NAME_DELETED_AT, TJSONNumber.Create(Item.deleted_at));
		if Item.deleted_from <> '' then
			Obj.AddPair(NAME_DELETED_FROM, Item.deleted_from);
		if Item.deleted_by <> 0 then
			Obj.AddPair(NAME_DELETED_BY, TJSONNumber.Create(Item.deleted_by));

		if Item.type_ = TYPE_FILE then
		begin
			Obj.AddPair(NAME_MTIME, TJSONNumber.Create(Item.mtime));
			Obj.AddPair(NAME_VIRUS_SCAN, Item.virus_scan);
			Obj.AddPair(NAME_HASH, Item.hash);
		end else begin
			Obj.AddPair(NAME_TREE, Item.tree);
			CountObj := TJSONObject.Create;
			CountObj.AddPair(NAME_FOLDERS, TJSONNumber.Create(Item.folders_count));
			CountObj.AddPair(NAME_FILES, TJSONNumber.Create(Item.files_count));
			Obj.AddPair(NAME_COUNT, CountObj);
		end;

		Result := Obj.ToJSON;
	finally
		Obj.Free;
	end;
end;

end.
