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
	end;

implementation

uses
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

end.
