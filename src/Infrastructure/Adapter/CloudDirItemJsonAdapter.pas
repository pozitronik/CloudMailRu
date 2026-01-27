unit CloudDirItemJsonAdapter;

{Infrastructure adapter for parsing JSON responses into TCloudDirItem.
 Separates JSON parsing infrastructure concern from domain value object.}

interface

uses
	CloudDirItem;

type
	TCloudDirItemJsonAdapter = class
	public
		{Parses JSON response into TCloudDirItem record.
		 @param JSON The JSON string from API response (expects body.* structure)
		 @param Item Output parameter that receives parsed data
		 @return True if parsing succeeded, False otherwise}
		class function Parse(const JSON: WideString; out Item: TCloudDirItem): Boolean; static;
	end;

implementation

uses
	CloudConstants,
	SafeJSON;

class function TCloudDirItemJsonAdapter.Parse(const JSON: WideString; out Item: TCloudDirItem): Boolean;
var
	Root, Body, CountNode: TSafeJSON;
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

		Item.size := Body.Get(NAME_SIZE).AsInt64;
		Item.kind := Body.Get(NAME_KIND).AsString;
		Item.weblink := Body.Get(NAME_WEBLINK).AsString;
		Item.type_ := Body.Get(NAME_TYPE).AsString;
		Item.home := Body.Get(NAME_HOME).AsString;
		Item.name := Body.Get(NAME_NAME).AsString;

		if Item.type_ = TYPE_FILE then
		begin
			Item.mtime := Body.Get(NAME_MTIME).AsInt64;
			Item.virus_scan := Body.Get(NAME_VIRUS_SCAN).AsString;
			Item.hash := Body.Get(NAME_HASH).AsString;
		end
		else
		begin
			Item.tree := Body.Get(NAME_TREE).AsString;
			Item.grev := Body.Get(NAME_GREV).AsInt;
			Item.rev := Body.Get(NAME_REV).AsInt;

			{Parse nested count object - TSafeJSON handles missing gracefully}
			CountNode := Body.Get(NAME_COUNT);
			Item.folders_count := CountNode.Get(NAME_FOLDERS).AsInt;
			Item.files_count := CountNode.Get(NAME_FILES).AsInt;

			Item.mtime := 0;
		end;

		Result := True;
	finally
		Root.Free;
	end;
end;

end.
