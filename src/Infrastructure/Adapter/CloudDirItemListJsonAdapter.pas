unit CloudDirItemListJsonAdapter;

{Infrastructure adapter for parsing JSON responses into TCloudDirItemList.
 Separates JSON parsing infrastructure concern from domain value object.}

interface

uses
	CloudDirItemList;

type
	TCloudDirItemListJsonAdapter = class
	public
		{Parses JSON response into TCloudDirItemList array.
		 @param JSON The JSON string from API response (expects body.list structure)
		 @param List Output parameter that receives parsed array
		 @return True if parsing succeeded, False otherwise}
		class function Parse(const JSON: WideString; var List: TCloudDirItemList): Boolean; static;
	end;

implementation

uses
	CloudConstants,
	SafeJSON;

class function TCloudDirItemListJsonAdapter.Parse(const JSON: WideString; var List: TCloudDirItemList): Boolean;
var
	Root, ListArray, Item, CountNode: TSafeJSON;
	I: Integer;
begin
	Result := False;
	SetLength(List, 0);

	Root := TSafeJSON.Parse(JSON);
	try
		if Root.IsNull then
			Exit;

		ListArray := Root.Get(NAME_BODY).Get(NAME_LIST);
		if ListArray.IsNull or not ListArray.IsArray then
			Exit;

		SetLength(List, ListArray.Count);
		for I := 0 to ListArray.Count - 1 do
		begin
			Item := ListArray.Item(I);

			List[I].size := Item.Get(NAME_SIZE).AsInt64;
			List[I].kind := Item.Get(NAME_KIND).AsString;
			List[I].weblink := Item.Get(NAME_WEBLINK).AsString;
			List[I].type_ := Item.Get(NAME_TYPE).AsString;
			List[I].home := Item.Get(NAME_HOME).AsString;
			List[I].name := Item.Get(NAME_NAME).AsString;
			List[I].visible_name := List[I].name;

			{Trash item fields}
			List[I].deleted_at := Item.Get(NAME_DELETED_AT).AsInt;
			List[I].deleted_from := Item.Get(NAME_DELETED_FROM).AsString;
			List[I].deleted_by := Item.Get(NAME_DELETED_BY).AsInt;

			List[I].grev := Item.Get(NAME_GREV).AsInt;
			List[I].rev := Item.Get(NAME_REV).AsInt;

			if List[I].type_ = TYPE_FILE then
			begin
				List[I].mtime := Item.Get(NAME_MTIME).AsInt64;
				List[I].virus_scan := Item.Get(NAME_VIRUS_SCAN).AsString;
				List[I].hash := Item.Get(NAME_HASH).AsString;
			end
			else
			begin
				List[I].tree := Item.Get(NAME_TREE).AsString;

				{Parse nested count object - TSafeJSON handles missing gracefully}
				CountNode := Item.Get(NAME_COUNT);
				List[I].folders_count := CountNode.Get(NAME_FOLDERS).AsInt;
				List[I].files_count := CountNode.Get(NAME_FILES).AsInt;

				List[I].mtime := 0;
			end;
		end;

		Result := True;
	finally
		Root.Free;
	end;
end;

end.
