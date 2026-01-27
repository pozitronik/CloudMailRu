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
	JSONHelper,
	JSON;

class function TCloudDirItemJsonAdapter.Parse(const JSON: WideString; out Item: TCloudDirItem): Boolean;
var
	ParserObj, JSONVal, CountObj: TJSONObject;
	TempFoldersCount, TempFilesCount: Integer;
begin
	Result := False;
	JSONVal := nil;
	Item := Default(TCloudDirItem);
	try
		try
			if not init(JSON, JSONVal) then
				Exit;
			ParserObj := JSONVal.Values[NAME_BODY] as TJSONObject;

			assignFromName(NAME_SIZE, ParserObj, Item.size);
			assignFromName(NAME_KIND, ParserObj, Item.kind);
			assignFromName(NAME_WEBLINK, ParserObj, Item.weblink);
			assignFromName(NAME_TYPE, ParserObj, Item.type_);
			assignFromName(NAME_HOME, ParserObj, Item.home);
			assignFromName(NAME_NAME, ParserObj, Item.name);

			if (Item.type_ = TYPE_FILE) then
			begin
				assignFromName(NAME_MTIME, ParserObj, Item.mtime);
				assignFromName(NAME_VIRUS_SCAN, ParserObj, Item.virus_scan);
				assignFromName(NAME_HASH, ParserObj, Item.hash);
			end else begin
				assignFromName(NAME_TREE, ParserObj, Item.tree);
				assignFromName(NAME_GREV, ParserObj, Item.grev);
				assignFromName(NAME_REV, ParserObj, Item.rev);
				if Assigned(ParserObj.Values[NAME_COUNT]) then
				begin
					CountObj := ParserObj.Values[NAME_COUNT] as TJSONObject;
					TempFoldersCount := 0;
					TempFilesCount := 0;
					assignFromName(NAME_FOLDERS, CountObj, TempFoldersCount);
					assignFromName(NAME_FILES, CountObj, TempFilesCount);
					Item.folders_count := TempFoldersCount;
					Item.files_count := TempFilesCount;
				end;
				Item.mtime := 0;
			end;

			Result := True;
		except
			Exit;
		end;
	finally
		JSONVal.Free;
	end;
end;

end.
