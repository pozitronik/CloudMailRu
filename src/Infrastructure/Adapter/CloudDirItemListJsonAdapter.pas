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
	System.Generics.Collections,
	CloudDirItem,
	CloudConstants,
	JSONHelper,
	JSON;

class function TCloudDirItemListJsonAdapter.Parse(const JSON: WideString; var List: TCloudDirItemList): Boolean;
var
	J: Integer;
	A: TJSONArray;
	JSONVal: TJSONObject;
	ParserObj: TJSONObject;
	CountObj: TJSONObject;
	TempFoldersCount, TempFilesCount: Integer;
begin
	Result := False;
	SetLength(List, 0);
	JSONVal := nil;
	try
		try
			if not init(JSON, JSONVal) then
				Exit;
			A := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_LIST] as TJSONArray;
			SetLength(List, A.Count);
			for J := 0 to A.Count - 1 do
			begin
				ParserObj := A.Items[J] as TJSONObject;

				assignFromName(NAME_SIZE, ParserObj, List[J].size);
				assignFromName(NAME_KIND, ParserObj, List[J].kind);
				assignFromName(NAME_WEBLINK, ParserObj, List[J].weblink);
				assignFromName(NAME_TYPE, ParserObj, List[J].type_);
				assignFromName(NAME_HOME, ParserObj, List[J].home);
				assignFromName(NAME_NAME, ParserObj, List[J].name);
				List[J].visible_name := List[J].name;
				assignFromName(NAME_DELETED_AT, ParserObj, List[J].deleted_at);
				assignFromName(NAME_DELETED_FROM, ParserObj, List[J].deleted_from);
				assignFromName(NAME_DELETED_BY, ParserObj, List[J].deleted_by);
				assignFromName(NAME_GREV, ParserObj, List[J].grev);
				assignFromName(NAME_REV, ParserObj, List[J].rev);

				if (List[J].type_ = TYPE_FILE) then
				begin
					assignFromName(NAME_MTIME, ParserObj, List[J].mtime);
					assignFromName(NAME_VIRUS_SCAN, ParserObj, List[J].virus_scan);
					assignFromName(NAME_HASH, ParserObj, List[J].hash);
				end else begin
					assignFromName(NAME_TREE, ParserObj, List[J].tree);

					if Assigned(ParserObj.Values[NAME_COUNT]) then
					begin
						CountObj := ParserObj.Values[NAME_COUNT] as TJSONObject;
						TempFoldersCount := 0;
						TempFilesCount := 0;
						assignFromName(NAME_FOLDERS, CountObj, TempFoldersCount);
						assignFromName(NAME_FILES, CountObj, TempFilesCount);
						List[J].folders_count := TempFoldersCount;
						List[J].files_count := TempFilesCount;
					end;
					List[J].mtime := 0;
				end;
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
