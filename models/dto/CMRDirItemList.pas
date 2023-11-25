unit CMRDirItemList;

interface

uses
	System.Generics.Collections,
	SysUtils,
	CMRDirItem,
	CMRConstants,
	JSONHelper,
	JSON;

type
	TCMRDirItemList = TArray<TCMRDirItem>;

	TCMRDirItemListHelper = record helper for TCMRDirItemList
	public
		function FromJSON(JSON: WideString): Boolean;
		function FindByName(ItemName: WideString): TCMRDirItem;
		function FindByHomePath(HomePath: WideString): TCMRDirItem;
	end;

implementation

{TCMRDirItemList}

{It seems that this kind of search is preferrable somehow, but I forgot, why. So, I jusst keep the logic.}
function TCMRDirItemListHelper.FindByHomePath(HomePath: WideString): TCMRDirItem;
var
	CurrentItem: TCMRDirItem;
begin
	HomePath := '/' + StringReplace(HomePath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]);
	for CurrentItem in self do
		if CurrentItem.home = HomePath then
			exit(CurrentItem);
	exit(CurrentItem.None);
end;

function TCMRDirItemListHelper.FindByName(ItemName: WideString): TCMRDirItem;
var
	CurrentItem: TCMRDirItem;
begin
	for CurrentItem in self do
		if CurrentItem.name = ItemName then
			exit(CurrentItem);
	exit(CurrentItem.None);
end;

function TCMRDirItemListHelper.FromJSON(JSON: WideString): Boolean;
var
	J: integer;
	A: TJSONArray;
	JSONVal: TJSONObject;
	ParserObj: TJSONObject;
begin
	result := False;
	SetLength(self, 0);
	try
		if (not init(JSON, JSONVal)) then
			exit;
		A := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_LIST] as TJSONArray;
		SetLength(self, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with self[J] do
			begin
				assignFromName(NAME_SIZE, ParserObj, size);
				assignFromName(NAME_KIND, ParserObj, kind);
				assignFromName(NAME_WEBLINK, ParserObj, weblink);
				assignFromName(NAME_TYPE, ParserObj, type_);
				assignFromName(NAME_HOME, ParserObj, home);
				assignFromName(NAME_NAME, ParserObj, name);
				visible_name := name;
				assignFromName(NAME_DELETED_AT, ParserObj, deleted_at);
				assignFromName(NAME_DELETED_FROM, ParserObj, deleted_from);
				assignFromName(NAME_DELETED_BY, ParserObj, deleted_by);
				assignFromName(NAME_GREV, ParserObj, grev);
				assignFromName(NAME_REV, ParserObj, rev);
				if (type_ = TYPE_FILE) then
				begin
					assignFromName(NAME_MTIME, ParserObj, mtime);
					assignFromName(NAME_VIRUS_SCAN, ParserObj, virus_scan);
					assignFromName(NAME_HASH, ParserObj, hash);
				end else begin
					assignFromName(NAME_TREE, ParserObj, tree);

					if Assigned(ParserObj.Values[NAME_COUNT]) then
					begin
						folders_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FOLDERS].Value.ToInteger();
						files_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FILES].Value.ToInteger();
					end;
					mtime := 0;
				end;
			end;
		end;
	except
		exit;
	end;
	result := true;
	JSONVal.free;
end;

end.
