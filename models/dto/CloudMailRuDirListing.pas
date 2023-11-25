unit CloudMailRuDirListing;

interface

uses
	System.Generics.Collections,
	SysUtils,
	CMRDirListingItem,
	CMRConstants,
	JSONHelper,
	JSON;

type
	TCloudMailRuDirListing = TArray<TCMRDirListingItem>;

function GetItemByName(DirListing: TCloudMailRuDirListing; ItemName: WideString): TCMRDirListingItem;
function GetItemByHomePath(DirListing: TCloudMailRuDirListing; HomePath: WideString): TCMRDirListingItem;
function FromJSON(JSON: WideString; var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean;

implementation

function GetItemByName(DirListing: TCloudMailRuDirListing; ItemName: WideString): TCMRDirListingItem;
var
	CurrentItem: TCMRDirListingItem;
begin
	for CurrentItem in DirListing do
		if CurrentItem.name = ItemName then
			exit(CurrentItem);
	FillChar(result, sizeof(TCMRDirListingItem), 0); // nothing found
end;

function GetItemByHomePath(DirListing: TCloudMailRuDirListing; HomePath: WideString): TCMRDirListingItem;
var
	CurrentItem: TCMRDirListingItem;
begin
	HomePath := '/' + StringReplace(HomePath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]);
	for CurrentItem in DirListing do
		if CurrentItem.home = HomePath then
			exit(CurrentItem);
	FillChar(result, sizeof(TCMRDirListingItem), 0); // nothing found
end;

function FromJSON(JSON: WideString; var CloudMailRuDirListing: TCloudMailRuDirListing): Boolean;
var
	J: integer;
	A: TJSONArray;
	JSONVal: TJSONObject;
	ParserObj: TJSONObject;
begin
	result := False;
	try
		if (not init(JSON, JSONVal)) then
			exit;
		A := (JSONVal.Values[NAME_BODY] as TJSONObject).Values[NAME_LIST] as TJSONArray;
		SetLength(CloudMailRuDirListing, A.count);
		for J := 0 to A.count - 1 do
		begin
			ParserObj := A.Items[J] as TJSONObject;
			with CloudMailRuDirListing[J] do
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
