﻿unit CloudMailRuDirListingItem;

interface

uses
	CMRConstants,
	CMRStrings,
	SystemHelper,
	DateUtils,
	SysUtils,
	Windows,
	JSONHelper,
	JSON;

type
	TCloudMailRuDirListingItem = Record
		tree: WideString;
		name: WideString;
		visible_name: WideString;
		grev: integer;
		size: int64;
		kind: WideString;
		weblink: WideString;
		rev: integer;
		type_: WideString;
		home: WideString;
		mtime: int64;
		hash: WideString;
		virus_scan: WideString;
		folders_count: integer;
		files_count: integer;
		deleted_at: integer;
		deleted_from: WideString;
		deleted_by: integer;
	End;

function CloudMailRuDirListingItemToFindData(DirListing: TCloudMailRuDirListingItem; DirsAsSymlinks: Boolean = false): tWIN32FINDDATAW;
function getFileStatus(JSON: WideString; var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean;

implementation

function CloudMailRuDirListingItemToFindData(DirListing: TCloudMailRuDirListingItem; DirsAsSymlinks: Boolean = false): tWIN32FINDDATAW;
begin
	FillChar(Result, sizeof(WIN32_FIND_DATA), 0);
	if (DirListing.deleted_from <> EMPTY_STR) then //items inside trash bin
	begin
		Result.ftCreationTime := DateTimeToFileTime(UnixToDateTime(DirListing.deleted_at, false));
		Result.ftLastWriteTime := Result.ftCreationTime;
		if (DirListing.type_ = TYPE_DIR) then
			Result.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY
	end else if (DirListing.type_ = TYPE_DIR) or (DirListing.kind = KIND_SHARED) then
	begin
		if not DirsAsSymlinks then
			Result.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
	end else begin
		Result.ftCreationTime := DateTimeToFileTime(UnixToDateTime(DirListing.mtime, false));
		Result.ftLastWriteTime := Result.ftCreationTime;

		Result.dwFileAttributes := 0;
	end;
	Result.nFileSizeHigh := DWORD((DirListing.size shr 32) and $FFFFFFFF);
	Result.nFileSizeLow := DWORD(DirListing.size and $FFFFFFFF);
	strpcopy(Result.cFileName, DirListing.name);
end;

function getFileStatus(JSON: WideString; var CloudMailRuDirListingItem: TCloudMailRuDirListingItem): Boolean;
var
	ParserObj, JSONVal: TJSONObject;
begin
	Result := false;
	try
		if (not init(JSON, JSONVal)) then
			Exit;
		ParserObj := JSONVal.Values[NAME_BODY] as TJSONObject;
		with CloudMailRuDirListingItem do
		begin
			assignFromName(NAME_SIZE, ParserObj, size);
			assignFromName(NAME_KIND, ParserObj, kind);
			assignFromName(NAME_WEBLINK, ParserObj, weblink);
			assignFromName(NAME_TYPE, ParserObj, type_);
			assignFromName(NAME_HOME, ParserObj, home);
			assignFromName(NAME_NAME, ParserObj, name);
			if (type_ = TYPE_FILE) then
			begin
				assignFromName(NAME_MTIME, ParserObj, mtime);
				assignFromName(NAME_VIRUS_SCAN, ParserObj, virus_scan);
				assignFromName(NAME_HASH, ParserObj, hash);
			end else begin
				assignFromName(NAME_TREE, ParserObj, tree);
				assignFromName(NAME_GREV, ParserObj, grev);
				assignFromName(NAME_REV, ParserObj, rev);
				if Assigned((ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FOLDERS]) then
					folders_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FOLDERS].Value.ToInteger();
				if Assigned((ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FILES]) then
					files_count := (ParserObj.Values[NAME_COUNT] as TJSONObject).Values[NAME_FILES].Value.ToInteger();
				mtime := 0;
			end;
		end;
	except
		Exit;
	end;
	Result := true;
	JSONVal.free;
end;

end.