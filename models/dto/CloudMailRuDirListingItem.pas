unit CloudMailRuDirListingItem;

interface

uses
	CMRConstants,
	CMRStrings,
	PluginHelper,
	DateUtils,
	SysUtils,
	Windows;

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

end.
