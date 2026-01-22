unit CMRDirItem;

interface

uses
	CMRConstants,
	LANGUAGE_STRINGS,
	SystemHelper,
	DateUtils,
	SysUtils,
	Windows;

type
	TCMRDirItem = Record
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
	private
		function GetIsNone: Boolean; //Check, if it is a special record which can't be used
		function GetIsDir: Boolean;
		function GetIsFile: Boolean;
		function GetIsPublished: Boolean;
	public
		property isNone: Boolean read GetIsNone;
		property isDir: Boolean read GetIsDir;
		property isFile: Boolean read GetIsFile;
		property isPublished: Boolean read GetIsPublished;
		function None: TCMRDirItem; // Creates a special record, which indicate that Item is not found/not applicable.
		function ToFindData(DirsAsSymlinks: Boolean = false): tWIN32FINDDATAW;
	End;

implementation

{TCMRDirListingItem}

function TCMRDirItem.GetIsDir: Boolean;
begin
	Result := self.type_ = TYPE_DIR
end;

function TCMRDirItem.GetIsFile: Boolean;
begin
	Result := type_ <> TYPE_DIR
end;

function TCMRDirItem.GetIsNone: Boolean;
begin
	Result := self.name = EmptyWideStr;
end;

function TCMRDirItem.GetIsPublished: Boolean;
begin
	Result := self.weblink <> EmptyWideStr;
end;

function TCMRDirItem.None: TCMRDirItem;
begin
	FillChar(self, sizeof(self), 0);
	Result := self;
end;

function TCMRDirItem.ToFindData(DirsAsSymlinks: Boolean): tWIN32FINDDATAW;
begin
	FillChar(Result, sizeof(WIN32_FIND_DATA), 0);
	if (self.deleted_from <> EmptyWideStr) then //items inside trash bin
	begin
		Result.ftCreationTime := DateTimeToFileTime(UnixToDateTime(self.deleted_at, false));
		Result.ftLastWriteTime := Result.ftCreationTime;
		if (self.type_ = TYPE_DIR) then
			Result.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY
	end else if (self.type_ = TYPE_DIR) or (self.kind = KIND_SHARED) then
	begin
		if not DirsAsSymlinks then
			Result.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
	end else begin
		Result.ftCreationTime := DateTimeToFileTime(UnixToDateTime(self.mtime, false));
		Result.ftLastWriteTime := Result.ftCreationTime;

		Result.dwFileAttributes := 0;
	end;
	Result.nFileSizeHigh := DWORD((self.size shr 32) and $FFFFFFFF);
	Result.nFileSizeLow := DWORD(self.size and $FFFFFFFF);
	strpcopy(Result.cFileName, self.name);
end;

end.
