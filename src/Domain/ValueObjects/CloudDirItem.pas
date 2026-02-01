unit CloudDirItem;

interface

uses
	CloudConstants,
	LanguageStrings,
	DateTimeUtils,
	DateUtils,
	SysUtils,
	Windows;

type
	TCloudDirItem = Record
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
		function None: TCloudDirItem; // Creates a special record, which indicate that Item is not found/not applicable.
		function ToFindData(DirsAsSymlinks: Boolean = false): tWIN32FINDDATAW;
	End;

implementation

{TCloudDirItem}

function TCloudDirItem.GetIsDir: Boolean;
begin
	Result := self.type_ = TYPE_DIR
end;

function TCloudDirItem.GetIsFile: Boolean;
begin
	Result := type_ <> TYPE_DIR
end;

function TCloudDirItem.GetIsNone: Boolean;
begin
	Result := self.name = EmptyWideStr;
end;

function TCloudDirItem.GetIsPublished: Boolean;
begin
	Result := self.weblink <> EmptyWideStr;
end;

function TCloudDirItem.None: TCloudDirItem;
begin
	Result := Default(TCloudDirItem);
end;

function TCloudDirItem.ToFindData(DirsAsSymlinks: Boolean): tWIN32FINDDATAW;
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
