unit RealPath;

interface

uses
	Classes,
	SysUtils,
	PluginHelper,
	StringHelper;

type
	TIsDir = (ID_Yes, ID_No, ID_Unset);

	TRealPath = record
		account: WideString;
		path: WideString;
		isDir: TIsDir; //is it a directory
		upDirItem: boolean; //path/../
		trashDir: boolean; //item is inside of a trash bin dir
		sharedDir: boolean; //item is inside of a shared links dir
		invitesDir: boolean; //item is inside of an invites dir
		procedure FromPath(VirtualPath: WideString; isDir: TIsDir = ID_Unset);
		class function GetRealPath(VirtualPath: WideString; isDir: TIsDir = ID_Unset): TRealPath; static;
		function ToPath: WideString;
		function IsInAccount(ignoreVirtual: boolean = true): boolean; //проверка, находится ли путь внутри аккаунта. ignoreVirtual - не считать виртуальные каталоги облачными
	end;

implementation

{TRealPath}

procedure TRealPath.FromPath(VirtualPath: WideString; isDir: TIsDir);
var
	List: TStringList;
begin
	self.account := EmptyWideStr;
	self.path := EmptyWideStr;
	(*
	 we can't rely on isDir property, cause it can't be clearly determined from the path
	 therefore the property value can be passed as the parameter, when it is known.
	*)
	self.isDir := isDir;
	self.upDirItem := False;
	self.trashDir := False;
	self.sharedDir := False;
	self.invitesDir := False;

	if VirtualPath = EmptyWideStr then
		exit; //root
	VirtualPath := Copy(VirtualPath, 2, Length(VirtualPath) - 1);

	List := TStringList.Create;
	MyExtractStrings(['\'], [], PWideChar(VirtualPath), List);

	if (List.Count > 0) and (List.Strings[List.Count - 1] = '..') then
		self.upDirItem := true;

	if (List.Count > 0) and (List.Strings[List.Count - 1] = '\') then
		self.isDir := ID_Yes; // it newer happens, actually

	if List.Count = 1 then
	begin
		self.account := List.Strings[0];
		if (self.account = VirtualPath) then
			self.isDir := ID_Yes;

	end else if (List.Count > 1) then
	begin
		self.account := List.Strings[0];
		List.Delete(0);
		self.path := Implode(List, '\');
	end;

	List.Destroy;

	if ExtractFileExt(self.account) = TrashPostfix then
	begin
		self.trashDir := true;
		self.account := Copy(self.account, 1, Length(self.account) - Length(TrashPostfix));
	end else if ExtractFileExt(self.account) = SharedPostfix then
	begin
		self.sharedDir := true;
		self.account := Copy(self.account, 1, Length(self.account) - Length(SharedPostfix));
	end else if ExtractFileExt(self.account) = InvitesPostfix then
	begin
		self.invitesDir := true;
		self.account := Copy(self.account, 1, Length(self.account) - Length(InvitesPostfix));
	end;
end;

class function TRealPath.GetRealPath(VirtualPath: WideString; isDir: TIsDir): TRealPath;
begin
	result.FromPath(VirtualPath, isDir);
end;

function TRealPath.IsInAccount(ignoreVirtual: boolean): boolean;
begin
	result := self.account <> EmptyWideStr;
	if result and ignoreVirtual then
		result := not(self.trashDir or self.sharedDir or self.invitesDir);
end;

function TRealPath.ToPath: WideString;
begin
	result := ExcludeTrailingPathDelimiter('\' + IncludeTrailingPathDelimiter(self.account) + self.path);
end;

end.
