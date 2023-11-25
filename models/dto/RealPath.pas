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
	end;

function ExtractRealPath(VirtualPath: WideString; isDir: TIsDir = ID_Unset): TRealPath;
function ExtractVirtualPath(RealPath: TRealPath): WideString;
function inAccount(path: TRealPath; ignoreVirtual: boolean = true): boolean;

implementation

function ExtractRealPath(VirtualPath: WideString; isDir: TIsDir = ID_Unset): TRealPath;
var
	List: TStringList;
begin
	Result.account := EmptyWideStr;
	Result.path := EmptyWideStr;
	(*
	 we can't rely on isDir property, cause it can't be clearly determined from the path
	 therefore the property value can be passed as the parameter, when it is known.
	*)
	Result.isDir := isDir;
	Result.upDirItem := False;
	Result.trashDir := False;
	Result.sharedDir := False;
	Result.invitesDir := False;

	if VirtualPath = EmptyWideStr then
		exit; //root
	VirtualPath := Copy(VirtualPath, 2, Length(VirtualPath) - 1);

	List := TStringList.Create;
	MyExtractStrings(['\'], [], PWideChar(VirtualPath), List);

	if (List.Count > 0) and (List.Strings[List.Count - 1] = '..') then
		Result.upDirItem := true;

	if (List.Count > 0) and (List.Strings[List.Count - 1] = '\') then
		Result.isDir := ID_Yes; // it newer happens, actually

	if List.Count = 1 then
	begin
		Result.account := List.Strings[0];
		if (Result.account = VirtualPath) then
			Result.isDir := ID_Yes;

	end else if (List.Count > 1) then
	begin
		Result.account := List.Strings[0];
		List.Delete(0);
		Result.path := Implode(List, '\');
	end;

	List.Destroy;

	if ExtractFileExt(Result.account) = TrashPostfix then
	begin
		Result.trashDir := true;
		Result.account := Copy(Result.account, 1, Length(Result.account) - Length(TrashPostfix));
	end else if ExtractFileExt(Result.account) = SharedPostfix then
	begin
		Result.sharedDir := true;
		Result.account := Copy(Result.account, 1, Length(Result.account) - Length(SharedPostfix));
	end else if ExtractFileExt(Result.account) = InvitesPostfix then
	begin
		Result.invitesDir := true;
		Result.account := Copy(Result.account, 1, Length(Result.account) - Length(InvitesPostfix));
	end;
end;

function ExtractVirtualPath(RealPath: TRealPath): WideString;
begin
	Result := ExcludeTrailingPathDelimiter('\' + IncludeTrailingPathDelimiter(RealPath.account) + RealPath.path);
end;

//проверка, находится ли путь внутри аккаунта. ignoreVirtual - не считать виртуальные каталоги облачными
function inAccount(path: TRealPath; ignoreVirtual: boolean = true): boolean;
begin
	Result := path.account <> EmptyWideStr;
	if Result and ignoreVirtual then
		Result := not(path.trashDir or path.sharedDir or path.invitesDir);
end;

end.
