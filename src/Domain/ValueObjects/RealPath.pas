unit RealPath;

interface

uses
	Classes,
	SysUtils,
	SETTINGS_CONSTANTS,
	StringHelper;

type
	TIsDir = (ID_True, ID_False, ID_Unset);

	TRealPath = record
	private
		function GetIsVirtual: boolean;
		function GetHasHomePath: boolean;
		function GetIsInAccountsList: boolean;
		function GetIsAccountEmpty: boolean;
	public
		account: WideString;
		path: WideString;
		isDir: TIsDir; //is it a directory
		upDirItem: boolean; //path/../
		trashDir: boolean; //item is inside of a trash bin dir
		sharedDir: boolean; //item is inside of a shared links dir
		invitesDir: boolean; //item is inside of an invites dir
		property isVirtual: boolean read GetIsVirtual;
		property hasHomePath: boolean read GetHasHomePath;
		property isInAccountsList: boolean read GetIsInAccountsList;
		property isAccountEmpty: boolean read GetIsAccountEmpty;
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
	{We can't rely on isDir property, cause it can't be clearly determined from the path
		therefore the property value can be passed as the parameter, when it is known.}
	self.isDir := isDir;
	self.upDirItem := False;
	self.trashDir := False;
	self.sharedDir := False;
	self.invitesDir := False;

	if VirtualPath = EmptyWideStr then
		exit; //root
	VirtualPath := Copy(VirtualPath, 2, Length(VirtualPath) - 1);

	List := TStringList.Create;
	try
		MyExtractStrings(['\'], [], PWideChar(VirtualPath), List);

		if (List.Count > 0) and (List.Strings[List.Count - 1] = '..') then
			self.upDirItem := true;

		if (List.Count > 0) and (List.Strings[List.Count - 1] = '\') then
			self.isDir := ID_True; // it newer happens, actually

		if List.Count = 1 then
		begin
			self.account := List.Strings[0];
			if (self.account = VirtualPath) then
				self.isDir := ID_True;

		end else if (List.Count > 1) then
		begin
			self.account := List.Strings[0];
			List.Delete(0);
			self.path := Implode(List, '\');
		end;
	finally
		List.Free;
	end;

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

{Returns True if current path is in the main accounts list.
	Note: account attribute can not be used for this check because it contains the currently listed item name}
function TRealPath.GetIsAccountEmpty: boolean;
begin
	result := self.account = EmptyWideStr;
end;

function TRealPath.GetIsInAccountsList: boolean;
begin
	result := self.path = EmptyWideStr;
end;

function TRealPath.GetIsVirtual: boolean;
begin
	result := self.trashDir or self.sharedDir or self.invitesDir;
end;

class function TRealPath.GetRealPath(VirtualPath: WideString; isDir: TIsDir): TRealPath;
begin
	result.FromPath(VirtualPath, isDir);
end;

function TRealPath.GetHasHomePath: boolean;
begin
	{Виртуальные каталоги не имеют HomePath. Почему тут не включается invitesDir - я не помню}
	result := not(self.trashDir or self.sharedDir);
end;

function TRealPath.IsInAccount(ignoreVirtual: boolean): boolean;
begin
	result := not self.isAccountEmpty;
	if result and ignoreVirtual then
		result := not(self.isVirtual);
end;

function TRealPath.ToPath: WideString;
begin
	result := ExcludeTrailingPathDelimiter('\' + IncludeTrailingPathDelimiter(self.account) + self.path);
end;

end.
