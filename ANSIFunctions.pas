unit ANSIFunctions;
{DIRTY ANSI PEASANTS}

interface

uses Windows, PLUGIN_TYPES, AnsiStrings;

procedure FsGetDefRootName(DefRootName: PAnsiChar; maxlen: integer); stdcall; //Процедура вызывается один раз при установке плагина
procedure FsStatusInfo(RemoteDir: PAnsiChar; InfoStartEnd, InfoOperation: integer); stdcall;
function FsFindFirst(path: PAnsiChar; var FindData: tWIN32FINDDATAA): THandle; stdcall;
function FsFindNext(Hdl: THandle; var FindData: tWIN32FINDDATAA): Bool; stdcall;
function FsExecuteFile(MainWin: THandle; RemoteName, Verb: PAnsiChar): integer; stdcall; //Запуск файла
function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall; //Копирование файла из файловой системы плагина
function FsPutFile(LocalName, RemoteName: PAnsiChar; CopyFlags: integer): integer; stdcall; //Копирование файла в файловую систему плагина
function FsDeleteFile(RemoteName: PAnsiChar): Bool; stdcall;
function FsRenMovFile(OldName: PAnsiChar; NewName: PAnsiChar; Move: Boolean; OverWrite: Boolean; ri: pRemoteInfo): integer;
function FsDisconnect(DisconnectRoot: PAnsiChar): Bool; stdcall;
function FsMkDir(path: PAnsiChar): Bool; stdcall;
function FsRemoveDir(RemoteName: PAnsiChar): Bool; stdcall;
procedure FsSetCryptCallback(PCryptProc: TCryptProcW; CryptoNr: integer; Flags: integer); stdcall;
function FsContentGetSupportedField(FieldIndex: integer; FieldName: PAnsiChar; Units: PAnsiChar; maxlen: integer): integer; stdcall;
function FsContentGetValue(FileName: PAnsiChar; FieldIndex: integer; UnitIndex: integer; FieldValue: Pointer; maxlen: integer; Flags: integer): integer; stdcall;
function FsExtractCustomIcon(RemoteName: pchar; ExtractFlags: integer; var TheIcon: hicon): integer; stdcall;

implementation

procedure FsGetDefRootName(DefRootName: PAnsiChar; maxlen: integer); stdcall; //Процедура вызывается один раз при установке плагина
Begin
	AnsiStrings.StrLCopy(DefRootName, PAnsiChar('CloudMailRu'), maxlen);
End;

procedure FsStatusInfo(RemoteDir: PAnsiChar; InfoStartEnd, InfoOperation: integer); stdcall;
begin
	SetLastError(ERROR_NOT_SUPPORTED);
end;

function FsFindFirst(path: PAnsiChar; var FindData: tWIN32FINDDATAA): THandle; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := ERROR_INVALID_HANDLE; //Ansi-заглушка
end;

function FsFindNext(Hdl: THandle; var FindData: tWIN32FINDDATAA): Bool; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := false; //Ansi-заглушка
end;

function FsExecuteFile(MainWin: THandle; RemoteName, Verb: PAnsiChar): integer; stdcall; //Запуск файла
Begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := FS_EXEC_ERROR; //Ansi-заглушка
End;

function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall; //Копирование файла из файловой системы плагина
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := FS_FILE_NOTSUPPORTED; //Ansi-заглушка
end;

function FsPutFile(LocalName, RemoteName: PAnsiChar; CopyFlags: integer): integer; stdcall; //Копирование файла в файловую систему плагина
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := FS_FILE_NOTSUPPORTED; //Ansi-заглушка
end;

function FsDeleteFile(RemoteName: PAnsiChar): Bool; stdcall; //Удаление файла из файловой ссистемы плагина
Begin
	SetLastError(ERROR_INVALID_FUNCTION); //Ansi-заглушка
	Result := false;
End;

function FsRenMovFile(OldName: PAnsiChar; NewName: PAnsiChar; Move: Boolean; OverWrite: Boolean; ri: pRemoteInfo): integer;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := FS_FILE_NOTSUPPORTED; //Ansi-заглушка
end;

function FsDisconnect(DisconnectRoot: PAnsiChar): Bool; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := false; //ansi-заглушка
end;

function FsMkDir(path: PAnsiChar): Bool; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := false; //ansi-заглушка
end;

function FsRemoveDir(RemoteName: PAnsiChar): Bool; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := false; //ansi-заглушка
end;

procedure FsSetCryptCallback(PCryptProc: TCryptProcW; CryptoNr: integer; Flags: integer); stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
end;

function FsContentGetSupportedField(FieldIndex: integer; FieldName: PAnsiChar; Units: PAnsiChar; maxlen: integer): integer; stdcall;
begin
	Result := ft_nomorefields;
	case FieldIndex of
		0:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'tree');
				Result := ft_stringw;
			end;
		1:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'name');
				Result := ft_stringw;
			end;
		2:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'grev');
				Result := ft_numeric_32;
			end;
		3:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'size');
				Result := ft_numeric_64;
			end;
		4:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'kind');
				Result := ft_stringw;
			end;
		5:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'weblink');
				Result := ft_stringw;
			end;
		6:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'rev');
				Result := ft_numeric_32;
			end;
		7:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'type');
				Result := ft_stringw;
			end;
		8:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'home');
				Result := ft_stringw;
			end;
		9:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'mtime');
				Result := ft_datetime;
			end;
		10:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'hash');
				Result := ft_stringw;
			end;
		11:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'virus_scan');
				Result := ft_stringw;
			end;
		12:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'folders_count');
				Result := ft_numeric_32;
			end;
		13:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'files_count');
				Result := ft_numeric_32;
			end;
		14:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'description');
				Result := ft_stringw;
			end;
		15:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'deleted_at');
				Result := ft_datetime;
			end;
		16:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'deleted_from');
				Result := ft_stringw;
			end;
		17:
			begin
				System.AnsiStrings.strpcopy(FieldName, 'deleted_by');
				Result := ft_stringw;
			end;
	end;
end;

function FsContentGetValue(FileName: PAnsiChar; FieldIndex: integer; UnitIndex: integer; FieldValue: Pointer; maxlen: integer; Flags: integer): integer; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := ft_nosuchfield;
end;

function FsExtractCustomIcon(RemoteName: pchar; ExtractFlags: integer; var TheIcon: hicon): integer; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := FS_FILE_NOTSUPPORTED; //Ansi-заглушка
end;

end.
