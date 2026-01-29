unit ANSICompatibility;
{Legacy ANSI stubs for older Total Commander versions}

interface

uses
	Windows,
	WFXTypes,
	AnsiStrings;

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
function FsContentGetValue(FileName: PAnsiChar; FieldIndex: integer; UnitIndex: integer; FieldValue: Pointer; maxlen: integer; Flags: integer): integer; stdcall;
function FsExtractCustomIcon(RemoteName: pchar; ExtractFlags: integer; var TheIcon: hicon): integer; stdcall;
function FsGetPreviewBitmap(RemoteName: pchar; Width, Height: integer; var ReturnedBitmap: HBITMAP): integer; stdcall;

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

function FsGetPreviewBitmap(RemoteName: pchar; Width, Height: integer; var ReturnedBitmap: HBITMAP): integer; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	ReturnedBitmap := 0;
	Result := FS_BITMAP_NONE; //Ansi-заглушка
end;

end.
