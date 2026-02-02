unit ANSICompatibility;
{Legacy ANSI stubs for older Total Commander versions}

interface

uses
	Windows,
	WFXTypes,
	AnsiStrings;

procedure FsGetDefRootName(DefRootName: PAnsiChar; maxlen: integer); stdcall; {Called once during plugin installation}
procedure FsStatusInfo(RemoteDir: PAnsiChar; InfoStartEnd, InfoOperation: integer); stdcall;
function FsFindFirst(path: PAnsiChar; var FindData: tWIN32FINDDATAA): THandle; stdcall;
function FsFindNext(Hdl: THandle; var FindData: tWIN32FINDDATAA): Bool; stdcall;
function FsExecuteFile(MainWin: THandle; RemoteName, Verb: PAnsiChar): integer; stdcall; {Execute file}
function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall; {Copy file from plugin filesystem}
function FsPutFile(LocalName, RemoteName: PAnsiChar; CopyFlags: integer): integer; stdcall; {Copy file to plugin filesystem}
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

procedure FsGetDefRootName(DefRootName: PAnsiChar; maxlen: integer); stdcall; {Called once during plugin installation}
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
	Result := ERROR_INVALID_HANDLE; {ANSI stub}
end;

function FsFindNext(Hdl: THandle; var FindData: tWIN32FINDDATAA): Bool; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := false; {ANSI stub}
end;

function FsExecuteFile(MainWin: THandle; RemoteName, Verb: PAnsiChar): integer; stdcall; {Execute file}
Begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := FS_EXEC_ERROR; {ANSI stub}
End;

function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall; {Copy file from plugin filesystem}
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := FS_FILE_NOTSUPPORTED; {ANSI stub}
end;

function FsPutFile(LocalName, RemoteName: PAnsiChar; CopyFlags: integer): integer; stdcall; {Copy file to plugin filesystem}
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := FS_FILE_NOTSUPPORTED; {ANSI stub}
end;

function FsDeleteFile(RemoteName: PAnsiChar): Bool; stdcall; {Delete file from plugin filesystem}
Begin
	SetLastError(ERROR_INVALID_FUNCTION); {ANSI stub}
	Result := false;
End;

function FsRenMovFile(OldName: PAnsiChar; NewName: PAnsiChar; Move: Boolean; OverWrite: Boolean; ri: pRemoteInfo): integer;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := FS_FILE_NOTSUPPORTED; {ANSI stub}
end;

function FsDisconnect(DisconnectRoot: PAnsiChar): Bool; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := false; {ANSI stub}
end;

function FsMkDir(path: PAnsiChar): Bool; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := false; {ANSI stub}
end;

function FsRemoveDir(RemoteName: PAnsiChar): Bool; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := false; {ANSI stub}
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
	Result := FS_FILE_NOTSUPPORTED; {ANSI stub}
end;

function FsGetPreviewBitmap(RemoteName: pchar; Width, Height: integer; var ReturnedBitmap: HBITMAP): integer; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	ReturnedBitmap := 0;
	Result := FS_BITMAP_NONE; {ANSI stub}
end;

end.
