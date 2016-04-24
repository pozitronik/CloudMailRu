library MailRuCloud;

{
	Sample wfx plugin for Total Commander by D1P.Antikiller

	It just realise all WFX functions

	This plugin is freeware. You may use this source code to
	write your own plugin (free or commercial).

	Functions and parameters descriptions see in plugin API help (fsplugin.hlp)
}

uses
  SysUtils,
  windows,
  Classes,
  PLUGIN_TYPES,
  PLUGIN_MAIN,
  messages,
  CloudMailRu in 'CloudMailRu.pas';

{$E wfx}
{$R *.res}

const
	MaxFileCount = 101; // Количество показываемых плагином "файлов" -1

var
	GlobalPath: string;
	FileCount: integer = 0;
	{ Callback data }
	PluginNum: integer;
	MyProgressProc: TProgressProc;
	MyLogProc: TLogProc;
	MyRequestProc: TRequestProc;

procedure FsStatusInfo(RemoteDir: PAnsiChar; InfoStartEnd, InfoOperation: integer); stdcall;
begin
	// Начало и конец операций FS
	if (InfoStartEnd = FS_STATUS_START) then
	begin
		case InfoOperation of
			FS_STATUS_OP_LIST:
				begin
				end;
			FS_STATUS_OP_GET_SINGLE:
				begin
				end;
			FS_STATUS_OP_GET_MULTI:
				begin
				end;
			FS_STATUS_OP_PUT_SINGLE:
				begin
				end;
			FS_STATUS_OP_PUT_MULTI:
				begin
				end;
			FS_STATUS_OP_RENMOV_SINGLE:
				begin
				end;
			FS_STATUS_OP_RENMOV_MULTI:
				begin
				end;
			FS_STATUS_OP_DELETE:
				begin
				end;
			FS_STATUS_OP_ATTRIB:
				begin
				end;
			FS_STATUS_OP_MKDIR:
				begin
				end;
			FS_STATUS_OP_EXEC:
				begin
				end;
			FS_STATUS_OP_CALCSIZE:
				begin
				end;
			FS_STATUS_OP_SEARCH:
				begin
				end;
			FS_STATUS_OP_SEARCH_TEXT:
				begin
				end;
		end;
		exit;
	end;
	if (InfoStartEnd = FS_STATUS_END) then
	begin
		case InfoOperation of
			FS_STATUS_OP_LIST:
				begin
				end;
			FS_STATUS_OP_GET_SINGLE:
				begin
				end;
			FS_STATUS_OP_GET_MULTI:
				begin
				end;
			FS_STATUS_OP_PUT_SINGLE:
				begin
				end;
			FS_STATUS_OP_PUT_MULTI:
				begin
				end;
			FS_STATUS_OP_RENMOV_SINGLE:
				begin
				end;
			FS_STATUS_OP_RENMOV_MULTI:
				begin
				end;
			FS_STATUS_OP_DELETE:
				begin
				end;
			FS_STATUS_OP_ATTRIB:
				begin
				end;
			FS_STATUS_OP_MKDIR:
				begin
				end;
			FS_STATUS_OP_EXEC:
				begin
				end;
			FS_STATUS_OP_CALCSIZE:
				begin
				end;
			FS_STATUS_OP_SEARCH:
				begin
				end;
			FS_STATUS_OP_SEARCH_TEXT:
				begin
				end;
		end;
		exit;
	end;
end;

function FsInit(PluginNr: integer; pProgressProc: TProgressProc; pLogProc: TLogProc; pRequestProc: TRequestProc): integer; stdcall;
Begin
	PluginNum := PluginNr;
	MyProgressProc := pProgressProc;
	MyLogProc := pLogProc;
	MyRequestProc := pRequestProc;
	// Вход в плагин.
	Result := 0;
end;

function FsInitW(PluginNr: integer; pProgressProc: TProgressProc; pLogProc: TLogProc; pRequestProc: TRequestProc): integer; stdcall;
Begin
	PluginNum := PluginNr;
	MyProgressProc := pProgressProc;
	MyLogProc := pLogProc;
	MyRequestProc := pRequestProc;
	// Вход в плагин.
	Result := 0;
end;

function FsFindFirst(path: PAnsiChar; var FindData: tWIN32FINDDATAA): thandle; stdcall;
begin
	// Получение первого файла в папке. Result тоталом не используется (можно использовать для работы плагина).
	setlasterror(ERROR_NO_MORE_FILES);
	GlobalPath := path;
	if path = '\' then
	begin
		FindData.dwFileAttributes := 16;
		FindData.nFileSizeHigh := 0;
		FindData.nFileSizeLow := 0;
		strpcopy(FindData.cFileName, PAnsiChar('FOLDER'));
	end;
	if path = '\FOLDER' then
	begin
		FindData.dwFileAttributes := 0;
		FindData.nFileSizeHigh := 0;
		FindData.nFileSizeLow := 0;
		strpcopy(FindData.cFileName, 'FILE' + inttostr(FileCount) + '.TXT');
	end;
end;

function FsFindFirstW(path: PWideChar; var FindData: tWIN32FINDDATAW): thandle; stdcall;
begin
	// Получение первого файла в папке. Result тоталом не используется (можно использовать для работы плагина).
	setlasterror(ERROR_NO_MORE_FILES);
	GlobalPath := path;
	if path = '\' then
	begin
		FindData.dwFileAttributes := 16;
		FindData.nFileSizeHigh := 0;
		FindData.nFileSizeLow := 0;
		strpcopy(FindData.cFileName, 'FOLDER');
	end;
	if path = '\FOLDER' then
	begin
		FindData.dwFileAttributes := 0;
		FindData.nFileSizeHigh := 0;
		FindData.nFileSizeLow := 0;
		strpcopy(FindData.cFileName, 'FILE' + inttostr(FileCount) + '.TXT');
	end;
end;

function FsFindNext(Hdl: thandle; var FindData: tWIN32FINDDATAA): bool; stdcall;
begin
	// Получение последующих файлов в папке (вызывается до тех пор, пока не вернёт false).
	if GlobalPath = '\' then
	begin
		Result := false;
		exit;
	end;
	if GlobalPath = '\FOLDER' then
	begin
		inc(FileCount);
		if FileCount = MaxFileCount then
		begin
			Result := false;
			exit;
		end
		else
			Result := true;
		FindData.dwFileAttributes := 0;
		FindData.nFileSizeHigh := 0;
		FindData.nFileSizeLow := 0;
		strpcopy(FindData.cFileName, PAnsiChar('FILE' + inttostr(FileCount)) + '.TXT');
	end;
end;

function FsFindNextW(Hdl: thandle; var FindData: tWIN32FINDDATAW): bool; stdcall;
begin
	// Получение последующих файлов в папке (вызывается до тех пор, пока не вернёт false).
	if GlobalPath = '\' then
	begin
		Result := false;
		exit;
	end;
	if GlobalPath = '\FOLDER' then
	begin
		inc(FileCount);
		if FileCount = MaxFileCount then
		begin
			Result := false;
			exit;
		end
		else
			Result := true;
		FindData.dwFileAttributes := 0;
		FindData.nFileSizeHigh := 0;
		FindData.nFileSizeLow := 0;
		strpcopy(FindData.cFileName, 'FILE' + inttostr(FileCount) + '.TXT');
	end;
end;

function FsFindClose(Hdl: thandle): integer; stdcall;
Begin
	// Завершение получения списка файлов. Result тоталом не используется (всегда равен 0)
	Result := 0;
	FileCount := 0;
end;

// Процедура вызывается один раз при установке плагина
procedure FsGetDefRootName(DefRootName: PAnsiChar; maxlen: integer); stdcall;
Begin
	strlcopy(DefRootName, PAnsiChar('Cloud'), maxlen);
	messagebox(FindTCWindow, PWideChar('Installation succeful'), 'Information', mb_ok + mb_iconinformation);
End;


function FsExecuteFile(MainWin: thandle; RemoteName, Verb: PAnsiChar): integer; stdcall;
Begin
	// Запуск файла
	Result := FS_EXEC_OK;
	if Verb = 'open' then
	begin
		if extractfileext(lowercase(RemoteName)) = '.txt' then
		begin
			WinExec('notepad.exe', 1);
		end;
	end
	else if Verb = 'properties' then
	begin
		messagebox(MainWin, PWideChar(RemoteName), PWideChar(Verb), mb_ok + mb_iconinformation);
	end
	else if copy(Verb, 1, 5) = 'chmod' then
	begin
	end
	else if copy(Verb, 1, 5) = 'quote' then
	begin
	end;
End;

function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall;
begin

	// Копирование файла из файловой системы плагина
end;

function FsPutFile(LocalName, RemoteName: PAnsiChar; CopyFlags: integer): integer; stdcall;
begin
	// Копирование файла в файловую систему плагина
end;

function FsDeleteFile(RemoteName: PAnsiChar): bool; stdcall;
Begin
	// Удаление файла из файловой ссистемы плагина
End;

exports
	FsGetDefRootName,
	FsInit,
	FsInitW,
	FsFindFirst,
	FsFindFirstW,
	FsFindNext,
	FsFindNextW,
	FsFindClose;

(* ,
	FsExecuteFile,
	FsGetFile,
	FsPutFile,
	FsDeleteFile,
	FsStatusInfo,
	; *)
// FsExtractCustomIcon, {В примере отсутствует - при помощи этой функции можно устанавливать свои иконки на отображаемый файл}
begin

end.
