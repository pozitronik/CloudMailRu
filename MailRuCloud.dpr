library MailRuCloud;

{$R *.dres}

uses
	SysUtils,
	DateUtils,
	windows,
	Classes,
	PLUGIN_TYPES,
	PLUGIN_MAIN,
	messages,
	inifiles,
	CloudMailRu in 'CloudMailRu.pas';

{$E wfx}
{$R *.res}

type
	TRealPath = record
		account: WideString;
		path: WideString;
	end;

var
	tmp: pchar;
	tempString: WideString;
	GlobalPath, PluginPath: WideString;
	FileCounter: integer = 0;
	{ Callback data }
	PluginNum: integer;
	MyProgressProc: TProgressProc;
	MyLogProc: TLogProc;
	MyRequestProc: TRequestProc;
	Cloud: TCloudMailRu;
	CurrentListing: TCloudMailRuDirListing;
	PluginIniFile: TIniFile;
	CurrentLogon: boolean;

function DateTimeToFileTime(FileTime: TDateTime): TFileTime;
var
	LocalFileTime, Ft: TFileTime;
	SystemTime: TSystemTime;
begin
	Result.dwLowDateTime := 0;
	Result.dwHighDateTime := 0;
	DateTimeToSystemTime(FileTime, SystemTime);
	SystemTimeToFileTime(SystemTime, LocalFileTime);
	LocalFileTimeToFileTime(LocalFileTime, Ft);
	Result := Ft;
end;

procedure FsStatusInfo(RemoteDir: PAnsiChar; InfoStartEnd, InfoOperation: integer); stdcall;
begin
	// Начало и конец операций FS
	if (InfoStartEnd = FS_STATUS_START) then begin
		case InfoOperation of
			FS_STATUS_OP_LIST: begin
				end;
			FS_STATUS_OP_GET_SINGLE: begin
				end;
			FS_STATUS_OP_GET_MULTI: begin
				end;
			FS_STATUS_OP_PUT_SINGLE: begin
				end;
			FS_STATUS_OP_PUT_MULTI: begin
				end;
			FS_STATUS_OP_RENMOV_SINGLE: begin
				end;
			FS_STATUS_OP_RENMOV_MULTI: begin
				end;
			FS_STATUS_OP_DELETE: begin
				end;
			FS_STATUS_OP_ATTRIB: begin
				end;
			FS_STATUS_OP_MKDIR: begin
				end;
			FS_STATUS_OP_EXEC: begin
				end;
			FS_STATUS_OP_CALCSIZE: begin
				end;
			FS_STATUS_OP_SEARCH: begin
				end;
			FS_STATUS_OP_SEARCH_TEXT: begin
				end;
		end;
		exit;
	end;
	if (InfoStartEnd = FS_STATUS_END) then begin
		case InfoOperation of
			FS_STATUS_OP_LIST: begin
				end;
			FS_STATUS_OP_GET_SINGLE: begin
				end;
			FS_STATUS_OP_GET_MULTI: begin
				end;
			FS_STATUS_OP_PUT_SINGLE: begin
				end;
			FS_STATUS_OP_PUT_MULTI: begin
				end;
			FS_STATUS_OP_RENMOV_SINGLE: begin
				end;
			FS_STATUS_OP_RENMOV_MULTI: begin
				end;
			FS_STATUS_OP_DELETE: begin
				end;
			FS_STATUS_OP_ATTRIB: begin
				end;
			FS_STATUS_OP_MKDIR: begin
				end;
			FS_STATUS_OP_EXEC: begin
				end;
			FS_STATUS_OP_CALCSIZE: begin
				end;
			FS_STATUS_OP_SEARCH: begin
				end;
			FS_STATUS_OP_SEARCH_TEXT: begin
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
var
	debug: WideString;
Begin
	PluginNum := PluginNr;
	MyProgressProc := pProgressProc;
	MyLogProc := pLogProc;
	MyRequestProc := pRequestProc;
	CurrentLogon := false;
	// Вход в плагин.

	// Cloud := TCloudMailRu.Create('mds_free', 'mail.ru', 'd;jgedst,kbe;f');
	// if (Cloud.login() and Cloud.getToken(debug)) then Result := 0
	// else Result := -1;
	Result := 0;
end;

function FsFindFirst(path: PAnsiChar; var FindData: tWIN32FINDDATAA): thandle; stdcall;
begin
end;

function FsFindNext(Hdl: thandle; var FindData: tWIN32FINDDATAA): bool; stdcall;
begin
end;

function Implode(S: TStringList; Delimiter: Char): WideString;
var
	iCount: integer;
begin
	Result := '';
	if (S.Count = 0) then exit;
	for iCount := 0 to pred(S.Count) do Result := Result + S.Strings[iCount] + Delimiter;
	System.Delete(Result, Length(Result), 1);
end;

function ExtractRealPath(VirtualPath: WideString): TRealPath;
var
	List: TStringList;
begin
	List := TStringList.Create;
	ExtractStrings(['\'], [], PWideChar(VirtualPath), List);
	ExtractRealPath.account := List.Strings[0];
	List.Delete(0);

	ExtractRealPath.path := Implode(List, '\');
	List.Free;
end;

function FsFindFirstW(path: PWideChar; var FindData: tWIN32FINDDATAW): thandle; stdcall;
var
	Sections: TStringList;
	RealPath: TRealPath;
	user, domain, password: WideString;
	debug: WideString;
begin
	// Получение первого файла в папке. Result тоталом не используется (можно использовать для работы плагина).
	// setlasterror(ERROR_NO_MORE_FILES);
	GlobalPath := path;
	if path = '\' then begin
		if Assigned(Cloud) then FreeAndNil(Cloud);

		Sections := TStringList.Create;
		PluginIniFile.ReadSections(Sections);
		if (Sections.Count > 0) then begin
			strpcopy(FindData.cFileName, Sections.Strings[0]);
			FindData.ftCreationTime.dwLowDateTime := 0;
			FindData.ftCreationTime.dwHighDateTime := 0;
			FindData.nFileSizeHigh := 0;
			FindData.nFileSizeLow := 0;
			FindData.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
			FileCounter := 1;
			exit;
		end
		else begin
			setlasterror(ERROR_NO_MORE_FILES);
			Result := INVALID_HANDLE_VALUE;
			exit;
		end;

	end
	else begin
		RealPath := ExtractRealPath(GlobalPath);
		user := PluginIniFile.ReadString(RealPath.account, 'user', '');
		domain := PluginIniFile.ReadString(RealPath.account, 'domain', '');
		password := PluginIniFile.ReadString(RealPath.account, 'password', '');
		// todo проверка на пустые данные
		if not Assigned(Cloud) then begin
			Cloud := TCloudMailRu.Create(user, domain, password,MyProgressProc,PluginNum);
			if (Cloud.login() and Cloud.getToken(debug)) then begin
				CurrentLogon := true;
			end
			else begin
				CurrentLogon := false;
				Cloud.Free;
			end;

		end;

		if CurrentLogon then begin
			Cloud.getDir(RealPath.path, CurrentListing);

			if Length(CurrentListing) = 0 then begin
				// setlasterror(ERROR_NO_MORE_FILES);
				Result := 0;
				exit;

			end;
			// Todo Function ListingToFindData
			if (CurrentListing[0].type_ = TYPE_DIR) then FindData.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY
			else FindData.dwFileAttributes := 0;
			if (CurrentListing[0].size > MAXDWORD) then FindData.nFileSizeHigh := CurrentListing[0].size div MAXDWORD
			else FindData.nFileSizeHigh := 0;
			FindData.nFileSizeLow := CurrentListing[0].size;
			FindData.ftCreationTime := DateTimeToFileTime(UnixToDateTime(CurrentListing[0].mtime)); // todo optimization
			FindData.ftLastWriteTime := DateTimeToFileTime(UnixToDateTime(CurrentListing[0].mtime));
			strpcopy(FindData.cFileName, CurrentListing[0].name);
			FileCounter := 1;
			Result := 1;
		end
		else begin
			strpcopy(FindData.cFileName, 'Ошибка входа по указанным данным');
		end;
	end;
end;

function FsFindNextW(Hdl: thandle; var FindData: tWIN32FINDDATAW): bool; stdcall;
var
	Sections: TStringList;
	RealPath: TRealPath;
begin
	if GlobalPath = '\' then begin
		Sections := TStringList.Create;
		PluginIniFile.ReadSections(Sections);
		if (Sections.Count > FileCounter) then begin
			strpcopy(FindData.cFileName, Sections.Strings[FileCounter]);
			FindData.ftCreationTime.dwLowDateTime := 0;
			FindData.ftCreationTime.dwHighDateTime := 0;
			FindData.nFileSizeHigh := 0;
			FindData.nFileSizeLow := 0;
			FindData.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
			inc(FileCounter);
			Result := true;
		end
		else Result := false;
	end
	else begin
		if not CurrentLogon then begin
			Result := false;
		end
		else begin
			// Получение последующих файлов в папке (вызывается до тех пор, пока не вернёт false).
			if (Length(CurrentListing) > FileCounter) then begin
				if (CurrentListing[FileCounter].type_ = TYPE_DIR) then FindData.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY
				else FindData.dwFileAttributes := 0;
				if (CurrentListing[FileCounter].size > MAXDWORD) then FindData.nFileSizeHigh := CurrentListing[FileCounter].size div MAXDWORD
				else FindData.nFileSizeHigh := 0;

				FindData.nFileSizeLow := CurrentListing[FileCounter].size;
				FindData.ftCreationTime := DateTimeToFileTime(UnixToDateTime(CurrentListing[FileCounter].mtime)); // todo optimization
				FindData.ftLastWriteTime := DateTimeToFileTime(UnixToDateTime(CurrentListing[FileCounter].mtime));
				strpcopy(FindData.cFileName, CurrentListing[FileCounter].name);
				Result := true;
				inc(FileCounter);
			end
			else begin
				FillChar(FindData, sizeof(WIN32_FIND_DATA), 0);
				FileCounter := 0;
				Result := false;
			end;
		end;
	end;
end;

function FsFindClose(Hdl: thandle): integer; stdcall;
Begin
	// Завершение получения списка файлов. Result тоталом не используется (всегда равен 0)
	Result := 0;
	FileCounter := 0;
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
	if Verb = 'open' then begin
		if extractfileext(lowercase(RemoteName)) = '.txt' then begin
			WinExec('notepad.exe', 1);
		end;
	end
	else if Verb = 'properties' then begin
		messagebox(MainWin, PWideChar(RemoteName), PWideChar(Verb), mb_ok + mb_iconinformation);
	end
	else if copy(Verb, 1, 5) = 'chmod' then begin
	end
	else if copy(Verb, 1, 5) = 'quote' then begin
	end;
End;

function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall;
begin

	// Копирование файла из файловой системы плагина
end;

function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall;
var
	RealPath: TRealPath;
begin
	RealPath := ExtractRealPath(RemoteName);
	// ProgressProc (PluginNum,pchar(InFileName),pchar(OutFilename),round(((SizeDone/SizeFile)*100)))
		MyProgressProc(PluginNum, LocalName, RemoteName, 0);
	Cloud.getFile(WideString(RealPath.path), WideString(LocalName),MyProgressProc);
	Result := FS_FILE_OK;
	MyProgressProc(PluginNum, LocalName, RemoteName, 100);
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

exports FsGetDefRootName, FsInit, FsInitW, FsFindFirst, FsFindFirstW, FsFindNext, FsFindNextW, FsFindClose, FsGetFile, FsGetFileW;

(* ,
	FsExecuteFile,
	FsGetFile,
	FsPutFile,
	FsDeleteFile,
	FsStatusInfo,
	; *)
// FsExtractCustomIcon, {В примере отсутствует - при помощи этой функции можно устанавливать свои иконки на отображаемый файл}
begin

	GetMem(tmp, max_path);
	GetModuleFilename(hInstance, tmp, max_path);
	PluginPath := tmp;
	freemem(tmp);
	PluginPath := IncludeTrailingbackslash(ExtractFilePath(PluginPath));
	tempString := PluginPath + 'MailRuCloud.ini';
	if not fileexists(tempString) then FileClose(FileCreate(tempString));
	PluginIniFile := TIniFile.Create(tempString);

end.
