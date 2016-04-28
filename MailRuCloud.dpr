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
	CloudMailRu in 'CloudMailRu.pas',
	MRC_Helper in 'MRC_Helper.pas';

{$IFDEF WIN64}
{$E wfx64}
{$ENDIF}
{$IFDEF WIN32}
{$E wfx}
{$ENDIF}
{$R *.res}

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

procedure FsGetDefRootName(DefRootName: PAnsiChar; maxlen: integer); stdcall; // Процедура вызывается один раз при установке плагина
Begin
	StrLCopy(DefRootName, PAnsiChar('Cloud'), maxlen);
	messagebox(FindTCWindow, PWideChar('Installation succeful'), 'Information', mb_ok + mb_iconinformation);
End;

function FsFindClose(Hdl: thandle): integer; stdcall;
Begin
	// Завершение получения списка файлов. Result тоталом не используется (всегда равен 0)
	Result := 0;
	FileCounter := 0;
end;

{ ANSI PEASANTS }

function FsInit(PluginNr: integer; pProgressProc: TProgressProc; pLogProc: TLogProc; pRequestProc: TRequestProc): integer; stdcall;
Begin
	PluginNum := PluginNr;
	MyProgressProc := pProgressProc;
	MyLogProc := pLogProc;
	MyRequestProc := pRequestProc;
	// Вход в плагин.
	Result := 0;
end;

procedure FsStatusInfo(RemoteDir: PAnsiChar; InfoStartEnd, InfoOperation: integer); stdcall;
begin
	SetLastError(ERROR_NOT_SUPPORTED);
end;

function FsFindFirst(path: PAnsiChar; var FindData: tWIN32FINDDATAA): thandle; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := ERROR_INVALID_HANDLE; // Ansi-заглушка
end;

function FsFindNext(Hdl: thandle; var FindData: tWIN32FINDDATAA): bool; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := false; // Ansi-заглушка
end;

function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall; // Копирование файла из файловой системы плагина
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := FS_FILE_NOTSUPPORTED; // Ansi-заглушка
end;

function FsPutFile(LocalName, RemoteName: PAnsiChar; CopyFlags: integer): integer; stdcall; // Копирование файла в файловую систему плагина
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := FS_FILE_NOTSUPPORTED; // Ansi-заглушка
end;

function FsDeleteFile(RemoteName: PAnsiChar): bool; stdcall; // Удаление файла из файловой ссистемы плагина
Begin
	SetLastError(ERROR_INVALID_FUNCTION); // Ansi-заглушка
	Result := false;
End;

function FsDisconnect(DisconnectRoot: PAnsiChar): bool; stdcall;
begin
	SetLastError(ERROR_INVALID_FUNCTION);
	Result := false; // ansi-заглушка
end;
{ GLORIOUS UNICODE MASTER RACE }

function FsInitW(PluginNr: integer; pProgressProc: TProgressProc; pLogProc: TLogProc; pRequestProc: TRequestProc): integer; stdcall; // Вход в плагин.
Begin
	PluginNum := PluginNr;
	MyProgressProc := pProgressProc;
	MyLogProc := pLogProc;
	MyRequestProc := pRequestProc;
	CurrentLogon := false;
	Result := 0;
end;

procedure FsStatusInfoW(RemoteDir: PWideChar; InfoStartEnd, InfoOperation: integer); stdcall;
begin
	if Assigned(Cloud) then Cloud.CancelCopy := false; // todo: временно сделал
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

function FsFindFirstW(path: PWideChar; var FindData: tWIN32FINDDATAW): thandle; stdcall;
var
	Sections: TStringList;
	RealPath: TRealPath;
	user, domain, password: WideString;
begin
	// Получение первого файла в папке. Result тоталом не используется (можно использовать для работы плагина).
	// setlasterror(ERROR_NO_MORE_FILES);
	GlobalPath := path;
	if path = '\' then
	begin
		if Assigned(Cloud) then FreeAndNil(Cloud);

		Sections := TStringList.Create;
		PluginIniFile.ReadSections(Sections);
		if (Sections.Count > 0) then
		begin
			strpcopy(FindData.cFileName, Sections.Strings[0]);
			FindData.ftCreationTime.dwLowDateTime := 0;
			FindData.ftCreationTime.dwHighDateTime := 0;
			FindData.nFileSizeHigh := 0;
			FindData.nFileSizeLow := 0;
			FindData.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
			FileCounter := 1;
			exit(0);
		end else begin
			SetLastError(ERROR_NO_MORE_FILES);
			exit(INVALID_HANDLE_VALUE);
		end;

	end else begin
		RealPath := ExtractRealPath(GlobalPath);
		if not Assigned(Cloud) then
		begin
			user := PluginIniFile.ReadString(RealPath.account, 'user', '');
			domain := PluginIniFile.ReadString(RealPath.account, 'domain', '');
			password := PluginIniFile.ReadString(RealPath.account, 'password', '');
			// todo проверка на пустые данные
			MyLogProc(PluginNum, MSGTYPE_CONNECT, PWideChar('CONNECT ' + user + '@' + domain));
			Cloud := TCloudMailRu.Create(user, domain, password, MyProgressProc, PluginNum, MyLogProc);
			if Cloud.login() then
			begin
				CurrentLogon := true;
			end else begin
				CurrentLogon := false;
				FreeAndNil(Cloud);
				SetLastError(ERROR_NETWORK_ACCESS_DENIED);
				exit(INVALID_HANDLE_VALUE);
			end;

		end;

		if CurrentLogon then
		begin
			if not Cloud.getDir(RealPath.path, CurrentListing) then
			begin
				SetLastError(ERROR_PATH_NOT_FOUND);
				exit(INVALID_HANDLE_VALUE);
			end;

			if Length(CurrentListing) = 0 then
			begin
				// setlasterror(ERROR_NO_MORE_FILES);
				exit(0);

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
		end else begin
			SetLastError(ERROR_INVALID_HANDLE);
			Result := INVALID_HANDLE_VALUE;
			strpcopy(FindData.cFileName, 'Ошибка входа по указанным данным'); // Сюда никогда не должны попасть
		end;
	end;
end;

function FsFindNextW(Hdl: thandle; var FindData: tWIN32FINDDATAW): bool; stdcall;
var
	Sections: TStringList;
begin
	if GlobalPath = '\' then
	begin
		Sections := TStringList.Create;
		PluginIniFile.ReadSections(Sections);
		if (Sections.Count > FileCounter) then
		begin
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
	end else begin
		if not CurrentLogon then
		begin
			Result := false;
		end else begin
			// Получение последующих файлов в папке (вызывается до тех пор, пока не вернёт false).
			if (Length(CurrentListing) > FileCounter) then
			begin
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
			end else begin
				FillChar(FindData, sizeof(WIN32_FIND_DATA), 0);
				FileCounter := 0;
				Result := false;
			end;
		end;
	end;
end;

function FsExecuteFileW(MainWin: thandle; RemoteName, Verb: PWideChar): integer; stdcall;
Begin
	// Запуск файла
	Result := FS_EXEC_OK;
	if Verb = 'open' then
	begin

	end else if Verb = 'properties' then
	begin
		messagebox(MainWin, PWideChar(RemoteName), PWideChar(Verb), mb_ok + mb_iconinformation);
	end else if copy(Verb, 1, 5) = 'chmod' then
	begin
	end else if copy(Verb, 1, 5) = 'quote' then
	begin
	end;
End;

function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall; // Копирование файла из файловой системы плагина
var
	RealPath: TRealPath;
begin
	Result := FS_FILE_NOTSUPPORTED;
	RealPath := ExtractRealPath(RemoteName);

	MyProgressProc(PluginNum, LocalName, RemoteName, 0);

	if CopyFlags = FS_FILE_OK then
	begin
		if FileExists(LocalName) then
		begin
			exit(FS_FILE_EXISTS);
		end else begin
			Result := Cloud.getFile(WideString(RealPath.path), WideString(LocalName));
		end;
	end;

	if CheckFlag(FS_COPYFLAGS_MOVE, CopyFlags) then
	begin
		Result := FS_FILE_NOTSUPPORTED; // todo
	end;
	if CheckFlag(FS_COPYFLAGS_RESUME, CopyFlags) then
	begin { NEVER CALLED HERE }
		Result := FS_FILE_NOTSUPPORTED;
	end;
	if CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags) then
	begin
		Result := Cloud.getFile(WideString(RealPath.path), WideString(LocalName));
	end;
	if Result = FS_FILE_OK then
	begin
		MyProgressProc(PluginNum, LocalName, RemoteName, 100);
		MyLogProc(PluginNum, MSGTYPE_TRANSFERCOMPLETE, PWideChar(RemoteName + '->' + LocalName));
	end;

end;

function FsPutFileW(LocalName, RemoteName: PWideChar; CopyFlags: integer): integer; stdcall;
var
	RealPath: TRealPath;
begin
	RealPath := ExtractRealPath(RemoteName);
	MyProgressProc(PluginNum, LocalName, PWideChar(RealPath.path), 0);
	if CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags) then
	begin
		if Cloud.deleteFile(RealPath.path) then
		begin
			Result := Cloud.putFile(WideString(LocalName), RealPath.path);
			if Result = FS_FILE_OK then
			begin
				MyProgressProc(PluginNum, LocalName, PWideChar(RealPath.path), 100);
				MyLogProc(PluginNum, MSGTYPE_TRANSFERCOMPLETE, PWideChar(LocalName + '->' + RemoteName));
			end;

		end else begin
			Result := FS_FILE_NOTSUPPORTED;

		end;

	end;
	if CheckFlag(FS_COPYFLAGS_RESUME, CopyFlags) then
	begin // NOT SUPPORTED
		exit(FS_FILE_NOTSUPPORTED);
	end;

	if CheckFlag(FS_COPYFLAGS_EXISTS_SAMECASE, CopyFlags) or CheckFlag(FS_COPYFLAGS_EXISTS_DIFFERENTCASE, CopyFlags) then // Облако не поддерживает разные регистры
	begin
		exit(FS_FILE_EXISTS);
	end;
	if CheckFlag(FS_COPYFLAGS_MOVE, CopyFlags) then
	begin
		Result := Cloud.putFile(WideString(LocalName), RealPath.path);
		if Result = FS_FILE_OK then
		begin
			MyProgressProc(PluginNum, LocalName, PWideChar(RealPath.path), 100);
			MyLogProc(PluginNum, MSGTYPE_TRANSFERCOMPLETE, PWideChar(LocalName + '->' + RemoteName));
		end;
		if not DeleteFileW(LocalName) then
		begin //Не получилось удалить
			exit(FS_FILE_NOTSUPPORTED);
		end;
	end;

	if CopyFlags = 0 then
	begin
		Result := Cloud.putFile(WideString(LocalName), RealPath.path);
		if Result = FS_FILE_OK then
		begin
			MyProgressProc(PluginNum, LocalName, PWideChar(RealPath.path), 100);
			MyLogProc(PluginNum, MSGTYPE_TRANSFERCOMPLETE, PWideChar(LocalName + '->' + RemoteName));
		end;
	end;

end;

function FsDeleteFileW(RemoteName: PWideChar): bool; stdcall; // Удаление файла из файловой ссистемы плагина
var
	RealPath: TRealPath;
Begin
	RealPath := ExtractRealPath(WideString(RemoteName));
	Result := Cloud.deleteFile(RealPath.path);
End;

function FsDisconnectW(DisconnectRoot: PWideChar): bool; stdcall;
begin
	if Assigned(Cloud) then FreeAndNil(Cloud);
	Result := true;
end;

exports FsGetDefRootName, FsInit, FsInitW, FsFindFirst, FsFindFirstW, FsFindNext, FsFindNextW, FsFindClose, FsGetFile, FsGetFileW,
	FsDisconnect, FsDisconnectW, FsStatusInfo, FsStatusInfoW, FsPutFile, FsPutFileW, FsDeleteFile, FsDeleteFileW;

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
	if not FileExists(tempString) then FileClose(FileCreate(tempString));
	PluginIniFile := TIniFile.Create(tempString);

end.
