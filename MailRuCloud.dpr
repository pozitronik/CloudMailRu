library MailRuCloud;

{TODO : Refactore: decrease cyclomatic complexity to 15 max}
{TODO: отрицательные значения должны вводиться корректно}
//todo: повторы операций не должны приводить к переполнению стека
{TODO корректно обрабатывать отмену операции при зацикливании}

{$R *.dres}

uses
	SysUtils, System.Generics.Collections, DateUtils, windows, Classes, PLUGIN_TYPES, IdSSLOpenSSLHeaders, messages, inifiles, Vcl.controls, CloudMailRu in 'CloudMailRu.pas', MRC_Helper in 'MRC_Helper.pas', Accounts in 'Accounts.pas'{AccountsForm}, RemoteProperty in 'RemoteProperty.pas'{PropertyForm}, Descriptions in 'Descriptions.pas', ConnectionManager in 'ConnectionManager.pas', Settings in 'Settings.pas', ANSIFunctions in 'ANSIFunctions.pas';

{$IFDEF WIN64}
{$E wfx64}
{$ENDIF}
{$IFDEF WIN32}
{$E wfx}
{$ENDIF}
{$R *.res}

const
{$IFDEF WIN64}
	PlatformDllPath = 'x64';
{$ENDIF}
{$IFDEF WIN32}
	PlatformDllPath = 'x32';
{$ENDIF}

var
	//PlatformDllPath: WideString;
	tmp: pWideChar;
	AccountsIniFilePath: WideString;
	SettingsIniFilePath: WideString;
	GlobalPath, PluginPath, AppDataDir, IniDir: WideString;
	FileCounter: integer = 0;
	ThreadSkipListDelete: TDictionary<DWORD, Bool>; //Массив id потоков, для которых операции получения листинга должны быть пропущены (при удалении)
	ThreadSkipListRenMov: TDictionary<DWORD, Bool>; //Массив id потоков, для которых операции получения листинга должны быть пропущены (при копировании/перемещении)
	ThreadRetryCountDownload: TDictionary<DWORD, Int32>; //массив [id потока => количество попыток] для подсчёта количества повторов скачивания файла
	ThreadRetryCountUpload: TDictionary<DWORD, Int32>; //массив [id потока => количество попыток] для подсчёта количества повторов закачивания файла
	{Callback data}
	PluginNum: integer;
	CryptoNum: integer;
	MyProgressProc: TProgressProcW;
	MyLogProc: TLogProcW;
	MyRequestProc: TRequestProcW;
	MyCryptProc: TCryptProcW;

	CurrentListing: TCloudMailRuDirListing;
	ConnectionManager: TConnectionManager;
	CurrentDescriptions: TDescription;
	ProxySettings: TProxySettings;

function CloudMailRuDirListingItemToFindData(DirListing: TCloudMailRuDirListingItem): tWIN32FINDDATAW;
begin
	if (DirListing.type_ = TYPE_DIR) then
	begin
		Result.ftCreationTime.dwLowDateTime := 0;
		Result.ftCreationTime.dwHighDateTime := 0;
		Result.ftLastWriteTime.dwHighDateTime := 0;
		Result.ftLastWriteTime.dwLowDateTime := 0;
		Result.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY
	end else begin
		Result.ftCreationTime := DateTimeToFileTime(UnixToDateTime(DirListing.mtime));
		Result.ftLastWriteTime := DateTimeToFileTime(UnixToDateTime(DirListing.mtime));

		Result.dwFileAttributes := 0;
	end;

	if (DirListing.size > MAXDWORD) then Result.nFileSizeHigh := DirListing.size div MAXDWORD
	else Result.nFileSizeHigh := 0;
	Result.nFileSizeLow := DirListing.size;

	strpcopy(Result.cFileName, DirListing.name);
end;

function FindData_emptyDir(DirName: WideString = '.'): tWIN32FINDDATAW;
begin
	strpcopy(Result.cFileName, DirName);
	Result.ftCreationTime.dwLowDateTime := 0;
	Result.ftCreationTime.dwHighDateTime := 0;
	Result.ftLastWriteTime.dwHighDateTime := 0;
	Result.ftLastWriteTime.dwLowDateTime := 0;
	Result.nFileSizeHigh := 0;
	Result.nFileSizeLow := 0;
	Result.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
end;

function FindListingItemByHomePath(DirListing: TCloudMailRuDirListing; HomePath: WideString): TCloudMailRuDirListingItem;
var
	I: integer;
begin
	HomePath := '/' + StringReplace(HomePath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]);
	for I := 0 to Length(DirListing) - 1 do
	begin
		if DirListing[I].home = HomePath then exit(DirListing[I]);
	end;
end;

function FindListingItemByName(DirListing: TCloudMailRuDirListing; ItemName: WideString): TCloudMailRuDirListingItem;
var
	I: integer;
begin
	for I := 0 to Length(DirListing) - 1 do
	begin
		if DirListing[I].name = ItemName then
		begin
			exit(DirListing[I]);
		end;
	end;
end;

function GetListingItemByName(CurrentListing: TCloudMailRuDirListing; path: TRealPath): TCloudMailRuDirListingItem;
var
	getResult: integer;
begin
	Result := FindListingItemByHomePath(CurrentListing, path.path); //сначала попробуем найти поле в имеющемся списке
	if Result.name = '' then //если там его нет (нажали пробел на папке, например), то запросим в облаке напрямую
	begin
		if ConnectionManager.get(path.account, getResult).statusFile(path.path, Result) then
		begin
			if Result.home = '' then MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Cant find file ' + path.path)); {Такого быть не может, но...}
		end;
	end; //Не рапортуем, это будет уровнем выше
end;

function DeleteLocalFile(LocalName: WideString): integer;
var
	UNCLocalName: WideString;
	DeleteFailOnUploadMode, DeleteFailOnUploadModeAsked: integer;
begin
	Result := FS_FILE_OK;
	DeleteFailOnUploadModeAsked := IDRETRY;
	UNCLocalName := GetUNCFilePath(LocalName);

	while (not DeleteFileW(pWideChar(UNCLocalName))) and (DeleteFailOnUploadModeAsked = IDRETRY) do //todo proc
	begin
		DeleteFailOnUploadMode := GetPluginSettings(SettingsIniFilePath).DeleteFailOnUploadMode;
		if DeleteFailOnUploadMode = DeleteFailOnUploadAsk then
		begin
			DeleteFailOnUploadModeAsked := messagebox(FindTCWindow, pWideChar('Can''t delete file ' + LocalName + '. Continue operation?'), 'File deletion error', MB_ABORTRETRYIGNORE + MB_ICONQUESTION);
			case DeleteFailOnUploadModeAsked of
				IDRETRY: continue;
				IDABORT: DeleteFailOnUploadMode := DeleteFailOnUploadAbort;
				IDIGNORE: DeleteFailOnUploadMode := DeleteFailOnUploadIgnore;
			end;
		end;

		case DeleteFailOnUploadMode of
			DeleteFailOnUploadAbort:
				begin
					MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Can''t delete file ' + LocalName + ', aborted'));
					exit(FS_FILE_NOTSUPPORTED);
				end;
			DeleteFailOnUploadDeleteIgnore, DeleteFailOnUploadDeleteAbort:
				begin
					//check if file just have RO attr, then remove it. If user has lack of rights, then ignore or abort
					if ((FileGetAttr(UNCLocalName) or faReadOnly) <> 0) and ((FileSetAttr(UNCLocalName, not faReadOnly) = 0) and (DeleteFileW(pWideChar(UNCLocalName)))) then
					begin
						MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Read only file ' + LocalName + ' deleted'));
						exit(FS_FILE_OK);
					end else begin
						if GetPluginSettings(SettingsIniFilePath).DeleteFailOnUploadMode = DeleteFailOnUploadDeleteIgnore then
						begin
							MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Can''t delete file ' + LocalName + ', ignored'));
							exit(FS_FILE_OK);
						end else begin
							MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Can''t delete file ' + LocalName + ', aborted'));
							exit(FS_FILE_NOTSUPPORTED);
						end;
					end;
				end;
			else
				begin
					MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Can''t delete file ' + LocalName + ', ignored'));
				end;
		end;
	end;
end;

function FsGetBackgroundFlags: integer; stdcall;
begin
	if GetPluginSettings(SettingsIniFilePath).DisableMultiThreading then Result := 0
	else Result := BG_DOWNLOAD + BG_UPLOAD; //+ BG_ASK_USER;
end;

function FsInit(PluginNr: integer; pProgressProc: TProgressProc; pLogProc: TLogProc; pRequestProc: TRequestProc): integer; stdcall;
Begin
	Result := 0;
end;

{GLORIOUS UNICODE MASTER RACE}

function FsInitW(PluginNr: integer; pProgressProc: TProgressProcW; pLogProc: TLogProcW; pRequestProc: TRequestProcW): integer; stdcall; //Вход в плагин.
Begin
	PluginNum := PluginNr;
	MyProgressProc := pProgressProc;
	MyLogProc := pLogProc;
	MyRequestProc := pRequestProc;
	Result := 0;
	CurrentDescriptions := TDescription.Create;
end;

procedure FsStatusInfoW(RemoteDir: pWideChar; InfoStartEnd, InfoOperation: integer); stdcall; //Начало и конец операций FS
var
	RealPath: TRealPath;
	getResult: integer;
	//DescriptionItem: TCloudMailRuDirListingItem;
	TmpIon: WideString;
begin
	RealPath := ExtractRealPath(RemoteDir);
	if RealPath.account = '' then RealPath.account := ExtractFileName(ExcludeTrailingBackslash(RemoteDir));
	if (InfoStartEnd = FS_STATUS_START) then
	begin
		case InfoOperation of
			FS_STATUS_OP_LIST:
				begin
					if (GetPluginSettings(SettingsIniFilePath).DescriptionEnabled) and (RealPath.account <> '') then
					begin
						TmpIon := GetTmpFileName('ion');
						if ConnectionManager.get(RealPath.account, getResult).getDescriptionFile(IncludeTrailingBackslash(RealPath.path) + 'descript.ion', TmpIon) = FS_FILE_OK then
						begin
							CurrentDescriptions.Read(TmpIon);
						end else begin
							CurrentDescriptions.Clear;
						end;
					end;
				end;
			FS_STATUS_OP_GET_SINGLE:
				begin
					ThreadRetryCountDownload.AddOrSetValue(GetCurrentThreadID(), 0);
				end;
			FS_STATUS_OP_GET_MULTI:
				begin
					ThreadRetryCountDownload.AddOrSetValue(GetCurrentThreadID(), 0);
				end;
			FS_STATUS_OP_PUT_SINGLE:
				begin
					ThreadRetryCountUpload.AddOrSetValue(GetCurrentThreadID(), 0);
				end;
			FS_STATUS_OP_PUT_MULTI:
				begin
					ThreadRetryCountUpload.AddOrSetValue(GetCurrentThreadID(), 0);
				end;
			FS_STATUS_OP_RENMOV_SINGLE:
				begin
				end;
			FS_STATUS_OP_RENMOV_MULTI:
				begin
					if ConnectionManager.get(RealPath.account, getResult).isPublicShare then
					begin
						MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Direct copying from public accounts not supported'));
						ThreadSkipListRenMov.AddOrSetValue(GetCurrentThreadID, true);
					end;

				end;
			FS_STATUS_OP_DELETE:
				begin
					//ThreadSkipListDelete.Add(GetCurrentThreadID());
					ThreadSkipListDelete.AddOrSetValue(GetCurrentThreadID, true);
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
			FS_STATUS_OP_SYNC_SEARCH:
				begin
				end;
			FS_STATUS_OP_SYNC_GET:
				begin
				end;
			FS_STATUS_OP_SYNC_PUT:
				begin
				end;
			FS_STATUS_OP_SYNC_DELETE:
				begin
				end;
			FS_STATUS_OP_GET_MULTI_THREAD:
				begin
					ThreadRetryCountDownload.AddOrSetValue(GetCurrentThreadID(), 0);
				end;
			FS_STATUS_OP_PUT_MULTI_THREAD:
				begin
					ThreadRetryCountUpload.AddOrSetValue(GetCurrentThreadID(), 0);
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
					if (RealPath.account <> '') and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_PUT_MULTI:
				begin
					if (RealPath.account <> '') and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_RENMOV_SINGLE:
				begin
					if (RealPath.account <> '') and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_RENMOV_MULTI:
				begin
					ThreadSkipListRenMov.AddOrSetValue(GetCurrentThreadID, false);
					if (RealPath.account <> '') and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_DELETE:
				begin
					ThreadSkipListDelete.AddOrSetValue(GetCurrentThreadID(), false);
					if (RealPath.account <> '') and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
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
			FS_STATUS_OP_SYNC_SEARCH:
				begin
				end;
			FS_STATUS_OP_SYNC_GET:
				begin
					if (RealPath.account <> '') and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_SYNC_PUT:
				begin
					if (RealPath.account <> '') and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_SYNC_DELETE:
				begin
					if (RealPath.account <> '') and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_GET_MULTI_THREAD:
				begin
					if (RealPath.account <> '') and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_PUT_MULTI_THREAD:
				begin
					if (RealPath.account <> '') and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
		end;
		exit;
	end;
end;

function FsFindFirstW(path: pWideChar; var FindData: tWIN32FINDDATAW): THandle; stdcall;
var //Получение первого файла в папке. Result тоталом не используется (можно использовать для работы плагина).
	Sections: TStringList;
	RealPath: TRealPath;
	getResult: integer;
	SkipListDelete, SkipListRenMov: Bool;
begin
	ThreadSkipListDelete.TryGetValue(GetCurrentThreadID(), SkipListDelete);
	ThreadSkipListRenMov.TryGetValue(GetCurrentThreadID(), SkipListRenMov);
	if SkipListDelete or SkipListRenMov then
	begin
		SetLastError(ERROR_NO_MORE_FILES);
		exit(INVALID_HANDLE_VALUE);
	end;

	SetLength(CurrentListing, 0);
	Result := 0;
	GlobalPath := path;
	if GlobalPath = '\' then
	begin //список соединений
		Sections := TStringList.Create;
		GetAccountsListFromIniFile(AccountsIniFilePath, Sections);

		if (Sections.Count > 0) then
		begin
			FindData := FindData_emptyDir(Sections.Strings[0]);
			FileCounter := 1;
		end else begin
			Result := INVALID_HANDLE_VALUE; //Нельзя использовать exit
			SetLastError(ERROR_NO_MORE_FILES);
		end;
		Sections.Free;
	end else begin
		RealPath := ExtractRealPath(GlobalPath);

		if RealPath.account = '' then RealPath.account := ExtractFileName(ExcludeTrailingBackslash(GlobalPath));

		if not ConnectionManager.get(RealPath.account, getResult).getDirListing(RealPath.path, CurrentListing) then SetLastError(ERROR_PATH_NOT_FOUND);
		if getResult <> CLOUD_OPERATION_OK then
		begin
			SetLastError(ERROR_ACCESS_DENIED);
			exit(INVALID_HANDLE_VALUE);
		end;

		if (Length(CurrentListing) = 0) then
		begin
			FindData := FindData_emptyDir(); //воркароунд бага с невозможностью входа в пустой каталог, см. http://www.ghisler.ch/board/viewtopic.php?t=42399
			Result := 0;
			SetLastError(ERROR_NO_MORE_FILES);
		end else begin
			FindData := CloudMailRuDirListingItemToFindData(CurrentListing[0]);
			FileCounter := 1;
			Result := 1;
		end;
	end;
end;

function FsFindNextW(Hdl: THandle; var FindData: tWIN32FINDDATAW): Bool; stdcall;
var
	Sections: TStringList;
begin
	if GlobalPath = '\' then
	begin
		Sections := TStringList.Create;
		GetAccountsListFromIniFile(AccountsIniFilePath, Sections);
		if (Sections.Count > FileCounter) then
		begin
			FindData := FindData_emptyDir(Sections.Strings[FileCounter]);
			inc(FileCounter);
			Result := true;
		end
		else Result := false;
		Sections.Free;
	end else begin
		//Получение последующих файлов в папке (вызывается до тех пор, пока не вернёт false).
		if (Length(CurrentListing) > FileCounter) then
		begin
			FindData := CloudMailRuDirListingItemToFindData(CurrentListing[FileCounter]);
			Result := true;
			inc(FileCounter);
		end else begin
			FillChar(FindData, SizeOf(WIN32_FIND_DATA), 0);
			FileCounter := 0;
			Result := false;
		end;
	end;
end;

function FsFindClose(Hdl: THandle): integer; stdcall;
Begin //Завершение получения списка файлов. Result тоталом не используется (всегда равен 0)
	//SetLength(CurrentListing, 0); // Пусть будет
	Result := 0;
	FileCounter := 0;
end;

function FsExecuteFileW(MainWin: THandle; RemoteName, Verb: pWideChar): integer; stdcall; //Запуск файла
var
	RealPath: TRealPath;
	CurrentItem: TCloudMailRuDirListingItem;
	Cloud: TCloudMailRu;
	getResult: integer;
	command, param: WideString;
Begin
	RealPath := ExtractRealPath(RemoteName);
	Result := FS_EXEC_OK;
	if Verb = 'open' then
	begin
		exit(FS_EXEC_YOURSELF);
	end else if Verb = 'properties' then
	begin
		if RealPath.path = '' then
		begin
			TAccountsForm.ShowAccounts(MainWin, AccountsIniFilePath, SettingsIniFilePath, MyCryptProc, PluginNum, CryptoNum, RemoteName);
		end else begin
			if ConnectionManager.get(RealPath.account, getResult).statusFile(RealPath.path, CurrentItem) then
			begin
				Cloud := ConnectionManager.get(RealPath.account, getResult);
				if CurrentItem.home <> '' then TPropertyForm.ShowProperty(MainWin, CurrentItem, Cloud)
				else
				begin
					MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Cant find file under cursor!'));
				end;
			end; //Не рапортуем, это будет уровнем выше
		end;
	end else if copy(Verb, 1, 5) = 'chmod' then
	begin
	end else if copy(Verb, 1, 5) = 'quote' then
	begin //обработка внутренних команд плагина
		command := LowerCase(GetWord(Verb, 1));
		if command = 'rmdir' then
		begin
			RealPath := ExtractRealPath(RemoteName + GetWord(Verb, 2));
			if (ConnectionManager.get(RealPath.account, getResult).removeDir(RealPath.path) <> true) then Result := FS_EXEC_ERROR;
		end else if command = 'share' then
		begin
			RealPath := ExtractRealPath(RemoteName);
			param := ExtractLinkFromUrl(GetWord(Verb, 2));
			if not(ConnectionManager.get(RealPath.account, getResult).shareFolder(RealPath.path, param, CLOUD_SHARE_RW)) then Result := FS_EXEC_ERROR;
		end else if command = 'clone' then
		begin
			RealPath := ExtractRealPath(RemoteName);
			if RealPath.account = '' then //Некрасивое решение, надо переделать
			begin
				RealPath.account := ExtractFileName(ExcludeTrailingBackslash(RemoteName));
				RealPath.path := '\';
			end;
			param := ExtractLinkFromUrl(GetWord(Verb, 2));
			if (ConnectionManager.get(RealPath.account, getResult).cloneWeblink(RealPath.path, param) = CLOUD_OPERATION_OK) then
			begin
				if GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
			end
			else Result := FS_EXEC_ERROR;
		end;

	end;
End;

function GetRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: integer): integer;
var
	getResult: integer;
	Item: TCloudMailRuDirListingItem;
begin
	Result := ConnectionManager.get(RemotePath.account, getResult).getFile(WideString(RemotePath.path), LocalName);

	if Result = FS_FILE_OK then
	begin
		if GetPluginSettings(SettingsIniFilePath).PreserveFileTime then
		begin
			Item := GetListingItemByName(CurrentListing, RemotePath);
			if Item.mtime <> 0 then SetAllFileTime(ExpandUNCFileName(LocalName), DateTimeToFileTime(UnixToDateTime(Item.mtime)));
		end;
		if CheckFlag(FS_COPYFLAGS_MOVE, CopyFlags) then ConnectionManager.get(RemotePath.account, getResult).deleteFile(RemotePath.path);
		MyProgressProc(PluginNum, pWideChar(LocalName), pWideChar(RemoteName), 100);
		MyLogProc(PluginNum, MSGTYPE_TRANSFERCOMPLETE, pWideChar(RemoteName + '->' + LocalName));
	end;
end;

function FsGetFileW(RemoteName, LocalName: pWideChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall; //Копирование файла из файловой системы плагина
var
	RealPath: TRealPath;

	OverwriteLocalMode: integer;
	RetryAttempts: integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	If CheckFlag(FS_COPYFLAGS_RESUME, CopyFlags) then exit; {NEVER CALLED HERE}
	RealPath := ExtractRealPath(RemoteName);
	MyProgressProc(PluginNum, RemoteName, LocalName, 0);

	OverwriteLocalMode := GetPluginSettings(SettingsIniFilePath).OverwriteLocalMode;
	if (FileExists(LocalName) and not(CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags))) then
	begin
		case OverwriteLocalMode of
			OverwriteLocalModeAsk: exit(FS_FILE_EXISTS); //TC will ask user
			OverwriteLocalModeIgnore:
				begin
					MyLogProc(PluginNum, MSGTYPE_DETAILS, pWideChar('Local file ' + LocalName + ' exists, ignored'));
					exit(FS_FILE_OK);
				end;
			OverwriteLocalModeOverwrite: MyLogProc(PluginNum, MSGTYPE_DETAILS, pWideChar('Local file ' + LocalName + ' exists, and will be overwritten'));
		end;
	end;

	Result := GetRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);

	if Result in [FS_FILE_OK, FS_FILE_USERABORT] then exit;

	case GetPluginSettings(SettingsIniFilePath).OperationErrorMode of
		OperationErrorModeAsk:
			begin
				while (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					case (messagebox(FindTCWindow, pWideChar('Error downloading file' + sLineBreak + RemoteName + sLineBreak + 'Continue operation?'), 'Download error', MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
						ID_ABORT: Result := FS_FILE_USERABORT;
						ID_RETRY: Result := GetRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
						ID_IGNORE: break;
					end;
				end;

			end;
		OperationErrorModeIgnore: exit;
		OperationErrorModeAbort: exit(FS_FILE_USERABORT);
		OperationErrorModeRetry:
			begin;
				RetryAttempts := GetPluginSettings(SettingsIniFilePath).RetryAttempts;
				while (ThreadRetryCountDownload.Items[GetCurrentThreadID()] <> RetryAttempts) and (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					ThreadRetryCountDownload.Items[GetCurrentThreadID()] := ThreadRetryCountDownload.Items[GetCurrentThreadID()] + 1;
					MyLogProc(PluginNum, MSGTYPE_DETAILS, pWideChar('Error downloading file ' + RemoteName + ' Retry attempt ' + ThreadRetryCountDownload.Items[GetCurrentThreadID()].ToString + RetryAttemptsToString(RetryAttempts)));
					Result := GetRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
					ProcessMessages;
					if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then ThreadRetryCountDownload.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
				end;
			end;
	end;

end;

function PutRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: integer): integer;
var
	getResult: integer;
begin
	Result := ConnectionManager.get(RemotePath.account, getResult).putFile(WideString(LocalName), RemotePath.path);
	if Result = FS_FILE_OK then
	begin
		MyProgressProc(PluginNum, pWideChar(LocalName), pWideChar(RemotePath.path), 100);
		MyLogProc(PluginNum, MSGTYPE_TRANSFERCOMPLETE, pWideChar(LocalName + '->' + RemoteName));
		if CheckFlag(FS_COPYFLAGS_MOVE, CopyFlags) then Result := DeleteLocalFile(LocalName);
	end;

end;

function FsPutFileW(LocalName, RemoteName: pWideChar; CopyFlags: integer): integer; stdcall;
var
	RealPath: TRealPath;
	RetryAttempts: integer;
	getResult: integer;
begin
	//Result := FS_FILE_NOTSUPPORTED;
	RealPath := ExtractRealPath(RemoteName);
	if RealPath.account = '' then exit(FS_FILE_NOTSUPPORTED);
	MyProgressProc(PluginNum, LocalName, pWideChar(RealPath.path), 0);

	if CheckFlag(FS_COPYFLAGS_RESUME, CopyFlags) then exit(FS_FILE_NOTSUPPORTED); //NOT SUPPORTED

	if (CheckFlag(FS_COPYFLAGS_EXISTS_SAMECASE, CopyFlags) or CheckFlag(FS_COPYFLAGS_EXISTS_DIFFERENTCASE, CopyFlags)) and not(CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags)) then exit(FS_FILE_EXISTS); //Облако не поддерживает разные регистры

	if CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags) then
	begin
		if not(ConnectionManager.get(RealPath.account, getResult).deleteFile(RealPath.path)) then exit(FS_FILE_NOTSUPPORTED); //Неизвестно, как перезаписать файл черз API, но мы можем его удалить
	end;
	Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);

	if Result in [FS_FILE_OK, FS_FILE_USERABORT] then exit;

	case GetPluginSettings(SettingsIniFilePath).OperationErrorMode of
		OperationErrorModeAsk:
			begin
				while (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					case (messagebox(FindTCWindow, pWideChar('Error uploading file' + sLineBreak + LocalName + sLineBreak + 'Continue operation?'), 'Upload error', MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
						ID_ABORT: Result := FS_FILE_USERABORT;
						ID_RETRY: Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
						ID_IGNORE: break;
					end;
				end;

			end;
		OperationErrorModeIgnore: exit;
		OperationErrorModeAbort: exit(FS_FILE_USERABORT);
		OperationErrorModeRetry:
			begin;
				RetryAttempts := GetPluginSettings(SettingsIniFilePath).RetryAttempts;
				while (ThreadRetryCountUpload.Items[GetCurrentThreadID()] <> RetryAttempts) and (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					ThreadRetryCountUpload.Items[GetCurrentThreadID()] := ThreadRetryCountUpload.Items[GetCurrentThreadID()] + 1;
					MyLogProc(PluginNum, MSGTYPE_DETAILS, pWideChar('Error uploading file ' + LocalName + ' Retry attempt ' + ThreadRetryCountUpload.Items[GetCurrentThreadID()].ToString + RetryAttemptsToString(RetryAttempts)));
					Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
					ProcessMessages;
					if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then ThreadRetryCountUpload.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
				end;
			end;
	end;

end;

function FsDeleteFileW(RemoteName: pWideChar): Bool; stdcall; //Удаление файла из файловой ссистемы плагина
var
	RealPath: TRealPath;
	getResult: integer;
Begin
	RealPath := ExtractRealPath(WideString(RemoteName));
	if RealPath.account = '' then exit(false);
	Result := ConnectionManager.get(RealPath.account, getResult).deleteFile(RealPath.path);
End;

function FsMkDirW(path: pWideChar): Bool; stdcall;
var
	RealPath: TRealPath;
	getResult: integer;
	SkipListRenMov: Bool;
Begin
	ThreadSkipListRenMov.TryGetValue(GetCurrentThreadID(), SkipListRenMov);
	if SkipListRenMov then exit(false); //skip create directory if this flag set on

	RealPath := ExtractRealPath(WideString(path));
	if RealPath.account = '' then exit(false);
	Result := ConnectionManager.get(RealPath.account, getResult).createDir(RealPath.path);
end;

function FsRemoveDirW(RemoteName: pWideChar): Bool; stdcall;
var
	RealPath: TRealPath;
	getResult: integer;
Begin
	RealPath := ExtractRealPath(WideString(RemoteName));
	Result := ConnectionManager.get(RealPath.account, getResult).removeDir(RealPath.path);
end;

Function RenMoveFileViaPublicLink(OldCloud, NewCloud: TCloudMailRu; OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean): integer;
var
	NeedUnpublish: Boolean;
	CurrentItem: TCloudMailRuDirListingItem;
	RetryAttempts: integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	NeedUnpublish := false;
	if OverWrite and not(NewCloud.deleteFile(NewRealPath.path)) then exit;

	if OldCloud.statusFile(OldRealPath.path, CurrentItem) then
	begin
		if CurrentItem.Weblink = '' then //create temporary weblink
		begin
			NeedUnpublish := true;
			if not(OldCloud.publishFile(CurrentItem.home, CurrentItem.Weblink)) then //problem publishing
			begin
				MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Can''t get temporary public link on ' + CurrentItem.home));
				exit(FS_FILE_READERROR);
			end;
		end;
		Result := NewCloud.cloneWeblink(ExtractFileDir(NewRealPath.path), CurrentItem.Weblink, CLOUD_CONFLICT_STRICT);

		if (NeedUnpublish) and not(OldCloud.publishFile(CurrentItem.home, CurrentItem.Weblink, CLOUD_UNPUBLISH)) then MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Can''t remove temporary public link on ' + CurrentItem.home));

		if Result <> CLOUD_OPERATION_OK then
		begin
			case GetPluginSettings(SettingsIniFilePath).OperationErrorMode of
				OperationErrorModeAsk:
					begin
						case (messagebox(FindTCWindow, pWideChar('File publish error: ' + TCloudMailRu.ErrorCodeText(Result) + sLineBreak + 'Continue operation?'), 'Operation error', MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
							ID_ABORT: exit(FS_FILE_USERABORT);
							ID_RETRY: exit(RenMoveFileViaPublicLink(OldCloud, NewCloud, OldRealPath, NewRealPath, Move, OverWrite));
							ID_IGNORE: exit;
						end;
					end;
				OperationErrorModeIgnore: exit;
				OperationErrorModeAbort: exit(FS_FILE_USERABORT);
				OperationErrorModeRetry:
					begin;
						RetryAttempts := GetPluginSettings(SettingsIniFilePath).RetryAttempts;

						while (ThreadRetryCountUpload.Items[GetCurrentThreadID()] <> RetryAttempts) and (Result <> FS_FILE_OK) and (Result <> FS_FILE_USERABORT) do
						begin
							ThreadRetryCountUpload.Items[GetCurrentThreadID()] := ThreadRetryCountUpload.Items[GetCurrentThreadID()] + 1;
							MyLogProc(PluginNum, MSGTYPE_DETAILS, pWideChar('File publish error: ' + TCloudMailRu.ErrorCodeText(Result) + ' Retry attempt ' + ThreadRetryCountUpload.Items[GetCurrentThreadID()].ToString + RetryAttemptsToString(RetryAttempts)));
							Result := RenMoveFileViaPublicLink(OldCloud, NewCloud, OldRealPath, NewRealPath, Move, OverWrite);
							if (Result = FS_FILE_OK) or (Result = FS_FILE_USERABORT) then ThreadRetryCountUpload.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
						end;
					end;
			end;
		end;

		if (Result = CLOUD_OPERATION_OK) and Move and not(OldCloud.deleteFile(OldRealPath.path)) then MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Can''t delete ' + CurrentItem.home)); //пишем в лог, но не отваливаемся
	end;
end;

function FsRenMovFileW(OldName: pWideChar; NewName: pWideChar; Move: Boolean; OverWrite: Boolean; ri: pRemoteInfo): integer; stdcall;
var
	OldRealPath: TRealPath;
	NewRealPath: TRealPath;
	getResult: integer;
	OldCloud, NewCloud: TCloudMailRu;
Begin
	MyProgressProc(PluginNum, OldName, NewName, 0);

	OldRealPath := ExtractRealPath(WideString(OldName));
	NewRealPath := ExtractRealPath(WideString(NewName));

	OldCloud := ConnectionManager.get(OldRealPath.account, getResult);
	NewCloud := ConnectionManager.get(NewRealPath.account, getResult);

	if OldRealPath.account <> NewRealPath.account then //разные аккаунты
	begin
		if OldCloud.isPublicShare then
		begin
			MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Direct operations from public accounts not supported'));
			exit(FS_FILE_USERABORT);
		end;

		if (GetPluginSettings(SettingsIniFilePath).OperationsViaPublicLinkEnabled) then //разрешено копирование через публичные ссылки
		begin
			Result := RenMoveFileViaPublicLink(OldCloud, NewCloud, OldRealPath, NewRealPath, Move, OverWrite);
		end else begin
			MyLogProc(PluginNum, MSGTYPE_IMPORTANTERROR, pWideChar('Direct operations between accounts not supported'));
			exit(FS_FILE_USERABORT);
		end;

	end else begin //один аккаунт

		if OverWrite and not(NewCloud.deleteFile(NewRealPath.path)) then exit(FS_FILE_NOTSUPPORTED); //мы не умеем перезаписывать, но мы можем удалить существующий файл
		if Move then
		begin
			Result := OldCloud.mvFile(OldRealPath.path, NewRealPath.path);
		end else begin
			Result := OldCloud.cpFile(OldRealPath.path, NewRealPath.path);
		end;

	end;
	MyProgressProc(PluginNum, OldName, NewName, 100);
end;

function FsDisconnectW(DisconnectRoot: pWideChar): Bool; stdcall;
begin
	ConnectionManager.freeAll;

	ThreadRetryCountDownload.Free;
	ThreadRetryCountUpload.Free;
	ThreadSkipListDelete.Free;
	ThreadSkipListRenMov.Free;

	//CurrentDescriptions.Destroy;
	Result := true;
end;

procedure FsSetCryptCallbackW(PCryptProc: TCryptProcW; CryptoNr: integer; Flags: integer); stdcall;
var
	CloudMaxFileSize: integer;
begin
	MyCryptProc := PCryptProc;
	CryptoNum := CryptoNr;

	ProxySettings := GetPluginSettings(SettingsIniFilePath).Proxy;
	GetProxyPasswordNow(ProxySettings, MyLogProc, MyCryptProc, PluginNum, CryptoNum);

	if ProxySettings.use_tc_password_manager then SetPluginSettingsValue(SettingsIniFilePath, 'ProxyTCPwdMngr', true);

	CloudMaxFileSize := GetPluginSettings(SettingsIniFilePath).CloudMaxFileSize;
	ConnectionManager := TConnectionManager.Create(AccountsIniFilePath, PluginNum, MyProgressProc, MyLogProc, ProxySettings, GetPluginSettings(SettingsIniFilePath).SocketTimeout, CloudMaxFileSize);
	ConnectionManager.CryptoNum := CryptoNum;
	ConnectionManager.MyCryptProc := MyCryptProc;

end;

function FsContentGetValueW(FileName: pWideChar; FieldIndex: integer; UnitIndex: integer; FieldValue: Pointer; maxlen: integer; Flags: integer): integer; stdcall;
var
	Item: TCloudMailRuDirListingItem;
	RealPath: TRealPath;
	FileTime: TFileTime;
begin
	Result := ft_nosuchfield;
	RealPath := ExtractRealPath(FileName);
	if RealPath.path = '' then
	begin
		if FieldIndex = 14 then
		begin
			strpcopy(FieldValue, GetAccountSettingsFromIniFile(AccountsIniFilePath, ExtractFileName(FileName)).description);
			exit(ft_stringw);
		end
		else exit(ft_nosuchfield);
	end;

	Item := GetListingItemByName(CurrentListing, RealPath);
	if Item.home = '' then exit(ft_nosuchfield);

	case FieldIndex of
		0:
			begin
				if Item.mtime <> 0 then exit(ft_nosuchfield);
				strpcopy(FieldValue, Item.tree);
				Result := ft_stringw;
			end;
		1:
			begin
				strpcopy(FieldValue, Item.name);
				Result := ft_stringw;
			end;
		2:
			begin
				if Item.mtime <> 0 then exit(ft_nosuchfield);
				Move(Item.grev, FieldValue^, SizeOf(Item.grev));
				Result := ft_numeric_32;
			end;
		3:
			begin
				Move(Item.size, FieldValue^, SizeOf(Item.size));
				Result := ft_numeric_64;
			end;
		4:
			begin
				strpcopy(FieldValue, Item.kind);
				Result := ft_stringw;
			end;
		5:
			begin
				strpcopy(FieldValue, Item.Weblink);
				Result := ft_stringw;
			end;
		6:
			begin
				if Item.mtime <> 0 then exit(ft_nosuchfield);
				Move(Item.rev, FieldValue^, SizeOf(Item.rev));
				Result := ft_numeric_32;
			end;
		7:
			begin
				strpcopy(FieldValue, Item.type_);
				Result := ft_stringw;
			end;
		8:
			begin
				strpcopy(FieldValue, Item.home);
				Result := ft_stringw;
			end;
		9:
			begin
				if Item.mtime = 0 then exit(ft_nosuchfield);
				FileTime.dwHighDateTime := 0;
				FileTime.dwLowDateTime := 0;
				FileTime := DateTimeToFileTime(UnixToDateTime(Item.mtime));
				Move(FileTime, FieldValue^, SizeOf(FileTime));
				Result := ft_datetime;
			end;
		10:
			begin
				strpcopy(FieldValue, Item.hash);
				Result := ft_stringw;
			end;
		11:
			begin
				strpcopy(FieldValue, Item.virus_scan);
				Result := ft_stringw;
			end;
		12:
			begin
				if Item.mtime <> 0 then exit(ft_nosuchfield);
				Move(Item.folders_count, FieldValue^, SizeOf(Item.folders_count));
				Result := ft_numeric_32;
			end;
		13:
			begin
				if Item.mtime <> 0 then exit(ft_nosuchfield);
				Move(Item.files_count, FieldValue^, SizeOf(Item.files_count));
				Result := ft_numeric_32;
			end;
		14:
			begin
				//При включённой сортировке Запрос происходит при появлении в списке
				if GetPluginSettings(SettingsIniFilePath).DescriptionEnabled then
				begin
					strpcopy(FieldValue, CurrentDescriptions.GetValue(Item.name));
				end else begin
					strpcopy(FieldValue, '<disabled>');
				end;
				Result := ft_stringw;
			end;
	end;
end;

function FsExtractCustomIconW(RemoteName: pWideChar; ExtractFlags: integer; var TheIcon: hicon): integer; stdcall;
var
	RealPath: TRealPath;
	Item: TCloudMailRuDirListingItem;

begin
	Result := FS_ICON_EXTRACTED;

	RealPath := ExtractRealPath(RemoteName);
	if (RealPath.path = '..') or (RemoteName = '\..\') then exit;
	//if (RealPath.path = '') and (RealPath.account = '') then exit;
	if GetPluginSettings(SettingsIniFilePath).IconsMode = IconsModeDisabled then exit(FS_ICON_USEDEFAULT);

	if (RealPath.path = '') then //connection list
	begin
		if (GetAccountSettingsFromIniFile(AccountsIniFilePath, copy(RemoteName, 2, StrLen(RemoteName) - 2)).public_account) then strpcopy(RemoteName, 'cloud_public')
		else strpcopy(RemoteName, 'cloud');
	end else begin
		//directories
		Item := GetListingItemByName(CurrentListing, RealPath);
		if Item.type_ = TYPE_DIR then
		begin

			if Item.kind = KIND_SHARED then
			begin
				strpcopy(RemoteName, 'shared');
			end else if Item.Weblink <> '' then
			begin
				strpcopy(RemoteName, 'shared_public');
			end else begin
				exit(FS_ICON_USEDEFAULT);
			end;
		end
		else exit(FS_ICON_USEDEFAULT);
	end;
	case GetPluginSettings(SettingsIniFilePath).IconsMode of
		IconsModeInternal: TheIcon := LoadImageW(hInstance, RemoteName, IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
		IconsModeInternalOverlay: TheIcon := CombineIcons(LoadImageW(hInstance, RemoteName, IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR), GetFolderIcon(GetPluginSettings(SettingsIniFilePath).IconsSize));
		IconsModeExternal:
			begin
				TheIcon := LoadPluginIcon(PluginPath + 'icons', RemoteName);
				if TheIcon = INVALID_HANDLE_VALUE then exit(FS_ICON_USEDEFAULT);
			end;
		IconsModeExternalOverlay:
			begin
				TheIcon := LoadPluginIcon(PluginPath + 'icons', RemoteName);
				if TheIcon = INVALID_HANDLE_VALUE then exit(FS_ICON_USEDEFAULT);
				TheIcon := CombineIcons(TheIcon, GetFolderIcon(GetPluginSettings(SettingsIniFilePath).IconsSize));
			end;

	end;
end;

exports FsGetDefRootName, FsInit, FsInitW, FsFindFirst, FsFindFirstW, FsFindNext, FsFindNextW, FsFindClose, FsGetFile, FsGetFileW, FsDisconnect, FsDisconnectW, FsStatusInfo, FsStatusInfoW, FsPutFile, FsPutFileW, FsDeleteFile, FsDeleteFileW, FsMkDir, FsMkDirW, FsRemoveDir, FsRemoveDirW, FsSetCryptCallback, FsSetCryptCallbackW, FsExecuteFileW, FsRenMovFile, FsRenMovFileW, FsGetBackgroundFlags, FsContentGetSupportedField, FsContentGetValue, FsContentGetValueW, FsExtractCustomIcon, FsExtractCustomIconW;

begin
	GetMem(tmp, max_path);
	GetModuleFilename(hInstance, tmp, max_path);
	PluginPath := tmp;
	freemem(tmp);
	AppDataDir := IncludeTrailingBackslash(IncludeTrailingBackslash(SysUtils.GetEnvironmentVariable('APPDATA')) + 'MailRuCloud');
	PluginPath := IncludeTrailingBackslash(ExtractFilePath(PluginPath));

	if not FileExists(PluginPath + 'MailRuCloud.global.ini') then
	begin
		If IsWriteable(PluginPath) then
		begin
			IniDir := PluginPath;
		end else begin
			IniDir := AppDataDir;
		end;

	end else begin
		case GetPluginSettings(PluginPath + 'MailRuCloud.global.ini').IniPath of
			0: //use default path
				begin
					IniDir := PluginPath;
				end;
			1: //use appdata path
				begin
					IniDir := AppDataDir;
				end;
			2: //use plugin dir if writeable
				begin
					if IsWriteable(PluginPath) then IniDir := PluginPath
					else IniDir := AppDataDir;
				end;
		end;
	end;

	if not FileExists(IniDir) then createDir(IniDir); //assume this in appdata dir

	AccountsIniFilePath := IniDir + 'MailRuCloud.ini';
	SettingsIniFilePath := IniDir + 'MailRuCloud.global.ini';
	//IsWriteable(PluginPath,)

	if GetPluginSettings(SettingsIniFilePath).LoadSSLDLLOnlyFromPluginDir then
	begin
		if ((DirectoryExists(PluginPath + PlatformDllPath)) and (FileExists(PluginPath + PlatformDllPath + '\ssleay32.dll')) and (FileExists(PluginPath + PlatformDllPath + '\libeay32.dll'))) then
		begin //try to load dll from platform subdir
			IdOpenSSLSetLibPath(PluginPath + PlatformDllPath);
		end else if ((FileExists(PluginPath + 'ssleay32.dll')) and (FileExists(PluginPath + 'libeay32.dll'))) then
		begin //else try to load it from plugin dir
			IdOpenSSLSetLibPath(PluginPath);
		end;
	end;

	IsMultiThread := not(GetPluginSettings(SettingsIniFilePath).DisableMultiThreading);
	ThreadRetryCountDownload := TDictionary<DWORD, Int32>.Create;
	ThreadRetryCountUpload := TDictionary<DWORD, Int32>.Create;
	ThreadSkipListDelete := TDictionary<DWORD, Bool>.Create;
	ThreadSkipListRenMov := TDictionary<DWORD, Bool>.Create;

end.
