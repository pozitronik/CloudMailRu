library MailRuCloud;

{$R *.dres}

uses
	SysUtils, System.Generics.Collections, DateUtils, windows, Classes, PLUGIN_TYPES, IdSSLOpenSSLHeaders, messages, inifiles, Vcl.controls, CloudMailRu in 'CloudMailRu.pas', MRC_Helper in 'MRC_Helper.pas', Accounts in 'Accounts.pas'{AccountsForm}, RemoteProperty in 'RemoteProperty.pas'{PropertyForm}, Descriptions in 'Descriptions.pas', ConnectionManager in 'ConnectionManager.pas', Settings in 'Settings.pas', ANSIFunctions in 'ANSIFunctions.pas', DeletedProperty in 'DeletedProperty.pas'{DeletedPropertyForm}, InviteProperty in 'InviteProperty.pas'{InvitePropertyForm}, CMLJSON in 'CMLJSON.pas', CMLTypes in 'CMLTypes.pas';

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
	AccountsIniFilePath: WideString;
	SettingsIniFilePath: WideString;
	GlobalPath, PluginPath, AppDataDir, IniDir: WideString;
	AccountsList: TStringList; //Global accounts list
	FileCounter: integer = 0;
	ThreadSkipListDelete: TDictionary<DWORD, Bool>; //Массив id потоков, для которых операции получения листинга должны быть пропущены (при удалении)
	ThreadSkipListRenMov: TDictionary<DWORD, Bool>; //Массив id потоков, для которых операции получения листинга должны быть пропущены (при копировании/перемещении)
	ThreadCanAbortRenMov: TDictionary<DWORD, Bool>; //Массив id потоков, для которых в операциях получения листинга должен быть выведен дополнительный диалог прогресса с возможностью отмены операции (fix issue #113)
	ThreadListingAborted: TDictionary<DWORD, Bool>; //Массив id потоков, для которых в операциях получения листинга была нажата отмена

	ThreadRetryCountDownload: TDictionary<DWORD, Int32>; //массив [id потока => количество попыток] для подсчёта количества повторов скачивания файла
	ThreadRetryCountUpload: TDictionary<DWORD, Int32>; //массив [id потока => количество попыток] для подсчёта количества повторов закачивания файла
	ThreadRetryCountRenMov: TDictionary<DWORD, Int32>; //массив [id потока => количество попыток] для подсчёта количества повторов межсерверных операций с файлом

	{Callback data}
	PluginNum: integer;
	CryptoNum: integer;
	MyProgressProc: TProgressProcW;
	MyLogProc: TLogProcW;
	MyRequestProc: TRequestProcW;
	MyCryptProc: TCryptProcW;
	CurrentListing: TCloudMailRuDirListing;
	CurrentIncomingInvitesListing: TCloudMailRuIncomingInviteInfoListing;
	ConnectionManager: TConnectionManager;
	CurrentDescriptions: TDescription;
	ProxySettings: TProxySettings;

procedure LogHandle(LogLevel, MsgType: integer; LogString: PWideChar); stdcall;
begin
//todo: включение значения
	if {(LogLevel >= GetPluginSettings(SettingsIniFilePath).LogLevel) and} (Assigned(MyLogProc)) then MyLogProc(PluginNum, MsgType, LogString);
end;

function CloudMailRuDirListingItemToFindData(DirListing: TCloudMailRuDirListingItem; DirsAsSymlinks: Boolean = false): tWIN32FINDDATAW;
begin
	if (DirListing.deleted_from <> '') then //items inside trash bin
	begin
		Result.ftCreationTime := DateTimeToFileTime(UnixToDateTime(DirListing.deleted_at));
		Result.ftLastWriteTime := Result.ftCreationTime;
		if (DirListing.type_ = TYPE_DIR) then Result.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY
		else Result.dwFileAttributes := 0;
	end else if (DirListing.type_ = TYPE_DIR) or (DirListing.kind = KIND_SHARED) then
	begin
		Result.ftCreationTime.dwLowDateTime := 0;
		Result.ftCreationTime.dwHighDateTime := 0;
		Result.ftLastWriteTime.dwHighDateTime := 0;
		Result.ftLastWriteTime.dwLowDateTime := 0;
		if DirsAsSymlinks then Result.dwFileAttributes := 0
		else Result.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
	end else begin
		Result.ftCreationTime := DateTimeToFileTime(UnixToDateTime(DirListing.mtime));
		Result.ftLastWriteTime := Result.ftCreationTime;

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

{Пытаемся найти объект в облаке по его пути, сначала в текущем списке, если нет - то ищем в облаке}
function FindListingItemByPath(CurrentListing: TCloudMailRuDirListing; path: TRealPath): TCloudMailRuDirListingItem;
var
	getResult: integer;
	Cloud: TCloudMailRu;

	function FindListingItemByName(DirListing: TCloudMailRuDirListing; ItemName: WideString): TCloudMailRuDirListingItem;
	var
		CurrentItem: TCloudMailRuDirListingItem;
	begin
		for CurrentItem in DirListing do
			if CurrentItem.name = ItemName then exit(CurrentItem);
	end;

	function FindListingItemByHomePath(DirListing: TCloudMailRuDirListing; HomePath: WideString): TCloudMailRuDirListingItem;
	var
		CurrentItem: TCloudMailRuDirListingItem;
	begin
		HomePath := '/' + StringReplace(HomePath, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]);
		for CurrentItem in DirListing do
			if CurrentItem.home = HomePath then exit(CurrentItem);
	end;

begin
	if path.trashDir or path.sharedDir{or path.invitesDir} then Result := FindListingItemByName(CurrentListing, path.path)//Виртуальные каталоги не возвращают HomePath
	else Result := FindListingItemByHomePath(CurrentListing, path.path); //сначала попробуем найти поле в имеющемся списке

	if Result.name = '' then //если там его нет (нажали пробел на папке, например), то запросим в облаке напрямую, в зависимости от того, внутри чего мы находимся
	begin
		Cloud := ConnectionManager.get(path.account, getResult);
		if path.trashDir then //корзина - обновим CurrentListing, поищем в нём
		begin
			if Cloud.getTrashbinListing(CurrentListing) then exit(FindListingItemByName(CurrentListing, path.path));
		end;
		if path.sharedDir then //ссылки - обновим список
		begin
			if Cloud.getSharedLinksListing(CurrentListing) then exit(FindListingItemByName(CurrentListing, path.path));
		end;
		if path.invitesDir then
		begin
			//FindIncomingInviteItemByPath in that case!
		end;
		if Cloud.statusFile(path.path, Result) then //Обычный каталог
		begin
			if (Result.home = '') and not Cloud.isPublicShare then LogHandle(LogLevelError, MSGTYPE_IMPORTANTERROR, PWideChar('Cant find file ' + path.path)); {Такого быть не может, но...}
		end;
	end; //Не рапортуем, это будет уровнем выше
end;

function FindIncomingInviteItemByPath(InviteListing: TCloudMailRuIncomingInviteInfoListing; path: TRealPath): TCloudMailRuIncomingInviteInfo;
var
	getResult: integer;

	function FindListingItemByName(InviteListing: TCloudMailRuIncomingInviteInfoListing; ItemName: WideString): TCloudMailRuIncomingInviteInfo;
	var
		CurrentItem: TCloudMailRuIncomingInviteInfo;
	begin
		for CurrentItem in InviteListing do
			if CurrentItem.name = ItemName then exit(CurrentItem);
	end;

begin
	Result := FindListingItemByName(InviteListing, path.path);
	{item not found in current global listing, so refresh it}
	if Result.name = '' then
		if ConnectionManager.get(path.account, getResult).getIncomingLinksListing(CurrentIncomingInvitesListing) then exit(FindListingItemByName(CurrentIncomingInvitesListing, path.path));

end;

function DeleteLocalFile(LocalName: WideString): integer;
var
	UNCLocalName: WideString;
	DeleteFailOnUploadMode, DeleteFailOnUploadModeAsked: integer;
begin
	Result := FS_FILE_OK;
	DeleteFailOnUploadModeAsked := IDRETRY;
	UNCLocalName := GetUNCFilePath(LocalName);

	while (not DeleteFileW(PWideChar(UNCLocalName))) and (DeleteFailOnUploadModeAsked = IDRETRY) do
	begin
		DeleteFailOnUploadMode := GetPluginSettings(SettingsIniFilePath).DeleteFailOnUploadMode;
		if DeleteFailOnUploadMode = DeleteFailOnUploadAsk then
		begin
			DeleteFailOnUploadModeAsked := messagebox(FindTCWindow, PWideChar('Can''t delete file ' + LocalName + '. Continue operation?'), 'File deletion error', MB_ABORTRETRYIGNORE + MB_ICONQUESTION);
			case DeleteFailOnUploadModeAsked of
				IDRETRY: continue;
				IDABORT: DeleteFailOnUploadMode := DeleteFailOnUploadAbort;
				IDIGNORE: DeleteFailOnUploadMode := DeleteFailOnUploadIgnore;
			end;
		end;

		case DeleteFailOnUploadMode of
			DeleteFailOnUploadAbort:
				begin
					LogHandle(LogLevelDetail, MSGTYPE_IMPORTANTERROR, PWideChar('Can''t delete file ' + LocalName + ', aborted'));
					exit(FS_FILE_NOTSUPPORTED);
				end;
			DeleteFailOnUploadDeleteIgnore, DeleteFailOnUploadDeleteAbort:
				begin
					//check if file just have RO attr, then remove it. If user has lack of rights, then ignore or abort
					if ((FileGetAttr(UNCLocalName) or faReadOnly) <> 0) and ((FileSetAttr(UNCLocalName, not faReadOnly) = 0) and (DeleteFileW(PWideChar(UNCLocalName)))) then
					begin
						LogHandle(LogLevelDetail, MSGTYPE_IMPORTANTERROR, PWideChar('Read only file ' + LocalName + ' deleted'));
						exit(FS_FILE_OK);
					end else begin
						if GetPluginSettings(SettingsIniFilePath).DeleteFailOnUploadMode = DeleteFailOnUploadDeleteIgnore then
						begin
							LogHandle(LogLevelDetail, MSGTYPE_IMPORTANTERROR, PWideChar('Can''t delete file ' + LocalName + ', ignored'));
							exit(FS_FILE_OK);
						end else begin
							LogHandle(LogLevelDetail, MSGTYPE_IMPORTANTERROR, PWideChar('Can''t delete file ' + LocalName + ', aborted'));
							exit(FS_FILE_NOTSUPPORTED);
						end;
					end;
				end;
			else
				begin
					LogHandle(LogLevelDetail, MSGTYPE_IMPORTANTERROR, PWideChar('Can''t delete file ' + LocalName + ', ignored'));
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
begin
	Result := 0;
end;

{GLORIOUS UNICODE MASTER RACE}

function FsInitW(PluginNr: integer; pProgressProc: TProgressProcW; pLogProc: TLogProcW; pRequestProc: TRequestProcW): integer; stdcall; //Вход в плагин.
begin
	PluginNum := PluginNr;
	MyProgressProc := pProgressProc;
	MyLogProc := pLogProc;
	MyRequestProc := pRequestProc;
	Result := 0;
	CurrentDescriptions := TDescription.Create(GetTmpFileName('ion'));

end;

procedure FsStatusInfoW(RemoteDir: PWideChar; InfoStartEnd, InfoOperation: integer); stdcall; //Начало и конец операций FS
var
	RealPath: TRealPath;
	getResult: integer;
begin
	RealPath := ExtractRealPath(RemoteDir);
	if (InfoStartEnd = FS_STATUS_START) then
	begin
		case InfoOperation of
			FS_STATUS_OP_LIST:
				begin
					if (GetPluginSettings(SettingsIniFilePath).DescriptionEnabled) and inAccount(RealPath) then
					begin
						if ConnectionManager.get(RealPath.account, getResult).getDescriptionFile(IncludeTrailingBackslash(RealPath.path) + 'descript.ion', CurrentDescriptions.ionFilename) = FS_FILE_OK then
						begin
							CurrentDescriptions.Read;
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
						LogHandle(LogLevelWarning, MSGTYPE_IMPORTANTERROR, PWideChar('Direct copying from public accounts not supported'));
						ThreadSkipListRenMov.AddOrSetValue(GetCurrentThreadID, true);
					end;
					ThreadRetryCountRenMov.AddOrSetValue(GetCurrentThreadID(), 0);
					ThreadCanAbortRenMov.AddOrSetValue(GetCurrentThreadID, true);
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
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_PUT_MULTI:
				begin
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_RENMOV_SINGLE:
				begin
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_RENMOV_MULTI:
				begin
					ThreadSkipListRenMov.AddOrSetValue(GetCurrentThreadID, false);
					ThreadCanAbortRenMov.AddOrSetValue(GetCurrentThreadID, false);
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_DELETE:
				begin
					ThreadSkipListDelete.AddOrSetValue(GetCurrentThreadID(), false);
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
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
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_SYNC_PUT:
				begin
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_SYNC_DELETE:
				begin
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_GET_MULTI_THREAD:
				begin
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_PUT_MULTI_THREAD:
				begin
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
		end;
		exit;
	end;
end;

function FsFindFirstW(path: PWideChar; var FindData: tWIN32FINDDATAW): THandle; stdcall;
var //Получение первого файла в папке. Result тоталом не используется (можно использовать для работы плагина).
	RealPath: TRealPath;
	getResult: integer;
	SkipListDelete, SkipListRenMov, CanAbortRenMov, RenMovAborted: Bool;
begin
	ThreadSkipListDelete.TryGetValue(GetCurrentThreadID(), SkipListDelete);
	ThreadSkipListRenMov.TryGetValue(GetCurrentThreadID(), SkipListRenMov);

	ThreadCanAbortRenMov.TryGetValue(GetCurrentThreadID(), CanAbortRenMov);

	if (CanAbortRenMov and (MyProgressProc(PluginNum, path, nil, 0) = 1)) then
	begin
		ThreadListingAborted.AddOrSetValue(GetCurrentThreadID(), true);
		RenMovAborted := true;
	end
	else RenMovAborted := false;

	if SkipListDelete or SkipListRenMov or RenMovAborted then
	begin
		SetLastError(ERROR_NO_MORE_FILES);
		exit(INVALID_HANDLE_VALUE);
	end;

	SetLength(CurrentListing, 0);
	//Result := FIND_NO_MORE_FILES;
	GlobalPath := path;
	if GlobalPath = '\' then
	begin //список соединений
		AccountsList := TStringList.Create;
		GetAccountsListFromIniFile(AccountsIniFilePath, AccountsList);

		if (AccountsList.Count > 0) then
		begin
			AddVirtualAccountsToAccountsList(AccountsIniFilePath, AccountsList, [GetPluginSettings(SettingsIniFilePath).ShowTrashFolders, GetPluginSettings(SettingsIniFilePath).ShowSharedFolders, GetPluginSettings(SettingsIniFilePath).ShowInvitesFolders]);

			FindData := FindData_emptyDir(AccountsList.Strings[0]);
			FileCounter := 1;
			Result := FIND_ROOT_DIRECTORY;
		end else begin
			Result := INVALID_HANDLE_VALUE; //Нельзя использовать exit
			SetLastError(ERROR_NO_MORE_FILES);
		end;
	end else begin
		RealPath := ExtractRealPath(GlobalPath);

		if RealPath.trashDir then
		begin
			if not ConnectionManager.get(RealPath.account, getResult).getTrashbinListing(CurrentListing) then SetLastError(ERROR_PATH_NOT_FOUND);
			if RealPath.path <> '' then
			begin
				SetLastError(ERROR_ACCESS_DENIED);
				exit(INVALID_HANDLE_VALUE);
			end;
		end else if RealPath.sharedDir then
		begin
			if not ConnectionManager.get(RealPath.account, getResult).getSharedLinksListing(CurrentListing) then SetLastError(ERROR_PATH_NOT_FOUND); //that will be interpreted as symlinks later
		end else if RealPath.invitesDir then
		begin
			if not ConnectionManager.get(RealPath.account, getResult).getIncomingLinksListing(CurrentListing, CurrentIncomingInvitesListing) then SetLastError(ERROR_PATH_NOT_FOUND); //одновременно получаем оба листинга, чтобы не перечитывать листинг инватов на каждый чих
		end else if not ConnectionManager.get(RealPath.account, getResult).getDirListing(RealPath.path, CurrentListing) then SetLastError(ERROR_PATH_NOT_FOUND);

		if getResult <> CLOUD_OPERATION_OK then
		begin
			SetLastError(ERROR_ACCESS_DENIED);
			exit(INVALID_HANDLE_VALUE);
		end;

		if (Length(CurrentListing) = 0) then
		begin
			FindData := FindData_emptyDir(); //воркароунд бага с невозможностью входа в пустой каталог, см. http://www.ghisler.ch/board/viewtopic.php?t=42399
			Result := FIND_NO_MORE_FILES;
			SetLastError(ERROR_NO_MORE_FILES);
		end else begin
			FindData := CloudMailRuDirListingItemToFindData(CurrentListing[0], RealPath.sharedDir); //folders inside shared links directory must be displayed as symlinks
			FileCounter := 1;
			if RealPath.sharedDir then Result := FIND_SHARED_LINKS
			else Result := FIND_OK;
		end;
	end;
end;

function FsFindNextW(Hdl: THandle; var FindData: tWIN32FINDDATAW): Bool; stdcall;
begin
	if GlobalPath = '\' then
	begin
		if (AccountsList.Count > FileCounter) then
		begin
			FindData := FindData_emptyDir(AccountsList.Strings[FileCounter]);
			inc(FileCounter);
			Result := true;
		end
		else Result := false;

	end else begin
		//Получение последующих файлов в папке (вызывается до тех пор, пока не вернёт false).
		if (Length(CurrentListing) > FileCounter) then
		begin
			FindData := CloudMailRuDirListingItemToFindData(CurrentListing[FileCounter], Hdl = FIND_SHARED_LINKS);
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
begin //Завершение получения списка файлов. Result тоталом не используется (всегда равен 0)
	//SetLength(CurrentListing, 0); // Пусть будет
	if Hdl = FIND_ROOT_DIRECTORY then FreeAndNil(AccountsList);

	Result := 0;
	FileCounter := 0;
end;

function ExecTrashbinProperties(MainWin: THandle; RealPath: TRealPath): integer;
var
	Cloud: TCloudMailRu;
	getResult: integer;
	CurrentItem: TCloudMailRuDirListingItem;
begin
	Result := FS_EXEC_OK;
	Cloud := ConnectionManager.get(RealPath.account, getResult);
	if RealPath.path = '' then //main trashbin folder properties
	begin
		if not Cloud.getTrashbinListing(CurrentListing) then exit(FS_EXEC_ERROR);
		getResult := TDeletedPropertyForm.ShowProperties(MainWin, CurrentListing, true, RealPath.account);
	end else begin //one item in trashbin
		CurrentItem := FindListingItemByPath(CurrentListing, RealPath); //для одинаково именованных файлов в корзине будут показываться свойства первого, сорян
		getResult := TDeletedPropertyForm.ShowProperties(MainWin, [CurrentItem]);
	end;
	case (getResult) of
		mrNo: if not Cloud.trashbinEmpty then exit(FS_EXEC_ERROR);
		mrYes: if not Cloud.trashbinRestore(CurrentItem.deleted_from + CurrentItem.name, CurrentItem.rev) then exit(FS_EXEC_ERROR);
		mrYesToAll: for CurrentItem in CurrentListing do
				if not Cloud.trashbinRestore(CurrentItem.deleted_from + CurrentItem.name, CurrentItem.rev) then exit(FS_EXEC_ERROR);
	end;

	PostMessage(MainWin, WM_USER + 51, 540, 0); //TC does not update current panel, so we should do it this way
end;

function ExecSharedAction(MainWin: THandle; RealPath: TRealPath; RemoteName: PWideChar; ActionOpen: Boolean = true): integer;
var
	Cloud: TCloudMailRu;
	CurrentItem: TCloudMailRuDirListingItem;
	getResult: integer;
begin
	Result := FS_EXEC_OK;
	if ActionOpen then //open item, i.e. treat it as symlink to original location
	begin
		CurrentItem := FindListingItemByPath(CurrentListing, RealPath);
		if CurrentItem.type_ = TYPE_FILE then strpcopy(RemoteName, '\' + RealPath.account + ExtractFilePath(UrlToPath(CurrentItem.home)))
		else strpcopy(RemoteName, '\' + RealPath.account + UrlToPath(CurrentItem.home));
		Result := FS_EXEC_SYMLINK;
	end else begin
		if RealPath.path = '' then TAccountsForm.ShowAccounts(MainWin, AccountsIniFilePath, SettingsIniFilePath, MyCryptProc, PluginNum, CryptoNum, RealPath.account)//main shared folder properties - open connection settings
		else
		begin
			Cloud := ConnectionManager.get(RealPath.account, getResult);
			CurrentItem := FindListingItemByPath(CurrentListing, RealPath);
			if Cloud.statusFile(CurrentItem.home, CurrentItem) then TPropertyForm.ShowProperty(MainWin, RealPath.path, CurrentItem, Cloud, GetPluginSettings(SettingsIniFilePath).DownloadLinksEncode, GetPluginSettings(SettingsIniFilePath).AutoUpdateDownloadListing)
		end;
	end;
end;

function ExecInvitesAction(MainWin: THandle; RealPath: TRealPath): integer;
var
	Cloud: TCloudMailRu;
	getResult: integer;
	CurrentInvite: TCloudMailRuIncomingInviteInfo;
begin
	Result := FS_EXEC_OK;
	Cloud := ConnectionManager.get(RealPath.account, getResult);
	if RealPath.path = '' then //main invites folder properties
	begin
		TAccountsForm.ShowAccounts(MainWin, AccountsIniFilePath, SettingsIniFilePath, MyCryptProc, PluginNum, CryptoNum, RealPath.account)
	end else begin //one invite item
		CurrentInvite := FindIncomingInviteItemByPath(CurrentIncomingInvitesListing, RealPath);
		if CurrentInvite.name = '' then exit(FS_EXEC_ERROR);

		getResult := TInvitePropertyForm.ShowProperties(MainWin, CurrentInvite);
	end;
	case (getResult) of
		mrAbort: Cloud.unmountFolder(CurrentInvite.name, true);
		mrClose: Cloud.unmountFolder(CurrentInvite.name, false);
		mrYes: Cloud.mountFolder(CurrentInvite.name, CurrentInvite.invite_token);
		mrNo: Cloud.rejectInvite(CurrentInvite.invite_token);

	end;

	PostMessage(MainWin, WM_USER + 51, 540, 0); //TC does not update current panel, so we should do it this way
end;

function ExecProperties(MainWin: THandle; RealPath: TRealPath): integer;
var
	Cloud: TCloudMailRu;
	CurrentItem: TCloudMailRuDirListingItem;
	getResult: integer;
begin
	Result := FS_EXEC_OK;
	if RealPath.path = '' then TAccountsForm.ShowAccounts(MainWin, AccountsIniFilePath, SettingsIniFilePath, MyCryptProc, PluginNum, CryptoNum, RealPath.account)//show account properties
	else
	begin
		Cloud := ConnectionManager.get(RealPath.account, getResult);
		if Cloud.statusFile(RealPath.path, CurrentItem) then //всегда нужно обновлять статус на сервере, CurrentListing может быть изменён в другой панели
		begin
			if Cloud.isPublicShare then TPropertyForm.ShowProperty(MainWin, RealPath.path, CurrentItem, Cloud, GetPluginSettings(SettingsIniFilePath).DownloadLinksEncode, GetPluginSettings(SettingsIniFilePath).AutoUpdateDownloadListing)
			else TPropertyForm.ShowProperty(MainWin, RealPath.path, CurrentItem, Cloud, GetPluginSettings(SettingsIniFilePath).DownloadLinksEncode, GetPluginSettings(SettingsIniFilePath).AutoUpdateDownloadListing);
		end;
	end;
end;

function ExecCommand(RemoteName: PWideChar; command: WideString; Parameter: WideString = ''): integer;
var
	RealPath: TRealPath;
	getResult: integer;
	Cloud: TCloudMailRu;
begin
	Result := FS_EXEC_OK;

	if command = 'rmdir' then
	begin
		RealPath := ExtractRealPath(RemoteName + Parameter);
		if (ConnectionManager.get(RealPath.account, getResult).removeDir(RealPath.path) <> true) then exit(FS_EXEC_ERROR);
	end;

	RealPath := ExtractRealPath(RemoteName); //default
	Cloud := ConnectionManager.get(RealPath.account, getResult);

	//undocumented, share current folder to email param
	if command = 'share' then
		if not(Cloud.shareFolder(RealPath.path, ExtractLinkFromUrl(Parameter), CLOUD_SHARE_RW)) then exit(FS_EXEC_ERROR);

	if command = 'clone' then
	begin
		if (Cloud.cloneWeblink(RealPath.path, ExtractLinkFromUrl(Parameter)) = CLOUD_OPERATION_OK) then
			if GetPluginSettings(SettingsIniFilePath).LogUserSpace then Cloud.logUserSpaceInfo
			else exit(FS_EXEC_ERROR);
	end;

	if command = 'trash' then //go to current account trash directory
	begin
		if Cloud.isPublicShare then exit(FS_EXEC_ERROR);
		if inAccount(RealPath, false) then
		begin
			strpcopy(RemoteName, '\' + RealPath.account + TrashPostfix);
			exit(FS_EXEC_SYMLINK);
		end;
	end;

	if command = 'shared' then
	begin
		if Cloud.isPublicShare then exit(FS_EXEC_ERROR);
		if inAccount(RealPath, false) then
		begin
			strpcopy(RemoteName, '\' + RealPath.account + SharedPostfix);
			exit(FS_EXEC_SYMLINK);
		end;
	end;

	if command = 'invites' then
	begin
		if Cloud.isPublicShare then exit(FS_EXEC_ERROR);
		if inAccount(RealPath, false) then
		begin
			strpcopy(RemoteName, '\' + RealPath.account + InvitesPostfix);
			exit(FS_EXEC_SYMLINK);
		end;
	end;

end;

function FsExecuteFileW(MainWin: THandle; RemoteName, Verb: PWideChar): integer; stdcall; //Запуск файла
var
	RealPath: TRealPath;
begin
	RealPath := ExtractRealPath(RemoteName);
	Result := FS_EXEC_OK;

	if RealPath.upDirItem then RealPath.path := ExtractFilePath(RealPath.path); //if somepath/.. item properties called

	if RealPath.trashDir and ((Verb = 'open') or (Verb = 'properties')) then exit(ExecTrashbinProperties(MainWin, RealPath));

	if RealPath.sharedDir then exit(ExecSharedAction(MainWin, RealPath, RemoteName, Verb = 'open'));

	if RealPath.invitesDir then exit(ExecInvitesAction(MainWin, RealPath));

	if Verb = 'properties' then exit(ExecProperties(MainWin, RealPath));

	if Verb = 'open' then exit(FS_EXEC_YOURSELF);

	if copy(Verb, 1, 5) = 'quote' then exit(ExecCommand(RemoteName, LowerCase(GetWord(Verb, 1)), GetWord(Verb, 2)));

	//if copy(Verb, 1, 5) = 'chmod' then exit; //future usage

end;

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
			Item := FindListingItemByPath(CurrentListing, RemotePath);
			if Item.mtime <> 0 then SetAllFileTime(ExpandUNCFileName(LocalName), DateTimeToFileTime(UnixToDateTime(Item.mtime)));
		end;
		if CheckFlag(FS_COPYFLAGS_MOVE, CopyFlags) then ConnectionManager.get(RemotePath.account, getResult).deleteFile(RemotePath.path);
		MyProgressProc(PluginNum, PWideChar(LocalName), PWideChar(RemoteName), 100);
		LogHandle(LogLevelFileOperation, MSGTYPE_TRANSFERCOMPLETE, PWideChar(RemoteName + '->' + LocalName));
	end;
end;

function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall; //Копирование файла из файловой системы плагина
var
	RealPath: TRealPath;
	OverwriteLocalMode: integer;
	RetryAttempts: integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if CheckFlag(FS_COPYFLAGS_RESUME, CopyFlags) then exit; {NEVER CALLED HERE}
	RealPath := ExtractRealPath(RemoteName);
	if RealPath.trashDir or RealPath.sharedDir or RealPath.invitesDir then exit;

	MyProgressProc(PluginNum, RemoteName, LocalName, 0);

	OverwriteLocalMode := GetPluginSettings(SettingsIniFilePath).OverwriteLocalMode;
	if (FileExists(LocalName) and not(CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags))) then
	begin
		case OverwriteLocalMode of
			OverwriteLocalModeAsk: exit(FS_FILE_EXISTS); //TC will ask user
			OverwriteLocalModeIgnore:
				begin
					LogHandle(LogLevelDetail, msgtype_details, PWideChar('Local file ' + LocalName + ' exists, ignored'));
					exit(FS_FILE_OK);
				end;
			OverwriteLocalModeOverwrite: LogHandle(LogLevelDetail, msgtype_details, PWideChar('Local file ' + LocalName + ' exists, and will be overwritten'));
		end;
	end;

	Result := GetRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);

	if Result <> FS_FILE_READERROR then exit;

	case GetPluginSettings(SettingsIniFilePath).OperationErrorMode of
		OperationErrorModeAsk:
			begin
				while (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					case (messagebox(FindTCWindow, PWideChar('Error downloading file' + sLineBreak + RemoteName + sLineBreak + 'Continue operation?'), 'Download error', MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
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
					LogHandle(LogLevelDetail, msgtype_details, PWideChar('Error downloading file ' + RemoteName + ' Retry attempt ' + ThreadRetryCountDownload.Items[GetCurrentThreadID()].ToString + RetryAttemptsToString(RetryAttempts)));
					Result := GetRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
					if MyProgressProc(PluginNum, PWideChar(LocalName), RemoteName, 0) = 1 then Result := FS_FILE_USERABORT;
					if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then ThreadRetryCountDownload.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
					ProcessMessages;
					Sleep(GetPluginSettings(SettingsIniFilePath).AttemptWait);
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
		MyProgressProc(PluginNum, PWideChar(LocalName), PWideChar(RemotePath.path), 100);
		LogHandle(LogLevelFileOperation, MSGTYPE_TRANSFERCOMPLETE, PWideChar(LocalName + '->' + RemoteName));
		if CheckFlag(FS_COPYFLAGS_MOVE, CopyFlags) then Result := DeleteLocalFile(LocalName);
	end;

end;

function FsPutFileW(LocalName, RemoteName: PWideChar; CopyFlags: integer): integer; stdcall;
var
	RealPath: TRealPath;
	RetryAttempts: integer;
	getResult: integer;
begin

	RealPath := ExtractRealPath(RemoteName);
	if (RealPath.account = '') or RealPath.trashDir or RealPath.sharedDir or RealPath.invitesDir then exit(FS_FILE_NOTSUPPORTED);
	MyProgressProc(PluginNum, LocalName, PWideChar(RealPath.path), 0);

	if CheckFlag(FS_COPYFLAGS_RESUME, CopyFlags) then exit(FS_FILE_NOTSUPPORTED); //NOT SUPPORTED

	if (CheckFlag(FS_COPYFLAGS_EXISTS_SAMECASE, CopyFlags) or CheckFlag(FS_COPYFLAGS_EXISTS_DIFFERENTCASE, CopyFlags)) and not(CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags)) then exit(FS_FILE_EXISTS); //Облако не поддерживает разные регистры

	if CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags) then
	begin
		if not(ConnectionManager.get(RealPath.account, getResult).deleteFile(RealPath.path)) then exit(FS_FILE_NOTSUPPORTED); //Неизвестно, как перезаписать файл черз API, но мы можем его удалить
	end;
	Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);

	//if Result in [FS_FILE_OK, FS_FILE_USERABORT, FS_FILE_NOTSUPPORTED] then exit;
	if Result <> FS_FILE_WRITEERROR then exit;

	case GetPluginSettings(SettingsIniFilePath).OperationErrorMode of
		OperationErrorModeAsk:
			begin
				while (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					case (messagebox(FindTCWindow, PWideChar('Error uploading file' + sLineBreak + LocalName + sLineBreak + 'Continue operation?'), 'Upload error', MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
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
					LogHandle(LogLevelDetail, msgtype_details, PWideChar('Error uploading file ' + LocalName + ' Retry attempt ' + ThreadRetryCountUpload.Items[GetCurrentThreadID()].ToString + RetryAttemptsToString(RetryAttempts)));
					Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
					if MyProgressProc(PluginNum, PWideChar(LocalName), RemoteName, 0) = 1 then Result := FS_FILE_USERABORT;
					if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then ThreadRetryCountUpload.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
					ProcessMessages;
					Sleep(GetPluginSettings(SettingsIniFilePath).AttemptWait);
				end;
			end;
	end;

end;

function FsDeleteFileW(RemoteName: PWideChar): Bool; stdcall; //Удаление файла из файловой системы плагина
var
	RealPath: TRealPath;
	getResult: integer;
	CurrentItem: TCloudMailRuDirListingItem;
	Cloud: TCloudMailRu;
	InvitesListing: TCloudMailRuInviteInfoListing;
	Invite: TCloudMailRuInviteInfo;
begin
	RealPath := ExtractRealPath(WideString(RemoteName));
	if (RealPath.account = '') or RealPath.trashDir or RealPath.invitesDir then exit(false);
	Cloud := ConnectionManager.get(RealPath.account, getResult);
	if RealPath.sharedDir then
	begin
		CurrentItem := FindListingItemByPath(CurrentListing, RealPath);
		Cloud.getShareInfo(CurrentItem.home, InvitesListing);
		for Invite in InvitesListing do Cloud.shareFolder(CurrentItem.home, Invite.email, CLOUD_SHARE_NO); //no reporting here
		if (CurrentItem.WebLink <> '') then Cloud.publishFile(CurrentItem.home, CurrentItem.WebLink, CLOUD_UNPUBLISH);
		Result := true;
	end
	else Result := Cloud.deleteFile(RealPath.path);
end;

function FsMkDirW(path: PWideChar): Bool; stdcall;
var
	RealPath: TRealPath;
	getResult: integer;
	SkipListRenMov: Bool;
begin
	ThreadSkipListRenMov.TryGetValue(GetCurrentThreadID(), SkipListRenMov);
	if SkipListRenMov then exit(false); //skip create directory if this flag set on

	RealPath := ExtractRealPath(WideString(path));
	if (RealPath.account = '') or RealPath.trashDir or RealPath.sharedDir or RealPath.invitesDir then exit(false);
	Result := ConnectionManager.get(RealPath.account, getResult).createDir(RealPath.path);
end;

function FsRemoveDirW(RemoteName: PWideChar): Bool; stdcall;
var
	RealPath: TRealPath;
	getResult: integer;
	ListingAborted: Bool;
begin
	ThreadListingAborted.TryGetValue(GetCurrentThreadID(), ListingAborted);
	if ListingAborted then
	begin
		ThreadListingAborted.AddOrSetValue(GetCurrentThreadID(), false);
		exit(false);
	end;
	RealPath := ExtractRealPath(WideString(RemoteName));
	if RealPath.trashDir or RealPath.sharedDir or RealPath.invitesDir then exit(false);
	Result := ConnectionManager.get(RealPath.account, getResult).removeDir(RealPath.path);
end;

function cloneWeblink(NewCloud, OldCloud: TCloudMailRu; CloudPath: WideString; CurrentItem: TCloudMailRuDirListingItem; NeedUnpublish: Boolean): integer;
begin
	Result := NewCloud.cloneWeblink(ExtractFileDir(CloudPath), CurrentItem.WebLink, CLOUD_CONFLICT_STRICT);
	if (NeedUnpublish) and not(OldCloud.publishFile(CurrentItem.home, CurrentItem.WebLink, CLOUD_UNPUBLISH)) then LogHandle(LogLevelError, MSGTYPE_IMPORTANTERROR, PWideChar('Can''t remove temporary public link on ' + CurrentItem.home));
end;

function RenMoveFileViaPublicLink(OldCloud, NewCloud: TCloudMailRu; OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean): integer;
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
		if CurrentItem.WebLink = '' then //create temporary weblink
		begin
			NeedUnpublish := true;
			if not(OldCloud.publishFile(CurrentItem.home, CurrentItem.WebLink)) then //problem publishing
			begin
				LogHandle(LogLevelError, MSGTYPE_IMPORTANTERROR, PWideChar('Can''t get temporary public link on ' + CurrentItem.home));
				exit(FS_FILE_READERROR);
			end;
		end;
		Result := cloneWeblink(NewCloud, OldCloud, NewRealPath.path, CurrentItem, NeedUnpublish);
		if not(Result in [FS_FILE_OK, FS_FILE_EXISTS]) then
		begin

			case GetPluginSettings(SettingsIniFilePath).OperationErrorMode of
				OperationErrorModeAsk:
					begin

						while (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
						begin
							case (messagebox(FindTCWindow, PWideChar('File publish error: ' + TCloudMailRu.ErrorCodeText(Result) + sLineBreak + 'Continue operation?'), 'Operation error', MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
								ID_ABORT: Result := FS_FILE_USERABORT;
								ID_RETRY: Result := cloneWeblink(NewCloud, OldCloud, NewRealPath.path, CurrentItem, NeedUnpublish);
								ID_IGNORE: break;
							end;
						end;

					end;
				OperationErrorModeIgnore: exit;
				OperationErrorModeAbort: exit(FS_FILE_USERABORT);
				OperationErrorModeRetry:
					begin;
						RetryAttempts := GetPluginSettings(SettingsIniFilePath).RetryAttempts;
						while (ThreadRetryCountRenMov.Items[GetCurrentThreadID()] <> RetryAttempts) and (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
						begin
							ThreadRetryCountRenMov.Items[GetCurrentThreadID()] := ThreadRetryCountRenMov.Items[GetCurrentThreadID()] + 1;
							LogHandle(LogLevelDetail, msgtype_details, PWideChar('File publish error: ' + TCloudMailRu.ErrorCodeText(Result) + ' Retry attempt ' + ThreadRetryCountRenMov.Items[GetCurrentThreadID()].ToString + RetryAttemptsToString(RetryAttempts)));
							Result := cloneWeblink(NewCloud, OldCloud, NewRealPath.path, CurrentItem, NeedUnpublish);
							if MyProgressProc(PluginNum, nil, nil, 0) = 1 then Result := FS_FILE_USERABORT;
							if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then ThreadRetryCountRenMov.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
							ProcessMessages;
							Sleep(GetPluginSettings(SettingsIniFilePath).AttemptWait);
						end;
					end;
			end;
		end;

		if (Result = CLOUD_OPERATION_OK) and Move and not(OldCloud.deleteFile(OldRealPath.path)) then LogHandle(LogLevelError, MSGTYPE_IMPORTANTERROR, PWideChar('Can''t delete ' + CurrentItem.home)); //пишем в лог, но не отваливаемся
	end;
end;

function FsRenMovFileW(OldName: PWideChar; NewName: PWideChar; Move: Boolean; OverWrite: Boolean; ri: pRemoteInfo): integer; stdcall;
var
	OldRealPath: TRealPath;
	NewRealPath: TRealPath;
	getResult: integer;
	OldCloud, NewCloud: TCloudMailRu;
begin
	MyProgressProc(PluginNum, OldName, NewName, 0);

	OldRealPath := ExtractRealPath(WideString(OldName));
	NewRealPath := ExtractRealPath(WideString(NewName));

	if OldRealPath.trashDir or NewRealPath.trashDir or OldRealPath.sharedDir or NewRealPath.sharedDir then exit(FS_FILE_NOTSUPPORTED);

	OldCloud := ConnectionManager.get(OldRealPath.account, getResult);
	NewCloud := ConnectionManager.get(NewRealPath.account, getResult);

	if OldRealPath.account <> NewRealPath.account then //разные аккаунты
	begin
		if OldCloud.isPublicShare then
		begin
			LogHandle(LogLevelWarning, MSGTYPE_IMPORTANTERROR, PWideChar('Direct operations from public accounts not supported'));
			exit(FS_FILE_USERABORT);
		end;

		if (GetPluginSettings(SettingsIniFilePath).OperationsViaPublicLinkEnabled) then //разрешено копирование через публичные ссылки
		begin
			Result := RenMoveFileViaPublicLink(OldCloud, NewCloud, OldRealPath, NewRealPath, Move, OverWrite);
		end else begin
			LogHandle(LogLevelWarning, MSGTYPE_IMPORTANTERROR, PWideChar('Direct operations between accounts not supported'));
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

function FsDisconnectW(DisconnectRoot: PWideChar): Bool; stdcall;
begin
	ConnectionManager.freeAll;

	Result := true;
end;

procedure FsSetCryptCallbackW(PCryptProc: TCryptProcW; CryptoNr: integer; Flags: integer); stdcall;
var
	CloudMaxFileSize: integer;
begin
	MyCryptProc := PCryptProc;
	CryptoNum := CryptoNr;

	ProxySettings := GetPluginSettings(SettingsIniFilePath).Proxy;
	GetProxyPasswordNow(ProxySettings, @LogHandle, MyCryptProc, PluginNum, CryptoNum); //todo plugin num unused

	if ProxySettings.use_tc_password_manager then SetPluginSettingsValue(SettingsIniFilePath, 'ProxyTCPwdMngr', true);

	CloudMaxFileSize := GetPluginSettings(SettingsIniFilePath).CloudMaxFileSize;
	ConnectionManager := TConnectionManager.Create(AccountsIniFilePath, PluginNum, MyProgressProc, @LogHandle, ProxySettings, GetPluginSettings(SettingsIniFilePath).SocketTimeout, CloudMaxFileSize, MyRequestProc);
	ConnectionManager.CryptoNum := CryptoNum;
	ConnectionManager.MyCryptProc := MyCryptProc;

end;

function FsContentGetValueW(FileName: PWideChar; FieldIndex: integer; UnitIndex: integer; FieldValue: Pointer; maxlen: integer; Flags: integer): integer; stdcall;
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

	Item := FindListingItemByPath(CurrentListing, RealPath);
	//if Item.home = '' then exit(ft_nosuchfield);

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
				strpcopy(FieldValue, Item.WebLink);
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
				if Item.type_ = TYPE_FILE then exit(ft_nosuchfield);
				Move(Item.folders_count, FieldValue^, SizeOf(Item.folders_count));
				Result := ft_numeric_32;
			end;
		13:
			begin
				if Item.type_ = TYPE_FILE then exit(ft_nosuchfield);
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
		15:
			begin
				if Item.deleted_at = 0 then exit(ft_nosuchfield);
				FileTime.dwHighDateTime := 0;
				FileTime.dwLowDateTime := 0;
				FileTime := DateTimeToFileTime(UnixToDateTime(Item.deleted_at));
				Move(FileTime, FieldValue^, SizeOf(FileTime));
				Result := ft_datetime;
			end;
		16:
			begin
				if Item.deleted_from = '' then exit(ft_nosuchfield);
				strpcopy(FieldValue, Item.deleted_from);
				Result := ft_stringw;
			end;
		17:
			begin
				if Item.deleted_by = 0 then exit(ft_nosuchfield);
				strpcopy(FieldValue, Item.deleted_by.ToString); //display user id as is, because no conversation api method performed
				Result := ft_stringw;
			end;
	end;
end;

function FsExtractCustomIconW(RemoteName: PWideChar; ExtractFlags: integer; var TheIcon: hicon): integer; stdcall;
var
	RealPath: TRealPath;
	Item: TCloudMailRuDirListingItem;
	IconsMode: integer;
	CurrentInviteItem: TCloudMailRuIncomingInviteInfo;
	IconsSize: integer;

	function GetFolderIconSize(IconsSize: integer): integer;
	begin
		if IconsSize <= 16 then exit(IconSizeSmall);
		if IconsSize <= 32 then exit(IconSizeNormal);
		exit(IconSizeLarge);
	end;

begin
	Result := FS_ICON_EXTRACTED;

	RealPath := ExtractRealPath(RemoteName);

	if RealPath.upDirItem then exit; //do not overlap updir icon

	IconsMode := GetPluginSettings(SettingsIniFilePath).IconsMode;
	IconsSize := GetTCIconsSize;

	if RealPath.trashDir and (RealPath.path = '') then //always draw system trash icon
	begin
		strpcopy(RemoteName, 'cloud_trash');
		TheIcon := GetSystemIcon(GetFolderIconSize(IconsSize));
		exit(FS_ICON_EXTRACTED_DESTROY);
	end;

	if RealPath.sharedDir then
	begin
		if (RealPath.path = '') then
		begin
			strpcopy(RemoteName, 'shared');
			TheIcon := CombineIcons(LoadImageW(hInstance, RemoteName, IMAGE_ICON, IconsSize, IconsSize, LR_DEFAULTCOLOR), GetFolderIcon(GetFolderIconSize(IconsSize)));
			exit(FS_ICON_EXTRACTED_DESTROY);
		end else begin
			if IconsMode = IconsModeDisabled then IconsMode := IconsModeInternalOverlay; //always draw icons in shared links directory
		end;
	end;

	if RealPath.invitesDir then
	begin
		if (RealPath.path = '') then
		begin
			strpcopy(RemoteName, 'shared_incoming');
			TheIcon := CombineIcons(LoadImageW(hInstance, RemoteName, IMAGE_ICON, IconsSize, IconsSize, LR_DEFAULTCOLOR), GetFolderIcon(GetFolderIconSize(IconsSize)));
			exit(FS_ICON_EXTRACTED_DESTROY);
		end else begin

			CurrentInviteItem := FindIncomingInviteItemByPath(CurrentIncomingInvitesListing, RealPath);
			if CurrentInviteItem.name = '' then exit(FS_ICON_USEDEFAULT);

			if CurrentInviteItem.home <> '' then //mounted item
			begin
				strpcopy(RemoteName, 'shared_incoming');
				TheIcon := CombineIcons(LoadImageW(hInstance, RemoteName, IMAGE_ICON, IconsSize, IconsSize, LR_DEFAULTCOLOR), GetFolderIcon(GetFolderIconSize(IconsSize)));
			end else begin
				strpcopy(RemoteName, 'shared');
				TheIcon := CombineIcons(LoadImageW(hInstance, RemoteName, IMAGE_ICON, IconsSize, IconsSize, LR_DEFAULTCOLOR), GetFolderIcon(GetFolderIconSize(IconsSize)));
			end;
			exit(FS_ICON_EXTRACTED_DESTROY);

		end;
	end;

	if IconsMode = IconsModeDisabled then exit(FS_ICON_USEDEFAULT);

	if (RealPath.path = '') then //connection list
	begin

		if (GetAccountSettingsFromIniFile(AccountsIniFilePath, copy(RemoteName, 2, StrLen(RemoteName) - 2)).public_account) then strpcopy(RemoteName, 'cloud_public')
		else strpcopy(RemoteName, 'cloud');
	end else begin //directories
		Item := FindListingItemByPath(CurrentListing, RealPath);
		if (Item.type_ = TYPE_DIR) or (Item.kind = KIND_SHARED) then
		begin
			if Item.kind = KIND_SHARED then strpcopy(RemoteName, 'shared')
			else if Item.WebLink <> '' then strpcopy(RemoteName, 'shared_public')
			else exit(FS_ICON_USEDEFAULT);
		end
		else exit(FS_ICON_USEDEFAULT);
	end;
	case IconsMode of
		IconsModeInternal: TheIcon := LoadImageW(hInstance, RemoteName, IMAGE_ICON, IconsSize, IconsSize, LR_DEFAULTCOLOR);
		IconsModeInternalOverlay: TheIcon := CombineIcons(LoadImageW(hInstance, RemoteName, IMAGE_ICON, IconsSize, IconsSize, LR_DEFAULTCOLOR), GetFolderIcon(GetFolderIconSize(IconsSize)));
		IconsModeExternal:
			begin
				TheIcon := LoadPluginIcon(PluginPath + 'icons', RemoteName);
				if TheIcon = INVALID_HANDLE_VALUE then exit(FS_ICON_USEDEFAULT);
				exit(FS_ICON_EXTRACTED_DESTROY);
			end;
		IconsModeExternalOverlay:
			begin
				TheIcon := LoadPluginIcon(PluginPath + 'icons', RemoteName);
				if TheIcon = INVALID_HANDLE_VALUE then exit(FS_ICON_USEDEFAULT);
				TheIcon := CombineIcons(TheIcon, GetFolderIcon(GetFolderIconSize(IconsSize)));
				exit(FS_ICON_EXTRACTED_DESTROY);
			end;

	end;
end;

procedure InitPluginData;
begin
	PluginPath := GetModuleName(hInstance);
	AppDataDir := IncludeTrailingBackslash(IncludeTrailingBackslash(SysUtils.GetEnvironmentVariable('APPDATA')) + 'MailRuCloud');
	PluginPath := IncludeTrailingBackslash(ExtractFilePath(PluginPath));

	if not FileExists(PluginPath + 'MailRuCloud.global.ini') then
	begin
		if IsWriteable(PluginPath) then
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
	ThreadRetryCountRenMov := TDictionary<DWORD, Int32>.Create;
	ThreadSkipListDelete := TDictionary<DWORD, Bool>.Create;
	ThreadSkipListRenMov := TDictionary<DWORD, Bool>.Create;
	ThreadCanAbortRenMov := TDictionary<DWORD, Bool>.Create;
	ThreadListingAborted := TDictionary<DWORD, Bool>.Create;
end;

procedure FreePluginData;
begin
	FreeAndNil(ThreadRetryCountDownload);
	FreeAndNil(ThreadRetryCountUpload);
	FreeAndNil(ThreadRetryCountRenMov);
	FreeAndNil(ThreadSkipListDelete);
	FreeAndNil(ThreadSkipListRenMov);
	FreeAndNil(ThreadCanAbortRenMov);
	FreeAndNil(ThreadListingAborted);
	FreeAndNil(ConnectionManager);
	FreeAndNil(AccountsList); //уже сделано, но не страшно, к тому же в будущем может не разрушаться ранее
	CurrentDescriptions.Free;
end;

procedure DllInit(Code: integer);
begin
	case Code of
		DLL_PROCESS_ATTACH:
			begin
				InitPluginData;
			end;
		DLL_PROCESS_DETACH:
			begin
				FreePluginData;
			end;
	end; //case
end;

exports
	FsGetDefRootName, FsInit, FsInitW, FsFindFirst, FsFindFirstW, FsFindNext, FsFindNextW, FsFindClose, FsGetFile, FsGetFileW, FsDisconnect, FsDisconnectW, FsStatusInfo,
	FsStatusInfoW, FsPutFile, FsPutFileW, FsDeleteFile, FsDeleteFileW, FsMkDir, FsMkDirW, FsRemoveDir, FsRemoveDirW, FsSetCryptCallback, FsSetCryptCallbackW, FsExecuteFileW,
	FsRenMovFile, FsRenMovFileW, FsGetBackgroundFlags, FsContentGetSupportedField, FsContentGetValue, FsContentGetValueW, FsExtractCustomIcon, FsExtractCustomIconW;

begin
	//ReportMemoryLeaksOnShutdown := true;
	DllProc := @DllInit;
	DllInit(DLL_PROCESS_ATTACH);

end.
