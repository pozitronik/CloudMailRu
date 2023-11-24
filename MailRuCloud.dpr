library MailRuCloud;

{$R *.dres}

uses
{$IFDEF DEBUG}
	FastMM4 in 'FastMM\FastMM4.pas',
	FastMM4Messages in 'FastMM\FastMM4Messages.pas',
{$ENDIF }
	JSON,
	SysUtils,
	System.Generics.Collections,
	DateUtils,
	windows,
	Classes,
	ANSIFunctions,
	PLUGIN_TYPES,
	CMRStrings,
	IdSSLOpenSSLHeaders,
	messages,
	inifiles,
	Vcl.controls,
	CloudMailRu in 'CloudMailRu.pas',
	Accounts in 'forms\Accounts.pas'{AccountsForm},
	RemoteProperty in 'forms\RemoteProperty.pas'{PropertyForm},
	DeletedProperty in 'forms\DeletedProperty.pas'{DeletedPropertyForm},
	InviteProperty in 'forms\InviteProperty.pas'{InvitePropertyForm},
	AskPassword in 'forms\AskPassword.pas'{AskPasswordForm},
	Registration in 'forms\Registration.pas'{AskPasswordForm},
	CMRConstants,
	DCPbase64 in 'DCPCrypt\DCPbase64.pas',
	DCPblockciphers in 'DCPCrypt\DCPblockciphers.pas',
	DCPconst in 'DCPCrypt\DCPconst.pas',
	DCPcrypt2 in 'DCPCrypt\DCPcrypt2.pas',
	DCPreg in 'DCPCrypt\DCPreg.pas',
	DCPtypes in 'DCPCrypt\DCPtypes.pas',
	DCPblowfish in 'DCPCrypt\Ciphers\DCPblowfish.pas',
	DCPcast128 in 'DCPCrypt\Ciphers\DCPcast128.pas',
	DCPcast256 in 'DCPCrypt\Ciphers\DCPcast256.pas',
	DCPdes in 'DCPCrypt\Ciphers\DCPdes.pas',
	DCPgost in 'DCPCrypt\Ciphers\DCPgost.pas',
	DCPice in 'DCPCrypt\Ciphers\DCPice.pas',
	DCPidea in 'DCPCrypt\Ciphers\DCPidea.pas',
	DCPmars in 'DCPCrypt\Ciphers\DCPmars.pas',
	DCPmisty1 in 'DCPCrypt\Ciphers\DCPmisty1.pas',
	DCPrc2 in 'DCPCrypt\Ciphers\DCPrc2.pas',
	DCPrc4 in 'DCPCrypt\Ciphers\DCPrc4.pas',
	DCPrc5 in 'DCPCrypt\Ciphers\DCPrc5.pas',
	DCPrc6 in 'DCPCrypt\Ciphers\DCPrc6.pas',
	DCPrijndael in 'DCPCrypt\Ciphers\DCPrijndael.pas',
	DCPserpent in 'DCPCrypt\Ciphers\DCPserpent.pas',
	DCPtea in 'DCPCrypt\Ciphers\DCPtea.pas',
	DCPtwofish in 'DCPCrypt\Ciphers\DCPtwofish.pas',
	DCPhaval in 'DCPCrypt\Hashes\DCPhaval.pas',
	DCPmd4 in 'DCPCrypt\Hashes\DCPmd4.pas',
	DCPmd5 in 'DCPCrypt\Hashes\DCPmd5.pas',
	DCPripemd128 in 'DCPCrypt\Hashes\DCPripemd128.pas',
	DCPripemd160 in 'DCPCrypt\Hashes\DCPripemd160.pas',
	DCPsha1 in 'DCPCrypt\Hashes\DCPsha1.pas',
	DCPsha256 in 'DCPCrypt\Hashes\DCPsha256.pas',
	DCPsha512 in 'DCPCrypt\Hashes\DCPsha512.pas',
	DCPtiger in 'DCPCrypt\Hashes\DCPtiger.pas',
	JSONHelper in 'helpers\JSONHelper.pas',
	PluginHelper in 'helpers\PluginHelper.pas',
	ChunkedFileStream in 'models\ChunkedFileStream.pas',
	ConnectionManager in 'models\ConnectionManager.pas',
	Description in 'models\Description.pas',
	FileSplitInfo in 'models\FileSplitInfo.pas',
	HashInfo in 'models\HashInfo.pas',
	TCPasswordManager in 'models\TCPasswordManager.pas',
	Settings in 'models\settings\Settings.pas',
	CloudMailRuHTTP in 'models\http\CloudMailRuHTTP.pas',
	HTTPManager in 'models\http\HTTPManager.pas',
	CloudMailRuDirListing in 'models\dto\CloudMailRuDirListing.pas',
	CloudMailRuDirListingItem in 'models\dto\CloudMailRuDirListingItem.pas',
	CloudMailRuFileIdentity in 'models\dto\CloudMailRuFileIdentity.pas',
	CloudMailRuIncomingInviteInfo in 'models\dto\CloudMailRuIncomingInviteInfo.pas',
	CloudMailRuIncomingInviteInfoListing in 'models\dto\CloudMailRuIncomingInviteInfoListing.pas',
	CloudMailRuInviteInfo in 'models\dto\CloudMailRuInviteInfo.pas',
	CloudMailRuInviteInfoListing in 'models\dto\CloudMailRuInviteInfoListing.pas',
	CloudMailRuOAuthInfo in 'models\dto\CloudMailRuOAuthInfo.pas',
	CloudMailRuOperationResult in 'models\dto\CloudMailRuOperationResult.pas',
	CloudMailRuOwnerInfo in 'models\dto\CloudMailRuOwnerInfo.pas',
	CloudMailRuSpaceInfo in 'models\dto\CloudMailRuSpaceInfo.pas',
	CloudMailRuTwostepData in 'models\dto\CloudMailRuTwostepData.pas',
	FileCipher in 'models\cipher\FileCipher.pas',
	ParsingHelper in 'helpers\ParsingHelper.pas',
	TCHelper in 'helpers\TCHelper.pas',
	WindowsHelper in 'helpers\WindowsHelper.pas',
	IconHelper in 'helpers\IconHelper.pas',
	SystemHelper in 'helpers\SystemHelper.pas',
	StringHelper in 'helpers\StringHelper.pas',
	FileHelper in 'helpers\FileHelper.pas',
	PathHelper in 'helpers\PathHelper.pas',
	DebugHelper in 'helpers\DebugHelper.pas',
	RealPath in 'models\dto\RealPath.pas',
	TCLogger in 'models\TCLogger.pas';

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
	CurrentlyMovedDir: TRealPath;
	ThreadSkipListDelete: TDictionary<DWORD, Boolean>; //Массив id потоков, для которых операции получения листинга должны быть пропущены (при удалении)
	ThreadSkipListRenMov: TDictionary<DWORD, Boolean>; //Массив id потоков, для которых операции получения листинга должны быть пропущены (при копировании/перемещении)
	ThreadCanAbortRenMov: TDictionary<DWORD, Boolean>; //Массив id потоков, для которых в операциях получения листинга должен быть выведен дополнительный диалог прогресса с возможностью отмены операции (fix issue #113)
	ThreadListingAborted: TDictionary<DWORD, Boolean>; //Массив id потоков, для которых в операциях получения листинга была нажата отмена

	ThreadRetryCountDownload: TDictionary<DWORD, Int32>; //массив [id потока => количество попыток] для подсчёта количества повторов скачивания файла
	ThreadRetryCountUpload: TDictionary<DWORD, Int32>; //массив [id потока => количество попыток] для подсчёта количества повторов закачивания файла
	ThreadRetryCountRenMov: TDictionary<DWORD, Int32>; //массив [id потока => количество попыток] для подсчёта количества повторов межсерверных операций с файлом
	ThreadBackgroundJobs: TDictionary<WideString, Int32>; //массив [account root => количество потоков] для хранения количества текущих фоновых задач (предохраняемся от удаления объектов, которые могут быть использованы потоками)
	ThreadBackgroundThreads: TDictionary<DWORD, Int32>; //массив [id потока => статус операции] для хранения текущих фоновых потоков (предохраняемся от завершения работы плагина при закрытии TC)
	ThreadFsStatusInfo: TDictionary<DWORD, Int32>; //массив [id потока => текущая операция] для хранения контекста выполняемой операции (применяем для отлова перемещений каталогов)
	ThreadFsRemoveDirSkippedPath: TDictionary<DWORD, TStringList>; //массив [id потока => путь] для хранения путей, пропускаемых при перемещении (см. issue #168).

	{Callback data}
	PluginNum: integer;
	MyProgressProc: TProgressProcW;
	MyRequestProc: TRequestProcW;
	{Global stuff}

	CurrentListing: TCloudMailRuDirListing;
	CurrentIncomingInvitesListing: TCloudMailRuIncomingInviteInfoListing;
	ConnectionManager: TConnectionManager;
	HTTPManager: THTTPManager;
	CurrentDescriptions: TDescription;
	PasswordManager: TTCPasswordManager;
	TCLogger: TTCLogger;

function RequestHandle(RequestType: integer; CustomTitle, CustomText, ReturnedText: PWideChar; maxlen: integer; AOwner: TComponent = nil): Bool; stdcall;
begin
	Result := false;
	if Assigned(MyRequestProc) then
		Result := MyRequestProc(PluginNum, RequestType, CustomTitle, CustomText, ReturnedText, maxlen);

end;

function ProgressHandle(SourceName, TargetName: PWideChar; PercentDone: integer): integer; stdcall;
begin
	Result := 0;
	if Assigned(MyProgressProc) then
		Result := MyProgressProc(PluginNum, SourceName, TargetName, PercentDone);
end;

{Пытаемся найти объект в облаке по его пути, сначала в текущем списке, если нет - то ищем в облаке}
function FindListingItemByPath(CurrentListing: TCloudMailRuDirListing; path: TRealPath; UpdateListing: Boolean = true): TCloudMailRuDirListingItem;
var
	getResult: integer;
	CurrentCloud: TCloudMailRu;
begin
	if path.trashDir or path.sharedDir or (path.isDir = ID_Unset){or path.invitesDir} then
		{Виртуальные каталоги не имеют HomePath.}
		Result := GetItemByName(CurrentListing, path.path)
	else
		Result := GetItemByHomePath(CurrentListing, path.path); //сначала попробуем найти поле в имеющемся списке

	if (Result.name = EMPTY_STR) and UpdateListing then //если там его нет (нажали пробел на папке, например), то запросим в облаке напрямую, в зависимости от того, внутри чего мы находимся
	begin
		CurrentCloud := ConnectionManager.get(path.account, getResult);
		if not Assigned(CurrentCloud) then
			exit;

		if path.trashDir then //корзина - обновим CurrentListing, поищем в нём
		begin
			if CurrentCloud.getTrashbinListing(CurrentListing) then
				exit(GetItemByName(CurrentListing, path.path));
		end;
		if path.sharedDir then //ссылки - обновим список
		begin
			if CurrentCloud.getSharedLinksListing(CurrentListing) then
				exit(GetItemByName(CurrentListing, path.path));
		end;
		if path.invitesDir then
		begin
			//FindIncomingInviteItemByPath in that case!
		end;
		if CurrentCloud.statusFile(path.path, Result) then //Обычный каталог
		begin
			if (Result.home = EMPTY_STR) and not CurrentCloud.public_account then
				TCLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_WHERE_IS_THE_FILE, [path.path]); {Такого быть не может, но...}
		end;
	end; //Не рапортуем, это будет уровнем выше
end;

function FindIncomingInviteItemByPath(InviteListing: TCloudMailRuIncomingInviteInfoListing; path: TRealPath): TCloudMailRuIncomingInviteInfo;
var
	getResult: integer;

begin
	Result := CloudMailRuIncomingInviteInfoListing.FindByName(InviteListing, path.path);
	{item not found in current global listing, so refresh it}
	if Result.name = EMPTY_STR then
		if ConnectionManager.get(path.account, getResult).getIncomingLinksListing(CurrentIncomingInvitesListing) then
			exit(CloudMailRuIncomingInviteInfoListing.FindByName(CurrentIncomingInvitesListing, path.path));

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
			DeleteFailOnUploadModeAsked := messagebox(FindTCWindow, PWideChar(Format(ERR_DELETE_FILE_ASK, [LocalName])), ERR_DELETE_FILE, MB_ABORTRETRYIGNORE + MB_ICONQUESTION);
			case DeleteFailOnUploadModeAsked of
				IDRETRY:
					continue;
				IDABORT:
					DeleteFailOnUploadMode := DeleteFailOnUploadAbort;
				IDIGNORE:
					DeleteFailOnUploadMode := DeleteFailOnUploadIgnore;
			end;
		end;

		case DeleteFailOnUploadMode of
			DeleteFailOnUploadAbort:
				begin
					TCLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_IMPORTANTERROR, ERR_DELETE_FILE_ABORT, [LocalName]);
					exit(FS_FILE_NOTSUPPORTED);
				end;
			DeleteFailOnUploadDeleteIgnore, DeleteFailOnUploadDeleteAbort:
				begin
					//check if file just have RO attr, then remove it. If user has lack of rights, then ignore or abort
					if ((FileGetAttr(UNCLocalName) or faReadOnly) <> 0) and ((FileSetAttr(UNCLocalName, not faReadOnly) = 0) and (DeleteFileW(PWideChar(UNCLocalName)))) then
					begin
						TCLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_IMPORTANTERROR, ERR_DELETE_FILE_DELETE, [LocalName]);
						exit(FS_FILE_OK);
					end else begin
						if GetPluginSettings(SettingsIniFilePath).DeleteFailOnUploadMode = DeleteFailOnUploadDeleteIgnore then
						begin
							TCLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_IMPORTANTERROR, ERR_DELETE_FILE_IGNORE, [LocalName]);
							exit(FS_FILE_OK);
						end else begin
							TCLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_IMPORTANTERROR, ERR_DELETE_FILE_ABORT, [LocalName]);
							exit(FS_FILE_NOTSUPPORTED);
						end;
					end;
				end;
			else
				begin
					TCLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_IMPORTANTERROR, ERR_DELETE_FILE_IGNORE, [LocalName]);
				end;
		end;
	end;
end;

function FsGetBackgroundFlags: integer; stdcall;
begin
	if GetPluginSettings(SettingsIniFilePath).DisableMultiThreading then
		Result := 0
	else
		Result := BG_DOWNLOAD + BG_UPLOAD; //+ BG_ASK_USER;
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
	MyRequestProc := pRequestProc;
	Result := 0;
	TCLogger := TTCLogger.Create(pLogProc, PluginNr, GetPluginSettings(SettingsIniFilePath).LogLevel);
	CurrentDescriptions := TDescription.Create(GetTmpFileName('ion'), GetTCCommentPreferredFormat);

end;

procedure FsStatusInfoW(RemoteDir: PWideChar; InfoStartEnd, InfoOperation: integer); stdcall; //Начало и конец операций FS
var
	RealPath: TRealPath;
	getResult: integer;
	BackgroundJobsCount: integer;
begin
	RealPath := ExtractRealPath(RemoteDir, ID_Yes); // RemoteDir always a directory
	if (InfoStartEnd = FS_STATUS_START) then
	begin
		ThreadFsStatusInfo.AddOrSetValue(GetCurrentThreadID(), InfoOperation);
		case InfoOperation of
			FS_STATUS_OP_LIST:
				begin
					if (GetPluginSettings(SettingsIniFilePath).DescriptionEnabled) and inAccount(RealPath) then
					begin
						if ConnectionManager.get(RealPath.account, getResult).getDescriptionFile(IncludeTrailingBackslash(RealPath.path) + GetDescriptionFileName(SettingsIniFilePath), CurrentDescriptions.ionFilename) then
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
					if ConnectionManager.get(RealPath.account, getResult).public_account then
					begin
						TCLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, ERR_DIRECT_COPY_SUPPORT);
						ThreadSkipListRenMov.AddOrSetValue(GetCurrentThreadID, true);
					end;
					ThreadRetryCountRenMov.AddOrSetValue(GetCurrentThreadID(), 0);
					ThreadCanAbortRenMov.AddOrSetValue(GetCurrentThreadID, true);
					ThreadFsRemoveDirSkippedPath.AddOrSetValue(GetCurrentThreadID, TStringList.Create());
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
					if not ThreadBackgroundJobs.TryGetValue(RealPath.account, BackgroundJobsCount) then
						BackgroundJobsCount := 0;
					ThreadBackgroundJobs.AddOrSetValue(RealPath.account, BackgroundJobsCount + 1);
					ThreadBackgroundThreads.AddOrSetValue(GetCurrentThreadID(), FS_STATUS_OP_GET_MULTI_THREAD);
				end;
			FS_STATUS_OP_PUT_MULTI_THREAD:
				begin
					ThreadRetryCountUpload.AddOrSetValue(GetCurrentThreadID(), 0);
					if not ThreadBackgroundJobs.TryGetValue(RealPath.account, BackgroundJobsCount) then
						BackgroundJobsCount := 0;
					ThreadBackgroundJobs.AddOrSetValue(RealPath.account, BackgroundJobsCount + 1);
					ThreadBackgroundThreads.AddOrSetValue(GetCurrentThreadID(), FS_STATUS_OP_PUT_MULTI_THREAD);
				end;
		end;
		exit;
	end;
	if (InfoStartEnd = FS_STATUS_END) then
	begin
		ThreadFsStatusInfo.Remove(GetCurrentThreadID);
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
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then
						ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_PUT_MULTI:
				begin
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then
						ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_RENMOV_SINGLE:
				begin
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then
						ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_RENMOV_MULTI:
				begin
					ThreadSkipListRenMov.AddOrSetValue(GetCurrentThreadID, false);
					ThreadCanAbortRenMov.AddOrSetValue(GetCurrentThreadID, false);

					ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID].Free;
					ThreadFsRemoveDirSkippedPath.AddOrSetValue(GetCurrentThreadID, nil);

					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then
						ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_DELETE:
				begin
					ThreadSkipListDelete.AddOrSetValue(GetCurrentThreadID(), false);
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then
						ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
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
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then
						ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_SYNC_PUT:
				begin
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then
						ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_SYNC_DELETE:
				begin
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then
						ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_GET_MULTI_THREAD:
				begin
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then
						ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
					if not ThreadBackgroundJobs.TryGetValue(RealPath.account, BackgroundJobsCount) then
						BackgroundJobsCount := 0;
					ThreadBackgroundJobs.AddOrSetValue(RealPath.account, BackgroundJobsCount - 1);
					ThreadBackgroundThreads.Remove(GetCurrentThreadID());

				end;
			FS_STATUS_OP_PUT_MULTI_THREAD:
				begin
					if inAccount(RealPath) and GetPluginSettings(SettingsIniFilePath).LogUserSpace then
						ConnectionManager.get(RealPath.account, getResult).logUserSpaceInfo;
					if not ThreadBackgroundJobs.TryGetValue(RealPath.account, BackgroundJobsCount) then
						BackgroundJobsCount := 0;
					ThreadBackgroundJobs.AddOrSetValue(RealPath.account, BackgroundJobsCount - 1);
					ThreadBackgroundThreads.Remove(GetCurrentThreadID());
				end;
		end;
		exit;
	end;
end;

function FsFindFirstW(path: PWideChar; var FindData: tWIN32FINDDATAW): THandle; stdcall;
var //Получение первого файла в папке. Result тоталом не используется (можно использовать для работы плагина).
	RealPath: TRealPath;
	getResult: integer;
	SkipListDelete, SkipListRenMov, CanAbortRenMov, RenMovAborted: Boolean;
	CurrentItem: TCloudMailRuDirListingItem;
	CurrentCloud: TCloudMailRu;
begin
	ThreadSkipListDelete.TryGetValue(GetCurrentThreadID(), SkipListDelete);
	ThreadSkipListRenMov.TryGetValue(GetCurrentThreadID(), SkipListRenMov);

	ThreadCanAbortRenMov.TryGetValue(GetCurrentThreadID(), CanAbortRenMov);

	if (CanAbortRenMov and (ProgressHandle(path, nil, 0) = 1)) then
	begin
		ThreadListingAborted.AddOrSetValue(GetCurrentThreadID(), true);
		RenMovAborted := true;
	end
	else
		RenMovAborted := false;

	if SkipListDelete or SkipListRenMov or RenMovAborted then
	begin
		SetLastError(ERROR_NO_MORE_FILES);
		exit(INVALID_HANDLE_VALUE);
	end;

	//Result := FIND_NO_MORE_FILES;
	GlobalPath := path;
	if GlobalPath = '\' then
	begin //список соединений
		AccountsList := TStringList.Create;
		GetAccountsListFromIniFile(AccountsIniFilePath, AccountsList);

		if (AccountsList.Count > 0) then
		begin
			AddVirtualAccountsToAccountsList(AccountsIniFilePath, AccountsList, [GetPluginSettings(SettingsIniFilePath).ShowTrashFolders, GetPluginSettings(SettingsIniFilePath).ShowSharedFolders, GetPluginSettings(SettingsIniFilePath).ShowInvitesFolders]);

			FindData := GetFindDataEmptyDir(AccountsList.Strings[0]);
			FileCounter := 1;
			Result := FIND_ROOT_DIRECTORY;
		end else begin
			Result := INVALID_HANDLE_VALUE; //Нельзя использовать exit
			SetLastError(ERROR_NO_MORE_FILES);
		end;
	end else begin
		RealPath := ExtractRealPath(GlobalPath);
		CurrentCloud := ConnectionManager.get(RealPath.account, getResult);

		if getResult <> CLOUD_OPERATION_OK then
		begin
			SetLastError(ERROR_ACCESS_DENIED);
			exit(INVALID_HANDLE_VALUE);
		end;

		if not Assigned(CurrentCloud) then
		begin
			SetLastError(ERROR_PATH_NOT_FOUND);
			exit(INVALID_HANDLE_VALUE);
		end;

		if RealPath.trashDir then
		begin
			if not CurrentCloud.getTrashbinListing(CurrentListing) then
				SetLastError(ERROR_PATH_NOT_FOUND);
		end else if RealPath.sharedDir then
		begin
			if not CurrentCloud.getSharedLinksListing(CurrentListing) then
				SetLastError(ERROR_PATH_NOT_FOUND); //that will be interpreted as symlinks later
		end else if RealPath.invitesDir then
		begin
			if not CurrentCloud.getIncomingLinksListing(CurrentListing, CurrentIncomingInvitesListing) then
				SetLastError(ERROR_PATH_NOT_FOUND); //одновременно получаем оба листинга, чтобы не перечитывать листинг инватов на каждый чих
		end else begin //Нужно проверить, является ли открываемый объект каталогом - для файлов API вернёт листинг вышестоящего каталога, см. issue #174
			if not CurrentCloud.getDirListing(RealPath.path, CurrentListing) then
				SetLastError(ERROR_PATH_NOT_FOUND);
		end;

		if (RealPath.invitesDir or RealPath.trashDir or RealPath.sharedDir) and (RealPath.path <> EMPTY_STR) then //игнорим попытки получить листинги объектов вирутальных каталогов
		begin
			SetLastError(ERROR_ACCESS_DENIED);
			exit(INVALID_HANDLE_VALUE);
		end;

		if CurrentCloud.public_account then
			CurrentItem := GetItemByName(CurrentListing, ExtractUniversalFileName(RealPath.path))
		else
			CurrentItem := GetItemByHomePath(CurrentListing, RealPath.path);

		if (CurrentItem.name <> EMPTY_STR) and (CurrentItem.type_ <> TYPE_DIR) then
		begin
			SetLastError(ERROR_PATH_NOT_FOUND);
			exit(INVALID_HANDLE_VALUE);
		end;

		if (Length(CurrentListing) = 0) then
		begin
			FindData := GetFindDataEmptyDir(); //воркароунд бага с невозможностью входа в пустой каталог, см. http://www.ghisler.ch/board/viewtopic.php?t=42399
			Result := FIND_NO_MORE_FILES;
			SetLastError(ERROR_NO_MORE_FILES);
		end else begin
			FindData := CloudMailRuDirListingItemToFindData(CurrentListing[0], RealPath.sharedDir); //folders inside shared links directory must be displayed as symlinks
			FileCounter := 1;
			if RealPath.sharedDir then
				Result := FIND_SHARED_LINKS
			else
				Result := FIND_OK;
		end;
	end;
end;

function FsFindNextW(Hdl: THandle; var FindData: tWIN32FINDDATAW): Bool; stdcall;
begin
	if GlobalPath = '\' then
	begin
		if (AccountsList.Count > FileCounter) then
		begin
			FindData := GetFindDataEmptyDir(AccountsList.Strings[FileCounter]);
			inc(FileCounter);
			Result := true;
		end
		else
			Result := false;

	end else begin
		//Получение последующих файлов в папке (вызывается до тех пор, пока не вернёт false).
		if (Length(CurrentListing) > FileCounter) then
		begin
			FindData := CloudMailRuDirListingItemToFindData(CurrentListing[FileCounter], Hdl = FIND_SHARED_LINKS);
			Result := true;
			inc(FileCounter);
		end else begin
			FillChar(FindData, sizeof(WIN32_FIND_DATA), 0);
			FileCounter := 0;
			Result := false;
		end;
	end;
end;

function FsFindClose(Hdl: THandle): integer; stdcall;
begin //Завершение получения списка файлов. Result тоталом не используется (всегда равен 0)
	//SetLength(CurrentListing, 0); // Пусть будет
	if Hdl = FIND_ROOT_DIRECTORY then
		FreeAndNil(AccountsList);

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
	if RealPath.path = EMPTY_STR then //main trashbin folder properties
	begin
		if not Cloud.getTrashbinListing(CurrentListing) then
			exit(FS_EXEC_ERROR);
		getResult := TDeletedPropertyForm.ShowProperties(MainWin, CurrentListing, true, RealPath.account);
	end else begin //one item in trashbin
		CurrentItem := FindListingItemByPath(CurrentListing, RealPath); //для одинаково именованных файлов в корзине будут показываться свойства первого, сорян
		getResult := TDeletedPropertyForm.ShowProperties(MainWin, [CurrentItem]);
	end;
	case (getResult) of
		mrNo:
			if not Cloud.trashbinEmpty then
				exit(FS_EXEC_ERROR);
		mrYes:
			if not Cloud.trashbinRestore(CurrentItem.deleted_from + CurrentItem.name, CurrentItem.rev) then
				exit(FS_EXEC_ERROR);
		mrYesToAll:
			for CurrentItem in CurrentListing do
				if not Cloud.trashbinRestore(CurrentItem.deleted_from + CurrentItem.name, CurrentItem.rev) then
					exit(FS_EXEC_ERROR);
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
		if CurrentItem.type_ = TYPE_FILE then
			strpcopy(RemoteName, '\' + RealPath.account + ExtractFilePath(UrlToPath(CurrentItem.home)))
		else
			strpcopy(RemoteName, '\' + RealPath.account + UrlToPath(CurrentItem.home));
		Result := FS_EXEC_SYMLINK;
	end else begin
		if RealPath.path = EMPTY_STR then
			TAccountsForm.ShowAccounts(MainWin, AccountsIniFilePath, SettingsIniFilePath, PasswordManager, RealPath.account) //main shared folder properties - open connection settings
		else
		begin
			Cloud := ConnectionManager.get(RealPath.account, getResult);
			CurrentItem := FindListingItemByPath(CurrentListing, RealPath);
			if Cloud.statusFile(CurrentItem.home, CurrentItem) then
				TPropertyForm.ShowProperty(MainWin, RealPath.path, CurrentItem, Cloud, GetPluginSettings(SettingsIniFilePath).DownloadLinksEncode, GetPluginSettings(SettingsIniFilePath).AutoUpdateDownloadListing, false, false, GetDescriptionFileName(SettingsIniFilePath))
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
	if RealPath.path = EMPTY_STR then //main invites folder properties
	begin
		TAccountsForm.ShowAccounts(MainWin, AccountsIniFilePath, SettingsIniFilePath, PasswordManager, RealPath.account)
	end else begin //one invite item
		CurrentInvite := FindIncomingInviteItemByPath(CurrentIncomingInvitesListing, RealPath);
		if CurrentInvite.name = EMPTY_STR then
			exit(FS_EXEC_ERROR);

		getResult := TInvitePropertyForm.ShowProperties(MainWin, CurrentInvite);
	end;
	case (getResult) of
		mrAbort:
			Cloud.unmountFolder(CurrentInvite.name, true);
		mrClose:
			Cloud.unmountFolder(CurrentInvite.name, false);
		mrYes:
			Cloud.mountFolder(CurrentInvite.name, CurrentInvite.invite_token);
		mrNo:
			Cloud.rejectInvite(CurrentInvite.invite_token);

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
	if RealPath.path = EMPTY_STR then
		TAccountsForm.ShowAccounts(MainWin, AccountsIniFilePath, SettingsIniFilePath, PasswordManager, RealPath.account) //show account properties
	else
	begin
		Cloud := ConnectionManager.get(RealPath.account, getResult);
		//всегда нужно обновлять статус на сервере, CurrentListing может быть изменён в другой панели
		if (Cloud.statusFile(RealPath.path, CurrentItem)) and (idContinue = TPropertyForm.ShowProperty(MainWin, RealPath.path, CurrentItem, Cloud, GetPluginSettings(SettingsIniFilePath).DownloadLinksEncode, GetPluginSettings(SettingsIniFilePath).AutoUpdateDownloadListing, GetPluginSettings(SettingsIniFilePath).DescriptionEnabled, GetPluginSettings(SettingsIniFilePath).DescriptionEditorEnabled, GetDescriptionFileName(SettingsIniFilePath))) then
			PostMessage(MainWin, WM_USER + 51, 540, 0); //refresh tc panel if description edited
	end;
end;

function ExecCommand(RemoteName: PWideChar; command: WideString; Parameter: WideString = EMPTY_STR): integer;
var
	RealPath: TRealPath;
	getResult: integer;
	Cloud: TCloudMailRu;
	HashInfo: THashInfo;
begin
	Result := FS_EXEC_OK;

	if command = 'rmdir' then
	begin
		RealPath := ExtractRealPath(RemoteName + Parameter);
		if (ConnectionManager.get(RealPath.account, getResult).removeDir(RealPath.path) <> true) then
			exit(FS_EXEC_ERROR);
	end;

	RealPath := ExtractRealPath(RemoteName); //default
	Cloud := ConnectionManager.get(RealPath.account, getResult);

	//undocumented, share current folder to email param
	if command = 'share' then
		if not(Cloud.shareFolder(RealPath.path, ExtractLinkFromUrl(Parameter), CLOUD_SHARE_RW)) then
			exit(FS_EXEC_ERROR);

	if command = 'hash' then //add file by hash & filesize
	begin
		HashInfo := THashInfo.Create(Parameter);
		if HashInfo.valid then
		begin
			Cloud.addFileByIdentity(HashInfo.CloudFileIdentity, IncludeTrailingPathDelimiter(RealPath.path) + HashInfo.name, CLOUD_CONFLICT_RENAME);
			HashInfo.Destroy;
		end else begin
			TCLogger.Log(LOG_LEVEL_DEBUG, msgtype_details, ERR_CLONE_BY_HASH, [HashInfo.errorString, Parameter]);
			HashInfo.Destroy;
			exit(FS_EXEC_ERROR);
		end;
	end;

	if command = 'clone' then //add file by weblink
	begin
		if (Cloud.cloneWeblink(RealPath.path, ExtractLinkFromUrl(Parameter)) = CLOUD_OPERATION_OK) then
			if GetPluginSettings(SettingsIniFilePath).LogUserSpace then
				Cloud.logUserSpaceInfo
			else
				exit(FS_EXEC_ERROR);
	end;

	if command = 'trash' then //go to current account trash directory
	begin
		if Cloud.public_account then
			exit(FS_EXEC_ERROR);
		if inAccount(RealPath, false) then
		begin
			strpcopy(RemoteName, '\' + RealPath.account + TrashPostfix);
			exit(FS_EXEC_SYMLINK);
		end;
	end;

	if command = 'shared' then
	begin
		if Cloud.public_account then
			exit(FS_EXEC_ERROR);
		if inAccount(RealPath, false) then
		begin
			strpcopy(RemoteName, '\' + RealPath.account + SharedPostfix);
			exit(FS_EXEC_SYMLINK);
		end;
	end;

	if command = 'invites' then
	begin
		if Cloud.public_account then
			exit(FS_EXEC_ERROR);
		if inAccount(RealPath, false) then
		begin
			strpcopy(RemoteName, '\' + RealPath.account + InvitesPostfix);
			exit(FS_EXEC_SYMLINK);
		end;
	end;

end;

function ExecuteFileStream(RealPath: TRealPath; StreamingOptions: TStreamingOptions): integer;
var
	StreamUrl: WideString;
	getResult: integer;
	CurrentCloud, TempPublicCloud: TCloudMailRu;
	CurrentItem: TCloudMailRuDirListingItem;
begin
	//может быть разница в атрибутах настоящих и полученных из листинга (они не рефрешатся)
	CurrentItem := FindListingItemByPath(CurrentListing, RealPath); //внутри публичного облака веблинк есть автоматически
	case StreamingOptions.Format of
		STREAMING_FORMAT_DISABLED:
			exit(FS_EXEC_OK);
		STREAMING_FORMAT_PLAYLIST:
			if not TempPublicCloud.getPublishedFileStreamUrl(CurrentItem, StreamUrl) then
				exit(FS_EXEC_ERROR);
		else
			begin
				if EmptyWideStr = CurrentItem.weblink then
				begin
					CurrentCloud := ConnectionManager.get(RealPath.account, getResult);
					if not CurrentCloud.publishFile(CurrentItem.home, CurrentItem.weblink) then
						exit(FS_EXEC_ERROR);
					//Здесь можно бы обновить листинг
				end;
				TCloudMailRu.TempPublicCloudInit(TempPublicCloud, PUBLIC_ACCESS_URL + CurrentItem.weblink);
				StreamUrl := TempPublicCloud.getSharedFileUrl(EmptyWideStr, ShardTypeFromStreamingFormat(StreamingOptions.Format));
			end;
	end;
	if EmptyWideStr = StreamingOptions.Parameters then
		StreamingOptions.Parameters := '%url%';
	StreamingOptions.Parameters := StringReplace(StreamingOptions.Parameters, '%url%', StreamUrl, [rfReplaceAll, rfIgnoreCase]);

	if (Run(StreamingOptions.command, StreamUrl, StreamingOptions.StartPath)) then
		Result := FS_EXEC_OK
	else
		Result := FS_EXEC_ERROR;
	TempPublicCloud.Free;
end;

function FsExecuteFileW(MainWin: THandle; RemoteName, Verb: PWideChar): integer; stdcall; //Запуск файла
var
	RealPath: TRealPath;
	StreamingOptions: TStreamingOptions;
begin
	RealPath := ExtractRealPath(RemoteName);

	if RealPath.upDirItem then
		RealPath.path := ExtractFilePath(RealPath.path); //if somepath/.. item properties called

	if RealPath.trashDir and ((Verb = 'open') or (Verb = 'properties')) then
		exit(ExecTrashbinProperties(MainWin, RealPath));

	if RealPath.sharedDir then
		exit(ExecSharedAction(MainWin, RealPath, RemoteName, Verb = 'open'));

	if RealPath.invitesDir then
		exit(ExecInvitesAction(MainWin, RealPath));

	if Verb = 'properties' then
		exit(ExecProperties(MainWin, RealPath));

	if Verb = 'open' then
	begin
		if (not(RealPath.isDir = ID_Yes)) and GetStreamingOptionsFromIniFile(SettingsIniFilePath, RealPath.path, StreamingOptions) and (STREAMING_FORMAT_NONE <> StreamingOptions.Format) then
			exit(ExecuteFileStream(RealPath, StreamingOptions));
		exit(FS_EXEC_YOURSELF);
	end;

	if copy(Verb, 1, 5) = 'quote' then
		exit(ExecCommand(RemoteName, LowerCase(GetWord(Verb, 1)), GetWord(Verb, 2)));

	//if copy(Verb, 1, 5) = 'chmod' then exit; //future usage
	exit(FS_EXEC_OK)

end;

procedure UpdateFileDescription(RemotePath: TRealPath; LocalFilePath: WideString; var Cloud: TCloudMailRu);
var
	RemoteDescriptions, LocalDescriptions: TDescription;
	RemoteIonPath, LocalTempPath: WideString;
	RemoteIonExists: Boolean;
begin
	RemoteIonPath := IncludeTrailingBackslash(ExtractFileDir(RemotePath.path)) + GetDescriptionFileName(SettingsIniFilePath);
	LocalTempPath := GetTmpFileName('ion');

	RemoteIonExists := Cloud.getDescriptionFile(RemoteIonPath, LocalTempPath);
	if not RemoteIonExists then
		exit; //удалённого файла описаний нет

	RemoteDescriptions := TDescription.Create(LocalTempPath, GetTCCommentPreferredFormat);
	RemoteDescriptions.Read;
	LocalDescriptions := TDescription.Create(IncludeTrailingPathDelimiter(ExtractFileDir(LocalFilePath)) + GetDescriptionFileName(SettingsIniFilePath), GetTCCommentPreferredFormat); //open local ion file
	LocalDescriptions.Read;
	LocalDescriptions.CopyFrom(RemoteDescriptions, ExtractFileName(LocalFilePath));
	LocalDescriptions.Write();
	LocalDescriptions.Destroy;
	RemoteDescriptions.Destroy
end;

procedure UpdateRemoteFileDescription(RemotePath: TRealPath; LocalFilePath: WideString; var Cloud: TCloudMailRu);
var
	RemoteDescriptions, LocalDescriptions: TDescription;
	RemoteIonPath, LocalIonPath, LocalTempPath: WideString;
	RemoteIonExists: Boolean;
begin
	RemoteIonPath := IncludeTrailingBackslash(ExtractFileDir(RemotePath.path)) + GetDescriptionFileName(SettingsIniFilePath);
	LocalIonPath := IncludeTrailingBackslash(ExtractFileDir(LocalFilePath)) + GetDescriptionFileName(SettingsIniFilePath);
	LocalTempPath := GetTmpFileName('ion');

	if (not FileExists(GetUNCFilePath(LocalIonPath))) then
		exit; //Файла описаний нет, не паримся

	LocalDescriptions := TDescription.Create(LocalIonPath, GetTCCommentPreferredFormat);
	LocalDescriptions.Read;

	RemoteIonExists := Cloud.getDescriptionFile(RemoteIonPath, LocalTempPath);
	RemoteDescriptions := TDescription.Create(LocalTempPath, GetTCCommentPreferredFormat);
	if RemoteIonExists then
		RemoteDescriptions.Read; //если был прежний файл - его надо перечитать

	RemoteDescriptions.CopyFrom(LocalDescriptions, ExtractFileName(RemotePath.path));
	RemoteDescriptions.Write();
	if RemoteIonExists then
		Cloud.deleteFile(RemoteIonPath); //Приходится удалять, потому что не знаем, как переписать

	Cloud.putDesriptionFile(RemoteIonPath, RemoteDescriptions.ionFilename);

	RemoteDescriptions.Destroy;
	LocalDescriptions.Destroy;
end;

//Предполагается, что процедура происходит внутри одного облака - в плагине запрещены прямые операции между аккаунтами
procedure RenameRemoteFileDescription(OldRemotePath, NewRemotePath: TRealPath; var Cloud: TCloudMailRu);
var
	OldDescriptions, NewDescriptions: TDescription;
	OldRemoteIonPath, NewRemoteIonPath, OldLocalTempPath, NewLocalTempPath: WideString;
	NewRemoteIonExists: Boolean;
	OldItem, NewItem: WideString;
begin
	OldItem := ExtractFileName(OldRemotePath.path);
	NewItem := ExtractFileName(NewRemotePath.path);
	OldRemoteIonPath := IncludeTrailingBackslash(ExtractFileDir(OldRemotePath.path)) + GetDescriptionFileName(SettingsIniFilePath);
	NewRemoteIonPath := IncludeTrailingBackslash(ExtractFileDir(NewRemotePath.path)) + GetDescriptionFileName(SettingsIniFilePath);
	OldLocalTempPath := GetTmpFileName('ion');
	NewLocalTempPath := GetTmpFileName('ion');

	if ExtractFileDir(OldRemotePath.path) = ExtractFileDir(NewRemotePath.path) then //переименование внутри одного файла
	begin
		if not Cloud.getDescriptionFile(OldRemoteIonPath, OldLocalTempPath) then
			exit; //описания нет, переносить нечего
		OldDescriptions := TDescription.Create(OldLocalTempPath, GetTCCommentPreferredFormat);
		OldDescriptions.Read;
		if (OldDescriptions.RenameItem(OldItem, NewItem)) then //метод сам проверит существование описания
		begin
			OldDescriptions.Write();
			Cloud.deleteFile(OldRemoteIonPath);
			Cloud.putDesriptionFile(OldRemoteIonPath, OldDescriptions.ionFilename);
		end;
		OldDescriptions.Destroy;
	end
	else //перенос и переименование в разных файлах (например, перемещение в подкаталог)
	begin
		if not Cloud.getDescriptionFile(OldRemoteIonPath, OldLocalTempPath) then
			exit; //описания нет, не заморачиваемся
		OldDescriptions := TDescription.Create(OldLocalTempPath, GetTCCommentPreferredFormat);
		OldDescriptions.Read;
		NewRemoteIonExists := Cloud.getDescriptionFile(NewRemoteIonPath, NewLocalTempPath);
		NewDescriptions := TDescription.Create(NewLocalTempPath, GetTCCommentPreferredFormat);
		if NewRemoteIonExists then
			NewDescriptions.Read; //прочитать существующий, если его нет - то и читать нечего

		NewDescriptions.SetValue(ExtractFileName(NewRemotePath.path), OldDescriptions.GetValue(ExtractFileName(OldRemotePath.path)));
		OldDescriptions.DeleteValue(ExtractFileName(OldRemotePath.path));
		OldDescriptions.Write();
		NewDescriptions.Write();
		Cloud.deleteFile(OldRemoteIonPath);
		Cloud.putDesriptionFile(OldRemoteIonPath, OldDescriptions.ionFilename);
		if NewRemoteIonExists then
			Cloud.deleteFile(NewRemoteIonPath); //Если файл существовал ранее, его нужно удалить для последующей записи на его место
		Cloud.putDesriptionFile(NewRemoteIonPath, NewDescriptions.ionFilename);
		OldDescriptions.Destroy;
		NewDescriptions.Destroy;
	end;

end;

procedure DeleteRemoteFileDescription(RemotePath: TRealPath; var Cloud: TCloudMailRu);
var
	RemoteDescriptions: TDescription;
	RemoteIonPath, LocalTempPath: WideString;
begin
	RemoteIonPath := IncludeTrailingBackslash(ExtractFileDir(RemotePath.path)) + GetDescriptionFileName(SettingsIniFilePath);
	LocalTempPath := GetTmpFileName('ion');
	if not Cloud.getDescriptionFile(RemoteIonPath, LocalTempPath) then
		exit; //описания нет, не заморачиваемся
	RemoteDescriptions := TDescription.Create(LocalTempPath, GetTCCommentPreferredFormat);
	RemoteDescriptions.Read;
	RemoteDescriptions.DeleteValue(ExtractFileName(RemotePath.path));
	RemoteDescriptions.Write();
	Cloud.deleteFile(RemoteIonPath); //Приходится удалять, потому что не знаем, как переписать
	Cloud.putDesriptionFile(RemoteIonPath, RemoteDescriptions.ionFilename);
	RemoteDescriptions.Destroy;
end;

function GetRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: integer): integer;
var
	getResult: integer;
	Item: TCloudMailRuDirListingItem;
	Cloud: TCloudMailRu;
	AccountSettings: TAccountSettings;
	resultHash: WideString;
begin
	if (GetPluginSettings(SettingsIniFilePath).CheckCRC) then
		resultHash := EmptyWideStr
	else
		resultHash := 'dummy'; //calculations will be ignored if variable is not empty
	Cloud := ConnectionManager.get(RemotePath.account, getResult);
	AccountSettings := GetAccountSettingsFromIniFile(AccountsIniFilePath, RemotePath.account);

	Result := Cloud.getFile(WideString(RemotePath.path), LocalName, resultHash);

	if Result = FS_FILE_OK then
	begin

		Item := FindListingItemByPath(CurrentListing, RemotePath);
		{Дополнительно проверим CRC скачанного файла}
		if GetPluginSettings(SettingsIniFilePath).CheckCRC then
		begin
			if (resultHash <> EmptyWideStr) and (Item.hash <> resultHash) then
				exit(FS_FILE_READERROR);
		end;

		if GetPluginSettings(SettingsIniFilePath).PreserveFileTime then
		begin
			if Item.mtime <> 0 then
				SetAllFileTime(ExpandUNCFileName(LocalName), DateTimeToFileTime(UnixToDateTime(Item.mtime)));
		end;
		if CheckFlag(FS_COPYFLAGS_MOVE, CopyFlags) then
		begin
			Cloud.deleteFile(RemotePath.path);
			if (GetPluginSettings(SettingsIniFilePath).DescriptionTrackCloudFS and RemoteDescriptionsSupportEnabled(GetAccountSettingsFromIniFile(AccountsIniFilePath, RemotePath.account))) then
				DeleteRemoteFileDescription(RemotePath, Cloud);
		end;
		ProgressHandle(PWideChar(LocalName), PWideChar(RemoteName), 100);
		TCLogger.Log(LOG_LEVEL_FILE_OPERATION, MSGTYPE_TRANSFERCOMPLETE, '%s -> %s', [RemoteName, LocalName]);

		if GetPluginSettings(SettingsIniFilePath).DescriptionCopyFromCloud then
			UpdateFileDescription(RemotePath, LocalName, Cloud);

	end;
end;

function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall; //Копирование файла из файловой системы плагина
var
	RealPath: TRealPath;
	OverwriteLocalMode: integer;
	RetryAttempts: integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if CheckFlag(FS_COPYFLAGS_RESUME, CopyFlags) then
		exit; {NEVER CALLED HERE}
	RealPath := ExtractRealPath(RemoteName);
	if RealPath.trashDir or RealPath.sharedDir or RealPath.invitesDir then
		exit;

	ProgressHandle(RemoteName, LocalName, 0);

	OverwriteLocalMode := GetPluginSettings(SettingsIniFilePath).OverwriteLocalMode;
	if (FileExists(GetUNCFilePath(LocalName)) and not(CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags))) then
	begin
		case OverwriteLocalMode of
			OverwriteLocalModeAsk:
				exit(FS_FILE_EXISTS); //TC will ask user
			OverwriteLocalModeIgnore:
				begin
					TCLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, FILE_EXISTS_IGNORE, [LocalName]);
					exit(FS_FILE_OK);
				end;
			OverwriteLocalModeOverwrite:
				TCLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, FILE_EXISTS_OVERWRITE, [LocalName]);
		end;
	end;

	Result := GetRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);

	if Result <> FS_FILE_READERROR then
		exit;

	case GetPluginSettings(SettingsIniFilePath).OperationErrorMode of
		OperationErrorModeAsk:
			begin
				while (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					case (messagebox(FindTCWindow, PWideChar(Format(ERR_DOWNLOAD_FILE_ASK, [RemoteName])), ERR_DOWNLOAD, MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
						ID_ABORT:
							Result := FS_FILE_USERABORT;
						ID_RETRY:
							Result := GetRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
						ID_IGNORE:
							break;
					end;
				end;

			end;
		OperationErrorModeIgnore:
			exit;
		OperationErrorModeAbort:
			exit(FS_FILE_USERABORT);
		OperationErrorModeRetry:
			begin;
				RetryAttempts := GetPluginSettings(SettingsIniFilePath).RetryAttempts;
				while (ThreadRetryCountDownload.Items[GetCurrentThreadID()] <> RetryAttempts) and (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					ThreadRetryCountDownload.Items[GetCurrentThreadID()] := ThreadRetryCountDownload.Items[GetCurrentThreadID()] + 1;
					TCLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, DOWNLOAD_FILE_RETRY, [RemoteName, ThreadRetryCountDownload.Items[GetCurrentThreadID()], RetryAttempts]);
					Result := GetRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
					if ProgressHandle(PWideChar(LocalName), RemoteName, 0) = 1 then
						Result := FS_FILE_USERABORT;
					if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then
						ThreadRetryCountDownload.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
					ProcessMessages;
					Sleep(GetPluginSettings(SettingsIniFilePath).AttemptWait);
				end;
			end;
	end;

end;

function PutRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: integer): integer;
var
	getResult: integer;
	Cloud: TCloudMailRu;
	AccountSettings: TAccountSettings;
begin
	Cloud := ConnectionManager.get(RemotePath.account, getResult);
	AccountSettings := GetAccountSettingsFromIniFile(AccountsIniFilePath, RemotePath.account);

	Result := Cloud.putFile(WideString(LocalName), RemotePath.path);
	if Result = FS_FILE_OK then
	begin
		ProgressHandle(PWideChar(LocalName), PWideChar(RemotePath.path), 100);
		TCLogger.Log(LOG_LEVEL_FILE_OPERATION, MSGTYPE_TRANSFERCOMPLETE, '%s -> %s', [LocalName, RemoteName]);
		if CheckFlag(FS_COPYFLAGS_MOVE, CopyFlags) then
			Result := DeleteLocalFile(LocalName);
		if (GetPluginSettings(SettingsIniFilePath).DescriptionCopyToCloud and RemoteDescriptionsSupportEnabled(AccountSettings)) then
			UpdateRemoteFileDescription(RemotePath, LocalName, Cloud);
	end;

end;

function FsPutFileW(LocalName, RemoteName: PWideChar; CopyFlags: integer): integer; stdcall;
var
	RealPath: TRealPath;
	RetryAttempts: integer;
	getResult: integer;
begin

	RealPath := ExtractRealPath(RemoteName);
	if not FileExists(GetUNCFilePath(LocalName)) then
		exit(FS_FILE_NOTFOUND);

	if (RealPath.account = EMPTY_STR) or RealPath.trashDir or RealPath.sharedDir or RealPath.invitesDir then
		exit(FS_FILE_NOTSUPPORTED);
	ProgressHandle(LocalName, PWideChar(RealPath.path), 0);

	if CheckFlag(FS_COPYFLAGS_RESUME, CopyFlags) then
		exit(FS_FILE_NOTSUPPORTED); //NOT SUPPORTED

	if (CheckFlag(FS_COPYFLAGS_EXISTS_SAMECASE, CopyFlags) or CheckFlag(FS_COPYFLAGS_EXISTS_DIFFERENTCASE, CopyFlags)) and not(CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags)) then
		exit(FS_FILE_EXISTS); //Облако не поддерживает разные регистры

	if CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags) then
	begin
		if not(ConnectionManager.get(RealPath.account, getResult).deleteFile(RealPath.path)) then
			exit(FS_FILE_NOTSUPPORTED); //Неизвестно, как перезаписать файл черз API, но мы можем его удалить
	end;
	Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);

	//if Result in [FS_FILE_OK, FS_FILE_USERABORT, FS_FILE_NOTSUPPORTED] then exit;
	if Result <> FS_FILE_WRITEERROR then
		exit;

	case GetPluginSettings(SettingsIniFilePath).OperationErrorMode of
		OperationErrorModeAsk:
			begin
				while (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					case (messagebox(FindTCWindow, PWideChar(Format(ERR_UPLOAD_FILE_ASK, [LocalName])), ERR_UPLOAD, MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
						ID_ABORT:
							Result := FS_FILE_USERABORT;
						ID_RETRY:
							Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
						ID_IGNORE:
							break;
					end;
				end;

			end;
		OperationErrorModeIgnore:
			exit;
		OperationErrorModeAbort:
			exit(FS_FILE_USERABORT);
		OperationErrorModeRetry:
			begin;
				RetryAttempts := GetPluginSettings(SettingsIniFilePath).RetryAttempts;
				while (ThreadRetryCountUpload.Items[GetCurrentThreadID()] <> RetryAttempts) and (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					ThreadRetryCountUpload.Items[GetCurrentThreadID()] := ThreadRetryCountUpload.Items[GetCurrentThreadID()] + 1;
					TCLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, UPLOAD_FILE_RETRY, [LocalName, ThreadRetryCountUpload.Items[GetCurrentThreadID()], RetryAttempts]);
					Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
					if ProgressHandle(PWideChar(LocalName), RemoteName, 0) = 1 then
						Result := FS_FILE_USERABORT;
					if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then
						ThreadRetryCountUpload.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
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
	if (RealPath.account = EMPTY_STR) or RealPath.trashDir or RealPath.invitesDir then
		exit(false);
	Cloud := ConnectionManager.get(RealPath.account, getResult);
	if RealPath.sharedDir then
	begin
		CurrentItem := FindListingItemByPath(CurrentListing, RealPath);
		Cloud.getShareInfo(CurrentItem.home, InvitesListing);
		for Invite in InvitesListing do
			Cloud.shareFolder(CurrentItem.home, Invite.email, CLOUD_SHARE_NO); //no reporting here
		if (CurrentItem.weblink <> EMPTY_STR) then
			Cloud.publishFile(CurrentItem.home, CurrentItem.weblink, CLOUD_UNPUBLISH);
		Result := true;
	end
	else
		Result := Cloud.deleteFile(RealPath.path);
	if (Result and GetPluginSettings(SettingsIniFilePath).DescriptionTrackCloudFS and RemoteDescriptionsSupportEnabled(GetAccountSettingsFromIniFile(AccountsIniFilePath, RealPath.account))) then
		DeleteRemoteFileDescription(RealPath, Cloud);

end;

function FsMkDirW(path: PWideChar): Bool; stdcall;
var
	RealPath: TRealPath;
	getResult: integer;
	SkipListRenMov: Boolean;
	OperationContextId: integer;
	account: TAccountSettings;
begin
	ThreadSkipListRenMov.TryGetValue(GetCurrentThreadID(), SkipListRenMov);
	if SkipListRenMov then
		exit(false); //skip create directory if this flag set on

	RealPath := ExtractRealPath(WideString(path));
	if (RealPath.path = EmptyWideStr) then //accounts list
	begin
		account.user := RealPath.account;
		Result := (mrOk = TRegistrationForm.ShowRegistration(FindTCWindow, GetPluginSettings(SettingsIniFilePath).ConnectionSettings, account));
		if Result then
		begin
			if account.use_tc_password_manager then //просим TC сохранить пароль
				Result := FS_FILE_OK = PasswordManager.SetPassword(account.name, account.password);
			if Result then
				Result := SetAccountSettingsToIniFile(account, AccountsIniFilePath);
		end;
		exit();
	end;
	if (RealPath.account = EmptyWideStr) or RealPath.trashDir or RealPath.sharedDir or RealPath.invitesDir then
		exit(false);

	Result := ConnectionManager.get(RealPath.account, getResult).createDir(RealPath.path);
	if Result then //need to check operation context => directory can be moved
	begin
		ThreadFsStatusInfo.TryGetValue(GetCurrentThreadID, OperationContextId);
		if OperationContextId = FS_STATUS_OP_RENMOV_MULTI then
			CurrentlyMovedDir := RealPath;
	end;

end;

function FsRemoveDirW(RemoteName: PWideChar): Bool; stdcall;
var
	RealPath: TRealPath;
	getResult: integer;
	ListingAborted: Boolean;
	Cloud: TCloudMailRu;
	OperationContextId: integer;
begin
	if (ThreadFsRemoveDirSkippedPath.ContainsKey(GetCurrentThreadID) and Assigned(ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID]) and ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID].Text.Contains(RemoteName)) then //файлы по удаляемому пути есть в блек-листе
		exit(false);
	ThreadListingAborted.TryGetValue(GetCurrentThreadID(), ListingAborted);
	if ListingAborted then
	begin
		ThreadListingAborted.AddOrSetValue(GetCurrentThreadID(), false);
		exit(false);
	end;
	RealPath := ExtractRealPath(WideString(RemoteName));
	if RealPath.trashDir or RealPath.sharedDir or RealPath.invitesDir then
		exit(false);
	Cloud := ConnectionManager.get(RealPath.account, getResult);
	Result := Cloud.removeDir(RealPath.path);

	if (Result and GetPluginSettings(SettingsIniFilePath).DescriptionTrackCloudFS and RemoteDescriptionsSupportEnabled(GetAccountSettingsFromIniFile(AccountsIniFilePath, RealPath.account))) then
	begin
		ThreadFsStatusInfo.TryGetValue(GetCurrentThreadID, OperationContextId); //need to check operation context => directory can be deleted after moving operation
		if OperationContextId = FS_STATUS_OP_RENMOV_MULTI then
		begin
			RenameRemoteFileDescription(RealPath, CurrentlyMovedDir, Cloud);
		end
		else
			DeleteRemoteFileDescription(RealPath, Cloud);
	end;

end;

function cloneWeblink(NewCloud, OldCloud: TCloudMailRu; CloudPath: WideString; CurrentItem: TCloudMailRuDirListingItem; NeedUnpublish: Boolean): integer;
begin
	Result := NewCloud.cloneWeblink(ExtractFileDir(CloudPath), CurrentItem.weblink, CLOUD_CONFLICT_STRICT);
	if (NeedUnpublish) and (FS_FILE_USERABORT <> Result) and not(OldCloud.publishFile(CurrentItem.home, CurrentItem.weblink, CLOUD_UNPUBLISH)) then
		TCLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, PREFIX_ERR_REMOVE_TEMP_PUBLIC_LINK + CurrentItem.home);
end;

function RenMoveFileViaHash(OldCloud, NewCloud: TCloudMailRu; OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean): integer;
var
	CurrentItem: TCloudMailRuDirListingItem;
	RetryAttempts: integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if OverWrite and not(NewCloud.deleteFile(NewRealPath.path)) then
		exit;
	if OldCloud.statusFile(OldRealPath.path, CurrentItem) then
	begin
		Result := NewCloud.addFileByIdentity(CurrentItem, IncludeTrailingPathDelimiter(ExtractFileDir(NewRealPath.path)) + ExtractFileName(NewRealPath.path));
		if not(Result in [FS_FILE_OK, FS_FILE_EXISTS]) then
		begin

			case GetPluginSettings(SettingsIniFilePath).OperationErrorMode of
				OperationErrorModeAsk:
					begin
						while (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
						begin
							case (messagebox(FindTCWindow, PWideChar(Format(ERR_CLONE_FILE_ASK, [TCloudMailRu.ErrorCodeText(Result)])), ERR_OPERATION, MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
								ID_ABORT:
									Result := FS_FILE_USERABORT;
								ID_RETRY:
									Result := NewCloud.addFileByIdentity(CurrentItem, IncludeTrailingPathDelimiter(ExtractFileDir(NewRealPath.path)) + CurrentItem.name);
								ID_IGNORE:
									break;
							end;
						end;
					end;
				OperationErrorModeIgnore:
					exit;
				OperationErrorModeAbort:
					exit(FS_FILE_USERABORT);
				OperationErrorModeRetry:
					begin;
						RetryAttempts := GetPluginSettings(SettingsIniFilePath).RetryAttempts;
						while (ThreadRetryCountRenMov.Items[GetCurrentThreadID()] <> RetryAttempts) and (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
						begin
							ThreadRetryCountRenMov.Items[GetCurrentThreadID()] := ThreadRetryCountRenMov.Items[GetCurrentThreadID()] + 1;
							TCLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, CLONE_FILE_RETRY, [TCloudMailRu.ErrorCodeText(Result), ThreadRetryCountRenMov.Items[GetCurrentThreadID()], RetryAttempts]);
							Result := NewCloud.addFileByIdentity(CurrentItem, IncludeTrailingPathDelimiter(ExtractFileDir(NewRealPath.path)) + ExtractFileName(NewRealPath.path));
							if ProgressHandle(nil, nil, 0) = 1 then
								Result := FS_FILE_USERABORT;
							if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then
								ThreadRetryCountRenMov.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
							ProcessMessages;
							Sleep(GetPluginSettings(SettingsIniFilePath).AttemptWait);
						end;
					end;
			end;
		end;

		if (Result = CLOUD_OPERATION_OK) and Move and not(OldCloud.deleteFile(OldRealPath.path)) then
			TCLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_DELETE, [CurrentItem.home]); //пишем в лог, но не отваливаемся
	end;
end;

function RenMoveFileViaPublicLink(OldCloud, NewCloud: TCloudMailRu; OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean): integer;
var
	NeedUnpublish: Boolean;
	CurrentItem: TCloudMailRuDirListingItem;
	RetryAttempts: integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	NeedUnpublish := false;
	if OverWrite and not(NewCloud.deleteFile(NewRealPath.path)) then
		exit;

	if OldCloud.statusFile(OldRealPath.path, CurrentItem) then
	begin
		if CurrentItem.weblink = EMPTY_STR then //create temporary weblink
		begin
			NeedUnpublish := true;
			if not(OldCloud.publishFile(CurrentItem.home, CurrentItem.weblink)) then //problem publishing
			begin
				TCLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_GET_TEMP_PUBLIC_LINK, [CurrentItem.home]);
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
							case (messagebox(FindTCWindow, PWideChar(Format(ERR_PUBLISH_FILE_ASK, [TCloudMailRu.ErrorCodeText(Result)])), ERR_PUBLISH_FILE, MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
								ID_ABORT:
									Result := FS_FILE_USERABORT;
								ID_RETRY:
									Result := cloneWeblink(NewCloud, OldCloud, NewRealPath.path, CurrentItem, NeedUnpublish);
								ID_IGNORE:
									break;
							end;
						end;

					end;
				OperationErrorModeIgnore:
					exit;
				OperationErrorModeAbort:
					exit(FS_FILE_USERABORT);
				OperationErrorModeRetry:
					begin;
						RetryAttempts := GetPluginSettings(SettingsIniFilePath).RetryAttempts;
						while (ThreadRetryCountRenMov.Items[GetCurrentThreadID()] <> RetryAttempts) and (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
						begin
							ThreadRetryCountRenMov.Items[GetCurrentThreadID()] := ThreadRetryCountRenMov.Items[GetCurrentThreadID()] + 1;
							TCLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, PUBLISH_FILE_RETRY, [TCloudMailRu.ErrorCodeText(Result), ThreadRetryCountRenMov.Items[GetCurrentThreadID()], RetryAttempts]);
							Result := cloneWeblink(NewCloud, OldCloud, NewRealPath.path, CurrentItem, NeedUnpublish);
							if ProgressHandle(nil, nil, 0) = 1 then
								Result := FS_FILE_USERABORT;
							if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then
								ThreadRetryCountRenMov.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
							ProcessMessages;
							Sleep(GetPluginSettings(SettingsIniFilePath).AttemptWait);
						end;
					end;
			end;
		end;

		if (Result = CLOUD_OPERATION_OK) and Move and not(OldCloud.deleteFile(OldRealPath.path)) then
			TCLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_DELETE, [CurrentItem.home]); //пишем в лог, но не отваливаемся
	end;
end;

function FsRenMovFileW(OldName: PWideChar; NewName: PWideChar; Move: Boolean; OverWrite: Boolean; ri: pRemoteInfo): integer; stdcall;
var
	OldRealPath: TRealPath;
	NewRealPath: TRealPath;
	getResult, SkippedFoundIndex: integer;
	OldCloud, NewCloud: TCloudMailRu;
begin
	ProgressHandle(OldName, NewName, 0);

	OldRealPath := ExtractRealPath(WideString(OldName));
	NewRealPath := ExtractRealPath(WideString(NewName));

	if OldRealPath.trashDir or NewRealPath.trashDir or OldRealPath.sharedDir or NewRealPath.sharedDir then
		exit(FS_FILE_NOTSUPPORTED);

	OldCloud := ConnectionManager.get(OldRealPath.account, getResult);
	NewCloud := ConnectionManager.get(NewRealPath.account, getResult);

	if OldRealPath.account <> NewRealPath.account then //разные аккаунты
	begin
		if OldCloud.public_account then
		begin
			TCLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, ERR_DIRECT_OPERATIONS_NOT_SUPPORTED);
			exit(FS_FILE_USERABORT);
		end;

		case GetPluginSettings(SettingsIniFilePath).CopyBetweenAccountsMode of
			CopyBetweenAccountsModeDisabled:
				begin
					TCLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, ERR_DIRECT_OPERATIONS_DISABLED);
					exit(FS_FILE_USERABORT);
				end;
			CopyBetweenAccountsModeViaHash:
				Result := RenMoveFileViaHash(OldCloud, NewCloud, OldRealPath, NewRealPath, Move, OverWrite);
			CopyBetweenAccountsModeViaPublicLink:
				Result := RenMoveFileViaPublicLink(OldCloud, NewCloud, OldRealPath, NewRealPath, Move, OverWrite);
			else
				exit(FS_FILE_WRITEERROR);
		end;

	end else begin //один аккаунт

		if OverWrite and not(NewCloud.deleteFile(NewRealPath.path)) then
			exit(FS_FILE_NOTSUPPORTED); //мы не умеем перезаписывать, но мы можем удалить существующий файл
		if Move then
		begin
			Result := OldCloud.mvFile(OldRealPath.path, NewRealPath.path);
			if (FS_FILE_EXISTS = Result) and (ThreadFsRemoveDirSkippedPath.ContainsKey(GetCurrentThreadID)) then //TC сразу же попытается удалить каталог, чтобы избежать этого - внесем путь в своеобразный блеклист
			begin
				ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID].Add(ExtractVirtualPath(OldRealPath));
			end else if (FS_FILE_OK = Result) and (ThreadFsRemoveDirSkippedPath.ContainsKey(GetCurrentThreadID)) then
			begin //Вытащим из блеклиста, если решили перезаписать

				if Assigned(ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID]) then
				begin
					SkippedFoundIndex := ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID].IndexOf(ExtractVirtualPath(OldRealPath));
					if (-1 <> SkippedFoundIndex) then
						ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID].Delete(SkippedFoundIndex);
				end;

			end;
			if ((FS_FILE_OK = Result) and GetPluginSettings(SettingsIniFilePath).DescriptionTrackCloudFS and RemoteDescriptionsSupportEnabled(GetAccountSettingsFromIniFile(AccountsIniFilePath, NewRealPath.account))) then
				RenameRemoteFileDescription(OldRealPath, NewRealPath, OldCloud);
		end else begin
			Result := OldCloud.cpFile(OldRealPath.path, NewRealPath.path);
		end;

	end;
	ProgressHandle(OldName, NewName, 100);
end;

function FsDisconnectW(DisconnectRoot: PWideChar): Bool; stdcall;
var
	BackgroundJobsCount: integer;
	//ThreadId: DWORD;
begin
	//ConnectionManager.freeAll;
	BackgroundJobsCount := 0;
	if ((not ThreadBackgroundJobs.TryGetValue(ExtractFileName(DisconnectRoot), BackgroundJobsCount)) or (BackgroundJobsCount = 0)) then
	begin
		ConnectionManager.Free(ExtractFileName(DisconnectRoot));
		Result := true;
	end else begin //здесь можно добавить механизм ожидания завершения фоновой операции
		Result := false;
	end;

end;

procedure FsSetCryptCallbackW(PCryptProc: TCryptProcW; CryptoNr: integer; Flags: integer); stdcall;
var
	PluginSettings: TPluginSettings;
begin
	PluginSettings := GetPluginSettings(SettingsIniFilePath);
	PasswordManager := TTCPasswordManager.Create(PCryptProc, PluginNum, CryptoNr, TCLogger);
	PasswordManager.GetProxyPassword(PluginSettings.ConnectionSettings.ProxySettings);
	if PluginSettings.ConnectionSettings.ProxySettings.use_tc_password_manager then
		SetPluginSettingsValue(SettingsIniFilePath, 'ProxyTCPwdMngr', true);

	HTTPManager := THTTPManager.Create(PluginSettings.ConnectionSettings, @ProgressHandle, TCLogger);
	ConnectionManager := TConnectionManager.Create(AccountsIniFilePath, PluginSettings, HTTPManager, @ProgressHandle, TCLogger, @RequestHandle, PasswordManager);

end;

function FsContentGetValueW(FileName: PWideChar; FieldIndex: integer; UnitIndex: integer; FieldValue: Pointer; maxlen: integer; Flags: integer): integer; stdcall;
var
	Item: TCloudMailRuDirListingItem;
	RealPath: TRealPath;
	FileTime: TFileTime;
begin
	Result := ft_nosuchfield;
	RealPath := ExtractRealPath(FileName);
	if RealPath.path = EMPTY_STR then
	begin
		if FieldIndex = 14 then
		begin
			strpcopy(FieldValue, GetAccountSettingsFromIniFile(AccountsIniFilePath, RealPath.account).Description);
			exit(ft_stringw);
		end
		else
			exit(ft_nosuchfield);
	end;

	Item := FindListingItemByPath(CurrentListing, RealPath, not RealPath.invitesDir);
	//if Item.home = '' then exit(ft_nosuchfield);

	case FieldIndex of
		0:
			begin
				if Item.mtime <> 0 then
					exit(ft_nosuchfield);
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
				if Item.mtime <> 0 then
					exit(ft_nosuchfield);
				Move(Item.grev, FieldValue^, sizeof(Item.grev));
				Result := ft_numeric_32;
			end;
		3:
			begin
				Move(Item.size, FieldValue^, sizeof(Item.size));
				Result := ft_numeric_64;
			end;
		4:
			begin
				strpcopy(FieldValue, Item.kind);
				Result := ft_stringw;
			end;
		5:
			begin
				strpcopy(FieldValue, Item.weblink);
				Result := ft_stringw;
			end;
		6:
			begin
				if Item.mtime <> 0 then
					exit(ft_nosuchfield);
				Move(Item.rev, FieldValue^, sizeof(Item.rev));
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
				if Item.mtime = 0 then
					exit(ft_nosuchfield);
				FileTime.dwHighDateTime := 0;
				FileTime.dwLowDateTime := 0;
				FileTime := DateTimeToFileTime(UnixToDateTime(Item.mtime));
				Move(FileTime, FieldValue^, sizeof(FileTime));
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
				if Item.type_ = TYPE_FILE then
					exit(ft_nosuchfield);
				Move(Item.folders_count, FieldValue^, sizeof(Item.folders_count));
				Result := ft_numeric_32;
			end;
		13:
			begin
				if Item.type_ = TYPE_FILE then
					exit(ft_nosuchfield);
				Move(Item.files_count, FieldValue^, sizeof(Item.files_count));
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
				if Item.deleted_at = 0 then
					exit(ft_nosuchfield);
				FileTime.dwHighDateTime := 0;
				FileTime.dwLowDateTime := 0;
				FileTime := DateTimeToFileTime(UnixToDateTime(Item.deleted_at));
				Move(FileTime, FieldValue^, sizeof(FileTime));
				Result := ft_datetime;
			end;
		16:
			begin
				if Item.deleted_from = EMPTY_STR then
					exit(ft_nosuchfield);
				strpcopy(FieldValue, Item.deleted_from);
				Result := ft_stringw;
			end;
		17:
			begin
				if Item.deleted_by = 0 then
					exit(ft_nosuchfield);
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
	FrontIcon, BackIcon: hicon;

	function GetFolderIconSize(IconsSize: integer): integer;
	begin
		if IconsSize <= 16 then
			exit(IconSizeSmall);
		if IconsSize <= 32 then
			exit(IconSizeNormal);
		exit(IconSizeLarge);
	end;

	procedure CombineMacro(var CombinedIcon: hicon);
	begin
		FrontIcon := LoadImageW(hInstance, RemoteName, IMAGE_ICON, IconsSize, IconsSize, LR_DEFAULTCOLOR);
		BackIcon := GetFolderIcon(GetFolderIconSize(IconsSize));
		CombinedIcon := CombineIcons(FrontIcon, BackIcon);
		DeleteObject(FrontIcon);
		DeleteObject(BackIcon);
	end;

begin
	Result := FS_ICON_EXTRACTED;

	RealPath := ExtractRealPath(RemoteName);

	if RealPath.upDirItem then
		exit; //do not overlap updir icon

	IconsMode := GetPluginSettings(SettingsIniFilePath).IconsMode;
	IconsSize := GetTCIconsSize;

	if RealPath.trashDir and (RealPath.path = EMPTY_STR) then //always draw system trash icon
	begin
		strpcopy(RemoteName, 'cloud_trash');
		TheIcon := GetSystemIcon(GetFolderIconSize(IconsSize));
		exit(FS_ICON_EXTRACTED_DESTROY);
	end;

	if RealPath.sharedDir then
	begin
		if (RealPath.path = EMPTY_STR) then
		begin
			strpcopy(RemoteName, 'shared');
			CombineMacro(TheIcon);

			exit(FS_ICON_EXTRACTED_DESTROY);
		end else begin
			if IconsMode = IconsModeDisabled then
				IconsMode := IconsModeInternalOverlay; //always draw icons in shared links directory
		end;
	end;

	if RealPath.invitesDir then
	begin
		if (RealPath.path = EMPTY_STR) then
		begin
			strpcopy(RemoteName, 'shared_incoming');
			CombineMacro(TheIcon);
			exit(FS_ICON_EXTRACTED_DESTROY);
		end else begin

			CurrentInviteItem := FindIncomingInviteItemByPath(CurrentIncomingInvitesListing, RealPath);
			if CurrentInviteItem.name = EMPTY_STR then
				exit(FS_ICON_USEDEFAULT);

			if CurrentInviteItem.home <> EMPTY_STR then //mounted item
			begin
				strpcopy(RemoteName, 'shared_incoming');
				CombineMacro(TheIcon);
			end else begin
				strpcopy(RemoteName, 'shared');
				CombineMacro(TheIcon);
			end;
			exit(FS_ICON_EXTRACTED_DESTROY);

		end;
	end;

	if IconsMode = IconsModeDisabled then
		exit(FS_ICON_USEDEFAULT);

	if (RealPath.path = EMPTY_STR) then //connection list
	begin

		if (GetAccountSettingsFromIniFile(AccountsIniFilePath, copy(RemoteName, 2, StrLen(RemoteName) - 2)).public_account) then
			strpcopy(RemoteName, 'cloud_public')
		else
			strpcopy(RemoteName, 'cloud');
	end else begin //directories
		Item := FindListingItemByPath(CurrentListing, RealPath);
		if (Item.type_ = TYPE_DIR) or (Item.kind = KIND_SHARED) then
		begin
			if Item.kind = KIND_SHARED then
				strpcopy(RemoteName, 'shared')
			else if Item.weblink <> EMPTY_STR then
				strpcopy(RemoteName, 'shared_public')
			else
				exit(FS_ICON_USEDEFAULT);
		end
		else
			exit(FS_ICON_USEDEFAULT);
	end;
	case IconsMode of
		IconsModeInternal:
			TheIcon := LoadImageW(hInstance, RemoteName, IMAGE_ICON, IconsSize, IconsSize, LR_DEFAULTCOLOR);
		IconsModeInternalOverlay:
			CombineMacro(TheIcon);
		IconsModeExternal:
			begin
				TheIcon := LoadPluginIcon(PluginPath + 'icons', RemoteName);
				if TheIcon = INVALID_HANDLE_VALUE then
					exit(FS_ICON_USEDEFAULT);
				exit(FS_ICON_EXTRACTED_DESTROY);
			end;
		IconsModeExternalOverlay:
			begin
				TheIcon := LoadPluginIcon(PluginPath + 'icons', RemoteName);
				if TheIcon = INVALID_HANDLE_VALUE then
					exit(FS_ICON_USEDEFAULT);
				BackIcon := GetFolderIcon(GetFolderIconSize(IconsSize));
				TheIcon := CombineIcons(TheIcon, BackIcon);
				DeleteObject(BackIcon);
				exit(FS_ICON_EXTRACTED_DESTROY);
			end;

	end;
end;

procedure InitPluginData;
begin
	PluginPath := GetModuleName(hInstance);
	AppDataDir := IncludeTrailingBackslash(IncludeTrailingBackslash(SysUtils.GetEnvironmentVariable('APPDATA')) + 'MailRuCloud');
	PluginPath := IncludeTrailingBackslash(ExtractFilePath(PluginPath));

	if not FileExists(GetUNCFilePath(PluginPath + 'MailRuCloud.global.ini')) then
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
					if IsWriteable(PluginPath) then
						IniDir := PluginPath
					else
						IniDir := AppDataDir;
				end;
		end;
	end;

	if not FileExists(GetUNCFilePath(IniDir)) then
		createDir(GetUNCFilePath(IniDir)); //assume this in appdata dir

	AccountsIniFilePath := IniDir + 'MailRuCloud.ini';
	SettingsIniFilePath := IniDir + 'MailRuCloud.global.ini';

	if GetPluginSettings(SettingsIniFilePath).LoadSSLDLLOnlyFromPluginDir then
	begin
		if ((DirectoryExists(PluginPath + PlatformDllPath)) and (FileExists(PluginPath + PlatformDllPath + '\ssleay32.dll')) and (FileExists(PluginPath + PlatformDllPath + '\libeay32.dll'))) then
		begin //try to load dll from platform subdir
			IdOpenSSLSetLibPath(PluginPath + PlatformDllPath);
		end else if ((FileExists(GetUNCFilePath(PluginPath + 'ssleay32.dll'))) and (FileExists(GetUNCFilePath(PluginPath + 'libeay32.dll')))) then
		begin //else try to load it from plugin dir
			IdOpenSSLSetLibPath(PluginPath);
		end;
	end;

	IsMultiThread := not(GetPluginSettings(SettingsIniFilePath).DisableMultiThreading);
	ThreadRetryCountDownload := TDictionary<DWORD, Int32>.Create;
	ThreadRetryCountUpload := TDictionary<DWORD, Int32>.Create;
	ThreadRetryCountRenMov := TDictionary<DWORD, Int32>.Create;
	ThreadSkipListDelete := TDictionary<DWORD, Boolean>.Create;
	ThreadSkipListRenMov := TDictionary<DWORD, Boolean>.Create;
	ThreadCanAbortRenMov := TDictionary<DWORD, Boolean>.Create;
	ThreadListingAborted := TDictionary<DWORD, Boolean>.Create;
	ThreadBackgroundJobs := TDictionary<WideString, Int32>.Create;
	ThreadBackgroundThreads := TDictionary<DWORD, Int32>.Create;
	ThreadFsStatusInfo := TDictionary<DWORD, Int32>.Create;
	ThreadFsRemoveDirSkippedPath := TDictionary<DWORD, TStringList>.Create;

end;

procedure FreePluginData();
begin
	FreeAndNil(ThreadRetryCountDownload);
	FreeAndNil(ThreadRetryCountUpload);
	FreeAndNil(ThreadRetryCountRenMov);
	FreeAndNil(ThreadSkipListDelete);
	FreeAndNil(ThreadSkipListRenMov);
	FreeAndNil(ThreadCanAbortRenMov);
	FreeAndNil(ThreadListingAborted);
	FreeAndNil(ThreadBackgroundJobs);
	FreeAndNil(ThreadFsStatusInfo);
	FreeAndNil(ThreadFsRemoveDirSkippedPath);
	FreeAndNil(ThreadBackgroundThreads);
	FreeAndNil(ConnectionManager);
	FreeAndNil(HTTPManager);

	FreeAndNil(AccountsList); //уже сделано, но не страшно, к тому же в будущем может не разрушаться ранее
	CurrentDescriptions.Free;

	PasswordManager.Free;

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
				FreePluginData();

			end;
		DLL_THREAD_ATTACH:
			begin
			end;
		DLL_THREAD_DETACH:
			begin
			end;
	end; //case
end;

exports
	FsGetDefRootName,
	FsInit,
	FsInitW,
	FsFindFirst,
	FsFindFirstW,
	FsFindNext,
	FsFindNextW,
	FsFindClose,
	FsGetFile,
	FsGetFileW,
	FsDisconnect,
	FsDisconnectW,
	FsStatusInfo,
	FsStatusInfoW,
	FsPutFile,
	FsPutFileW,
	FsDeleteFile,
	FsDeleteFileW,
	FsMkDir,
	FsMkDirW,
	FsRemoveDir,
	FsRemoveDirW,
	FsSetCryptCallback,
	FsSetCryptCallbackW,
	FsExecuteFileW,
	FsRenMovFile,
	FsRenMovFileW,
	FsGetBackgroundFlags,
	FsContentGetSupportedField,
	FsContentGetValue,
	FsContentGetValueW,
	FsExtractCustomIcon,
	FsExtractCustomIconW;

begin
{$IFDEF DEBUG}
	ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
	DllProc := @DllInit;
	DllInit(DLL_PROCESS_ATTACH);

end.
