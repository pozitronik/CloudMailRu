unit MailRuCloudWFX;

interface

uses
	AnsiStrings,
	WFXInterface,
	CloudMailRu,
	Windows,
	SysUtils,
	DateUtils,
	Classes,
	Generics.Collections,
	PLUGIN_TYPES,
	RealPath,
	PluginSettingsManager,
	Accountsmanager,
	WSList,
	CMRConstants,
	LANGUAGE_STRINGS,
	SETTINGS_CONSTANTS,
	CMRInviteList,
	CMRInvite,
	CMRDirItem,
	CMRDirItemList,
	CMRIncomingInviteList,
	ConnectionManager,
	IdSSLOpenSSLHeaders,
	Description,
	TCPasswordManager,
	TCLogger,
	TCProgress,
	TCRequest,
	PathHelper,
	WindowsHelper,
	TCHelper,
	CMRIncomingInvite,
	AccountSettings,
	Accounts,
	Registration,
	InviteProperty,
	RemoteProperty,
	DeletedProperty,
	Controls,
	Messages,
	HashInfo,
	StringHelper,
	PluginHelper,
	FileHelper,
	IconHelper,
	SystemHelper,
	StreamingSettings;

type
	TMailRuCloudWFX = class(TInterfacedObject, IWFXInterface)
	private const
{$IFDEF WIN64}
		PlatformDllPath = 'x64';
{$ENDIF}
{$IFDEF WIN32}
		PlatformDllPath = 'x32';
{$ENDIF}

	var
		GlobalPath, PluginPath: WideString;
		FileCounter: Integer;
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

		PluginNum: Integer;

		SettingsManager: TPluginSettingsManager;
		AccountSettings: TAccountsManager;
		Accounts: TWSList;

		CurrentListing: TCMRDirItemList;
		CurrentIncomingInvitesListing: TCMRIncomingInviteList;
		ConnectionManager: TConnectionManager;
		CurrentDescriptions: TDescription;
		PasswordManager: TTCPasswordManager;
		TCLogger: TTCLogger;
		TCProgress: TTCProgress;
		TCRequest: TTCRequest;
	protected
		function FindListingItemByPath(CurrentListing: TCMRDirItemList; Path: TRealPath; UpdateListing: Boolean = true): TCMRDirItem;
		function FindIncomingInviteItemByPath(InviteListing: TCMRIncomingInviteList; Path: TRealPath): TCMRIncomingInvite;
		function DeleteLocalFile(LocalName: WideString): Integer;
		function ExecTrashbinProperties(MainWin: THandle; RealPath: TRealPath): Integer;
		function ExecSharedAction(MainWin: THandle; RealPath: TRealPath; RemoteName: PWideChar; ActionOpen: Boolean = true): Integer;
		function ExecInvitesAction(MainWin: THandle; RealPath: TRealPath): Integer;
		function ExecProperties(MainWin: THandle; RealPath: TRealPath): Integer;
		function ExecCommand(RemoteName: PWideChar; Command: WideString; Parameter: WideString = ''): Integer;
		function ExecuteFileStream(RealPath: TRealPath; StreamingSettings: TStreamingSettings): Integer;
		procedure UpdateFileDescription(RemotePath: TRealPath; LocalFilePath: WideString; var Cloud: TCloudMailRu);
		procedure UpdateRemoteFileDescription(RemotePath: TRealPath; LocalFilePath: WideString; var Cloud: TCloudMailRu);
		procedure RenameRemoteFileDescription(OldRemotePath, NewRemotePath: TRealPath; var Cloud: TCloudMailRu);
		procedure DeleteRemoteFileDescription(RemotePath: TRealPath; var Cloud: TCloudMailRu);
		function GetRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
		function PutRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
		function CloneWeblink(NewCloud, OldCloud: TCloudMailRu; CloudPath: WideString; CurrentItem: TCMRDirItem; NeedUnpublish: Boolean): Integer;
		function RenMoveFileViaHash(OldCloud, NewCloud: TCloudMailRu; OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean): Integer;
		function RenMoveFileViaPublicLink(OldCloud, NewCloud: TCloudMailRu; OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean): Integer;
	public
		constructor Create();
		destructor Destroy; override;
		{Initialization methods}
		function FsInit(PluginNr: Integer; pProgressProc: TProgressProcW; pLogProc: TLogProcW; pRequestProc: TRequestProcW): Integer;
		procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer);
		procedure FsSetCryptCallback(PCryptProc: TCryptProcW; CryptoNr: Integer; Flags: Integer);
		function FsGetBackgroundFlags: Integer;
		{Mandatory filesystem methods}
		function FsFindFirst(Path: WideString; var FindData: tWIN32FINDDATAW): THandle;
		function FsFindNext(Hdl: THandle; var FindData: tWIN32FINDDATAW): Boolean;
		function FsFindClose(Hdl: THandle): Integer;
		{Optional filesystem methods}
		procedure FsStatusInfo(RemoteDir: WideString; InfoStartEnd, InfoOperation: Integer);
		function FsExecuteFile(MainWin: THandle; RemoteName, Verb: PWideChar): Integer;
		function FsGetFile(RemoteName, LocalName: WideString; CopyFlags: Integer; RemoteInfo: pRemoteInfo): Integer;
		function FsPutFile(LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
		function FsDeleteFile(RemoteName: WideString): Boolean;
		function FsMkDir(Path: WideString): Boolean;
		function FsRemoveDir(RemoteName: WideString): Boolean;
		function FsRenMovFile(OldName: PWideChar; NewName: PWideChar; Move: Boolean; OverWrite: Boolean; ri: pRemoteInfo): Integer;

		function FsDisconnect(DisconnectRoot: PWideChar): Boolean;

		{Content methods}
		function FsContentGetSupportedField(FieldIndex: Integer; FieldName: PAnsiChar; Units: PAnsiChar; MaxLen: Integer): Integer;
		function FsContentGetValue(FileName: PWideChar; FieldIndex: Integer; UnitIndex: Integer; FieldValue: Pointer; MaxLen: Integer; Flags: Integer): Integer;
		function FsExtractCustomIcon(RemoteName: PWideChar; ExtractFlags: Integer; var TheIcon: hIcon): Integer;

	end;

implementation

{TMailRuCloudWFX}

function TMailRuCloudWFX.CloneWeblink(NewCloud, OldCloud: TCloudMailRu; CloudPath: WideString; CurrentItem: TCMRDirItem; NeedUnpublish: Boolean): Integer;
begin
	Result := NewCloud.CloneWeblink(ExtractFileDir(CloudPath), CurrentItem.weblink, CLOUD_CONFLICT_STRICT);
	if (NeedUnpublish) and (FS_FILE_USERABORT <> Result) and not(OldCloud.publishFile(CurrentItem.home, CurrentItem.weblink, CLOUD_UNPUBLISH)) then
		TCLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, PREFIX_ERR_REMOVE_TEMP_PUBLIC_LINK + CurrentItem.home);
end;

constructor TMailRuCloudWFX.Create();
begin

	PluginPath := GetModuleName(hInstance);
	PluginPath := IncludeTrailingBackslash(ExtractFilePath(PluginPath));

	SettingsManager := TPluginSettingsManager.Create();

	if SettingsManager.Settings.LoadSSLDLLOnlyFromPluginDir then
	begin
		if ((DirectoryExists(PluginPath + PlatformDllPath)) and (FileExists(PluginPath + PlatformDllPath + '\ssleay32.dll')) and (FileExists(PluginPath + PlatformDllPath + '\libeay32.dll'))) then
		begin //try to load dll from platform subdir
			IdOpenSSLSetLibPath(PluginPath + PlatformDllPath);
		end else if ((FileExists(GetUNCFilePath(PluginPath + 'ssleay32.dll'))) and (FileExists(GetUNCFilePath(PluginPath + 'libeay32.dll')))) then
		begin //else try to load it from plugin dir
			IdOpenSSLSetLibPath(PluginPath);
		end;
	end;

	IsMultiThread := not(SettingsManager.Settings.DisableMultiThreading);
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

	AccountSettings := TAccountsManager.Create(SettingsManager.AccountsIniFilePath);
end;

function TMailRuCloudWFX.FsInit(PluginNr: Integer; pProgressProc: TProgressProcW; pLogProc: TLogProcW; pRequestProc: TRequestProcW): Integer;
begin
	PluginNum := PluginNr;
	TCLogger := TTCLogger.Create(pLogProc, PluginNr, SettingsManager.Settings.LogLevel);
	TCProgress := TTCProgress.Create(pProgressProc, PluginNr);
	TCRequest := TTCRequest.Create(pRequestProc, PluginNr);
	CurrentDescriptions := TDescription.Create(GetTmpFileName('ion'), GetTCCommentPreferredFormat);
	Result := 0;
end;

function TMailRuCloudWFX.DeleteLocalFile(LocalName: WideString): Integer;
var
	UNCLocalName: WideString;
	DeleteFailOnUploadMode, DeleteFailOnUploadModeAsked: Integer;
begin
	Result := FS_FILE_OK;
	DeleteFailOnUploadModeAsked := IDRETRY;
	UNCLocalName := GetUNCFilePath(LocalName);

	while (not DeleteFileW(PWideChar(UNCLocalName))) and (DeleteFailOnUploadModeAsked = IDRETRY) do
	begin
		DeleteFailOnUploadMode := SettingsManager.Settings.DeleteFailOnUploadMode;
		if DeleteFailOnUploadMode = DeleteFailOnUploadAsk then
		begin
			DeleteFailOnUploadModeAsked := MsgBox(ERR_DELETE_FILE_ASK, [LocalName], ERR_DELETE_FILE, MB_ABORTRETRYIGNORE + MB_ICONQUESTION);
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
						if SettingsManager.Settings.DeleteFailOnUploadMode = DeleteFailOnUploadDeleteIgnore then
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

procedure TMailRuCloudWFX.DeleteRemoteFileDescription(RemotePath: TRealPath; var Cloud: TCloudMailRu);
var
	RemoteDescriptions: TDescription;
	RemoteIonPath, LocalTempPath: WideString;
begin
	RemoteIonPath := IncludeTrailingBackslash(ExtractFileDir(RemotePath.Path)) + SettingsManager.Settings.DescriptionFileName;
	LocalTempPath := GetTmpFileName('ion');
	if not Cloud.getDescriptionFile(RemoteIonPath, LocalTempPath) then
		exit; //описания нет, не заморачиваемся
	RemoteDescriptions := TDescription.Create(LocalTempPath, GetTCCommentPreferredFormat);
	RemoteDescriptions.Read;
	RemoteDescriptions.DeleteValue(ExtractFileName(RemotePath.Path));
	RemoteDescriptions.Write();
	Cloud.deleteFile(RemoteIonPath); //Приходится удалять, потому что не знаем, как переписать
	Cloud.PutDescriptionFile(RemoteIonPath, RemoteDescriptions.ionFilename);
	RemoteDescriptions.Destroy;
end;

destructor TMailRuCloudWFX.Destroy;
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

	CurrentDescriptions.Free;

	SettingsManager.Free;
	AccountSettings.Free;
	PasswordManager.Free;
	TCLogger.Free;
	TCProgress.Free;
	TCRequest.Free;
	inherited;
end;

function TMailRuCloudWFX.ExecCommand(RemoteName: PWideChar; Command, Parameter: WideString): Integer;
var
	RealPath: TRealPath;
	getResult: Integer;
	Cloud: TCloudMailRu;
	HashInfo: THashInfo;
begin
	Result := FS_EXEC_OK;

	if Command = 'rmdir' then
	begin
		RealPath.FromPath(RemoteName + Parameter);
		if (ConnectionManager.Get(RealPath.account, getResult).removeDir(RealPath.Path) <> true) then
			exit(FS_EXEC_ERROR);
	end;

	RealPath.FromPath(RemoteName); //default
	Cloud := ConnectionManager.Get(RealPath.account, getResult);

	//undocumented, share current folder to email param
	if Command = 'share' then
		if not(Cloud.shareFolder(RealPath.Path, ExtractLinkFromUrl(Parameter), CLOUD_SHARE_RW)) then
			exit(FS_EXEC_ERROR);

	if Command = 'hash' then //add file by hash & filesize
	begin
		HashInfo := THashInfo.Create(Parameter);
		if HashInfo.valid then
		begin
			Cloud.addFileByIdentity(HashInfo.CloudFileIdentity, IncludeTrailingPathDelimiter(RealPath.Path) + HashInfo.name, CLOUD_CONFLICT_RENAME);
			HashInfo.Destroy;
		end else begin
			TCLogger.Log(LOG_LEVEL_DEBUG, msgtype_details, ERR_CLONE_BY_HASH, [HashInfo.errorString, Parameter]);
			HashInfo.Destroy;
			exit(FS_EXEC_ERROR);
		end;
	end;

	if Command = 'clone' then //add file by weblink
	begin
		if (Cloud.CloneWeblink(RealPath.Path, ExtractLinkFromUrl(Parameter)) = CLOUD_OPERATION_OK) then
			if SettingsManager.Settings.LogUserSpace then
				Cloud.logUserSpaceInfo
			else
				exit(FS_EXEC_ERROR);
	end;

	if Command = 'trash' then //go to current account trash directory
	begin
		if Cloud.IsPublicAccount then
			exit(FS_EXEC_ERROR);
		if RealPath.IsInAccount(false) then
		begin
			strpcopy(RemoteName, '\' + RealPath.account + TrashPostfix);
			exit(FS_EXEC_SYMLINK);
		end;
	end;

	if Command = 'shared' then
	begin
		if Cloud.IsPublicAccount then
			exit(FS_EXEC_ERROR);
		if RealPath.IsInAccount(false) then
		begin
			strpcopy(RemoteName, '\' + RealPath.account + SharedPostfix);
			exit(FS_EXEC_SYMLINK);
		end;
	end;

	if Command = 'invites' then
	begin
		if Cloud.IsPublicAccount then
			exit(FS_EXEC_ERROR);
		if RealPath.IsInAccount(false) then
		begin
			strpcopy(RemoteName, '\' + RealPath.account + InvitesPostfix);
			exit(FS_EXEC_SYMLINK);
		end;
	end;
end;

function TMailRuCloudWFX.ExecInvitesAction(MainWin: THandle; RealPath: TRealPath): Integer;
var
	Cloud: TCloudMailRu;
	getResult: Integer;
	CurrentInvite: TCMRIncomingInvite;
begin
	Result := FS_EXEC_OK;
	Cloud := ConnectionManager.Get(RealPath.account, getResult);
	if RealPath.isInAccountsList then //main invites folder properties
	begin
		if TAccountsForm.ShowAccounts(MainWin, PasswordManager, RealPath.account) then
			SettingsManager.Refresh;
	end else begin //one invite item
		CurrentInvite := FindIncomingInviteItemByPath(CurrentIncomingInvitesListing, RealPath);
		if CurrentInvite.name = EmptyWideStr then
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

function TMailRuCloudWFX.ExecProperties(MainWin: THandle; RealPath: TRealPath): Integer;
var
	Cloud: TCloudMailRu;
	CurrentItem: TCMRDirItem;
	getResult: Integer;
begin
	Result := FS_EXEC_OK;
	if RealPath.isInAccountsList then
	begin
		if TAccountsForm.ShowAccounts(MainWin, PasswordManager, RealPath.account) then //show account properties
			SettingsManager.Refresh;
	end else begin
		Cloud := ConnectionManager.Get(RealPath.account, getResult);
		//всегда нужно обновлять статус на сервере, CurrentListing может быть изменён в другой панели
		if (Cloud.statusFile(RealPath.Path, CurrentItem)) and (idContinue = TPropertyForm.ShowProperty(MainWin, RealPath.Path, CurrentItem, Cloud, SettingsManager.Settings.DownloadLinksEncode, SettingsManager.Settings.AutoUpdateDownloadListing, SettingsManager.Settings.DescriptionEnabled, SettingsManager.Settings.DescriptionEditorEnabled, SettingsManager.Settings.DescriptionFileName)) then
			PostMessage(MainWin, WM_USER + 51, 540, 0); //refresh tc panel if description edited
	end;
end;

function TMailRuCloudWFX.ExecSharedAction(MainWin: THandle; RealPath: TRealPath; RemoteName: PWideChar; ActionOpen: Boolean): Integer;
var
	Cloud: TCloudMailRu;
	CurrentItem: TCMRDirItem;
	getResult: Integer;
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
		if RealPath.isInAccountsList then
		begin
			if TAccountsForm.ShowAccounts(MainWin, PasswordManager, RealPath.account) then //main shared folder properties - open connection settings
				SettingsManager.Refresh;
		end else begin
			Cloud := ConnectionManager.Get(RealPath.account, getResult);
			CurrentItem := FindListingItemByPath(CurrentListing, RealPath);
			if Cloud.statusFile(CurrentItem.home, CurrentItem) then
				TPropertyForm.ShowProperty(MainWin, RealPath.Path, CurrentItem, Cloud, SettingsManager.Settings.DownloadLinksEncode, SettingsManager.Settings.AutoUpdateDownloadListing, false, false, SettingsManager.Settings.DescriptionFileName)
		end;
	end;
end;

function TMailRuCloudWFX.ExecTrashbinProperties(MainWin: THandle; RealPath: TRealPath): Integer;
var
	Cloud: TCloudMailRu;
	getResult: Integer;
	CurrentItem: TCMRDirItem;
begin
	Result := FS_EXEC_OK;
	Cloud := ConnectionManager.Get(RealPath.account, getResult);
	if RealPath.isInAccountsList then //main trashbin folder properties
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

function TMailRuCloudWFX.ExecuteFileStream(RealPath: TRealPath; StreamingSettings: TStreamingSettings): Integer;
var
	StreamUrl: WideString;
	getResult: Integer;
	CurrentCloud, TempPublicCloud: TCloudMailRu;
	CurrentItem: TCMRDirItem;
begin
	Result := FS_EXEC_OK;
	if (STREAMING_FORMAT_DISABLED = StreamingSettings.Format) or (STREAMING_FORMAT_UNSET = StreamingSettings.Format) then
		exit;

	//может быть разница в атрибутах настоящих и полученных из листинга (они не рефрешатся)
	CurrentItem := FindListingItemByPath(CurrentListing, RealPath); //внутри публичного облака веблинк есть автоматически

	if TCloudMailRu.TempPublicCloudInit(TempPublicCloud, PUBLIC_ACCESS_URL + CurrentItem.weblink) then
	begin
		if STREAMING_FORMAT_PLAYLIST = StreamingSettings.Format then
		begin
			if not TempPublicCloud.getPublishedFileStreamUrl(CurrentItem, StreamUrl) then
				Result := FS_EXEC_ERROR;
		end else begin
			if not CurrentItem.isPublished then
			begin
				CurrentCloud := ConnectionManager.Get(RealPath.account, getResult);
				if not CurrentCloud.publishFile(CurrentItem.home, CurrentItem.weblink) then
					Result := FS_EXEC_ERROR;
				//Здесь можно бы обновить листинг
			end;
			if FS_EXEC_OK = Result then
				StreamUrl := TempPublicCloud.getSharedFileUrl(EmptyWideStr, ShardTypeFromStreamingFormat(StreamingSettings.Format));
		end;

		if FS_EXEC_OK = Result then
		begin
			if EmptyWideStr = StreamingSettings.Parameters then
				StreamingSettings.Parameters := '%url%';
			StreamingSettings.Parameters := StringReplace(StreamingSettings.Parameters, '%url%', StreamUrl, [rfReplaceAll, rfIgnoreCase]);

			if not(Run(StreamingSettings.Command, StreamUrl, StreamingSettings.StartPath)) then
				Result := FS_EXEC_ERROR;
		end;

	end;

	FreeAndNil(TempPublicCloud);
end;

function TMailRuCloudWFX.FindIncomingInviteItemByPath(InviteListing: TCMRIncomingInviteList; Path: TRealPath): TCMRIncomingInvite;
var
	getResult: Integer;
begin
	Result := InviteListing.FindByName(Path.Path);
	{item not found in current global listing, so refresh it}
	if Result.isNone then
		if ConnectionManager.Get(Path.account, getResult).getIncomingLinksListing(CurrentIncomingInvitesListing) then
			exit(CurrentIncomingInvitesListing.FindByName(Path.Path));
end;

function TMailRuCloudWFX.FindListingItemByPath(CurrentListing: TCMRDirItemList; Path: TRealPath; UpdateListing: Boolean): TCMRDirItem;
var
	getResult: Integer;
	CurrentCloud: TCloudMailRu;
begin
	CurrentCloud := ConnectionManager.Get(Path.account, getResult);
	if not Assigned(CurrentCloud) then
		exit;

	if Path.HasHomePath and not CurrentCloud.IsPublicAccount then
		Result := CurrentListing.FindByHomePath(Path.Path) //сначала попробуем найти поле в имеющемся списке
	else
		Result := CurrentListing.FindByName(ExtractUniversalFileName(Path.Path));

	if Result.isNone and UpdateListing then //если там его нет (нажали пробел на папке, например), то запросим в облаке напрямую, в зависимости от того, внутри чего мы находимся
	begin

		if Path.trashDir then //корзина - обновим CurrentListing, поищем в нём
		begin
			if CurrentCloud.getTrashbinListing(CurrentListing) then
				exit(CurrentListing.FindByName(Path.Path));
		end;
		if Path.sharedDir then //ссылки - обновим список
		begin
			if CurrentCloud.getSharedLinksListing(CurrentListing) then
				exit(CurrentListing.FindByName(Path.Path));
		end;
		if Path.invitesDir then
		begin
			//FindIncomingInviteItemByPath in that case!
		end;
		if CurrentCloud.statusFile(Path.Path, Result) then //Обычный каталог
		begin
			if (Result.home = EmptyWideStr) and not CurrentCloud.IsPublicAccount then
				TCLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_WHERE_IS_THE_FILE, [Path.Path]); {Такого быть не может, но...}
		end;
	end; //Не рапортуем, это будет уровнем выше
end;

function TMailRuCloudWFX.FsContentGetSupportedField(FieldIndex: Integer; FieldName, Units: PAnsiChar; MaxLen: Integer): Integer;
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

function TMailRuCloudWFX.FsContentGetValue(FileName: PWideChar; FieldIndex, UnitIndex: Integer; FieldValue: Pointer; MaxLen, Flags: Integer): Integer;
var
	Item: TCMRDirItem;
	RealPath: TRealPath;
	FileTime: TFileTime;
begin
	Result := ft_nosuchfield;
	RealPath.FromPath(FileName);
	if RealPath.isInAccountsList then
	begin
		if FieldIndex = 14 then
		begin
			strpcopy(FieldValue, AccountSettings.GetAccountSettings(RealPath.account).Description);
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
				if SettingsManager.Settings.DescriptionEnabled then
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
				if Item.deleted_from = EmptyWideStr then
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

function TMailRuCloudWFX.FsDeleteFile(RemoteName: WideString): Boolean;
var
	RealPath: TRealPath;
	getResult: Integer;
	CurrentItem: TCMRDirItem;
	Cloud: TCloudMailRu;
	InvitesListing: TCMRInviteList;
	Invite: TCMRInvite;
begin
	RealPath.FromPath(WideString(RemoteName));
	if RealPath.isAccountEmpty or RealPath.trashDir or RealPath.invitesDir then
		exit(false);
	Cloud := ConnectionManager.Get(RealPath.account, getResult);
	if RealPath.sharedDir then
	begin
		CurrentItem := FindListingItemByPath(CurrentListing, RealPath);
		Cloud.getShareInfo(CurrentItem.home, InvitesListing);
		for Invite in InvitesListing do
			Cloud.shareFolder(CurrentItem.home, Invite.email, CLOUD_SHARE_NO); //no reporting here
		if CurrentItem.isPublished then
			Cloud.publishFile(CurrentItem.home, CurrentItem.weblink, CLOUD_UNPUBLISH);
		Result := true;
	end
	else
		Result := Cloud.deleteFile(RealPath.Path);
	if (Result and SettingsManager.Settings.DescriptionTrackCloudFS and AccountSettings.GetAccountSettings(RealPath.account).IsRemoteDescriptionsSupported) then
		DeleteRemoteFileDescription(RealPath, Cloud);
end;

function TMailRuCloudWFX.FsDisconnect(DisconnectRoot: PWideChar): Boolean;
var
	BackgroundJobsCount: Integer;
begin
	BackgroundJobsCount := 0;
	if ((not ThreadBackgroundJobs.TryGetValue(ExtractFileName(DisconnectRoot), BackgroundJobsCount)) or (BackgroundJobsCount = 0)) then
	begin
		ConnectionManager.Free(ExtractFileName(DisconnectRoot));
		Result := true;
	end else begin //здесь можно добавить механизм ожидания завершения фоновой операции
		Result := false;
	end;
end;

function TMailRuCloudWFX.FsExecuteFile(MainWin: THandle; RemoteName, Verb: PWideChar): Integer;
var
	RealPath: TRealPath;
	TargetStreamingSettings: TStreamingSettings;
begin
	RealPath.FromPath(RemoteName);

	if RealPath.upDirItem then
		RealPath.Path := ExtractFilePath(RealPath.Path); //if somepath/.. item properties called

	if RealPath.trashDir and ((Verb = VERB_OPEN) or (Verb = VERB_PROPERTIES)) then
		exit(ExecTrashbinProperties(MainWin, RealPath));

	if RealPath.sharedDir then
		exit(ExecSharedAction(MainWin, RealPath, RemoteName, Verb = VERB_OPEN));

	if RealPath.invitesDir then
		exit(ExecInvitesAction(MainWin, RealPath));

	if Verb = VERB_PROPERTIES then
		exit(ExecProperties(MainWin, RealPath));

	if Verb = VERB_OPEN then
	begin
		if (not(RealPath.isDir = ID_True)) then
			TargetStreamingSettings := SettingsManager.GetStreamingSettings(RealPath.Path);
		if (TargetStreamingSettings.Format <> STREAMING_FORMAT_UNSET) and (TargetStreamingSettings.Format <> STREAMING_FORMAT_NONE) then
			exit(ExecuteFileStream(RealPath, TargetStreamingSettings));
		exit(FS_EXEC_YOURSELF);
	end;

	if copy(Verb, 1, 5) = VERB_QUOTE then
		exit(ExecCommand(RemoteName, LowerCase(GetWord(Verb, 1)), GetWord(Verb, 2)));

	//if copy(Verb, 1, 5) = 'chmod' then exit; //future usage
	exit(FS_EXEC_OK)
end;

function TMailRuCloudWFX.FsExtractCustomIcon(RemoteName: PWideChar; ExtractFlags: Integer; var TheIcon: hIcon): Integer;
var
	RealPath: TRealPath;
	Item: TCMRDirItem;
	IconsMode: Integer;
	CurrentInviteItem: TCMRIncomingInvite;
	IconsSize: Integer;
	FrontIcon, BackIcon: hIcon;

	function GetFolderIconSize(IconsSize: Integer): Integer;
	begin
		if IconsSize <= 16 then
			exit(IconSizeSmall);
		if IconsSize <= 32 then
			exit(IconSizeNormal);
		exit(IconSizeLarge);
	end;

	procedure CombineMacro(var CombinedIcon: hIcon);
	begin
		FrontIcon := LoadImageW(hInstance, RemoteName, IMAGE_ICON, IconsSize, IconsSize, LR_DEFAULTCOLOR);
		BackIcon := GetFolderIcon(GetFolderIconSize(IconsSize));
		CombinedIcon := CombineIcons(FrontIcon, BackIcon);
		DeleteObject(FrontIcon);
		DeleteObject(BackIcon);
	end;

begin
	Result := FS_ICON_EXTRACTED;

	RealPath.FromPath(RemoteName);

	if RealPath.upDirItem then
		exit; //do not overlap updir icon

	IconsMode := SettingsManager.Settings.IconsMode;
	IconsSize := GetTCIconsSize;

	if RealPath.trashDir and RealPath.isInAccountsList then //always draw system trash icon
	begin
		strpcopy(RemoteName, 'cloud_trash');
		TheIcon := GetSystemIcon(GetFolderIconSize(IconsSize));
		exit(FS_ICON_EXTRACTED_DESTROY);
	end;

	if RealPath.sharedDir then
	begin
		if RealPath.isInAccountsList then
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
		if RealPath.isInAccountsList then
		begin
			strpcopy(RemoteName, 'shared_incoming');
			CombineMacro(TheIcon);
			exit(FS_ICON_EXTRACTED_DESTROY);
		end else begin

			CurrentInviteItem := FindIncomingInviteItemByPath(CurrentIncomingInvitesListing, RealPath);
			if CurrentInviteItem.name = EmptyWideStr then
				exit(FS_ICON_USEDEFAULT);

			if CurrentInviteItem.isMounted then //mounted item
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

	if RealPath.isInAccountsList then //connection list
	begin
		if AccountSettings.GetAccountSettings(copy(RemoteName, 2, StrLen(RemoteName) - 2)).PublicAccount then
			strpcopy(RemoteName, 'cloud_public')
		else
			strpcopy(RemoteName, 'cloud');
	end else begin //directories
		Item := FindListingItemByPath(CurrentListing, RealPath);
		if (Item.type_ = TYPE_DIR) or (Item.kind = KIND_SHARED) then
		begin
			if Item.kind = KIND_SHARED then
				strpcopy(RemoteName, 'shared')
			else if Item.isPublished then
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

function TMailRuCloudWFX.FsFindClose(Hdl: THandle): Integer;
begin
	FileCounter := 0;
	Result := 0;
end;

function TMailRuCloudWFX.FsFindFirst(Path: WideString; var FindData: tWIN32FINDDATAW): THandle;
var //Получение первого файла в папке. Result тоталом не используется (можно использовать для работы плагина).
	RealPath: TRealPath;
	getResult: Integer;
	SkipListDelete, SkipListRenMov, CanAbortRenMov, RenMovAborted: Boolean;
	CurrentItem: TCMRDirItem;
	CurrentCloud: TCloudMailRu;
begin
	ThreadSkipListDelete.TryGetValue(GetCurrentThreadID(), SkipListDelete);
	ThreadSkipListRenMov.TryGetValue(GetCurrentThreadID(), SkipListRenMov);

	ThreadCanAbortRenMov.TryGetValue(GetCurrentThreadID(), CanAbortRenMov);

	if (CanAbortRenMov and TCProgress.Progress(Path)) then
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
	GlobalPath := Path;
	if GlobalPath = '\' then
	begin //список соединений
		Accounts := AccountSettings.GetAccountsList([ATPrivate, ATPublic], SettingsManager.Settings.EnabledVirtualTypes);
		if (Accounts.Count > 0) then
		begin
			FindData := GetFindDataEmptyDir(Accounts[0]);
			FileCounter := 1;
			Result := FIND_ROOT_DIRECTORY;
		end else begin
			Result := INVALID_HANDLE_VALUE; //Нельзя использовать exit
			SetLastError(ERROR_NO_MORE_FILES);
		end;
	end else begin
		RealPath.FromPath(GlobalPath);
		CurrentCloud := ConnectionManager.Get(RealPath.account, getResult);

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
			if not CurrentCloud.getDirListing(RealPath.Path, CurrentListing) then
				SetLastError(ERROR_PATH_NOT_FOUND);
		end;

		if RealPath.isVirtual and not RealPath.isInAccountsList then //игнорим попытки получить листинги объектов вирутальных каталогов
		begin
			SetLastError(ERROR_ACCESS_DENIED);
			exit(INVALID_HANDLE_VALUE);
		end;

		if CurrentCloud.IsPublicAccount then
			CurrentItem := CurrentListing.FindByName(ExtractUniversalFileName(RealPath.Path))
		else
			CurrentItem := CurrentListing.FindByHomePath(RealPath.Path);

		if not(CurrentItem.isNone or CurrentItem.isDir) then
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

			FindData := CurrentListing[0].ToFindData(RealPath.sharedDir); //folders inside shared links directory must be displayed as symlinks
			FileCounter := 1;
			if RealPath.sharedDir then
				Result := FIND_SHARED_LINKS
			else
				Result := FIND_OK;
		end;
	end;
end;

function TMailRuCloudWFX.FsFindNext(Hdl: THandle; var FindData: tWIN32FINDDATAW): Boolean;
begin
	if GlobalPath = '\' then
	begin
		if (Accounts.Count > FileCounter) then
		begin
			FindData := GetFindDataEmptyDir(Accounts[FileCounter]);
			inc(FileCounter);
			Result := true;
		end
		else
			Result := false;

	end else begin
		//Получение последующих файлов в папке (вызывается до тех пор, пока не вернёт false).
		if (Length(CurrentListing) > FileCounter) then
		begin
			FindData := CurrentListing[FileCounter].ToFindData(Hdl = FIND_SHARED_LINKS);
			Result := true;
			inc(FileCounter);
		end else begin
			FillChar(FindData, sizeof(WIN32_FIND_DATA), 0);
			FileCounter := 0;
			Result := false;
		end;
	end;
end;

function TMailRuCloudWFX.FsGetBackgroundFlags: Integer;
begin
	if SettingsManager.Settings.DisableMultiThreading then
		Result := 0
	else
		Result := BG_DOWNLOAD + BG_UPLOAD; //+ BG_ASK_USER;
end;

procedure TMailRuCloudWFX.FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer);
begin

end;

function TMailRuCloudWFX.FsGetFile(RemoteName, LocalName: WideString; CopyFlags: Integer; RemoteInfo: pRemoteInfo): Integer;
var
	RealPath: TRealPath;
	OverwriteLocalMode: Integer;
	RetryAttempts: Integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if CheckFlag(FS_COPYFLAGS_RESUME, CopyFlags) then
		exit; {NEVER CALLED HERE}
	RealPath.FromPath(RemoteName);
	if RealPath.isVirtual then
		exit;

	TCProgress.Progress(RemoteName, LocalName, 0);

	OverwriteLocalMode := SettingsManager.Settings.OverwriteLocalMode;
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

	case SettingsManager.Settings.OperationErrorMode of
		OperationErrorModeAsk:
			begin
				while (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					case (MsgBox(ERR_DOWNLOAD_FILE_ASK, [RemoteName], ERR_DOWNLOAD, MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
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
				RetryAttempts := SettingsManager.Settings.RetryAttempts;
				while (ThreadRetryCountDownload.Items[GetCurrentThreadID()] <> RetryAttempts) and (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					ThreadRetryCountDownload.Items[GetCurrentThreadID()] := ThreadRetryCountDownload.Items[GetCurrentThreadID()] + 1;
					TCLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, DOWNLOAD_FILE_RETRY, [RemoteName, ThreadRetryCountDownload.Items[GetCurrentThreadID()], RetryAttempts]);
					Result := GetRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
					if TCProgress.Progress(PWideChar(LocalName), RemoteName, 0) then
						Result := FS_FILE_USERABORT;
					if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then
						ThreadRetryCountDownload.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
					ProcessMessages;
					Sleep(SettingsManager.Settings.AttemptWait);
				end;
			end;
	end;
end;

function TMailRuCloudWFX.FsMkDir(Path: WideString): Boolean;
var
	RealPath: TRealPath;
	getResult: Integer;
	SkipListRenMov: Boolean;
	OperationContextId: Integer;
	RegisteredAccount: TAccountSettings;
begin
	ThreadSkipListRenMov.TryGetValue(GetCurrentThreadID(), SkipListRenMov);
	if SkipListRenMov then
		exit(false); //skip create directory if this flag set on

	RealPath.FromPath(WideString(Path));
	if RealPath.isInAccountsList then //accounts list
	begin
		RegisteredAccount := AccountSettings.GetAccountSettings(RealPath.account);

		Result := (mrOk = TRegistrationForm.ShowRegistration(FindTCWindow, SettingsManager.Settings.ConnectionSettings, RegisteredAccount));
		if Result then
		begin
			if RegisteredAccount.UseTCPasswordManager then //просим TC сохранить пароль
				Result := FS_FILE_OK = PasswordManager.SetPassword(RealPath.account, RegisteredAccount.password);
			if Result then
				AccountSettings.SetAccountSettings(RealPath.account, RegisteredAccount);
		end;
		exit();
	end;
	if (RealPath.isAccountEmpty) or RealPath.isVirtual then
		exit(false);

	Result := ConnectionManager.Get(RealPath.account, getResult).createDir(RealPath.Path);
	if Result then //need to check operation context => directory can be moved
	begin
		ThreadFsStatusInfo.TryGetValue(GetCurrentThreadID, OperationContextId);
		if OperationContextId = FS_STATUS_OP_RENMOV_MULTI then
			CurrentlyMovedDir := RealPath;
	end;
end;

function TMailRuCloudWFX.FsPutFile(LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
var
	RealPath: TRealPath;
	RetryAttempts: Integer;
	getResult: Integer;
begin
	RealPath.FromPath(RemoteName);
	if not FileExists(GetUNCFilePath(LocalName)) then
		exit(FS_FILE_NOTFOUND);

	if RealPath.isAccountEmpty or RealPath.isVirtual then
		exit(FS_FILE_NOTSUPPORTED);
	TCProgress.Progress(LocalName, PWideChar(RealPath.Path), 0);

	if CheckFlag(FS_COPYFLAGS_RESUME, CopyFlags) then
		exit(FS_FILE_NOTSUPPORTED); //NOT SUPPORTED

	if (CheckFlag(FS_COPYFLAGS_EXISTS_SAMECASE, CopyFlags) or CheckFlag(FS_COPYFLAGS_EXISTS_DIFFERENTCASE, CopyFlags)) and not(CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags)) then
		exit(FS_FILE_EXISTS); //Облако не поддерживает разные регистры

	if CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags) then
	begin
		if not(ConnectionManager.Get(RealPath.account, getResult).deleteFile(RealPath.Path)) then
			exit(FS_FILE_NOTSUPPORTED); //Неизвестно, как перезаписать файл черз API, но мы можем его удалить
	end;
	Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);

	//if Result in [FS_FILE_OK, FS_FILE_USERABORT, FS_FILE_NOTSUPPORTED] then exit;
	if Result <> FS_FILE_WRITEERROR then
		exit;

	case SettingsManager.Settings.OperationErrorMode of
		OperationErrorModeAsk:
			begin
				while (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					case (MsgBox(ERR_UPLOAD_FILE_ASK, [LocalName], ERR_UPLOAD, MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
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
				RetryAttempts := SettingsManager.Settings.RetryAttempts;
				while (ThreadRetryCountUpload.Items[GetCurrentThreadID()] <> RetryAttempts) and (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
				begin
					ThreadRetryCountUpload.Items[GetCurrentThreadID()] := ThreadRetryCountUpload.Items[GetCurrentThreadID()] + 1;
					TCLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, UPLOAD_FILE_RETRY, [LocalName, ThreadRetryCountUpload.Items[GetCurrentThreadID()], RetryAttempts]);
					Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
					if TCProgress.Progress(PWideChar(LocalName), RemoteName, 0) then
						Result := FS_FILE_USERABORT;
					if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then
						ThreadRetryCountUpload.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
					ProcessMessages;
					Sleep(SettingsManager.Settings.AttemptWait);
				end;
			end;
	end;
end;

function TMailRuCloudWFX.FsRemoveDir(RemoteName: WideString): Boolean;
var
	RealPath: TRealPath;
	getResult: Integer;
	ListingAborted: Boolean;
	Cloud: TCloudMailRu;
	OperationContextId: Integer;
begin
	if (ThreadFsRemoveDirSkippedPath.ContainsKey(GetCurrentThreadID) and Assigned(ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID]) and ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID].Text.Contains(RemoteName)) then //файлы по удаляемому пути есть в блек-листе
		exit(false);
	ThreadListingAborted.TryGetValue(GetCurrentThreadID(), ListingAborted);
	if ListingAborted then
	begin
		ThreadListingAborted.AddOrSetValue(GetCurrentThreadID(), false);
		exit(false);
	end;
	RealPath.FromPath(WideString(RemoteName));
	if RealPath.isVirtual then
		exit(false);
	Cloud := ConnectionManager.Get(RealPath.account, getResult);
	Result := Cloud.removeDir(RealPath.Path);

	if (Result and SettingsManager.Settings.DescriptionTrackCloudFS and AccountSettings.GetAccountSettings(RealPath.account).IsRemoteDescriptionsSupported) then
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

function TMailRuCloudWFX.FsRenMovFile(OldName, NewName: PWideChar; Move, OverWrite: Boolean; ri: pRemoteInfo): Integer;
var
	OldRealPath: TRealPath;
	NewRealPath: TRealPath;
	getResult, SkippedFoundIndex: Integer;
	OldCloud, NewCloud: TCloudMailRu;
begin
	TCProgress.Progress(OldName, NewName, 0);

	OldRealPath.FromPath(WideString(OldName));
	NewRealPath.FromPath(WideString(NewName));

	{TODO: Check the behavior inside virtual directories}
	if OldRealPath.trashDir or NewRealPath.trashDir or OldRealPath.sharedDir or NewRealPath.sharedDir then
		exit(FS_FILE_NOTSUPPORTED);

	OldCloud := ConnectionManager.Get(OldRealPath.account, getResult);
	NewCloud := ConnectionManager.Get(NewRealPath.account, getResult);

	if OldRealPath.account <> NewRealPath.account then //разные аккаунты
	begin
		if OldCloud.IsPublicAccount then
		begin
			TCLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, ERR_DIRECT_OPERATIONS_NOT_SUPPORTED);
			exit(FS_FILE_USERABORT);
		end;

		case SettingsManager.Settings.CopyBetweenAccountsMode of
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

		if OverWrite and not(NewCloud.deleteFile(NewRealPath.Path)) then
			exit(FS_FILE_NOTSUPPORTED); //мы не умеем перезаписывать, но мы можем удалить существующий файл
		if Move then
		begin
			Result := OldCloud.FileMove(OldRealPath.Path, NewRealPath.Path);
			if (FS_FILE_EXISTS = Result) and (ThreadFsRemoveDirSkippedPath.ContainsKey(GetCurrentThreadID)) then //TC сразу же попытается удалить каталог, чтобы избежать этого - внесем путь в своеобразный блеклист
			begin
				ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID].Add(OldRealPath.ToPath);
			end else if (FS_FILE_OK = Result) and (ThreadFsRemoveDirSkippedPath.ContainsKey(GetCurrentThreadID)) then
			begin //Вытащим из блеклиста, если решили перезаписать

				if Assigned(ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID]) then
				begin
					SkippedFoundIndex := ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID].IndexOf(OldRealPath.ToPath);
					if (-1 <> SkippedFoundIndex) then
						ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID].Delete(SkippedFoundIndex);
				end;

			end;
			if ((FS_FILE_OK = Result) and SettingsManager.Settings.DescriptionTrackCloudFS and AccountSettings.GetAccountSettings(NewRealPath.account).IsRemoteDescriptionsSupported) then
				RenameRemoteFileDescription(OldRealPath, NewRealPath, OldCloud);
		end else begin
			Result := OldCloud.FileCopy(OldRealPath.Path, NewRealPath.Path);
		end;

	end;
	TCProgress.Progress(OldName, NewName, 100);
end;

procedure TMailRuCloudWFX.FsSetCryptCallback(PCryptProc: TCryptProcW; CryptoNr, Flags: Integer);
begin
	PasswordManager := TTCPasswordManager.Create(PCryptProc, PluginNum, CryptoNr, TCLogger);
	ConnectionManager := TConnectionManager.Create(SettingsManager.Settings, TCProgress, TCLogger, TCRequest, PasswordManager);
end;

procedure TMailRuCloudWFX.FsStatusInfo(RemoteDir: WideString; InfoStartEnd, InfoOperation: Integer);
var
	RealPath: TRealPath;
	getResult: Integer;
	BackgroundJobsCount: Integer;
begin
	RealPath.FromPath(RemoteDir, ID_True); // RemoteDir always a directory
	if (InfoStartEnd = FS_STATUS_START) then
	begin
		ThreadFsStatusInfo.AddOrSetValue(GetCurrentThreadID(), InfoOperation);
		case InfoOperation of
			FS_STATUS_OP_LIST:
				begin
					if (SettingsManager.Settings.DescriptionEnabled) and RealPath.IsInAccount() then
					begin
						if ConnectionManager.Get(RealPath.account, getResult).getDescriptionFile(IncludeTrailingBackslash(RealPath.Path) + SettingsManager.Settings.DescriptionFileName, CurrentDescriptions.ionFilename) then
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
					if ConnectionManager.Get(RealPath.account, getResult).IsPublicAccount then
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
					if RealPath.IsInAccount() and SettingsManager.Settings.LogUserSpace then
						ConnectionManager.Get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_PUT_MULTI:
				begin
					if RealPath.IsInAccount() and SettingsManager.Settings.LogUserSpace then
						ConnectionManager.Get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_RENMOV_SINGLE:
				begin
					if RealPath.IsInAccount() and SettingsManager.Settings.LogUserSpace then
						ConnectionManager.Get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_RENMOV_MULTI:
				begin
					ThreadSkipListRenMov.AddOrSetValue(GetCurrentThreadID, false);
					ThreadCanAbortRenMov.AddOrSetValue(GetCurrentThreadID, false);

					ThreadFsRemoveDirSkippedPath.Items[GetCurrentThreadID].Free;
					ThreadFsRemoveDirSkippedPath.AddOrSetValue(GetCurrentThreadID, nil);

					if RealPath.IsInAccount() and SettingsManager.Settings.LogUserSpace then
						ConnectionManager.Get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_DELETE:
				begin
					ThreadSkipListDelete.AddOrSetValue(GetCurrentThreadID(), false);
					if RealPath.IsInAccount() and SettingsManager.Settings.LogUserSpace then
						ConnectionManager.Get(RealPath.account, getResult).logUserSpaceInfo;
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
					if RealPath.IsInAccount() and SettingsManager.Settings.LogUserSpace then
						ConnectionManager.Get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_SYNC_PUT:
				begin
					if RealPath.IsInAccount() and SettingsManager.Settings.LogUserSpace then
						ConnectionManager.Get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_SYNC_DELETE:
				begin
					if RealPath.IsInAccount() and SettingsManager.Settings.LogUserSpace then
						ConnectionManager.Get(RealPath.account, getResult).logUserSpaceInfo;
				end;
			FS_STATUS_OP_GET_MULTI_THREAD:
				begin
					if RealPath.IsInAccount() and SettingsManager.Settings.LogUserSpace then
						ConnectionManager.Get(RealPath.account, getResult).logUserSpaceInfo;
					if not ThreadBackgroundJobs.TryGetValue(RealPath.account, BackgroundJobsCount) then
						BackgroundJobsCount := 0;
					ThreadBackgroundJobs.AddOrSetValue(RealPath.account, BackgroundJobsCount - 1);
					ThreadBackgroundThreads.Remove(GetCurrentThreadID());

				end;
			FS_STATUS_OP_PUT_MULTI_THREAD:
				begin
					if RealPath.IsInAccount() and SettingsManager.Settings.LogUserSpace then
						ConnectionManager.Get(RealPath.account, getResult).logUserSpaceInfo;
					if not ThreadBackgroundJobs.TryGetValue(RealPath.account, BackgroundJobsCount) then
						BackgroundJobsCount := 0;
					ThreadBackgroundJobs.AddOrSetValue(RealPath.account, BackgroundJobsCount - 1);
					ThreadBackgroundThreads.Remove(GetCurrentThreadID());
				end;
		end;
		exit;
	end;
end;

function TMailRuCloudWFX.GetRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
var
	getResult: Integer;
	Item: TCMRDirItem;
	Cloud: TCloudMailRu;
	resultHash: WideString;
begin
	if (SettingsManager.Settings.CheckCRC) then
		resultHash := EmptyWideStr
	else
		resultHash := 'dummy'; //calculations will be ignored if variable is not empty
	Cloud := ConnectionManager.Get(RemotePath.account, getResult);

	Result := Cloud.getFile(WideString(RemotePath.Path), LocalName, resultHash);

	if Result = FS_FILE_OK then
	begin

		Item := FindListingItemByPath(CurrentListing, RemotePath);
		{Дополнительно проверим CRC скачанного файла}
		if SettingsManager.Settings.CheckCRC then
		begin
			if (resultHash <> EmptyWideStr) and (Item.hash <> resultHash) then
				exit(FS_FILE_READERROR);
		end;

		if SettingsManager.Settings.PreserveFileTime then
		begin
			if Item.mtime <> 0 then
				SetAllFileTime(ExpandUNCFileName(LocalName), DateTimeToFileTime(UnixToDateTime(Item.mtime)));
		end;
		if CheckFlag(FS_COPYFLAGS_MOVE, CopyFlags) then
		begin
			Cloud.deleteFile(RemotePath.Path);
			if (SettingsManager.Settings.DescriptionTrackCloudFS and AccountSettings.GetAccountSettings(RemotePath.account).IsRemoteDescriptionsSupported) then
				DeleteRemoteFileDescription(RemotePath, Cloud);
		end;
		TCProgress.Progress(PWideChar(LocalName), PWideChar(RemoteName), 100);
		TCLogger.Log(LOG_LEVEL_FILE_OPERATION, MSGTYPE_TRANSFERCOMPLETE, '%s -> %s', [RemoteName, LocalName]);

		if SettingsManager.Settings.DescriptionCopyFromCloud then
			UpdateFileDescription(RemotePath, LocalName, Cloud);

	end;
end;

function TMailRuCloudWFX.PutRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
var
	getResult: Integer;
	Cloud: TCloudMailRu;
begin
	Cloud := ConnectionManager.Get(RemotePath.account, getResult);

	Result := Cloud.putFile(WideString(LocalName), RemotePath.Path);
	if Result = FS_FILE_OK then
	begin
		TCProgress.Progress(PWideChar(LocalName), PWideChar(RemotePath.Path), 100);
		TCLogger.Log(LOG_LEVEL_FILE_OPERATION, MSGTYPE_TRANSFERCOMPLETE, '%s -> %s', [LocalName, RemoteName]);
		if CheckFlag(FS_COPYFLAGS_MOVE, CopyFlags) then
			Result := DeleteLocalFile(LocalName);
		if (SettingsManager.Settings.DescriptionCopyToCloud and AccountSettings.GetAccountSettings(RemotePath.account).IsRemoteDescriptionsSupported) then
			UpdateRemoteFileDescription(RemotePath, LocalName, Cloud);
	end;
end;

{Assume the operation is inside of the same cloud instance - plugin does not support direct operations between different accounts}
procedure TMailRuCloudWFX.RenameRemoteFileDescription(OldRemotePath, NewRemotePath: TRealPath; var Cloud: TCloudMailRu);
var
	OldDescriptions, NewDescriptions: TDescription;
	OldRemoteIonPath, NewRemoteIonPath, OldLocalTempPath, NewLocalTempPath: WideString;
	NewRemoteIonExists: Boolean;
	OldItem, NewItem: WideString;
begin
	OldItem := ExtractFileName(OldRemotePath.Path);
	NewItem := ExtractFileName(NewRemotePath.Path);
	OldRemoteIonPath := IncludeTrailingBackslash(ExtractFileDir(OldRemotePath.Path)) + SettingsManager.Settings.DescriptionFileName;
	NewRemoteIonPath := IncludeTrailingBackslash(ExtractFileDir(NewRemotePath.Path)) + SettingsManager.Settings.DescriptionFileName;
	OldLocalTempPath := GetTmpFileName('ion');
	NewLocalTempPath := GetTmpFileName('ion');

	if ExtractFileDir(OldRemotePath.Path) = ExtractFileDir(NewRemotePath.Path) then //переименование внутри одного файла
	begin
		if not Cloud.getDescriptionFile(OldRemoteIonPath, OldLocalTempPath) then
			exit; //описания нет, переносить нечего
		OldDescriptions := TDescription.Create(OldLocalTempPath, GetTCCommentPreferredFormat);
		OldDescriptions.Read;
		if (OldDescriptions.RenameItem(OldItem, NewItem)) then //метод сам проверит существование описания
		begin
			OldDescriptions.Write();
			Cloud.deleteFile(OldRemoteIonPath);
			Cloud.PutDescriptionFile(OldRemoteIonPath, OldDescriptions.ionFilename);
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

		NewDescriptions.SetValue(ExtractFileName(NewRemotePath.Path), OldDescriptions.GetValue(ExtractFileName(OldRemotePath.Path)));
		OldDescriptions.DeleteValue(ExtractFileName(OldRemotePath.Path));
		OldDescriptions.Write();
		NewDescriptions.Write();
		Cloud.deleteFile(OldRemoteIonPath);
		Cloud.PutDescriptionFile(OldRemoteIonPath, OldDescriptions.ionFilename);
		if NewRemoteIonExists then
			Cloud.deleteFile(NewRemoteIonPath); //Если файл существовал ранее, его нужно удалить для последующей записи на его место
		Cloud.PutDescriptionFile(NewRemoteIonPath, NewDescriptions.ionFilename);
		OldDescriptions.Destroy;
		NewDescriptions.Destroy;
	end;
end;

function TMailRuCloudWFX.RenMoveFileViaHash(OldCloud, NewCloud: TCloudMailRu; OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean): Integer;
var
	CurrentItem: TCMRDirItem;
	RetryAttempts: Integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if OverWrite and not(NewCloud.deleteFile(NewRealPath.Path)) then
		exit;
	if OldCloud.statusFile(OldRealPath.Path, CurrentItem) then
	begin
		Result := NewCloud.addFileByIdentity(CurrentItem, IncludeTrailingPathDelimiter(ExtractFileDir(NewRealPath.Path)) + ExtractFileName(NewRealPath.Path));
		if not(Result in [FS_FILE_OK, FS_FILE_EXISTS]) then
		begin

			case SettingsManager.Settings.OperationErrorMode of
				OperationErrorModeAsk:
					begin
						while (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
						begin
							case (MsgBox(ERR_CLONE_FILE_ASK, [TCloudMailRu.ErrorCodeText(Result)], ERR_OPERATION, MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
								ID_ABORT:
									Result := FS_FILE_USERABORT;
								ID_RETRY:
									Result := NewCloud.addFileByIdentity(CurrentItem, IncludeTrailingPathDelimiter(ExtractFileDir(NewRealPath.Path)) + CurrentItem.name);
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
						RetryAttempts := SettingsManager.Settings.RetryAttempts;
						while (ThreadRetryCountRenMov.Items[GetCurrentThreadID()] <> RetryAttempts) and (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
						begin
							ThreadRetryCountRenMov.Items[GetCurrentThreadID()] := ThreadRetryCountRenMov.Items[GetCurrentThreadID()] + 1;
							TCLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, CLONE_FILE_RETRY, [TCloudMailRu.ErrorCodeText(Result), ThreadRetryCountRenMov.Items[GetCurrentThreadID()], RetryAttempts]);
							Result := NewCloud.addFileByIdentity(CurrentItem, IncludeTrailingPathDelimiter(ExtractFileDir(NewRealPath.Path)) + ExtractFileName(NewRealPath.Path));
							if TCProgress.Aborted() then
								Result := FS_FILE_USERABORT;
							if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then
								ThreadRetryCountRenMov.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
							ProcessMessages;
							Sleep(SettingsManager.Settings.AttemptWait);
						end;
					end;
			end;
		end;

		if (Result = CLOUD_OPERATION_OK) and Move and not(OldCloud.deleteFile(OldRealPath.Path)) then
			TCLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_DELETE, [CurrentItem.home]); //пишем в лог, но не отваливаемся
	end;
end;

function TMailRuCloudWFX.RenMoveFileViaPublicLink(OldCloud, NewCloud: TCloudMailRu; OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean): Integer;
var
	NeedUnpublish: Boolean;
	CurrentItem: TCMRDirItem;
	RetryAttempts: Integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	NeedUnpublish := false;
	if OverWrite and not(NewCloud.deleteFile(NewRealPath.Path)) then
		exit;

	if OldCloud.statusFile(OldRealPath.Path, CurrentItem) then
	begin
		if not CurrentItem.isPublished then //create temporary weblink
		begin
			NeedUnpublish := true;
			if not(OldCloud.publishFile(CurrentItem.home, CurrentItem.weblink)) then //problem publishing
			begin
				TCLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_GET_TEMP_PUBLIC_LINK, [CurrentItem.home]);
				exit(FS_FILE_READERROR);
			end;
		end;
		Result := CloneWeblink(NewCloud, OldCloud, NewRealPath.Path, CurrentItem, NeedUnpublish);
		if not(Result in [FS_FILE_OK, FS_FILE_EXISTS]) then
		begin

			case SettingsManager.Settings.OperationErrorMode of
				OperationErrorModeAsk:
					begin

						while (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
						begin
							case (MsgBox(ERR_PUBLISH_FILE_ASK, [TCloudMailRu.ErrorCodeText(Result)], ERR_PUBLISH_FILE, MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
								ID_ABORT:
									Result := FS_FILE_USERABORT;
								ID_RETRY:
									Result := CloneWeblink(NewCloud, OldCloud, NewRealPath.Path, CurrentItem, NeedUnpublish);
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
						RetryAttempts := SettingsManager.Settings.RetryAttempts;
						while (ThreadRetryCountRenMov.Items[GetCurrentThreadID()] <> RetryAttempts) and (not(Result in [FS_FILE_OK, FS_FILE_USERABORT])) do
						begin
							ThreadRetryCountRenMov.Items[GetCurrentThreadID()] := ThreadRetryCountRenMov.Items[GetCurrentThreadID()] + 1;
							TCLogger.Log(LOG_LEVEL_DETAIL, msgtype_details, PUBLISH_FILE_RETRY, [TCloudMailRu.ErrorCodeText(Result), ThreadRetryCountRenMov.Items[GetCurrentThreadID()], RetryAttempts]);
							Result := CloneWeblink(NewCloud, OldCloud, NewRealPath.Path, CurrentItem, NeedUnpublish);
							if TCProgress.Aborted() then
								Result := FS_FILE_USERABORT;
							if (Result in [FS_FILE_OK, FS_FILE_USERABORT]) then
								ThreadRetryCountRenMov.Items[GetCurrentThreadID()] := 0; //сбросим счётчик попыток
							ProcessMessages;
							Sleep(SettingsManager.Settings.AttemptWait);
						end;
					end;
			end;
		end;

		if (Result = CLOUD_OPERATION_OK) and Move and not(OldCloud.deleteFile(OldRealPath.Path)) then
			TCLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_DELETE, [CurrentItem.home]); //пишем в лог, но не отваливаемся
	end;
end;

procedure TMailRuCloudWFX.UpdateFileDescription(RemotePath: TRealPath; LocalFilePath: WideString; var Cloud: TCloudMailRu);
var
	RemoteDescriptions, LocalDescriptions: TDescription;
	RemoteIonPath, LocalTempPath: WideString;
	RemoteIonExists: Boolean;
begin
	RemoteIonPath := IncludeTrailingBackslash(ExtractFileDir(RemotePath.Path)) + SettingsManager.Settings.DescriptionFileName;
	LocalTempPath := GetTmpFileName('ion');

	RemoteIonExists := Cloud.getDescriptionFile(RemoteIonPath, LocalTempPath);
	if not RemoteIonExists then
		exit; //удалённого файла описаний нет

	RemoteDescriptions := TDescription.Create(LocalTempPath, GetTCCommentPreferredFormat);
	RemoteDescriptions.Read;
	LocalDescriptions := TDescription.Create(IncludeTrailingPathDelimiter(ExtractFileDir(LocalFilePath)) + SettingsManager.Settings.DescriptionFileName, GetTCCommentPreferredFormat); //open local ion file
	LocalDescriptions.Read;
	LocalDescriptions.CopyFrom(RemoteDescriptions, ExtractFileName(LocalFilePath));
	LocalDescriptions.Write();
	LocalDescriptions.Destroy;
	RemoteDescriptions.Destroy
end;

procedure TMailRuCloudWFX.UpdateRemoteFileDescription(RemotePath: TRealPath; LocalFilePath: WideString; var Cloud: TCloudMailRu);
var
	RemoteDescriptions, LocalDescriptions: TDescription;
	RemoteIonPath, LocalIonPath, LocalTempPath: WideString;
	RemoteIonExists: Boolean;
begin
	RemoteIonPath := IncludeTrailingBackslash(ExtractFileDir(RemotePath.Path)) + SettingsManager.Settings.DescriptionFileName;
	LocalIonPath := IncludeTrailingBackslash(ExtractFileDir(LocalFilePath)) + SettingsManager.Settings.DescriptionFileName;
	LocalTempPath := GetTmpFileName('ion');

	if (not FileExists(GetUNCFilePath(LocalIonPath))) then
		exit; //Файла описаний нет, не паримся

	LocalDescriptions := TDescription.Create(LocalIonPath, GetTCCommentPreferredFormat);
	LocalDescriptions.Read;

	RemoteIonExists := Cloud.getDescriptionFile(RemoteIonPath, LocalTempPath);
	RemoteDescriptions := TDescription.Create(LocalTempPath, GetTCCommentPreferredFormat);
	if RemoteIonExists then
		RemoteDescriptions.Read; //если был прежний файл - его надо перечитать

	RemoteDescriptions.CopyFrom(LocalDescriptions, ExtractFileName(RemotePath.Path));
	RemoteDescriptions.Write();
	if RemoteIonExists then
		Cloud.deleteFile(RemoteIonPath); //Приходится удалять, потому что не знаем, как переписать

	Cloud.PutDescriptionFile(RemoteIonPath, RemoteDescriptions.ionFilename);

	RemoteDescriptions.Destroy;
	LocalDescriptions.Destroy
end;

end.
