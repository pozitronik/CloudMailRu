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
	IPasswordManagerInterface,
	TCPasswordManager,
	ILoggerInterface,
	IProgressInterface,
	IRequestInterface,
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
	StringHelper,
	PluginHelper,
	FileHelper,
	IconHelper,
	SystemHelper,
	StreamingSettings,
	IPasswordUIProviderInterface,
	PasswordUIProvider,
	IHTTPManagerInterface,
	HTTPManager,
	ICipherValidatorInterface,
	CipherValidator,
	IFileSystemInterface,
	WindowsFileSystem,
	IConfigFileInterface,
	IniConfigFile,
	IThreadStateManagerInterface,
	ThreadStateManager,
	IContentFieldProviderInterface,
	ContentFieldProvider,
	IIconProviderInterface,
	IconProvider,
	IOperationLifecycleInterface,
	OperationLifecycleHandler,
	ICloudDescriptionOpsInterface,
	IDescriptionSyncManagerInterface,
	DescriptionSyncManager,
	CloudDescriptionOpsAdapter,
	IRetryHandlerInterface,
	RetryHandler,
	ICommandDispatcherInterface,
	CommandDispatcher,
	IListingProviderInterface,
	ListingProvider,
	IDescriptionSyncGuardInterface,
	DescriptionSyncGuard;

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
		FThreadState: IThreadStateManager;
		FContentFieldProvider: IContentFieldProvider;
		FIconProvider: IIconProvider;
		FOperationLifecycle: IOperationLifecycleHandler;
		FDescriptionSync: IDescriptionSyncManager;
		FDescriptionSyncGuard: IDescriptionSyncGuard;
		FRetryHandler: IRetryHandler;
		FCommandDispatcher: ICommandDispatcher;
		FListingProvider: IListingProvider;

		PluginNum: Integer;

		SettingsManager: TPluginSettingsManager;
		AccountSettings: TAccountsManager;
		Accounts: TWSList;

		CurrentListing: TCMRDirItemList;
		CurrentIncomingInvitesListing: TCMRIncomingInviteList;
		ConnectionManager: TConnectionManager;
		CurrentDescriptions: TDescription;
		PasswordManager: IPasswordManager;
		PasswordUI: IPasswordUIProvider;
		HTTPMgr: IHTTPManager;
		CipherVal: ICipherValidator;
		TCLogger: ILogger;
		TCProgress: IProgress;
		TCRequest: IRequest;
		FFileSystem: IFileSystem;
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
		function GetRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
		function PutRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
		function CloneWeblink(NewCloud, OldCloud: TCloudMailRu; CloudPath: WideString; CurrentItem: TCMRDirItem; NeedUnpublish: Boolean): Integer;
		function RenMoveFileViaHash(OldCloud, NewCloud: TCloudMailRu; OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean): Integer;
		function RenMoveFileViaPublicLink(OldCloud, NewCloud: TCloudMailRu; OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean): Integer;
		procedure ExecuteOperationActions(Actions: TOperationActions; const RealPath: TRealPath; Operation: Integer);
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

uses
	TCLogger;

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
	FThreadState := TThreadStateManager.Create;
	FContentFieldProvider := TContentFieldProvider.Create;
	FIconProvider := TIconProvider.Create;
	FOperationLifecycle := TOperationLifecycleHandler.Create;
	FListingProvider := TListingProvider.Create;

	AccountSettings := TAccountsManager.Create(TIniConfigFile.Create(SettingsManager.AccountsIniFilePath));
	FFileSystem := TWindowsFileSystem.Create;
	FDescriptionSync := TDescriptionSyncManager.Create(SettingsManager.Settings.DescriptionFileName, FFileSystem);
	FDescriptionSyncGuard := TDescriptionSyncGuard.Create(FDescriptionSync, SettingsManager, AccountSettings);
end;

function TMailRuCloudWFX.FsInit(PluginNr: Integer; pProgressProc: TProgressProcW; pLogProc: TLogProcW; pRequestProc: TRequestProcW): Integer;
var
	Logger: ILogger;
begin
	PluginNum := PluginNr;
	TCLogger := TTCLogger.Create(pLogProc, PluginNr, SettingsManager.Settings.LogLevel);
	TCProgress := TTCProgress.Create(pProgressProc, PluginNr);
	TCRequest := TTCRequest.Create(pRequestProc, PluginNr);
	CurrentDescriptions := TDescription.Create(GetTmpFileName(DESCRIPTION_TEMP_EXT), FFileSystem, GetTCCommentPreferredFormat);

	{Create retry handler with log callback that uses TCLogger}
	Logger := TCLogger;
	FRetryHandler := TRetryHandler.Create(FThreadState, SettingsManager, nil,
		procedure(LogLevel, MsgType: Integer; const Msg: WideString; const Args: array of const)
		begin
			Logger.Log(LogLevel, MsgType, Msg, Args);
		end
	);
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

destructor TMailRuCloudWFX.Destroy;
begin
	FThreadState := nil; {IThreadStateManager is reference-counted, setting to nil releases it}
	FRetryHandler := nil;
	FCommandDispatcher := nil;
	FListingProvider := nil;
	FDescriptionSyncGuard := nil;
	FDescriptionSync := nil;
	FreeAndNil(ConnectionManager);

	CurrentDescriptions.Free;

	SettingsManager.Free;
	AccountSettings.Free;
	PasswordManager := nil; {IPasswordManager is reference-counted, setting to nil releases it}
	TCLogger := nil; {ILogger is reference-counted, setting to nil releases it}
	TCProgress := nil; {IProgress is reference-counted, setting to nil releases it}
	TCRequest := nil; {IRequest is reference-counted, setting to nil releases it}
	inherited;
end;

function TMailRuCloudWFX.ExecCommand(RemoteName: PWideChar; Command, Parameter: WideString): Integer;
var
	CmdResult: TCommandResult;
begin
	CmdResult := FCommandDispatcher.Execute(RemoteName, Command, Parameter);
	Result := CmdResult.ResultCode;

	{Update RemoteName for symlink navigation commands}
	if CmdResult.ResultCode = FS_EXEC_SYMLINK then
		strpcopy(RemoteName, CmdResult.SymlinkPath);
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

	PostMessage(MainWin, WM_USER + TC_REFRESH_MESSAGE, TC_REFRESH_PARAM, 0); //TC does not update current panel, so we should do it this way
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
		if (Cloud.statusFile(RealPath.Path, CurrentItem)) and (idContinue = TPropertyForm.ShowProperty(MainWin, RealPath.Path, CurrentItem, Cloud, FFileSystem, SettingsManager.Settings.DownloadLinksEncode, SettingsManager.Settings.AutoUpdateDownloadListing, SettingsManager.Settings.DescriptionEnabled, SettingsManager.Settings.DescriptionEditorEnabled, SettingsManager.Settings.DescriptionFileName)) then
			PostMessage(MainWin, WM_USER + TC_REFRESH_MESSAGE, TC_REFRESH_PARAM, 0); //refresh tc panel if description edited
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
				TPropertyForm.ShowProperty(MainWin, RealPath.Path, CurrentItem, Cloud, FFileSystem, SettingsManager.Settings.DownloadLinksEncode, SettingsManager.Settings.AutoUpdateDownloadListing, false, false, SettingsManager.Settings.DescriptionFileName)
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

	PostMessage(MainWin, WM_USER + TC_REFRESH_MESSAGE, TC_REFRESH_PARAM, 0); //TC does not update current panel, so we should do it this way
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
	Result := FContentFieldProvider.GetSupportedField(FieldIndex, FieldName, MaxLen);
end;

function TMailRuCloudWFX.FsContentGetValue(FileName: PWideChar; FieldIndex, UnitIndex: Integer; FieldValue: Pointer; MaxLen, Flags: Integer): Integer;
var
	Item: TCMRDirItem;
	RealPath: TRealPath;
	Context: TContentFieldContext;
begin
	RealPath.FromPath(FileName);

	{ Build context for the provider }
	Context.IsAccountRoot := RealPath.isInAccountsList;
	Context.DescriptionsEnabled := SettingsManager.Settings.DescriptionEnabled;
	if Context.IsAccountRoot then
		Context.AccountDescription := AccountSettings.GetAccountSettings(RealPath.account).Description
	else
		Context.AccountDescription := '';

	{ Account root only supports description field via context }
	if Context.IsAccountRoot then
	begin
		Item := Default(TCMRDirItem);
		Result := FContentFieldProvider.GetValue(FieldIndex, Item, FieldValue, Context);
		exit;
	end;

	{ Find the item for regular paths }
	Item := FindListingItemByPath(CurrentListing, RealPath, not RealPath.invitesDir);
	Context.FileDescription := CurrentDescriptions.GetValue(Item.name);

	Result := FContentFieldProvider.GetValue(FieldIndex, Item, FieldValue, Context);
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
	if Result then
		FDescriptionSyncGuard.OnFileDeleted(RealPath, Cloud);
end;

function TMailRuCloudWFX.FsDisconnect(DisconnectRoot: PWideChar): Boolean;
begin
	if not FThreadState.HasActiveBackgroundJobs(ExtractFileName(DisconnectRoot)) then
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
	Context: TIconContext;
	IconInfo: TIconInfo;
	IconsSize: Integer;
	FrontIcon, BackIcon: hIcon;

	function GetFolderIconSize(Size: Integer): Integer;
	begin
		if Size <= 16 then exit(IconSizeSmall);
		if Size <= 32 then exit(IconSizeNormal);
		exit(IconSizeLarge);
	end;
begin
	Result := FS_ICON_EXTRACTED;
	RealPath.FromPath(RemoteName);
	IconsSize := GetTCIconsSize;

	{ Build context for provider }
	Context.IconsMode := SettingsManager.Settings.IconsMode;
	Context.HasItem := False;
	Context.HasInviteItem := False;
	Context.IsPublicAccount := False;

	if RealPath.isInAccountsList and not RealPath.isVirtual then
		Context.IsPublicAccount := AccountSettings.GetAccountSettings(
			copy(RemoteName, 2, StrLen(RemoteName) - 2)).PublicAccount;

	if RealPath.invitesDir and not RealPath.isInAccountsList then
	begin
		Context.InviteItem := FindIncomingInviteItemByPath(CurrentIncomingInvitesListing, RealPath);
		Context.HasInviteItem := True;
	end
	else if not RealPath.isInAccountsList and not RealPath.isVirtual then
	begin
		Context.Item := FindListingItemByPath(CurrentListing, RealPath);
		Context.HasItem := True;
	end;

	{ Get icon info from provider }
	IconInfo := FIconProvider.GetIcon(RealPath, Context);

	{ Render based on icon type }
	case IconInfo.IconType of
		itUseDefault:
			exit(FS_ICON_USEDEFAULT);

		itSystemTrash:
			begin
				TheIcon := GetSystemIcon(GetFolderIconSize(IconsSize));
				exit(FS_ICON_EXTRACTED_DESTROY);
			end;

		itInternal:
			begin
				strpcopy(RemoteName, IconInfo.IconName);
				TheIcon := LoadImageW(hInstance, RemoteName, IMAGE_ICON, IconsSize, IconsSize, LR_DEFAULTCOLOR);
			end;

		itInternalOverlay:
			begin
				strpcopy(RemoteName, IconInfo.IconName);
				FrontIcon := LoadImageW(hInstance, RemoteName, IMAGE_ICON, IconsSize, IconsSize, LR_DEFAULTCOLOR);
				BackIcon := GetFolderIcon(GetFolderIconSize(IconsSize));
				TheIcon := CombineIcons(FrontIcon, BackIcon);
				DeleteObject(FrontIcon);
				DeleteObject(BackIcon);
				exit(FS_ICON_EXTRACTED_DESTROY);
			end;

		itExternal:
			begin
				TheIcon := LoadPluginIcon(PluginPath + 'icons', IconInfo.IconName);
				if TheIcon = INVALID_HANDLE_VALUE then
					exit(FS_ICON_USEDEFAULT);
				exit(FS_ICON_EXTRACTED_DESTROY);
			end;

		itExternalOverlay:
			begin
				TheIcon := LoadPluginIcon(PluginPath + 'icons', IconInfo.IconName);
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
var //Получение первого файла в папке. Result не используется (можно использовать для работы плагина).
	RealPath: TRealPath;
	getResult: Integer;
	SkipListDelete, SkipListRenMov, CanAbortRenMov, RenMovAborted: Boolean;
	CurrentItem: TCMRDirItem;
	CurrentCloud: TCloudMailRu;
begin
	SkipListDelete := FThreadState.GetSkipListDelete;
	SkipListRenMov := FThreadState.GetSkipListRenMov;
	CanAbortRenMov := FThreadState.GetCanAbortRenMov;

	if (CanAbortRenMov and TCProgress.Progress(Path, 0)) then
	begin
		FThreadState.SetListingAborted(True);
		RenMovAborted := true;
	end
	else
		RenMovAborted := false;

	if SkipListDelete or SkipListRenMov or RenMovAborted then
	begin
		CurrentListing := [];
		SetLastError(ERROR_NO_MORE_FILES);
		Result := FIND_NO_MORE_FILES;
	end else begin
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

			if not FListingProvider.FetchListing(CurrentCloud, RealPath, CurrentListing, CurrentIncomingInvitesListing) then
				SetLastError(ERROR_PATH_NOT_FOUND);

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

	Result := FRetryHandler.HandleOperationError(Result, rotDownload, ERR_DOWNLOAD_FILE_ASK, ERR_DOWNLOAD, DOWNLOAD_FILE_RETRY, RemoteName,
		function: Integer
		begin
			Result := GetRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
		end,
		function: Boolean
		begin
			Result := TCProgress.Progress(PWideChar(LocalName), RemoteName, 0);
		end
	);
end;

function TMailRuCloudWFX.FsMkDir(Path: WideString): Boolean;
var
	RealPath: TRealPath;
	getResult: Integer;
	SkipListRenMov: Boolean;
	OperationContextId: Integer;
	RegisteredAccount: TAccountSettings;
begin
	SkipListRenMov := FThreadState.GetSkipListRenMov;
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
		OperationContextId := FThreadState.GetFsStatusInfo;
		if OperationContextId = FS_STATUS_OP_RENMOV_MULTI then
			CurrentlyMovedDir := RealPath;
	end;
end;

function TMailRuCloudWFX.FsPutFile(LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
var
	RealPath: TRealPath;
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

	if Result <> FS_FILE_WRITEERROR then
		exit;

	Result := FRetryHandler.HandleOperationError(Result, rotUpload, ERR_UPLOAD_FILE_ASK, ERR_UPLOAD, UPLOAD_FILE_RETRY, LocalName, function: Integer
		begin
			Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
		end,
		function: Boolean
		begin
			Result := TCProgress.Progress(PWideChar(LocalName), RemoteName, 0);
		end
	);
end;

function TMailRuCloudWFX.FsRemoveDir(RemoteName: WideString): Boolean;
var
	RealPath: TRealPath;
	getResult: Integer;
	ListingAborted: Boolean;
	Cloud: TCloudMailRu;
	OperationContextId: Integer;
begin
	if FThreadState.IsPathSkipped(RemoteName) then
		exit(false);
	ListingAborted := FThreadState.GetListingAborted;
	if ListingAborted then
	begin
		FThreadState.SetListingAborted(False);
		exit(false);
	end;
	RealPath.FromPath(WideString(RemoteName));
	if RealPath.isVirtual then
		exit(false);
	Cloud := ConnectionManager.Get(RealPath.account, getResult);
	Result := Cloud.removeDir(RealPath.Path);

	if Result then
	begin
		OperationContextId := FThreadState.GetFsStatusInfo; {Directory can be deleted after moving operation}
		if OperationContextId = FS_STATUS_OP_RENMOV_MULTI then
			FDescriptionSyncGuard.OnFileRenamed(RealPath, CurrentlyMovedDir, Cloud)
		else
			FDescriptionSyncGuard.OnFileDeleted(RealPath, Cloud);
	end;
end;

function TMailRuCloudWFX.FsRenMovFile(OldName, NewName: PWideChar; Move, OverWrite: Boolean; ri: pRemoteInfo): Integer;
var
	OldRealPath: TRealPath;
	NewRealPath: TRealPath;
	getResult: Integer;
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
			if (FS_FILE_EXISTS = Result) and FThreadState.HasRemoveDirSkippedPath then //TC сразу же попытается удалить каталог, чтобы избежать этого - внесем путь в своеобразный блеклист
			begin
				FThreadState.AddSkippedPath(OldRealPath.ToPath);
			end else if (FS_FILE_OK = Result) and FThreadState.HasRemoveDirSkippedPath then
			begin //Вытащим из блеклиста, если решили перезаписать
				FThreadState.RemoveSkippedPath(OldRealPath.ToPath);
			end;
			if (FS_FILE_OK = Result) then
				FDescriptionSyncGuard.OnFileRenamed(OldRealPath, NewRealPath, OldCloud);
		end else begin
			Result := OldCloud.FileCopy(OldRealPath.Path, NewRealPath.Path);
		end;

	end;
	TCProgress.Progress(OldName, NewName, 100);
end;

procedure TMailRuCloudWFX.FsSetCryptCallback(PCryptProc: TCryptProcW; CryptoNr, Flags: Integer);
begin
	PasswordManager := TTCPasswordManager.Create(PCryptProc, PluginNum, CryptoNr, TCLogger);
	PasswordUI := TPasswordUIProvider.Create;
	HTTPMgr := THTTPManager.Create(SettingsManager.Settings.ConnectionSettings, TCLogger, TCProgress);
	CipherVal := TCipherValidator.Create;
	ConnectionManager := TConnectionManager.Create(SettingsManager, AccountSettings, HTTPMgr, PasswordUI, CipherVal, TWindowsFileSystem.Create, TCProgress, TCLogger, TCRequest, PasswordManager);
	FCommandDispatcher := TCommandDispatcher.Create(ConnectionManager, TCLogger, SettingsManager);
end;

procedure TMailRuCloudWFX.ExecuteOperationActions(Actions: TOperationActions; const RealPath: TRealPath; Operation: Integer);
var
	getResult: Integer;
begin
	{ Retry resets }
	if oaResetRetryDownload in Actions then
		FThreadState.ResetRetryCountDownload;
	if oaResetRetryUpload in Actions then
		FThreadState.ResetRetryCountUpload;
	if oaResetRetryRenMov in Actions then
		FThreadState.ResetRetryCountRenMov;

	{ Skip list flags }
	if oaSetSkipListDelete in Actions then
		FThreadState.SetSkipListDelete(True);
	if oaClearSkipListDelete in Actions then
		FThreadState.SetSkipListDelete(False);
	if oaSetSkipListRenMov in Actions then
		FThreadState.SetSkipListRenMov(True);
	if oaClearSkipListRenMov in Actions then
		FThreadState.SetSkipListRenMov(False);

	{ RenMov abort control }
	if oaSetCanAbortRenMov in Actions then
		FThreadState.SetCanAbortRenMov(True);
	if oaClearCanAbortRenMov in Actions then
		FThreadState.SetCanAbortRenMov(False);

	{ Skipped path management }
	if oaCreateSkippedPath in Actions then
		FThreadState.CreateRemoveDirSkippedPath;
	if oaClearSkippedPath in Actions then
		FThreadState.ClearRemoveDirSkippedPath;

	{ Background job tracking }
	if oaIncrementBackgroundJobs in Actions then
		FThreadState.IncrementBackgroundJobs(RealPath.account);
	if oaDecrementBackgroundJobs in Actions then
		FThreadState.DecrementBackgroundJobs(RealPath.account);

	{ Background thread status }
	if oaSetBackgroundThreadStatus in Actions then
		FThreadState.SetBackgroundThreadStatus(Operation);
	if oaRemoveBackgroundThread in Actions then
		FThreadState.RemoveBackgroundThread;

	{ User space logging }
	if oaLogUserSpaceInfo in Actions then
		ConnectionManager.Get(RealPath.account, getResult).logUserSpaceInfo;

	{ Description loading }
	if oaLoadDescriptions in Actions then
	begin
		if ConnectionManager.Get(RealPath.account, getResult).getDescriptionFile(
			IncludeTrailingBackslash(RealPath.Path) + SettingsManager.Settings.DescriptionFileName,
			CurrentDescriptions.ionFilename) then
			CurrentDescriptions.Read
		else
			CurrentDescriptions.Clear;
	end;

	{ Public account warning - also sets skip flag to prevent unsupported operations }
	if oaWarnPublicAccountCopy in Actions then
	begin
		TCLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, ERR_DIRECT_COPY_SUPPORT);
		FThreadState.SetSkipListRenMov(True);
	end;
end;

procedure TMailRuCloudWFX.FsStatusInfo(RemoteDir: WideString; InfoStartEnd, InfoOperation: Integer);
var
	RealPath: TRealPath;
	Context: TOperationContext;
	Actions: TOperationActions;
	getResult: Integer;
begin
	RealPath.FromPath(RemoteDir, ID_True); { RemoteDir always a directory }

	{ Build context for the handler }
	Context.Operation := InfoOperation;
	Context.IsInAccount := RealPath.IsInAccount();
	Context.DescriptionsEnabled := SettingsManager.Settings.DescriptionEnabled;
	Context.LogUserSpaceEnabled := SettingsManager.Settings.LogUserSpace;

	if Context.IsInAccount then { Clear intention despite it only needed for RENMOV_MULTI to show warning }
		Context.IsPublicAccount := ConnectionManager.Get(RealPath.account, getResult).IsPublicAccount
	else
		Context.IsPublicAccount := False;

	if InfoStartEnd = FS_STATUS_START then
	begin
		FThreadState.SetFsStatusInfo(InfoOperation);
		Actions := FOperationLifecycle.GetStartActions(Context);
		ExecuteOperationActions(Actions, RealPath, InfoOperation);
	end
	else if InfoStartEnd = FS_STATUS_END then
	begin
		FThreadState.RemoveFsStatusInfo;
		Actions := FOperationLifecycle.GetEndActions(Context);
		ExecuteOperationActions(Actions, RealPath, InfoOperation);
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
			FDescriptionSyncGuard.OnFileDeleted(RemotePath, Cloud);
		end;
		TCProgress.Progress(PWideChar(LocalName), PWideChar(RemoteName), 100);
		TCLogger.Log(LOG_LEVEL_FILE_OPERATION, MSGTYPE_TRANSFERCOMPLETE, '%s -> %s', [RemoteName, LocalName]);

		FDescriptionSyncGuard.OnFileDownloaded(RemotePath, LocalName, Cloud);

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
		FDescriptionSyncGuard.OnFileUploaded(RemotePath, LocalName, Cloud);
	end;
end;

{RenMoveFileViaHash and RenMoveFileViaPublicLink share similar error handling logic.
 Kept separate intentionally: context-specific error messages, ViaPublicLink has extra publish/unpublish logic,
 and extracting to generic callback pattern adds complexity for marginal benefit.}
function TMailRuCloudWFX.RenMoveFileViaHash(OldCloud, NewCloud: TCloudMailRu; OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean): Integer;
var
	CurrentItem: TCMRDirItem;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if OverWrite and not(NewCloud.deleteFile(NewRealPath.Path)) then
		exit;
	if OldCloud.statusFile(OldRealPath.Path, CurrentItem) then
	begin
		Result := NewCloud.addFileByIdentity(CurrentItem, IncludeTrailingPathDelimiter(ExtractFileDir(NewRealPath.Path)) + ExtractFileName(NewRealPath.Path));
		if not(Result in [FS_FILE_OK, FS_FILE_EXISTS]) then
			Result := FRetryHandler.HandleOperationError(Result, rotRenMov, ERR_CLONE_FILE_ASK, ERR_OPERATION, CLONE_FILE_RETRY, TCloudMailRu.ErrorCodeText(Result),
				function: Integer
				begin
					Result := NewCloud.addFileByIdentity(CurrentItem, IncludeTrailingPathDelimiter(ExtractFileDir(NewRealPath.Path)) + ExtractFileName(NewRealPath.Path));
				end,
				function: Boolean
				begin
					Result := TCProgress.Aborted();
				end
			);

		if (Result = CLOUD_OPERATION_OK) and Move and not(OldCloud.deleteFile(OldRealPath.Path)) then
			TCLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_DELETE, [CurrentItem.home]); //пишем в лог, но не отваливаемся
	end;
end;

function TMailRuCloudWFX.RenMoveFileViaPublicLink(OldCloud, NewCloud: TCloudMailRu; OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean): Integer;
var
	NeedUnpublish: Boolean;
	CurrentItem: TCMRDirItem;
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
			Result := FRetryHandler.HandleOperationError(Result, rotRenMov, ERR_PUBLISH_FILE_ASK, ERR_PUBLISH_FILE, PUBLISH_FILE_RETRY, TCloudMailRu.ErrorCodeText(Result),
			function: Integer
				begin
					Result := CloneWeblink(NewCloud, OldCloud, NewRealPath.Path, CurrentItem, NeedUnpublish);
				end,
				function: Boolean
				begin
					Result := TCProgress.Aborted();
				end
			);

		if (Result = CLOUD_OPERATION_OK) and Move and not(OldCloud.deleteFile(OldRealPath.Path)) then
			TCLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_DELETE, [CurrentItem.home]); //пишем в лог, но не отваливаемся
	end;
end;

end.
