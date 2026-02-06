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
	WFXTypes,
	RealPath,
	PluginSettingsManager,
	Accountsmanager,
	WSList,
	CloudConstants,
	LanguageStrings,
	SettingsConstants,
	CloudInviteList,
	CloudInvite,
	CloudDirItem,
	CloudDirItemList,
	CloudIncomingInviteList,
	ConnectionManager,
	ConnectionSettings,
	IdSSLOpenSSLHeaders,
	Description,
	PasswordManager,
	Logger,
	Progress,
	Request,
	PathHelper,
	CommandExecutor,
	TCHandler,
	CloudIncomingInvite,
	AccountSettings,
	Accounts,
	InviteProperty,
	RemoteProperty,
	DeletedProperty,
	Controls,
	Messages,
	StringHelper,
	IconHelper,
	StreamingSettings,
	PasswordUIProvider,
	HTTPManager,
	CipherValidator,
	CipherProfile,
	BCryptProvider,
	FileSystem,
	Environment,
	OAuthAppAuthStrategy,
	ConfigFile,
	ThreadStateManager,
	ContentFieldProvider,
	IconProvider,
	OperationLifecycleHandler,
	DescriptionSyncManager,
	CloudDescriptionOperationsAdapter,
	RetryHandler,
	CommandDispatcher,
	ListingProvider,
	DescriptionSyncGuard,
	TimestampSyncManager,
	TimestampSyncGuard,
	LocalFileDeletionHandler,
	DownloadSuccessHandler,
	OperationActionExecutor,
	ListingSkipDecider,
	ListingPathValidator,
	SameAccountMoveHandler,
	FileStreamExecutor,
	LocalFileConflictResolver,
	ListingItemFetcher,
	SharedItemDeletionHandler,
	TrashBinOperationHandler,
	InviteOperationHandler,
	CrossAccountFileOperationHandler,
	IconRenderingEngine,
	FileExecutionDispatcher,
	SharedItemActionHandler,
	MoveOperationContextTracker,
	DirectoryDeletionPreCheck,
	UploadPreparationValidator,
	DownloadPreparationValidator,
	UploadCompletionHandler,
	RootListingHandler,
	PathListingHandler,
	IconContextBuilder,
	OverwritePreparationHandler,
	OperationStatusContextBuilder,
	ListingResultApplier,
	DownloadOrchestrator,
	CloudMailRuFactory,
	OpenSSLProvider,
	SSLHandlerFactory,
	IndySSLHandlerFactory,
	IndySecSSLHandlerFactory,
	AccountCredentialsProvider,
	FileEncryptionResolver,
	ProxyPasswordResolver,
	CloudAuthorizationState,
	TranslationManager,
	ServerProfileManager;

type
	TWFXApplication = class(TInterfacedObject, IWFXInterface)
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
		FThreadState: IThreadStateManager;
		FMoveOperationTracker: IMoveOperationContextTracker;
		FDirectoryDeletionPreCheck: IDirectoryDeletionPreCheck;
		FUploadPreparationValidator: IUploadPreparationValidator;
		FDownloadPreparationValidator: IDownloadPreparationValidator;
		FContentFieldProvider: IContentFieldProvider;
		FIconProvider: IIconProvider;
		FOperationLifecycle: IOperationLifecycleHandler;
		FDescriptionSync: IDescriptionSyncManager;
		FDescriptionSyncGuard: IDescriptionSyncGuard;
		FTimestampSync: ITimestampSyncManager;
		FTimestampSyncGuard: ITimestampSyncGuard;
		FRetryHandler: IRetryHandler;
		FCommandDispatcher: ICommandDispatcher;
		FListingProvider: IListingProvider;
		FLocalFileDeletionHandler: ILocalFileDeletionHandler;
		FDownloadSuccessHandler: IDownloadSuccessHandler;
		FActionExecutor: IOperationActionExecutor;
		FListingSkipDecider: IListingSkipDecider;
		FListingPathValidator: IListingPathValidator;
		FSameAccountMoveHandler: ISameAccountMoveHandler;
		FFileStreamExecutor: IFileStreamExecutor;
		FLocalFileConflictResolver: ILocalFileConflictResolver;
		FListingItemFetcher: IListingItemFetcher;
		FSharedItemDeletionHandler: ISharedItemDeletionHandler;
		FTrashBinOperationHandler: ITrashBinOperationHandler;
		FInviteOperationHandler: IInviteOperationHandler;
		FCrossAccountFileOperationHandler: ICrossAccountFileOperationHandler;
		FIconRenderingEngine: IIconRenderingEngine;
		FIconContextBuilder: IIconContextBuilder;
		FFileExecutionDispatcher: IFileExecutionDispatcher;
		FSharedItemActionHandler: ISharedItemActionHandler;
		FUploadCompletionHandler: IUploadCompletionHandler;
		FRootListingHandler: IRootListingHandler;
		FPathListingHandler: IPathListingHandler;
		FOverwritePreparationHandler: IOverwritePreparationHandler;
		FOperationStatusContextBuilder: IOperationStatusContextBuilder;
		FListingResultApplier: IListingResultApplier;
		FDownloadOrchestrator: IDownloadOrchestrator;
		FTCHandler: ITCHandler;
		FOpenSSLProvider: IOpenSSLProvider;
		FSSLHandlerFactory: ISSLHandlerFactory;

		PluginNum: Integer;

		SettingsManager: IPluginSettingsManager;
		AccountSettings: IAccountsManager;
		Accounts: TWSList;

		CurrentListing: TCloudDirItemList;
		CurrentIncomingInvitesListing: TCloudIncomingInviteList;
		ConnectionManager: IConnectionManager;
		CurrentDescriptions: TDescription;
		PasswordManager: IPasswordManager;
		PasswordUI: IPasswordUIProvider;
		HTTPMgr: IHTTPManager;
		CipherVal: ICipherValidator;
		Logger: ILogger;
		Progress: IProgress;
		Request: IRequest;
		FFileSystem: IFileSystem;

		{Creates SSL handler factory based on backend setting.
			Auto mode tries IndySec first (for OpenSSL 3.x support), falls back to standard Indy.}
		function CreateSSLHandlerFactory(SSLBackend: Integer): ISSLHandlerFactory;
		procedure LoadTranslationOnStartup;
	protected
		{Ensures cloud is authorized. Returns True if authorized, False otherwise.
			Attempts authorization if not yet authorized. Sets LastError on failure.}
		function EnsureAuthorized(Cloud: TCloudMailRu): Boolean;
		function FindListingItemByPath(CurrentListing: TCloudDirItemList; Path: TRealPath; UpdateListing: Boolean = true): TCloudDirItem;
		function FindIncomingInviteItemByPath(InviteListing: TCloudIncomingInviteList; Path: TRealPath): TCloudIncomingInvite;
		function DeleteLocalFile(LocalName: WideString): Integer;
		function ExecTrashbinProperties(MainWin: THandle; RealPath: TRealPath): Integer;
		function ExecSharedAction(MainWin: THandle; RealPath: TRealPath; RemoteName: PWideChar; ActionOpen: Boolean = true): Integer;
		function ExecInvitesAction(MainWin: THandle; RealPath: TRealPath): Integer;
		function ExecProperties(MainWin: THandle; RealPath: TRealPath): Integer;
		function ExecCommand(RemoteName: PWideChar; Command: WideString; Parameter: WideString = ''): Integer;
		function ExecuteFileStream(RealPath: TRealPath; StreamingSettings: TStreamingSettings): Integer;
		function GetRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
		function PutRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
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
		function FsExecuteFile(MainWin: THandle; RemoteName: PWideChar; Verb: WideString): Integer;
		function FsGetFile(RemoteName, LocalName: WideString; CopyFlags: Integer; RemoteInfo: pRemoteInfo): Integer;
		function FsPutFile(LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
		function FsDeleteFile(RemoteName: WideString): Boolean;
		function FsMkDir(Path: WideString): Boolean;
		function FsRemoveDir(RemoteName: WideString): Boolean;
		function FsRenMovFile(OldName: WideString; NewName: WideString; Move: Boolean; OverWrite: Boolean; ri: pRemoteInfo): Integer;

		function FsDisconnect(DisconnectRoot: WideString): Boolean;

		{Content methods}
		function FsContentGetSupportedField(FieldIndex: Integer; FieldName: PAnsiChar; Units: PAnsiChar; MaxLen: Integer): Integer;
		function FsContentGetValue(FileName: WideString; FieldIndex: Integer; UnitIndex: Integer; FieldValue: Pointer; MaxLen: Integer; Flags: Integer): Integer;
		function FsExtractCustomIcon(RemoteName: PWideChar; ExtractFlags: Integer; var TheIcon: hIcon): Integer;

		{Thumbnail support - TC 7.0+}
		function FsGetPreviewBitmap(RemoteName: WideString; Width, Height: Integer; var ReturnedBitmap: HBITMAP): Integer;

		{Shutdown safety check - returns True if no background operations are active}
		function HasActiveOperations: Boolean;

	end;

implementation

{TWFXApplication}

function TWFXApplication.CreateSSLHandlerFactory(SSLBackend: Integer): ISSLHandlerFactory;
begin
	case SSLBackend of
		SSLBackendIndy:
			Result := TIndySSLHandlerFactory.Create;
		SSLBackendIndySec:
			Result := TIndySecSSLHandlerFactory.Create;
		else {SSLBackendAuto}
			{Auto-detection: prefer IndySec for OpenSSL 3.x support.
				Try IndySec first - it supports OpenSSL 1.1.x and 3.x with TLS 1.3.
				If it can load OpenSSL, use it. Otherwise fall back to standard Indy.}
			try
				Result := TIndySecSSLHandlerFactory.Create;
				{Check if OpenSSL loaded successfully by attempting to get handle}
				if Result.GetLibCryptoHandle = 0 then
				begin
					Result := nil;
					Result := TIndySSLHandlerFactory.Create;
				end;
			except
				{IndySec failed to initialize - fall back to standard Indy}
				Result := TIndySSLHandlerFactory.Create;
			end;
	end;
end;

procedure TWFXApplication.LoadTranslationOnStartup;
var
	Manager: TTranslationManager;
	ErrorMsg: WideString;
begin
	if SettingsManager.GetSettings.Language = '' then
		Exit;

	Manager := TTranslationManager.Create(TWindowsFileSystem.Create, PluginPath + 'language\');
	try
		Manager.Apply(SettingsManager.GetSettings.Language, ErrorMsg);
		{Silently ignore errors on startup}
	finally
		Manager.Free;
	end;
end;

constructor TWFXApplication.Create();
begin

	PluginPath := GetModuleName(hInstance);
	PluginPath := IncludeTrailingBackslash(ExtractFilePath(PluginPath));

	SettingsManager := TPluginSettingsManager.Create();

	{Load saved translation if configured}
	LoadTranslationOnStartup;

	{Configure Indy's OpenSSL library path for HTTPS connections}
	if SettingsManager.GetSettings.LoadSSLDLLOnlyFromPluginDir then
	begin
		if ((DirectoryExists(PluginPath + PlatformDllPath)) and (FileExists(PluginPath + PlatformDllPath + '\ssleay32.dll')) and (FileExists(PluginPath + PlatformDllPath + '\libeay32.dll'))) then
		begin {Try to load DLL from platform subdir}
			IdOpenSSLSetLibPath(PluginPath + PlatformDllPath);
		end else if ((FileExists(GetUNCFilePath(PluginPath + 'ssleay32.dll'))) and (FileExists(GetUNCFilePath(PluginPath + 'libeay32.dll')))) then
		begin {Else try to load it from plugin dir}
			IdOpenSSLSetLibPath(PluginPath);
		end;
	end;

	{Create centralized OpenSSL provider for hash calculation - respects same settings as Indy}
	FOpenSSLProvider := TOpenSSLProvider.Create(PluginPath, SettingsManager.GetSettings.LoadSSLDLLOnlyFromPluginDir);

	{Create SSL handler factory based on settings}
	FSSLHandlerFactory := CreateSSLHandlerFactory(SettingsManager.GetSettings.SSLBackend);

	{Register cipher profiles with available backends}
	TCipherProfileRegistry.Initialize(FOpenSSLProvider, TBCryptProvider.Create);

	IsMultiThread := not(SettingsManager.GetSettings.DisableMultiThreading);
	FThreadState := TThreadStateManager.Create;
	FMoveOperationTracker := TMoveOperationContextTracker.Create(FThreadState);
	FDirectoryDeletionPreCheck := TDirectoryDeletionPreCheck.Create(FThreadState);
	FUploadPreparationValidator := TUploadPreparationValidator.Create(
		function(const Path: WideString): Boolean
		begin
			Result := FileExists(Path);
		end);
	FDownloadPreparationValidator := TDownloadPreparationValidator.Create;
	FContentFieldProvider := TContentFieldProvider.Create;
	FIconProvider := TIconProvider.Create;
	FOperationLifecycle := TOperationLifecycleHandler.Create;
	FListingProvider := TListingProvider.Create;

	FFileSystem := TWindowsFileSystem.Create;
	FTCHandler := TTCHandler.Create(TWindowsEnvironment.Create);
	FDescriptionSync := TDescriptionSyncManager.Create(SettingsManager.GetSettings.DescriptionFileName, FFileSystem, FTCHandler);
	FDescriptionSyncGuard := TDescriptionSyncGuard.Create(FDescriptionSync, SettingsManager);
	FTimestampSync := TTimestampSyncManager.Create(SettingsManager.GetSettings.TimestampFileName, FFileSystem, SettingsManager.GetSettings.TimestampConflictMode);
	FTimestampSyncGuard := TTimestampSyncGuard.Create(FTimestampSync, SettingsManager);
end;

function TWFXApplication.FsInit(PluginNr: Integer; pProgressProc: TProgressProcW; pLogProc: TLogProcW; pRequestProc: TRequestProcW): Integer;
var
	Logger: ILogger;
begin
	PluginNum := PluginNr;
	Logger := TTCLogger.Create(pLogProc, PluginNr, SettingsManager.GetSettings.LogLevel);
	Progress := TTCProgress.Create(pProgressProc, PluginNr);
	Request := TTCRequest.Create(pRequestProc, PluginNr);
	CurrentDescriptions := TDescription.Create(FFileSystem.GetTmpFileName(DESCRIPTION_TEMP_EXT), FFileSystem, FTCHandler.GetTCCommentPreferredFormat);

	{Create retry handler with callbacks for message boxes and logging}
	Self.Logger := Logger;
	AccountSettings := TAccountsManager.Create(TIniConfigFile.Create(SettingsManager.GetAccountsIniFilePath), Logger);
	FRetryHandler := TRetryHandler.Create(FThreadState, SettingsManager, FTCHandler,
		function(const Text: WideString; const Args: array of const; const Caption: WideString; Flags: Integer): Integer
		begin
			Result := MessageBoxW(FTCHandler.FindTCWindow, PWideChar(Format(Text, Args)), PWideChar(Caption), Flags);
		end,
		procedure(LogLevel, MsgType: Integer; const Msg: WideString; const Args: array of const)
		begin
			Logger.Log(LogLevel, MsgType, Msg, Args);
		end);

	{Create local file deletion handler with callbacks for file operations and user dialogs}
	FLocalFileDeletionHandler := TLocalFileDeletionHandler.Create(SettingsManager, Logger,
		function(const Path: WideString): Boolean
		begin
			Result := DeleteFileW(PWideChar(Path));
		end,
		function(const Path: WideString): Integer
		begin
			Result := FileGetAttr(Path);
		end,
		function(const Path: WideString; Attr: Integer): Boolean
		begin
			Result := FileSetAttr(Path, Attr) = 0; {FileSetAttr returns 0 on success}
		end,
		function(const FileName: WideString): Integer
		begin
			Result := MessageBoxW(FTCHandler.FindTCWindow, PWideChar(Format(ERR_DELETE_FILE_ASK, [FileName])), PWideChar(ERR_DELETE_FILE), MB_ABORTRETRYIGNORE + MB_ICONQUESTION);
		end);

	{Create download success handler for post-download operations}
	FDownloadSuccessHandler := TDownloadSuccessHandler.Create(SettingsManager, Logger, Progress, FDescriptionSyncGuard, FTimestampSyncGuard, FFileSystem);

	{Create listing skip decider for FsFindFirst skip logic}
	FListingSkipDecider := TListingSkipDecider.Create(FThreadState, Progress);

	{Create listing path validator for FsFindFirst path validation}
	FListingPathValidator := TListingPathValidator.Create;

	{Create same-account move handler for FsRenMovFile}
	FSameAccountMoveHandler := TSameAccountMoveHandler.Create(FThreadState, FDescriptionSyncGuard, FTimestampSyncGuard);

	{Create file stream executor for ExecuteFileStream}
	FFileStreamExecutor := TFileStreamExecutor.Create(TPublicCloudFactory.Create, TWindowsCommandExecutor.Create);

	{Create local file conflict resolver for FsGetFile}
	FLocalFileConflictResolver := TLocalFileConflictResolver.Create(Logger);

	{Create listing item fetcher for FindListingItemByPath}
	FListingItemFetcher := TListingItemFetcher.Create(Logger);

	{Create shared item deletion handler for FsDeleteFile}
	FSharedItemDeletionHandler := TSharedItemDeletionHandler.Create;

	{Create trashbin operation handler for ExecTrashbinProperties}
	FTrashBinOperationHandler := TTrashBinOperationHandler.Create;

	{Create invite operation handler for ExecInvitesAction}
	FInviteOperationHandler := TInviteOperationHandler.Create;

	{Create cross-account file operation handler for FsRenMovFile}
	FCrossAccountFileOperationHandler := TCrossAccountFileOperationHandler.Create(FRetryHandler, Logger);

	{Create icon rendering engine for FsExtractCustomIcon}
	FIconRenderingEngine := TIconRenderingEngine.Create;

	{Create file execution dispatcher for FsExecuteFile routing}
	FFileExecutionDispatcher := TFileExecutionDispatcher.Create;

	{Create shared item action handler for ExecSharedAction}
	FSharedItemActionHandler := TSharedItemActionHandler.Create;

	{Create upload completion handler for PutRemoteFile}
	FUploadCompletionHandler := TUploadCompletionHandler.Create(Logger, Progress, FLocalFileDeletionHandler, FDescriptionSyncGuard, FTimestampSyncGuard);

	{Create listing handlers for FsFindFirst}
	FRootListingHandler := TRootListingHandler.Create;

	{Create listing result applier for FsFindFirst}
	FListingResultApplier := TListingResultApplier.Create;

	{Create download orchestrator for FsGetFile}
	FDownloadOrchestrator := TDownloadOrchestrator.Create(FDownloadPreparationValidator, FLocalFileConflictResolver, FRetryHandler, SettingsManager);
	Result := 0;
end;

function TWFXApplication.EnsureAuthorized(Cloud: TCloudMailRu): Boolean;
begin
	if Cloud.AuthorizationState = asAuthorized then
		Exit(True);

	{Attempt authorization if not yet authorized}
	Result := Cloud.Authorize;

	if not Result then
		SetLastError(ERROR_ACCESS_DENIED);
end;

function TWFXApplication.DeleteLocalFile(LocalName: WideString): Integer;
begin
	Result := FLocalFileDeletionHandler.DeleteLocalFile(LocalName);
end;

destructor TWFXApplication.Destroy;
begin
	FThreadState := nil; {IThreadStateManager is reference-counted, setting to nil releases it}
	FRetryHandler := nil;
	FCommandDispatcher := nil;
	FListingProvider := nil;
	FDescriptionSyncGuard := nil;
	FDescriptionSync := nil;
	FTimestampSyncGuard := nil;
	FTimestampSync := nil;
	FLocalFileDeletionHandler := nil;
	FDownloadSuccessHandler := nil;
	FActionExecutor := nil;
	FListingSkipDecider := nil;
	FListingPathValidator := nil;
	FSameAccountMoveHandler := nil;
	FFileStreamExecutor := nil;
	FLocalFileConflictResolver := nil;
	FListingItemFetcher := nil;
	FSharedItemDeletionHandler := nil;
	FTrashBinOperationHandler := nil;
	FInviteOperationHandler := nil;
	FCrossAccountFileOperationHandler := nil;
	FIconRenderingEngine := nil;
	FIconContextBuilder := nil;
	FFileExecutionDispatcher := nil;
	FSharedItemActionHandler := nil;
	FUploadCompletionHandler := nil;
	FRootListingHandler := nil;
	FPathListingHandler := nil;
	FOverwritePreparationHandler := nil;
	FOperationStatusContextBuilder := nil;
	FListingResultApplier := nil;
	FDownloadOrchestrator := nil;
	FTCHandler := nil;
	ConnectionManager := nil;
	FOpenSSLProvider := nil;

	CurrentDescriptions.Free;

	SettingsManager := nil; {IPluginSettingsManager is reference-counted, setting to nil releases it}
	AccountSettings := nil; {IAccountsManager is reference-counted, setting to nil releases it}
	PasswordManager := nil; {IPasswordManager is reference-counted, setting to nil releases it}
	Logger := nil; {ILogger is reference-counted, setting to nil releases it}
	Progress := nil; {IProgress is reference-counted, setting to nil releases it}
	Request := nil; {IRequest is reference-counted, setting to nil releases it}
	inherited;
end;

function TWFXApplication.ExecCommand(RemoteName: PWideChar; Command, Parameter: WideString): Integer;
var
	CmdResult: TCommandResult;
begin
	CmdResult := FCommandDispatcher.Execute(RemoteName, Command, Parameter);
	Result := CmdResult.ResultCode;

	{Update RemoteName for symlink navigation commands}
	if CmdResult.ResultCode = FS_EXEC_SYMLINK then
		strpcopy(RemoteName, CmdResult.SymlinkPath);
end;

function TWFXApplication.ExecInvitesAction(MainWin: THandle; RealPath: TRealPath): Integer;
var
	Cloud: TCloudMailRu;
	CurrentInvite: TCloudIncomingInvite;
begin
	Result := FS_EXEC_OK;
	Cloud := ConnectionManager.Get(RealPath.account);
	if RealPath.isInAccountsList then
	begin {Main invites folder properties}
		if TAccountsForm.ShowAccounts(MainWin, PasswordManager, RealPath.account) then
			SettingsManager.Refresh;
	end else begin {One invite item - delegate to handler}
		if not EnsureAuthorized(Cloud) then
			exit(FS_EXEC_ERROR);
		CurrentInvite := FindIncomingInviteItemByPath(CurrentIncomingInvitesListing, RealPath);
		if CurrentInvite.name = EmptyWideStr then
			exit(FS_EXEC_ERROR);

		Result := FInviteOperationHandler.Execute(MainWin, Cloud.ShareService, CurrentInvite,
			function(ParentWindow: HWND; const Inv: TCloudIncomingInvite): Integer
			begin
				Result := TInvitePropertyForm.ShowProperties(ParentWindow, Inv);
			end);
	end;

	PostMessage(MainWin, WM_USER + TC_REFRESH_MESSAGE, TC_REFRESH_PARAM, 0); {TC does not auto-refresh the current panel}
end;

function TWFXApplication.ExecProperties(MainWin: THandle; RealPath: TRealPath): Integer;
var
	Cloud: TCloudMailRu;
	CurrentItem: TCloudDirItem;
begin
	Result := FS_EXEC_OK;
	if RealPath.isInAccountsList then
	begin
		if TAccountsForm.ShowAccounts(MainWin, PasswordManager, RealPath.account) then
			SettingsManager.Refresh;
	end else begin
		Cloud := ConnectionManager.Get(RealPath.account);
		if not EnsureAuthorized(Cloud) then
			exit(FS_EXEC_ERROR);
		{Always refresh status from server -- CurrentListing may have been changed in another panel}
		if (Cloud.ListingService.StatusFile(RealPath.Path, CurrentItem)) and (idContinue = TPropertyForm.ShowProperty(MainWin, RealPath.Path, CurrentItem, Cloud, FFileSystem, FTCHandler, SettingsManager.GetSettings.DescriptionEnabled, SettingsManager.GetSettings.DescriptionEditorEnabled, SettingsManager.GetSettings.DescriptionFileName)) then
			PostMessage(MainWin, WM_USER + TC_REFRESH_MESSAGE, TC_REFRESH_PARAM, 0); {Refresh TC panel if description was edited}
	end;
end;

function TWFXApplication.ExecSharedAction(MainWin: THandle; RealPath: TRealPath; RemoteName: PWideChar; ActionOpen: Boolean): Integer;
var
	Cloud: TCloudMailRu;
	CurrentItem: TCloudDirItem;
	ActionResult: TSharedItemActionResult;
begin
	Result := FS_EXEC_OK;

	{Determine action from handler}
	ActionResult := FSharedItemActionHandler.HandleAction(RealPath, ActionOpen, CurrentListing);

	case ActionResult.ActionType of
		satSymlink:
			begin
				strpcopy(RemoteName, ActionResult.SymlinkPath);
				Result := FS_EXEC_SYMLINK;
			end;
		satAccountSettings:
			begin
				if TAccountsForm.ShowAccounts(MainWin, PasswordManager, RealPath.account) then
					SettingsManager.Refresh;
			end;
		satPropertyDialog:
			begin
				Cloud := ConnectionManager.Get(RealPath.account);
				if not EnsureAuthorized(Cloud) then
					exit(FS_EXEC_ERROR);
				CurrentItem := ActionResult.CurrentItem;
				if Cloud.ListingService.StatusFile(CurrentItem.home, CurrentItem) then
					TPropertyForm.ShowProperty(MainWin, RealPath.Path, CurrentItem, Cloud, FFileSystem, FTCHandler, false, false, SettingsManager.GetSettings.DescriptionFileName);
			end;
	end;
end;

function TWFXApplication.ExecTrashbinProperties(MainWin: THandle; RealPath: TRealPath): Integer;
var
	Cloud: TCloudMailRu;
	CurrentItem: TCloudDirItem;
	IsTrashDir: Boolean;
begin
	Cloud := ConnectionManager.Get(RealPath.account);
	if not EnsureAuthorized(Cloud) then
		exit(FS_EXEC_ERROR);
	IsTrashDir := RealPath.isInAccountsList;

	if IsTrashDir then
	begin {Main trashbin folder properties}
		if not Cloud.ListingService.GetTrashbin(CurrentListing) then
			exit(FS_EXEC_ERROR);
		CurrentItem := CurrentItem.None;
	end else begin {One item in trashbin}
		{For identically named files in trash, properties of the first match are shown}
		CurrentItem := FindListingItemByPath(CurrentListing, RealPath);
	end;

	Result := FTrashBinOperationHandler.Execute(MainWin, Cloud.ListingService, CurrentListing, CurrentItem, IsTrashDir, RealPath.account,
		function(ParentWindow: HWND; Items: TCloudDirItemList; TrashDir: Boolean; const AccountName: WideString): Integer
		begin
			Result := TDeletedPropertyForm.ShowProperties(ParentWindow, Items, TrashDir, AccountName);
		end);

	PostMessage(MainWin, WM_USER + TC_REFRESH_MESSAGE, TC_REFRESH_PARAM, 0); {TC does not auto-refresh the current panel}
end;

function TWFXApplication.ExecuteFileStream(RealPath: TRealPath; StreamingSettings: TStreamingSettings): Integer;
var
	CurrentItem: TCloudDirItem;
begin
	{Real attributes may differ from listing cache (listing is not auto-refreshed)}
	CurrentItem := FindListingItemByPath(CurrentListing, RealPath); {Inside public cloud, weblink is available automatically}

	{Delegate streaming execution to handler}
	Result := FFileStreamExecutor.Execute(RealPath, CurrentItem, StreamingSettings, ConnectionManager);
end;

function TWFXApplication.FindIncomingInviteItemByPath(InviteListing: TCloudIncomingInviteList; Path: TRealPath): TCloudIncomingInvite;
begin
	Result := InviteListing.FindByName(Path.Path);
	{item not found in current global listing, so refresh it}
	if Result.isNone then
		if ConnectionManager.Get(Path.account).ListingService.GetIncomingInvites(CurrentIncomingInvitesListing) then
			exit(CurrentIncomingInvitesListing.FindByName(Path.Path));
end;

function TWFXApplication.FindListingItemByPath(CurrentListing: TCloudDirItemList; Path: TRealPath; UpdateListing: Boolean): TCloudDirItem;
var
	CurrentCloud: TCloudMailRu;
begin
	CurrentCloud := ConnectionManager.Get(Path.account);
	Result := FListingItemFetcher.FetchItem(CurrentListing, Path, CurrentCloud, UpdateListing);
end;

function TWFXApplication.FsContentGetSupportedField(FieldIndex: Integer; FieldName, Units: PAnsiChar; MaxLen: Integer): Integer;
begin
	Result := FContentFieldProvider.GetSupportedField(FieldIndex, FieldName, MaxLen);
end;

function TWFXApplication.FsContentGetValue(FileName: WideString; FieldIndex, UnitIndex: Integer; FieldValue: Pointer; MaxLen, Flags: Integer): Integer;
var
	Item: TCloudDirItem;
	RealPath: TRealPath;
	Context: TContentFieldContext;
begin
	RealPath.FromPath(FileName);

	{Parent directory entry has no content fields}
	if RealPath.upDirItem then
		Exit(FT_NOSUCHFIELD);

	{Build context for the provider}
	Context.IsAccountRoot := RealPath.isInAccountsList;
	Context.DescriptionsEnabled := SettingsManager.GetSettings.DescriptionEnabled;
	if Context.IsAccountRoot then
		Context.AccountDescription := AccountSettings.GetAccountSettings(RealPath.account).Description
	else
		Context.AccountDescription := '';

	{Account root only supports description field via context}
	if Context.IsAccountRoot then
	begin
		Item := Default (TCloudDirItem);
		Result := FContentFieldProvider.GetValue(FieldIndex, Item, FieldValue, Context);
		exit;
	end;

	{Find the item for regular paths}
	Item := FindListingItemByPath(CurrentListing, RealPath, not RealPath.invitesDir);
	Context.FileDescription := CurrentDescriptions.GetValue(Item.name);

	Result := FContentFieldProvider.GetValue(FieldIndex, Item, FieldValue, Context);
end;

function TWFXApplication.FsDeleteFile(RemoteName: WideString): Boolean;
var
	RealPath: TRealPath;
	CurrentItem: TCloudDirItem;
	Cloud: TCloudMailRu;
begin
	RealPath.FromPath(WideString(RemoteName));
	if RealPath.isAccountEmpty or RealPath.TrashDir or RealPath.invitesDir then
		exit(false);
	Cloud := ConnectionManager.Get(RealPath.account);
	if not EnsureAuthorized(Cloud) then
		exit(false);
	if RealPath.sharedDir then
	begin
		CurrentItem := FindListingItemByPath(CurrentListing, RealPath);
		Result := FSharedItemDeletionHandler.Execute(Cloud, CurrentItem);
	end
	else
		Result := Cloud.FileOperations.Delete(RealPath.Path);
	if Result then
	begin
		FDescriptionSyncGuard.OnFileDeleted(RealPath, Cloud);
		FTimestampSyncGuard.OnFileDeleted(RealPath, Cloud);
	end;
end;

function TWFXApplication.FsDisconnect(DisconnectRoot: WideString): Boolean;
begin
	if not FThreadState.HasActiveBackgroundJobs(ExtractFileName(DisconnectRoot)) then
	begin
		ConnectionManager.Free(ExtractFileName(DisconnectRoot));
		Result := true;
	end else begin {Could add a wait mechanism for background operation completion here}
		Result := false;
	end;
end;

function TWFXApplication.FsExecuteFile(MainWin: THandle; RemoteName: PWideChar; Verb: WideString): Integer;
var
	Action: TExecutionAction;
begin
	{Dispatch verb to determine appropriate action}
	Action := FFileExecutionDispatcher.GetAction(RemoteName, Verb,
		function(const Path: WideString): TStreamingSettings
		begin
			Result := SettingsManager.GetStreamingSettings(Path);
		end);

	{Execute the appropriate handler based on action type}
	case Action.ActionType of
		eatTrashbinProperties:
			Result := ExecTrashbinProperties(MainWin, Action.RealPath);
		eatSharedAction:
			Result := ExecSharedAction(MainWin, Action.RealPath, RemoteName, Action.ActionOpen);
		eatInvitesAction:
			Result := ExecInvitesAction(MainWin, Action.RealPath);
		eatProperties:
			Result := ExecProperties(MainWin, Action.RealPath);
		eatStream:
			Result := ExecuteFileStream(Action.RealPath, Action.StreamingSettings);
		eatCommand:
			Result := ExecCommand(RemoteName, Action.Command, Action.Parameter);
		eatOpenYourself:
			Result := FS_EXEC_YOURSELF;
		else
			Result := FS_EXEC_OK;
	end;
end;

function TWFXApplication.FsExtractCustomIcon(RemoteName: PWideChar; ExtractFlags: Integer; var TheIcon: hIcon): Integer;
var
	RealPath: TRealPath;
	Input: TIconContextInput;
	Context: TIconContext;
	IconInfo: TIconInfo;
	IconsSize: Integer;
	RenderResult: TIconRenderResult;
begin
	RealPath.FromPath(RemoteName);

	{Parent directory entry uses default icon}
	if RealPath.upDirItem then
		Exit(FS_ICON_USEDEFAULT);

	IconsSize := FTCHandler.GetTCIconsSize;

	{Build context using builder}
	Input.Path := RealPath;
	Input.IconsMode := SettingsManager.GetSettings.IconsMode;
	Context := FIconContextBuilder.BuildContext(Input, CurrentListing, CurrentIncomingInvitesListing);

	{Get icon info from provider}
	IconInfo := FIconProvider.GetIcon(RealPath, Context);

	{Delegate rendering to engine}
	RenderResult := FIconRenderingEngine.Render(IconInfo, IconsSize, PluginPath);

	{Apply render result}
	TheIcon := RenderResult.IconHandle;
	Result := RenderResult.ResultCode;

	{For internal resources, TC expects resource name in RemoteName buffer}
	if RenderResult.ResourceName <> '' then
		strpcopy(RemoteName, RenderResult.ResourceName);
end;

function TWFXApplication.FsGetPreviewBitmap(RemoteName: WideString; Width, Height: Integer; var ReturnedBitmap: HBITMAP): Integer;
var
	RealPath: TRealPath;
	Cloud: TCloudMailRu;
begin
	Result := FS_BITMAP_NONE;
	ReturnedBitmap := 0;

	RealPath.FromPath(RemoteName);

	{Skip non-file paths (root, virtual folders, etc.)}
	if RealPath.IsVirtual or RealPath.upDirItem or (RealPath.Path = '') then
		Exit;

	{Skip files with unsupported extensions (also skips directories which have no extension)}
	if not SettingsManager.GetSettings.IsThumbnailExtension(ExtractFileExt(RealPath.Path)) then
		Exit;

	{Get cloud connection}
	Cloud := ConnectionManager.Get(RealPath.account);
	if not EnsureAuthorized(Cloud) then
		Exit;

	{NOTE: FS_BITMAP_CACHE tells TC to write thumbnails to tcthumbs.idb, and TC does write them.
		However, TC always re-requests thumbnails from WFX plugins on every directory entry
		regardless of this flag - it never reads cached bitmaps back for virtual file systems.
		The flag is kept in case future TC versions fix this behavior.}
	ReturnedBitmap := Cloud.GetThumbnail(RealPath.Path, Width, Height);
	if ReturnedBitmap <> 0 then
		Result := FS_BITMAP_EXTRACTED + FS_BITMAP_CACHE;
end;

function TWFXApplication.FsFindClose(Hdl: THandle): Integer;
begin
	FileCounter := 0;
	Result := 0;
end;

function TWFXApplication.FsFindFirst(Path: WideString; var FindData: tWIN32FINDDATAW): THandle;
var
	SkipResult: TListingSkipResult;
	RootResult: TRootListingResult;
	PathResult: TPathListingResult;
	BaseResult: TListingResultBase;
begin
	{Check if listing should be skipped (delete/renmov operation in progress or user abort)}
	SkipResult := FListingSkipDecider.ShouldSkipListing(Path);
	if SkipResult.ShouldSkip then
	begin
		CurrentListing := [];
		SetLastError(ERROR_NO_MORE_FILES);
		exit(FIND_NO_MORE_FILES);
	end;

	GlobalPath := Path;
	if GlobalPath = '\' then
	begin {Root listing - list accounts}
		RootResult := FRootListingHandler.ExecuteWithAccounts(AccountSettings.GetAccountsList([ATPrivate, ATPublic], SettingsManager.GetSettings.EnabledVirtualTypes));
		Accounts := RootResult.Accounts;

		{Apply common result fields}
		BaseResult.FileCounter := RootResult.FileCounter;
		BaseResult.FindData := RootResult.FindData;
		BaseResult.ErrorCode := RootResult.ErrorCode;
		BaseResult.Handle := RootResult.Handle;
		Result := FListingResultApplier.Apply(BaseResult, FindData, FileCounter);
	end else begin {Regular path listing}
		PathResult := FPathListingHandler.Execute(GlobalPath);
		CurrentListing := PathResult.Listing;
		CurrentIncomingInvitesListing := PathResult.IncomingInvites;

		{Apply common result fields}
		BaseResult.FileCounter := PathResult.FileCounter;
		BaseResult.FindData := PathResult.FindData;
		BaseResult.ErrorCode := PathResult.ErrorCode;
		BaseResult.Handle := PathResult.Handle;
		Result := FListingResultApplier.Apply(BaseResult, FindData, FileCounter);
	end;
end;

function TWFXApplication.FsFindNext(Hdl: THandle; var FindData: tWIN32FINDDATAW): Boolean;
begin
	if GlobalPath = '\' then
	begin
		if (Accounts.Count > FileCounter) then
		begin
			FindData.InitAsEmptyDir(Accounts[FileCounter]);
			inc(FileCounter);
			Result := true;
		end
		else
			Result := false;

	end else begin
		{Get subsequent files in directory (called until returns false)}
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

function TWFXApplication.FsGetBackgroundFlags: Integer;
begin
	if SettingsManager.GetSettings.DisableMultiThreading then
		Result := 0
	else
		Result := BG_DOWNLOAD + BG_UPLOAD; {+ BG_ASK_USER}
end;

procedure TWFXApplication.FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer);
begin

end;

function TWFXApplication.FsGetFile(RemoteName, LocalName: WideString; CopyFlags: Integer; RemoteInfo: pRemoteInfo): Integer;
begin
	Result := FDownloadOrchestrator.Execute(RemoteName, LocalName, CopyFlags,
		function(const RemotePath: TRealPath; const ALocalName, ARemoteName: WideString; ACopyFlags: Integer): Integer
		begin
			Result := GetRemoteFile(RemotePath, ALocalName, ARemoteName, ACopyFlags);
		end,
		function(const Source, Target: WideString; PercentDone: Integer): Boolean
		begin
			Result := Progress.Progress(PWideChar(Source), PWideChar(Target), PercentDone);
		end);
end;

function TWFXApplication.FsMkDir(Path: WideString): Boolean;
var
	RealPath: TRealPath;
	SkipListRenMov: Boolean;
	Cloud: TCloudMailRu;
begin
	SkipListRenMov := FThreadState.GetSkipListRenMov;
	if SkipListRenMov then
		exit(false); {Skip directory creation when flag is set}

	RealPath.FromPath(WideString(Path));
	if RealPath.isInAccountsList then {Accounts list - registration not supported}
		Exit(False);
	if (RealPath.isAccountEmpty) or RealPath.isVirtual then
		exit(false);

	Cloud := ConnectionManager.Get(RealPath.account);
	if not EnsureAuthorized(Cloud) then
		exit(false);

	Result := Cloud.FileOperations.CreateDirectory(RealPath.Path);
	{Need to check operation context => directory can be moved}
	if Result and FMoveOperationTracker.IsMoveOperation then
		FMoveOperationTracker.TrackMoveTarget(RealPath);
end;

function TWFXApplication.FsPutFile(LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
var
	RealPath: TRealPath;
	ValidationResult: TUploadValidationResult;
	OverwriteResult: TOverwritePreparationResult;
begin
	RealPath.FromPath(RemoteName);

	{Validate upload preconditions}
	ValidationResult := FUploadPreparationValidator.Validate(LocalName, RealPath, CopyFlags);
	if not ValidationResult.ShouldProceed then
		exit(ValidationResult.ResultCode);

	Progress.Progress(LocalName, PWideChar(RealPath.Path), 0);

	{Prepare for overwrite if required (cloud API doesn't support overwrite, so delete first)}
	OverwriteResult := FOverwritePreparationHandler.Prepare(RealPath, ValidationResult.RequiresOverwrite);
	if not OverwriteResult.Success then
		exit(OverwriteResult.ResultCode);

	Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);

	if Result <> FS_FILE_WRITEERROR then
		exit;

	Result := FRetryHandler.HandleOperationError(Result, rotUpload, ERR_UPLOAD_FILE_ASK, ERR_UPLOAD, UPLOAD_FILE_RETRY, LocalName,
		function: Integer
		begin
			Result := PutRemoteFile(RealPath, LocalName, RemoteName, CopyFlags);
		end,
		function: Boolean
		begin
			Result := Progress.Progress(PWideChar(LocalName), RemoteName, 0);
		end);
end;

function TWFXApplication.FsRemoveDir(RemoteName: WideString): Boolean;
var
	RealPath: TRealPath;
	Cloud: TCloudMailRu;
begin
	if not FDirectoryDeletionPreCheck.ShouldProceed(RemoteName) then
		exit(false);

	RealPath.FromPath(WideString(RemoteName));
	if RealPath.isVirtual then
		exit(false);

	Cloud := ConnectionManager.Get(RealPath.account);
	if not EnsureAuthorized(Cloud) then
		exit(false);
	Result := Cloud.FileOperations.RemoveDirectory(RealPath.Path);

	if Result then
	begin
		{Directory can be deleted after moving operation - use tracker to check context}
		if FMoveOperationTracker.IsMoveOperation then
		begin
			FDescriptionSyncGuard.OnFileRenamed(RealPath, FMoveOperationTracker.GetMoveTarget, Cloud);
			FTimestampSyncGuard.OnFileRenamed(RealPath, FMoveOperationTracker.GetMoveTarget, Cloud);
		end
		else
		begin
			FDescriptionSyncGuard.OnFileDeleted(RealPath, Cloud);
			FTimestampSyncGuard.OnFileDeleted(RealPath, Cloud);
		end;
	end;
end;

function TWFXApplication.FsRenMovFile(OldName, NewName: WideString; Move, OverWrite: Boolean; ri: pRemoteInfo): Integer;
var
	OldRealPath: TRealPath;
	NewRealPath: TRealPath;
	OldCloud, NewCloud: TCloudMailRu;
begin
	Progress.Progress(OldName, NewName, 0);

	OldRealPath.FromPath(OldName);
	NewRealPath.FromPath(NewName);

	if OldRealPath.IsVirtual or NewRealPath.IsVirtual then
		exit(FS_FILE_NOTSUPPORTED);

	OldCloud := ConnectionManager.Get(OldRealPath.account);
	NewCloud := ConnectionManager.Get(NewRealPath.account);

	if not EnsureAuthorized(OldCloud) then
		exit(FS_FILE_NOTSUPPORTED);
	if (OldRealPath.account <> NewRealPath.account) and not EnsureAuthorized(NewCloud) then
		exit(FS_FILE_NOTSUPPORTED);

	if OldRealPath.account <> NewRealPath.account then {Cross-account operation - delegate to handler}
		Result := FCrossAccountFileOperationHandler.Execute(OldCloud, NewCloud, OldRealPath, NewRealPath, Move, OverWrite, SettingsManager.GetSettings.CopyBetweenAccountsMode, OldCloud.IsPublicAccount,
			function: Boolean
			begin
				Result := Progress.Aborted();
			end)
	else {Same account - delegate to handler}
		Result := FSameAccountMoveHandler.Execute(OldCloud, OldRealPath, NewRealPath, Move, OverWrite);

	Progress.Progress(OldName, NewName, 100);
end;

procedure TWFXApplication.FsSetCryptCallback(PCryptProc: TCryptProcW; CryptoNr, Flags: Integer);
begin
	PasswordManager := TTCPasswordManager.Create(PCryptProc, PluginNum, CryptoNr, Logger, FTCHandler);
	PasswordUI := TPasswordUIProvider.Create;
	HTTPMgr := THTTPManager.Create(SettingsManager.GetSettings.ConnectionSettings, FSSLHandlerFactory, Logger, Progress, TCloudHTTPFactory.Create);
	CipherVal := TCipherValidator.Create;
	{TODO: ConnectionManager and dependent components are created here because they require
	PasswordManager, which needs PCryptProc from this callback. This makes FsSetCryptCallback
	a de-facto "second initialization phase", which is not its intended purpose.
	Investigate alternatives: lazy initialization, dependency restructuring, or deferred injection.}
	ConnectionManager := TConnectionManager.Create(SettingsManager, AccountSettings, HTTPMgr,
		TFileEncryptionResolver.Create(PasswordManager, PasswordUI, CipherVal, AccountSettings, FTCHandler, Logger),
		TProxyPasswordResolver.Create(HTTPMgr, PasswordManager, PasswordUI, SettingsManager, FTCHandler, Logger),
		TWindowsFileSystem.Create, Progress, Logger, Request,
		FTCHandler, TDefaultAuthStrategyFactory.Create, FOpenSSLProvider,
		TAccountCredentialsProvider.Create(PasswordManager, PasswordUI, Logger, FTCHandler, AccountSettings),
		TServerProfileManager.Create(TIniConfigFile.Create(SettingsManager.GetSettings.IniFilePath)));
	FCommandDispatcher := TCommandDispatcher.Create(ConnectionManager, Logger, SettingsManager);

	{Create icon context builder for FsExtractCustomIcon}
	FIconContextBuilder := TIconContextBuilder.Create(AccountSettings, ConnectionManager, FListingItemFetcher);

	{Create operation status context builder for FsStatusInfo - requires ConnectionManager}
	FOperationStatusContextBuilder := TOperationStatusContextBuilder.Create(SettingsManager, ConnectionManager);

	{Create operation action executor for lifecycle event handling - requires ConnectionManager}
	FActionExecutor := TOperationActionExecutor.Create(FThreadState, ConnectionManager, SettingsManager, CurrentDescriptions, Logger);

	{Create path listing handler for FsFindFirst - requires ConnectionManager}
	FPathListingHandler := TPathListingHandler.Create(ConnectionManager, FListingProvider, FListingPathValidator);

	{Create overwrite preparation handler for FsPutFile - requires ConnectionManager}
	FOverwritePreparationHandler := TOverwritePreparationHandler.Create(ConnectionManager);
end;

procedure TWFXApplication.FsStatusInfo(RemoteDir: WideString; InfoStartEnd, InfoOperation: Integer);
var
	RealPath: TRealPath;
	Context: TOperationContext;
	Actions: TOperationActions;
begin
	RealPath.FromPath(RemoteDir, ID_True); {RemoteDir always a directory}

	{Build context for the handler}
	Context := FOperationStatusContextBuilder.BuildContext(RealPath, InfoOperation);

	if InfoStartEnd = FS_STATUS_START then
	begin
		FThreadState.SetFsStatusInfo(InfoOperation);
		Actions := FOperationLifecycle.GetStartActions(Context);
		FActionExecutor.Execute(Actions, RealPath, InfoOperation);
	end else if InfoStartEnd = FS_STATUS_END then
	begin
		FThreadState.RemoveFsStatusInfo;
		Actions := FOperationLifecycle.GetEndActions(Context);
		FActionExecutor.Execute(Actions, RealPath, InfoOperation);
	end;
end;

function TWFXApplication.GetRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
var
	Cloud: TCloudMailRu;
	resultHash: WideString;
	DownloadContext: TDownloadContext;
begin
	if (SettingsManager.GetSettings.CheckCRC) then
		resultHash := EmptyWideStr
	else
		resultHash := 'dummy'; {Calculations will be ignored if variable is not empty}
	Cloud := ConnectionManager.Get(RemotePath.account);
	if not EnsureAuthorized(Cloud) then
		exit(FS_FILE_NOTSUPPORTED);

	Result := Cloud.Downloader.Download(WideString(RemotePath.Path), LocalName, resultHash);

	if Result = FS_FILE_OK then
	begin
		{Build context and delegate to success handler}
		DownloadContext.RemotePath := RemotePath;
		DownloadContext.LocalName := LocalName;
		DownloadContext.RemoteName := RemoteName;
		DownloadContext.CopyFlags := CopyFlags;
		DownloadContext.resultHash := resultHash;
		DownloadContext.Item := FindListingItemByPath(CurrentListing, RemotePath);
		DownloadContext.Cloud := Cloud;
		Result := FDownloadSuccessHandler.HandleSuccess(DownloadContext);
	end;
end;

function TWFXApplication.PutRemoteFile(RemotePath: TRealPath; LocalName, RemoteName: WideString; CopyFlags: Integer): Integer;
var
	Cloud: TCloudMailRu;
	CompletionContext: TUploadCompletionContext;
begin
	Cloud := ConnectionManager.Get(RemotePath.account);
	if not EnsureAuthorized(Cloud) then
		exit(FS_FILE_NOTSUPPORTED);

	Result := Cloud.Uploader.Upload(WideString(LocalName), RemotePath.Path);
	if Result = FS_FILE_OK then
	begin
		CompletionContext.RemotePath := RemotePath;
		CompletionContext.LocalName := LocalName;
		CompletionContext.RemoteName := RemoteName;
		CompletionContext.CopyFlags := CopyFlags;
		CompletionContext.Cloud := Cloud;
		Result := FUploadCompletionHandler.HandleCompletion(CompletionContext);
	end;
end;

function TWFXApplication.HasActiveOperations: Boolean;
begin
	Result := FThreadState.HasAnyActiveOperations;
end;

end.
