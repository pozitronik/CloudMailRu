library MailRuCloud;

{$R *.dres}

uses
	ANSICompatibility in 'src\Infrastructure\Protocol\ANSICompatibility.pas',
	AccountSettings in 'src\Infrastructure\Settings\AccountSettings.pas',
	Accounts in 'src\Presentation\UI\Forms\Accounts.pas' {AccountsForm} ,
	AccountsManager in 'src\Infrastructure\Settings\AccountsManager.pas',
	AskPassword in 'src\Presentation\UI\Forms\AskPassword.pas' {AskPasswordForm} ,
	AskEncryptionPassword in 'src\Presentation\UI\Forms\AskEncryptionPassword.pas' {AskEncryptionPasswordForm} ,
	CloudConstants in 'src\Domain\Constants\CloudConstants.pas',
	CloudDirItem in 'src\Domain\ValueObjects\CloudDirItem.pas',
	DateTimeUtils in 'src\Domain\Utils\DateTimeUtils.pas',
	CloudDirItemList in 'src\Domain\ValueObjects\CloudDirItemList.pas',
	CloudFileIdentity in 'src\Domain\ValueObjects\CloudFileIdentity.pas',
	CloudIncomingInvite in 'src\Domain\ValueObjects\CloudIncomingInvite.pas',
	CloudIncomingInviteList in 'src\Domain\ValueObjects\CloudIncomingInviteList.pas',
	CloudInvite in 'src\Domain\ValueObjects\CloudInvite.pas',
	CloudInviteList in 'src\Domain\ValueObjects\CloudInviteList.pas',
	CloudOAuth in 'src\Domain\ValueObjects\CloudOAuth.pas',
	CloudOperationResult in 'src\Domain\ValueObjects\CloudOperationResult.pas',
	CloudOwner in 'src\Domain\ValueObjects\CloudOwner.pas',
	CloudEndpoints in 'src\Domain\ValueObjects\CloudEndpoints.pas',
	CloudSpace in 'src\Domain\ValueObjects\CloudSpace.pas',
	LanguageStrings in 'src\Presentation\Localization\LanguageStrings.pas',
	ChunkedFileStream in 'src\Infrastructure\IO\ChunkedFileStream.pas',
	Classes,
	CloudAuthorizationState in 'src\Domain\Services\CloudAuthorizationState.pas',
	CloudContext in 'src\Domain\Services\CloudContext.pas',
	CloudMailRu in 'src\Domain\Services\CloudMailRu.pas',
	CloudMailRuFactory in 'src\Domain\Services\CloudMailRuFactory.pas',
	CloudHTTP in 'src\Infrastructure\HTTP\CloudHTTP.pas',
	CloudSettings in 'src\Infrastructure\Settings\CloudSettings.pas',
	ConnectionManager in 'src\Infrastructure\Services\ConnectionManager.pas',
	ConnectionSettings in 'src\Infrastructure\Settings\ConnectionSettings.pas',
	DCPbase64 in 'src\libs\DCPCrypt\DCPbase64.pas',
	DCPblockciphers in 'src\libs\DCPCrypt\DCPblockciphers.pas',
	DCPconst in 'src\libs\DCPCrypt\DCPconst.pas',
	DCPcrypt2 in 'src\libs\DCPCrypt\DCPcrypt2.pas',
	DCPrijndael in 'src\libs\DCPCrypt\Ciphers\DCPrijndael.pas',
	DCPtwofish in 'src\libs\DCPCrypt\Ciphers\DCPtwofish.pas',
	DCPserpent in 'src\libs\DCPCrypt\Ciphers\DCPserpent.pas',
	DCPsha1 in 'src\libs\DCPCrypt\Hashes\DCPsha1.pas',
	DCPsha256 in 'src\libs\DCPCrypt\Hashes\DCPsha256.pas',
	DCPtypes in 'src\libs\DCPCrypt\DCPtypes.pas',
	DateUtils,
	DeletedProperty in 'src\Presentation\UI\Forms\DeletedProperty.pas' {DeletedPropertyForm} ,
	Description in 'src\Domain\Services\Description.pas',
	Cipher in 'src\Infrastructure\Cipher\Cipher.pas',
	CipherProfile in 'src\Infrastructure\Cipher\CipherProfile.pas',
	CipherStreams in 'src\Infrastructure\Cipher\CipherStreams.pas',
	BlockCipher in 'src\Infrastructure\Cipher\BlockCipher.pas',
	OpenSSLCipher in 'src\Infrastructure\Cipher\OpenSSLCipher.pas',
	BCryptProvider in 'src\Infrastructure\Cipher\BCryptProvider.pas',
	BCryptCipher in 'src\Infrastructure\Cipher\BCryptCipher.pas',
	FileSplitInfo in 'src\Infrastructure\IO\FileSplitInfo.pas',
	HTTPManager in 'src\Infrastructure\HTTP\HTTPManager.pas',
	SSLHandlerFactory in 'src\Infrastructure\SSL\SSLHandlerFactory.pas',
	IndySSLHandlerFactory in 'src\Infrastructure\SSL\IndySSLHandlerFactory.pas',
	IndySecSSLHandlerFactory in 'src\Infrastructure\SSL\IndySecSSLHandlerFactory.pas',
	HashInfo in 'src\Domain\ValueObjects\HashInfo.pas',
	IconHelper in 'src\Presentation\Icon\IconHelper.pas',
	IdSSLOpenSSLHeaders,
	IniFiles,
	IniFilesHelper in 'src\Infrastructure\Config\IniFilesHelper.pas',
	InviteProperty in 'src\Presentation\UI\Forms\InviteProperty.pas' {InvitePropertyForm} ,
	JSON,
	JSONHelper in 'src\Infrastructure\HTTP\JSONHelper.pas',
	SafeJSON in 'src\Infrastructure\HTTP\SafeJSON.pas',
	Messages,
	WFXTypes in 'src\Infrastructure\Protocol\WFXTypes.pas',
	ParsingHelper in 'src\Infrastructure\HTTP\ParsingHelper.pas',
	PathHelper in 'src\Infrastructure\IO\PathHelper.pas',
	PluginSettings in 'src\Infrastructure\Settings\PluginSettings.pas',
	PluginSettingsManager in 'src\Infrastructure\Settings\PluginSettingsManager.pas',
	ProxySettings in 'src\Infrastructure\Settings\ProxySettings.pas',
	RealPath in 'src\Domain\ValueObjects\RealPath.pas',
	RemoteProperty in 'src\Presentation\UI\Forms\RemoteProperty.pas' {PropertyForm} ,
	RemotePropertyPresenter in 'src\Presentation\Presenter\RemotePropertyPresenter.pas',
	AccountsPresenter in 'src\Presentation\Presenter\AccountsPresenter.pas',
	SettingsConstants in 'src\Infrastructure\Settings\SettingsConstants.pas',
	StreamingSettings in 'src\Infrastructure\Settings\StreamingSettings.pas',
	StringHelper in 'src\Infrastructure\IO\StringHelper.pas',
	TokenRetryHelper in 'src\Application\Retry\TokenRetryHelper.pas',
	SysUtils,
	System.Generics.Collections,
	SystemHelper in 'src\Infrastructure\OS\SystemHelper.pas',
	TCHandler in 'src\Infrastructure\TC\TCHandler.pas',
	Logger in 'src\Infrastructure\Logger\Logger.pas',
	PasswordManager in 'src\Infrastructure\Password\PasswordManager.pas',
	Progress in 'src\Infrastructure\Progress\Progress.pas',
	Request in 'src\Infrastructure\Request\Request.pas',
	Variants,
	Vcl.Controls,
	WSList in 'src\Domain\ValueObjects\WSList.pas',
	Windows,
	PluginForm in 'src\Presentation\UI\PluginForm.pas',
	CommandExecutor in 'src\Infrastructure\OS\CommandExecutor.pas',
	MailRuCloudWFX in 'src\MailRuCloudWFX.pas',
	WFXInterface in 'src\WFXInterface.pas',
	PasswordUIProvider in 'src\Infrastructure\Password\PasswordUIProvider.pas',
	CipherValidator in 'src\Infrastructure\Cipher\CipherValidator.pas',
	FileSystem in 'src\Infrastructure\FileSystem\FileSystem.pas',
	ConfigFile in 'src\Infrastructure\Config\ConfigFile.pas',
	Environment in 'src\Infrastructure\Environment\Environment.pas',
	AuthStrategy in 'src\Infrastructure\Authentication\AuthStrategy.pas',
	OAuthAppAuthStrategy in 'src\Infrastructure\Authentication\OAuthAppAuthStrategy.pas',
	VKIDAuthStrategy in 'src\Infrastructure\Authentication\VKIDAuthStrategy.pas',
	CookiePersistence in 'src\Infrastructure\Authentication\CookiePersistence.pas',
	VKIDLogin in 'src\Presentation\UI\Forms\VKIDLogin.pas' {VKIDLoginForm} ,
	ThreadStateManager in 'src\Infrastructure\State\ThreadStateManager.pas',
	ContentFieldProvider in 'src\Presentation\ContentField\ContentFieldProvider.pas',
	IconProvider in 'src\Presentation\Icon\IconProvider.pas',
	OperationLifecycleHandler in 'src\Application\Operation\OperationLifecycleHandler.pas',
	RemoteMetadataStore in 'src\Application\Metadata\RemoteMetadataStore.pas',
	BaseRemoteMetadataSyncManager in 'src\Application\Metadata\BaseRemoteMetadataSyncManager.pas',
	DescriptionStoreAdapter in 'src\Application\Description\DescriptionStoreAdapter.pas',
	TimestampStoreAdapter in 'src\Application\Timestamp\TimestampStoreAdapter.pas',
	DescriptionSyncManager in 'src\Application\Description\DescriptionSyncManager.pas',
	TimestampEntry in 'src\Domain\ValueObjects\TimestampEntry.pas',
	TimestampMetadata in 'src\Application\Timestamp\TimestampMetadata.pas',
	TimestampSyncManager in 'src\Application\Timestamp\TimestampSyncManager.pas',
	TimestampSyncGuard in 'src\Application\Timestamp\TimestampSyncGuard.pas',
	CloudAccessMapper in 'src\Domain\Services\CloudAccessMapper.pas',
	CloudFileOperationsAdapter in 'src\Infrastructure\Adapter\CloudFileOperationsAdapter.pas',
	CloudDescriptionOperationsAdapter in 'src\Infrastructure\Adapter\CloudDescriptionOperationsAdapter.pas',
	CloudDirItemJsonAdapter in 'src\Infrastructure\Adapter\CloudDirItemJsonAdapter.pas',
	CloudDirItemListJsonAdapter in 'src\Infrastructure\Adapter\CloudDirItemListJsonAdapter.pas',
	CloudOAuthJsonAdapter in 'src\Infrastructure\Adapter\CloudOAuthJsonAdapter.pas',
	CloudOperationResultJsonAdapter in 'src\Infrastructure\Adapter\CloudOperationResultJsonAdapter.pas',
	CloudSpaceJsonAdapter in 'src\Infrastructure\Adapter\CloudSpaceJsonAdapter.pas',
	CloudInviteListJsonAdapter in 'src\Infrastructure\Adapter\CloudInviteListJsonAdapter.pas',
	CloudEndpointsJsonAdapter in 'src\Infrastructure\Adapter\CloudEndpointsJsonAdapter.pas',
	CloudIncomingInviteListJsonAdapter in 'src\Infrastructure\Adapter\CloudIncomingInviteListJsonAdapter.pas',
	RetryHandler in 'src\Application\Retry\RetryHandler.pas',
	CommandDispatcher in 'src\Application\Command\CommandDispatcher.pas',
	ListingProvider in 'src\Application\Listing\ListingProvider.pas',
	DescriptionSyncGuard in 'src\Application\Description\DescriptionSyncGuard.pas',
	LocalFileDeletionHandler in 'src\Application\FileOps\LocalFileDeletionHandler.pas',
	DownloadSuccessHandler in 'src\Application\Download\DownloadSuccessHandler.pas',
	OperationActionExecutor in 'src\Application\Operation\OperationActionExecutor.pas',
	ListingSkipDecider in 'src\Application\Listing\ListingSkipDecider.pas',
	ListingPathValidator in 'src\Application\Listing\ListingPathValidator.pas',
	SameAccountMoveHandler in 'src\Application\FileOps\SameAccountMoveHandler.pas',
	FileStreamExecutor in 'src\Application\FileOps\FileStreamExecutor.pas',
	LocalFileConflictResolver in 'src\Application\Download\LocalFileConflictResolver.pas',
	ListingItemFetcher in 'src\Application\Listing\ListingItemFetcher.pas',
	SharedItemDeletionHandler in 'src\Application\FileOps\SharedItemDeletionHandler.pas',
	TrashBinOperationHandler in 'src\Application\Operations\TrashBinOperationHandler.pas',
	InviteOperationHandler in 'src\Application\Operations\InviteOperationHandler.pas',
	CrossAccountFileOperationHandler in 'src\Application\FileOps\CrossAccountFileOperationHandler.pas',
	CrossServerFileOperationHandler in 'src\Application\FileOps\CrossServerFileOperationHandler.pas',
	IconRenderingEngine in 'src\Presentation\Icon\IconRenderingEngine.pas',
	FileExecutionDispatcher in 'src\Application\Operations\FileExecutionDispatcher.pas',
	SharedItemActionHandler in 'src\Application\Operations\SharedItemActionHandler.pas',
	MoveOperationContextTracker in 'src\Application\FileOps\MoveOperationContextTracker.pas',
	DirectoryDeletionPreCheck in 'src\Application\FileOps\DirectoryDeletionPreCheck.pas',
	UploadPreparationValidator in 'src\Application\Upload\UploadPreparationValidator.pas',
	DownloadPreparationValidator in 'src\Application\Download\DownloadPreparationValidator.pas',
	UploadCompletionHandler in 'src\Application\Upload\UploadCompletionHandler.pas',
	RootListingHandler in 'src\Application\Listing\RootListingHandler.pas',
	PathListingHandler in 'src\Application\Listing\PathListingHandler.pas',
	IconContextBuilder in 'src\Presentation\Icon\IconContextBuilder.pas',
	OverwritePreparationHandler in 'src\Application\Upload\OverwritePreparationHandler.pas',
	OperationStatusContextBuilder in 'src\Application\Operation\OperationStatusContextBuilder.pas',
	ListingResultApplier in 'src\Application\Listing\ListingResultApplier.pas',
	DownloadOrchestrator in 'src\Application\Download\DownloadOrchestrator.pas',
	BaseDiskCache in 'src\Application\Cache\BaseDiskCache.pas',
	DirectoryCache in 'src\Application\Cache\DirectoryCache.pas',
	FileCache in 'src\Application\Cache\FileCache.pas',
	CloudHashCalculator in 'src\Infrastructure\Hash\CloudHashCalculator.pas',
	CloudShardManager in 'src\Infrastructure\Shard\CloudShardManager.pas',
	CloudErrorMapper in 'src\Domain\Services\CloudErrorMapper.pas',
	CloudCallbackTypes in 'src\Application\CloudCallbackTypes.pas',
	CloudFileDownloader in 'src\Application\Download\CloudFileDownloader.pas',
	ChunkedUploadHandler in 'src\Application\Upload\ChunkedUploadHandler.pas',
	CloudFileUploader in 'src\Application\Upload\CloudFileUploader.pas',
	CloudShareService in 'src\Application\Share\CloudShareService.pas',
	CloudListingService in 'src\Application\Listing\CloudListingService.pas',
	CloudFileOperations in 'src\Application\FileOps\CloudFileOperations.pas',
	InvitePropertyPresenter in 'src\Presentation\Presenter\InvitePropertyPresenter.pas',
	DeletedPropertyPresenter in 'src\Presentation\Presenter\DeletedPropertyPresenter.pas',
	AskPasswordPresenter in 'src\Presentation\Presenter\AskPasswordPresenter.pas',
	AskEncryptionPasswordPresenter in 'src\Presentation\Presenter\AskEncryptionPasswordPresenter.pas',
	OpenSSLProvider in 'src\Infrastructure\OpenSSL\OpenSSLProvider.pas',
	AccountCredentialsProvider in 'src\Infrastructure\Password\AccountCredentialsProvider.pas',
	FileEncryptionResolver in 'src\Infrastructure\Password\FileEncryptionResolver.pas',
	ProxyPasswordResolver in 'src\Infrastructure\Password\ProxyPasswordResolver.pas',
	ThumbnailSizeSelector in 'src\Application\Thumbnail\ThumbnailSizeSelector.pas',
	CloudThumbnailService in 'src\Application\Thumbnail\CloudThumbnailService.pas',
	ThumbnailBitmapConverter in 'src\Application\Thumbnail\ThumbnailBitmapConverter.pas',
	ServerProfile in 'src\Domain\ValueObjects\ServerProfile.pas',
	ServerProfileManager in 'src\Infrastructure\Settings\ServerProfileManager.pas',
	ServerConfigFetcher in 'src\Infrastructure\HTTP\ServerConfigFetcher.pas',
	TranslationManager in 'src\Infrastructure\Localization\TranslationManager.pas',
	CloudFileVersion in 'src\Domain\ValueObjects\CloudFileVersion.pas',
	CloudFileVersionJsonAdapter in 'src\Infrastructure\Adapter\CloudFileVersionJsonAdapter.pas';

{$IFDEF WIN64}
{$E wfx64}
{$ENDIF}
{$IFDEF WIN32}
{$E wfx}
{$ENDIF}
{$R *.res}

var
	MailRuCloudWFX: TWFXApplication;

function FsGetBackgroundFlags: Integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsGetBackgroundFlags);
end;

function FsInit(PluginNr: Integer; pProgressProc: TProgressProc; pLogProc: TLogProc; pRequestProc: TRequestProc): Integer; stdcall;
begin
	Result := 0;
end;

{GLORIOUS UNICODE MASTER RACE}

function FsInitW(PluginNr: Integer; pProgressProc: TProgressProcW; pLogProc: TLogProcW; pRequestProc: TRequestProcW): Integer; stdcall;
begin //Вход в плагин.
	Result := MailRuCloudWFX.FsInit(PluginNr, pProgressProc, pLogProc, pRequestProc);
end;

procedure FsStatusInfoW(RemoteDir: PWideChar; InfoStartEnd, InfoOperation: Integer); stdcall;
begin //Начало и конец операций FS
	MailRuCloudWFX.FsStatusInfo(RemoteDir, InfoStartEnd, InfoOperation);
end;

function FsFindFirstW(path: PWideChar; var FindData: tWIN32FINDDATAW): THandle; stdcall;
begin
	Exit(MailRuCloudWFX.FsFindFirst(path, FindData));
end;

function FsFindNextW(Hdl: THandle; var FindData: tWIN32FINDDATAW): Bool; stdcall;
begin
	Exit(MailRuCloudWFX.FsFindNext(Hdl, FindData));
end;

function FsFindClose(Hdl: THandle): Integer; stdcall;
begin //Завершение получения списка файлов. Result тоталом не используется (всегда равен 0)
	Exit(MailRuCloudWFX.FsFindClose(Hdl));
end;

function FsExecuteFileW(MainWin: THandle; RemoteName, Verb: PWideChar): Integer; stdcall; //Запуск файла
begin
	Exit(MailRuCloudWFX.FsExecuteFile(MainWin, RemoteName, Verb));
end;

function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: Integer; RemoteInfo: pRemoteInfo): Integer; stdcall;
begin //Копирование файла из файловой системы плагина
	Exit(MailRuCloudWFX.FsGetFile(RemoteName, LocalName, CopyFlags, RemoteInfo));
end;

function FsPutFileW(LocalName, RemoteName: PWideChar; CopyFlags: Integer): Integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsPutFile(LocalName, RemoteName, CopyFlags));
end;

function FsDeleteFileW(RemoteName: PWideChar): Bool; stdcall;
begin //Удаление файла из файловой системы плагина
	Exit(MailRuCloudWFX.FsDeleteFile(RemoteName));
end;

function FsMkDirW(path: PWideChar): Bool; stdcall;
begin
	Exit(MailRuCloudWFX.FsMkDir(path));
end;

function FsRemoveDirW(RemoteName: PWideChar): Bool; stdcall;
begin
	Exit(MailRuCloudWFX.FsRemoveDir(RemoteName));
end;

function FsRenMovFileW(OldName: PWideChar; NewName: PWideChar; Move: Boolean; OverWrite: Boolean; ri: pRemoteInfo): Integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsRenMovFile(OldName, NewName, Move, OverWrite, ri));
end;

function FsDisconnectW(DisconnectRoot: PWideChar): Bool; stdcall;
begin
	Exit(MailRuCloudWFX.FsDisconnect(DisconnectRoot));
end;

{The password manager can be created only after this method is being called — it needs passed parameters}
procedure FsSetCryptCallbackW(PCryptProc: TCryptProcW; CryptoNr: Integer; Flags: Integer); stdcall;
begin
	MailRuCloudWFX.FsSetCryptCallback(PCryptProc, CryptoNr, Flags);
end;

function FsContentGetSupportedField(FieldIndex: Integer; FieldName: PAnsiChar; Units: PAnsiChar; maxlen: Integer): Integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsContentGetSupportedField(FieldIndex, FieldName, Units, maxlen));
end;

function FsContentGetValueW(FileName: PWideChar; FieldIndex: Integer; UnitIndex: Integer; FieldValue: Pointer; maxlen: Integer; Flags: Integer): Integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsContentGetValue(FileName, FieldIndex, UnitIndex, FieldValue, maxlen, Flags));
end;

function FsExtractCustomIconW(RemoteName: PWideChar; ExtractFlags: Integer; var TheIcon: hicon): Integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsExtractCustomIcon(RemoteName, ExtractFlags, TheIcon));
end;

function FsGetPreviewBitmapW(RemoteName: PWideChar; Width, Height: Integer; var ReturnedBitmap: HBITMAP): Integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsGetPreviewBitmap(RemoteName, Width, Height, ReturnedBitmap));
end;

procedure InitPluginData;
begin
	MailRuCloudWFX := TWFXApplication.Create();
end;

procedure FreePluginData();
begin
	{Skip cleanup if background operations are active - process is likely terminating,
		OS will clean up all resources. Attempting cleanup while threads are running
		causes access violations when threads try to use freed objects.}
	if MailRuCloudWFX.HasActiveOperations then
	begin
		{Force immediate process termination to prevent Indy's finalization from
			clearing SSL function pointers while background threads are still using them.
			Without this, background HTTPS threads crash when calling nil function pointers.
			Process is terminating anyway - OS will clean up all resources.}
		ExitProcess(0);
	end;
	FreeAndNil(MailRuCloudWFX);
end;

procedure DllInit(Code: Integer);
begin
	case Code of
		DLL_PROCESS_ATTACH:
			InitPluginData;
		DLL_PROCESS_DETACH:
			FreePluginData();
		DLL_THREAD_ATTACH:
			begin
			end;
		DLL_THREAD_DETACH:
			begin
			end;
	end; //case
end;

exports
	FsGetDefRootName, FsInit, FsInitW, FsFindFirst, FsFindFirstW, FsFindNext, FsFindNextW, FsFindClose, FsGetFile, FsGetFileW, FsDisconnect, FsDisconnectW, FsStatusInfo, FsStatusInfoW, FsPutFile, FsPutFileW, FsDeleteFile, FsDeleteFileW, FsMkDir, FsMkDirW, FsRemoveDir, FsRemoveDirW, FsSetCryptCallback, FsSetCryptCallbackW, FsExecuteFileW, FsRenMovFile, FsRenMovFileW, FsGetBackgroundFlags, FsContentGetSupportedField, FsContentGetValue, FsContentGetValueW, FsExtractCustomIcon, FsExtractCustomIconW, FsGetPreviewBitmap, FsGetPreviewBitmapW;

begin
	DllProc := @DllInit;
	DllInit(DLL_PROCESS_ATTACH);

end.
