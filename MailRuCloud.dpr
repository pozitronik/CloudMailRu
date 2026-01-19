library MailRuCloud;

{$R *.dres}

uses
	DebugHelper in 'src\helpers\DebugHelper.pas',
	ANSIFunctions in 'src\models\wfx\ANSIFunctions.pas',
	AccountSettings in 'src\models\settings\AccountSettings.pas',
	Accounts in 'src\forms\Accounts.pas'{AccountsForm},
	AccountsManager in 'src\models\settings\AccountsManager.pas',
	AskPassword in 'src\forms\AskPassword.pas'{AskPasswordForm},
	CMRConstants in 'src\types\CMRConstants.pas',
	CMRDirItem in 'src\models\dto\CMRDirItem.pas',
	CMRDirItemList in 'src\models\dto\CMRDirItemList.pas',
	CMRFileIdentity in 'src\models\dto\CMRFileIdentity.pas',
	CMRIncomingInvite in 'src\models\dto\CMRIncomingInvite.pas',
	CMRIncomingInviteList in 'src\models\dto\CMRIncomingInviteList.pas',
	CMRInvite in 'src\models\dto\CMRInvite.pas',
	CMRInviteList in 'src\models\dto\CMRInviteList.pas',
	CMROAuth in 'src\models\dto\CMROAuth.pas',
	CMROperationResult in 'src\models\dto\CMROperationResult.pas',
	CMROwner in 'src\models\dto\CMROwner.pas',
	CMRSpace in 'src\models\dto\CMRSpace.pas',
	LANGUAGE_STRINGS in 'src\types\LANGUAGE_STRINGS.pas',
	CMRTwostep in 'src\models\dto\CMRTwostep.pas',
	ChunkedFileStream in 'src\models\ChunkedFileStream.pas',
	Classes,
	CloudMailRu in 'src\models\CloudMailRu.pas',
	CloudMailRuHTTP in 'src\models\http\CloudMailRuHTTP.pas',
	ICloudHTTPInterface in 'src\models\http\ICloudHTTPInterface.pas',
	CloudSettings in 'src\models\settings\CloudSettings.pas',
	IConnectionManagerInterface in 'src\models\IConnectionManagerInterface.pas',
	ConnectionManager in 'src\models\ConnectionManager.pas',
	ConnectionSettings in 'src\models\settings\ConnectionSettings.pas',
	DCPbase64 in 'src\libs\DCPCrypt\DCPbase64.pas',
	DCPblockciphers in 'src\libs\DCPCrypt\DCPblockciphers.pas',
	DCPconst in 'src\libs\DCPCrypt\DCPconst.pas',
	DCPcrypt2 in 'src\libs\DCPCrypt\DCPcrypt2.pas',
	DCPrijndael in 'src\libs\DCPCrypt\Ciphers\DCPrijndael.pas',
	DCPsha1 in 'src\libs\DCPCrypt\Hashes\DCPsha1.pas',
	DCPtypes in 'src\libs\DCPCrypt\DCPtypes.pas',
	DateUtils,
	DeletedProperty in 'src\forms\DeletedProperty.pas'{DeletedPropertyForm},
	Description in 'src\models\Description.pas',
	FileCipher in 'src\models\cipher\FileCipher.pas',
	FileHelper in 'src\helpers\FileHelper.pas',
	FileSplitInfo in 'src\models\FileSplitInfo.pas',
	HTTPManager in 'src\models\http\HTTPManager.pas',
	HashInfo in 'src\models\HashInfo.pas',
	IconHelper in 'src\helpers\IconHelper.pas',
	IdSSLOpenSSLHeaders,
	IniFiles,
	IniFilesHelper in 'src\helpers\IniFilesHelper.pas',
	InviteProperty in 'src\forms\InviteProperty.pas'{InvitePropertyForm},
	JSON,
	JSONHelper in 'src\helpers\JSONHelper.pas',
	Messages,
	PLUGIN_TYPES in 'src\types\PLUGIN_TYPES.pas',
	ParsingHelper in 'src\helpers\ParsingHelper.pas',
	PathHelper in 'src\helpers\PathHelper.pas',
	PluginHelper in 'src\helpers\PluginHelper.pas',
	PluginSettings in 'src\models\settings\PluginSettings.pas',
	PluginSettingsManager in 'src\models\settings\PluginSettingsManager.pas',
	ProxySettings in 'src\models\settings\ProxySettings.pas',
	RealPath in 'src\models\dto\RealPath.pas',
	Registration in 'src\forms\Registration.pas'{AskPasswordForm},
	RemoteProperty in 'src\forms\RemoteProperty.pas'{PropertyForm},
	SETTINGS_CONSTANTS in 'src\models\settings\SETTINGS_CONSTANTS.pas',
	StreamingSettings in 'src\models\settings\StreamingSettings.pas',
	StringHelper in 'src\helpers\StringHelper.pas',
	TokenRetryHelper in 'src\helpers\TokenRetryHelper.pas',
	SysUtils,
	System.Generics.Collections,
	SystemHelper in 'src\helpers\SystemHelper.pas',
	TCHelper in 'src\helpers\TCHelper.pas',
	ILoggerInterface in 'src\models\logger\ILoggerInterface.pas',
	IProgressInterface in 'src\models\progress\IProgressInterface.pas',
	TCLogger in 'src\models\logger\TCLogger.pas',
	IPasswordManagerInterface in 'src\models\password\IPasswordManagerInterface.pas',
	TCPasswordManager in 'src\models\password\TCPasswordManager.pas',
	TCProgress in 'src\models\progress\TCProgress.pas',
	IRequestInterface in 'src\models\request\IRequestInterface.pas',
	TCRequest in 'src\models\request\TCRequest.pas',
	Variants,
	Vcl.Controls,
	WSList in 'src\models\WSList.pas',
	Windows,
	WindowsHelper in 'src\helpers\WindowsHelper.pas',
	MailRuCloudWFX in 'src\models\wfx\MailRuCloudWFX.pas',
	WFXInterface in 'src\models\wfx\WFXInterface.pas',
	CipherInterface in 'src\models\cipher\CipherInterface.pas',
	IAccountsManagerInterface in 'src\models\settings\IAccountsManagerInterface.pas',
	IPluginSettingsManagerInterface in 'src\models\settings\IPluginSettingsManagerInterface.pas',
	IPasswordUIProviderInterface in 'src\models\ui\IPasswordUIProviderInterface.pas',
	PasswordUIProvider in 'src\models\ui\PasswordUIProvider.pas',
	IHTTPManagerInterface in 'src\models\http\IHTTPManagerInterface.pas',
	ICipherValidatorInterface in 'src\models\cipher\ICipherValidatorInterface.pas',
	CipherValidator in 'src\models\cipher\CipherValidator.pas',
	IFileSystemInterface in 'src\models\filesystem\IFileSystemInterface.pas',
	WindowsFileSystem in 'src\models\filesystem\WindowsFileSystem.pas',
	IConfigFileInterface in 'src\models\config\IConfigFileInterface.pas',
	IniConfigFile in 'src\models\config\IniConfigFile.pas',
	IEnvironmentInterface in 'src\models\environment\IEnvironmentInterface.pas',
	WindowsEnvironment in 'src\models\environment\WindowsEnvironment.pas',
	IAuthStrategyInterface in 'src\models\auth\IAuthStrategyInterface.pas',
	OAuthAppAuthStrategy in 'src\models\auth\OAuthAppAuthStrategy.pas',
	SharedAccountAuthStrategy in 'src\models\auth\SharedAccountAuthStrategy.pas',
	WebAuthStrategy in 'src\models\auth\WebAuthStrategy.pas',
	TwoStepAuthStrategy in 'src\models\auth\TwoStepAuthStrategy.pas',
	OldOAuthStrategy in 'src\models\auth\OldOAuthStrategy.pas',
	ThreadStateManager in 'src\Infrastructure\State\ThreadStateManager.pas',
	ContentFieldProvider in 'src\Presentation\ContentField\ContentFieldProvider.pas',
	IconProvider in 'src\Presentation\Icon\IconProvider.pas',
	OperationLifecycleHandler in 'src\Application\Operation\OperationLifecycleHandler.pas',
	DescriptionSyncManager in 'src\Application\Description\DescriptionSyncManager.pas',
	CloudDescriptionOpsAdapter in 'src\Infrastructure\Adapter\CloudDescriptionOpsAdapter.pas',
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
	AccountRegistrationHandler in 'src\Application\Operations\AccountRegistrationHandler.pas',
	TrashBinOperationHandler in 'src\Application\Operations\TrashBinOperationHandler.pas',
	InviteOperationHandler in 'src\Application\Operations\InviteOperationHandler.pas',
	CrossAccountFileOperationHandler in 'src\Application\FileOps\CrossAccountFileOperationHandler.pas',
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
	DownloadOrchestrator in 'src\Application\Download\DownloadOrchestrator.pas';

{$IFDEF WIN64}
{$E wfx64}
{$ENDIF}
{$IFDEF WIN32}
{$E wfx}
{$ENDIF}
{$R *.res}

var
	MailRuCloudWFX: TMailRuCloudWFX;

function FsGetBackgroundFlags: Integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsGetBackgroundFlags);
end;

function FsInit(PluginNr: Integer; pProgressProc: TProgressProc; pLogProc: TLogProc; pRequestProc: TRequestProc): Integer; stdcall;
begin
	Result := 0;
end;

{GLORIOUS UNICODE MASTER RACE}

function FsInitW(PluginNr: Integer; pProgressProc: TProgressProcW; pLogProc: TLogProcW; pRequestProc: TRequestProcW): Integer; stdcall; //Вход в плагин.
begin
	Result := MailRuCloudWFX.FsInit(PluginNr, pProgressProc, pLogProc, pRequestProc);
end;

procedure FsStatusInfoW(RemoteDir: PWideChar; InfoStartEnd, InfoOperation: Integer); stdcall; //Начало и конец операций FS
begin
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

function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: Integer; RemoteInfo: pRemoteInfo): Integer; stdcall; //Копирование файла из файловой системы плагина
begin
	Exit(MailRuCloudWFX.FsGetFile(RemoteName, LocalName, CopyFlags, RemoteInfo));
end;

function FsPutFileW(LocalName, RemoteName: PWideChar; CopyFlags: Integer): Integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsPutFile(LocalName, RemoteName, CopyFlags));
end;

function FsDeleteFileW(RemoteName: PWideChar): Bool; stdcall; //Удаление файла из файловой системы плагина
begin
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

procedure InitPluginData;
begin
	MailRuCloudWFX := TMailRuCloudWFX.Create();
end;

procedure FreePluginData();
begin
	MailRuCloudWFX.Destroy;
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
	FsGetDefRootName, FsInit, FsInitW, FsFindFirst, FsFindFirstW, FsFindNext, FsFindNextW, FsFindClose, FsGetFile, FsGetFileW, FsDisconnect, FsDisconnectW, FsStatusInfo, FsStatusInfoW, FsPutFile, FsPutFileW, FsDeleteFile, FsDeleteFileW, FsMkDir, FsMkDirW, FsRemoveDir, FsRemoveDirW, FsSetCryptCallback, FsSetCryptCallbackW, FsExecuteFileW, FsRenMovFile, FsRenMovFileW, FsGetBackgroundFlags, FsContentGetSupportedField, FsContentGetValue, FsContentGetValueW, FsExtractCustomIcon, FsExtractCustomIconW;

begin
	DllProc := @DllInit;
	DllInit(DLL_PROCESS_ATTACH);

end.
