library MailRuCloud;

{$R *.dres}

uses
{$IFDEF DEBUG}
	FastMM4 in 'src\libs\FastMM\FastMM4.pas',
	FastMM4Messages in 'src\libs\FastMM\FastMM4Messages.pas',
{$ENDIF}
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
	CloudSettings in 'src\models\settings\CloudSettings.pas',
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
	SysUtils,
	System.Generics.Collections,
	SystemHelper in 'src\helpers\SystemHelper.pas',
	TCHelper in 'src\helpers\TCHelper.pas',
	TCLogger in 'src\models\tc\TCLogger.pas',
	TCPasswordManager in 'src\models\tc\TCPasswordManager.pas',
	TCProgress in 'src\models\tc\TCProgress.pas',
	TCRequest in 'src\models\tc\TCRequest.pas',
	Variants,
	Vcl.Controls,
	WSList in 'src\models\WSList.pas',
	Windows,
	WindowsHelper in 'src\helpers\WindowsHelper.pas',
	MailRuCloudWFX in 'src\models\wfx\MailRuCloudWFX.pas',
	WFXInterface in 'src\models\wfx\WFXInterface.pas',
	CipherInterface in 'src\models\cipher\CipherInterface.pas';

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
{$IFDEF DEBUG}
	ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
	DllProc := @DllInit;
	DllInit(DLL_PROCESS_ATTACH);

end.
