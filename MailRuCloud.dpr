library MailRuCloud;

{$R *.dres}

uses
{$IFDEF DEBUG}
	FastMM4 in 'FastMM\FastMM4.pas',
	FastMM4Messages in 'FastMM\FastMM4Messages.pas',
{$ENDIF }
	ANSIFunctions in 'types\ANSIFunctions.pas',
	AccountSettings in 'models\settings\AccountSettings.pas',
	Accounts in 'forms\Accounts.pas'{AccountsForm},
	AccountsManager in 'models\settings\AccountsManager.pas',
	AskPassword in 'forms\AskPassword.pas'{AskPasswordForm},
	CMRConstants in 'types\CMRConstants.pas',
	CMRDirItem in 'models\dto\CMRDirItem.pas',
	CMRDirItemList in 'models\dto\CMRDirItemList.pas',
	CMRFileIdentity in 'models\dto\CMRFileIdentity.pas',
	CMRIncomingInvite in 'models\dto\CMRIncomingInvite.pas',
	CMRIncomingInviteList in 'models\dto\CMRIncomingInviteList.pas',
	CMRInvite in 'models\dto\CMRInvite.pas',
	CMRInviteList in 'models\dto\CMRInviteList.pas',
	CMROAuth in 'models\dto\CMROAuth.pas',
	CMROperationResult in 'models\dto\CMROperationResult.pas',
	CMROwner in 'models\dto\CMROwner.pas',
	CMRSpace in 'models\dto\CMRSpace.pas',
	LANGUAGE_STRINGS in 'types\LANGUAGE_STRINGS.pas',
	CMRTwostep in 'models\dto\CMRTwostep.pas',
	ChunkedFileStream in 'models\ChunkedFileStream.pas',
	Classes,
	CloudMailRu in 'models\CloudMailRu.pas',
	CloudMailRuHTTP in 'models\http\CloudMailRuHTTP.pas',
	CloudSettings in 'models\settings\CloudSettings.pas',
	ConnectionManager in 'models\ConnectionManager.pas',
	ConnectionSettings in 'models\settings\ConnectionSettings.pas',
	DCPbase64 in 'DCPCrypt\DCPbase64.pas',
	DCPblockciphers in 'DCPCrypt\DCPblockciphers.pas',
	DCPconst in 'DCPCrypt\DCPconst.pas',
	DCPcrypt2 in 'DCPCrypt\DCPcrypt2.pas',
	DCPrijndael in 'DCPCrypt\Ciphers\DCPrijndael.pas',
	DCPsha1 in 'DCPCrypt\Hashes\DCPsha1.pas',
	DCPtypes in 'DCPCrypt\DCPtypes.pas',
	DateUtils,
	DebugHelper in 'helpers\DebugHelper.pas',
	DeletedProperty in 'forms\DeletedProperty.pas'{DeletedPropertyForm},
	Description in 'models\Description.pas',
	FileCipher in 'models\cipher\FileCipher.pas',
	FileHelper in 'helpers\FileHelper.pas',
	FileSplitInfo in 'models\FileSplitInfo.pas',
	HTTPManager in 'models\http\HTTPManager.pas',
	HashInfo in 'models\HashInfo.pas',
	IconHelper in 'helpers\IconHelper.pas',
	IdSSLOpenSSLHeaders,
	IniFiles,
	IniFilesHelper in 'helpers\IniFilesHelper.pas',
	InviteProperty in 'forms\InviteProperty.pas'{InvitePropertyForm},
	JSON,
	JSONHelper in 'helpers\JSONHelper.pas',
	Messages,
	PLUGIN_TYPES in 'types\PLUGIN_TYPES.pas',
	ParsingHelper in 'helpers\ParsingHelper.pas',
	PathHelper in 'helpers\PathHelper.pas',
	PluginHelper in 'helpers\PluginHelper.pas',
	PluginSettings in 'models\settings\PluginSettings.pas',
	PluginSettingsManager in 'models\settings\PluginSettingsManager.pas',
	ProxySettings in 'models\settings\ProxySettings.pas',
	RealPath in 'models\dto\RealPath.pas',
	Registration in 'forms\Registration.pas'{AskPasswordForm},
	RemoteProperty in 'forms\RemoteProperty.pas'{PropertyForm},
	SETTINGS_CONSTANTS in 'models\settings\SETTINGS_CONSTANTS.pas',
	StreamingSettings in 'models\settings\StreamingSettings.pas',
	StringHelper in 'helpers\StringHelper.pas',
	SysUtils,
	System.Generics.Collections,
	SystemHelper in 'helpers\SystemHelper.pas',
	TCHelper in 'helpers\TCHelper.pas',
	TCLogger in 'models\TCLogger.pas',
	TCPasswordManager in 'models\TCPasswordManager.pas',
	TCProgress in 'models\TCProgress.pas',
	TCRequest in 'models\TCRequest.pas',
	Variants,
	Vcl.controls,
	WSList in 'models\WSList.pas',
	Windows,
	WindowsHelper in 'helpers\WindowsHelper.pas',
	MailRuCloudWFX in 'models\wfx\MailRuCloudWFX.pas',
	WFXInterface in 'models\wfx\WFXInterface.pas';

{$IFDEF WIN64}
{$E wfx64}
{$ENDIF}
{$IFDEF WIN32}
{$E wfx}
{$ENDIF}
{$R *.res}

var
	MailRuCloudWFX: TMailRuCloudWFX;

function FsGetBackgroundFlags: integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsGetBackgroundFlags);
end;

function FsInit(PluginNr: integer; pProgressProc: TProgressProc; pLogProc: TLogProc; pRequestProc: TRequestProc): integer; stdcall;
begin
	Result := 0;
end;

{GLORIOUS UNICODE MASTER RACE}

function FsInitW(PluginNr: integer; pProgressProc: TProgressProcW; pLogProc: TLogProcW; pRequestProc: TRequestProcW): integer; stdcall; //Вход в плагин.
begin
	Result := MailRuCloudWFX.FsInit(PluginNr, pProgressProc, pLogProc, pRequestProc);
end;

procedure FsStatusInfoW(RemoteDir: PWideChar; InfoStartEnd, InfoOperation: integer); stdcall; //Начало и конец операций FS
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

function FsFindClose(Hdl: THandle): integer; stdcall;
begin //Завершение получения списка файлов. Result тоталом не используется (всегда равен 0)
	Exit(MailRuCloudWFX.FsFindClose(Hdl));
end;

function FsExecuteFileW(MainWin: THandle; RemoteName, Verb: PWideChar): integer; stdcall; //Запуск файла
begin
	Exit(MailRuCloudWFX.FsExecuteFile(MainWin, RemoteName, Verb));
end;

function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: integer; RemoteInfo: pRemoteInfo): integer; stdcall; //Копирование файла из файловой системы плагина
begin
	Exit(MailRuCloudWFX.FsGetFile(RemoteName, LocalName, CopyFlags, RemoteInfo));
end;

function FsPutFileW(LocalName, RemoteName: PWideChar; CopyFlags: integer): integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsPutFile(RemoteName, LocalName, CopyFlags));
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

function FsRenMovFileW(OldName: PWideChar; NewName: PWideChar; Move: Boolean; OverWrite: Boolean; ri: pRemoteInfo): integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsRenMovFile(OldName, NewName, Move, OverWrite, ri));
end;

function FsDisconnectW(DisconnectRoot: PWideChar): Bool; stdcall;
begin
	Exit(MailRuCloudWFX.FsDisconnect(DisconnectRoot));
end;

{The password manager can be created only after this method is being called — it needs passed parameters}
procedure FsSetCryptCallbackW(PCryptProc: TCryptProcW; CryptoNr: integer; Flags: integer); stdcall;
begin
	MailRuCloudWFX.FsSetCryptCallback(PCryptProc, CryptoNr, Flags);
end;

function FsContentGetSupportedField(FieldIndex: integer; FieldName: PAnsiChar; Units: PAnsiChar; maxlen: integer): integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsContentGetSupportedField(FieldIndex, FieldName, Units, maxlen));
end;

function FsContentGetValueW(FileName: PWideChar; FieldIndex: integer; UnitIndex: integer; FieldValue: Pointer; maxlen: integer; Flags: integer): integer; stdcall;
begin
	Exit(MailRuCloudWFX.FsContentGetValue(FileName, FieldIndex, UnitIndex, FieldValue, maxlen, Flags));
end;

function FsExtractCustomIconW(RemoteName: PWideChar; ExtractFlags: integer; var TheIcon: hicon): integer; stdcall;
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

procedure DllInit(Code: integer);
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
