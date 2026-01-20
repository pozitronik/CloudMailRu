unit WFXInterface;

interface

uses
	Windows,
	PLUGIN_TYPES;

type
	{This class implements a basic abstract filesystem plugin}
	{TODO: do not use pointers}
	IWFXInterface = interface
		['{30C665E4-3796-435A-BE77-18E09268B8FF}']
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

end.
