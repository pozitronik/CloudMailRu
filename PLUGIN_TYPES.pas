unit PLUGIN_Types;

{Типы и константы, используемые в плагине}
interface

uses windows;

const
	FS_FILE_OK = 0;
	FS_FILE_EXISTS = 1;
	FS_FILE_NOTFOUND = 2;
	FS_FILE_READERROR = 3;
	FS_FILE_WRITEERROR = 4;
	FS_FILE_USERABORT = 5;
	FS_FILE_NOTSUPPORTED = 6;
	FS_FILE_EXISTSRESUMEALLOWED = 7;

	FS_EXEC_OK = 0;

	FS_EXEC_ERROR = 1;
	FS_EXEC_YOURSELF = -1;
	FS_EXEC_SYMLINK = -2;

	FS_COPYFLAGS_OVERWRITE = 1;
	FS_COPYFLAGS_RESUME = 2;

	FS_COPYFLAGS_MOVE = 4;
	FS_COPYFLAGS_EXISTS_SAMECASE = 8;
	FS_COPYFLAGS_EXISTS_DIFFERENTCASE = 16;

const
	RT_Other = 0;
	RT_UserName = 1;
	RT_Password = 2;
	RT_Account = 3;
	RT_UserNameFirewall = 4;
	RT_PasswordFirewall = 5;
	RT_TargetDir = 6;
	RT_URL = 7;
	RT_MsgOK = 8;
	RT_MsgYesNo = 9;
	RT_MsgOKCancel = 10;

const
	msgtype_connect = 1;
	msgtype_disconnect = 2;
	msgtype_details = 3;
	msgtype_transfercomplete = 4;
	msgtype_connectcomplete = 5;
	msgtype_importanterror = 6;
	msgtype_operationcomplete = 7;

const
	FS_STATUS_START = 0;
	FS_STATUS_END = 1;

	FS_STATUS_OP_LIST = 1;
	FS_STATUS_OP_GET_SINGLE = 2;
	FS_STATUS_OP_GET_MULTI = 3;
	FS_STATUS_OP_PUT_SINGLE = 4;
	FS_STATUS_OP_PUT_MULTI = 5;
	FS_STATUS_OP_RENMOV_SINGLE = 6;
	FS_STATUS_OP_RENMOV_MULTI = 7;
	FS_STATUS_OP_DELETE = 8;
	FS_STATUS_OP_ATTRIB = 9;
	FS_STATUS_OP_MKDIR = 10;
	FS_STATUS_OP_EXEC = 11;
	FS_STATUS_OP_CALCSIZE = 12;
	FS_STATUS_OP_SEARCH = 13;
	FS_STATUS_OP_SEARCH_TEXT = 14;
	FS_STATUS_OP_SYNC_SEARCH = 15;
	FS_STATUS_OP_SYNC_GET = 16;
	FS_STATUS_OP_SYNC_PUT = 17;
	FS_STATUS_OP_SYNC_DELETE = 18;
	FS_STATUS_OP_GET_MULTI_THREAD = 19;
	FS_STATUS_OP_PUT_MULTI_THREAD = 20;

const
	FS_ICONFLAG_SMALL = 1;
	FS_ICONFLAG_BACKGROUND = 2;
	FS_ICON_USEDEFAULT = 0;
	FS_ICON_EXTRACTED = 1;
	FS_ICON_EXTRACTED_DESTROY = 2;
	FS_ICON_DELAYED = 3;

	{Flags for crypto callback function}
	FS_CRYPT_SAVE_PASSWORD = 1;
	FS_CRYPT_LOAD_PASSWORD = 2;
	FS_CRYPT_LOAD_PASSWORD_NO_UI = 3; {Load password only if master password has already been entered!}
	FS_CRYPT_COPY_PASSWORD = 4;
	FS_CRYPT_MOVE_PASSWORD = 5;
	FS_CRYPT_DELETE_PASSWORD = 6;

	FS_CRYPTOPT_MASTERPASS_SET = 1; {The user already has a master password defined}

	BG_DOWNLOAD = 1; {Plugin supports downloads in background}
	BG_UPLOAD = 2; {Plugin supports uploads in background}
	BG_ASK_USER = 4; {Plugin requires separate connection for background transfers -> ask user first}

const
	ft_nomorefields = 0;
	ft_numeric_32 = 1;
	ft_numeric_64 = 2;
	ft_numeric_floating = 3;
	ft_date = 4;
	ft_time = 5;
	ft_boolean = 6;
	ft_multiplechoice = 7;
	ft_string = 8;
	ft_fulltext = 9;
	ft_datetime = 10;
	ft_stringw = 11;

	//for ContentGetValue
	ft_nosuchfield = -1;
	ft_fileerror = -2;

type
	tRemoteInfo = record
		SizeLow, SizeHigh: longint;
		LastWriteTime: TFileTime;
		Attr: longint;
	end;

	pRemoteInfo = ^tRemoteInfo;

type
	tFsDefaultParamStruct = record
		size, PluginInterfaceVersionLow, PluginInterfaceVersionHi: longint;
		DefaultIniName: array [0 .. MAX_PATH - 1] of char;
	end;

	pFsDefaultParamStruct = ^tFsDefaultParamStruct;

type
	TProgressProc = function(PluginNr: integer; SourceName, TargetName: pchar; PercentDone: integer): integer; stdcall;
	TProgressProcW = function(PluginNr: integer; SourceName, TargetName: pwidechar; PercentDone: integer): integer; stdcall;
	TLogProc = procedure(PluginNr, MsgType: integer; LogString: pchar); stdcall;
	TLogProcW = procedure(PluginNr, MsgType: integer; LogString: pwidechar); stdcall;
	TRequestProc = function(PluginNr, RequestType: integer; CustomTitle, CustomText, ReturnedText: pchar; maxlen: integer): bool; stdcall;
	TRequestProcW = function(PluginNr, RequestType: integer; CustomTitle, CustomText, ReturnedText: pwidechar; maxlen: integer): bool; stdcall;
	PCryptProc = ^TCryptProc;
	TCryptProc = function(PluginNr, CryptoNumber: integer; mode: integer; ConnectionName, Password: pchar; maxlen: integer): integer; stdcall;
	PCryptProcW = ^TCryptProcW;
	TCryptProcW = function(PluginNr, CryptoNumber: integer; mode: integer; ConnectionName, Password: pwidechar; maxlen: integer): integer; stdcall;

	{------------------------------------------------------------------------------}

implementation

end.
