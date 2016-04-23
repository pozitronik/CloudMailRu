unit PLUGIN_Types;
{Типы и константы, используемые в плагине}
interface

uses windows;


const FS_FILE_OK=0;
      FS_FILE_EXISTS=1;
      FS_FILE_NOTFOUND=2;
      FS_FILE_READERROR=3;
      FS_FILE_WRITEERROR=4;
      FS_FILE_USERABORT=5;
      FS_FILE_NOTSUPPORTED=6;
      FS_FILE_EXISTSRESUMEALLOWED=7;

      FS_EXEC_OK=0;
      FS_EXEC_ERROR=1;      FS_EXEC_YOURSELF=-1;
      FS_EXEC_SYMLINK=-2;

      FS_COPYFLAGS_OVERWRITE=1;
      FS_COPYFLAGS_RESUME=2;

      FS_COPYFLAGS_MOVE=4;
      FS_COPYFLAGS_EXISTS_SAMECASE=8;
      FS_COPYFLAGS_EXISTS_DIFFERENTCASE=16;

const
  RT_Other=0;
  RT_UserName=1;
  RT_Password=2;
  RT_Account=3;
  RT_UserNameFirewall=4;
  RT_PasswordFirewall=5;
  RT_TargetDir=6;
  RT_URL=7;
  RT_MsgOK=8;
  RT_MsgYesNo=9;
  RT_MsgOKCancel=10;

const msgtype_connect=1;
      msgtype_disconnect=2;
      msgtype_details=3;
      msgtype_transfercomplete=4;
      msgtype_connectcomplete=5;
      msgtype_importanterror=6;
      msgtype_operationcomplete=7;

const FS_STATUS_START=0;
      FS_STATUS_END=1;

      FS_STATUS_OP_LIST=1;
      FS_STATUS_OP_GET_SINGLE=2;
      FS_STATUS_OP_GET_MULTI=3;
      FS_STATUS_OP_PUT_SINGLE=4;
      FS_STATUS_OP_PUT_MULTI=5;
      FS_STATUS_OP_RENMOV_SINGLE=6;
      FS_STATUS_OP_RENMOV_MULTI=7;
      FS_STATUS_OP_DELETE=8;
      FS_STATUS_OP_ATTRIB=9;
      FS_STATUS_OP_MKDIR=10;
      FS_STATUS_OP_EXEC=11;
      FS_STATUS_OP_CALCSIZE=12;
      FS_STATUS_OP_SEARCH=13;
      FS_STATUS_OP_SEARCH_TEXT=14;

const FS_ICONFLAG_SMALL=1;
      FS_ICONFLAG_BACKGROUND=2;
      FS_ICON_USEDEFAULT=0;
      FS_ICON_EXTRACTED=1;
      FS_ICON_EXTRACTED_DESTROY=2;
      FS_ICON_DELAYED=3;

Const
  Hallo='TWinAmp2 (Fast) version 1.0 by Pavel Dubrovsky.';
  About=Hallo;
  NO_INI_FILE='?';
type
  tRemoteInfo=record
    SizeLow,SizeHigh:longint;
    LastWriteTime:TFileTime;
    Attr:longint;
  end;
  pRemoteInfo=^tRemoteInfo;

type
  tFsDefaultParamStruct=record
    size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi:longint;
    DefaultIniName:array[0..MAX_PATH-1] of char;
  end;
  pFsDefaultParamStruct=^tFsDefaultParamStruct;

type
  TProgressProc=function(PluginNr:integer;SourceName,TargetName:pchar;PercentDone:integer):integer; stdcall;
  TLogProc=procedure(PluginNr,MsgType:integer;LogString:pchar); stdcall;
  TRequestProc=function(PluginNr,RequestType:integer;CustomTitle,CustomText,ReturnedText:pchar;maxlen:integer):bool; stdcall;



{------------------------------------------------------------------------------}

implementation



end.
