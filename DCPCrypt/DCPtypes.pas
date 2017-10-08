unit DCPtypes;

{$INCLUDE 'jedi.inc'}

interface

{ ************************************ }
{ A few predefined types to help out }
{ ************************************ }

type
  Pbyte = ^byte;
  Pword = ^word;
  Pdword = ^dword;
  Pint64 = ^int64;
  dword = longword;
  Pwordarray = ^Twordarray;
  Twordarray = array [0..19383] of word;
  Pdwordarray = ^Tdwordarray;
  Tdwordarray = array [0..8191] of dword;
{$IFNDEF SUPPORTS_UNICODE_STRING}
  RawByteString = AnsiString;
{$ENDIF}
{$IFNDEF COMPILER11_UP}
  TBytes = array of Byte;
{$ENDIF ~COMPILER11_UP}
{$IFNDEF SUPPORTS_UINT64}
  UInt64 = Int64;
{$ENDIF ~SUPPORTS_UINT64}

type
{$IFNDEF COMPILER16_UP}
  NativeUInt = {$IFDEF CPU64} UInt64 {$ELSE} Cardinal {$ENDIF};
{$ENDIF}
  PointerToInt = {$IFDEF COMPILER16_UP} PByte {$ELSE} NativeUInt {$ENDIF};

implementation

end.
