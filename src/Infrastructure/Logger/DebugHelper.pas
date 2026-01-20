unit DebugHelper;

interface

{$IFDEF DEBUG}

uses
	SysUtils,
	StrUtils;

Procedure FileLog(S: WideString; FileMask: WideString = 'debug_%t.log');
{$ENDIF}

implementation

{$IFDEF DEBUG}

Procedure FileLog(S: WideString; FileMask: WideString = 'debug_%t.log');
var
	DebugFileHandler: TextFile;
	FileName: WideString;
begin
	FileName := ReplaceText(FileMask, '%t', FormatDateTime('dd.mm.yyyy_hh.nn.ss', Now));
	Assign(DebugFileHandler, IncludeTrailingBackslash(ExtractFilePath(GetModuleName(hInstance))) + FileName);
	Rewrite(DebugFileHandler);
	Write(DebugFileHandler, S);
	close(DebugFileHandler);
end;
{$ENDIF}

end.
