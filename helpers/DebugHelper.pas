unit DebugHelper;

interface

{$IFDEF DEBUG}

uses
	SysUtils;

Procedure FileLog(S: WideString);
{$ENDIF}

implementation

{$IFDEF DEBUG}

Procedure FileLog(S: WideString);
var
	f: textfile;
begin
	Assign(f, IncludeTrailingBackslash(ExtractFilePath(GetModuleName(hInstance))) + 'debug.log');
	Rewrite(f);
	Write(f, S);
	close(f);
end;
{$ENDIF}

end.
