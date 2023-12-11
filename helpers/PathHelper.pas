unit PathHelper;

{Paths manipulation helper methods}
interface

uses
	SysUtils,
	StringHelper;

function GetUNCFilePath(FilePath: WideString): WideString;
function GetLFCFilePath(FilePath: WideString): WideString; //UNC => LFC
function IncludeSlash(const Str: WideString): WideString;
function ChangePathFileName(const FilePath, NewFileName: WideString): WideString;
function CopyExt(FromFilename, ToFilename: WideString): WideString;
function PathToUrl(path: WideString; RestrictEmptyUrl: boolean = true; DoUrlEncode: boolean = true): WideString;
function UrlToPath(URL: WideString): WideString;
function ExtractUniversalFilePath(const FileName: string): string;
function ExtractUniversalFileName(const FileName: string): string;
function ExtractUniversalFileExt(const FileName: string; TrimDot: boolean = False): string;

implementation

function IncludeSlash(const Str: WideString): WideString;
begin
	if Str = EmptyWideStr then
		exit('/');

	Result := Str;
	if not(Result[High(Result)] = '/') then
		Result := Result + '/';
end;

function ChangePathFileName(const FilePath, NewFileName: WideString): WideString;
begin
	Result := ExtractUniversalFilePath(FilePath) + NewFileName;
end;

function CopyExt(FromFilename, ToFilename: WideString): WideString;
begin
	Result := ChangeFileExt(ToFilename, ExtractFileExt(FromFilename));
end;

function GetUNCFilePath(FilePath: WideString): WideString;
begin
	Result := ExpandUNCFileName(FilePath);
	if not(Pos(WideString('\\'), Result) = 1) then
		Result := '\\?\' + FilePath;
end;

function GetLFCFilePath(FilePath: WideString): WideString; //UNC => LFC
begin
	Result := FilePath;
	if Pos(WideString('\\?\'), Result) = 1 then
		Result := Copy(FilePath, 5, Length(FilePath) - 4);
end;

function PathToUrl(path: WideString; RestrictEmptyUrl: boolean = true; DoUrlEncode: boolean = true): WideString;
begin
	Result := StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]);
	if DoUrlEncode then
		Result := UrlEncode(Result);
	if (Result = EmptyWideStr) and RestrictEmptyUrl then
		Result := '/';
end;

function UrlToPath(URL: WideString): WideString;
begin
	Result := StringReplace(URL, WideString('/'), WideString('\'), [rfReplaceAll, rfIgnoreCase]);
end;

function ExtractUniversalFilePath(const FileName: string): string;
var
	I: integer;
begin
	I := FileName.LastDelimiter('/' + '\' + DriveDelim);
	Result := FileName.Substring(0, I + 1);
end;

function ExtractUniversalFileName(const FileName: string): string;
var
	I: integer;
begin
	I := FileName.LastDelimiter('/' + '\' + DriveDelim);
	Result := FileName.Substring(I + 1);
end;

function ExtractUniversalFileExt(const FileName: string; TrimDot: boolean = False): string;
var
	I: integer;
begin
	I := FileName.LastDelimiter('.' + '/' + '\' + DriveDelim);
	if (I >= 0) and (FileName.Chars[I] = '.') then
		if TrimDot then
			Result := FileName.Substring(I + 1)
		else
			Result := FileName.Substring(I)
	else
		Result := EmptyWideStr;
end;

end.
