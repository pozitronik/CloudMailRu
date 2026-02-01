unit PathHelper;

{Paths manipulation helper methods}
interface

uses
	SysUtils,
	StringHelper;

function GetUNCFilePath(FilePath: WideString): WideString;
function IncludeSlash(const URL: WideString): WideString;
function PathToUrl(path: WideString; RestrictEmptyUrl: boolean = true; DoUrlEncode: boolean = true): WideString;
function ExtractUniversalFilePath(const FileName: string): string;
function ExtractUniversalFileName(const FileName: string): string;

implementation

function IncludeSlash(const URL: WideString): WideString;
begin
	if URL = EmptyWideStr then
		exit('/');

	Result := URL;
	if not(Result[High(Result)] = '/') then
		Result := Result + '/';
end;

function GetUNCFilePath(FilePath: WideString): WideString;
begin
	Result := ExpandUNCFileName(FilePath);
	if not(Pos(WideString('\\'), Result) = 1) then
		Result := '\\?\' + FilePath;
end;

function PathToUrl(path: WideString; RestrictEmptyUrl: boolean = true; DoUrlEncode: boolean = true): WideString;
begin
	Result := StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]);
	if DoUrlEncode then
		Result := UrlEncode(Result);
	if (Result = EmptyWideStr) and RestrictEmptyUrl then
		Result := '/';
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

end.
