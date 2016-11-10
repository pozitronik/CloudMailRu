unit MRC_Helper;

interface

uses Classes, Windows, SysUtils, MultiMon, Math;

const
	MAX_UNC_PATH = 32767;

type
	TRealPath = record
		account: WideString;
		path: WideString;
	end;


function Implode(S: TStringList; Delimiter: WideString): WideString;
function Explode(S: WideString; Delimiter: char): TStringList;
function ExtractRealPath(VirtualPath: WideString): TRealPath;
function SizeOfFile(const FileName: String): Int64;
function DateTimeToUnix(ConvDate: TDateTime): Integer;
function CheckFlag(Check: Byte; Flags: Integer): boolean; // Определяет, установлен ли указанный бит
function DateTimeToFileTime(FileTime: TDateTime): TFileTime;
procedure SetAllFileTime(const FileName: string; const FileTime: TFileTime);
procedure CenterWindow(WindowToStay, WindowToCenter: HWND);
function UrlEncode(URL: WideString): WideString;
function FindTCWindow: HWND;
function GetTmpDir: WideString;
function GetTmpFileName(Prefix: WideString = ''): WideString;
function CopyExt(FromFilename, ToFilename: WideString): WideString;
function GetUNCFilePath(FilePath: WideString): WideString;
function GetWord(command: WideString; WordIndex: Integer = 0): WideString; // Возвращает указанное значащее слово из строки с учётом кавычек (парсинг команд)
function ExtractLinkFromUrl(URL: WideString): WideString; // При необходимости преобразует адрес публичной ссылки к нужному виду
function IsWriteable(const DirName: WideString; FileName: WideString = 'delete.me'; CleanFile: boolean = true): boolean;

implementation

function Implode(S: TStringList; Delimiter: WideString): WideString;
var
	iCount: Integer;
begin
	Result := '';
	if (S.Count = 0) then exit;
	for iCount := 0 to pred(S.Count) do Result := Result + S.Strings[iCount] + Delimiter;
	System.Delete(Result, Length(Result), 1);
end;

function Explode(S: WideString; Delimiter: char): TStringList;
begin
	Result := TStringList.Create;
	Result.DelimitedText := S;
	Result.Delimiter := Delimiter;
end;

function ExtractRealPath(VirtualPath: WideString): TRealPath;
var
	List: TStringList;
begin
	List := TStringList.Create;
	ExtractStrings(['\'], [], PWideChar(VirtualPath), List);
	if List.Count < 2 then
	begin // в виртуальной ФС это каталог первого уровня
		Result.account := '';
		Result.path := '';
	end else begin
		Result.account := List.Strings[0];
		List.Delete(0);

		Result.path := Implode(List, '\');
		if Result.path = '' then ExtractRealPath.path := '\';
	end;

	List.Destroy;
end;

function DateTimeToUnix(ConvDate: TDateTime): Integer;
const
	UnixStartDate: TDateTime = 25569.0;
begin
	// example: DateTimeToUnix(now);
	Result := Round((ConvDate - UnixStartDate) * 86400);
end;

function CheckFlag(Check: Byte; Flags: LongInt): boolean; // Определяет, установлен ли указанный бит
begin
	Result := (Flags and Check) <> 0;
end;

function SizeOfFile(const FileName: String): Int64;
var
	fHandle: DWORD;
begin
	fHandle := CreateFile(PChar(FileName), 0, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if fHandle = INVALID_HANDLE_VALUE then Result := -1
	else
		try
			Int64Rec(Result).Lo := GetFileSize(fHandle, @Int64Rec(Result).Hi);
		finally
			CloseHandle(fHandle);
		end;
end;

function DateTimeToFileTime(FileTime: TDateTime): TFileTime;
var
	LocalFileTime, Ft: TFileTime;
	SystemTime: TSystemTime;
begin
	Result.dwLowDateTime := 0;
	Result.dwHighDateTime := 0;
	DateTimeToSystemTime(FileTime, SystemTime);
	SystemTimeToFileTime(SystemTime, LocalFileTime);
	LocalFileTimeToFileTime(LocalFileTime, Ft);
	Result := Ft;
end;

procedure SetAllFileTime(const FileName: string; const FileTime: TFileTime);
var
	Handle: thandle;
begin
	Handle := CreateFile(PChar(FileName), FILE_WRITE_ATTRIBUTES, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	if Handle = INVALID_HANDLE_VALUE then RaiseLastOSError;
	try
		if not SetFileTime(Handle, @FileTime, @FileTime, @FileTime) then RaiseLastOSError;
	finally
		CloseHandle(Handle);
	end;
end;



procedure CenterWindow(WindowToStay, WindowToCenter: HWND);
var
	R1: TRect;
	R2: TRect;
	Monitor: HMonitor;
	MonInfo: TMonitorInfo;
	MonRect: TRect;
	x: Integer;
	Y: Integer;
begin
	GetWindowRect(WindowToStay, R1);
	GetWindowRect(WindowToCenter, R2);
	Monitor := MonitorFromWindow(WindowToStay, MONITOR_DEFAULTTONEAREST);
	MonInfo.cbSize := SizeOf(MonInfo);
	GetMonitorInfo(Monitor, @MonInfo);
	MonRect := MonInfo.rcWork;
	with R1 do
	begin
		x := (Right - Left - R2.Right + R2.Left) div 2 + Left;
		Y := (Bottom - Top - R2.Bottom + R2.Top) div 2 + Top;
	end;
	x := Max(MonRect.Left, Min(x, MonRect.Right - R2.Right + R2.Left));
	Y := Max(MonRect.Top, Min(Y, MonRect.Bottom - R2.Bottom + R2.Top));
	SetWindowPos(WindowToCenter, 0, x, Y, 0, 0, SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOSIZE or SWP_NOZORDER);
end;

function UrlEncode(URL: WideString): WideString;
var
	I: Integer;
	UTF8: UTF8String;
begin
	UTF8 := UTF8String(URL);
	Result := '';
	for I := 1 to Length(UTF8) do
		if UTF8[I] in ['a' .. 'z', 'A' .. 'Z', '/', '_', '-', '.', '0' .. '9'] then Result := Result + WideString(UTF8[I])
		else Result := Result + '%' + IntToHex(Ord(UTF8[I]), 2);
end;

function FindTCWindow: HWND;
begin
	Result := FindWindow('TTOTAL_CMD', nil); { Хендл отдаётся корректно даже при нескольких запущенных тоталах }
end;

function GetTmpDir: WideString;
var
	tempFolder: array [0 .. MAX_UNC_PATH] of WideChar;
begin
	GetTempPathW(MAX_PATH, @tempFolder);
	Result := IncludeTrailingBackslash(StrPas(tempFolder));
end;

function GetTmpFileName(Prefix: WideString = ''): WideString;
var
	tempFile: array [0 .. MAX_UNC_PATH] of WideChar;
begin
	GetTempFileNameW(PWideChar(GetTmpDir), PWideChar(Prefix), 0, tempFile);
	Result := StrPas(tempFile);
end;

function CopyExt(FromFilename, ToFilename: WideString): WideString;
begin
	Result := ChangeFileExt(ToFilename, ExtractFileExt(FromFilename));
end;

function GetUNCFilePath(FilePath: WideString): WideString;
begin
	Result := '\\?\' + FilePath;
end;

function GetWord(command: WideString; WordIndex: Integer = 0): WideString;
var
	Exploded: TStringList;
begin
	Result := '';
	Exploded := Explode(command, ' ');
	if Exploded.Count = 0 then exit;
	if Exploded.Count <= WordIndex then exit;
	Result := Exploded.Strings[WordIndex];
end;

function ExtractLinkFromUrl(URL: WideString): WideString; // При необходимости преобразует адрес публичной ссылки к нужному виду
const
	pulicPrefix = 'https://cloud.mail.ru/public';
begin
	Result := URL;
	if pos(WideString(pulicPrefix), URL) <> 0 then Result := Copy(URL, Length(pulicPrefix) + 1, Length(URL) - Length(pulicPrefix));
end;

function IsWriteable(const DirName: WideString; FileName: WideString = 'delete.me'; CleanFile: boolean = true): boolean;
var
	NewName: WideString;
	H: thandle;
begin
	NewName := IncludeTrailingPathDelimiter(DirName) + FileName;
	if CleanFile then
	begin
		H := CreateFile(PChar(NewName), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
	end else begin //
		H := CreateFile(PChar(NewName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING or CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
	end;
	Result := H <> INVALID_HANDLE_VALUE;
	if Result then CloseHandle(H);
end;

end.
