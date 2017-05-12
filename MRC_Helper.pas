unit MRC_Helper;

interface

uses Classes, Windows, SysUtils, MultiMon, Math, ShellApi, ShlObj, Vcl.Graphics, Inifiles;

const
	MAX_UNC_PATH = 32767;
	//FsFindFirst* success return codes (INVALID_HANDLE_VALUE returns on error)
	FIND_NO_MORE_FILES = 0;
	FIND_OK = 1;
	FIND_ROOT_DIRECTORY = 2;
	FIND_SHARED_LINKS = 3; //.shared folder

	IconSizeSmall = 0; //SHGFI_SMALLICON
	IconSizeNormal = 1; //SHGFI_ICON
	IconSizeLarge = 2; //SHGFI_LARGEICON

	TrashPostfix = '.trash';
	SharedPostfix = '.shared';
	InvitesPostfix = '.invites';

	TYPE_BYTES = 0;
	TYPE_KYLOBYTES = 1;
	TYPE_MEGABYTES = 2;

type
	TRealPath = record
		account: WideString;
		path: WideString;
		upDirItem: boolean; //path/../
		trashDir: boolean; //item is inside trash bin dir
		sharedDir: boolean; //item is inside shared links dir
		invitesDir: boolean; //item is inside invites dir
	end;

function Implode(S: TStringList; Delimiter: WideString): WideString;
function Explode(S: WideString; Delimiter: char): TStringList;
function ExtractRealPath(VirtualPath: WideString): TRealPath;
function inAccount(path: TRealPath; ignoreVirtual: boolean = true): boolean;
function SizeOfFile(const FileName: String): Int64;
function DateTimeToUnix(ConvDate: TDateTime): Integer;
function CheckFlag(Check: Byte; Flags: Integer): boolean; //Определяет, установлен ли указанный бит
function DateTimeToFileTime(FileTime: TDateTime): TFileTime;
procedure SetAllFileTime(const FileName: string; const FileTime: TFileTime);
procedure CenterWindow(WindowToStay, WindowToCenter: HWND);
function UrlEncode(URL: WideString): WideString;
function FindTCWindow: HWND;
function FindTCIniPath: WideString;
function GetTCIconsSize: Integer;
function GetTmpDir: WideString;
function GetTmpFileName(Prefix: WideString = ''): WideString;
function CopyExt(FromFilename, ToFilename: WideString): WideString;
function GetUNCFilePath(FilePath: WideString): WideString;
function GetWord(command: WideString; WordIndex: Integer = 0): WideString; //Возвращает указанное значащее слово из строки с учётом кавычек (парсинг команд)
function ExtractLinkFromUrl(URL: WideString): WideString; //При необходимости преобразует адрес публичной ссылки к нужному виду
function IsWriteable(const DirName: WideString; FileName: WideString = 'delete.me'; CleanFile: boolean = true): boolean;
function PosLast(Substring, S: WideString; Offset: Integer = 0): Integer;
function PathToUrl(path: WideString; RestrictEmptyUrl: boolean = true; DoUrlEncode: boolean = true): WideString;
function UrlToPath(URL: WideString): WideString;
function GetFolderIcon(const size: Integer = IconSizeSmall): Hicon;
function GetSystemIcon(const size: Integer = IconSizeSmall; ItemType: Integer = CSIDL_BITBUCKET): Hicon;
function CombineIcons(FrontIcon, BackIcon: Hicon): Hicon; //taken from http://www.swissdelphicenter.ch/en/showcode.php?id=1636
function LoadIcon(const FileName: WideString): Hicon;
function LoadPluginIcon(const path: WideString; identifier: WideString): Hicon;
function RetryAttemptsToString(Attempt: Integer): WideString;
procedure ProcessMessages;
function IncludeSlash(const Str: WideString): WideString;
function FormatSize(size: Int64; SizeType: Integer = TYPE_MEGABYTES): WideString; //Форматируем размер в удобочитаемый вид
//Procedure FileLog(S: WideString);

implementation
{
Procedure FileLog(S: WideString);
var
	f: textfile;
begin
	Assign(f, 'd:\WORK\CODE\CloudMailRu\log.txt');
	Rewrite(f);
	Write(f, S);
	close(f);
end;}

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
	Result.account := '';
	Result.path := '';
	Result.upDirItem := false;
	Result.trashDir := false;
	Result.sharedDir := false;
	Result.invitesDir := false;

	if VirtualPath = '' then exit; //root
	VirtualPath := Copy(VirtualPath, 2, Length(VirtualPath) - 1);

	List := TStringList.Create;
	ExtractStrings(['\'], [], PWideChar(VirtualPath), List);

	if (List.Count > 0) and (List.Strings[List.Count - 1] = '..') then Result.upDirItem := true;

	if List.Count = 1 then
	begin
		Result.account := List.Strings[0];
	end else if (List.Count > 1) then
	begin
		Result.account := List.Strings[0];
		List.Delete(0);
		Result.path := Implode(List, '\');
	end;
	List.Destroy;

	if ExtractFileExt(Result.account) = TrashPostfix then
	begin
		Result.trashDir := true;
		Result.account := Copy(Result.account, 1, Length(Result.account) - Length(TrashPostfix));
	end else if ExtractFileExt(Result.account) = SharedPostfix then
	begin
		Result.sharedDir := true;
		Result.account := Copy(Result.account, 1, Length(Result.account) - Length(SharedPostfix));
	end else if ExtractFileExt(Result.account) = InvitesPostfix then
	begin
		Result.invitesDir := true;
		Result.account := Copy(Result.account, 1, Length(Result.account) - Length(InvitesPostfix));
	end;
end;

//проверка, находится ли путь внутри аккаунта. ignoreVirtual - не считать виртуальные каталоги облачными
function inAccount(path: TRealPath; ignoreVirtual: boolean = true): boolean;
begin
	Result := path.account <> '';
	if Result and ignoreVirtual then Result := not(path.trashDir or path.sharedDir or path.invitesDir);
end;

function DateTimeToUnix(ConvDate: TDateTime): Integer;
const
	UnixStartDate: TDateTime = 25569.0;
begin
	Result := Round((ConvDate - UnixStartDate) * 86400);
end;

function CheckFlag(Check: Byte; Flags: LongInt): boolean; //Определяет, установлен ли указанный бит
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
	Handle := INVALID_HANDLE_VALUE;
	try
		Handle := CreateFileW(PWideChar(GetUNCFilePath(FileName)), FILE_WRITE_ATTRIBUTES, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
		if Handle = INVALID_HANDLE_VALUE then
		begin
			CloseHandle(Handle);
			exit;
		end;

		SetFileTime(Handle, @FileTime, @FileTime, @FileTime);
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
	Result := FindWindow('TTOTAL_CMD', nil); {Хендл отдаётся корректно даже при нескольких запущенных тоталах}
end;

function FindTCIniPath: WideString;
begin
	exit(GetEnvironmentVariable('COMMANDER_INI'));
end;

function GetTCIconsSize: Integer;
var
	TC_INI: TIniFile;
	ResolutionSpecific: boolean;
	IconsSizeSectionName: WideString;
	MonInfo: TMonitorInfo;
begin
	Result := 16; //some default value
	if FileExists(FindTCIniPath) then
	begin
		TC_INI := TIniFile.Create(FindTCIniPath);
		ResolutionSpecific := TC_INI.ReadBool('Configuration', 'ResolutionSpecific', true);
		if ResolutionSpecific then
		begin
			MonInfo.cbSize := SizeOf(MonInfo);
			GetMonitorInfo(MonitorFromWindow(FindTCWindow, MONITOR_DEFAULTTONEAREST), @MonInfo);
			IconsSizeSectionName := Format('%dx%d', [MonInfo.rcMonitor.Right - MonInfo.rcMonitor.Left, MonInfo.rcMonitor.Bottom - MonInfo.rcMonitor.Top]) + ' (8x16)'; //normal font section
			if not TC_INI.SectionExists(IconsSizeSectionName) then
			begin
				IconsSizeSectionName := Format('%dx%d', [MonInfo.rcMonitor.Right - MonInfo.rcMonitor.Left, MonInfo.rcMonitor.Bottom - MonInfo.rcMonitor.Top]) + ' (10x20)'; //large font section
				if not TC_INI.SectionExists(IconsSizeSectionName) then IconsSizeSectionName := 'AllResolutions'; //fuck that shit
			end;
		end
		else IconsSizeSectionName := 'AllResolutions';

		Result := TC_INI.ReadInteger(IconsSizeSectionName, 'Iconsize32', Result);

	end;

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
	Result := ExpandUNCFileName(FilePath);
	if not(Pos(WideString('\\'), Result) = 1) then Result := '\\?\' + FilePath;
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

function ExtractLinkFromUrl(URL: WideString): WideString; //При необходимости преобразует адрес публичной ссылки к нужному виду
const
	pulicPrefix = 'https://cloud.mail.ru/public';
begin
	Result := URL;
	if Pos(WideString(pulicPrefix), URL) <> 0 then Result := Copy(URL, Length(pulicPrefix) + 1, Length(URL) - Length(pulicPrefix));
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

function PosLast(Substring, S: WideString; Offset: Integer = 0): Integer;
var
	tmp: Integer;
begin
	tmp := Offset;
	Repeat
		Result := tmp;
		tmp := Pos(Substring, S, tmp + 1);
	until tmp = 0;
end;

function PathToUrl(path: WideString; RestrictEmptyUrl: boolean = true; DoUrlEncode: boolean = true): WideString;
begin

	Result := StringReplace(path, WideString('\'), WideString('/'), [rfReplaceAll, rfIgnoreCase]);
	if DoUrlEncode then Result := UrlEncode(Result);

	if (Result = '') and RestrictEmptyUrl then Result := '/';
end;

function UrlToPath(URL: WideString): WideString;
begin
	Result := StringReplace(URL, WideString('/'), WideString('\'), [rfReplaceAll, rfIgnoreCase]);
end;

function GetFolderIcon(const size: Integer = IconSizeSmall): Hicon;
var
	SFI: TSHFileInfo;
	uFlags: uint;
begin
	Result := INVALID_HANDLE_VALUE;
	uFlags := SHGFI_ICON;
	FillChar(SFI, SizeOf(SFI), 0);
	case size of
		IconSizeSmall: uFlags := SHGFI_ICON or SHGFI_SMALLICON;
		IconSizeNormal: uFlags := SHGFI_ICON;
		IconSizeLarge: uFlags := SHGFI_ICON or SHGFI_LARGEICON; //not working with SHGetFileInfo
	end;

	if SHGetFileInfo('booya', FILE_ATTRIBUTE_DIRECTORY, SFI, SizeOf(SFI), uFlags or SHGFI_USEFILEATTRIBUTES) <> 0 then Result := SFI.Hicon;

end;

function GetSystemIcon(const size: Integer = IconSizeSmall; ItemType: Integer = CSIDL_BITBUCKET): Hicon;
var
	SFI: TSHFileInfo;
	PIDL: PItemIDList;
	uFlags: uint;
begin
	Result := INVALID_HANDLE_VALUE;
	uFlags := SHGFI_ICON;
	case size of
		IconSizeSmall: uFlags := SHGFI_ICON or SHGFI_SMALLICON;
		IconSizeNormal: uFlags := SHGFI_ICON;
		IconSizeLarge: uFlags := SHGFI_ICON or SHGFI_LARGEICON; //not working with SHGetFileInfo
	end;
	SHGetSpecialFolderLocation(FindTCWindow, ItemType, PIDL);
	if SHGetFileInfo(PChar(PIDL), 0, SFI, SizeOf(SFI), SHGFI_PIDL or SHGFI_SYSICONINDEX or uFlags) <> 0 then Result := SFI.Hicon;
end;

function CombineIcons(FrontIcon, BackIcon: Hicon): Hicon;
var
	WinDC: HDC;
	FrontInfo: TIconInfo;
	FrontDC: HDC;
	FrontSv: HBITMAP;
	BackInfo: TIconInfo;
	BackDC: HDC;
	BackSv: HBITMAP;
	BmpObj: tagBitmap;
begin
	WinDC := GetDC(0);

	GetIconInfo(FrontIcon, FrontInfo);
	FrontDC := CreateCompatibleDC(WinDC);
	FrontSv := SelectObject(FrontDC, FrontInfo.hbmMask);

	GetIconInfo(BackIcon, BackInfo);
	BackDC := CreateCompatibleDC(WinDC);
	BackSv := SelectObject(BackDC, BackInfo.hbmMask);

	GetObject(FrontInfo.hbmMask, SizeOf(BmpObj), @BmpObj);
	BitBlt(BackDC, 0, 0, BmpObj.bmWidth, BmpObj.bmHeight, FrontDC, 0, 0, SRCAND);

	SelectObject(BackDC, BackInfo.hbmColor);
	DrawIconEx(BackDC, 0, 0, FrontIcon, 0, 0, 0, 0, DI_NORMAL);

	Result := CreateIconIndirect(BackInfo);

	SelectObject(FrontDC, FrontSv);
	DeleteDC(FrontDC);
	SelectObject(BackDC, BackSv);
	DeleteDC(BackDC);
	ReleaseDC(0, WinDC);
	DeleteObject(FrontInfo.hbmColor);
	DeleteObject(FrontInfo.hbmMask);
	DeleteObject(BackInfo.hbmColor);
	DeleteObject(BackInfo.hbmMask);
end;

function LoadIcon(const FileName: WideString): Hicon;
var
	Icon: TIcon;
begin
	Icon := nil;
	LoadIcon := INVALID_HANDLE_VALUE;
	if not FileExists(FileName) then exit;

	try
		Icon := TIcon.Create;
		Icon.LoadFromFile(FileName);
		Result := CopyIcon(Icon.Handle);
	finally
		Icon.Free;
	end;

end;

function LoadPluginIcon(const path: WideString; identifier: WideString): Hicon;
begin
	exit(LoadIcon(IncludeTrailingBackslash(path) + identifier + '.ico'));
end;

function RetryAttemptsToString(Attempt: Integer): WideString;
begin
	if Attempt < 0 then exit('')
	else exit(' of ' + Attempt.ToString);
end;

procedure ProcessMessages;
var
	Msg: TMsg;
begin
	while true do
	begin
		if not PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then Break;
		if Msg.Message <> $0012 then
		begin
			TranslateMessage(Msg);
			DispatchMessage(Msg);
		end;
	end;
end;

function IncludeSlash(const Str: WideString): WideString;
begin
	Result := Str;
	if not(Result[High(Result)] = '/') then Result := Result + '/';
end;

function FormatSize(size: Int64; SizeType: Integer = TYPE_MEGABYTES): WideString; //Форматируем размер в удобочитаемый вид
const
	postfixes: array [0 .. 6] of string = ('b', 'kb', 'Mb', 'Gb', 'Tb', 'Pb', 'Eb');
var
	iteration: Integer;
begin
	iteration := 0;
	while size > 1024 do
	begin
		iteration := iteration + 1;
		size := size div 1024;
	end;
	exit(size.ToString() + ' ' + postfixes[iteration + SizeType]);
end;

end.
