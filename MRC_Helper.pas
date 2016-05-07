unit MRC_Helper;

interface

uses Classes, Windows, SysUtils, IniFiles, MultiMon, Math;

type
	TRealPath = record
		account: WideString;
		path: WideString;
	end;

	TAccountSettings = record
		name, email, password: WideString;
		use_tc_password_manager: boolean;
		user, domain: WideString; // parsed values from email
	end;

function Implode(S: TStringList; Delimiter: Char): WideString;
function ExtractRealPath(VirtualPath: WideString): TRealPath;
function SizeOfFile(const FileName: String): Int64;
function DateTimeToUnix(ConvDate: TDateTime): Integer;
function CheckFlag(Check: Byte; Flags: Integer): boolean; // Определяет, установлен ли указанный бит
function DateTimeToFileTime(FileTime: TDateTime): TFileTime;
function GetAccountSettingsFromIniFile(IniFilePath: WideString; AccountName: WideString): TAccountSettings;
function SetAccountSettingsToIniFile(IniFilePath: WideString; AccountSettings: TAccountSettings): boolean;
procedure GetAccountsListFromIniFile(IniFilePath: WideString; var AccountsList: TStringList);
procedure DeleteAccountFromIniFile(IniFilePath: WideString; AccountName: WideString);
procedure CenterWindow(WindowToStay, WindowToCenter: HWND);
function UrlEncode(URL: UTF8String): WideString; { TODO : Временная реализация! }

implementation

function Implode(S: TStringList; Delimiter: Char): WideString;
var
	iCount: Integer;
begin
	Result := '';
	if (S.Count = 0) then exit;
	for iCount := 0 to pred(S.Count) do Result := Result + S.Strings[iCount] + Delimiter;
	System.Delete(Result, Length(Result), 1);
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

function GetAccountSettingsFromIniFile(IniFilePath: WideString; AccountName: WideString): TAccountSettings;
var
	IniFile: TIniFile;
	AtPos: Integer;
begin
	IniFile := TIniFile.Create(IniFilePath);
	Result.name := AccountName;
	Result.email := IniFile.ReadString(Result.name, 'email', '');
	Result.password := IniFile.ReadString(Result.name, 'password', '');
	Result.use_tc_password_manager := IniFile.ReadBool(Result.name, 'tc_pwd_mngr', false);
	AtPos := AnsiPos('@', Result.email);
	if AtPos <> 0 then
	begin
		Result.user := Copy(Result.email, 0, AtPos - 1);
		Result.domain := Copy(Result.email, AtPos + 1, Length(Result.email) - Length(Result.user) + 1);
	end;
	IniFile.Destroy;
end;

function SetAccountSettingsToIniFile(IniFilePath: WideString; AccountSettings: TAccountSettings): boolean;
var
	IniFile: TIniFile;
begin
	Result := false;
	if AccountSettings.name <> '' then Result := true;
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.WriteString(AccountSettings.name, 'email', AccountSettings.email);
	IniFile.WriteString(AccountSettings.name, 'password', AccountSettings.password);
	IniFile.WriteBool(AccountSettings.name, 'tc_pwd_mngr', AccountSettings.use_tc_password_manager);
	IniFile.Destroy;
end;

procedure GetAccountsListFromIniFile(IniFilePath: WideString; var AccountsList: TStringList);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.ReadSections(AccountsList);
	IniFile.Destroy;
end;

procedure DeleteAccountFromIniFile(IniFilePath: WideString; AccountName: WideString);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.EraseSection(AccountName);
	IniFile.Destroy;
end;

procedure CenterWindow(WindowToStay, WindowToCenter: HWND);
var
	R1: TRect;
	R2: TRect;
	Monitor: HMonitor;
	MonInfo: TMonitorInfo;
	MonRect: TRect;
	X: Integer;
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
		X := (Right - Left - R2.Right + R2.Left) div 2 + Left;
		Y := (Bottom - Top - R2.Bottom + R2.Top) div 2 + Top;
	end;
	X := Max(MonRect.Left, Min(X, MonRect.Right - R2.Right + R2.Left));
	Y := Max(MonRect.Top, Min(Y, MonRect.Bottom - R2.Bottom + R2.Top));
	SetWindowPos(WindowToCenter, 0, X, Y, 0, 0, SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOSIZE or SWP_NOZORDER);
end;

function UrlEncode(URL: UTF8String): WideString; // todo нужно добиться корректного формирования урлов
var
	I: integer;
begin
	Result := '';
	for I := 1 to Length(URL) do
		if URL[I] in ['a' .. 'z', 'A' .. 'Z', '/', '_', '-', '.','0'..'9'] then Result := Result + URL[I]
		else Result := Result + '%' + IntToHex(Ord(URL[I]), 2);
end;
end.
