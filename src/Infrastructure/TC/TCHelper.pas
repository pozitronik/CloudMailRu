unit TCHelper;

{Total Commander related methods}
interface

uses
	Inifiles,
	SysUtils,
	MultiMon,
	Description,
	Windows;

const
	{Total Commander panel refresh message constants}
	TC_REFRESH_MESSAGE = 51; {WM_USER + TC_REFRESH_MESSAGE to refresh panel}
	TC_REFRESH_PARAM = 540; {wParam for panel refresh message}

function FindTCWindow: HWND;
function FindTCIniPath: WideString;
function GetTCIconsSize: integer;
function GetTCCommentPreferredFormat: integer;

implementation

function FindTCWindow: HWND;
begin
	Result := FindWindow('TTOTAL_CMD', nil); {Хендл отдаётся корректно даже при нескольких запущенных тоталах}
end;

function FindTCIniPath: WideString;
begin
	exit(GetEnvironmentVariable('COMMANDER_INI'));
end;

function GetTCIconsSize: integer;
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
		try
			ResolutionSpecific := TC_INI.ReadBool('Configuration', 'ResolutionSpecific', true);
			if ResolutionSpecific then
			begin
				MonInfo.cbSize := SizeOf(MonInfo);
				GetMonitorInfo(MonitorFromWindow(FindTCWindow, MONITOR_DEFAULTTONEAREST), @MonInfo);
				IconsSizeSectionName := Format('%dx%d', [MonInfo.rcMonitor.Right - MonInfo.rcMonitor.Left, MonInfo.rcMonitor.Bottom - MonInfo.rcMonitor.Top]) + ' (8x16)'; //normal font section
				if not TC_INI.SectionExists(IconsSizeSectionName) then
				begin
					IconsSizeSectionName := Format('%dx%d', [MonInfo.rcMonitor.Right - MonInfo.rcMonitor.Left, MonInfo.rcMonitor.Bottom - MonInfo.rcMonitor.Top]) + ' (10x20)'; //large font section
					if not TC_INI.SectionExists(IconsSizeSectionName) then
						IconsSizeSectionName := 'AllResolutions'; //fuck that shit
				end;
			end
			else
				IconsSizeSectionName := 'AllResolutions';

			Result := TC_INI.ReadInteger(IconsSizeSectionName, 'Iconsize32', Result);
		finally
			TC_INI.Free;
		end;
	end;
end;

function GetTCCommentPreferredFormat: integer;
var
	TC_INI: TIniFile;
begin
	Result := ENCODING_UTF8; //UTF-8
	if FileExists(FindTCIniPath) then
	begin
		TC_INI := TIniFile.Create(FindTCIniPath);
		try
			Result := TC_INI.ReadInteger('Configuration', 'CommentPreferredFormat', Result);
		finally
			TC_INI.Free;
		end;
	end;
	if not(Result in [ENCODING_DEFAULT, ENCODING_UNICODE, ENCODING_UNCODE_BE, ENCODING_UTF8]) then
		Result := ENCODING_UTF8; //ignore "combined" TC encodings
end;

end.
