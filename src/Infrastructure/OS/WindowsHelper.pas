unit WindowsHelper;

{Windows & Shell & related stuff helper methods}
interface

uses
	SysUtils,
	Windows,
	Math,
	MultiMon;

procedure CenterWindow(WindowToStay, WindowToCenter: HWND);
function GetFindDataEmptyDir(DirName: WideString = '.'): tWIN32FINDDATAW;

implementation

procedure CenterWindow(WindowToStay, WindowToCenter: HWND);
var
	R1: TRect;
	R2: TRect;
	Monitor: HMonitor;
	MonInfo: TMonitorInfo;
	MonRect: TRect;
	x: integer;
	Y: integer;
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

function GetFindDataEmptyDir(DirName: WideString = '.'): tWIN32FINDDATAW;
begin
	FillChar(Result, SizeOf(WIN32_FIND_DATA), 0);
	strpcopy(Result.cFileName, DirName);
	Result.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
end;

end.
