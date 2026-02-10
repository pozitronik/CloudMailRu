unit PluginForm;

{Base form class for all plugin dialog forms.
	Provides automatic centering relative to the TC parent window.
	Centering runs on Activate (re-center when form regains focus)
	and on Resize (re-center after VCL DPI scaling changes form size).}

interface

uses
	Vcl.Forms,
	Windows,
	Math,
	MultiMon;

type
	TPluginForm = class(TForm)
	private
		procedure CenterToParent;
	protected
		procedure Activate; override;
		procedure Resize; override;
	end;

implementation

procedure TPluginForm.Activate;
begin
	inherited;
	CenterToParent;
end;

procedure TPluginForm.Resize;
begin
	inherited;
	CenterToParent;
end;

procedure TPluginForm.CenterToParent;
var
	R1: TRect;
	R2: TRect;
	Monitor: HMonitor;
	MonInfo: TMonitorInfo;
	MonRect: TRect;
	X, Y: Integer;
begin
	if ParentWindow = 0 then
		Exit;
	GetWindowRect(ParentWindow, R1);
	GetWindowRect(Handle, R2);
	Monitor := MonitorFromWindow(ParentWindow, MONITOR_DEFAULTTONEAREST);
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
	SetWindowPos(Handle, 0, X, Y, 0, 0, SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOSIZE or SWP_NOZORDER);
end;

end.
