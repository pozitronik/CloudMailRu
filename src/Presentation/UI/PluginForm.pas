unit PluginForm;

{Base form class for all plugin dialog forms.
	Provides automatic centering relative to the TC parent window.
	Centering is deferred via PostMessage so VCL initialization
	completes before we read the final window size.
	On multi-monitor setups with mixed DPI, the initial centering
	may trigger WM_DPICHANGED when the form crosses a DPI boundary.
	VCL then overrides our position with the Windows suggested rect,
	so we re-center once more after the DPI change is handled.}

interface

uses
	Vcl.Forms,
	Windows,
	Messages,
	Math,
	MultiMon;

type
	TPluginForm = class(TForm)
	private
		const WM_DEFERRED_CENTER = WM_USER + 1;
	private
		FCenterCount: Integer;
		procedure CenterToParent;
		procedure WMDeferredCenter(var Message: TMessage); message WM_DEFERRED_CENTER;
		procedure WMDpiChanged(var Message: TMessage); message $02E0; // WM_DPICHANGED
	protected
		procedure Activate; override;
	end;

implementation

procedure TPluginForm.Activate;
begin
	inherited;
	if FCenterCount = 0 then
		PostMessage(Handle, WM_DEFERRED_CENTER, 0, 0);
end;

procedure TPluginForm.WMDeferredCenter(var Message: TMessage);
begin
	Inc(FCenterCount);
	CenterToParent;
end;

procedure TPluginForm.WMDpiChanged(var Message: TMessage);
begin
	inherited;
	// VCL repositions the form using the Windows suggested rect,
	// overriding our centering. Re-center once after the first
	// DPI change (FCenterCount = 1 means initial centering just happened).
	if FCenterCount = 1 then
		PostMessage(Handle, WM_DEFERRED_CENTER, 0, 0);
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
