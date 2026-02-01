unit IconHelper;

{Icons manipulation helper methods}
interface

uses
	SysUtils,
	Graphics,
	Windows;

function CombineIcons(FrontIcon, BackIcon: Hicon): Hicon; //taken from http://www.swissdelphicenter.ch/en/showcode.php?id=1636
function LoadPluginIcon(const path: WideString; identifier: WideString): Hicon;

implementation

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
	if not FileExists(FileName) then
		exit;

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

end.
