unit IconHelper;

{Icons manipulation helper methods}
interface

uses
	SysUtils,
	Graphics,
	Windows,
	ShlObj,
	ShellApi,
	ActiveX;

const
	IconSizeSmall = 0; //SHGFI_SMALLICON
	IconSizeNormal = 1; //SHGFI_ICON
	IconSizeLarge = 2; //SHGFI_LARGEICON

function CombineIcons(FrontIcon, BackIcon: Hicon): Hicon; //taken from http://www.swissdelphicenter.ch/en/showcode.php?id=1636
function LoadPluginIcon(const path: WideString; identifier: WideString): Hicon;
function GetFolderIcon(const size: integer = IconSizeSmall): Hicon;
function GetSystemIcon(ParentWindow: HWND; const size: integer = IconSizeSmall; ItemType: integer = CSIDL_BITBUCKET): Hicon;

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

function GetFolderIcon(const size: integer = IconSizeSmall): Hicon;
var
	SFI: TSHFileInfo;
	uFlags: uint;
begin
	Result := INVALID_HANDLE_VALUE;
	uFlags := SHGFI_ICON;
	FillChar(SFI, SizeOf(SFI), 0);
	case size of
		IconSizeSmall:
			uFlags := SHGFI_ICON or SHGFI_SMALLICON;
		IconSizeNormal:
			uFlags := SHGFI_ICON;
		IconSizeLarge:
			uFlags := SHGFI_ICON or SHGFI_LARGEICON; //not working with SHGetFileInfo
	end;

	if SHGetFileInfo('booya', FILE_ATTRIBUTE_DIRECTORY, SFI, SizeOf(SFI), uFlags or SHGFI_USEFILEATTRIBUTES) <> 0 then
		Result := SFI.Hicon;

end;

function GetSystemIcon(ParentWindow: HWND; const size: integer = IconSizeSmall; ItemType: integer = CSIDL_BITBUCKET): Hicon;
var
	SFI: TSHFileInfo;
	PIDL: PItemIDList;
	uFlags: uint;
begin
	Result := INVALID_HANDLE_VALUE;
	uFlags := SHGFI_ICON;
	case size of
		IconSizeSmall:
			uFlags := SHGFI_ICON or SHGFI_SMALLICON;
		IconSizeNormal:
			uFlags := SHGFI_ICON;
		IconSizeLarge:
			uFlags := SHGFI_ICON or SHGFI_LARGEICON; //not working with SHGetFileInfo
	end;
	SHGetSpecialFolderLocation(ParentWindow, ItemType, PIDL);
	try
		if SHGetFileInfo(PChar(PIDL), 0, SFI, SizeOf(SFI), SHGFI_PIDL or SHGFI_SYSICONINDEX or uFlags) <> 0 then
			Result := SFI.Hicon;
	finally
		CoTaskMemFree(PIDL);
	end;
end;

end.
