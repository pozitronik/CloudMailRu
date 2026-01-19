unit IconRenderingEngine;

{Renders icons based on icon info from IIconProvider.
 Handles loading from DLL resources, external files, and icon composition.}

interface

uses
	Windows,
	IIconProviderInterface,
	IIconRenderingEngineInterface;

type
	TIconRenderingEngine = class(TInterfacedObject, IIconRenderingEngine)
	private
		{Converts pixel size to icon size constant for system icon APIs}
		function GetFolderIconSize(PixelSize: Integer): Integer;

		{Renders system trash icon}
		function RenderSystemTrash(IconsSize: Integer): TIconRenderResult;

		{Renders icon from DLL resources}
		function RenderInternal(const IconName: WideString; IconsSize: Integer): TIconRenderResult;

		{Renders icon from DLL resources with folder overlay}
		function RenderInternalOverlay(const IconName: WideString; IconsSize: Integer): TIconRenderResult;

		{Renders icon from external .ico file}
		function RenderExternal(const IconName, PluginPath: WideString): TIconRenderResult;

		{Renders icon from external .ico file with folder overlay}
		function RenderExternalOverlay(const IconName, PluginPath: WideString; IconsSize: Integer): TIconRenderResult;
	public
		function Render(const IconInfo: TIconInfo; IconsSize: Integer;
			const PluginPath: WideString): TIconRenderResult;
	end;

implementation

uses
	SysUtils,
	PLUGIN_TYPES,
	WindowsHelper,
	IconHelper;

function TIconRenderingEngine.GetFolderIconSize(PixelSize: Integer): Integer;
begin
	if PixelSize <= 16 then
		Exit(IconSizeSmall);
	if PixelSize <= 32 then
		Exit(IconSizeNormal);
	Exit(IconSizeLarge);
end;

function TIconRenderingEngine.RenderSystemTrash(IconsSize: Integer): TIconRenderResult;
begin
	Result := TIconRenderResult.ExtractedDestroy(
		GetSystemIcon(GetFolderIconSize(IconsSize)));
end;

function TIconRenderingEngine.RenderInternal(const IconName: WideString; IconsSize: Integer): TIconRenderResult;
var
	IconHandle: HICON;
begin
	IconHandle := LoadImageW(hInstance, PWideChar(IconName), IMAGE_ICON,
		IconsSize, IconsSize, LR_DEFAULTCOLOR);
	Result := TIconRenderResult.InternalResource(IconHandle, IconName);
end;

function TIconRenderingEngine.RenderInternalOverlay(const IconName: WideString; IconsSize: Integer): TIconRenderResult;
var
	FrontIcon, BackIcon, CombinedIcon: HICON;
begin
	FrontIcon := LoadImageW(hInstance, PWideChar(IconName), IMAGE_ICON,
		IconsSize, IconsSize, LR_DEFAULTCOLOR);
	BackIcon := GetFolderIcon(GetFolderIconSize(IconsSize));
	CombinedIcon := CombineIcons(FrontIcon, BackIcon);
	DeleteObject(FrontIcon);
	DeleteObject(BackIcon);
	Result := TIconRenderResult.ExtractedDestroy(CombinedIcon);
end;

function TIconRenderingEngine.RenderExternal(const IconName, PluginPath: WideString): TIconRenderResult;
var
	IconHandle: HICON;
begin
	IconHandle := LoadPluginIcon(PluginPath + 'icons', IconName);
	if IconHandle = INVALID_HANDLE_VALUE then
		Exit(TIconRenderResult.UseDefault);
	Result := TIconRenderResult.ExtractedDestroy(IconHandle);
end;

function TIconRenderingEngine.RenderExternalOverlay(const IconName, PluginPath: WideString; IconsSize: Integer): TIconRenderResult;
var
	FrontIcon, BackIcon, CombinedIcon: HICON;
begin
	FrontIcon := LoadPluginIcon(PluginPath + 'icons', IconName);
	if FrontIcon = INVALID_HANDLE_VALUE then
		Exit(TIconRenderResult.UseDefault);
	BackIcon := GetFolderIcon(GetFolderIconSize(IconsSize));
	CombinedIcon := CombineIcons(FrontIcon, BackIcon);
	DeleteObject(BackIcon);
	Result := TIconRenderResult.ExtractedDestroy(CombinedIcon);
end;

function TIconRenderingEngine.Render(const IconInfo: TIconInfo; IconsSize: Integer;
	const PluginPath: WideString): TIconRenderResult;
begin
	case IconInfo.IconType of
		itUseDefault:
			Result := TIconRenderResult.UseDefault;
		itSystemTrash:
			Result := RenderSystemTrash(IconsSize);
		itInternal:
			Result := RenderInternal(IconInfo.IconName, IconsSize);
		itInternalOverlay:
			Result := RenderInternalOverlay(IconInfo.IconName, IconsSize);
		itExternal:
			Result := RenderExternal(IconInfo.IconName, PluginPath);
		itExternalOverlay:
			Result := RenderExternalOverlay(IconInfo.IconName, PluginPath, IconsSize);
	else
		Result := TIconRenderResult.UseDefault;
	end;
end;

end.
