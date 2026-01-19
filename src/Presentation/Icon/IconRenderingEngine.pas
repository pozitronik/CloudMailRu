unit IconRenderingEngine;

{Renders icons based on icon info from IIconProvider.
 Takes icon info (what to show) and renders the actual icon handle.
 Separates icon loading/composition from WFX plugin orchestration.
 Handles loading from DLL resources, external files, and icon composition.}

interface

uses
	Windows,
	IconProvider;

type
	{Result of icon rendering operation}
	TIconRenderResult = record
		ResultCode: Integer;        {FS_ICON_EXTRACTED, FS_ICON_USEDEFAULT, FS_ICON_EXTRACTED_DESTROY}
		IconHandle: HICON;          {The loaded/composed icon handle}
		ResourceName: WideString;   {For internal icons - resource name to write to RemoteName buffer}

		class function UseDefault: TIconRenderResult; static;
		class function Extracted(AIcon: HICON): TIconRenderResult; static;
		class function ExtractedDestroy(AIcon: HICON): TIconRenderResult; static;
		class function InternalResource(AIcon: HICON; const AName: WideString): TIconRenderResult; static;
	end;

	{Renders icons based on icon info from IIconProvider.
	 Handles loading from resources, external files, and icon composition.}
	IIconRenderingEngine = interface
		['{B2C3D4E5-F6A7-8901-BCDE-F23456789012}']

		{Renders icon based on icon info.
		 @param IconInfo Icon type and name from IIconProvider
		 @param IconsSize Target icon size in pixels
		 @param PluginPath Plugin directory for external icon files
		 @return Render result with icon handle and result code}
		function Render(const IconInfo: TIconInfo; IconsSize: Integer;
			const PluginPath: WideString): TIconRenderResult;
	end;

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

{ TIconRenderResult }

class function TIconRenderResult.UseDefault: TIconRenderResult;
begin
	Result.ResultCode := FS_ICON_USEDEFAULT;
	Result.IconHandle := 0;
	Result.ResourceName := '';
end;

class function TIconRenderResult.Extracted(AIcon: HICON): TIconRenderResult;
begin
	Result.ResultCode := FS_ICON_EXTRACTED;
	Result.IconHandle := AIcon;
	Result.ResourceName := '';
end;

class function TIconRenderResult.ExtractedDestroy(AIcon: HICON): TIconRenderResult;
begin
	Result.ResultCode := FS_ICON_EXTRACTED_DESTROY;
	Result.IconHandle := AIcon;
	Result.ResourceName := '';
end;

class function TIconRenderResult.InternalResource(AIcon: HICON; const AName: WideString): TIconRenderResult;
begin
	Result.ResultCode := FS_ICON_EXTRACTED;
	Result.IconHandle := AIcon;
	Result.ResourceName := AName;
end;

{ TIconRenderingEngine }

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
