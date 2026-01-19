unit IIconRenderingEngineInterface;

{Interface for icon rendering engine.
 Takes icon info (what to show) and renders the actual icon handle.
 Separates icon loading/composition from WFX plugin orchestration.}

interface

uses
	Windows,
	IIconProviderInterface;

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

implementation

uses
	PLUGIN_TYPES;

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

end.
