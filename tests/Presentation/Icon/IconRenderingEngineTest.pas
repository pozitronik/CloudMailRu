unit IconRenderingEngineTest;

{Unit tests for TIconRenderingEngine.
 Note: Some rendering tests require actual icon resources and are marked as integration tests.}

interface

uses
	DUnitX.TestFramework,
	Windows,
	IconProvider,
	IconRenderingEngine;

type
	[TestFixture]
	TIconRenderingEngineTest = class
	private
		FEngine: IIconRenderingEngine;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{UseDefault tests}
		[Test]
		procedure TestRender_UseDefault_ReturnsUseDefaultCode;
		[Test]
		procedure TestRender_UseDefault_ReturnsZeroIconHandle;

		{SystemTrash tests}
		[Test]
		procedure TestRender_SystemTrash_ReturnsExtractedDestroyCode;
		[Test]
		procedure TestRender_SystemTrash_ReturnsValidIconHandle;

		{Internal resource tests - require DLL resources}
		[Test]
		procedure TestRender_Internal_ReturnsExtractedCode;
		[Test]
		procedure TestRender_Internal_SetsResourceName;

		{InternalOverlay tests - require DLL resources}
		[Test]
		procedure TestRender_InternalOverlay_ReturnsExtractedDestroyCode;

		{External icon tests - returns default when file not found}
		[Test]
		procedure TestRender_External_MissingFile_ReturnsUseDefault;
		[Test]
		procedure TestRender_ExternalOverlay_MissingFile_ReturnsUseDefault;

		{Size-dependent rendering -- GetFolderIconSize branches}
		[Test]
		{Covers GetFolderIconSize small branch (PixelSize <= 16)}
		procedure TestRender_SystemTrash_SmallSize_ReturnsValidResult;
		[Test]
		{Covers GetFolderIconSize large branch (PixelSize > 32)}
		procedure TestRender_SystemTrash_LargeSize_ReturnsValidResult;

		{External icon with real file}
		[Test]
		{Covers RenderExternal success path (line 143) using a real .ico file}
		procedure TestRender_External_ValidFile_ReturnsExtractedDestroy;
		[Test]
		{Covers RenderExternalOverlay success path (lines 153-156) using a real .ico file}
		procedure TestRender_ExternalOverlay_ValidFile_ReturnsExtractedDestroy;

		{Edge cases}
		[Test]
		procedure TestRender_UnknownIconType_ReturnsUseDefault;

		{TIconRenderResult factory methods}
		[Test]
		procedure TestIconRenderResult_UseDefault_SetsCorrectValues;
		[Test]
		procedure TestIconRenderResult_Extracted_SetsCorrectValues;
		[Test]
		procedure TestIconRenderResult_ExtractedDestroy_SetsCorrectValues;
		[Test]
		procedure TestIconRenderResult_InternalResource_SetsCorrectValues;
	end;

implementation

uses
	SysUtils,
	Graphics,
	IconHelper,
	WFXTypes;

procedure TIconRenderingEngineTest.Setup;
begin
	FEngine := TIconRenderingEngine.Create;
end;

procedure TIconRenderingEngineTest.TearDown;
begin
	FEngine := nil;
end;

{UseDefault tests}

procedure TIconRenderingEngineTest.TestRender_UseDefault_ReturnsUseDefaultCode;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
begin
	IconInfo := TIconInfo.UseDefault;

	RenderResult := FEngine.Render(IconInfo, 32, 'C:\Test\');

	Assert.AreEqual(FS_ICON_USEDEFAULT, RenderResult.ResultCode);
end;

procedure TIconRenderingEngineTest.TestRender_UseDefault_ReturnsZeroIconHandle;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
begin
	IconInfo := TIconInfo.UseDefault;

	RenderResult := FEngine.Render(IconInfo, 32, 'C:\Test\');

	Assert.AreEqual(HICON(0), RenderResult.IconHandle);
end;

{SystemTrash tests}

procedure TIconRenderingEngineTest.TestRender_SystemTrash_ReturnsExtractedDestroyCode;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
begin
	IconInfo := TIconInfo.SystemTrash;

	RenderResult := FEngine.Render(IconInfo, 32, 'C:\Test\');

	Assert.AreEqual(FS_ICON_EXTRACTED_DESTROY, RenderResult.ResultCode);
	{Clean up icon if created}
	if RenderResult.IconHandle <> 0 then
		DeleteObject(RenderResult.IconHandle);
end;

procedure TIconRenderingEngineTest.TestRender_SystemTrash_ReturnsValidIconHandle;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
begin
	IconInfo := TIconInfo.SystemTrash;

	RenderResult := FEngine.Render(IconInfo, 32, 'C:\Test\');

	Assert.IsTrue(RenderResult.IconHandle <> 0, 'Should return valid icon handle');
	{Clean up icon}
	if RenderResult.IconHandle <> 0 then
		DeleteObject(RenderResult.IconHandle);
end;

{Internal resource tests}

procedure TIconRenderingEngineTest.TestRender_Internal_ReturnsExtractedCode;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
begin
	IconInfo := TIconInfo.Create(itInternal, 'cloud_folder');

	RenderResult := FEngine.Render(IconInfo, 32, 'C:\Test\');

	{Internal resources return FS_ICON_EXTRACTED regardless of whether icon was found}
	Assert.AreEqual(FS_ICON_EXTRACTED, RenderResult.ResultCode);
	{Clean up if icon was loaded}
	if RenderResult.IconHandle <> 0 then
		DeleteObject(RenderResult.IconHandle);
end;

procedure TIconRenderingEngineTest.TestRender_Internal_SetsResourceName;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
begin
	IconInfo := TIconInfo.Create(itInternal, 'cloud_folder');

	RenderResult := FEngine.Render(IconInfo, 32, 'C:\Test\');

	Assert.AreEqual('cloud_folder', RenderResult.ResourceName);
	{Clean up if icon was loaded}
	if RenderResult.IconHandle <> 0 then
		DeleteObject(RenderResult.IconHandle);
end;

{InternalOverlay tests}

procedure TIconRenderingEngineTest.TestRender_InternalOverlay_ReturnsExtractedDestroyCode;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
begin
	IconInfo := TIconInfo.Create(itInternalOverlay, 'cloud_shared');

	RenderResult := FEngine.Render(IconInfo, 32, 'C:\Test\');

	{InternalOverlay always returns EXTRACTED_DESTROY as it composes icons}
	Assert.AreEqual(FS_ICON_EXTRACTED_DESTROY, RenderResult.ResultCode);
	{Clean up if icon was loaded}
	if RenderResult.IconHandle <> 0 then
		DeleteObject(RenderResult.IconHandle);
end;

{External icon tests}

procedure TIconRenderingEngineTest.TestRender_External_MissingFile_ReturnsUseDefault;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
begin
	IconInfo := TIconInfo.Create(itExternal, 'nonexistent_icon');

	RenderResult := FEngine.Render(IconInfo, 32, 'C:\NonExistentPath\');

	Assert.AreEqual(FS_ICON_USEDEFAULT, RenderResult.ResultCode);
end;

procedure TIconRenderingEngineTest.TestRender_ExternalOverlay_MissingFile_ReturnsUseDefault;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
begin
	IconInfo := TIconInfo.Create(itExternalOverlay, 'nonexistent_icon');

	RenderResult := FEngine.Render(IconInfo, 32, 'C:\NonExistentPath\');

	Assert.AreEqual(FS_ICON_USEDEFAULT, RenderResult.ResultCode);
end;

{Size-dependent rendering}

procedure TIconRenderingEngineTest.TestRender_SystemTrash_SmallSize_ReturnsValidResult;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
begin
	IconInfo := TIconInfo.SystemTrash;
	{Size 16 hits GetFolderIconSize <= 16 branch -> IconSizeSmall}
	RenderResult := FEngine.Render(IconInfo, 16, 'C:\Test\');
	Assert.AreEqual(FS_ICON_EXTRACTED_DESTROY, RenderResult.ResultCode);
	if RenderResult.IconHandle <> 0 then
		DeleteObject(RenderResult.IconHandle);
end;

procedure TIconRenderingEngineTest.TestRender_SystemTrash_LargeSize_ReturnsValidResult;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
begin
	IconInfo := TIconInfo.SystemTrash;
	{Size 64 hits GetFolderIconSize > 32 branch -> IconSizeLarge}
	RenderResult := FEngine.Render(IconInfo, 64, 'C:\Test\');
	Assert.AreEqual(FS_ICON_EXTRACTED_DESTROY, RenderResult.ResultCode);
	if RenderResult.IconHandle <> 0 then
		DeleteObject(RenderResult.IconHandle);
end;

{External icon with real file}

procedure TIconRenderingEngineTest.TestRender_External_ValidFile_ReturnsExtractedDestroy;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
	TempDir, IconsDir, IconFile: string;
	Icon: TIcon;
	FolderIcon: HICON;
begin
	TempDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
		'IconRenderTest_' + IntToStr(GetCurrentProcessId);
	IconsDir := IncludeTrailingPathDelimiter(TempDir) + 'icons';
	IconFile := IncludeTrailingPathDelimiter(IconsDir) + 'test_ext.ico';
	ForceDirectories(IconsDir);
	try
		{Create a real .ico file}
		FolderIcon := GetFolderIcon(IconSizeSmall);
		Icon := TIcon.Create;
		try
			Icon.Handle := FolderIcon;
			Icon.SaveToFile(IconFile);
		finally
			Icon.Free;
		end;

		IconInfo := TIconInfo.Create(itExternal, 'test_ext');
		{PluginPath must have trailing backslash: production code does PluginPath + 'icons'}
		RenderResult := FEngine.Render(IconInfo, 32, IncludeTrailingPathDelimiter(TempDir));

		Assert.AreEqual(FS_ICON_EXTRACTED_DESTROY, RenderResult.ResultCode);
		if RenderResult.IconHandle <> 0 then
			DestroyIcon(RenderResult.IconHandle);
	finally
		if FileExists(IconFile) then DeleteFile(IconFile);
		RemoveDir(IconsDir);
		RemoveDir(TempDir);
	end;
end;

procedure TIconRenderingEngineTest.TestRender_ExternalOverlay_ValidFile_ReturnsExtractedDestroy;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
	TempDir, IconsDir, IconFile: string;
	Icon: TIcon;
	FolderIcon: HICON;
begin
	TempDir := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
		'IconRenderTest2_' + IntToStr(GetCurrentProcessId);
	IconsDir := IncludeTrailingPathDelimiter(TempDir) + 'icons';
	IconFile := IncludeTrailingPathDelimiter(IconsDir) + 'test_overlay.ico';
	ForceDirectories(IconsDir);
	try
		{Create a real .ico file}
		FolderIcon := GetFolderIcon(IconSizeSmall);
		Icon := TIcon.Create;
		try
			Icon.Handle := FolderIcon;
			Icon.SaveToFile(IconFile);
		finally
			Icon.Free;
		end;

		IconInfo := TIconInfo.Create(itExternalOverlay, 'test_overlay');
		{PluginPath must have trailing backslash}
		RenderResult := FEngine.Render(IconInfo, 32, IncludeTrailingPathDelimiter(TempDir));

		Assert.AreEqual(FS_ICON_EXTRACTED_DESTROY, RenderResult.ResultCode);
		if RenderResult.IconHandle <> 0 then
			DestroyIcon(RenderResult.IconHandle);
	finally
		if FileExists(IconFile) then DeleteFile(IconFile);
		RemoveDir(IconsDir);
		RemoveDir(TempDir);
	end;
end;

{Edge cases}

procedure TIconRenderingEngineTest.TestRender_UnknownIconType_ReturnsUseDefault;
var
	IconInfo: TIconInfo;
	RenderResult: TIconRenderResult;
begin
	{Create icon info with invalid type by direct assignment}
	IconInfo.IconType := TIconType(99);
	IconInfo.IconName := 'test';

	RenderResult := FEngine.Render(IconInfo, 32, 'C:\Test\');

	Assert.AreEqual(FS_ICON_USEDEFAULT, RenderResult.ResultCode);
end;

{TIconRenderResult factory methods}

procedure TIconRenderingEngineTest.TestIconRenderResult_UseDefault_SetsCorrectValues;
var
	Result: TIconRenderResult;
begin
	Result := TIconRenderResult.UseDefault;

	Assert.AreEqual(FS_ICON_USEDEFAULT, Result.ResultCode);
	Assert.AreEqual(HICON(0), Result.IconHandle);
	Assert.AreEqual('', Result.ResourceName);
end;

procedure TIconRenderingEngineTest.TestIconRenderResult_Extracted_SetsCorrectValues;
var
	Result: TIconRenderResult;
	TestHandle: HICON;
begin
	TestHandle := 12345;

	Result := TIconRenderResult.Extracted(TestHandle);

	Assert.AreEqual(FS_ICON_EXTRACTED, Result.ResultCode);
	Assert.AreEqual(TestHandle, Result.IconHandle);
	Assert.AreEqual('', Result.ResourceName);
end;

procedure TIconRenderingEngineTest.TestIconRenderResult_ExtractedDestroy_SetsCorrectValues;
var
	Result: TIconRenderResult;
	TestHandle: HICON;
begin
	TestHandle := 12345;

	Result := TIconRenderResult.ExtractedDestroy(TestHandle);

	Assert.AreEqual(FS_ICON_EXTRACTED_DESTROY, Result.ResultCode);
	Assert.AreEqual(TestHandle, Result.IconHandle);
	Assert.AreEqual('', Result.ResourceName);
end;

procedure TIconRenderingEngineTest.TestIconRenderResult_InternalResource_SetsCorrectValues;
var
	Result: TIconRenderResult;
	TestHandle: HICON;
begin
	TestHandle := 12345;

	Result := TIconRenderResult.InternalResource(TestHandle, 'test_resource');

	Assert.AreEqual(FS_ICON_EXTRACTED, Result.ResultCode);
	Assert.AreEqual(TestHandle, Result.IconHandle);
	Assert.AreEqual('test_resource', Result.ResourceName);
end;

initialization
	TDUnitX.RegisterTestFixture(TIconRenderingEngineTest);

end.
