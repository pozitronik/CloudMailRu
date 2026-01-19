unit IconRenderingEngineTest;

{Unit tests for TIconRenderingEngine.
 Note: Some rendering tests require actual icon resources and are marked as integration tests.}

interface

uses
	DUnitX.TestFramework,
	Windows,
	IIconProviderInterface,
	IIconRenderingEngineInterface,
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
	PLUGIN_TYPES;

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
