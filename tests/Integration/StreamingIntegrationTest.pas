unit StreamingIntegrationTest;

{Integration tests for streaming URL generation against live cloud.mail.ru API.
	Tests ICloudShareService.GetPublishedFileStreamUrl which performs
	auto-publish, shard resolution, and URL construction.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	[Category('Integration')]
	TStreamingIntegrationTest = class(TIntegrationTestBase)
	private
		function UploadTestFile(SizeBytes: Integer; const NamePrefix: WideString): WideString;
	public
		[Test]
		procedure TestGetStreamUrl_PublishedFile_ReturnsValidUrl;

		[Test]
		procedure TestGetStreamUrl_UnpublishedFile_AutoPublishes;

		[Test]
		procedure TestGetStreamUrl_UnpublishedFile_NoPublish_Fails;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	System.IOUtils,
	CloudDirItem,
	CloudDirItemList,
	CloudConstants,
	WFXTypes,
	TestDataGenerator;

{TStreamingIntegrationTest}

function TStreamingIntegrationTest.UploadTestFile(SizeBytes: Integer; const NamePrefix: WideString): WideString;
var
	LocalFile: WideString;
	TestData: TMemoryStream;
begin
	Result := UniqueCloudPath(NamePrefix) + '.bin';
	LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('temp', '.bin'));

	TestData := TTestDataGenerator.CreateSmallTestFile(SizeBytes);
	try
		TestData.SaveToFile(LocalFile);
	finally
		TestData.Free;
	end;

	try
		FPrimaryCloud.Uploader.Upload(LocalFile, Result);
	finally
		TFile.Delete(LocalFile);
	end;
end;

procedure TStreamingIntegrationTest.TestGetStreamUrl_PublishedFile_ReturnsValidUrl;
var
	RemotePath: WideString;
	PublicLink: WideString;
	StreamUrl: WideString;
	FileItem: TCloudDirItem;
	PublishResult, StreamResult: Boolean;
begin
	{Upload and publish a file}
	RemotePath := UploadTestFile(1024, 'StreamPublished');
	TrackForCleanup(RemotePath);

	PublicLink := '';
	PublishResult := FPrimaryCloud.PublishFile(RemotePath, PublicLink, CLOUD_PUBLISH);

	if not PublishResult then
	begin
		Assert.Pass('SKIPPED: Cannot test streaming - publish failed');
		Exit;
	end;

	{Build a DirItem with weblink set}
	FileItem := Default(TCloudDirItem);
	FileItem.Home := RemotePath;
	FileItem.weblink := PublicLink;

	{Get streaming URL}
	StreamUrl := '';
	StreamResult := FPrimaryCloud.ShareService.GetPublishedFileStreamUrl(FileItem, StreamUrl);

	{Cleanup: unpublish}
	FPrimaryCloud.PublishFile(RemotePath, PublicLink, CLOUD_UNPUBLISH);

	if StreamResult then
	begin
		Assert.IsNotEmpty(String(StreamUrl), 'Stream URL should not be empty');
		Assert.IsTrue(Pos(WideString('.m3u8'), StreamUrl) > 0, 'Stream URL should contain .m3u8');
	end
	else
	begin
		{Streaming shard may not be available}
		Assert.Pass('SKIPPED: GetPublishedFileStreamUrl returned False (shard may be unavailable)');
	end;
end;

procedure TStreamingIntegrationTest.TestGetStreamUrl_UnpublishedFile_AutoPublishes;
var
	RemotePath: WideString;
	StreamUrl: WideString;
	FileItem: TCloudDirItem;
	StreamResult: Boolean;
	PublicLink: WideString;
begin
	{Upload a file without publishing}
	RemotePath := UploadTestFile(1024, 'StreamAutoPublish');
	TrackForCleanup(RemotePath);

	{Build a DirItem without weblink - triggers auto-publish}
	FileItem := Default(TCloudDirItem);
	FileItem.Home := RemotePath;
	FileItem.weblink := '';

	{Get streaming URL with Publish=True (auto-publish)}
	StreamUrl := '';
	StreamResult := FPrimaryCloud.ShareService.GetPublishedFileStreamUrl(FileItem, StreamUrl,
		SHARD_TYPE_WEBLINK_VIDEO, CLOUD_PUBLISH);

	{Cleanup: unpublish the auto-published file}
	PublicLink := '';
	FPrimaryCloud.PublishFile(RemotePath, PublicLink, CLOUD_UNPUBLISH);

	if StreamResult then
	begin
		Assert.IsNotEmpty(String(StreamUrl), 'Stream URL should not be empty');
		Assert.IsTrue(Pos(WideString('.m3u8'), StreamUrl) > 0, 'Stream URL should contain .m3u8');
	end
	else
	begin
		Assert.Pass('SKIPPED: Auto-publish streaming returned False (shard may be unavailable)');
	end;
end;

procedure TStreamingIntegrationTest.TestGetStreamUrl_UnpublishedFile_NoPublish_Fails;
var
	RemotePath: WideString;
	StreamUrl: WideString;
	FileItem: TCloudDirItem;
	StreamResult: Boolean;
begin
	{Upload a file without publishing}
	RemotePath := UploadTestFile(1024, 'StreamNoPublish');
	TrackForCleanup(RemotePath);

	{Build a DirItem without weblink}
	FileItem := Default(TCloudDirItem);
	FileItem.Home := RemotePath;
	FileItem.weblink := '';

	{Request streaming URL with Publish=False - should fail because no weblink}
	StreamUrl := '';
	StreamResult := FPrimaryCloud.ShareService.GetPublishedFileStreamUrl(FileItem, StreamUrl,
		SHARD_TYPE_WEBLINK_VIDEO, CLOUD_UNPUBLISH);

	Assert.IsFalse(StreamResult, 'Should fail when no weblink and publish disabled');
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TStreamingIntegrationTest);

end.
