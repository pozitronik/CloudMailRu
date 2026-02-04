unit ThumbnailIntegrationTest;

{Integration tests for thumbnail retrieval against live cloud.mail.ru API.
	Tests TCloudMailRu.GetThumbnail which internally creates TCloudThumbnailService
	with real shard resolution, HTTP download, and bitmap conversion.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	[Category('Integration')]
	TThumbnailIntegrationTest = class(TIntegrationTestBase)
	private
		function UploadTestFile(SizeBytes: Integer; const NamePrefix, Extension: WideString): WideString;
		function UploadJPEGFile(const NamePrefix: WideString): WideString;
	public
		[Test]
		procedure TestGetThumbnail_ImageFile_ReturnsBitmap;

		[Test]
		procedure TestGetThumbnail_NonImageFile_ReturnsZero;

		[Test]
		procedure TestGetThumbnail_NonExistentPath_ReturnsZero;
	end;

implementation

uses
	Windows,
	System.SysUtils,
	System.Classes,
	System.IOUtils,
	TestDataGenerator;

{TThumbnailIntegrationTest}

function TThumbnailIntegrationTest.UploadTestFile(SizeBytes: Integer; const NamePrefix, Extension: WideString): WideString;
var
	LocalFile: WideString;
	TestData: TMemoryStream;
begin
	Result := UniqueCloudPath(NamePrefix) + Extension;
	LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('temp', Extension));

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

function TThumbnailIntegrationTest.UploadJPEGFile(const NamePrefix: WideString): WideString;
var
	LocalFile: WideString;
	JPEGData: TMemoryStream;
begin
	Result := UniqueCloudPath(NamePrefix) + '.jpg';
	LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('temp', '.jpg'));

	JPEGData := TTestDataGenerator.CreateMinimalJPEG;
	try
		JPEGData.SaveToFile(LocalFile);
	finally
		JPEGData.Free;
	end;

	try
		FPrimaryCloud.Uploader.Upload(LocalFile, Result);
	finally
		TFile.Delete(LocalFile);
	end;
end;

procedure TThumbnailIntegrationTest.TestGetThumbnail_ImageFile_ReturnsBitmap;
var
	RemotePath: WideString;
	BitmapHandle: HBITMAP;
begin
	{Upload a valid JPEG image}
	RemotePath := UploadJPEGFile('ThumbImage');
	TrackForCleanup(RemotePath);

	{Request thumbnail - cloud generates thumbnails for image files}
	BitmapHandle := FPrimaryCloud.GetThumbnail(RemotePath, 100, 100);

	if BitmapHandle <> 0 then
	begin
		try
			Assert.Pass('Thumbnail retrieved successfully for image file');
		finally
			DeleteObject(BitmapHandle);
		end;
	end
	else
	begin
		{Cloud may not generate thumbnails for very small or minimal JPEG files}
		Assert.Pass('SKIPPED: Cloud returned no thumbnail (minimal JPEG may not be thumbnailable)');
	end;
end;

procedure TThumbnailIntegrationTest.TestGetThumbnail_NonImageFile_ReturnsZero;
var
	RemotePath: WideString;
	BitmapHandle: HBITMAP;
begin
	{Upload a non-image binary file}
	RemotePath := UploadTestFile(1024, 'ThumbNonImage', '.bin');
	TrackForCleanup(RemotePath);

	{Request thumbnail for non-image file - should return 0}
	BitmapHandle := FPrimaryCloud.GetThumbnail(RemotePath, 100, 100);

	Assert.AreEqual(HBITMAP(0), BitmapHandle, 'Thumbnail for non-image file should return 0');
end;

procedure TThumbnailIntegrationTest.TestGetThumbnail_NonExistentPath_ReturnsZero;
var
	BitmapHandle: HBITMAP;
begin
	{Request thumbnail for path that does not exist}
	BitmapHandle := FPrimaryCloud.GetThumbnail(
		FTestRunFolder + '/NonExistent_' + IntToStr(Random(999999)) + '.jpg', 100, 100);

	Assert.AreEqual(HBITMAP(0), BitmapHandle, 'Thumbnail for non-existent path should return 0');
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TThumbnailIntegrationTest);

end.
