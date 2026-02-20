unit CloudThumbnailServiceTest;

{Unit tests for CloudThumbnailService.
	Tests thumbnail URL construction and download logic.}

interface

uses
	Windows,
	Classes,
	SysUtils,
	System.Generics.Collections,
	DUnitX.TestFramework,
	CloudThumbnailService,
	ThumbnailBitmapConverter,
	CloudHTTP,
	CloudShardManager,
	CloudOAuth,
	CloudConstants,
	Logger,
	Progress,
	IdCookieManager,
	IdHTTP,
	WFXTypes;

type
	{Mock HTTP that tracks URL and returns configurable responses}
	TMockThumbnailHTTP = class(TInterfacedObject, ICloudHTTP)
	private
		FLastGetFileURL: WideString;
		FReturnCode: Integer;
		FReturnData: TBytes;
	public
		constructor Create;

		{Test configuration}
		procedure ConfigureSuccess(const ImageData: TBytes);
		procedure ConfigureFailure(ErrorCode: Integer);
		function GetLastGetFileURL: WideString;

		{ICloudHTTP}
		function GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
		function GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean = True): Integer;
		function PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = True; ProgressEnabled: Boolean = True): Boolean;
		function PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
		procedure SetProgressNames(SourceName, TargetName: WideString);
		procedure SetProgress(Progress: IProgress);
		procedure SetAuthCookie(Value: TIdCookieManager);
		function GetAuthCookie: TIdCookieManager;
		procedure SetCSRFToken(const Token: WideString);
		function GetHTTP: TIdHTTP;
	end;

	{Mock shard manager that returns configurable shard URL}
	TMockThumbnailShardManager = class(TInterfacedObject, ICloudShardManager)
	private
		FThumbnailShard: WideString;
		FResolveSuccess: Boolean;
	public
		constructor Create;

		{Test configuration}
		procedure ConfigureShard(const ShardURL: WideString);
		procedure ConfigureResolveFailure;

		{ICloudShardManager}
		function ResolveShard(var Shard: WideString; ShardType: WideString): Boolean;
		function GetDownloadShard: WideString;
		procedure SetDownloadShard(const Shard: WideString);
		function GetUploadShard: WideString;
		procedure SetUploadShard(const Shard: WideString);
		function GetPublicShard: WideString;
		procedure SetPublicShard(const Shard: WideString);
		function GetDownloadShardOverride: WideString;
		function GetUploadShardOverride: WideString;
		function HasDownloadOverride: Boolean;
		function HasUploadOverride: Boolean;
		procedure InvalidateShard(ShardType: WideString);
		procedure InvalidateAll;
		function EnsureDownloadShard: WideString;
		function EnsureUploadShard: WideString;
	end;

	[TestFixture]
	TCloudThumbnailServiceTest = class
	private
		FMockHTTP: TMockThumbnailHTTP;
		FMockShardManager: TMockThumbnailShardManager;
		FMockLogger: ILogger;
		FTestOAuthToken: TCloudOAuth;
		FService: ICloudThumbnailService;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{URL construction tests}
		[Test]
		procedure TestGetThumbnail_ConstructsCorrectURL;
		[Test]
		procedure TestGetThumbnail_UsesCorrectSizePreset;
		[Test]
		procedure TestGetThumbnail_PathStartsWithSlash;
		[Test]
		procedure TestGetThumbnail_IncludesAuthParameters;

		{Shard resolution tests}
		[Test]
		procedure TestGetThumbnail_UsesFallbackWhenShardResolveFails;

		{Error handling tests}
		[Test]
		procedure TestGetThumbnail_ReturnsZeroOnDownloadFailure;
		[Test]
		procedure TestGetThumbnail_ReturnsZeroOnEmptyResponse;
	end;

implementation

{TMockThumbnailHTTP}

constructor TMockThumbnailHTTP.Create;
begin
	inherited;
	FReturnCode := FS_FILE_OK;
	FLastGetFileURL := '';
end;

procedure TMockThumbnailHTTP.ConfigureSuccess(const ImageData: TBytes);
begin
	FReturnCode := FS_FILE_OK;
	FReturnData := ImageData;
end;

procedure TMockThumbnailHTTP.ConfigureFailure(ErrorCode: Integer);
begin
	FReturnCode := ErrorCode;
	SetLength(FReturnData, 0);
end;

function TMockThumbnailHTTP.GetLastGetFileURL: WideString;
begin
	Result := FLastGetFileURL;
end;

function TMockThumbnailHTTP.GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean): Integer;
begin
	FLastGetFileURL := URL;
	Result := FReturnCode;
	if (Result = FS_FILE_OK) and (Length(FReturnData) > 0) then
		FileStream.WriteBuffer(FReturnData[0], Length(FReturnData));
end;

function TMockThumbnailHTTP.GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
begin Result := False; end;
function TMockThumbnailHTTP.PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString; LogErrors: Boolean; ProgressEnabled: Boolean): Boolean;
begin Result := False; end;
function TMockThumbnailHTTP.PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
begin Result := FS_FILE_NOTSUPPORTED; end;
procedure TMockThumbnailHTTP.SetProgressNames(SourceName, TargetName: WideString);
begin end;
procedure TMockThumbnailHTTP.SetProgress(Progress: IProgress);
begin end;
procedure TMockThumbnailHTTP.SetAuthCookie(Value: TIdCookieManager);
begin end;
function TMockThumbnailHTTP.GetAuthCookie: TIdCookieManager;
begin Result := nil; end;
procedure TMockThumbnailHTTP.SetCSRFToken(const Token: WideString);
begin end;
function TMockThumbnailHTTP.GetHTTP: TIdHTTP;
begin Result := nil; end;

{TMockThumbnailShardManager}

constructor TMockThumbnailShardManager.Create;
begin
	inherited;
	FThumbnailShard := 'https://thumb.test.mail.ru/thumb';
	FResolveSuccess := True;
end;

procedure TMockThumbnailShardManager.ConfigureShard(const ShardURL: WideString);
begin
	FThumbnailShard := ShardURL;
	FResolveSuccess := True;
end;

procedure TMockThumbnailShardManager.ConfigureResolveFailure;
begin
	FResolveSuccess := False;
end;

function TMockThumbnailShardManager.ResolveShard(var Shard: WideString; ShardType: WideString): Boolean;
begin
	if FResolveSuccess and (ShardType = SHARD_TYPE_THUMBNAILS) then
	begin
		Shard := FThumbnailShard;
		Result := True;
	end
	else
		Result := False;
end;

function TMockThumbnailShardManager.GetDownloadShard: WideString;
begin Result := ''; end;
procedure TMockThumbnailShardManager.SetDownloadShard(const Shard: WideString);
begin end;
function TMockThumbnailShardManager.GetUploadShard: WideString;
begin Result := ''; end;
procedure TMockThumbnailShardManager.SetUploadShard(const Shard: WideString);
begin end;
function TMockThumbnailShardManager.GetPublicShard: WideString;
begin Result := ''; end;
procedure TMockThumbnailShardManager.SetPublicShard(const Shard: WideString);
begin end;
function TMockThumbnailShardManager.GetDownloadShardOverride: WideString;
begin Result := ''; end;
function TMockThumbnailShardManager.GetUploadShardOverride: WideString;
begin Result := ''; end;
function TMockThumbnailShardManager.HasDownloadOverride: Boolean;
begin Result := False; end;
function TMockThumbnailShardManager.HasUploadOverride: Boolean;
begin Result := False; end;
procedure TMockThumbnailShardManager.InvalidateShard(ShardType: WideString);
begin end;
procedure TMockThumbnailShardManager.InvalidateAll;
begin end;
function TMockThumbnailShardManager.EnsureDownloadShard: WideString;
begin Result := ''; end;
function TMockThumbnailShardManager.EnsureUploadShard: WideString;
begin Result := ''; end;

{TCloudThumbnailServiceTest}

procedure TCloudThumbnailServiceTest.Setup;
begin
	FMockHTTP := TMockThumbnailHTTP.Create;
	FMockShardManager := TMockThumbnailShardManager.Create;
	FMockLogger := TNullLogger.Create;
	FTestOAuthToken.access_token := 'test-token-12345';
	FService := TCloudThumbnailService.Create(FMockHTTP, FMockShardManager, FMockLogger, FTestOAuthToken, TThumbnailBitmapConverter.Create);
end;

procedure TCloudThumbnailServiceTest.TearDown;
begin
	FService := nil;
	FMockLogger := nil;
	{FMockHTTP and FMockShardManager are freed by reference counting}
end;

procedure TCloudThumbnailServiceTest.TestGetThumbnail_ConstructsCorrectURL;
begin
	{Shard URL from dispatcher includes /thumb/ path}
	FMockShardManager.ConfigureShard('https://thumb.cloud.mail.ru/thumb');
	FMockHTTP.ConfigureFailure(FS_FILE_NOTFOUND); {Fail so we can check URL without needing valid image}

	FService.GetThumbnail('/folder/image.jpg', 160, 120);

	{URL should be: shard/preset/path with auth params}
	Assert.Contains(FMockHTTP.GetLastGetFileURL, 'https://thumb.cloud.mail.ru/thumb/');
	Assert.Contains(FMockHTTP.GetLastGetFileURL, '/folder/image.jpg');
end;

procedure TCloudThumbnailServiceTest.TestGetThumbnail_UsesCorrectSizePreset;
begin
	FMockShardManager.ConfigureShard('https://thumb.test.ru/thumb');
	FMockHTTP.ConfigureFailure(FS_FILE_NOTFOUND);

	{Request 160x120 - should select xw17 (160x120)}
	FService.GetThumbnail('/test.jpg', 160, 120);

	{URL should contain preset}
	Assert.Contains(FMockHTTP.GetLastGetFileURL, '/thumb/xw');
end;

procedure TCloudThumbnailServiceTest.TestGetThumbnail_PathStartsWithSlash;
begin
	FMockShardManager.ConfigureShard('https://thumb.test.ru/thumb');
	FMockHTTP.ConfigureFailure(FS_FILE_NOTFOUND);

	FService.GetThumbnail('/folder/subfolder/image.png', 100, 100);

	{Path should be preserved with leading slash}
	Assert.Contains(FMockHTTP.GetLastGetFileURL, '/folder/subfolder/image.png');
end;

procedure TCloudThumbnailServiceTest.TestGetThumbnail_IncludesAuthParameters;
begin
	FMockShardManager.ConfigureShard('https://thumb.test.ru/thumb');
	FMockHTTP.ConfigureFailure(FS_FILE_NOTFOUND);

	FService.GetThumbnail('/test.jpg', 100, 100);

	{URL should include OAuth authentication parameters}
	Assert.Contains(FMockHTTP.GetLastGetFileURL, 'client_id=' + OAUTH_CLIENT_ID);
	Assert.Contains(FMockHTTP.GetLastGetFileURL, 'token=test-token-12345');
end;

procedure TCloudThumbnailServiceTest.TestGetThumbnail_UsesFallbackWhenShardResolveFails;
begin
	FMockShardManager.ConfigureResolveFailure;
	FMockHTTP.ConfigureFailure(FS_FILE_NOTFOUND);

	FService.GetThumbnail('/test.jpg', 100, 100);

	{Should use fallback URL from CloudConstants}
	Assert.Contains(FMockHTTP.GetLastGetFileURL, THUMB_CLOUD_URL);
end;

procedure TCloudThumbnailServiceTest.TestGetThumbnail_ReturnsZeroOnDownloadFailure;
var
	Result: HBITMAP;
begin
	FMockHTTP.ConfigureFailure(FS_FILE_NOTFOUND);

	Result := FService.GetThumbnail('/test.jpg', 100, 100);

	Assert.AreEqual(HBITMAP(0), Result);
end;

procedure TCloudThumbnailServiceTest.TestGetThumbnail_ReturnsZeroOnEmptyResponse;
var
	EmptyData: TBytes;
	Result: HBITMAP;
begin
	SetLength(EmptyData, 0);
	FMockHTTP.ConfigureSuccess(EmptyData);

	Result := FService.GetThumbnail('/test.jpg', 100, 100);

	Assert.AreEqual(HBITMAP(0), Result);
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudThumbnailServiceTest);

end.
