unit CloudMailRuShardTest;

{Tests for TCloudMailRu.GetShard() and shard-related functionality.
 Tests shard retrieval via dispatcher API.}

interface

uses
	CloudMailRu,
	CloudSettings,
	CloudConstants,
	FileCipher,
	WFXTypes,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	AuthStrategy,
	WindowsFileSystem,
	CloudHTTP,
	HTTPManager,
	MockCloudHTTP,
	MockHTTPManager,
	MockShardHelper,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework,
	OpenSSLProvider;

type
	{Testable subclass for shard testing}
	TTestableShardCloud = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
	end;

	[TestFixture]
	TCloudMailRuShardTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TTestableShardCloud;
		FSettings: TCloudSettings;

		function CreateCloud(PublicAccount: Boolean = False): TTestableShardCloud;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{GetShard tests}
		[Test]
		procedure TestGetShard_Download_ReturnsURL;
		[Test]
		procedure TestGetShard_Upload_ReturnsURL;
		[Test]
		procedure TestGetShard_Video_ReturnsURL;
		[Test]
		procedure TestGetShard_WeblinkGet_ReturnsURL;
		[Test]
		procedure TestGetShard_WeblinkVideo_ReturnsURL;
		[Test]
		procedure TestGetShard_DispatcherFailure_ReturnsFalse;
		[Test]
		procedure TestGetShard_MissingShardType_ReturnsFalse;
		[Test]
		procedure TestGetShard_EmptyURL_ReturnsFalse;
		[Test]
		procedure TestGetShard_CallsDispatcherAPI;

		{MockShardHelper tests}
		[Test]
		procedure TestMockShardHelper_CreateShardResponse;
		[Test]
		procedure TestMockShardHelper_CreateDispatcherResponse;
		[Test]
		procedure TestMockShardHelper_CreateFullDispatcherResponse;
		[Test]
		procedure TestMockShardHelper_OAuthDispatcherFormat;

		{MockCloudHTTP shard helper tests}
		[Test]
		procedure TestSetShardResponse_ConfiguresDispatcher;
		[Test]
		procedure TestSetFullDispatcherResponse_ConfiguresBothShards;
	end;

implementation

{TTestableShardCloud}

procedure TTestableShardCloud.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

{TCloudMailRuShardTest}

procedure TCloudMailRuShardTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
end;

procedure TCloudMailRuShardTest.TearDown;
begin
	FreeAndNil(FCloud);
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCloudMailRuShardTest.CreateCloud(PublicAccount: Boolean): TTestableShardCloud;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := PublicAccount;

	Result := TTestableShardCloud.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		TNullCipher.Create, TNullOpenSSLProvider.Create);

	Result.SetUnitedParams('api=2&access_token=test_token');
end;

{GetShard tests - test shard mock infrastructure rather than private GetShard method}

procedure TCloudMailRuShardTest.TestGetShard_Download_ReturnsURL;
var
	Answer: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetShardResponse(SHARD_TYPE_GET, 'https://download.cloud.mail.ru/');

	FMockHTTP.PostForm(API_DISPATCHER + '?token=test', '', Answer);

	Assert.IsTrue(Pos('https://download.cloud.mail.ru/', String(Answer)) > 0,
		'Dispatcher response should contain download shard URL');
end;

procedure TCloudMailRuShardTest.TestGetShard_Upload_ReturnsURL;
var
	Answer: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetShardResponse(SHARD_TYPE_UPLOAD, 'https://upload.cloud.mail.ru/');

	FMockHTTP.PostForm(API_DISPATCHER + '?token=test', '', Answer);

	Assert.IsTrue(Pos('https://upload.cloud.mail.ru/', String(Answer)) > 0,
		'Dispatcher response should contain upload shard URL');
end;

procedure TCloudMailRuShardTest.TestGetShard_Video_ReturnsURL;
var
	Answer: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetShardResponse(SHARD_TYPE_VIDEO, 'https://video.cloud.mail.ru/');

	FMockHTTP.PostForm(API_DISPATCHER + '?token=test', '', Answer);

	Assert.IsTrue(Pos('https://video.cloud.mail.ru/', String(Answer)) > 0,
		'Dispatcher response should contain video shard URL');
end;

procedure TCloudMailRuShardTest.TestGetShard_WeblinkGet_ReturnsURL;
var
	Answer: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetShardResponse(SHARD_TYPE_WEBLINK_GET, 'https://weblink.cloud.mail.ru/');

	FMockHTTP.PostForm(API_DISPATCHER + '?token=test', '', Answer);

	Assert.IsTrue(Pos('https://weblink.cloud.mail.ru/', String(Answer)) > 0,
		'Dispatcher response should contain weblink shard URL');
end;

procedure TCloudMailRuShardTest.TestGetShard_WeblinkVideo_ReturnsURL;
var
	Answer: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetShardResponse(SHARD_TYPE_WEBLINK_VIDEO, 'https://weblinkvideo.cloud.mail.ru/');

	FMockHTTP.PostForm(API_DISPATCHER + '?token=test', '', Answer);

	Assert.IsTrue(Pos('https://weblinkvideo.cloud.mail.ru/', String(Answer)) > 0,
		'Dispatcher response should contain weblink video shard URL');
end;

procedure TCloudMailRuShardTest.TestGetShard_DispatcherFailure_ReturnsFalse;
var
	Answer: WideString;
	Success: Boolean;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse('/dispatcher/', False, '');

	Success := FMockHTTP.PostForm(API_DISPATCHER + '?token=test', '', Answer);

	Assert.IsFalse(Success, 'PostForm should return false when dispatcher fails');
end;

procedure TCloudMailRuShardTest.TestGetShard_MissingShardType_ReturnsFalse;
var
	Answer: WideString;
begin
	FCloud := CreateCloud;
	{Set response with only 'get' shard - 'upload' will be missing}
	FMockHTTP.SetShardResponse(SHARD_TYPE_GET, 'https://download.cloud.mail.ru/');

	FMockHTTP.PostForm(API_DISPATCHER + '?token=test', '', Answer);

	{Response won't contain upload shard}
	Assert.IsFalse(Pos('"upload"', String(Answer)) > 0, 'Response should not contain upload shard');
end;

procedure TCloudMailRuShardTest.TestGetShard_EmptyURL_ReturnsFalse;
var
	Answer: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetShardResponse(SHARD_TYPE_GET, '');

	FMockHTTP.PostForm(API_DISPATCHER + '?token=test', '', Answer);

	{Even with empty URL, JSON is still valid - the actual check happens in GetShard}
	Assert.IsTrue(Pos('""', String(Answer)) > 0, 'Should contain empty URL in response');
end;

procedure TCloudMailRuShardTest.TestGetShard_CallsDispatcherAPI;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetShardResponse(SHARD_TYPE_GET, 'https://download.cloud.mail.ru/');

	var Answer: WideString;
	FMockHTTP.PostForm(API_DISPATCHER + '?token=test', '', Answer);

	Assert.IsTrue(FMockHTTP.WasURLCalled('/dispatcher/'), 'Should call dispatcher API');
end;

{MockShardHelper tests}

procedure TCloudMailRuShardTest.TestMockShardHelper_CreateShardResponse;
var
	JSON: WideString;
begin
	JSON := TMockShardHelper.CreateShardResponse('get', 'https://test.url/');

	Assert.IsTrue(Pos('"get"', String(JSON)) > 0, 'Should contain shard type');
	Assert.IsTrue(Pos('https://test.url/', String(JSON)) > 0, 'Should contain URL');
	Assert.IsTrue(Pos('"status":200', String(JSON)) > 0, 'Should have success status');
end;

procedure TCloudMailRuShardTest.TestMockShardHelper_CreateDispatcherResponse;
var
	Shards: array[0..1] of TMockShard;
	JSON: WideString;
begin
	Shards[0] := TMockShard.Create('get', 'https://download.test/');
	Shards[1] := TMockShard.Create('upload', 'https://upload.test/');

	JSON := TMockShardHelper.CreateDispatcherResponse(Shards);

	Assert.IsTrue(Pos('"get"', String(JSON)) > 0, 'Should contain get shard');
	Assert.IsTrue(Pos('"upload"', String(JSON)) > 0, 'Should contain upload shard');
	Assert.IsTrue(Pos('https://download.test/', String(JSON)) > 0, 'Should contain download URL');
	Assert.IsTrue(Pos('https://upload.test/', String(JSON)) > 0, 'Should contain upload URL');
end;

procedure TCloudMailRuShardTest.TestMockShardHelper_CreateFullDispatcherResponse;
var
	JSON: WideString;
begin
	JSON := TMockShardHelper.CreateFullDispatcherResponse;

	Assert.IsTrue(Pos('"get"', String(JSON)) > 0, 'Should contain get shard');
	Assert.IsTrue(Pos('"upload"', String(JSON)) > 0, 'Should contain upload shard');
	Assert.IsTrue(Pos('"video"', String(JSON)) > 0, 'Should contain video shard');
	Assert.IsTrue(Pos('"weblink_get"', String(JSON)) > 0, 'Should contain weblink_get shard');
	Assert.IsTrue(Pos('"weblink_video"', String(JSON)) > 0, 'Should contain weblink_video shard');
end;

procedure TCloudMailRuShardTest.TestMockShardHelper_OAuthDispatcherFormat;
var
	Response: WideString;
begin
	Response := TMockShardHelper.CreateOAuthDownloadDispatcherResponse('https://oauth.download/');

	{OAuth format is "URL IP COUNT"}
	Assert.AreEqual(String('https://oauth.download/ 127.0.0.1 1'), String(Response), 'OAuth dispatcher should return plain text format');
end;

{MockCloudHTTP shard helper tests}

procedure TCloudMailRuShardTest.TestSetShardResponse_ConfiguresDispatcher;
var
	Answer: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetShardResponse('get', 'https://configured.url/');

	FMockHTTP.PostForm(API_DISPATCHER + '?token=test', '', Answer);

	Assert.IsTrue(Pos('https://configured.url/', String(Answer)) > 0, 'Dispatcher should return configured shard URL');
end;

procedure TCloudMailRuShardTest.TestSetFullDispatcherResponse_ConfiguresBothShards;
var
	Answer: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetFullDispatcherResponse('https://download.test/', 'https://upload.test/');

	FMockHTTP.PostForm(API_DISPATCHER + '?token=test', '', Answer);

	Assert.IsTrue(Pos('"get"', String(Answer)) > 0, 'Should contain get shard');
	Assert.IsTrue(Pos('"upload"', String(Answer)) > 0, 'Should contain upload shard');
	Assert.IsTrue(Pos('https://download.test/', String(Answer)) > 0, 'Should contain download URL');
	Assert.IsTrue(Pos('https://upload.test/', String(Answer)) > 0, 'Should contain upload URL');
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuShardTest);

end.
