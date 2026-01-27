unit CloudMailRuMiscOpsTest;

{Tests for TCloudMailRu miscellaneous operations:
 - RejectInvite
 - GetPublishedFileStreamUrl}

interface

uses
	CloudMailRu,
	CloudSettings,
	CloudDirItem,
	CloudConstants,
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
	TestHelper,
	System.SysUtils,
	System.Classes,
	DUnitX.TestFramework;

type
	{Testable subclass that exposes protected members}
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
	end;

	[TestFixture]
	TCloudMailRuMiscOpsTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TTestableCloudMailRu;
		FSettings: TCloudSettings;

		function CreateCloud(PublicAccount: Boolean = False): TTestableCloudMailRu;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{RejectInvite tests}
		[Test]
		procedure TestRejectInvite_Success;
		[Test]
		procedure TestRejectInvite_Failure;
		[Test]
		procedure TestRejectInvite_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestRejectInvite_SendsCorrectToken;

		{GetPublishedFileStreamUrl tests}
		[Test]
		procedure TestGetPublishedFileStreamUrl_WithExistingWeblink;
		[Test]
		procedure TestGetPublishedFileStreamUrl_FailsIfNoWeblinkAndPublishDisabled;
		[Test]
		procedure TestGetPublishedFileStreamUrl_FailsIfShardFails;
		[Test]
		procedure TestGetPublishedFileStreamUrl_GeneratesM3U8Url;
	end;

implementation

const
	JSON_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';
	JSON_FAILURE = '{"email":"test@mail.ru","body":{"home":{"error":"invalid"}},"status":400}';

	JSON_SHARD_SUCCESS =
		'{"email":"test@mail.ru","body":{' +
		'"weblink_video":[{"url":"https://cloclo1.cloud.mail.ru/videoweb/"}],' +
		'"get":[{"url":"https://cloclo1.cloud.mail.ru/get/"}]' +
		'},"status":200}';

	JSON_PUBLISH_SUCCESS =
		'{"email":"test@mail.ru","body":"abc123weblink","status":200}';

{TTestableCloudMailRu}

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

{TCloudMailRuMiscOpsTest}

procedure TCloudMailRuMiscOpsTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
end;

procedure TCloudMailRuMiscOpsTest.TearDown;
begin
	FCloud.Free;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCloudMailRuMiscOpsTest.CreateCloud(PublicAccount: Boolean): TTestableCloudMailRu;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := PublicAccount;

	Result := TTestableCloudMailRu.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create);

	Result.SetUnitedParams('api=2&access_token=test_token');
end;

{RejectInvite tests}

procedure TCloudMailRuMiscOpsTest.TestRejectInvite_Success;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_INVITE_REJECT, True, JSON_SUCCESS);

	var Success := FCloud.ShareService.RejectInvite('invite_token_123');

	Assert.IsTrue(Success, 'RejectInvite should return True on success');
end;

procedure TCloudMailRuMiscOpsTest.TestRejectInvite_Failure;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_INVITE_REJECT, True, JSON_FAILURE);

	var Success := FCloud.ShareService.RejectInvite('invite_token_123');

	Assert.IsFalse(Success, 'RejectInvite should return False on failure');
end;

procedure TCloudMailRuMiscOpsTest.TestRejectInvite_PublicAccount_ReturnsFalse;
begin
	FCloud := CreateCloud(True);
	FMockHTTP.SetResponse(API_INVITE_REJECT, True, JSON_SUCCESS);

	var Success := FCloud.ShareService.RejectInvite('invite_token_123');

	Assert.IsFalse(Success, 'RejectInvite should return False for public accounts');
end;

procedure TCloudMailRuMiscOpsTest.TestRejectInvite_SendsCorrectToken;
var
	PostedData: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_INVITE_REJECT, True, JSON_SUCCESS);

	FCloud.ShareService.RejectInvite('my_invite_token_xyz');

	PostedData := FMockHTTP.GetLastPostedData;
	Assert.IsTrue(Pos(String('invite_token=my_invite_token_xyz'), String(PostedData)) > 0,
		'Should send correct invite_token parameter');
end;

{GetPublishedFileStreamUrl tests}

procedure TCloudMailRuMiscOpsTest.TestGetPublishedFileStreamUrl_WithExistingWeblink;
var
	FileInfo: TCloudDirItem;
	StreamUrl: WideString;
begin
	FCloud := CreateCloud;
	{Setup shard response}
	FMockHTTP.SetResponse(API_DISPATCHER, True, JSON_SHARD_SUCCESS);

	FileInfo := Default(TCloudDirItem);
	FileInfo.weblink := 'existing_weblink';
	FileInfo.Home := '/test/file.mp4';

	var Success := FCloud.ShareService.GetPublishedFileStreamUrl(FileInfo, StreamUrl);

	Assert.IsTrue(Success, 'Should succeed with existing weblink');
	Assert.IsNotEmpty(StreamUrl, 'StreamUrl should not be empty');
end;

procedure TCloudMailRuMiscOpsTest.TestGetPublishedFileStreamUrl_FailsIfNoWeblinkAndPublishDisabled;
var
	FileInfo: TCloudDirItem;
	StreamUrl: WideString;
begin
	FCloud := CreateCloud;

	FileInfo := Default(TCloudDirItem);
	FileInfo.weblink := ''; {No weblink}
	FileInfo.Home := '/test/file.mp4';

	{Publish=False means no publishing, so should fail immediately}
	var Success := FCloud.ShareService.GetPublishedFileStreamUrl(FileInfo, StreamUrl, SHARD_TYPE_WEBLINK_VIDEO, False);

	Assert.IsFalse(Success, 'Should fail when weblink is empty and publish is disabled');
end;

procedure TCloudMailRuMiscOpsTest.TestGetPublishedFileStreamUrl_FailsIfShardFails;
var
	FileInfo: TCloudDirItem;
	StreamUrl: WideString;
begin
	FCloud := CreateCloud;
	{Setup failed shard response}
	FMockHTTP.SetResponse(API_DISPATCHER, True, JSON_FAILURE);

	FileInfo := Default(TCloudDirItem);
	FileInfo.weblink := 'existing_weblink';
	FileInfo.Home := '/test/file.mp4';

	var Success := FCloud.ShareService.GetPublishedFileStreamUrl(FileInfo, StreamUrl);

	Assert.IsFalse(Success, 'Should fail if shard retrieval fails');
end;

procedure TCloudMailRuMiscOpsTest.TestGetPublishedFileStreamUrl_GeneratesM3U8Url;
var
	FileInfo: TCloudDirItem;
	StreamUrl: WideString;
begin
	FCloud := CreateCloud;
	FMockHTTP.SetResponse(API_DISPATCHER, True, JSON_SHARD_SUCCESS);

	FileInfo := Default(TCloudDirItem);
	FileInfo.weblink := 'test_weblink';
	FileInfo.Home := '/test/file.mp4';

	FCloud.ShareService.GetPublishedFileStreamUrl(FileInfo, StreamUrl);

	Assert.IsTrue(Pos(String('.m3u8'), String(StreamUrl)) > 0, 'StreamUrl should contain .m3u8 extension');
	Assert.IsTrue(Pos(String('double_encode=1'), String(StreamUrl)) > 0, 'StreamUrl should contain double_encode parameter');
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuMiscOpsTest);

end.
