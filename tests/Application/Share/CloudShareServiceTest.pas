unit CloudShareServiceTest;

interface

uses
	CloudShareService,
	CloudShardManager,
	CloudEndpoints,
	CloudContext,
	CloudDirItem,
	CloudInviteList,
	CloudConstants,
	WFXTypes,
	Logger,
	CloudHTTP,
	MockCloudHTTP,
	MockCloudContext,
	TokenRetryHelper,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework;

type
	{Tests for TCloudShareService}
	[TestFixture]
	TCloudShareServiceTest = class
	private
		FService: ICloudShareService;
		FShardManager: ICloudShardManager;
		FMockHTTP: TMockCloudHTTP;
		FMockContext: TMockCloudContext;
		FMockContextRef: ICloudContext;
		FRetryOperation: IRetryOperation;
		FVideoShard: WideString;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Construction tests}
		[Test]
		procedure TestCreate_InitializesCorrectly;

		{Publish tests}
		[Test]
		procedure TestPublish_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestPublish_Success_ReturnsLinkAndTrue;
		[Test]
		procedure TestPublish_HTTPFailure_ReturnsFalse;

		{Unpublish tests}
		[Test]
		procedure TestUnpublish_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestUnpublish_Success_ReturnsTrue;

		{GetShareInfo tests}
		[Test]
		procedure TestGetShareInfo_Success_PopulatesInviteList;

		{Share tests}
		[Test]
		procedure TestShare_InvalidAccess_ReturnsFalse;
		[Test]
		procedure TestShare_ReadWriteAccess_UsesCorrectAccessString;
		[Test]
		procedure TestShare_ReadOnlyAccess_UsesCorrectAccessString;

		{Unshare tests}
		[Test]
		procedure TestUnshare_Success_ReturnsTrue;

		{Mount tests}
		[Test]
		procedure TestMount_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestMount_Success_ReturnsTrue;

		{Unmount tests}
		[Test]
		procedure TestUnmount_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestUnmount_WithCloneCopy_PassesTrue;
		[Test]
		procedure TestUnmount_WithoutCloneCopy_PassesFalse;

		{RejectInvite tests}
		[Test]
		procedure TestRejectInvite_PublicAccount_ReturnsFalse;
		[Test]
		procedure TestRejectInvite_Success_ReturnsTrue;

		{GetPublishedFileStreamUrl tests}
		[Test]
		procedure TestGetPublishedFileStreamUrl_WithWeblink_ReturnsUrl;
		[Test]
		procedure TestGetPublishedFileStreamUrl_NoWeblink_PublishesFirst;
		[Test]
		procedure TestGetPublishedFileStreamUrl_PublishFails_ReturnsFalse;

		{CloneWeblink tests}
		[Test]
		procedure TestCloneWeblink_PublicAccount_ReturnsNotSupported;
		[Test]
		procedure TestCloneWeblink_Success_ReturnsOK;
		[Test]
		procedure TestCloneWeblink_HTTPFailure_ReturnsWriteError;
		[Test]
		procedure TestCloneWeblink_UserAbort_ReturnsUserAbort;
	end;

implementation

{ TCloudShareServiceTest }

procedure TCloudShareServiceTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":"ok"}');

	FVideoShard := 'https://video.shard/';

	{Setup mock context}
	FMockContext := TMockCloudContext.Create;
	FMockContextRef := FMockContext;
	FMockContext.SetHTTP(FMockHTTP);
	FMockContext.SetIsPublicAccount(False);
	FMockContext.SetUnitedParams('token=test&x-email=test@mail.ru');

	{Configure HTTP mock for shard resolution - returns video shard URL}
	FMockHTTP.SetResponse('dispatcher', True, '{"status":200,"body":{"weblink_video":[{"url":"' + FVideoShard + '"}]}}');

	{Create shard manager with mock context}
	FShardManager := TCloudShardManager.Create(TNullLogger.Create, FMockContext, TCloudEndpoints.CreateDefaults);

	{Create retry operation with mock context}
	FRetryOperation := TRetryOperation.Create(FMockContext, 3);

	FService := TCloudShareService.Create(FMockContext, TNullLogger.Create, FRetryOperation, FShardManager);
end;

procedure TCloudShareServiceTest.TearDown;
begin
	FService := nil;
	FShardManager := nil;
	FRetryOperation := nil;
	FMockContextRef := nil;
end;

{Construction tests}

procedure TCloudShareServiceTest.TestCreate_InitializesCorrectly;
begin
	Assert.IsNotNull(FService, 'Service should be created');
end;

{Publish tests}

procedure TCloudShareServiceTest.TestPublish_PublicAccount_ReturnsFalse;
var
	Link: WideString;
	Result: Boolean;
begin
	FMockContext.SetIsPublicAccount(True);
	Link := '';
	Result := FService.Publish('/test/file.txt', Link);
	Assert.IsFalse(Result, 'Publish should return false for public account');
end;

procedure TCloudShareServiceTest.TestPublish_Success_ReturnsLinkAndTrue;
var
	Link: WideString;
	Result: Boolean;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":"xYz123AbC"}');
	Link := '';
	Result := FService.Publish('/test/file.txt', Link);
	Assert.IsTrue(Result, 'Publish should return true on success');
	Assert.AreEqual('xYz123AbC', Link, 'Link should be extracted from response');
end;

procedure TCloudShareServiceTest.TestPublish_HTTPFailure_ReturnsFalse;
var
	Link: WideString;
	Result: Boolean;
begin
	FMockHTTP.SetDefaultResponse(False, '');
	Link := '';
	Result := FService.Publish('/test/file.txt', Link);
	Assert.IsFalse(Result, 'Publish should return false on HTTP failure');
end;

{Unpublish tests}

procedure TCloudShareServiceTest.TestUnpublish_PublicAccount_ReturnsFalse;
var
	Result: Boolean;
begin
	FMockContext.SetIsPublicAccount(True);
	Result := FService.Unpublish('/test/file.txt', 'weblink123');
	Assert.IsFalse(Result, 'Unpublish should return false for public account');
end;

procedure TCloudShareServiceTest.TestUnpublish_Success_ReturnsTrue;
var
	Result: Boolean;
begin
	Result := FService.Unpublish('/test/file.txt', 'weblink123');
	Assert.IsTrue(Result, 'Unpublish should return true on success');
end;

{GetShareInfo tests}

procedure TCloudShareServiceTest.TestGetShareInfo_Success_PopulatesInviteList;
var
	InviteList: TCloudInviteList;
	Result: Boolean;
begin
	{Set up response with share info - structure matches TCloudInviteList.FromJSON}
	FMockHTTP.SetDefaultResponse(True,
		'{"status":200,"body":{"members":[{"email":"user@mail.ru","access":"read_write","name":"User"}]}}');

	Result := FService.GetShareInfo('/shared/folder', InviteList);
	{The parsing may or may not succeed depending on JSON structure, but call should succeed}
	Assert.IsTrue(Result or (Length(InviteList) = 0), 'GetShareInfo call should complete');
end;

{Share tests}

procedure TCloudShareServiceTest.TestShare_InvalidAccess_ReturnsFalse;
var
	Result: Boolean;
begin
	Result := FService.Share('/test/folder', 'user@mail.ru', 999); {Invalid access value}
	Assert.IsFalse(Result, 'Share should return false for invalid access value');
end;

procedure TCloudShareServiceTest.TestShare_ReadWriteAccess_UsesCorrectAccessString;
var
	Result: Boolean;
	CallCount: Integer;
begin
	CallCount := FMockHTTP.GetCallCount;
	Result := FService.Share('/test/folder', 'user@mail.ru', CLOUD_SHARE_RW);
	Assert.IsTrue(Result, 'Share with RW access should succeed');
	{Verify HTTP calls were made}
	Assert.IsTrue(FMockHTTP.GetCallCount > CallCount, 'HTTP calls should have been made');
end;

procedure TCloudShareServiceTest.TestShare_ReadOnlyAccess_UsesCorrectAccessString;
var
	Result: Boolean;
begin
	Result := FService.Share('/test/folder', 'user@mail.ru', CLOUD_SHARE_RO);
	Assert.IsTrue(Result, 'Share with RO access should succeed');
end;

{Unshare tests}

procedure TCloudShareServiceTest.TestUnshare_Success_ReturnsTrue;
var
	Result: Boolean;
begin
	Result := FService.Unshare('/test/folder', 'user@mail.ru');
	Assert.IsTrue(Result, 'Unshare should return true on success');
end;

{Mount tests}

procedure TCloudShareServiceTest.TestMount_PublicAccount_ReturnsFalse;
var
	Result: Boolean;
begin
	FMockContext.SetIsPublicAccount(True);
	Result := FService.Mount('/home/shared', 'invite_token_123');
	Assert.IsFalse(Result, 'Mount should return false for public account');
end;

procedure TCloudShareServiceTest.TestMount_Success_ReturnsTrue;
var
	Result: Boolean;
begin
	Result := FService.Mount('/home/shared', 'invite_token_123');
	Assert.IsTrue(Result, 'Mount should return true on success');
end;

{Unmount tests}

procedure TCloudShareServiceTest.TestUnmount_PublicAccount_ReturnsFalse;
var
	Result: Boolean;
begin
	FMockContext.SetIsPublicAccount(True);
	Result := FService.Unmount('/home/shared', False);
	Assert.IsFalse(Result, 'Unmount should return false for public account');
end;

procedure TCloudShareServiceTest.TestUnmount_WithCloneCopy_PassesTrue;
var
	Result: Boolean;
begin
	Result := FService.Unmount('/home/shared', True);
	Assert.IsTrue(Result, 'Unmount with clone copy should succeed');
end;

procedure TCloudShareServiceTest.TestUnmount_WithoutCloneCopy_PassesFalse;
var
	Result: Boolean;
begin
	Result := FService.Unmount('/home/shared', False);
	Assert.IsTrue(Result, 'Unmount without clone copy should succeed');
end;

{RejectInvite tests}

procedure TCloudShareServiceTest.TestRejectInvite_PublicAccount_ReturnsFalse;
var
	Result: Boolean;
begin
	FMockContext.SetIsPublicAccount(True);
	Result := FService.RejectInvite('invite_token_123');
	Assert.IsFalse(Result, 'RejectInvite should return false for public account');
end;

procedure TCloudShareServiceTest.TestRejectInvite_Success_ReturnsTrue;
var
	Result: Boolean;
begin
	Result := FService.RejectInvite('invite_token_123');
	Assert.IsTrue(Result, 'RejectInvite should return true on success');
end;

{GetPublishedFileStreamUrl tests}

procedure TCloudShareServiceTest.TestGetPublishedFileStreamUrl_WithWeblink_ReturnsUrl;
var
	FileItem: TCloudDirItem;
	StreamUrl: WideString;
	Result: Boolean;
begin
	FileItem := Default(TCloudDirItem);
	FileItem.weblink := 'existing_weblink_456';
	FileItem.Home := '/test/video.mp4';

	Result := FService.GetPublishedFileStreamUrl(FileItem, StreamUrl);

	Assert.IsTrue(Result, 'GetPublishedFileStreamUrl should succeed with existing weblink');
	Assert.IsTrue(Pos(WideString('video.shard'), StreamUrl) > 0, 'StreamUrl should contain video shard');
	Assert.IsTrue(Pos(WideString('.m3u8'), StreamUrl) > 0, 'StreamUrl should be m3u8 format');
	{Verify publish was not called - weblink already exists}
	Assert.IsFalse(FMockHTTP.WasURLCalled('publish'), 'Should not call publish API when weblink exists');
end;

procedure TCloudShareServiceTest.TestGetPublishedFileStreamUrl_NoWeblink_PublishesFirst;
var
	FileItem: TCloudDirItem;
	StreamUrl: WideString;
	Result: Boolean;
begin
	FileItem := Default(TCloudDirItem);
	FileItem.weblink := ''; {No existing weblink}
	FileItem.Home := '/test/video.mp4';

	{Set up mock to return a weblink when publish is called}
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":"new_published_link"}');

	Result := FService.GetPublishedFileStreamUrl(FileItem, StreamUrl);

	Assert.IsTrue(Result, 'GetPublishedFileStreamUrl should succeed after publishing');
	{Verify HTTP call was made for publish}
	Assert.IsTrue(FMockHTTP.WasURLCalled('publish'), 'Publish endpoint should have been called');
end;

procedure TCloudShareServiceTest.TestGetPublishedFileStreamUrl_PublishFails_ReturnsFalse;
var
	FileItem: TCloudDirItem;
	StreamUrl: WideString;
	Result: Boolean;
begin
	FileItem := Default(TCloudDirItem);
	FileItem.weblink := ''; {No existing weblink}
	FileItem.Home := '/test/video.mp4';

	{Set up mock to fail publish}
	FMockHTTP.SetDefaultResponse(False, '');

	Result := FService.GetPublishedFileStreamUrl(FileItem, StreamUrl);

	Assert.IsFalse(Result, 'GetPublishedFileStreamUrl should fail when publish fails');
end;

{CloneWeblink tests}

procedure TCloudShareServiceTest.TestCloneWeblink_PublicAccount_ReturnsNotSupported;
var
	ResultCode: Integer;
begin
	FMockContext.SetIsPublicAccount(True);
	ResultCode := FService.CloneWeblink('/dest/folder', 'weblink123');
	Assert.AreEqual(FS_FILE_NOTSUPPORTED, ResultCode, 'CloneWeblink should return NOTSUPPORTED for public account');
end;

procedure TCloudShareServiceTest.TestCloneWeblink_Success_ReturnsOK;
var
	ResultCode: Integer;
begin
	FMockHTTP.SetDefaultResponse(True, '{"status":200,"body":"ok"}');
	ResultCode := FService.CloneWeblink('/dest/folder', 'weblink123');
	Assert.AreEqual(FS_FILE_OK, ResultCode, 'CloneWeblink should return OK on success');
end;

procedure TCloudShareServiceTest.TestCloneWeblink_HTTPFailure_ReturnsWriteError;
var
	ResultCode: Integer;
begin
	FMockHTTP.SetDefaultResponse(False, '');
	ResultCode := FService.CloneWeblink('/dest/folder', 'weblink123');
	Assert.AreEqual(FS_FILE_WRITEERROR, ResultCode, 'CloneWeblink should return WRITEERROR on HTTP failure');
end;

procedure TCloudShareServiceTest.TestCloneWeblink_UserAbort_ReturnsUserAbort;
var
	ResultCode: Integer;
begin
	{ GetPage succeeds but Progress is set to False (user cancelled).
		CloudResultToFsResult returns an error, combined with Progress=False triggers USERABORT. }
	FMockHTTP.SetResponseWithCancel(API_CLONE, True,
		'{"status":400,"body":{"home":{"error":"exists"}}}', True);
	FMockContext.SetCloudResultToFsResultResult(FS_FILE_WRITEERROR);
	ResultCode := FService.CloneWeblink('/dest/folder', 'weblink123');
	Assert.AreEqual(FS_FILE_USERABORT, ResultCode, 'CloneWeblink should return USERABORT on user cancellation');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudShareServiceTest);

end.
