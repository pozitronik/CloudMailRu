unit CloudShardManagerTest;

interface

uses
	CloudShardManager,
	CloudContext,
	CloudConstants,
	TCLogger,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework;

type
	{Mock implementation of IShardContext for shard manager tests}
	TMockShardContext = class(TInterfacedObject, IShardContext)
	private
		FPostFormResult: Boolean;
		FPostFormResponse: WideString;
		FPostFormCalled: Boolean;
		FResultToBooleanResult: Boolean;
		FUnitedParams: WideString;
		FGetPageResult: Boolean;
		FGetPageResponse: WideString;
		FOAuthAccessToken: WideString;
	public
		constructor Create;
		procedure SetPostFormResult(Value: Boolean; const Response: WideString);
		procedure SetResultToBooleanResult(Value: Boolean);
		procedure SetUnitedParams(const Value: WideString);
		procedure SetGetPageResult(Value: Boolean; const Response: WideString);
		procedure SetOAuthAccessToken(const Value: WideString);
		function WasPostFormCalled: Boolean;

		{IShardContext implementation}
		function PostForm(const URL, Data: WideString; var Answer: WideString): Boolean;
		function CloudResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean;
		function GetUnitedParams: WideString;
		function GetPage(const URL: WideString; var Response: WideString; var ShowProgress: Boolean): Boolean;
		function GetOAuthAccessToken: WideString;
	end;

	{Tests for TCloudShardManager.
	 The shard manager caches shard URLs and handles override configuration.}
	[TestFixture]
	TCloudShardManagerTest = class
	private
		FManager: ICloudShardManager;
		FMockContext: TMockShardContext;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ Initial state tests }
		[Test]
		procedure TestInitialState_DownloadShardEmpty;
		[Test]
		procedure TestInitialState_UploadShardEmpty;
		[Test]
		procedure TestInitialState_PublicShardEmpty;

		{ Set/Get download shard tests }
		[Test]
		procedure TestSetDownloadShard_StoresValue;
		[Test]
		procedure TestSetDownloadShard_OverwritesPreviousValue;

		{ Set/Get upload shard tests }
		[Test]
		procedure TestSetUploadShard_StoresValue;
		[Test]
		procedure TestSetUploadShard_OverwritesPreviousValue;

		{ Set/Get public shard tests }
		[Test]
		procedure TestSetPublicShard_StoresValue;
		[Test]
		procedure TestSetPublicShard_OverwritesPreviousValue;

		{ Invalidation tests }
		[Test]
		procedure TestInvalidateShard_ClearsDownloadShard;
		[Test]
		procedure TestInvalidateShard_ClearsUploadShard;
		[Test]
		procedure TestInvalidateShard_ClearsPublicShard;
		[Test]
		procedure TestInvalidateAll_ClearsAllShards;

		{ Override tests }
		[Test]
		procedure TestHasDownloadOverride_FalseWhenEmpty;
		[Test]
		procedure TestHasDownloadOverride_TrueWhenSet;
		[Test]
		procedure TestHasUploadOverride_FalseWhenEmpty;
		[Test]
		procedure TestHasUploadOverride_TrueWhenSet;
		[Test]
		procedure TestGetDownloadShardOverride_ReturnsConfiguredValue;
		[Test]
		procedure TestGetUploadShardOverride_ReturnsConfiguredValue;

		{ ResolveShard tests }
		[Test]
		procedure TestResolveShard_Success_ReturnsTrueAndShard;
		[Test]
		procedure TestResolveShard_PostFormFails_ReturnsFalse;
		[Test]
		procedure TestResolveShard_InvalidJSON_ReturnsFalse;

		{ EnsureDownloadShard tests }
		[Test]
		procedure TestEnsureDownloadShard_Cached_ReturnsCachedValue;
		[Test]
		procedure TestEnsureDownloadShard_ResolvesViaDispatcher;
		[Test]
		procedure TestEnsureDownloadShard_DispatcherFails_ReturnsEmpty;
		[Test]
		procedure TestEnsureDownloadShard_CachesResolvedValue;

		{ EnsureUploadShard tests }
		[Test]
		procedure TestEnsureUploadShard_Cached_ReturnsCachedValue;
		[Test]
		procedure TestEnsureUploadShard_ResolvesViaDispatcher;
		[Test]
		procedure TestEnsureUploadShard_DispatcherFails_ReturnsEmpty;
		[Test]
		procedure TestEnsureUploadShard_CachesResolvedValue;
	end;

	{Tests for TCloudShardManager with overrides configured}
	[TestFixture]
	TCloudShardManagerWithOverridesTest = class
	private
		FManager: ICloudShardManager;
		FMockContext: TMockShardContext;
		const
			DownloadOverrideUrl = 'https://custom.download.url/';
			UploadOverrideUrl = 'https://custom.upload.url/';
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestHasDownloadOverride_ReturnsTrue;
		[Test]
		procedure TestHasUploadOverride_ReturnsTrue;
		[Test]
		procedure TestGetDownloadOverride_ReturnsConfiguredUrl;
		[Test]
		procedure TestGetUploadOverride_ReturnsConfiguredUrl;

		{EnsureShard with overrides}
		[Test]
		procedure TestEnsureDownloadShard_UsesOverride;
		[Test]
		procedure TestEnsureUploadShard_UsesOverride;
	end;

	{Tests for TNullShardManager}
	[TestFixture]
	TNullShardManagerTest = class
	private
		FManager: ICloudShardManager;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestResolveShard_AlwaysReturnsFalse;
		[Test]
		procedure TestGetDownloadShard_AlwaysEmpty;
		[Test]
		procedure TestGetUploadShard_AlwaysEmpty;
		[Test]
		procedure TestGetPublicShard_AlwaysEmpty;
		[Test]
		procedure TestSetDownloadShard_NoOp;
		[Test]
		procedure TestHasDownloadOverride_AlwaysFalse;
		[Test]
		procedure TestHasUploadOverride_AlwaysFalse;
		[Test]
		procedure TestEnsureDownloadShard_AlwaysEmpty;
		[Test]
		procedure TestEnsureUploadShard_AlwaysEmpty;
	end;

implementation

{ TMockShardContext }

constructor TMockShardContext.Create;
begin
	inherited Create;
	FPostFormResult := True;
	FPostFormResponse := '';
	FPostFormCalled := False;
	FResultToBooleanResult := True;
	FUnitedParams := 'token=test';
	FGetPageResult := True;
	FGetPageResponse := '';
	FOAuthAccessToken := 'test-token';
end;

procedure TMockShardContext.SetPostFormResult(Value: Boolean; const Response: WideString);
begin
	FPostFormResult := Value;
	FPostFormResponse := Response;
end;

procedure TMockShardContext.SetResultToBooleanResult(Value: Boolean);
begin
	FResultToBooleanResult := Value;
end;

procedure TMockShardContext.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

function TMockShardContext.WasPostFormCalled: Boolean;
begin
	Result := FPostFormCalled;
end;

function TMockShardContext.PostForm(const URL, Data: WideString; var Answer: WideString): Boolean;
begin
	FPostFormCalled := True;
	Answer := FPostFormResponse;
	Result := FPostFormResult;
end;

function TMockShardContext.CloudResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean;
begin
	{Check JSON for status:200 when configured to do so, otherwise return configured result}
	if FResultToBooleanResult then
		Result := Pos(WideString('"status":200'), JSON) > 0
	else
		Result := False;
end;

function TMockShardContext.GetUnitedParams: WideString;
begin
	Result := FUnitedParams;
end;

procedure TMockShardContext.SetGetPageResult(Value: Boolean; const Response: WideString);
begin
	FGetPageResult := Value;
	FGetPageResponse := Response;
end;

procedure TMockShardContext.SetOAuthAccessToken(const Value: WideString);
begin
	FOAuthAccessToken := Value;
end;

function TMockShardContext.GetPage(const URL: WideString; var Response: WideString; var ShowProgress: Boolean): Boolean;
begin
	Response := FGetPageResponse;
	Result := FGetPageResult;
end;

function TMockShardContext.GetOAuthAccessToken: WideString;
begin
	Result := FOAuthAccessToken;
end;

{ TCloudShardManagerTest }

procedure TCloudShardManagerTest.Setup;
begin
	FMockContext := TMockShardContext.Create;
	FManager := TCloudShardManager.Create(TNullLogger.Create, FMockContext);
end;

procedure TCloudShardManagerTest.TearDown;
begin
	FManager := nil;
	FMockContext := nil;
end;

{ Initial state tests }

procedure TCloudShardManagerTest.TestInitialState_DownloadShardEmpty;
begin
	Assert.AreEqual('', FManager.GetDownloadShard, 'Download shard should be empty initially');
end;

procedure TCloudShardManagerTest.TestInitialState_UploadShardEmpty;
begin
	Assert.AreEqual('', FManager.GetUploadShard, 'Upload shard should be empty initially');
end;

procedure TCloudShardManagerTest.TestInitialState_PublicShardEmpty;
begin
	Assert.AreEqual('', FManager.GetPublicShard, 'Public shard should be empty initially');
end;

{ Download shard tests }

procedure TCloudShardManagerTest.TestSetDownloadShard_StoresValue;
const
	TestShard = 'https://cloclo1.cloud.mail.ru/get/';
begin
	FManager.SetDownloadShard(TestShard);
	Assert.AreEqual(TestShard, FManager.GetDownloadShard);
end;

procedure TCloudShardManagerTest.TestSetDownloadShard_OverwritesPreviousValue;
const
	OldShard = 'https://old.url/';
	NewShard = 'https://new.url/';
begin
	FManager.SetDownloadShard(OldShard);
	FManager.SetDownloadShard(NewShard);
	Assert.AreEqual(NewShard, FManager.GetDownloadShard);
end;

{ Upload shard tests }

procedure TCloudShardManagerTest.TestSetUploadShard_StoresValue;
const
	TestShard = 'https://cloclo1.cloud.mail.ru/upload/';
begin
	FManager.SetUploadShard(TestShard);
	Assert.AreEqual(TestShard, FManager.GetUploadShard);
end;

procedure TCloudShardManagerTest.TestSetUploadShard_OverwritesPreviousValue;
const
	OldShard = 'https://old.upload.url/';
	NewShard = 'https://new.upload.url/';
begin
	FManager.SetUploadShard(OldShard);
	FManager.SetUploadShard(NewShard);
	Assert.AreEqual(NewShard, FManager.GetUploadShard);
end;

{ Public shard tests }

procedure TCloudShardManagerTest.TestSetPublicShard_StoresValue;
const
	TestShard = 'https://cloclo1.cloud.mail.ru/weblink/';
begin
	FManager.SetPublicShard(TestShard);
	Assert.AreEqual(TestShard, FManager.GetPublicShard);
end;

procedure TCloudShardManagerTest.TestSetPublicShard_OverwritesPreviousValue;
const
	OldShard = 'https://old.public.url/';
	NewShard = 'https://new.public.url/';
begin
	FManager.SetPublicShard(OldShard);
	FManager.SetPublicShard(NewShard);
	Assert.AreEqual(NewShard, FManager.GetPublicShard);
end;

{ Invalidation tests }

procedure TCloudShardManagerTest.TestInvalidateShard_ClearsDownloadShard;
begin
	FManager.SetDownloadShard('https://some.url/');
	FManager.InvalidateShard(SHARD_TYPE_GET);
	Assert.AreEqual('', FManager.GetDownloadShard);
end;

procedure TCloudShardManagerTest.TestInvalidateShard_ClearsUploadShard;
begin
	FManager.SetUploadShard('https://some.url/');
	FManager.InvalidateShard(SHARD_TYPE_UPLOAD);
	Assert.AreEqual('', FManager.GetUploadShard);
end;

procedure TCloudShardManagerTest.TestInvalidateShard_ClearsPublicShard;
begin
	FManager.SetPublicShard('https://some.url/');
	FManager.InvalidateShard(SHARD_TYPE_WEBLINK_GET);
	Assert.AreEqual('', FManager.GetPublicShard);
end;

procedure TCloudShardManagerTest.TestInvalidateAll_ClearsAllShards;
begin
	FManager.SetDownloadShard('https://download.url/');
	FManager.SetUploadShard('https://upload.url/');
	FManager.SetPublicShard('https://public.url/');

	FManager.InvalidateAll;

	Assert.AreEqual('', FManager.GetDownloadShard);
	Assert.AreEqual('', FManager.GetUploadShard);
	Assert.AreEqual('', FManager.GetPublicShard);
end;

{ Override tests }

procedure TCloudShardManagerTest.TestHasDownloadOverride_FalseWhenEmpty;
begin
	Assert.IsFalse(FManager.HasDownloadOverride);
end;

procedure TCloudShardManagerTest.TestHasDownloadOverride_TrueWhenSet;
var
	ManagerWithOverride: ICloudShardManager;
begin
	ManagerWithOverride := TCloudShardManager.Create(TNullLogger.Create, FMockContext, 'https://override.url/');
	Assert.IsTrue(ManagerWithOverride.HasDownloadOverride);
end;

procedure TCloudShardManagerTest.TestHasUploadOverride_FalseWhenEmpty;
begin
	Assert.IsFalse(FManager.HasUploadOverride);
end;

procedure TCloudShardManagerTest.TestHasUploadOverride_TrueWhenSet;
var
	ManagerWithOverride: ICloudShardManager;
begin
	ManagerWithOverride := TCloudShardManager.Create(TNullLogger.Create, FMockContext, '', 'https://override.url/');
	Assert.IsTrue(ManagerWithOverride.HasUploadOverride);
end;

procedure TCloudShardManagerTest.TestGetDownloadShardOverride_ReturnsConfiguredValue;
const
	OverrideUrl = 'https://custom.override.url/';
var
	ManagerWithOverride: ICloudShardManager;
begin
	ManagerWithOverride := TCloudShardManager.Create(TNullLogger.Create, FMockContext, OverrideUrl);
	Assert.AreEqual(OverrideUrl, ManagerWithOverride.GetDownloadShardOverride);
end;

procedure TCloudShardManagerTest.TestGetUploadShardOverride_ReturnsConfiguredValue;
const
	OverrideUrl = 'https://custom.upload.override.url/';
var
	ManagerWithOverride: ICloudShardManager;
begin
	ManagerWithOverride := TCloudShardManager.Create(TNullLogger.Create, FMockContext, '', OverrideUrl);
	Assert.AreEqual(OverrideUrl, ManagerWithOverride.GetUploadShardOverride);
end;

{ ResolveShard tests }

procedure TCloudShardManagerTest.TestResolveShard_Success_ReturnsTrueAndShard;
const
	TestShardUrl = 'https://cloclo1.cloud.mail.ru/get/';
var
	Shard: WideString;
	Success: Boolean;
begin
	FMockContext.SetPostFormResult(True, '{"status":200,"body":{"get":[{"url":"' + TestShardUrl + '"}]}}');
	Shard := '';
	Success := FManager.ResolveShard(Shard, SHARD_TYPE_GET);
	Assert.IsTrue(FMockContext.WasPostFormCalled, 'PostForm should have been called');
	Assert.IsTrue(Success, 'ResolveShard should return true on success');
	Assert.AreEqual(TestShardUrl, Shard, 'Shard URL should be extracted from response');
end;

procedure TCloudShardManagerTest.TestResolveShard_PostFormFails_ReturnsFalse;
var
	Shard: WideString;
	Success: Boolean;
begin
	FMockContext.SetPostFormResult(False, '');
	Shard := '';
	Success := FManager.ResolveShard(Shard, SHARD_TYPE_GET);
	Assert.IsFalse(Success, 'ResolveShard should return false when PostForm fails');
end;

procedure TCloudShardManagerTest.TestResolveShard_InvalidJSON_ReturnsFalse;
var
	Shard: WideString;
	Success: Boolean;
begin
	FMockContext.SetPostFormResult(True, '{"status":500,"error":"internal error"}');
	Shard := '';
	Success := FManager.ResolveShard(Shard, SHARD_TYPE_GET);
	Assert.IsFalse(Success, 'ResolveShard should return false when JSON status is not 200');
end;

{ EnsureDownloadShard tests }

procedure TCloudShardManagerTest.TestEnsureDownloadShard_Cached_ReturnsCachedValue;
const
	CachedShard = 'https://cached.download.shard/';
begin
	FManager.SetDownloadShard(CachedShard);
	Assert.AreEqual(CachedShard, FManager.EnsureDownloadShard, 'Should return cached value without resolution');
end;

procedure TCloudShardManagerTest.TestEnsureDownloadShard_ResolvesViaDispatcher;
const
	DispatcherShard = 'https://dispatcher.download.shard/';
begin
	{OAuth dispatcher returns "URL IP COUNT" format}
	FMockContext.SetGetPageResult(True, DispatcherShard + ' 127.0.0.1 1');
	Assert.AreEqual(DispatcherShard, FManager.EnsureDownloadShard, 'Should resolve via OAuth dispatcher');
end;

procedure TCloudShardManagerTest.TestEnsureDownloadShard_DispatcherFails_ReturnsEmpty;
begin
	FMockContext.SetGetPageResult(False, '');
	Assert.AreEqual('', FManager.EnsureDownloadShard, 'Should return empty when dispatcher fails');
end;

procedure TCloudShardManagerTest.TestEnsureDownloadShard_CachesResolvedValue;
const
	DispatcherShard = 'https://resolved.download.shard/';
begin
	FMockContext.SetGetPageResult(True, DispatcherShard + ' 127.0.0.1 1');
	FManager.EnsureDownloadShard;
	Assert.AreEqual(DispatcherShard, FManager.GetDownloadShard, 'Resolved shard should be cached');
end;

{ EnsureUploadShard tests }

procedure TCloudShardManagerTest.TestEnsureUploadShard_Cached_ReturnsCachedValue;
const
	CachedShard = 'https://cached.upload.shard/';
begin
	FManager.SetUploadShard(CachedShard);
	Assert.AreEqual(CachedShard, FManager.EnsureUploadShard, 'Should return cached value without resolution');
end;

procedure TCloudShardManagerTest.TestEnsureUploadShard_ResolvesViaDispatcher;
const
	DispatcherShard = 'https://dispatcher.upload.shard/';
begin
	FMockContext.SetGetPageResult(True, DispatcherShard + ' 127.0.0.1 1');
	Assert.AreEqual(DispatcherShard, FManager.EnsureUploadShard, 'Should resolve via OAuth dispatcher');
end;

procedure TCloudShardManagerTest.TestEnsureUploadShard_DispatcherFails_ReturnsEmpty;
begin
	FMockContext.SetGetPageResult(False, '');
	Assert.AreEqual('', FManager.EnsureUploadShard, 'Should return empty when dispatcher fails');
end;

procedure TCloudShardManagerTest.TestEnsureUploadShard_CachesResolvedValue;
const
	DispatcherShard = 'https://resolved.upload.shard/';
begin
	FMockContext.SetGetPageResult(True, DispatcherShard + ' 127.0.0.1 1');
	FManager.EnsureUploadShard;
	Assert.AreEqual(DispatcherShard, FManager.GetUploadShard, 'Resolved shard should be cached');
end;

{ TCloudShardManagerWithOverridesTest }

procedure TCloudShardManagerWithOverridesTest.Setup;
begin
	FMockContext := TMockShardContext.Create;
	FManager := TCloudShardManager.Create(TNullLogger.Create, FMockContext, DownloadOverrideUrl, UploadOverrideUrl);
end;

procedure TCloudShardManagerWithOverridesTest.TearDown;
begin
	FManager := nil;
	FMockContext := nil;
end;

procedure TCloudShardManagerWithOverridesTest.TestHasDownloadOverride_ReturnsTrue;
begin
	Assert.IsTrue(FManager.HasDownloadOverride);
end;

procedure TCloudShardManagerWithOverridesTest.TestHasUploadOverride_ReturnsTrue;
begin
	Assert.IsTrue(FManager.HasUploadOverride);
end;

procedure TCloudShardManagerWithOverridesTest.TestGetDownloadOverride_ReturnsConfiguredUrl;
begin
	Assert.AreEqual(DownloadOverrideUrl, FManager.GetDownloadShardOverride);
end;

procedure TCloudShardManagerWithOverridesTest.TestGetUploadOverride_ReturnsConfiguredUrl;
begin
	Assert.AreEqual(UploadOverrideUrl, FManager.GetUploadShardOverride);
end;

procedure TCloudShardManagerWithOverridesTest.TestEnsureDownloadShard_UsesOverride;
begin
	Assert.AreEqual(DownloadOverrideUrl, FManager.EnsureDownloadShard, 'Should use download override when configured');
end;

procedure TCloudShardManagerWithOverridesTest.TestEnsureUploadShard_UsesOverride;
begin
	Assert.AreEqual(UploadOverrideUrl, FManager.EnsureUploadShard, 'Should use upload override when configured');
end;

{ TNullShardManagerTest }

procedure TNullShardManagerTest.Setup;
begin
	FManager := TNullShardManager.Create;
end;

procedure TNullShardManagerTest.TearDown;
begin
	FManager := nil;
end;

procedure TNullShardManagerTest.TestResolveShard_AlwaysReturnsFalse;
var
	Shard: WideString;
	Success: Boolean;
begin
	Shard := '';
	Success := FManager.ResolveShard(Shard, SHARD_TYPE_GET);
	Assert.IsFalse(Success, 'Null manager ResolveShard should always return false');
	Assert.AreEqual('', Shard, 'Null manager should set shard to empty string');
end;

procedure TNullShardManagerTest.TestGetDownloadShard_AlwaysEmpty;
begin
	Assert.AreEqual('', FManager.GetDownloadShard);
end;

procedure TNullShardManagerTest.TestGetUploadShard_AlwaysEmpty;
begin
	Assert.AreEqual('', FManager.GetUploadShard);
end;

procedure TNullShardManagerTest.TestGetPublicShard_AlwaysEmpty;
begin
	Assert.AreEqual('', FManager.GetPublicShard);
end;

procedure TNullShardManagerTest.TestSetDownloadShard_NoOp;
begin
	FManager.SetDownloadShard('https://some.url/');
	Assert.AreEqual('', FManager.GetDownloadShard, 'Null manager should ignore set operations');
end;

procedure TNullShardManagerTest.TestHasDownloadOverride_AlwaysFalse;
begin
	Assert.IsFalse(FManager.HasDownloadOverride);
end;

procedure TNullShardManagerTest.TestHasUploadOverride_AlwaysFalse;
begin
	Assert.IsFalse(FManager.HasUploadOverride);
end;

procedure TNullShardManagerTest.TestEnsureDownloadShard_AlwaysEmpty;
begin
	Assert.AreEqual('', FManager.EnsureDownloadShard, 'Null manager EnsureDownloadShard should return empty');
end;

procedure TNullShardManagerTest.TestEnsureUploadShard_AlwaysEmpty;
begin
	Assert.AreEqual('', FManager.EnsureUploadShard, 'Null manager EnsureUploadShard should return empty');
end;

initialization

TDUnitX.RegisterTestFixture(TCloudShardManagerTest);
TDUnitX.RegisterTestFixture(TCloudShardManagerWithOverridesTest);
TDUnitX.RegisterTestFixture(TNullShardManagerTest);

end.
