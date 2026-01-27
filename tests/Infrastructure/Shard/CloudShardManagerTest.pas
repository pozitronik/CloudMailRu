unit CloudShardManagerTest;

interface

uses
	CloudShardManager,
	CloudConstants,
	TCLogger,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework;

type
	{Tests for TCloudShardManager.
	 The shard manager caches shard URLs and handles override configuration.}
	[TestFixture]
	TCloudShardManagerTest = class
	private
		FManager: ICloudShardManager;
		FPostFormCalled: Boolean;
		FPostFormResult: Boolean;
		FPostFormResponse: WideString;
		FParamsResult: WideString;

		function NullPostForm(const URL, Data: WideString; var Answer: WideString): Boolean;
		function NullResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean;
		function NullGetParams: WideString;
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
	end;

	{Tests for TCloudShardManager with overrides configured}
	[TestFixture]
	TCloudShardManagerWithOverridesTest = class
	private
		FManager: ICloudShardManager;
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
	end;

implementation

{ TCloudShardManagerTest }

function TCloudShardManagerTest.NullPostForm(const URL, Data: WideString; var Answer: WideString): Boolean;
begin
	FPostFormCalled := True;
	Answer := FPostFormResponse;
	Result := FPostFormResult;
end;

function TCloudShardManagerTest.NullResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean;
begin
	Result := Pos(WideString('"status":200'), JSON) > 0;
end;

function TCloudShardManagerTest.NullGetParams: WideString;
begin
	Result := FParamsResult;
end;

procedure TCloudShardManagerTest.Setup;
begin
	FPostFormCalled := False;
	FPostFormResult := True;
	FPostFormResponse := '';
	FParamsResult := 'token=test';

	FManager := TCloudShardManager.Create(TNullLogger.Create,
		function(const URL, Data: WideString; var Answer: WideString): Boolean
		begin
			Result := Self.NullPostForm(URL, Data, Answer);
		end,
		function(const JSON, ErrorPrefix: WideString): Boolean
		begin
			Result := Self.NullResultToBoolean(JSON, ErrorPrefix);
		end,
		function: WideString
		begin
			Result := Self.NullGetParams;
		end);
end;

procedure TCloudShardManagerTest.TearDown;
begin
	FManager := nil;
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
	ManagerWithOverride := TCloudShardManager.Create(TNullLogger.Create,
		function(const URL, Data: WideString; var Answer: WideString): Boolean begin Result := True; end,
		function(const JSON, ErrorPrefix: WideString): Boolean begin Result := True; end,
		function: WideString begin Result := ''; end,
		'https://override.url/');
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
	ManagerWithOverride := TCloudShardManager.Create(TNullLogger.Create,
		function(const URL, Data: WideString; var Answer: WideString): Boolean begin Result := True; end,
		function(const JSON, ErrorPrefix: WideString): Boolean begin Result := True; end,
		function: WideString begin Result := ''; end,
		'', 'https://override.url/');
	Assert.IsTrue(ManagerWithOverride.HasUploadOverride);
end;

procedure TCloudShardManagerTest.TestGetDownloadShardOverride_ReturnsConfiguredValue;
const
	OverrideUrl = 'https://custom.override.url/';
var
	ManagerWithOverride: ICloudShardManager;
begin
	ManagerWithOverride := TCloudShardManager.Create(TNullLogger.Create,
		function(const URL, Data: WideString; var Answer: WideString): Boolean begin Result := True; end,
		function(const JSON, ErrorPrefix: WideString): Boolean begin Result := True; end,
		function: WideString begin Result := ''; end,
		OverrideUrl);
	Assert.AreEqual(OverrideUrl, ManagerWithOverride.GetDownloadShardOverride);
end;

procedure TCloudShardManagerTest.TestGetUploadShardOverride_ReturnsConfiguredValue;
const
	OverrideUrl = 'https://custom.upload.override.url/';
var
	ManagerWithOverride: ICloudShardManager;
begin
	ManagerWithOverride := TCloudShardManager.Create(TNullLogger.Create,
		function(const URL, Data: WideString; var Answer: WideString): Boolean begin Result := True; end,
		function(const JSON, ErrorPrefix: WideString): Boolean begin Result := True; end,
		function: WideString begin Result := ''; end,
		'', OverrideUrl);
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
	FPostFormResult := True;
	FPostFormResponse := '{"status":200,"body":{"get":[{"url":"' + TestShardUrl + '"}]}}';
	Shard := '';
	Success := FManager.ResolveShard(Shard, SHARD_TYPE_GET);
	Assert.IsTrue(FPostFormCalled, 'PostForm should have been called');
	Assert.IsTrue(Success, 'ResolveShard should return true on success');
	Assert.AreEqual(TestShardUrl, Shard, 'Shard URL should be extracted from response');
end;

procedure TCloudShardManagerTest.TestResolveShard_PostFormFails_ReturnsFalse;
var
	Shard: WideString;
	Success: Boolean;
begin
	FPostFormResult := False;
	Shard := '';
	Success := FManager.ResolveShard(Shard, SHARD_TYPE_GET);
	Assert.IsFalse(Success, 'ResolveShard should return false when PostForm fails');
end;

procedure TCloudShardManagerTest.TestResolveShard_InvalidJSON_ReturnsFalse;
var
	Shard: WideString;
	Success: Boolean;
begin
	FPostFormResult := True;
	FPostFormResponse := '{"status":500,"error":"internal error"}';
	Shard := '';
	Success := FManager.ResolveShard(Shard, SHARD_TYPE_GET);
	Assert.IsFalse(Success, 'ResolveShard should return false when JSON status is not 200');
end;

{ TCloudShardManagerWithOverridesTest }

procedure TCloudShardManagerWithOverridesTest.Setup;
begin
	FManager := TCloudShardManager.Create(TNullLogger.Create,
		function(const URL, Data: WideString; var Answer: WideString): Boolean begin Result := True; end,
		function(const JSON, ErrorPrefix: WideString): Boolean begin Result := True; end,
		function: WideString begin Result := ''; end,
		DownloadOverrideUrl, UploadOverrideUrl);
end;

procedure TCloudShardManagerWithOverridesTest.TearDown;
begin
	FManager := nil;
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

initialization

TDUnitX.RegisterTestFixture(TCloudShardManagerTest);
TDUnitX.RegisterTestFixture(TCloudShardManagerWithOverridesTest);
TDUnitX.RegisterTestFixture(TNullShardManagerTest);

end.
