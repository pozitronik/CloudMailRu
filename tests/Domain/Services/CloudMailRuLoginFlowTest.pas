unit CloudMailRuLoginFlowTest;

{Integration tests for TCloudMailRu.Login using MockAuthStrategy.
 Tests regular login, shared login, authentication failure, and GetUserSpace.}

interface

uses
	CloudMailRu,
	CloudSettings,
	CMRConstants,
	CMROAuth,
	CMRSpace,
	PLUGIN_TYPES,
	SETTINGS_CONSTANTS,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	IAuthStrategyInterface,
	WindowsFileSystem,
	CloudHTTP,
	HTTPManager,
	CloudShardManager,
	MockCloudHTTP,
	MockHTTPManager,
	MockAuthStrategy,
	TestHelper,
	System.SysUtils,
	DUnitX.TestFramework;

type
	{Testable subclass exposing protected fields for verification}
	TTestableLoginCloud = class(TCloudMailRu)
	public
		function GetAuthToken: WideString;
		function GetOAuthToken: TCMROAuth;
		function GetUnitedParams: WideString;
		function GetPublicShard: WideString;
	end;

	[TestFixture]
	TCloudMailRuLoginFlowTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TTestableLoginCloud;
		FSettings: TCloudSettings;

		function CreateRegularCloud(AuthStrategy: IAuthStrategy): TTestableLoginCloud;
		function CreatePublicCloud(const PublicUrl: WideString): TTestableLoginCloud;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{LoginRegular tests}
		[Test]
		procedure TestLogin_RegularAccount_Success;
		[Test]
		procedure TestLogin_RegularAccount_AuthFails_ReturnsFalse;
		[Test]
		procedure TestLogin_RegularAccount_SetsAuthToken;
		[Test]
		procedure TestLogin_RegularAccount_SetsOAuthToken;
		[Test]
		procedure TestLogin_RegularAccount_SetsUnitedParams;
		[Test]
		procedure TestLogin_RegularAccount_CallsAuthStrategy;

		{LoginShared tests}
		[Test]
		procedure TestLogin_PublicAccount_Success;
		[Test]
		procedure TestLogin_PublicAccount_HTTPFails_ReturnsFalse;
		[Test]
		procedure TestLogin_PublicAccount_ExtractsPublicShard;
		[Test]
		procedure TestLogin_PublicAccount_InvalidPage_ReturnsFalse;

		{GetUserSpace tests}
		[Test]
		procedure TestGetUserSpace_Success_ParsesResponse;
		[Test]
		procedure TestGetUserSpace_ParsesTotal;
		[Test]
		procedure TestGetUserSpace_ParsesUsed;
		[Test]
		procedure TestGetUserSpace_ParsesOverquota;
		[Test]
		procedure TestGetUserSpace_HTTPFails_ReturnsFalse;
		[Test]
		procedure TestGetUserSpace_PublicAccount_Works;
	end;

implementation

const
	JSON_USER_SPACE = '{"email":"test@mail.ru","body":{"bytes_total":10737418240,"bytes_used":5368709120,"overquota":false},"status":200}';
	JSON_USER_SPACE_OVERQUOTA = '{"email":"test@mail.ru","body":{"bytes_total":10737418240,"bytes_used":12884901888,"overquota":true},"status":200}';
	JSON_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';

	{Public page with shard URL embedded - matches extractPublicShard parsing}
	PUBLIC_PAGE_WITH_SHARD = '"weblink_get":[{"count":1,"url":"https://cloclo123.datacloudmail.ru/weblink/"}]';
	PUBLIC_PAGE_INVALID = '<html><body>Invalid page without shard</body></html>';

{TTestableLoginCloud}

function TTestableLoginCloud.GetAuthToken: WideString;
begin
	Result := FAuthToken;
end;

function TTestableLoginCloud.GetOAuthToken: TCMROAuth;
begin
	Result := FOAuthToken;
end;

function TTestableLoginCloud.GetUnitedParams: WideString;
begin
	Result := FUnitedParams;
end;

function TTestableLoginCloud.GetPublicShard: WideString;
begin
	Result := FShardManager.GetPublicShard;
end;

{TCloudMailRuLoginFlowTest}

procedure TCloudMailRuLoginFlowTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
end;

procedure TCloudMailRuLoginFlowTest.TearDown;
begin
	FreeAndNil(FCloud);
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCloudMailRuLoginFlowTest.CreateRegularCloud(AuthStrategy: IAuthStrategy): TTestableLoginCloud;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.Email := 'test@mail.ru';
	FSettings.AccountSettings.Password := 'password';
	FSettings.AccountSettings.PublicAccount := False;

	Result := TTestableLoginCloud.Create(
		FSettings,
		FMockHTTPManager,
		AuthStrategy,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create);
end;

function TCloudMailRuLoginFlowTest.CreatePublicCloud(const PublicUrl: WideString): TTestableLoginCloud;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.PublicAccount := True;
	FSettings.AccountSettings.PublicUrl := PublicUrl;

	Result := TTestableLoginCloud.Create(
		FSettings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create);
end;

{LoginRegular tests}

procedure TCloudMailRuLoginFlowTest.TestLogin_RegularAccount_Success;
var
	AuthStrategy: TMockAuthStrategy;
begin
	AuthStrategy := TMockAuthStrategy.CreateSuccess('test_auth_token');
	FCloud := CreateRegularCloud(AuthStrategy);

	{Setup user space response for LogUserSpaceInfo call after login}
	FMockHTTP.SetResponse(API_USER_SPACE, True, JSON_USER_SPACE);

	var Result := FCloud.Login;

	Assert.IsTrue(Result, 'Login should succeed with valid auth strategy');
end;

procedure TCloudMailRuLoginFlowTest.TestLogin_RegularAccount_AuthFails_ReturnsFalse;
var
	AuthStrategy: TMockAuthStrategy;
begin
	AuthStrategy := TMockAuthStrategy.CreateFailure('Invalid credentials');
	FCloud := CreateRegularCloud(AuthStrategy);

	var Result := FCloud.Login;

	Assert.IsFalse(Result, 'Login should fail when auth strategy fails');
end;

procedure TCloudMailRuLoginFlowTest.TestLogin_RegularAccount_SetsAuthToken;
var
	AuthStrategy: TMockAuthStrategy;
begin
	AuthStrategy := TMockAuthStrategy.CreateSuccess('expected_auth_token');
	FCloud := CreateRegularCloud(AuthStrategy);
	FMockHTTP.SetResponse(API_USER_SPACE, True, JSON_USER_SPACE);

	FCloud.Login;

	Assert.AreEqual(String('expected_auth_token'), String(FCloud.GetAuthToken), 'AuthToken should be set from auth result');
end;

procedure TCloudMailRuLoginFlowTest.TestLogin_RegularAccount_SetsOAuthToken;
var
	AuthStrategy: TMockAuthStrategy;
	OAuthToken: TCMROAuth;
begin
	AuthStrategy := TMockAuthStrategy.CreateOAuthSuccess('access_123', 'refresh_456', 7200);
	FCloud := CreateRegularCloud(AuthStrategy);
	FMockHTTP.SetResponse(API_USER_SPACE, True, JSON_USER_SPACE);

	FCloud.Login;

	OAuthToken := FCloud.GetOAuthToken;
	Assert.AreEqual(String('access_123'), String(OAuthToken.access_token), 'OAuth access_token should be set');
	Assert.AreEqual(String('refresh_456'), String(OAuthToken.refresh_token), 'OAuth refresh_token should be set');
	Assert.AreEqual(7200, OAuthToken.expires_in, 'OAuth expires_in should be set');
end;

procedure TCloudMailRuLoginFlowTest.TestLogin_RegularAccount_SetsUnitedParams;
var
	AuthStrategy: TMockAuthStrategy;
begin
	AuthStrategy := TMockAuthStrategy.CreateSuccess('token');
	AuthStrategy.SetUnitedParams('api=2&token=csrf_token');
	FCloud := CreateRegularCloud(AuthStrategy);
	FMockHTTP.SetResponse(API_USER_SPACE, True, JSON_USER_SPACE);

	FCloud.Login;

	Assert.AreEqual(String('api=2&token=csrf_token'), String(FCloud.GetUnitedParams), 'UnitedParams should be set from auth result');
end;

procedure TCloudMailRuLoginFlowTest.TestLogin_RegularAccount_CallsAuthStrategy;
var
	AuthStrategy: TMockAuthStrategy;
begin
	FSettings := Default(TCloudSettings);
	FSettings.AccountSettings.Email := 'test@mail.ru';
	FSettings.AccountSettings.Password := 'secret';
	FSettings.AccountSettings.PublicAccount := False;

	AuthStrategy := TMockAuthStrategy.CreateSuccess('token');

	FCloud := TTestableLoginCloud.Create(
		FSettings,
		FMockHTTPManager,
		AuthStrategy,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create);

	FMockHTTP.SetResponse(API_USER_SPACE, True, JSON_USER_SPACE);

	FCloud.Login;

	Assert.IsTrue(AuthStrategy.AuthenticateCalled, 'Auth strategy should be called');
	Assert.AreEqual(String('test@mail.ru'), String(AuthStrategy.LastCredentials.Email), 'Should pass email to strategy');
	Assert.AreEqual(String('secret'), String(AuthStrategy.LastCredentials.Password), 'Should pass password to strategy');
end;

{LoginShared tests}

procedure TCloudMailRuLoginFlowTest.TestLogin_PublicAccount_Success;
begin
	FCloud := CreatePublicCloud('https://cloud.mail.ru/public/abc123');
	FMockHTTP.SetResponse('https://cloud.mail.ru/public/abc123', True, PUBLIC_PAGE_WITH_SHARD);

	var Result := FCloud.Login;

	Assert.IsTrue(Result, 'Public account login should succeed when shard is extracted');
end;

procedure TCloudMailRuLoginFlowTest.TestLogin_PublicAccount_HTTPFails_ReturnsFalse;
begin
	FCloud := CreatePublicCloud('https://cloud.mail.ru/public/invalid');
	FMockHTTP.SetResponse('https://cloud.mail.ru/public/invalid', False, '');

	var Result := FCloud.Login;

	Assert.IsFalse(Result, 'Public account login should fail when HTTP fails');
end;

procedure TCloudMailRuLoginFlowTest.TestLogin_PublicAccount_ExtractsPublicShard;
begin
	FCloud := CreatePublicCloud('https://cloud.mail.ru/public/abc123');
	FMockHTTP.SetResponse('https://cloud.mail.ru/public/abc123', True, PUBLIC_PAGE_WITH_SHARD);

	FCloud.Login;

	Assert.AreEqual(String('https://cloclo123.datacloudmail.ru/weblink/'), String(FCloud.GetPublicShard),
		'Should extract public shard from page');
end;

procedure TCloudMailRuLoginFlowTest.TestLogin_PublicAccount_InvalidPage_ReturnsFalse;
begin
	FCloud := CreatePublicCloud('https://cloud.mail.ru/public/abc123');
	FMockHTTP.SetResponse('https://cloud.mail.ru/public/abc123', True, PUBLIC_PAGE_INVALID);

	var Result := FCloud.Login;

	Assert.IsFalse(Result, 'Public account login should fail when shard cannot be extracted');
end;

{GetUserSpace tests}

procedure TCloudMailRuLoginFlowTest.TestGetUserSpace_Success_ParsesResponse;
var
	SpaceInfo: TCMRSpace;
begin
	FCloud := CreateRegularCloud(TNullAuthStrategy.Create);
	FMockHTTP.SetResponse(API_USER_SPACE, True, JSON_USER_SPACE);

	var Result := FCloud.GetUserSpace(SpaceInfo);

	Assert.IsTrue(Result, 'GetUserSpace should succeed');
end;

procedure TCloudMailRuLoginFlowTest.TestGetUserSpace_ParsesTotal;
var
	SpaceInfo: TCMRSpace;
begin
	FCloud := CreateRegularCloud(TNullAuthStrategy.Create);
	FMockHTTP.SetResponse(API_USER_SPACE, True, JSON_USER_SPACE);

	FCloud.GetUserSpace(SpaceInfo);

	Assert.AreEqual(Int64(10737418240), Int64(SpaceInfo.total), 'Should parse bytes_total (10GB)');
end;

procedure TCloudMailRuLoginFlowTest.TestGetUserSpace_ParsesUsed;
var
	SpaceInfo: TCMRSpace;
begin
	FCloud := CreateRegularCloud(TNullAuthStrategy.Create);
	FMockHTTP.SetResponse(API_USER_SPACE, True, JSON_USER_SPACE);

	FCloud.GetUserSpace(SpaceInfo);

	Assert.AreEqual(Int64(5368709120), Int64(SpaceInfo.used), 'Should parse bytes_used (5GB)');
end;

procedure TCloudMailRuLoginFlowTest.TestGetUserSpace_ParsesOverquota;
var
	SpaceInfo: TCMRSpace;
begin
	FCloud := CreateRegularCloud(TNullAuthStrategy.Create);
	FMockHTTP.SetResponse(API_USER_SPACE, True, JSON_USER_SPACE_OVERQUOTA);

	FCloud.GetUserSpace(SpaceInfo);

	Assert.IsTrue(SpaceInfo.overquota, 'Should parse overquota flag');
end;

procedure TCloudMailRuLoginFlowTest.TestGetUserSpace_HTTPFails_ReturnsFalse;
var
	SpaceInfo: TCMRSpace;
begin
	FCloud := CreateRegularCloud(TNullAuthStrategy.Create);
	FMockHTTP.SetResponse(API_USER_SPACE, False, '');

	var Result := FCloud.GetUserSpace(SpaceInfo);

	Assert.IsFalse(Result, 'GetUserSpace should fail when HTTP fails');
end;

procedure TCloudMailRuLoginFlowTest.TestGetUserSpace_PublicAccount_Works;
var
	SpaceInfo: TCMRSpace;
begin
	FCloud := CreatePublicCloud('https://cloud.mail.ru/public/abc');
	FMockHTTP.SetResponse(API_USER_SPACE, True, JSON_USER_SPACE);

	var Result := FCloud.GetUserSpace(SpaceInfo);

	Assert.IsTrue(Result, 'GetUserSpace should work for public accounts');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_USER_SPACE), 'Should call user space API');
end;

initialization
	TDUnitX.RegisterTestFixture(TCloudMailRuLoginFlowTest);

end.
