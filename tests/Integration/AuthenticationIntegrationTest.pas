unit AuthenticationIntegrationTest;

{Integration tests for authentication operations against live cloud.mail.ru API.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	TAuthenticationIntegrationTest = class(TIntegrationTestBase)
	public
		[Test]
		procedure TestLogin_ValidCredentials_Succeeds;

		[Test]
		procedure TestLogin_InvalidPassword_Fails;

		[Test]
		procedure TestLogin_TokenRefresh_Succeeds;

		[Test]
		procedure TestLogin_PublicAccount_Succeeds;
	end;

implementation

uses
	System.SysUtils,
	CloudMailRu,
	CloudSettings,
	AccountSettings,
	AuthStrategy,
	OAuthAppAuthStrategy,
	WindowsFileSystem,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	CloudConstants;

{TAuthenticationIntegrationTest}

procedure TAuthenticationIntegrationTest.TestLogin_ValidCredentials_Succeeds;
begin
	{Already logged in during SetupFixture, verify state}
	Assert.IsNotNull(FPrimaryCloud, 'Primary cloud should be initialized');
	Assert.IsTrue(FPrimaryCloud.Login, 'Login with valid credentials should succeed');
end;

procedure TAuthenticationIntegrationTest.TestLogin_InvalidPassword_Fails;
var
	Cloud: TCloudMailRu;
	Settings: TCloudSettings;
begin
	Settings := CreateCloudSettings(
		FConfig.PrimaryEmail,
		'invalid_password_12345',
		FConfig.PrimaryUseAppPassword);

	Cloud := TCloudMailRu.Create(
		Settings,
		nil,
		TOAuthAppAuthStrategy.Create,
		TWindowsFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		nil);
	try
		Assert.IsFalse(Cloud.Login, 'Login with invalid password should fail');
	finally
		Cloud.Free;
	end;
end;

procedure TAuthenticationIntegrationTest.TestLogin_TokenRefresh_Succeeds;
var
	Cloud: TCloudMailRu;
	LoginResult1, LoginResult2: Boolean;
begin
	{Create a new cloud instance and login twice to verify token handling}
	Cloud := CreatePrimaryCloud;
	try
		LoginResult1 := Cloud.Login;
		Assert.IsTrue(LoginResult1, 'First login should succeed');

		{Login again - should work with existing or refreshed token}
		LoginResult2 := Cloud.Login;
		Assert.IsTrue(LoginResult2, 'Second login should succeed (token refresh path)');
	finally
		Cloud.Free;
	end;
end;

procedure TAuthenticationIntegrationTest.TestLogin_PublicAccount_Succeeds;
var
	Cloud: TCloudMailRu;
begin
	RequirePublicUrl;

	Cloud := CreatePublicCloud;
	try
		Assert.IsTrue(Cloud.Login, 'Public account login should succeed');
		Assert.IsTrue(Cloud.IsPublicAccount, 'Should be marked as public account');
	finally
		Cloud.Free;
	end;
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TAuthenticationIntegrationTest);

end.
