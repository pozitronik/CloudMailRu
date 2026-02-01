unit AccountSettingsTest;

interface

uses
	AccountSettings,
	CloudConstants,
	DUnitX.TestFramework;

type

	[TestFixture]
	TAccountSettingsTest = class
	public
		{ GetAccountType tests }
		[Test]
		procedure TestGetAccountTypePrivate;
		[Test]
		procedure TestGetAccountTypePublic;

		{ GetDomain tests }
		[Test]
		procedure TestGetDomainFromEmail;
		[Test]
		procedure TestGetDomainCached;

		{ GetUser tests }
		[Test]
		procedure TestGetUserFromEmail;

		{ Email parsing edge cases }
		[Test]
		procedure TestGetDomainFromEmailWithoutAt;
		[Test]
		procedure TestGetDomainFromEmailAtEnd;
		[Test]
		procedure TestGetUserFromEmailAtStart;

		{ AuthMethod tests }
		[Test]
		procedure TestAuthMethodDefaultValue;
		[Test]
		procedure TestAuthMethodOAuthAppPassword;
		[Test]
		procedure TestUseAppPasswordWithOAuth;
	end;

implementation

{ GetAccountType tests }

procedure TAccountSettingsTest.TestGetAccountTypePrivate;
var
	Settings: TAccountSettings;
begin
	Settings := Default(TAccountSettings);
	Settings.PublicAccount := False;

	Assert.IsTrue(ATPrivate in Settings.AccountType);
	Assert.IsFalse(ATPublic in Settings.AccountType);
end;

procedure TAccountSettingsTest.TestGetAccountTypePublic;
var
	Settings: TAccountSettings;
begin
	Settings := Default(TAccountSettings);
	Settings.PublicAccount := True;

	Assert.IsTrue(ATPublic in Settings.AccountType);
	Assert.IsFalse(ATPrivate in Settings.AccountType);
end;

{ GetDomain tests }

procedure TAccountSettingsTest.TestGetDomainFromEmail;
var
	Settings: TAccountSettings;
begin
	Settings := Default(TAccountSettings);
	Settings.Email := 'user@mail.ru';

	Assert.AreEqual('mail.ru', Settings.Domain);
end;

procedure TAccountSettingsTest.TestGetDomainCached;
var
	Settings: TAccountSettings;
	Domain1, Domain2: WideString;
begin
	{ Domain should be cached after first access }
	Settings := Default(TAccountSettings);
	Settings.Email := 'test@example.com';

	Domain1 := Settings.Domain;
	{ Change email - but cached value should still be used }
	Settings.Email := 'different@other.com';
	Domain2 := Settings.Domain;

	{ Both should return the same (cached) value }
	Assert.AreEqual(Domain1, Domain2);
end;

{ GetUser tests }

procedure TAccountSettingsTest.TestGetUserFromEmail;
var
	Settings: TAccountSettings;
begin
	{ NOTE: There is a BUG in AccountSettings.pas:71 - GetUser returns FDomain instead of FUser }
	{ This test documents the EXPECTED behavior, which currently fails due to the bug }
	Settings := Default(TAccountSettings);
	Settings.Email := 'testuser@mail.ru';

	{ Expected: 'testuser', but bug returns 'mail.ru' }
	{ When the bug is fixed, this test will pass }
	Assert.AreEqual('testuser', Settings.User);
end;

{ Email parsing edge cases }

procedure TAccountSettingsTest.TestGetDomainFromEmailWithoutAt;
var
	Settings: TAccountSettings;
begin
	{ Email without @ should return empty domain }
	Settings := Default(TAccountSettings);
	Settings.Email := 'invalidemail';

	Assert.AreEqual('', Settings.Domain);
end;

procedure TAccountSettingsTest.TestGetDomainFromEmailAtEnd;
var
	Settings: TAccountSettings;
begin
	{ @ at end means empty domain - parsing should not set it }
	Settings := Default(TAccountSettings);
	Settings.Email := 'user@';

	Assert.AreEqual('', Settings.Domain);
end;

procedure TAccountSettingsTest.TestGetUserFromEmailAtStart;
var
	Settings: TAccountSettings;
begin
	{ @ at the start means empty username }
	Settings := Default(TAccountSettings);
	Settings.Email := '@domain.com';

	Assert.AreEqual('', Settings.User);
	Assert.AreEqual('domain.com', Settings.Domain);
end;

{ AuthMethod tests }

procedure TAccountSettingsTest.TestAuthMethodDefaultValue;
var
	Settings: TAccountSettings;
begin
	{ Default value should be 0 (classic web auth) }
	Settings := Default(TAccountSettings);

	Assert.AreEqual(0, Settings.AuthMethod);
end;

procedure TAccountSettingsTest.TestAuthMethodOAuthAppPassword;
var
	Settings: TAccountSettings;
begin
	{ OAuth app password should be stored correctly }
	Settings := Default(TAccountSettings);
	Settings.AuthMethod := CLOUD_AUTH_METHOD_OAUTH_APP;

	Assert.AreEqual(CLOUD_AUTH_METHOD_OAUTH_APP, Settings.AuthMethod);
end;

procedure TAccountSettingsTest.TestUseAppPasswordWithOAuth;
var
	Settings: TAccountSettings;
begin
	{ UseAppPassword flag should work with OAuth }
	Settings := Default(TAccountSettings);
	Settings.AuthMethod := CLOUD_AUTH_METHOD_OAUTH_APP;
	Settings.UseAppPassword := True;

	Assert.IsTrue(Settings.UseAppPassword);
	Assert.AreEqual(CLOUD_AUTH_METHOD_OAUTH_APP, Settings.AuthMethod);
end;

initialization

TDUnitX.RegisterTestFixture(TAccountSettingsTest);

end.
