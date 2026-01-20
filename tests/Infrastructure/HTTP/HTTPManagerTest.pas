unit HTTPManagerTest;

interface

uses
	HTTPManager,
	ConnectionSettings,
	DUnitX.TestFramework;

type
	[TestFixture]
	TNullHTTPManagerTest = class
	public
		[Test]
		{Verifies TNullHTTPManager can be assigned to IHTTPManager variable}
		procedure TestImplementsIHTTPManager;

		[Test]
		{Verifies Get returns nil}
		procedure TestGetReturnsNil;

		[Test]
		{Verifies GetConnectionSettings returns default settings}
		procedure TestGetConnectionSettingsReturnsDefault;

		[Test]
		{Verifies SetProxyPassword completes without exception}
		procedure TestSetProxyPasswordNoException;

		[Test]
		{Verifies SetProxyPassword stores the password in ConnectionSettings}
		procedure TestSetProxyPasswordStoresValue;

		[Test]
		{Verifies multiple calls work correctly}
		procedure TestMultipleCalls;
	end;

implementation

procedure TNullHTTPManagerTest.TestImplementsIHTTPManager;
var
	Manager: IHTTPManager;
begin
	Manager := TNullHTTPManager.Create;
	Assert.IsNotNull(Manager);
end;

procedure TNullHTTPManagerTest.TestGetReturnsNil;
var
	Manager: IHTTPManager;
begin
	Manager := TNullHTTPManager.Create;
	Assert.IsNull(Manager.Get(1234));
end;

procedure TNullHTTPManagerTest.TestGetConnectionSettingsReturnsDefault;
var
	Manager: IHTTPManager;
	Settings: TConnectionSettings;
begin
	Manager := TNullHTTPManager.Create;
	Settings := Manager.GetConnectionSettings;
	{Default settings should have zero values}
	Assert.AreEqual(0, Settings.SocketTimeout);
end;

procedure TNullHTTPManagerTest.TestSetProxyPasswordNoException;
var
	Manager: IHTTPManager;
begin
	Manager := TNullHTTPManager.Create;
	Manager.SetProxyPassword('test_password');
	Assert.Pass('SetProxyPassword completed without exception');
end;

procedure TNullHTTPManagerTest.TestSetProxyPasswordStoresValue;
var
	Manager: IHTTPManager;
begin
	Manager := TNullHTTPManager.Create;
	Manager.SetProxyPassword('my_proxy_pwd');
	Assert.AreEqual('my_proxy_pwd', Manager.GetConnectionSettings.ProxySettings.Password);
end;

procedure TNullHTTPManagerTest.TestMultipleCalls;
var
	Manager: IHTTPManager;
begin
	Manager := TNullHTTPManager.Create;

	Assert.IsNull(Manager.Get(100));
	Assert.IsNull(Manager.Get(200));

	Manager.SetProxyPassword('pwd1');
	Manager.SetProxyPassword('pwd2');
	Assert.AreEqual('pwd2', Manager.ConnectionSettings.ProxySettings.Password);

	Assert.Pass('Multiple calls completed without exception');
end;

initialization

TDUnitX.RegisterTestFixture(TNullHTTPManagerTest);

end.
