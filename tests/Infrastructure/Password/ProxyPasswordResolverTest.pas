unit ProxyPasswordResolverTest;

interface

uses
	ProxyPasswordResolver,
	HTTPManager,
	PasswordManager,
	PasswordUIProvider,
	PluginSettingsManager,
	PluginSettings,
	ConnectionSettings,
	ProxySettings,
	StreamingSettings,
	TCHandler,
	Logger,
	Winapi.Windows,
	System.Classes,
	System.Generics.Collections,
	DUnitX.TestFramework;

type
	{Mock password manager returning configurable GetPassword results}
	TMockPasswordManagerForProxy = class(TInterfacedObject, IPasswordManager)
	private
		FGetPasswordResult: Integer;
		FGetPasswordValue: WideString;
		FSetPasswordResult: Integer;
		FSetPasswordCalled: Boolean;
	public
		constructor Create(GetPasswordResult: Integer; const PasswordValue: WideString = '');
		function GetPassword(Key: WideString; var Password: WideString): Integer;
		function SetPassword(Key, Password: WideString): Integer;
		property SetPasswordResult: Integer read FSetPasswordResult write FSetPasswordResult;
		property SetPasswordCalled: Boolean read FSetPasswordCalled;
	end;

	{Mock password UI returning configurable AskPassword results}
	TMockPasswordUIForProxy = class(TInterfacedObject, IPasswordUIProvider)
	private
		FAskPasswordResult: Integer;
		FAskPasswordValue: WideString;
	public
		constructor Create(AskPasswordResult: Integer; const PasswordValue: WideString = '');
		function AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
		function AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
	end;

	{Mock plugin settings manager for proxy tests}
	TMockPluginSettingsManagerForProxy = class(TInterfacedObject, IPluginSettingsManager)
	private
		FPluginSettings: TPluginSettings;
		FSwitchProxyPasswordStorageCalled: Boolean;
	public
		constructor Create;
		function GetSettings: TPluginSettings;
		procedure SetSettings(Value: TPluginSettings);
		procedure Save;
		procedure SwitchProxyPasswordStorage;
		function GetStreamingSettings(const FileName: WideString): TStreamingSettings;
		procedure SetStreamingSettings(const FileName: WideString; StreamingSettings: TStreamingSettings);
		procedure GetStreamingExtensionsList(ExtensionsList: TStrings);
		procedure RemoveStreamingExtension(const Extension: WideString);
		function GetAccountsIniFilePath: WideString;
		procedure Refresh;
		property SwitchProxyPasswordStorageCalled: Boolean read FSwitchProxyPasswordStorageCalled;
	end;

	[TestFixture]
	TProxyPasswordResolverTest = class
	public
		[Test]
		{Proxy password from TC password manager -> sets HTTPManager proxy password}
		procedure TestResolvePassword_TCManagerFound;

		[Test]
		{Proxy with empty password, user enters, SetPassword succeeds -> SwitchProxyPasswordStorage called}
		procedure TestResolvePassword_EmptyPwd_UserEnters_SaveOK;

		[Test]
		{Proxy with empty password, user enters, SetPassword fails -> SwitchProxyPasswordStorage not called}
		procedure TestResolvePassword_EmptyPwd_UserEnters_SaveFails;

		[Test]
		{Non-public account with proxy configured, password already in INI}
		procedure TestResolvePassword_PasswordFromINI;

		[Test]
		{No proxy configured -> returns True immediately}
		procedure TestResolvePassword_NoProxy_ReturnsTrue;
	end;

	[TestFixture]
	TNullProxyPasswordResolverTest = class
	public
		[Test]
		{Null resolver always returns True}
		procedure TestNullResolverReturnsTrue;
	end;

implementation

uses
	CloudConstants,
	SettingsConstants,
	WFXTypes,
	MockHTTPManager,
	MockCloudHTTP,
	CloudHTTP,
	Vcl.Controls,
	System.SysUtils;

{TMockPasswordManagerForProxy}

constructor TMockPasswordManagerForProxy.Create(GetPasswordResult: Integer; const PasswordValue: WideString);
begin
	inherited Create;
	FGetPasswordResult := GetPasswordResult;
	FGetPasswordValue := PasswordValue;
	FSetPasswordResult := FS_FILE_OK;
	FSetPasswordCalled := False;
end;

function TMockPasswordManagerForProxy.GetPassword(Key: WideString; var Password: WideString): Integer;
begin
	Password := FGetPasswordValue;
	Result := FGetPasswordResult;
end;

function TMockPasswordManagerForProxy.SetPassword(Key, Password: WideString): Integer;
begin
	FSetPasswordCalled := True;
	Result := FSetPasswordResult;
end;

{TMockPasswordUIForProxy}

constructor TMockPasswordUIForProxy.Create(AskPasswordResult: Integer; const PasswordValue: WideString);
begin
	inherited Create;
	FAskPasswordResult := AskPasswordResult;
	FAskPasswordValue := PasswordValue;
end;

function TMockPasswordUIForProxy.AskPassword(Title, Text: WideString; var Password: WideString; var UseTCPwdMngr: Boolean; DisablePWDManagerCB: Boolean; ParentWindow: HWND): Integer;
begin
	Password := FAskPasswordValue;
	Result := FAskPasswordResult;
end;

function TMockPasswordUIForProxy.AskAction(Title, Text: WideString; ActionsList: TDictionary<Int32, WideString>; ParentWindow: HWND): Integer;
begin
	Result := mrCancel;
end;

{TMockPluginSettingsManagerForProxy}

constructor TMockPluginSettingsManagerForProxy.Create;
begin
	inherited Create;
	FSwitchProxyPasswordStorageCalled := False;
	FPluginSettings := Default(TPluginSettings);
end;

function TMockPluginSettingsManagerForProxy.GetSettings: TPluginSettings;
begin
	Result := FPluginSettings;
end;

procedure TMockPluginSettingsManagerForProxy.SetSettings(Value: TPluginSettings);
begin
	FPluginSettings := Value;
end;

procedure TMockPluginSettingsManagerForProxy.Save;
begin
end;

procedure TMockPluginSettingsManagerForProxy.SwitchProxyPasswordStorage;
begin
	FSwitchProxyPasswordStorageCalled := True;
end;

function TMockPluginSettingsManagerForProxy.GetStreamingSettings(const FileName: WideString): TStreamingSettings;
begin
	Result := Default(TStreamingSettings);
end;

procedure TMockPluginSettingsManagerForProxy.SetStreamingSettings(const FileName: WideString; StreamingSettings: TStreamingSettings);
begin
end;

procedure TMockPluginSettingsManagerForProxy.GetStreamingExtensionsList(ExtensionsList: TStrings);
begin
	ExtensionsList.Clear;
end;

procedure TMockPluginSettingsManagerForProxy.RemoveStreamingExtension(const Extension: WideString);
begin
end;

function TMockPluginSettingsManagerForProxy.GetAccountsIniFilePath: WideString;
begin
	Result := EmptyWideStr;
end;

procedure TMockPluginSettingsManagerForProxy.Refresh;
begin
end;

{TProxyPasswordResolverTest}

procedure TProxyPasswordResolverTest.TestResolvePassword_TCManagerFound;
var
	Resolver: IProxyPasswordResolver;
	MockHTTPMgr: TMockHTTPManager;
	PasswordMgr: TMockPasswordManagerForProxy;
	ProxyConnSettings: TConnectionSettings;
begin
	{Proxy with UseTCPasswordManager=True and password found in TC store.
	 ResolvePassword retrieves password and sets it on HTTPManager.}
	MockHTTPMgr := TMockHTTPManager.Create(TNullCloudHTTP.Create);
	ProxyConnSettings := Default(TConnectionSettings);
	ProxyConnSettings.ProxySettings.ProxyType := ProxyHTTP;
	ProxyConnSettings.ProxySettings.User := 'user';
	ProxyConnSettings.ProxySettings.Password := '';
	ProxyConnSettings.ProxySettings.UseTCPasswordManager := True;
	MockHTTPMgr.SetConnectionSettings(ProxyConnSettings);

	PasswordMgr := TMockPasswordManagerForProxy.Create(FS_FILE_OK, 'tc-proxy-pass');

	Resolver := TProxyPasswordResolver.Create(
		MockHTTPMgr, PasswordMgr, TNullPasswordUIProvider.Create,
		TMockPluginSettingsManagerForProxy.Create,
		TNullTCHandler.Create, TNullLogger.Create);

	Assert.IsTrue(Resolver.ResolvePassword, 'ResolvePassword should succeed with TC password');
	Assert.AreEqual('tc-proxy-pass', string(MockHTTPMgr.GetConnectionSettings.ProxySettings.Password),
		'Proxy password should be set on HTTPManager');
end;

procedure TProxyPasswordResolverTest.TestResolvePassword_EmptyPwd_UserEnters_SaveOK;
var
	Resolver: IProxyPasswordResolver;
	MockHTTPMgr: TMockHTTPManager;
	PasswordMgr: TMockPasswordManagerForProxy;
	PasswordUI: TMockPasswordUIForProxy;
	PluginSettingsMgr: TMockPluginSettingsManagerForProxy;
	ProxyConnSettings: TConnectionSettings;
begin
	{Proxy with empty password, not using TC password manager.
	 User enters password via AskPassword, SetPassword succeeds -> SwitchProxyPasswordStorage called.}
	MockHTTPMgr := TMockHTTPManager.Create(TNullCloudHTTP.Create);
	ProxyConnSettings := Default(TConnectionSettings);
	ProxyConnSettings.ProxySettings.ProxyType := ProxyHTTP;
	ProxyConnSettings.ProxySettings.User := 'user';
	ProxyConnSettings.ProxySettings.Password := '';
	ProxyConnSettings.ProxySettings.UseTCPasswordManager := False;
	MockHTTPMgr.SetConnectionSettings(ProxyConnSettings);

	PasswordMgr := TMockPasswordManagerForProxy.Create(FS_FILE_NOTFOUND, '');
	PasswordMgr.SetPasswordResult := FS_FILE_OK;

	PasswordUI := TMockPasswordUIForProxy.Create(mrOk, 'user-proxy-pass');

	PluginSettingsMgr := TMockPluginSettingsManagerForProxy.Create;

	Resolver := TProxyPasswordResolver.Create(
		MockHTTPMgr, PasswordMgr, PasswordUI,
		PluginSettingsMgr,
		TNullTCHandler.Create, TNullLogger.Create);

	Assert.IsTrue(Resolver.ResolvePassword, 'ResolvePassword should succeed after user enters password');
	Assert.IsTrue(PasswordMgr.SetPasswordCalled, 'SetPassword should have been called');
	Assert.IsTrue(PluginSettingsMgr.SwitchProxyPasswordStorageCalled,
		'SwitchProxyPasswordStorage should be called when password saved to TC store');
end;

procedure TProxyPasswordResolverTest.TestResolvePassword_EmptyPwd_UserEnters_SaveFails;
var
	Resolver: IProxyPasswordResolver;
	MockHTTPMgr: TMockHTTPManager;
	PasswordMgr: TMockPasswordManagerForProxy;
	PasswordUI: TMockPasswordUIForProxy;
	PluginSettingsMgr: TMockPluginSettingsManagerForProxy;
	ProxyConnSettings: TConnectionSettings;
begin
	{Same as SaveOK but SetPassword returns FS_FILE_WRITEERROR -> SwitchProxyPasswordStorage not called.}
	MockHTTPMgr := TMockHTTPManager.Create(TNullCloudHTTP.Create);
	ProxyConnSettings := Default(TConnectionSettings);
	ProxyConnSettings.ProxySettings.ProxyType := ProxyHTTP;
	ProxyConnSettings.ProxySettings.User := 'user';
	ProxyConnSettings.ProxySettings.Password := '';
	ProxyConnSettings.ProxySettings.UseTCPasswordManager := False;
	MockHTTPMgr.SetConnectionSettings(ProxyConnSettings);

	PasswordMgr := TMockPasswordManagerForProxy.Create(FS_FILE_NOTFOUND, '');
	PasswordMgr.SetPasswordResult := FS_FILE_WRITEERROR;

	PasswordUI := TMockPasswordUIForProxy.Create(mrOk, 'user-proxy-pass');

	PluginSettingsMgr := TMockPluginSettingsManagerForProxy.Create;

	Resolver := TProxyPasswordResolver.Create(
		MockHTTPMgr, PasswordMgr, PasswordUI,
		PluginSettingsMgr,
		TNullTCHandler.Create, TNullLogger.Create);

	Assert.IsFalse(Resolver.ResolvePassword, 'ResolvePassword should fail when password save fails');
	Assert.IsTrue(PasswordMgr.SetPasswordCalled, 'SetPassword should have been called');
	Assert.IsFalse(PluginSettingsMgr.SwitchProxyPasswordStorageCalled,
		'SwitchProxyPasswordStorage should NOT be called when password save fails');
end;

procedure TProxyPasswordResolverTest.TestResolvePassword_PasswordFromINI;
var
	Resolver: IProxyPasswordResolver;
	MockHTTPMgr: TMockHTTPManager;
	ProxyConnSettings: TConnectionSettings;
begin
	{Proxy with password already available from INI settings.}
	MockHTTPMgr := TMockHTTPManager.Create(TNullCloudHTTP.Create);
	ProxyConnSettings := Default(TConnectionSettings);
	ProxyConnSettings.ProxySettings.ProxyType := ProxyHTTP;
	ProxyConnSettings.ProxySettings.User := 'proxyuser';
	ProxyConnSettings.ProxySettings.Password := 'proxypass';
	MockHTTPMgr.SetConnectionSettings(ProxyConnSettings);

	Resolver := TProxyPasswordResolver.Create(
		MockHTTPMgr, TNullPasswordManager.Create, TNullPasswordUIProvider.Create,
		TMockPluginSettingsManagerForProxy.Create,
		TNullTCHandler.Create, TNullLogger.Create);

	Assert.IsTrue(Resolver.ResolvePassword, 'ResolvePassword should succeed with password from INI');
end;

procedure TProxyPasswordResolverTest.TestResolvePassword_NoProxy_ReturnsTrue;
var
	Resolver: IProxyPasswordResolver;
	MockHTTPMgr: TMockHTTPManager;
	ProxyConnSettings: TConnectionSettings;
begin
	{No proxy configured -> returns True immediately}
	MockHTTPMgr := TMockHTTPManager.Create(TNullCloudHTTP.Create);
	ProxyConnSettings := Default(TConnectionSettings);
	ProxyConnSettings.ProxySettings.ProxyType := ProxyNone;
	MockHTTPMgr.SetConnectionSettings(ProxyConnSettings);

	Resolver := TProxyPasswordResolver.Create(
		MockHTTPMgr, TNullPasswordManager.Create, TNullPasswordUIProvider.Create,
		TMockPluginSettingsManagerForProxy.Create,
		TNullTCHandler.Create, TNullLogger.Create);

	Assert.IsTrue(Resolver.ResolvePassword, 'ResolvePassword should return True when no proxy');
end;

{TNullProxyPasswordResolverTest}

procedure TNullProxyPasswordResolverTest.TestNullResolverReturnsTrue;
var
	Resolver: IProxyPasswordResolver;
begin
	Resolver := TNullProxyPasswordResolver.Create;
	Assert.IsTrue(Resolver.ResolvePassword, 'Null resolver should always return True');
end;

initialization

TDUnitX.RegisterTestFixture(TProxyPasswordResolverTest);
TDUnitX.RegisterTestFixture(TNullProxyPasswordResolverTest);

end.
