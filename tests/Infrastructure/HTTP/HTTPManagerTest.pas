unit HTTPManagerTest;

interface

uses
	HTTPManager,
	CloudHTTP,
	ConnectionSettings,
	SSLHandlerFactory,
	IndySSLHandlerFactory,
	Logger,
	Progress,
	Classes,
	System.Generics.Collections,
	IdHTTP,
	IdCookieManager,
	DUnitX.TestFramework;

type

	{Mock HTTP connection for testing pooling behavior}
	TMockHTTPForPooling = class(TInterfacedObject, ICloudHTTP)
	private
		FId: Integer;
	public
		constructor Create(Id: Integer);
		property Id: Integer read FId;

		{ICloudHTTP implementation - minimal stubs}
		function GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
		function GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean = True): Integer;
		function PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = ''; LogErrors: Boolean = True; ProgressEnabled: Boolean = True): Boolean;
		function PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
		procedure SetProgressNames(SourceName, TargetName: WideString);
		procedure SetProgress(Progress: IProgress);
		procedure SetAuthCookie(Value: TIdCookieManager);
		procedure SetCSRFToken(const Token: WideString);
		function GetHTTP: TIdHTTP;
	end;

	{Mock factory that creates TMockHTTPForPooling instances with incrementing IDs}
	TMockHTTPFactory = class(TInterfacedObject, ICloudHTTPFactory)
	private
		FCreateCount: Integer;
	public
		constructor Create;
		function CreateHTTP(Settings: TConnectionSettings; SSLFactory: ISSLHandlerFactory; Logger: ILogger; Progress: IProgress): ICloudHTTP;
		property CreateCount: Integer read FCreateCount;
	end;

	[TestFixture]
	TNullHTTPManagerTest = class
	public
		[Test]
		procedure Test_ImplementsIHTTPManager;

		[Test]
		procedure Test_Get_ReturnsNullCloudHTTP;

		[Test]
		procedure Test_GetConnectionSettings_ReturnsDefault;

		[Test]
		procedure Test_SetProxyPassword_NoException;

		[Test]
		procedure Test_SetProxyPassword_StoresValue;

		[Test]
		procedure Test_MultipleCalls;
	end;

	[TestFixture]
	THTTPManagerTest = class
	private
		FManager: THTTPManager;
		FManagerRef: IHTTPManager;
		FMockFactory: TMockHTTPFactory;
		FMockFactoryRef: ICloudHTTPFactory;
		FSettings: TConnectionSettings;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Construction tests}
		[Test]
		procedure Test_Create_StoresSettings;

		[Test]
		procedure Test_Create_StoresFactory;

		{GetConnectionSettings tests}
		[Test]
		procedure Test_GetConnectionSettings_ReturnsStoredSettings;

		{SetProxyPassword tests}
		[Test]
		procedure Test_SetProxyPassword_UpdatesSettings;

		{Connection pooling tests}
		[Test]
		procedure Test_Get_CreatesNewConnection;

		[Test]
		procedure Test_Get_SameThread_ReturnsSameConnection;

		[Test]
		procedure Test_Get_DifferentThreads_ReturnDifferentConnections;

		[Test]
		procedure Test_Get_MultipleThreads_CreatesCorrectCount;

		[Test]
		procedure Test_Get_ReusesExistingConnection;
	end;

	[TestFixture]
	TCloudHTTPFactoryTest = class
	public
		[Test]
		procedure Test_ImplementsInterface;

		[Test]
		procedure Test_CreateHTTP_ReturnsNonNil;
	end;

implementation

uses
	SysUtils,
	WFXTypes;

{TMockHTTPForPooling}

constructor TMockHTTPForPooling.Create(Id: Integer);
begin
	inherited Create;
	FId := Id;
end;

function TMockHTTPForPooling.GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
begin
	Result := False;
end;

function TMockHTTPForPooling.GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean): Integer;
begin
	Result := FS_FILE_READERROR;
end;

function TMockHTTPForPooling.PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString; LogErrors: Boolean; ProgressEnabled: Boolean): Boolean;
begin
	Result := False;
end;

function TMockHTTPForPooling.PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
begin
	Result := FS_FILE_WRITEERROR;
end;

procedure TMockHTTPForPooling.SetProgressNames(SourceName, TargetName: WideString);
begin
end;

procedure TMockHTTPForPooling.SetProgress(Progress: IProgress);
begin
end;

procedure TMockHTTPForPooling.SetAuthCookie(Value: TIdCookieManager);
begin
end;

procedure TMockHTTPForPooling.SetCSRFToken(const Token: WideString);
begin
end;

function TMockHTTPForPooling.GetHTTP: TIdHTTP;
begin
	Result := nil;
end;

{TMockHTTPFactory}

constructor TMockHTTPFactory.Create;
begin
	inherited Create;
	FCreateCount := 0;
end;

function TMockHTTPFactory.CreateHTTP(Settings: TConnectionSettings; SSLFactory: ISSLHandlerFactory; Logger: ILogger; Progress: IProgress): ICloudHTTP;
begin
	Inc(FCreateCount);
	Result := TMockHTTPForPooling.Create(FCreateCount);
end;

{TNullHTTPManagerTest}

procedure TNullHTTPManagerTest.Test_ImplementsIHTTPManager;
var
	Manager: IHTTPManager;
begin
	Manager := TNullHTTPManager.Create;
	Assert.IsNotNull(Manager);
end;

procedure TNullHTTPManagerTest.Test_Get_ReturnsNullCloudHTTP;
var
	Manager: IHTTPManager;
	Connection: ICloudHTTP;
begin
	Manager := TNullHTTPManager.Create;
	Connection := Manager.Get(1234);
	{Should return a valid TNullCloudHTTP instance, not nil}
	Assert.IsNotNull(Connection);
end;

procedure TNullHTTPManagerTest.Test_GetConnectionSettings_ReturnsDefault;
var
	Manager: IHTTPManager;
	Settings: TConnectionSettings;
begin
	Manager := TNullHTTPManager.Create;
	Settings := Manager.GetConnectionSettings;
	Assert.AreEqual(0, Settings.SocketTimeout);
end;

procedure TNullHTTPManagerTest.Test_SetProxyPassword_NoException;
var
	Manager: IHTTPManager;
begin
	Manager := TNullHTTPManager.Create;
	Manager.SetProxyPassword('test_password');
	Assert.Pass;
end;

procedure TNullHTTPManagerTest.Test_SetProxyPassword_StoresValue;
var
	Manager: IHTTPManager;
begin
	Manager := TNullHTTPManager.Create;
	Manager.SetProxyPassword('my_proxy_pwd');
	Assert.AreEqual('my_proxy_pwd', Manager.GetConnectionSettings.ProxySettings.Password);
end;

procedure TNullHTTPManagerTest.Test_MultipleCalls;
var
	Manager: IHTTPManager;
begin
	Manager := TNullHTTPManager.Create;

	{Multiple Get calls should return valid connections}
	Assert.IsNotNull(Manager.Get(100));
	Assert.IsNotNull(Manager.Get(200));

	Manager.SetProxyPassword('pwd1');
	Manager.SetProxyPassword('pwd2');
	Assert.AreEqual('pwd2', Manager.ConnectionSettings.ProxySettings.Password);

	Assert.Pass;
end;

{THTTPManagerTest}

procedure THTTPManagerTest.Setup;
begin
	FSettings := Default(TConnectionSettings);
	FSettings.SocketTimeout := 30000;
	FSettings.UserAgent := 'TestAgent';

	FMockFactory := TMockHTTPFactory.Create;
	FMockFactoryRef := FMockFactory;

	FManager := THTTPManager.Create(FSettings, TIndySSLHandlerFactory.Create, TNullLogger.Create, TNullProgress.Create, FMockFactoryRef);
	FManagerRef := FManager;
end;

procedure THTTPManagerTest.TearDown;
begin
	FManagerRef := nil;
	FMockFactoryRef := nil;
end;

procedure THTTPManagerTest.Test_Create_StoresSettings;
begin
	Assert.AreEqual(30000, FManager.GetConnectionSettings.SocketTimeout);
	Assert.AreEqual('TestAgent', FManager.GetConnectionSettings.UserAgent);
end;

procedure THTTPManagerTest.Test_Create_StoresFactory;
begin
	{Factory is stored - calling Get should use it}
	FManager.Get(1);
	Assert.AreEqual(1, FMockFactory.CreateCount);
end;

procedure THTTPManagerTest.Test_GetConnectionSettings_ReturnsStoredSettings;
var
	Retrieved: TConnectionSettings;
begin
	Retrieved := FManager.GetConnectionSettings;
	Assert.AreEqual(FSettings.SocketTimeout, Retrieved.SocketTimeout);
	Assert.AreEqual(FSettings.UserAgent, Retrieved.UserAgent);
end;

procedure THTTPManagerTest.Test_SetProxyPassword_UpdatesSettings;
begin
	FManager.SetProxyPassword('secret_proxy_pass');
	Assert.AreEqual('secret_proxy_pass', FManager.GetConnectionSettings.ProxySettings.Password);
end;

procedure THTTPManagerTest.Test_Get_CreatesNewConnection;
var
	Connection: ICloudHTTP;
begin
	Assert.AreEqual(0, FMockFactory.CreateCount);

	Connection := FManager.Get(100);

	Assert.IsNotNull(Connection);
	Assert.AreEqual(1, FMockFactory.CreateCount);
end;

procedure THTTPManagerTest.Test_Get_SameThread_ReturnsSameConnection;
var
	Conn1, Conn2, Conn3: ICloudHTTP;
begin
	Conn1 := FManager.Get(100);
	Conn2 := FManager.Get(100);
	Conn3 := FManager.Get(100);

	{All should be the same instance}
	Assert.AreEqual(TMockHTTPForPooling(Conn1).Id, TMockHTTPForPooling(Conn2).Id);
	Assert.AreEqual(TMockHTTPForPooling(Conn2).Id, TMockHTTPForPooling(Conn3).Id);

	{Only one creation should have occurred}
	Assert.AreEqual(1, FMockFactory.CreateCount);
end;

procedure THTTPManagerTest.Test_Get_DifferentThreads_ReturnDifferentConnections;
var
	Conn1, Conn2: ICloudHTTP;
begin
	Conn1 := FManager.Get(100);
	Conn2 := FManager.Get(200);

	{Should be different instances}
	Assert.AreNotEqual(TMockHTTPForPooling(Conn1).Id, TMockHTTPForPooling(Conn2).Id);

	{Two creations should have occurred}
	Assert.AreEqual(2, FMockFactory.CreateCount);
end;

procedure THTTPManagerTest.Test_Get_MultipleThreads_CreatesCorrectCount;
var
	i: Integer;
begin
	{Simulate 5 different threads}
	for i := 1 to 5 do
		FManager.Get(Cardinal(i * 1000));

	Assert.AreEqual(5, FMockFactory.CreateCount);

	{Call again for same threads - no new creations}
	for i := 1 to 5 do
		FManager.Get(Cardinal(i * 1000));

	Assert.AreEqual(5, FMockFactory.CreateCount);
end;

procedure THTTPManagerTest.Test_Get_ReusesExistingConnection;
var
	Conn1, Conn2: ICloudHTTP;
	Id1: Integer;
begin
	Conn1 := FManager.Get(999);
	Id1 := TMockHTTPForPooling(Conn1).Id;

	{Release reference}
	Conn1 := nil;

	{Get again - should return same pooled connection}
	Conn2 := FManager.Get(999);

	Assert.AreEqual(Id1, TMockHTTPForPooling(Conn2).Id);
	Assert.AreEqual(1, FMockFactory.CreateCount);
end;

{TCloudHTTPFactoryTest}

procedure TCloudHTTPFactoryTest.Test_ImplementsInterface;
var
	Factory: ICloudHTTPFactory;
begin
	Factory := TCloudHTTPFactory.Create;
	Assert.IsNotNull(Factory);
end;

procedure TCloudHTTPFactoryTest.Test_CreateHTTP_ReturnsNonNil;
var
	Factory: ICloudHTTPFactory;
	Settings: TConnectionSettings;
	HTTP: ICloudHTTP;
begin
	Factory := TCloudHTTPFactory.Create;
	Settings := Default(TConnectionSettings);

	HTTP := Factory.CreateHTTP(Settings, TIndySSLHandlerFactory.Create, TNullLogger.Create, TNullProgress.Create);

	Assert.IsNotNull(HTTP);
end;

initialization

TDUnitX.RegisterTestFixture(TNullHTTPManagerTest);
TDUnitX.RegisterTestFixture(THTTPManagerTest);
TDUnitX.RegisterTestFixture(TCloudHTTPFactoryTest);

end.
