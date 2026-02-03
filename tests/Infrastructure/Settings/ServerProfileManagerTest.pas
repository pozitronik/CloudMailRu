unit ServerProfileManagerTest;

interface

uses
	ServerProfile,
	ServerProfileManager,
	CloudEndpoints,
	ConfigFile,
	WSList,
	DUnitX.TestFramework;

type

	[TestFixture]
	TServerProfileManagerTest = class
	private
		FConfigFile: IConfigFile;
		FManager: IServerProfileManager;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestGetProfileNames_Empty;
		[Test]
		procedure TestGetProfileNames_IgnoresNonServerSections;
		[Test]
		procedure TestSetAndGetProfile;
		[Test]
		procedure TestGetProfile_NotFound_ReturnsDefault;
		[Test]
		procedure TestGetProfile_EmptyName_ReturnsDefault;
		[Test]
		procedure TestDeleteProfile;
		[Test]
		procedure TestDeleteProfile_EmptyName_NoOp;
		[Test]
		procedure TestRenameProfile;
		[Test]
		procedure TestRenameProfile_SameName_NoOp;
		[Test]
		procedure TestResolveEndpoints_EmptyName_ReturnsDefaults;
		[Test]
		procedure TestResolveEndpoints_CustomProfile;
		[Test]
		procedure TestSetProfile_DefaultValuesNotWritten;
		[Test]
		procedure TestGetProfile_ServerUrlInfersEndpoints;
		[Test]
		procedure TestGetProfile_ExplicitOverridesWinOverInferred;
		[Test]
		procedure TestInferEndpointsFromServerUrl;
		[Test]
		procedure TestInferEndpointsFromServerUrl_Empty;
		[Test]
		procedure TestInferEndpointsFromServerUrl_TrailingSlash;
		[Test]
		procedure TestInferEndpointsFromServerUrl_NoScheme;
		[Test]
		procedure TestMultipleProfiles;
		[Test]
		procedure TestNullManager_ReturnsDefaults;
	end;

implementation

uses
	CloudConstants;

procedure TServerProfileManagerTest.Setup;
begin
	FConfigFile := TMemoryConfigFile.Create;
	FManager := TServerProfileManager.Create(FConfigFile);
end;

procedure TServerProfileManagerTest.TearDown;
begin
	FManager := nil;
	FConfigFile := nil;
end;

procedure TServerProfileManagerTest.TestGetProfileNames_Empty;
var
	Names: TWSList;
begin
	Names := FManager.GetProfileNames;
	Assert.AreEqual(0, Names.Count);
end;

procedure TServerProfileManagerTest.TestGetProfileNames_IgnoresNonServerSections;
begin
	{Create a regular account section and a server section}
	FConfigFile.WriteString('MyAccount', 'email', 'user@mail.ru');
	FConfigFile.WriteString('Server:TestServer', 'server_url', 'http://localhost');

	var Names := FManager.GetProfileNames;
	Assert.AreEqual(1, Names.Count);
	Assert.AreEqual('TestServer', Names[0]);
end;

procedure TServerProfileManagerTest.TestSetAndGetProfile;
var
	Profile, Loaded: TServerProfile;
begin
	Profile := TServerProfile.CreateDefault;
	Profile.Name := 'MyServer';
	Profile.ServerUrl := 'http://localhost:8080';
	Profile.Endpoints.ApiBase := 'http://localhost:8080/api/v2';
	Profile.Endpoints.OAuthUrl := 'http://localhost:8081/token';

	FManager.SetProfile(Profile);

	Loaded := FManager.GetProfile('MyServer');
	Assert.AreEqual('MyServer', Loaded.Name);
	Assert.AreEqual('http://localhost:8080', Loaded.ServerUrl);
	Assert.AreEqual('http://localhost:8080/api/v2', Loaded.Endpoints.ApiBase);
	Assert.AreEqual('http://localhost:8081/token', Loaded.Endpoints.OAuthUrl);
end;

procedure TServerProfileManagerTest.TestGetProfile_NotFound_ReturnsDefault;
var
	Profile: TServerProfile;
	Defaults: TCloudEndpoints;
begin
	Profile := FManager.GetProfile('NonExistent');
	Defaults := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(Defaults.ApiBase, Profile.Endpoints.ApiBase);
end;

procedure TServerProfileManagerTest.TestGetProfile_EmptyName_ReturnsDefault;
var
	Profile: TServerProfile;
begin
	Profile := FManager.GetProfile('');
	Assert.AreEqual('', string(Profile.Name));
	Assert.AreEqual('', string(Profile.ServerUrl));
end;

procedure TServerProfileManagerTest.TestDeleteProfile;
begin
	FConfigFile.WriteString('Server:ToDelete', 'server_url', 'http://localhost');
	Assert.AreEqual(1, FManager.GetProfileNames.Count);

	FManager.DeleteProfile('ToDelete');
	Assert.AreEqual(0, FManager.GetProfileNames.Count);
end;

procedure TServerProfileManagerTest.TestDeleteProfile_EmptyName_NoOp;
begin
	{Should not crash}
	FManager.DeleteProfile('');
end;

procedure TServerProfileManagerTest.TestRenameProfile;
var
	Profile: TServerProfile;
begin
	Profile := TServerProfile.CreateDefault;
	Profile.Name := 'OldName';
	Profile.ServerUrl := 'http://old.server';
	Profile.Endpoints.ApiBase := 'http://old.server/api/v2';
	FManager.SetProfile(Profile);

	FManager.RenameProfile('OldName', 'NewName');

	Assert.AreEqual('', WideString(FManager.GetProfile('OldName').Name), 'Old profile should not exist');
	var Renamed := FManager.GetProfile('NewName');
	Assert.AreEqual('NewName', Renamed.Name);
	Assert.AreEqual('http://old.server', Renamed.ServerUrl);
end;

procedure TServerProfileManagerTest.TestRenameProfile_SameName_NoOp;
var
	Profile: TServerProfile;
begin
	Profile := TServerProfile.CreateDefault;
	Profile.Name := 'Same';
	Profile.ServerUrl := 'http://same.server';
	FManager.SetProfile(Profile);

	FManager.RenameProfile('Same', 'Same');
	Assert.AreEqual('http://same.server', FManager.GetProfile('Same').ServerUrl);
end;

procedure TServerProfileManagerTest.TestResolveEndpoints_EmptyName_ReturnsDefaults;
var
	Endpoints, Defaults: TCloudEndpoints;
begin
	Endpoints := FManager.ResolveEndpoints('');
	Defaults := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(Defaults.ApiBase, Endpoints.ApiBase);
	Assert.AreEqual(Defaults.OAuthUrl, Endpoints.OAuthUrl);
	Assert.AreEqual(Defaults.DispatcherUrl, Endpoints.DispatcherUrl);
end;

procedure TServerProfileManagerTest.TestResolveEndpoints_CustomProfile;
var
	Profile: TServerProfile;
	Endpoints: TCloudEndpoints;
begin
	Profile := TServerProfile.CreateDefault;
	Profile.Name := 'Custom';
	Profile.Endpoints.ApiBase := 'http://custom/api/v2';
	FManager.SetProfile(Profile);

	Endpoints := FManager.ResolveEndpoints('Custom');
	Assert.AreEqual('http://custom/api/v2', Endpoints.ApiBase);
	Assert.AreEqual('http://custom/api/v2/file', Endpoints.ApiFile);
end;

procedure TServerProfileManagerTest.TestSetProfile_DefaultValuesNotWritten;
begin
	{When all endpoints are defaults, only server_url should be written if set}
	var Profile := TServerProfile.CreateDefault;
	Profile.Name := 'DefaultsOnly';
	FManager.SetProfile(Profile);

	{Section should exist but with no endpoint keys (all are defaults)}
	Assert.IsFalse(FConfigFile.SectionExists('Server:DefaultsOnly'),
		'Section should not exist when all values are defaults');
end;

procedure TServerProfileManagerTest.TestGetProfile_ServerUrlInfersEndpoints;
begin
	FConfigFile.WriteString('Server:Inferred', 'server_url', 'http://myhost:9090');

	var Profile := FManager.GetProfile('Inferred');
	Assert.AreEqual('http://myhost:9090/api/v2', Profile.Endpoints.ApiBase);
	Assert.AreEqual('http://myhost:9090/token', Profile.Endpoints.OAuthUrl);
	Assert.AreEqual('http://myhost:9090/dispatcher', Profile.Endpoints.DispatcherUrl);
	Assert.AreEqual('http://myhost:9090/thumb', Profile.Endpoints.ThumbnailUrl);
	Assert.AreEqual('http://myhost:9090/public/', Profile.Endpoints.PublicUrl);
end;

procedure TServerProfileManagerTest.TestGetProfile_ExplicitOverridesWinOverInferred;
begin
	{server_url infers api_url as http://myhost/api/v2,
		but explicit api_url override wins}
	FConfigFile.WriteString('Server:Mixed', 'server_url', 'http://myhost');
	FConfigFile.WriteString('Server:Mixed', 'api_url', 'http://other-host/api/v2');

	var Profile := FManager.GetProfile('Mixed');
	Assert.AreEqual('http://other-host/api/v2', Profile.Endpoints.ApiBase);
	{Other endpoints still inferred from server_url}
	Assert.AreEqual('http://myhost/token', Profile.Endpoints.OAuthUrl);
end;

procedure TServerProfileManagerTest.TestInferEndpointsFromServerUrl;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TServerProfileManager.InferEndpointsFromServerUrl('http://localhost:8080');
	Assert.AreEqual('http://localhost:8080/api/v2', Endpoints.ApiBase);
	Assert.AreEqual('http://localhost:8080/token', Endpoints.OAuthUrl);
	Assert.AreEqual('http://localhost:8080/dispatcher', Endpoints.DispatcherUrl);
	Assert.AreEqual('http://localhost:8080/thumb', Endpoints.ThumbnailUrl);
	Assert.AreEqual('http://localhost:8080/public/', Endpoints.PublicUrl);
end;

procedure TServerProfileManagerTest.TestInferEndpointsFromServerUrl_Empty;
var
	Endpoints, Defaults: TCloudEndpoints;
begin
	Endpoints := TServerProfileManager.InferEndpointsFromServerUrl('');
	Defaults := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(Defaults.ApiBase, Endpoints.ApiBase);
end;

procedure TServerProfileManagerTest.TestInferEndpointsFromServerUrl_TrailingSlash;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TServerProfileManager.InferEndpointsFromServerUrl('http://localhost:8080/');
	Assert.AreEqual('http://localhost:8080/api/v2', Endpoints.ApiBase);
end;

procedure TServerProfileManagerTest.TestInferEndpointsFromServerUrl_NoScheme;
var
	Endpoints: TCloudEndpoints;
begin
	{User enters just host:port without http:// -- should assume http://}
	Endpoints := TServerProfileManager.InferEndpointsFromServerUrl('localhost:8080');
	Assert.AreEqual('http://localhost:8080/api/v2', Endpoints.ApiBase);
	Assert.AreEqual('http://localhost:8080/token', Endpoints.OAuthUrl);
	Assert.AreEqual('http://localhost:8080/dispatcher', Endpoints.DispatcherUrl);
	Assert.AreEqual('http://localhost:8080/thumb', Endpoints.ThumbnailUrl);
	Assert.AreEqual('http://localhost:8080/public/', Endpoints.PublicUrl);
end;

procedure TServerProfileManagerTest.TestMultipleProfiles;
var
	P1, P2: TServerProfile;
begin
	P1 := TServerProfile.CreateDefault;
	P1.Name := 'Server1';
	P1.ServerUrl := 'http://server1';
	P1.Endpoints.ApiBase := 'http://server1/api/v2';

	P2 := TServerProfile.CreateDefault;
	P2.Name := 'Server2';
	P2.ServerUrl := 'http://server2';
	P2.Endpoints.ApiBase := 'http://server2/api/v2';

	FManager.SetProfile(P1);
	FManager.SetProfile(P2);

	var Names := FManager.GetProfileNames;
	Assert.AreEqual(2, Names.Count);

	Assert.AreEqual('http://server1/api/v2', FManager.GetProfile('Server1').Endpoints.ApiBase);
	Assert.AreEqual('http://server2/api/v2', FManager.GetProfile('Server2').Endpoints.ApiBase);
end;

procedure TServerProfileManagerTest.TestNullManager_ReturnsDefaults;
var
	NullManager: IServerProfileManager;
	Defaults: TCloudEndpoints;
begin
	NullManager := TNullServerProfileManager.Create;
	Defaults := TCloudEndpoints.CreateDefaults;

	Assert.AreEqual(0, NullManager.GetProfileNames.Count);
	Assert.AreEqual(Defaults.ApiBase, NullManager.ResolveEndpoints('anything').ApiBase);
	Assert.AreEqual('', string(NullManager.GetProfile('test').Name));
end;

initialization

TDUnitX.RegisterTestFixture(TServerProfileManagerTest);

end.
