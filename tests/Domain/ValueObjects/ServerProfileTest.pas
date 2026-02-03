unit ServerProfileTest;

interface

uses
	ServerProfile,
	CloudEndpoints,
	DUnitX.TestFramework;

type

	[TestFixture]
	TServerProfileTest = class
	public
		[Test]
		procedure TestCreateDefault_NameEmpty;
		[Test]
		procedure TestCreateDefault_ServerUrlEmpty;
		[Test]
		procedure TestCreateDefault_EndpointsAreDefaults;
		[Test]
		procedure TestCustomProfile;
	end;

implementation

procedure TServerProfileTest.TestCreateDefault_NameEmpty;
var
	Profile: TServerProfile;
begin
	Profile := TServerProfile.CreateDefault;
	Assert.AreEqual('', string(Profile.Name));
end;

procedure TServerProfileTest.TestCreateDefault_ServerUrlEmpty;
var
	Profile: TServerProfile;
begin
	Profile := TServerProfile.CreateDefault;
	Assert.AreEqual('', string(Profile.ServerUrl));
end;

procedure TServerProfileTest.TestCreateDefault_EndpointsAreDefaults;
var
	Profile: TServerProfile;
	Defaults: TCloudEndpoints;
begin
	Profile := TServerProfile.CreateDefault;
	Defaults := TCloudEndpoints.CreateDefaults;
	Assert.AreEqual(Defaults.ApiBase, Profile.Endpoints.ApiBase);
	Assert.AreEqual(Defaults.OAuthUrl, Profile.Endpoints.OAuthUrl);
	Assert.AreEqual(Defaults.DispatcherUrl, Profile.Endpoints.DispatcherUrl);
	Assert.AreEqual(Defaults.ThumbnailUrl, Profile.Endpoints.ThumbnailUrl);
	Assert.AreEqual(Defaults.PublicUrl, Profile.Endpoints.PublicUrl);
end;

procedure TServerProfileTest.TestCustomProfile;
var
	Profile: TServerProfile;
begin
	Profile := TServerProfile.CreateDefault;
	Profile.Name := 'TestServer';
	Profile.ServerUrl := 'http://localhost:8080';
	Profile.Endpoints.ApiBase := 'http://localhost:8080/api/v2';
	Profile.Endpoints.OAuthUrl := 'http://localhost:8081/token';

	Assert.AreEqual('TestServer', Profile.Name);
	Assert.AreEqual('http://localhost:8080', Profile.ServerUrl);
	Assert.AreEqual('http://localhost:8080/api/v2/file', Profile.Endpoints.ApiFile);
	Assert.AreEqual('http://localhost:8081/token', Profile.Endpoints.OAuthUrl);
end;

initialization

TDUnitX.RegisterTestFixture(TServerProfileTest);

end.
