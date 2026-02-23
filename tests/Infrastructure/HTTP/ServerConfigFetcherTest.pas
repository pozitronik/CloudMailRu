unit ServerConfigFetcherTest;

interface

uses
	DUnitX.TestFramework,
	CloudEndpoints,
	ServerConfigFetcher;

type
	[TestFixture]
	TServerConfigFetcherTest = class
	private
		FFetcher: IServerConfigFetcher;
	public
		[Setup]
		procedure Setup;

		[Test]
		{Verifies that Fetch populates API endpoint from server URL}
		procedure TestFetchSetsApiBase;

		[Test]
		{Verifies that Fetch populates OAuth URL from server URL}
		procedure TestFetchSetsOAuthUrl;

		[Test]
		{Verifies that Fetch populates dispatcher URL from server URL}
		procedure TestFetchSetsDispatcherUrl;

		[Test]
		{Verifies that Fetch populates thumbnail URL from server URL}
		procedure TestFetchSetsThumbnailUrl;

		[Test]
		{Verifies that Fetch populates public URL from server URL}
		procedure TestFetchSetsPublicUrl;

		[Test]
		{Verifies that trailing slash in server URL is handled correctly}
		procedure TestFetchHandlesTrailingSlash;

		[Test]
		{Verifies that empty server URL returns failure}
		procedure TestFetchWithEmptyUrlFails;

		[Test]
		{Connection failure returns False with error message}
		procedure TestFetchReturnsFalseOnConnectionError;

		[Test]
		{Verifies that null fetcher always returns False}
		procedure TestNullFetcherReturnsFalse;

		[Test]
		{Verifies that null fetcher sets error message}
		procedure TestNullFetcherSetsErrorMsg;
	end;

	[TestFixture]
	TNullServerConfigFetcherTest = class
	public
		[Test]
		{Verifies null implementation always fails with descriptive message}
		procedure TestAlwaysFails;
	end;

implementation

uses
	SysUtils;

{TServerConfigFetcherTest}

procedure TServerConfigFetcherTest.Setup;
begin
	FFetcher := TServerConfigFetcher.Create;
end;

procedure TServerConfigFetcherTest.TestFetchSetsApiBase;
var
	Endpoints: TCloudEndpoints;
	ErrorMsg: WideString;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	FFetcher.Fetch('http://myserver:8080', Endpoints, ErrorMsg);
	Assert.AreEqual('http://myserver:8080/api/v2', string(Endpoints.ApiBase));
end;

procedure TServerConfigFetcherTest.TestFetchSetsOAuthUrl;
var
	Endpoints: TCloudEndpoints;
	ErrorMsg: WideString;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	FFetcher.Fetch('http://myserver:8080', Endpoints, ErrorMsg);
	Assert.AreEqual('http://myserver:8080/token', string(Endpoints.OAuthUrl));
end;

procedure TServerConfigFetcherTest.TestFetchSetsDispatcherUrl;
var
	Endpoints: TCloudEndpoints;
	ErrorMsg: WideString;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	FFetcher.Fetch('http://myserver:8080', Endpoints, ErrorMsg);
	Assert.AreEqual('http://myserver:8080/dispatcher', string(Endpoints.DispatcherUrl));
end;

procedure TServerConfigFetcherTest.TestFetchSetsThumbnailUrl;
var
	Endpoints: TCloudEndpoints;
	ErrorMsg: WideString;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	FFetcher.Fetch('http://myserver:8080', Endpoints, ErrorMsg);
	Assert.AreEqual('http://myserver:8080/thumb', string(Endpoints.ThumbnailUrl));
end;

procedure TServerConfigFetcherTest.TestFetchSetsPublicUrl;
var
	Endpoints: TCloudEndpoints;
	ErrorMsg: WideString;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	FFetcher.Fetch('http://myserver:8080', Endpoints, ErrorMsg);
	Assert.AreEqual('http://myserver:8080/public/', string(Endpoints.PublicUrl));
end;

procedure TServerConfigFetcherTest.TestFetchHandlesTrailingSlash;
var
	Endpoints: TCloudEndpoints;
	ErrorMsg: WideString;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	FFetcher.Fetch('http://myserver:8080/', Endpoints, ErrorMsg);
	Assert.AreEqual('http://myserver:8080/api/v2', string(Endpoints.ApiBase));
end;

procedure TServerConfigFetcherTest.TestFetchWithEmptyUrlFails;
var
	Endpoints: TCloudEndpoints;
	ErrorMsg: WideString;
	Success: Boolean;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Success := FFetcher.Fetch('', Endpoints, ErrorMsg);
	Assert.IsFalse(Success);
	Assert.IsNotEmpty(string(ErrorMsg));
end;

procedure TServerConfigFetcherTest.TestFetchReturnsFalseOnConnectionError;
var
	Endpoints: TCloudEndpoints;
	ErrorMsg: WideString;
	Success: Boolean;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Success := FFetcher.Fetch('http://myserver:8080', Endpoints, ErrorMsg);
	Assert.IsFalse(Success, 'Fetch to unreachable server should return False');
	Assert.IsNotEmpty(string(ErrorMsg), 'ErrorMsg should describe the connection failure');
end;

procedure TServerConfigFetcherTest.TestNullFetcherReturnsFalse;
var
	Fetcher: IServerConfigFetcher;
	Endpoints: TCloudEndpoints;
	ErrorMsg: WideString;
begin
	Fetcher := TNullServerConfigFetcher.Create;
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.IsFalse(Fetcher.Fetch('http://myserver:8080', Endpoints, ErrorMsg));
end;

procedure TServerConfigFetcherTest.TestNullFetcherSetsErrorMsg;
var
	Fetcher: IServerConfigFetcher;
	Endpoints: TCloudEndpoints;
	ErrorMsg: WideString;
begin
	Fetcher := TNullServerConfigFetcher.Create;
	Endpoints := TCloudEndpoints.CreateDefaults;
	Fetcher.Fetch('http://myserver:8080', Endpoints, ErrorMsg);
	Assert.IsNotEmpty(string(ErrorMsg));
end;

{TNullServerConfigFetcherTest}

procedure TNullServerConfigFetcherTest.TestAlwaysFails;
var
	Fetcher: IServerConfigFetcher;
	Endpoints: TCloudEndpoints;
	ErrorMsg: WideString;
begin
	Fetcher := TNullServerConfigFetcher.Create;
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.IsFalse(Fetcher.Fetch('http://anything', Endpoints, ErrorMsg));
	Assert.AreEqual('Self-configure not available', string(ErrorMsg));
end;

initialization
	TDUnitX.RegisterTestFixture(TServerConfigFetcherTest);
	TDUnitX.RegisterTestFixture(TNullServerConfigFetcherTest);

end.
