unit CloudEndpointsJsonAdapterTest;

interface

uses
	CloudEndpoints,
	CloudEndpointsJsonAdapter,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCloudEndpointsJsonAdapterTest = class
	public
		[Test]
		procedure TestMerge_AllFields;
		[Test]
		procedure TestMerge_PartialFields;
		[Test]
		procedure TestMerge_EmptyFieldsNotOverridden;
		[Test]
		procedure TestMerge_InvalidJson;
		[Test]
		procedure TestMerge_EmptyJson;
		[Test]
		procedure TestMerge_EmptyObject;
		[Test]
		procedure TestMerge_UnknownFieldsIgnored;
		[Test]
		procedure TestMerge_PreservesExistingValues;
	end;

implementation

const
	JSON_ALL_FIELDS =
		'{"api":"http://localhost:8080/api/v2",' +
		'"oauth":"http://localhost:8081/token",' +
		'"dispatcher":"http://localhost:8082",' +
		'"thumbnail":"http://localhost:8083/thumb",' +
		'"public":"http://localhost:8080/public/",' +
		'"download":"http://localhost:8084/get",' +
		'"upload":"http://localhost:8085/upload"}';

	JSON_PARTIAL =
		'{"api":"http://custom.server/api/v2",' +
		'"oauth":"http://custom.server/token"}';

	JSON_WITH_EMPTY =
		'{"api":"http://custom.server/api/v2",' +
		'"oauth":"",' +
		'"dispatcher":"http://custom.server/dispatch"}';

	JSON_UNKNOWN_FIELDS =
		'{"api":"http://custom.server/api/v2",' +
		'"unknown_field":"some_value",' +
		'"another":123}';

procedure TCloudEndpointsJsonAdapterTest.TestMerge_AllFields;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.IsTrue(TCloudEndpointsJsonAdapter.Merge(JSON_ALL_FIELDS, Endpoints));

	Assert.AreEqual('http://localhost:8080/api/v2', Endpoints.ApiBase);
	Assert.AreEqual('http://localhost:8081/token', Endpoints.OAuthUrl);
	Assert.AreEqual('http://localhost:8082', Endpoints.DispatcherUrl);
	Assert.AreEqual('http://localhost:8083/thumb', Endpoints.ThumbnailUrl);
	Assert.AreEqual('http://localhost:8080/public/', Endpoints.PublicUrl);
	Assert.AreEqual('http://localhost:8084/get', Endpoints.DownloadUrl);
	Assert.AreEqual('http://localhost:8085/upload', Endpoints.UploadUrl);

	{Computed paths should derive from new ApiBase}
	Assert.AreEqual('http://localhost:8080/api/v2/file', Endpoints.ApiFile);
end;

procedure TCloudEndpointsJsonAdapterTest.TestMerge_PartialFields;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.IsTrue(TCloudEndpointsJsonAdapter.Merge(JSON_PARTIAL, Endpoints));

	{Specified fields overridden}
	Assert.AreEqual('http://custom.server/api/v2', Endpoints.ApiBase);
	Assert.AreEqual('http://custom.server/token', Endpoints.OAuthUrl);

	{Unspecified fields remain at defaults}
	Assert.AreEqual('https://dispatcher.cloud.mail.ru', Endpoints.DispatcherUrl);
	Assert.AreEqual('https://thumb.cloud.mail.ru/thumb', Endpoints.ThumbnailUrl);
	Assert.AreEqual('https://cloud.mail.ru/public/', Endpoints.PublicUrl);
end;

procedure TCloudEndpointsJsonAdapterTest.TestMerge_EmptyFieldsNotOverridden;
var
	Endpoints: TCloudEndpoints;
	OriginalOAuth: WideString;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	OriginalOAuth := Endpoints.OAuthUrl;
	Assert.IsTrue(TCloudEndpointsJsonAdapter.Merge(JSON_WITH_EMPTY, Endpoints));

	{Non-empty field overridden}
	Assert.AreEqual('http://custom.server/api/v2', Endpoints.ApiBase);
	{Empty field NOT overridden -- original value preserved}
	Assert.AreEqual(OriginalOAuth, Endpoints.OAuthUrl);
	{Non-empty field overridden}
	Assert.AreEqual('http://custom.server/dispatch', Endpoints.DispatcherUrl);
end;

procedure TCloudEndpointsJsonAdapterTest.TestMerge_InvalidJson;
var
	Endpoints: TCloudEndpoints;
	OriginalApiBase: WideString;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	OriginalApiBase := Endpoints.ApiBase;
	Assert.IsFalse(TCloudEndpointsJsonAdapter.Merge('not valid json', Endpoints));

	{Endpoints unchanged on failure}
	Assert.AreEqual(OriginalApiBase, Endpoints.ApiBase);
end;

procedure TCloudEndpointsJsonAdapterTest.TestMerge_EmptyJson;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.IsFalse(TCloudEndpointsJsonAdapter.Merge('', Endpoints));
end;

procedure TCloudEndpointsJsonAdapterTest.TestMerge_EmptyObject;
var
	Endpoints: TCloudEndpoints;
	Defaults: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Defaults := TCloudEndpoints.CreateDefaults;
	Assert.IsTrue(TCloudEndpointsJsonAdapter.Merge('{}', Endpoints));

	{All fields remain at defaults}
	Assert.AreEqual(Defaults.ApiBase, Endpoints.ApiBase);
	Assert.AreEqual(Defaults.OAuthUrl, Endpoints.OAuthUrl);
	Assert.AreEqual(Defaults.DispatcherUrl, Endpoints.DispatcherUrl);
end;

procedure TCloudEndpointsJsonAdapterTest.TestMerge_UnknownFieldsIgnored;
var
	Endpoints: TCloudEndpoints;
begin
	Endpoints := TCloudEndpoints.CreateDefaults;
	Assert.IsTrue(TCloudEndpointsJsonAdapter.Merge(JSON_UNKNOWN_FIELDS, Endpoints));

	{Known field applied}
	Assert.AreEqual('http://custom.server/api/v2', Endpoints.ApiBase);
	{No crash, unknown fields silently ignored}
end;

procedure TCloudEndpointsJsonAdapterTest.TestMerge_PreservesExistingValues;
var
	Endpoints: TCloudEndpoints;
begin
	{Start with custom values, merge partial JSON}
	Endpoints := TCloudEndpoints.CreateDefaults;
	Endpoints.ThumbnailUrl := 'http://my-thumb-server/thumb';
	Endpoints.DownloadUrl := 'http://my-download-server/get';

	Assert.IsTrue(TCloudEndpointsJsonAdapter.Merge(JSON_PARTIAL, Endpoints));

	{Merged fields changed}
	Assert.AreEqual('http://custom.server/api/v2', Endpoints.ApiBase);
	{Non-merged fields kept custom values}
	Assert.AreEqual('http://my-thumb-server/thumb', Endpoints.ThumbnailUrl);
	Assert.AreEqual('http://my-download-server/get', Endpoints.DownloadUrl);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudEndpointsJsonAdapterTest);

end.
