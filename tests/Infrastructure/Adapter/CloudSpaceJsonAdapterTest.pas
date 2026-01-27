unit CloudSpaceJsonAdapterTest;

interface

uses
	CloudSpace,
	CloudSpaceJsonAdapter,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCloudSpaceJsonAdapterTest = class
	private
		const
			{Normal space info}
			JSON_SPACE_NORMAL = '{"status":200,"body":{"overquota":false,"bytes_total":17179869184,"bytes_used":5368709120}}';

			{Overquota space info}
			JSON_SPACE_OVERQUOTA = '{"status":200,"body":{"overquota":true,"bytes_total":17179869184,"bytes_used":20000000000}}';

			{Large values}
			JSON_SPACE_LARGE = '{"status":200,"body":{"overquota":false,"bytes_total":1099511627776,"bytes_used":549755813888}}';

			{Invalid JSON}
			JSON_INVALID = 'not valid json';

			{Empty string}
			JSON_EMPTY = '';
	public
		[Test]
		procedure TestParse_Normal_ReturnsTrue;
		[Test]
		procedure TestParse_Normal_ParsesTotal;
		[Test]
		procedure TestParse_Normal_ParsesUsed;
		[Test]
		procedure TestParse_Normal_ParsesOverquotaFalse;
		[Test]
		procedure TestParse_Overquota_ParsesOverquotaTrue;
		[Test]
		procedure TestParse_LargeValues_ParsesCorrectly;
		[Test]
		procedure TestParse_InvalidJSON_ReturnsFalse;
		[Test]
		procedure TestParse_EmptyString_ReturnsFalse;
	end;

implementation

procedure TCloudSpaceJsonAdapterTest.TestParse_Normal_ReturnsTrue;
var
	Space: TCloudSpace;
begin
	Assert.IsTrue(TCloudSpaceJsonAdapter.Parse(JSON_SPACE_NORMAL, Space));
end;

procedure TCloudSpaceJsonAdapterTest.TestParse_Normal_ParsesTotal;
var
	Space: TCloudSpace;
begin
	TCloudSpaceJsonAdapter.Parse(JSON_SPACE_NORMAL, Space);
	Assert.AreEqual(Int64(17179869184), Space.total);
end;

procedure TCloudSpaceJsonAdapterTest.TestParse_Normal_ParsesUsed;
var
	Space: TCloudSpace;
begin
	TCloudSpaceJsonAdapter.Parse(JSON_SPACE_NORMAL, Space);
	Assert.AreEqual(Int64(5368709120), Space.used);
end;

procedure TCloudSpaceJsonAdapterTest.TestParse_Normal_ParsesOverquotaFalse;
var
	Space: TCloudSpace;
begin
	TCloudSpaceJsonAdapter.Parse(JSON_SPACE_NORMAL, Space);
	Assert.IsFalse(Space.overquota);
end;

procedure TCloudSpaceJsonAdapterTest.TestParse_Overquota_ParsesOverquotaTrue;
var
	Space: TCloudSpace;
begin
	TCloudSpaceJsonAdapter.Parse(JSON_SPACE_OVERQUOTA, Space);
	Assert.IsTrue(Space.overquota);
end;

procedure TCloudSpaceJsonAdapterTest.TestParse_LargeValues_ParsesCorrectly;
var
	Space: TCloudSpace;
begin
	TCloudSpaceJsonAdapter.Parse(JSON_SPACE_LARGE, Space);
	Assert.AreEqual(Int64(1099511627776), Space.total);
	Assert.AreEqual(Int64(549755813888), Space.used);
end;

procedure TCloudSpaceJsonAdapterTest.TestParse_InvalidJSON_ReturnsFalse;
var
	Space: TCloudSpace;
begin
	Assert.IsFalse(TCloudSpaceJsonAdapter.Parse(JSON_INVALID, Space));
end;

procedure TCloudSpaceJsonAdapterTest.TestParse_EmptyString_ReturnsFalse;
var
	Space: TCloudSpace;
begin
	Assert.IsFalse(TCloudSpaceJsonAdapter.Parse(JSON_EMPTY, Space));
end;

initialization

TDUnitX.RegisterTestFixture(TCloudSpaceJsonAdapterTest);

end.
