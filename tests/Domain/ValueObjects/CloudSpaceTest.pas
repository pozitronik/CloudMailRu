unit CloudSpaceTest;

interface

uses
	CloudSpace,
	CloudSpaceJsonAdapter,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCloudSpaceTest = class
	public
		[Test]
		procedure TestFromJSONValid;
		[Test]
		procedure TestFromJSONOverquotaTrue;
		[Test]
		procedure TestFromJSONOverquotaFalse;
		[Test]
		procedure TestFromJSONLargeValues;
		[Test]
		procedure TestFromJSONInvalid;
		[Test]
		procedure TestFromJSONEmpty;
	end;

implementation

const
	JSON_SPACE_NORMAL = '{"status":200,"body":{"overquota":false,"bytes_total":17179869184,"bytes_used":5368709120}}';
	JSON_SPACE_OVERQUOTA = '{"status":200,"body":{"overquota":true,"bytes_total":17179869184,"bytes_used":20000000000}}';
	JSON_SPACE_LARGE = '{"status":200,"body":{"overquota":false,"bytes_total":1099511627776,"bytes_used":549755813888}}';

procedure TCloudSpaceTest.TestFromJSONValid;
var
	Space: TCloudSpace;
begin
	Assert.IsTrue(TCloudSpaceJsonAdapter.Parse(JSON_SPACE_NORMAL, Space));

	Assert.IsFalse(Space.overquota);
	Assert.AreEqual(Int64(17179869184), Space.total);
	Assert.AreEqual(Int64(5368709120), Space.used);
end;

procedure TCloudSpaceTest.TestFromJSONOverquotaTrue;
var
	Space: TCloudSpace;
begin
	Assert.IsTrue(TCloudSpaceJsonAdapter.Parse(JSON_SPACE_OVERQUOTA, Space));

	Assert.IsTrue(Space.overquota);
end;

procedure TCloudSpaceTest.TestFromJSONOverquotaFalse;
var
	Space: TCloudSpace;
begin
	Assert.IsTrue(TCloudSpaceJsonAdapter.Parse(JSON_SPACE_NORMAL, Space));

	Assert.IsFalse(Space.overquota);
end;

procedure TCloudSpaceTest.TestFromJSONLargeValues;
var
	Space: TCloudSpace;
begin
	{ Test with 1TB total and 512GB used }
	Assert.IsTrue(TCloudSpaceJsonAdapter.Parse(JSON_SPACE_LARGE, Space));

	Assert.AreEqual(Int64(1099511627776), Space.total);
	Assert.AreEqual(Int64(549755813888), Space.used);
end;

procedure TCloudSpaceTest.TestFromJSONInvalid;
var
	Space: TCloudSpace;
begin
	Assert.IsFalse(TCloudSpaceJsonAdapter.Parse('invalid json', Space));
end;

procedure TCloudSpaceTest.TestFromJSONEmpty;
var
	Space: TCloudSpace;
begin
	Assert.IsFalse(TCloudSpaceJsonAdapter.Parse('', Space));
end;

initialization

TDUnitX.RegisterTestFixture(TCloudSpaceTest);

end.
