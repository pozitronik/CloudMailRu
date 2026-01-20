unit CMRSpaceTest;

interface

uses
	CMRSpace,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCMRSpaceTest = class
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

procedure TCMRSpaceTest.TestFromJSONValid;
var
	Space: TCMRSpace;
begin
	Assert.IsTrue(Space.fromJSON(JSON_SPACE_NORMAL));

	Assert.IsFalse(Space.overquota);
	Assert.AreEqual(Int64(17179869184), Space.total);
	Assert.AreEqual(Int64(5368709120), Space.used);
end;

procedure TCMRSpaceTest.TestFromJSONOverquotaTrue;
var
	Space: TCMRSpace;
begin
	Assert.IsTrue(Space.fromJSON(JSON_SPACE_OVERQUOTA));

	Assert.IsTrue(Space.overquota);
end;

procedure TCMRSpaceTest.TestFromJSONOverquotaFalse;
var
	Space: TCMRSpace;
begin
	Assert.IsTrue(Space.fromJSON(JSON_SPACE_NORMAL));

	Assert.IsFalse(Space.overquota);
end;

procedure TCMRSpaceTest.TestFromJSONLargeValues;
var
	Space: TCMRSpace;
begin
	{ Test with 1TB total and 512GB used }
	Assert.IsTrue(Space.fromJSON(JSON_SPACE_LARGE));

	Assert.AreEqual(Int64(1099511627776), Space.total);
	Assert.AreEqual(Int64(549755813888), Space.used);
end;

procedure TCMRSpaceTest.TestFromJSONInvalid;
var
	Space: TCMRSpace;
begin
	Assert.IsFalse(Space.fromJSON('invalid json'));
end;

procedure TCMRSpaceTest.TestFromJSONEmpty;
var
	Space: TCMRSpace;
begin
	Assert.IsFalse(Space.fromJSON(''));
end;

initialization

TDUnitX.RegisterTestFixture(TCMRSpaceTest);

end.
