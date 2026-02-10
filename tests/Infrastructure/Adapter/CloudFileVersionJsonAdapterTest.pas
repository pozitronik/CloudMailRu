unit CloudFileVersionJsonAdapterTest;

{Unit tests for TCloudFileVersionJsonAdapter - parsing file/history API responses}

interface

uses
	DUnitX.TestFramework,
	CloudFileVersion,
	CloudFileVersionJsonAdapter;

type
	[TestFixture]
	TCloudFileVersionJsonAdapterTest = class
	private
		const
			{Paid account response with hash and rev fields}
			JSON_PAID = '{"body":[{"hash":"DEADBEEF","uid":1,"time":1700000000,"rev":3,"name":"test.txt","path":"/test.txt","size":12345},' +
				'{"hash":"CAFEBABE","uid":2,"time":1700100000,"rev":4,"name":"test.txt","path":"/test.txt","size":23456}]}';

			{Free account response without hash/rev}
			JSON_FREE = '{"body":[{"uid":1,"time":1700000000,"name":"test.txt","path":"/test.txt","size":12345},' +
				'{"uid":2,"time":1700100000,"name":"test.txt","path":"/test.txt","size":23456}]}';

			{Single entry}
			JSON_SINGLE = '{"body":[{"hash":"SINGLE","uid":5,"time":1700050000,"rev":1,"name":"doc.pdf","path":"/doc.pdf","size":99999}]}';

			{Empty body array}
			JSON_EMPTY_BODY = '{"body":[]}';

			{Missing body}
			JSON_NO_BODY = '{"status":200}';

			{Invalid JSON}
			JSON_INVALID = 'not valid json';

			{Empty string}
			JSON_EMPTY = '';
	public
		[Test]
		procedure TestParse_PaidAccount_ReturnsTrue;
		[Test]
		procedure TestParse_PaidAccount_ParsesHash;
		[Test]
		procedure TestParse_PaidAccount_ParsesRev;
		[Test]
		procedure TestParse_PaidAccount_ParsesAllFields;
		[Test]
		procedure TestParse_PaidAccount_ParsesMultipleEntries;
		[Test]
		procedure TestParse_FreeAccount_ReturnsTrue;
		[Test]
		procedure TestParse_FreeAccount_HashIsEmpty;
		[Test]
		procedure TestParse_FreeAccount_RevIsZero;
		[Test]
		procedure TestParse_FreeAccount_ParsesOtherFields;
		[Test]
		procedure TestParse_SingleEntry_ReturnsOneVersion;
		[Test]
		procedure TestParse_EmptyBody_ReturnsTrueWithNoVersions;
		[Test]
		procedure TestParse_NoBody_ReturnsFalse;
		[Test]
		procedure TestParse_InvalidJSON_ReturnsFalse;
		[Test]
		procedure TestParse_EmptyString_ReturnsFalse;
		[Test]
		procedure TestParse_InvalidJSON_SetsEmptyVersions;
		[Test]
		procedure TestParse_LargeNumbers_ParsesCorrectly;
		[Test]
		procedure TestParse_ExtraFields_IgnoredGracefully;
		[Test]
		procedure TestParse_NullBodyField_ReturnsFalse;
		[Test]
		procedure TestParse_MixedPaidFreeEntries_ParsesBoth;
	end;

implementation

procedure TCloudFileVersionJsonAdapterTest.TestParse_PaidAccount_ReturnsTrue;
var
	Versions: TCloudFileVersionList;
begin
	Assert.IsTrue(TCloudFileVersionJsonAdapter.Parse(JSON_PAID, Versions));
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_PaidAccount_ParsesHash;
var
	Versions: TCloudFileVersionList;
begin
	TCloudFileVersionJsonAdapter.Parse(JSON_PAID, Versions);
	Assert.AreEqual('DEADBEEF', Versions[0].Hash);
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_PaidAccount_ParsesRev;
var
	Versions: TCloudFileVersionList;
begin
	TCloudFileVersionJsonAdapter.Parse(JSON_PAID, Versions);
	Assert.AreEqual(3, Versions[0].Rev);
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_PaidAccount_ParsesAllFields;
var
	Versions: TCloudFileVersionList;
begin
	TCloudFileVersionJsonAdapter.Parse(JSON_PAID, Versions);

	Assert.AreEqual('DEADBEEF', Versions[0].Hash);
	Assert.AreEqual('test.txt', Versions[0].Name);
	Assert.AreEqual('/test.txt', Versions[0].Path);
	Assert.AreEqual(Int64(12345), Versions[0].Size);
	Assert.AreEqual(Int64(1700000000), Versions[0].Time);
	Assert.AreEqual(3, Versions[0].Rev);
	Assert.AreEqual(1, Versions[0].UID);
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_PaidAccount_ParsesMultipleEntries;
var
	Versions: TCloudFileVersionList;
begin
	TCloudFileVersionJsonAdapter.Parse(JSON_PAID, Versions);

	Assert.AreEqual(Integer(2), Integer(Length(Versions)), 'Should parse 2 entries');
	Assert.AreEqual('DEADBEEF', Versions[0].Hash);
	Assert.AreEqual('CAFEBABE', Versions[1].Hash);
	Assert.AreEqual(Int64(23456), Versions[1].Size);
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_FreeAccount_ReturnsTrue;
var
	Versions: TCloudFileVersionList;
begin
	Assert.IsTrue(TCloudFileVersionJsonAdapter.Parse(JSON_FREE, Versions));
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_FreeAccount_HashIsEmpty;
var
	Versions: TCloudFileVersionList;
begin
	TCloudFileVersionJsonAdapter.Parse(JSON_FREE, Versions);
	Assert.AreEqual('', Versions[0].Hash, 'Hash should be empty for free account');
	Assert.IsFalse(Versions[0].HasHash, 'HasHash should return False');
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_FreeAccount_RevIsZero;
var
	Versions: TCloudFileVersionList;
begin
	TCloudFileVersionJsonAdapter.Parse(JSON_FREE, Versions);
	Assert.AreEqual(0, Versions[0].Rev, 'Rev should be 0 for free account');
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_FreeAccount_ParsesOtherFields;
var
	Versions: TCloudFileVersionList;
begin
	TCloudFileVersionJsonAdapter.Parse(JSON_FREE, Versions);

	Assert.AreEqual(Integer(2), Integer(Length(Versions)), 'Should parse 2 entries');
	Assert.AreEqual('test.txt', Versions[0].Name);
	Assert.AreEqual('/test.txt', Versions[0].Path);
	Assert.AreEqual(Int64(12345), Versions[0].Size);
	Assert.AreEqual(Int64(1700000000), Versions[0].Time);
	Assert.AreEqual(1, Versions[0].UID);
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_SingleEntry_ReturnsOneVersion;
var
	Versions: TCloudFileVersionList;
begin
	TCloudFileVersionJsonAdapter.Parse(JSON_SINGLE, Versions);

	Assert.AreEqual(Integer(1), Integer(Length(Versions)), 'Should parse 1 entry');
	Assert.AreEqual('SINGLE', Versions[0].Hash);
	Assert.AreEqual('doc.pdf', Versions[0].Name);
	Assert.AreEqual(Int64(99999), Versions[0].Size);
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_EmptyBody_ReturnsTrueWithNoVersions;
var
	Versions: TCloudFileVersionList;
begin
	Assert.IsTrue(TCloudFileVersionJsonAdapter.Parse(JSON_EMPTY_BODY, Versions));
	Assert.AreEqual(Integer(0), Integer(Length(Versions)), 'Should return empty array');
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_NoBody_ReturnsFalse;
var
	Versions: TCloudFileVersionList;
begin
	Assert.IsFalse(TCloudFileVersionJsonAdapter.Parse(JSON_NO_BODY, Versions));
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_InvalidJSON_ReturnsFalse;
var
	Versions: TCloudFileVersionList;
begin
	Assert.IsFalse(TCloudFileVersionJsonAdapter.Parse(JSON_INVALID, Versions));
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_EmptyString_ReturnsFalse;
var
	Versions: TCloudFileVersionList;
begin
	Assert.IsFalse(TCloudFileVersionJsonAdapter.Parse(JSON_EMPTY, Versions));
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_InvalidJSON_SetsEmptyVersions;
var
	Versions: TCloudFileVersionList;
begin
	TCloudFileVersionJsonAdapter.Parse(JSON_INVALID, Versions);
	Assert.AreEqual(Integer(0), Integer(Length(Versions)), 'Invalid JSON should result in empty array');
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_LargeNumbers_ParsesCorrectly;
var
	Versions: TCloudFileVersionList;
const
	{File larger than 2GB, timestamp far in the future}
	JSON_LARGE = '{"body":[{"hash":"BIG","uid":999,"time":4102444800,"rev":100,"name":"big.zip","path":"/big.zip","size":5368709120}]}';
begin
	TCloudFileVersionJsonAdapter.Parse(JSON_LARGE, Versions);

	Assert.AreEqual(Int64(5368709120), Versions[0].Size, 'Should handle sizes larger than 2GB');
	Assert.AreEqual(Int64(4102444800), Versions[0].Time, 'Should handle large timestamps (year 2100)');
	Assert.AreEqual(100, Versions[0].Rev);
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_ExtraFields_IgnoredGracefully;
var
	Versions: TCloudFileVersionList;
const
	{Response with extra fields not in our record}
	JSON_EXTRA = '{"body":[{"hash":"ABC","uid":1,"time":1700000000,"rev":1,"name":"test.txt","path":"/test.txt","size":100,"unknown_field":"value","nested":{"a":1}}]}';
begin
	Assert.IsTrue(TCloudFileVersionJsonAdapter.Parse(JSON_EXTRA, Versions), 'Should succeed with extra fields');
	Assert.AreEqual(Integer(1), Integer(Length(Versions)), 'Should parse 1 entry');
	Assert.AreEqual('ABC', Versions[0].Hash, 'Known fields should be parsed correctly');
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_NullBodyField_ReturnsFalse;
var
	Versions: TCloudFileVersionList;
const
	JSON_NULL_BODY = '{"body":null}';
begin
	Assert.IsFalse(TCloudFileVersionJsonAdapter.Parse(JSON_NULL_BODY, Versions), 'Null body should return False');
end;

procedure TCloudFileVersionJsonAdapterTest.TestParse_MixedPaidFreeEntries_ParsesBoth;
var
	Versions: TCloudFileVersionList;
const
	{First entry has hash (paid), second doesn't (simulating mixed response)}
	JSON_MIXED = '{"body":[' +
		'{"hash":"HASHVAL","uid":1,"time":1700000000,"rev":2,"name":"test.txt","path":"/test.txt","size":1024},' +
		'{"uid":2,"time":1700100000,"name":"test.txt","path":"/test.txt","size":2048}' +
		']}';
begin
	Assert.IsTrue(TCloudFileVersionJsonAdapter.Parse(JSON_MIXED, Versions));
	Assert.AreEqual(Integer(2), Integer(Length(Versions)), 'Should parse both entries');
	Assert.IsTrue(Versions[0].HasHash, 'First entry should have hash');
	Assert.AreEqual('HASHVAL', Versions[0].Hash);
	Assert.IsFalse(Versions[1].HasHash, 'Second entry should not have hash');
	Assert.AreEqual('', Versions[1].Hash);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudFileVersionJsonAdapterTest);

end.
