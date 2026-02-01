unit ParsingHelperTest;

interface

uses
	ParsingHelper,
	DUnitX.TestFramework;

type

	[TestFixture]
	TParsingHelperTest = class
	public
		[Test]
		procedure TestExtractPublicShard;
		[Test]
		procedure TestExtractEmailPartsValid;
		[Test]
		procedure TestExtractEmailPartsNoAt;
		[Test]
		procedure TestExtractEmailPartsAtEnd;
		[Test]
		procedure TestExtractEmailPartsAtStart;

	end;

implementation

procedure TParsingHelperTest.TestExtractPublicShard;
var
	Text, Shard: WideString;
begin
	Text := '"weblink_get":{"count":1,"url":"https://shard.example.com/"}';
	Assert.IsTrue(extractPublicShard(Text, Shard));
	Assert.AreEqual('https://shard.example.com/', Shard);
end;

procedure TParsingHelperTest.TestExtractEmailPartsValid;
var
	Username, Domain: WideString;
begin
	Assert.IsTrue(ExtractEmailParts('user@example.com', Username, Domain));
	Assert.AreEqual('user', Username);
	Assert.AreEqual('example.com', Domain);
end;

procedure TParsingHelperTest.TestExtractEmailPartsNoAt;
var
	Username, Domain: WideString;
begin
	Assert.IsFalse(ExtractEmailParts('invalidemail', Username, Domain));
end;

procedure TParsingHelperTest.TestExtractEmailPartsAtEnd;
var
	Username, Domain: WideString;
begin
	{ @ at the end means empty domain - should fail }
	Assert.IsFalse(ExtractEmailParts('user@', Username, Domain));
end;

procedure TParsingHelperTest.TestExtractEmailPartsAtStart;
var
	Username, Domain: WideString;
begin
	{ @ at the start means empty username - should succeed per implementation }
	Assert.IsTrue(ExtractEmailParts('@domain.com', Username, Domain));
	Assert.AreEqual('', Username);
	Assert.AreEqual('domain.com', Domain);
end;

initialization

TDUnitX.RegisterTestFixture(TParsingHelperTest);

end.
