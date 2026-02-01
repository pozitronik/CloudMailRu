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

initialization

TDUnitX.RegisterTestFixture(TParsingHelperTest);

end.
