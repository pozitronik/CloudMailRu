unit ParsingHelperTest;

interface

uses
	ParsingHelper,
	DUnitX.TestFramework;

type

	[TestFixture]
	TParsingHelperTest = class
	public
		{Happy path}
		[Test]
		procedure TestExtractPublicShard;
		[Test]
		procedure TestExtractPublicShardWithWhitespace;
		[Test]
		procedure TestExtractPublicShardWithNewlines;
		[Test]
		procedure TestExtractPublicShardWithTabs;
		[Test]
		procedure TestExtractPublicShardWithMixedWhitespace;

		{Multiple shards in response - weblink_get should be found}
		[Test]
		procedure TestExtractPublicShardFromFullDispatcherResponse;

		{Failure paths}
		[Test]
		procedure TestExtractPublicShardEmptyInput;
		[Test]
		procedure TestExtractPublicShardNoWeblinkGet;
		[Test]
		procedure TestExtractPublicShardMissingUrl;
		[Test]
		procedure TestExtractPublicShardEmptyUrl;
		[Test]
		procedure TestExtractPublicShardMalformedJson;
		[Test]
		procedure TestExtractPublicShardOnlyAnchorNoBody;

		{Edge cases}
		[Test]
		procedure TestExtractPublicShardTrailingSlash;
		[Test]
		procedure TestExtractPublicShardNoTrailingSlash;
	end;

implementation

procedure TParsingHelperTest.TestExtractPublicShard;
var
	Shard: WideString;
begin
	Assert.IsTrue(extractPublicShard('"weblink_get":{"count":1,"url":"https://shard.example.com/"}', Shard));
	Assert.AreEqual('https://shard.example.com/', Shard);
end;

procedure TParsingHelperTest.TestExtractPublicShardWithWhitespace;
var
	Shard: WideString;
begin
	{Spaces around JSON structure should be stripped before parsing}
	Assert.IsTrue(extractPublicShard('"weblink_get" : { "count" : 1, "url" : "https://cloclo1.cloud.mail.ru/get/" }', Shard));
	Assert.AreEqual('https://cloclo1.cloud.mail.ru/get/', Shard);
end;

procedure TParsingHelperTest.TestExtractPublicShardWithNewlines;
var
	Shard: WideString;
begin
	{Newlines (LF and CR) should be stripped before parsing}
	Assert.IsTrue(extractPublicShard('"weblink_get":'#$A'{"count":1,'#$D#$A'"url":"https://shard.example.com/"}', Shard));
	Assert.AreEqual('https://shard.example.com/', Shard);
end;

procedure TParsingHelperTest.TestExtractPublicShardWithTabs;
var
	Shard: WideString;
begin
	{Tab characters should be stripped before parsing}
	Assert.IsTrue(extractPublicShard('"weblink_get":'#9'{"count":1,"url":"https://shard.example.com/"}', Shard));
	Assert.AreEqual('https://shard.example.com/', Shard);
end;

procedure TParsingHelperTest.TestExtractPublicShardWithMixedWhitespace;
var
	Shard: WideString;
begin
	{All whitespace types combined}
	Assert.IsTrue(extractPublicShard(
		'"weblink_get" :'#$D#$A#9'{ "count" : 1 ,'#$A#9'"url" : "https://shard.example.com/" }',
		Shard));
	Assert.AreEqual('https://shard.example.com/', Shard);
end;

procedure TParsingHelperTest.TestExtractPublicShardFromFullDispatcherResponse;
var
	Shard: WideString;
begin
	{Realistic dispatcher response with multiple shard types - weblink_get must be found}
	Assert.IsTrue(extractPublicShard(
		'"get":{"count":1,"url":"https://cloclo1.cloud.mail.ru/get/"},' +
		'"upload":{"count":1,"url":"https://cloclo1.cloud.mail.ru/upload/"},' +
		'"weblink_get":{"count":1,"url":"https://cloclo1.cloud.mail.ru/weblink_get/"},' +
		'"thumbnails":{"count":1,"url":"https://cloclo1.cloud.mail.ru/thumbnails/"}',
		Shard));
	Assert.AreEqual('https://cloclo1.cloud.mail.ru/weblink_get/', Shard);
end;

procedure TParsingHelperTest.TestExtractPublicShardEmptyInput;
var
	Shard: WideString;
begin
	Assert.IsFalse(extractPublicShard('', Shard));
	Assert.AreEqual('', Shard);
end;

procedure TParsingHelperTest.TestExtractPublicShardNoWeblinkGet;
var
	Shard: WideString;
begin
	{Response without weblink_get shard}
	Assert.IsFalse(extractPublicShard('"get":{"count":1,"url":"https://shard.example.com/"}', Shard));
end;

procedure TParsingHelperTest.TestExtractPublicShardMissingUrl;
var
	Shard: WideString;
begin
	{weblink_get present but no url field inside}
	Assert.IsFalse(extractPublicShard('"weblink_get":{"count":1}', Shard));
end;

procedure TParsingHelperTest.TestExtractPublicShardEmptyUrl;
var
	Shard: WideString;
begin
	{url field present but empty value}
	Assert.IsFalse(extractPublicShard('"weblink_get":{"count":1,"url":""}', Shard));
end;

procedure TParsingHelperTest.TestExtractPublicShardMalformedJson;
var
	Shard: WideString;
begin
	{Broken JSON - anchor found but no proper delimiters}
	Assert.IsFalse(extractPublicShard('"weblink_get":not-json', Shard));
end;

procedure TParsingHelperTest.TestExtractPublicShardOnlyAnchorNoBody;
var
	Shard: WideString;
begin
	{Anchor found but nothing follows}
	Assert.IsFalse(extractPublicShard('"weblink_get":', Shard));
end;

procedure TParsingHelperTest.TestExtractPublicShardTrailingSlash;
var
	Shard: WideString;
begin
	Assert.IsTrue(extractPublicShard('"weblink_get":{"url":"https://shard.example.com/path/"}', Shard));
	Assert.AreEqual('https://shard.example.com/path/', Shard);
end;

procedure TParsingHelperTest.TestExtractPublicShardNoTrailingSlash;
var
	Shard: WideString;
begin
	Assert.IsTrue(extractPublicShard('"weblink_get":{"url":"https://shard.example.com/path"}', Shard));
	Assert.AreEqual('https://shard.example.com/path', Shard);
end;

initialization

TDUnitX.RegisterTestFixture(TParsingHelperTest);

end.
