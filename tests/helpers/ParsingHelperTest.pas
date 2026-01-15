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
		procedure TestExtractNearValueSimple;
		[Test]
		procedure TestExtractNearValueNotFound;
		[Test]
		procedure TestExtractNearValueCustomDelimiters;
		[Test]
		procedure TestExtractTokenFromText;
		[Test]
		procedure TestExtractTokenFromTextNotFound;
		[Test]
		procedure TestExtractPublicTokenFromText;
		[Test]
		procedure TestExtract_x_page_id_FromText;
		[Test]
		procedure TestExtract_build_FromText;
		[Test]
		procedure TestExtract_upload_url_FromText;
		[Test]
		procedure TestExtract_upload_url_NotFound;
		[Test]
		procedure TestExtractPublicShard;
		[Test]
		procedure TestExtractTwostepJson;
		[Test]
		procedure TestExtractTwostepJsonNotFound;
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

procedure TParsingHelperTest.TestExtractNearValueSimple;
var
	Text: WideString;
begin
	Text := '"csrf":"abc123token"';
	Assert.AreEqual('abc123token', extractNearValue(Text, '"csrf"'));
end;

procedure TParsingHelperTest.TestExtractNearValueNotFound;
var
	Text: WideString;
begin
	Text := '"other":"value"';
	Assert.AreEqual('', extractNearValue(Text, '"csrf"'));
end;

procedure TParsingHelperTest.TestExtractNearValueCustomDelimiters;
var
	Text: WideString;
begin
	Text := '"key":{inner_value}';
	Assert.AreEqual('inner_value', extractNearValue(Text, '"key":', '{', '}'));
end;

procedure TParsingHelperTest.TestExtractTokenFromText;
var
	Text, Token: WideString;
begin
	Text := '{"data":{"csrf":"mytoken123"}}';
	Assert.IsTrue(extractTokenFromText(Text, Token));
	Assert.AreEqual('mytoken123', Token);
end;

procedure TParsingHelperTest.TestExtractTokenFromTextNotFound;
var
	Text, Token: WideString;
begin
	Text := '{"data":{"other":"value"}}';
	Assert.IsFalse(extractTokenFromText(Text, Token));
end;

procedure TParsingHelperTest.TestExtractPublicTokenFromText;
var
	Text, Token: WideString;
begin
	Text := '{"tokens":{"download":"public_download_token"}}';
	Assert.IsTrue(extractPublicTokenFromText(Text, Token));
	Assert.AreEqual('public_download_token', Token);
end;

procedure TParsingHelperTest.TestExtract_x_page_id_FromText;
var
	Text, PageId: WideString;
begin
	Text := '"x-page-id":"page123"';
	Assert.IsTrue(extract_x_page_id_FromText(Text, PageId));
	Assert.AreEqual('page123', PageId);
end;

procedure TParsingHelperTest.TestExtract_build_FromText;
var
	Text, Build: WideString;
begin
	Text := '"BUILD":"release-2024.01"';
	Assert.IsTrue(extract_build_FromText(Text, Build));
	Assert.AreEqual('release-2024.01', Build);
end;

procedure TParsingHelperTest.TestExtract_upload_url_FromText;
var
	Text, UploadUrl: WideString;
begin
	{ Text must have 'mail.ru/upload/"' at position > 50 to avoid integer overflow bug in production code.
	  BUG FOUND: extract_upload_url_FromText does 'start1 := start - 50' with Cardinal types,
	  causing overflow when match position < 50. }
	Text := '0123456789012345678901234567890123456789012345678901234567890https://cloclo1.cloud.mail.ru/upload/" more text';
	Assert.IsTrue(extract_upload_url_FromText(Text, UploadUrl));
	Assert.IsNotEmpty(UploadUrl);
	Assert.StartsWith('https://', UploadUrl);
end;

procedure TParsingHelperTest.TestExtract_upload_url_NotFound;
var
	Text, UploadUrl: WideString;
begin
	Text := 'no upload url here';
	Assert.IsFalse(extract_upload_url_FromText(Text, UploadUrl));
end;

procedure TParsingHelperTest.TestExtractPublicShard;
var
	Text, Shard: WideString;
begin
	Text := '"weblink_get":{"count":1,"url":"https://shard.example.com/"}';
	Assert.IsTrue(extractPublicShard(Text, Shard));
	Assert.AreEqual('https://shard.example.com/', Shard);
end;

procedure TParsingHelperTest.TestExtractTwostepJson;
var
	Text, JSON: WideString;
begin
	Text := '<html><script type="text/html" id="json">{"key":"value"}</script></html>';
	Assert.IsTrue(extractTwostepJson(Text, JSON));
	Assert.AreEqual('{"key":"value"}', JSON);
end;

procedure TParsingHelperTest.TestExtractTwostepJsonNotFound;
var
	Text, JSON: WideString;
begin
	Text := '<html><body>No json script here</body></html>';
	Assert.IsFalse(extractTwostepJson(Text, JSON));
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
