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
		procedure TestExtract_upload_url_EarlyPosition;
		[Test]
		procedure TestExtract_upload_url_NotFound;
		[Test]
		procedure TestExtract_upload_url_NoHttpsPrefix;
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

		{Edge cases for extractNearValue - missing delimiters after anchor found}
		[Test]
		procedure TestExtractNearValue_AnchorFoundStartCharMissing;
		[Test]
		procedure TestExtractNearValue_AnchorFoundEndCharMissing;
		[Test]
		procedure TestExtractNearValue_BothDelimitersMissing;
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
	{ Standard case with URL in the middle of text }
	Text := '0123456789012345678901234567890123456789012345678901234567890https://cloclo1.cloud.mail.ru/upload/" more text';
	Assert.IsTrue(extract_upload_url_FromText(Text, UploadUrl));
	Assert.IsNotEmpty(UploadUrl);
	Assert.StartsWith('https://', UploadUrl);
end;

procedure TParsingHelperTest.TestExtract_upload_url_EarlyPosition;
var
	Text, UploadUrl: WideString;
begin
	{ Edge case: URL appears early in text (position < 50).
	  Previously caused Cardinal underflow in 'start - 50'. }
	Text := 'https://cloclo1.cloud.mail.ru/upload/" end';
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
	Assert.IsEmpty(UploadUrl);
end;

procedure TParsingHelperTest.TestExtract_upload_url_NoHttpsPrefix;
var
	Text, UploadUrl: WideString;
begin
	{ Pattern found but no https:// prefix - should return false }
	Text := 'some text mail.ru/upload/" without https prefix';
	Assert.IsFalse(extract_upload_url_FromText(Text, UploadUrl));
	Assert.IsEmpty(UploadUrl);
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

procedure TParsingHelperTest.TestExtractNearValue_AnchorFoundStartCharMissing;
var
	Text: WideString;
begin
	{Anchor found but StartChar (") not found after anchor.
	 Pos returns 0, then start = 0 + 1 = 1, end_ could be anywhere or 0.
	 This tests the edge case behavior.}
	Text := '"csrf":no_quotes_here';
	{Should return empty or partial string - verify it doesn't crash}
	extractNearValue(Text, '"csrf"');
	Assert.Pass('No crash when StartChar missing after anchor');
end;

procedure TParsingHelperTest.TestExtractNearValue_AnchorFoundEndCharMissing;
var
	Text, Result: WideString;
begin
	{Anchor found, StartChar found, but EndChar not found.
	 This causes end_ = 0, resulting in Copy(Text, start, 0 - start) = negative length.
	 Delphi's Copy handles negative length gracefully by returning empty string.}
	Text := '"csrf":"value_without_closing_quote';
	Result := extractNearValue(Text, '"csrf"');
	{Delphi Copy with negative count returns empty string}
	Assert.AreEqual('', Result, 'Should return empty when EndChar not found');
end;

procedure TParsingHelperTest.TestExtractNearValue_BothDelimitersMissing;
var
	Text, Result: WideString;
begin
	{Anchor found but neither StartChar nor EndChar present}
	Text := '"csrf":value';
	Result := extractNearValue(Text, '"csrf"');
	{Both Pos calls return 0, Copy with negative length returns empty}
	Assert.AreEqual('', Result, 'Should return empty when delimiters missing');
end;

initialization

TDUnitX.RegisterTestFixture(TParsingHelperTest);

end.
