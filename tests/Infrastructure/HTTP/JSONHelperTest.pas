unit JSONHelperTest;

interface

uses
	JSONHelper,
	CloudConstants,
	DUnitX.TestFramework;

type

	[TestFixture]
	TJSONHelperTest = class
	public
		[Test]
		procedure TestGetPublicLinkValid;
		[Test]
		procedure TestGetPublicLinkInvalid;
		[Test]
		procedure TestGetPublicLinkEmpty;
		[Test]
		procedure TestGetPublicLinkMalformed;
		[Test]
		procedure TestGetShardValid;
		[Test]
		procedure TestGetShardInvalid;
		[Test]
		procedure TestGetShardMissingShardType;
		[Test]
		procedure TestGetShardEmptyArray;
		[Test]
		procedure TestGetBodyErrorValid;
		[Test]
		procedure TestGetBodyErrorInvalid;
		[Test]
		procedure TestGetBodyErrorEmpty;
		[Test]
		procedure TestGetBodyTokenValid;
		[Test]
		procedure TestGetBodyTokenInvalid;
		[Test]
		procedure TestGetBodyTokenMissingToken;
		{isNotAuthorizedError tests}
		[Test]
		procedure TestIsNotAuthorizedError_ValidResponse;
		[Test]
		procedure TestIsNotAuthorizedError_DifferentError;
		[Test]
		procedure TestIsNotAuthorizedError_EmptyString;
		[Test]
		procedure TestIsNotAuthorizedError_InvalidJSON;
		[Test]
		procedure TestIsNotAuthorizedError_BodyTokenError;
	end;

implementation

const
	JSON_PUBLIC_LINK = '{"status":200,"body":"/ABC123/shared_file"}';
	JSON_SHARD = '{"status":200,"body":{"get":[{"url":"https://cloclo1.cloud.mail.ru/get/"}]}}';
	JSON_SHARD_UPLOAD = '{"status":200,"body":{"upload":[{"url":"https://cloclo1.cloud.mail.ru/upload/"}]}}';
	JSON_BODY_ERROR = '{"status":400,"body":"token"}';
	JSON_BODY_TOKEN = '{"status":200,"body":{"token":"csrf_token_value"}}';

procedure TJSONHelperTest.TestGetPublicLinkValid;
var
	Link: WideString;
begin
	Assert.IsTrue(getPublicLink(JSON_PUBLIC_LINK, Link));
	Assert.AreEqual('/ABC123/shared_file', Link);
end;

procedure TJSONHelperTest.TestGetPublicLinkInvalid;
var
	Link: WideString;
begin
	Assert.IsFalse(getPublicLink('invalid', Link));
end;

procedure TJSONHelperTest.TestGetPublicLinkEmpty;
var
	Link: WideString;
begin
	Assert.IsFalse(getPublicLink('', Link));
end;

procedure TJSONHelperTest.TestGetPublicLinkMalformed;
var
	Link: WideString;
begin
	{Body is object instead of string}
	Assert.IsFalse(getPublicLink('{"status":200,"body":{"nested":"value"}}', Link));
end;

procedure TJSONHelperTest.TestGetShardValid;
var
	Shard: WideString;
begin
	Assert.IsTrue(getShard(JSON_SHARD, Shard, SHARD_TYPE_GET));
	Assert.AreEqual('https://cloclo1.cloud.mail.ru/get/', Shard);
end;

procedure TJSONHelperTest.TestGetShardInvalid;
var
	Shard: WideString;
begin
	Assert.IsFalse(getShard('invalid', Shard));
end;

procedure TJSONHelperTest.TestGetShardMissingShardType;
var
	Shard: WideString;
begin
	{Request upload shard from JSON that only has get shard}
	Assert.IsFalse(getShard(JSON_SHARD, Shard, 'upload'));
end;

procedure TJSONHelperTest.TestGetShardEmptyArray;
var
	Shard: WideString;
begin
	{Empty array for shard type}
	Assert.IsFalse(getShard('{"status":200,"body":{"get":[]}}', Shard, SHARD_TYPE_GET));
end;

procedure TJSONHelperTest.TestGetBodyErrorValid;
var
	Error: WideString;
begin
	Error := getBodyError(JSON_BODY_ERROR);
	Assert.AreEqual('token', Error);
end;

procedure TJSONHelperTest.TestGetBodyErrorInvalid;
var
	Error: WideString;
begin
	Error := getBodyError('invalid');
	Assert.AreEqual('', Error);
end;

procedure TJSONHelperTest.TestGetBodyErrorEmpty;
var
	Error: WideString;
begin
	Error := getBodyError('');
	Assert.AreEqual('', Error);
end;

procedure TJSONHelperTest.TestGetBodyTokenValid;
var
	Token: WideString;
begin
	Assert.IsTrue(getBodyToken(JSON_BODY_TOKEN, Token));
	Assert.AreEqual('csrf_token_value', Token);
end;

procedure TJSONHelperTest.TestGetBodyTokenInvalid;
var
	Token: WideString;
begin
	Assert.IsFalse(getBodyToken('invalid', Token));
end;

procedure TJSONHelperTest.TestGetBodyTokenMissingToken;
var
	Token: WideString;
begin
	{Body exists but token field is missing}
	Assert.IsFalse(getBodyToken('{"status":200,"body":{"other":"value"}}', Token));
end;

procedure TJSONHelperTest.TestIsNotAuthorizedError_ValidResponse;
begin
	Assert.IsTrue(isNotAuthorizedError('{"error":"NOT/AUTHORIZED"}'));
end;

procedure TJSONHelperTest.TestIsNotAuthorizedError_DifferentError;
begin
	Assert.IsFalse(isNotAuthorizedError('{"error":"SOME_OTHER_ERROR"}'));
end;

procedure TJSONHelperTest.TestIsNotAuthorizedError_EmptyString;
begin
	Assert.IsFalse(isNotAuthorizedError(''));
end;

procedure TJSONHelperTest.TestIsNotAuthorizedError_InvalidJSON;
begin
	Assert.IsFalse(isNotAuthorizedError('not valid json'));
end;

procedure TJSONHelperTest.TestIsNotAuthorizedError_BodyTokenError;
begin
	{The old-style token error should NOT match isNotAuthorizedError}
	Assert.IsFalse(isNotAuthorizedError('{"body":"token"}'));
end;

initialization

TDUnitX.RegisterTestFixture(TJSONHelperTest);

end.
