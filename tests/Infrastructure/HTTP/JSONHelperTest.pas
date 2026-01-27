unit JSONHelperTest;

interface

uses
	JSONHelper,
	CloudConstants,
	JSON,
	DUnitX.TestFramework;

type

	[TestFixture]
	TJSONHelperTest = class
	public
		[Test]
		procedure TestInitValid;
		[Test]
		procedure TestInitInvalid;
		[Test]
		procedure TestInitEmpty;
		[Test]
		procedure TestGetPublicLinkValid;
		[Test]
		procedure TestGetPublicLinkInvalid;
		[Test]
		procedure TestGetShardValid;
		[Test]
		procedure TestGetShardInvalid;
		[Test]
		procedure TestGetBodyErrorValid;
		[Test]
		procedure TestGetBodyErrorInvalid;
		[Test]
		procedure TestGetBodyTokenValid;
		[Test]
		procedure TestGetBodyTokenInvalid;
		[Test]
		procedure TestAssignFromNameWideString;
		[Test]
		procedure TestAssignFromNameInt64;
		[Test]
		procedure TestAssignFromNameInteger;
		[Test]
		procedure TestAssignFromNameBoolean;
		[Test]
		procedure TestAssignFromNameMissing;
	end;

implementation

const
	JSON_PUBLIC_LINK = '{"status":200,"body":"/ABC123/shared_file"}';
	JSON_SHARD = '{"status":200,"body":{"get":[{"url":"https://cloclo1.cloud.mail.ru/get/"}]}}';
	JSON_BODY_ERROR = '{"status":400,"body":"token"}';
	JSON_BODY_TOKEN = '{"status":200,"body":{"token":"csrf_token_value"}}';
	JSON_TEST_OBJECT = '{"name":"test","size":12345,"count":42,"active":true}';

procedure TJSONHelperTest.TestInitValid;
var
	JSONVal: TJSONObject;
begin
	Assert.IsTrue(init('{"key":"value"}', JSONVal));
	Assert.IsNotNull(JSONVal);
	JSONVal.Free;
end;

procedure TJSONHelperTest.TestInitInvalid;
var
	JSONVal: TJSONObject;
begin
	Assert.IsFalse(init('not valid json', JSONVal));
end;

procedure TJSONHelperTest.TestInitEmpty;
var
	JSONVal: TJSONObject;
begin
	Assert.IsFalse(init('', JSONVal));
end;

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

procedure TJSONHelperTest.TestAssignFromNameWideString;
var
	JSONVal: TJSONObject;
	Value: WideString;
begin
	Value := '';
	init(JSON_TEST_OBJECT, JSONVal);
	assignFromName('name', JSONVal, Value);

	Assert.AreEqual('test', Value);
	JSONVal.Free;
end;

procedure TJSONHelperTest.TestAssignFromNameInt64;
var
	JSONVal: TJSONObject;
	Value: Int64;
begin
	Value := 0;
	init(JSON_TEST_OBJECT, JSONVal);
	assignFromName('size', JSONVal, Value);

	Assert.AreEqual(Int64(12345), Value);
	JSONVal.Free;
end;

procedure TJSONHelperTest.TestAssignFromNameInteger;
var
	JSONVal: TJSONObject;
	Value: Integer;
begin
	Value := 0;
	init(JSON_TEST_OBJECT, JSONVal);
	assignFromName('count', JSONVal, Value);

	Assert.AreEqual(42, Value);
	JSONVal.Free;
end;

procedure TJSONHelperTest.TestAssignFromNameBoolean;
var
	JSONVal: TJSONObject;
	Value: Boolean;
begin
	Value := False;
	init(JSON_TEST_OBJECT, JSONVal);
	assignFromName('active', JSONVal, Value);

	Assert.IsTrue(Value);
	JSONVal.Free;
end;

procedure TJSONHelperTest.TestAssignFromNameMissing;
var
	JSONVal: TJSONObject;
	Value: WideString;
begin
	Value := 'default';
	init(JSON_TEST_OBJECT, JSONVal);
	assignFromName('nonexistent', JSONVal, Value);

	{ Value should remain unchanged when key is missing }
	Assert.AreEqual('default', Value);
	JSONVal.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TJSONHelperTest);

end.
