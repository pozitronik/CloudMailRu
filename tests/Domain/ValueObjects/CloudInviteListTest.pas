unit CloudInviteListTest;

interface

uses
	CloudInviteList,
	CloudInviteListJsonAdapter,
	CloudInvite,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCloudInviteListTest = class
	public
		[Test]
		procedure TestFromJSONValidSingle;
		[Test]
		procedure TestFromJSONValidMultiple;
		[Test]
		procedure TestFromJSONNoInvites;
		[Test]
		procedure TestFromJSONEmptyInvitedArray;
		[Test]
		procedure TestFromJSONInvalidJSON;
		[Test]
		procedure TestFromJSONEmptyString;
		[Test]
		procedure TestInviteFields;
	end;

implementation

const
	{ Single invite }
	JSON_SINGLE_INVITE = '{"status":200,"body":{"invited":[' +
		'{"email":"user@mail.ru","status":"accepted","access":"read_write","name":"User Name"}' +
		']}}';

	{ Multiple invites }
	JSON_MULTIPLE_INVITES = '{"status":200,"body":{"invited":[' +
		'{"email":"user1@mail.ru","status":"accepted","access":"read_write","name":"User One"},' +
		'{"email":"user2@mail.ru","status":"pending","access":"read_only","name":"User Two"},' +
		'{"email":"user3@mail.ru","status":"rejected","access":"read_only","name":"User Three"}' +
		']}}';

	{ No invited array - should return true with empty list }
	JSON_NO_INVITES = '{"status":200,"body":{}}';

	{ Empty invited array }
	JSON_EMPTY_INVITES = '{"status":200,"body":{"invited":[]}}';

procedure TCloudInviteListTest.TestFromJSONValidSingle;
var
	List: TCloudInviteList;
begin
	Assert.IsTrue(TCloudInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List));

	Assert.AreEqual(Integer(1), Integer(Length(List)));
end;

procedure TCloudInviteListTest.TestFromJSONValidMultiple;
var
	List: TCloudInviteList;
begin
	Assert.IsTrue(TCloudInviteListJsonAdapter.Parse(JSON_MULTIPLE_INVITES, List));

	Assert.AreEqual(Integer(3), Integer(Length(List)));
end;

procedure TCloudInviteListTest.TestFromJSONNoInvites;
var
	List: TCloudInviteList;
begin
	{ When there's no "invited" array, should return true with empty list }
	Assert.IsTrue(TCloudInviteListJsonAdapter.Parse(JSON_NO_INVITES, List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudInviteListTest.TestFromJSONEmptyInvitedArray;
var
	List: TCloudInviteList;
begin
	Assert.IsTrue(TCloudInviteListJsonAdapter.Parse(JSON_EMPTY_INVITES, List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudInviteListTest.TestFromJSONInvalidJSON;
var
	List: TCloudInviteList;
begin
	Assert.IsFalse(TCloudInviteListJsonAdapter.Parse('invalid json', List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudInviteListTest.TestFromJSONEmptyString;
var
	List: TCloudInviteList;
begin
	Assert.IsFalse(TCloudInviteListJsonAdapter.Parse('', List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudInviteListTest.TestInviteFields;
var
	List: TCloudInviteList;
begin
	TCloudInviteListJsonAdapter.Parse(JSON_MULTIPLE_INVITES, List);

	{ Check first invite }
	Assert.AreEqual('user1@mail.ru', List[0].email);
	Assert.AreEqual('accepted', List[0].status);
	Assert.AreEqual('read_write', List[0].access);
	Assert.AreEqual('User One', List[0].name);

	{ Check second invite }
	Assert.AreEqual('user2@mail.ru', List[1].email);
	Assert.AreEqual('pending', List[1].status);
	Assert.AreEqual('read_only', List[1].access);
	Assert.AreEqual('User Two', List[1].name);

	{ Check third invite }
	Assert.AreEqual('user3@mail.ru', List[2].email);
	Assert.AreEqual('rejected', List[2].status);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudInviteListTest);

end.
