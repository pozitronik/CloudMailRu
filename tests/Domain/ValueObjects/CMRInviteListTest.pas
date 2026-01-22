unit CMRInviteListTest;

interface

uses
	CMRInviteList,
	CMRInviteListJsonAdapter,
	CMRInvite,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCMRInviteListTest = class
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

procedure TCMRInviteListTest.TestFromJSONValidSingle;
var
	List: TCMRInviteList;
begin
	Assert.IsTrue(TCMRInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List));

	Assert.AreEqual(Integer(1), Integer(Length(List)));
end;

procedure TCMRInviteListTest.TestFromJSONValidMultiple;
var
	List: TCMRInviteList;
begin
	Assert.IsTrue(TCMRInviteListJsonAdapter.Parse(JSON_MULTIPLE_INVITES, List));

	Assert.AreEqual(Integer(3), Integer(Length(List)));
end;

procedure TCMRInviteListTest.TestFromJSONNoInvites;
var
	List: TCMRInviteList;
begin
	{ When there's no "invited" array, should return true with empty list }
	Assert.IsTrue(TCMRInviteListJsonAdapter.Parse(JSON_NO_INVITES, List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRInviteListTest.TestFromJSONEmptyInvitedArray;
var
	List: TCMRInviteList;
begin
	Assert.IsTrue(TCMRInviteListJsonAdapter.Parse(JSON_EMPTY_INVITES, List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRInviteListTest.TestFromJSONInvalidJSON;
var
	List: TCMRInviteList;
begin
	Assert.IsFalse(TCMRInviteListJsonAdapter.Parse('invalid json', List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRInviteListTest.TestFromJSONEmptyString;
var
	List: TCMRInviteList;
begin
	Assert.IsFalse(TCMRInviteListJsonAdapter.Parse('', List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRInviteListTest.TestInviteFields;
var
	List: TCMRInviteList;
begin
	TCMRInviteListJsonAdapter.Parse(JSON_MULTIPLE_INVITES, List);

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

TDUnitX.RegisterTestFixture(TCMRInviteListTest);

end.
