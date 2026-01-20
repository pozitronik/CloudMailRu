unit CMRIncomingInviteListTest;

interface

uses
	CMRIncomingInviteList,
	CMRIncomingInvite,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCMRIncomingInviteListTest = class
	public
		[Test]
		procedure TestFromJSONValidSingle;
		[Test]
		procedure TestFromJSONValidMultiple;
		[Test]
		procedure TestFromJSONWithOwner;
		[Test]
		procedure TestFromJSONMountedItem;
		[Test]
		procedure TestFromJSONUnmountedItem;
		[Test]
		procedure TestFromJSONNoList;
		[Test]
		procedure TestFromJSONEmptyList;
		[Test]
		procedure TestFromJSONInvalidJSON;
		[Test]
		procedure TestFromJSONEmptyString;
		[Test]
		procedure TestFindByNameFound;
		[Test]
		procedure TestFindByNameNotFound;
		[Test]
		procedure TestFindByNameEmptyList;
	end;

implementation

const
	{ Single incoming invite with all fields }
	JSON_SINGLE_INVITE = '{"status":200,"body":{"list":[' +
		'{"owner":{"email":"owner@mail.ru","name":"Owner Name"},"tree":"abc123","access":"read_write","name":"SharedFolder","size":1024,"home":"/SharedFolder","invite_token":"token123"}' +
		']}}';

	{ Multiple incoming invites }
	JSON_MULTIPLE_INVITES = '{"status":200,"body":{"list":[' +
		'{"owner":{"email":"user1@mail.ru","name":"User One"},"name":"Folder1","size":100,"access":"read_only"},' +
		'{"owner":{"email":"user2@mail.ru","name":"User Two"},"name":"Folder2","size":200,"access":"read_write","home":"/Folder2"},' +
		'{"owner":{"email":"user3@mail.ru","name":"User Three"},"name":"Folder3","size":300,"invite_token":"token_abc"}' +
		']}}';

	{ Invite without owner (edge case) }
	JSON_NO_OWNER = '{"status":200,"body":{"list":[' +
		'{"name":"NoOwnerFolder","size":500,"access":"read_only"}' +
		']}}';

	{ No list array }
	JSON_NO_LIST = '{"status":200,"body":{}}';

	{ Empty list }
	JSON_EMPTY_LIST = '{"status":200,"body":{"list":[]}}';

procedure TCMRIncomingInviteListTest.TestFromJSONValidSingle;
var
	List: TCMRIncomingInviteList;
begin
	Assert.IsTrue(List.FromJSON(JSON_SINGLE_INVITE));

	Assert.AreEqual(Integer(1), Integer(Length(List)));
end;

procedure TCMRIncomingInviteListTest.TestFromJSONValidMultiple;
var
	List: TCMRIncomingInviteList;
begin
	Assert.IsTrue(List.FromJSON(JSON_MULTIPLE_INVITES));

	Assert.AreEqual(Integer(3), Integer(Length(List)));
end;

procedure TCMRIncomingInviteListTest.TestFromJSONWithOwner;
var
	List: TCMRIncomingInviteList;
begin
	List.FromJSON(JSON_SINGLE_INVITE);

	Assert.AreEqual('owner@mail.ru', List[0].owner.email);
	Assert.AreEqual('Owner Name', List[0].owner.name);
end;

procedure TCMRIncomingInviteListTest.TestFromJSONMountedItem;
var
	List: TCMRIncomingInviteList;
begin
	List.FromJSON(JSON_SINGLE_INVITE);

	{ Item with home path is considered mounted }
	Assert.AreEqual('/SharedFolder', List[0].home);
	Assert.IsTrue(List[0].isMounted);
end;

procedure TCMRIncomingInviteListTest.TestFromJSONUnmountedItem;
var
	List: TCMRIncomingInviteList;
begin
	List.FromJSON(JSON_MULTIPLE_INVITES);

	{ First item has no home path - not mounted }
	Assert.IsFalse(List[0].isMounted);

	{ Second item has home path - mounted }
	Assert.IsTrue(List[1].isMounted);

	{ Third item has no home path - not mounted }
	Assert.IsFalse(List[2].isMounted);
end;

procedure TCMRIncomingInviteListTest.TestFromJSONNoList;
var
	List: TCMRIncomingInviteList;
begin
	{ When there's no list array, should return false }
	Assert.IsFalse(List.FromJSON(JSON_NO_LIST));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRIncomingInviteListTest.TestFromJSONEmptyList;
var
	List: TCMRIncomingInviteList;
begin
	Assert.IsTrue(List.FromJSON(JSON_EMPTY_LIST));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRIncomingInviteListTest.TestFromJSONInvalidJSON;
var
	List: TCMRIncomingInviteList;
begin
	Assert.IsFalse(List.FromJSON('invalid json'));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRIncomingInviteListTest.TestFromJSONEmptyString;
var
	List: TCMRIncomingInviteList;
begin
	Assert.IsFalse(List.FromJSON(''));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRIncomingInviteListTest.TestFindByNameFound;
var
	List: TCMRIncomingInviteList;
	Item: TCMRIncomingInvite;
begin
	List.FromJSON(JSON_MULTIPLE_INVITES);

	Item := List.FindByName('Folder2');

	Assert.IsFalse(Item.isNone);
	Assert.AreEqual('Folder2', Item.name);
	Assert.AreEqual('user2@mail.ru', Item.owner.email);
	Assert.AreEqual(Int64(200), Item.size);
end;

procedure TCMRIncomingInviteListTest.TestFindByNameNotFound;
var
	List: TCMRIncomingInviteList;
	Item: TCMRIncomingInvite;
begin
	List.FromJSON(JSON_MULTIPLE_INVITES);

	Item := List.FindByName('NonexistentFolder');

	Assert.IsTrue(Item.isNone);
end;

procedure TCMRIncomingInviteListTest.TestFindByNameEmptyList;
var
	List: TCMRIncomingInviteList;
	Item: TCMRIncomingInvite;
begin
	List.FromJSON(JSON_EMPTY_LIST);

	Item := List.FindByName('AnyName');

	Assert.IsTrue(Item.isNone);
end;

initialization

TDUnitX.RegisterTestFixture(TCMRIncomingInviteListTest);

end.
