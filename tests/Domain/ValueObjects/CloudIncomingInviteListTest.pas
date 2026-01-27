unit CloudIncomingInviteListTest;

interface

uses
	CloudIncomingInviteList,
	CloudIncomingInviteListJsonAdapter,
	CloudIncomingInvite,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCloudIncomingInviteListTest = class
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

procedure TCloudIncomingInviteListTest.TestFromJSONValidSingle;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsTrue(TCloudIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List));

	Assert.AreEqual(Integer(1), Integer(Length(List)));
end;

procedure TCloudIncomingInviteListTest.TestFromJSONValidMultiple;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsTrue(TCloudIncomingInviteListJsonAdapter.Parse(JSON_MULTIPLE_INVITES, List));

	Assert.AreEqual(Integer(3), Integer(Length(List)));
end;

procedure TCloudIncomingInviteListTest.TestFromJSONWithOwner;
var
	List: TCloudIncomingInviteList;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);

	Assert.AreEqual('owner@mail.ru', List[0].owner.email);
	Assert.AreEqual('Owner Name', List[0].owner.name);
end;

procedure TCloudIncomingInviteListTest.TestFromJSONMountedItem;
var
	List: TCloudIncomingInviteList;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);

	{ Item with home path is considered mounted }
	Assert.AreEqual('/SharedFolder', List[0].home);
	Assert.IsTrue(List[0].isMounted);
end;

procedure TCloudIncomingInviteListTest.TestFromJSONUnmountedItem;
var
	List: TCloudIncomingInviteList;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_MULTIPLE_INVITES, List);

	{ First item has no home path - not mounted }
	Assert.IsFalse(List[0].isMounted);

	{ Second item has home path - mounted }
	Assert.IsTrue(List[1].isMounted);

	{ Third item has no home path - not mounted }
	Assert.IsFalse(List[2].isMounted);
end;

procedure TCloudIncomingInviteListTest.TestFromJSONNoList;
var
	List: TCloudIncomingInviteList;
begin
	{ When there's no list array, should return false }
	Assert.IsFalse(TCloudIncomingInviteListJsonAdapter.Parse(JSON_NO_LIST, List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudIncomingInviteListTest.TestFromJSONEmptyList;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsTrue(TCloudIncomingInviteListJsonAdapter.Parse(JSON_EMPTY_LIST, List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudIncomingInviteListTest.TestFromJSONInvalidJSON;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsFalse(TCloudIncomingInviteListJsonAdapter.Parse('invalid json', List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudIncomingInviteListTest.TestFromJSONEmptyString;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsFalse(TCloudIncomingInviteListJsonAdapter.Parse('', List));

	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudIncomingInviteListTest.TestFindByNameFound;
var
	List: TCloudIncomingInviteList;
	Item: TCloudIncomingInvite;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_MULTIPLE_INVITES, List);

	Item := List.FindByName('Folder2');

	Assert.IsFalse(Item.isNone);
	Assert.AreEqual('Folder2', Item.name);
	Assert.AreEqual('user2@mail.ru', Item.owner.email);
	Assert.AreEqual(Int64(200), Item.size);
end;

procedure TCloudIncomingInviteListTest.TestFindByNameNotFound;
var
	List: TCloudIncomingInviteList;
	Item: TCloudIncomingInvite;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_MULTIPLE_INVITES, List);

	Item := List.FindByName('NonexistentFolder');

	Assert.IsTrue(Item.isNone);
end;

procedure TCloudIncomingInviteListTest.TestFindByNameEmptyList;
var
	List: TCloudIncomingInviteList;
	Item: TCloudIncomingInvite;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_EMPTY_LIST, List);

	Item := List.FindByName('AnyName');

	Assert.IsTrue(Item.isNone);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudIncomingInviteListTest);

end.
