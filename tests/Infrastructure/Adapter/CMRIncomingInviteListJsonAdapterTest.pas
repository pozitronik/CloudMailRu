unit CMRIncomingInviteListJsonAdapterTest;

interface

uses
	CMRIncomingInviteList,
	CMRIncomingInviteListJsonAdapter,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCMRIncomingInviteListJsonAdapterTest = class
	private
		const
			{Single invite}
			JSON_SINGLE_INVITE = '{"status":200,"body":{"list":[' +
				'{"owner":{"email":"owner@mail.ru","name":"Owner Name"},' +
				'"tree":"abc123","access":"rw","name":"SharedFolder",' +
				'"home":"/SharedFolder","size":1024,"invite_token":"token123"}' +
				']}}';

			{Multiple invites}
			JSON_MULTIPLE_INVITES = '{"status":200,"body":{"list":[' +
				'{"owner":{"email":"owner1@mail.ru","name":"Owner One"},' +
				'"tree":"abc","access":"rw","name":"Folder1",' +
				'"home":"/Folder1","size":100,"invite_token":"token1"},' +
				'{"owner":{"email":"owner2@mail.ru","name":"Owner Two"},' +
				'"tree":"def","access":"ro","name":"Folder2",' +
				'"home":"/Folder2","size":200,"invite_token":"token2"}' +
				']}}';

			{No list property}
			JSON_NO_LIST = '{"status":200,"body":{}}';

			{Empty list}
			JSON_EMPTY_LIST = '{"status":200,"body":{"list":[]}}';

			{Invalid JSON}
			JSON_INVALID = 'not valid json';

			{Empty string}
			JSON_EMPTY = '';
	public
		[Test]
		procedure TestParse_SingleInvite_ReturnsTrue;
		[Test]
		procedure TestParse_SingleInvite_ParsesOwnerEmail;
		[Test]
		procedure TestParse_SingleInvite_ParsesOwnerName;
		[Test]
		procedure TestParse_SingleInvite_ParsesName;
		[Test]
		procedure TestParse_SingleInvite_ParsesHome;
		[Test]
		procedure TestParse_SingleInvite_ParsesSize;
		[Test]
		procedure TestParse_SingleInvite_ParsesInviteToken;
		[Test]
		procedure TestParse_MultipleInvites_ParsesAll;
		[Test]
		procedure TestParse_NoList_ReturnsFalse;
		[Test]
		procedure TestParse_EmptyList_ReturnsTrue;
		[Test]
		procedure TestParse_InvalidJSON_ReturnsFalse;
		[Test]
		procedure TestParse_EmptyString_ReturnsFalse;
	end;

implementation

procedure TCMRIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ReturnsTrue;
var
	List: TCMRIncomingInviteList;
begin
	Assert.IsTrue(TCMRIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List));
end;

procedure TCMRIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesOwnerEmail;
var
	List: TCMRIncomingInviteList;
begin
	TCMRIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('owner@mail.ru', List[0].owner.email);
end;

procedure TCMRIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesOwnerName;
var
	List: TCMRIncomingInviteList;
begin
	TCMRIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('Owner Name', List[0].owner.name);
end;

procedure TCMRIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesName;
var
	List: TCMRIncomingInviteList;
begin
	TCMRIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('SharedFolder', List[0].name);
end;

procedure TCMRIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesHome;
var
	List: TCMRIncomingInviteList;
begin
	TCMRIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('/SharedFolder', List[0].home);
end;

procedure TCMRIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesSize;
var
	List: TCMRIncomingInviteList;
begin
	TCMRIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual(Int64(1024), List[0].size);
end;

procedure TCMRIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesInviteToken;
var
	List: TCMRIncomingInviteList;
begin
	TCMRIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('token123', List[0].invite_token);
end;

procedure TCMRIncomingInviteListJsonAdapterTest.TestParse_MultipleInvites_ParsesAll;
var
	List: TCMRIncomingInviteList;
begin
	TCMRIncomingInviteListJsonAdapter.Parse(JSON_MULTIPLE_INVITES, List);
	Assert.AreEqual(Integer(2), Integer(Length(List)));
	Assert.AreEqual('Folder1', List[0].name);
	Assert.AreEqual('Folder2', List[1].name);
end;

procedure TCMRIncomingInviteListJsonAdapterTest.TestParse_NoList_ReturnsFalse;
var
	List: TCMRIncomingInviteList;
begin
	Assert.IsFalse(TCMRIncomingInviteListJsonAdapter.Parse(JSON_NO_LIST, List));
end;

procedure TCMRIncomingInviteListJsonAdapterTest.TestParse_EmptyList_ReturnsTrue;
var
	List: TCMRIncomingInviteList;
begin
	Assert.IsTrue(TCMRIncomingInviteListJsonAdapter.Parse(JSON_EMPTY_LIST, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRIncomingInviteListJsonAdapterTest.TestParse_InvalidJSON_ReturnsFalse;
var
	List: TCMRIncomingInviteList;
begin
	Assert.IsFalse(TCMRIncomingInviteListJsonAdapter.Parse(JSON_INVALID, List));
end;

procedure TCMRIncomingInviteListJsonAdapterTest.TestParse_EmptyString_ReturnsFalse;
var
	List: TCMRIncomingInviteList;
begin
	Assert.IsFalse(TCMRIncomingInviteListJsonAdapter.Parse(JSON_EMPTY, List));
end;

initialization

TDUnitX.RegisterTestFixture(TCMRIncomingInviteListJsonAdapterTest);

end.
