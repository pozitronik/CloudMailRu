unit CMRInviteListJsonAdapterTest;

interface

uses
	CMRInviteList,
	CMRInviteListJsonAdapter,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCMRInviteListJsonAdapterTest = class
	private
		const
			{Single invite}
			JSON_SINGLE_INVITE = '{"status":200,"body":{"invited":[' +
				'{"email":"user@mail.ru","status":"accepted","access":"rw","name":"User Name"}' +
				']}}';

			{Multiple invites}
			JSON_MULTIPLE_INVITES = '{"status":200,"body":{"invited":[' +
				'{"email":"user1@mail.ru","status":"accepted","access":"rw","name":"User One"},' +
				'{"email":"user2@mail.ru","status":"pending","access":"ro","name":"User Two"}' +
				']}}';

			{No invites array}
			JSON_NO_INVITES = '{"status":200,"body":{}}';

			{Empty invites array}
			JSON_EMPTY_INVITES = '{"status":200,"body":{"invited":[]}}';

			{Invalid JSON}
			JSON_INVALID = 'not valid json';

			{Empty string}
			JSON_EMPTY = '';
	public
		[Test]
		procedure TestParse_SingleInvite_ReturnsTrue;
		[Test]
		procedure TestParse_SingleInvite_ParsesCorrectCount;
		[Test]
		procedure TestParse_SingleInvite_ParsesEmail;
		[Test]
		procedure TestParse_SingleInvite_ParsesStatus;
		[Test]
		procedure TestParse_SingleInvite_ParsesAccess;
		[Test]
		procedure TestParse_SingleInvite_ParsesName;
		[Test]
		procedure TestParse_MultipleInvites_ParsesAll;
		[Test]
		procedure TestParse_NoInvites_ReturnsTrue;
		[Test]
		procedure TestParse_EmptyInvites_ReturnsTrue;
		[Test]
		procedure TestParse_InvalidJSON_ReturnsFalse;
		[Test]
		procedure TestParse_EmptyString_ReturnsFalse;
	end;

implementation

procedure TCMRInviteListJsonAdapterTest.TestParse_SingleInvite_ReturnsTrue;
var
	List: TCMRInviteList;
begin
	Assert.IsTrue(TCMRInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List));
end;

procedure TCMRInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesCorrectCount;
var
	List: TCMRInviteList;
begin
	TCMRInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual(Integer(1), Integer(Length(List)));
end;

procedure TCMRInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesEmail;
var
	List: TCMRInviteList;
begin
	TCMRInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('user@mail.ru', List[0].email);
end;

procedure TCMRInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesStatus;
var
	List: TCMRInviteList;
begin
	TCMRInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('accepted', List[0].status);
end;

procedure TCMRInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesAccess;
var
	List: TCMRInviteList;
begin
	TCMRInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('rw', List[0].access);
end;

procedure TCMRInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesName;
var
	List: TCMRInviteList;
begin
	TCMRInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('User Name', List[0].name);
end;

procedure TCMRInviteListJsonAdapterTest.TestParse_MultipleInvites_ParsesAll;
var
	List: TCMRInviteList;
begin
	TCMRInviteListJsonAdapter.Parse(JSON_MULTIPLE_INVITES, List);
	Assert.AreEqual(Integer(2), Integer(Length(List)));
	Assert.AreEqual('user1@mail.ru', List[0].email);
	Assert.AreEqual('user2@mail.ru', List[1].email);
end;

procedure TCMRInviteListJsonAdapterTest.TestParse_NoInvites_ReturnsTrue;
var
	List: TCMRInviteList;
begin
	Assert.IsTrue(TCMRInviteListJsonAdapter.Parse(JSON_NO_INVITES, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRInviteListJsonAdapterTest.TestParse_EmptyInvites_ReturnsTrue;
var
	List: TCMRInviteList;
begin
	Assert.IsTrue(TCMRInviteListJsonAdapter.Parse(JSON_EMPTY_INVITES, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCMRInviteListJsonAdapterTest.TestParse_InvalidJSON_ReturnsFalse;
var
	List: TCMRInviteList;
begin
	Assert.IsFalse(TCMRInviteListJsonAdapter.Parse(JSON_INVALID, List));
end;

procedure TCMRInviteListJsonAdapterTest.TestParse_EmptyString_ReturnsFalse;
var
	List: TCMRInviteList;
begin
	Assert.IsFalse(TCMRInviteListJsonAdapter.Parse(JSON_EMPTY, List));
end;

initialization

TDUnitX.RegisterTestFixture(TCMRInviteListJsonAdapterTest);

end.
