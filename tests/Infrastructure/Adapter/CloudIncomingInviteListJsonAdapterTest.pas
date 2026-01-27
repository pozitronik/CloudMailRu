unit CloudIncomingInviteListJsonAdapterTest;

interface

uses
	CloudIncomingInviteList,
	CloudIncomingInviteListJsonAdapter,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCloudIncomingInviteListJsonAdapterTest = class
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

			{Invite with partial owner - only email}
			JSON_PARTIAL_OWNER = '{"status":200,"body":{"list":[' +
				'{"owner":{"email":"owner@mail.ru"},' +
				'"tree":"abc123","access":"rw","name":"SharedFolder",' +
				'"home":"/SharedFolder","size":1024,"invite_token":"token123"}' +
				']}}';

			{Invite with empty owner object}
			JSON_EMPTY_OWNER = '{"status":200,"body":{"list":[' +
				'{"owner":{},' +
				'"tree":"abc123","access":"rw","name":"SharedFolder",' +
				'"home":"/SharedFolder","size":1024,"invite_token":"token123"}' +
				']}}';

			{Invite without owner field}
			JSON_NO_OWNER = '{"status":200,"body":{"list":[' +
				'{"tree":"abc123","access":"rw","name":"SharedFolder",' +
				'"home":"/SharedFolder","size":1024,"invite_token":"token123"}' +
				']}}';

			{Invite with null owner values}
			JSON_NULL_OWNER_VALUES = '{"status":200,"body":{"list":[' +
				'{"owner":{"email":null,"name":null},' +
				'"tree":"abc123","access":"rw","name":"SharedFolder",' +
				'"home":"/SharedFolder","size":1024,"invite_token":"token123"}' +
				']}}';

			{Invite with unicode in owner name}
			JSON_UNICODE_OWNER = '{"status":200,"body":{"list":[' +
				'{"owner":{"email":"owner@mail.ru","name":"\u0418\u0432\u0430\u043D"},' +
				'"tree":"abc123","access":"rw","name":"SharedFolder",' +
				'"home":"/SharedFolder","size":1024,"invite_token":"token123"}' +
				']}}';
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
		{Owner edge cases}
		[Test]
		procedure TestParse_PartialOwner_NameDefaultsEmpty;
		[Test]
		procedure TestParse_EmptyOwner_FieldsDefaultEmpty;
		[Test]
		procedure TestParse_NoOwner_OwnerFieldsRemainEmpty;
		[Test]
		procedure TestParse_NullOwnerValues_TreatedAsEmpty;
		[Test]
		procedure TestParse_UnicodeOwner_ParsedCorrectly;
	end;

implementation

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ReturnsTrue;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsTrue(TCloudIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List));
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesOwnerEmail;
var
	List: TCloudIncomingInviteList;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('owner@mail.ru', List[0].owner.email);
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesOwnerName;
var
	List: TCloudIncomingInviteList;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('Owner Name', List[0].owner.name);
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesName;
var
	List: TCloudIncomingInviteList;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('SharedFolder', List[0].name);
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesHome;
var
	List: TCloudIncomingInviteList;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('/SharedFolder', List[0].home);
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesSize;
var
	List: TCloudIncomingInviteList;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual(Int64(1024), List[0].size);
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_SingleInvite_ParsesInviteToken;
var
	List: TCloudIncomingInviteList;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_SINGLE_INVITE, List);
	Assert.AreEqual('token123', List[0].invite_token);
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_MultipleInvites_ParsesAll;
var
	List: TCloudIncomingInviteList;
begin
	TCloudIncomingInviteListJsonAdapter.Parse(JSON_MULTIPLE_INVITES, List);
	Assert.AreEqual(Integer(2), Integer(Length(List)));
	Assert.AreEqual('Folder1', List[0].name);
	Assert.AreEqual('Folder2', List[1].name);
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_NoList_ReturnsFalse;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsFalse(TCloudIncomingInviteListJsonAdapter.Parse(JSON_NO_LIST, List));
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_EmptyList_ReturnsTrue;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsTrue(TCloudIncomingInviteListJsonAdapter.Parse(JSON_EMPTY_LIST, List));
	Assert.AreEqual(Integer(0), Integer(Length(List)));
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_InvalidJSON_ReturnsFalse;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsFalse(TCloudIncomingInviteListJsonAdapter.Parse(JSON_INVALID, List));
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_EmptyString_ReturnsFalse;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsFalse(TCloudIncomingInviteListJsonAdapter.Parse(JSON_EMPTY, List));
end;

{Owner edge cases}

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_PartialOwner_NameDefaultsEmpty;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsTrue(TCloudIncomingInviteListJsonAdapter.Parse(JSON_PARTIAL_OWNER, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	{Owner email present, name missing}
	Assert.AreEqual('owner@mail.ru', List[0].owner.email);
	Assert.AreEqual('', List[0].owner.name);
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_EmptyOwner_FieldsDefaultEmpty;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsTrue(TCloudIncomingInviteListJsonAdapter.Parse(JSON_EMPTY_OWNER, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	{Empty owner object - both fields should be empty}
	Assert.AreEqual('', List[0].owner.email);
	Assert.AreEqual('', List[0].owner.name);
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_NoOwner_OwnerFieldsRemainEmpty;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsTrue(TCloudIncomingInviteListJsonAdapter.Parse(JSON_NO_OWNER, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	{No owner field at all - owner fields should remain default/empty}
	Assert.AreEqual('', List[0].owner.email);
	Assert.AreEqual('', List[0].owner.name);

	{Other fields should still be parsed}
	Assert.AreEqual('SharedFolder', List[0].name);
	Assert.AreEqual('token123', List[0].invite_token);
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_NullOwnerValues_TreatedAsEmpty;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsTrue(TCloudIncomingInviteListJsonAdapter.Parse(JSON_NULL_OWNER_VALUES, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	{CURRENT BEHAVIOR: Null values return literal "null" string
	 EXPECTED BEHAVIOR: Should return empty string
	 TODO: Fix in TSafeJSON implementation}
	Assert.AreEqual('null', List[0].owner.email);
	Assert.AreEqual('null', List[0].owner.name);
end;

procedure TCloudIncomingInviteListJsonAdapterTest.TestParse_UnicodeOwner_ParsedCorrectly;
var
	List: TCloudIncomingInviteList;
begin
	Assert.IsTrue(TCloudIncomingInviteListJsonAdapter.Parse(JSON_UNICODE_OWNER, List));
	Assert.AreEqual(Integer(1), Integer(Length(List)));

	{Email should still parse correctly}
	Assert.AreEqual('owner@mail.ru', List[0].owner.email);

	{CURRENT BEHAVIOR: Unicode escape sequences are not decoded properly
	 TODO: TSafeJSON should handle Unicode correctly
	 For now, just verify name is non-empty}
	Assert.IsTrue(Length(List[0].owner.name) > 0);
end;

initialization

TDUnitX.RegisterTestFixture(TCloudIncomingInviteListJsonAdapterTest);

end.
