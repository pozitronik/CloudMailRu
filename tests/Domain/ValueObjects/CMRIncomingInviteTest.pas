unit CMRIncomingInviteTest;

interface

uses
	CMRIncomingInvite,
	CMROwner,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCMRIncomingInviteTest = class
	public
		[Test]
		procedure TestIsNoneWhenEmpty;
		[Test]
		procedure TestIsNoneWhenHasName;
		[Test]
		procedure TestIsMountedWhenHasHome;
		[Test]
		procedure TestIsMountedWhenNoHome;
		[Test]
		procedure TestNoneCreatesEmptyRecord;
		[Test]
		procedure TestNoneIsNoneTrue;
		[Test]
		procedure TestRecordWithAllFields;
	end;

implementation

procedure TCMRIncomingInviteTest.TestIsNoneWhenEmpty;
var
	Invite: TCMRIncomingInvite;
begin
	{ Empty record (default initialized) should be considered "none" }
	FillChar(Invite, SizeOf(Invite), 0);

	Assert.IsTrue(Invite.isNone);
end;

procedure TCMRIncomingInviteTest.TestIsNoneWhenHasName;
var
	Invite: TCMRIncomingInvite;
begin
	FillChar(Invite, SizeOf(Invite), 0);
	Invite.name := 'SharedFolder';

	Assert.IsFalse(Invite.isNone);
end;

procedure TCMRIncomingInviteTest.TestIsMountedWhenHasHome;
var
	Invite: TCMRIncomingInvite;
begin
	FillChar(Invite, SizeOf(Invite), 0);
	Invite.home := '/mounted/path';

	Assert.IsTrue(Invite.isMounted);
end;

procedure TCMRIncomingInviteTest.TestIsMountedWhenNoHome;
var
	Invite: TCMRIncomingInvite;
begin
	FillChar(Invite, SizeOf(Invite), 0);
	Invite.name := 'PendingInvite';
	{ home is empty }

	Assert.IsFalse(Invite.isMounted);
end;

procedure TCMRIncomingInviteTest.TestNoneCreatesEmptyRecord;
var
	Invite: TCMRIncomingInvite;
begin
	{ Start with some data }
	Invite.name := 'SomeFolder';
	Invite.home := '/some/path';
	Invite.size := 12345;

	{ Call None to reset }
	Invite := Invite.None;

	{ All fields should be zeroed }
	Assert.AreEqual('', string(Invite.name));
	Assert.AreEqual('', string(Invite.home));
	Assert.AreEqual(Int64(0), Invite.size);
end;

procedure TCMRIncomingInviteTest.TestNoneIsNoneTrue;
var
	Invite: TCMRIncomingInvite;
begin
	Invite := Invite.None;

	Assert.IsTrue(Invite.isNone);
end;

procedure TCMRIncomingInviteTest.TestRecordWithAllFields;
var
	Invite: TCMRIncomingInvite;
begin
	FillChar(Invite, SizeOf(Invite), 0);

	Invite.owner.email := 'owner@mail.ru';
	Invite.owner.name := 'Owner Name';
	Invite.tree := 'tree123';
	Invite.access := 'read_write';
	Invite.name := 'SharedDocs';
	Invite.size := 1024000;
	Invite.home := '/SharedDocs';
	Invite.invite_token := 'token_abc';

	Assert.IsFalse(Invite.isNone);
	Assert.IsTrue(Invite.isMounted);
	Assert.AreEqual('owner@mail.ru', Invite.owner.email);
	Assert.AreEqual('Owner Name', Invite.owner.name);
	Assert.AreEqual('tree123', Invite.tree);
	Assert.AreEqual('read_write', Invite.access);
	Assert.AreEqual('SharedDocs', Invite.name);
	Assert.AreEqual(Int64(1024000), Invite.size);
	Assert.AreEqual('/SharedDocs', Invite.home);
	Assert.AreEqual('token_abc', Invite.invite_token);
end;

initialization

TDUnitX.RegisterTestFixture(TCMRIncomingInviteTest);

end.
