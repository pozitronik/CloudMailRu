unit RealPathTest;

interface

uses
	RealPath,
	SettingsConstants,
	DUnitX.TestFramework;

type

	[TestFixture]
	TRealPathTest = class
	public
		[Test]
		procedure TestFromPathEmpty;
		[Test]
		procedure TestFromPathAccountOnly;
		[Test]
		procedure TestFromPathAccountWithSubpath;
		[Test]
		procedure TestFromPathDeepNesting;
		[Test]
		procedure TestTrashVirtualDir;
		[Test]
		procedure TestSharedVirtualDir;
		[Test]
		procedure TestInvitesVirtualDir;
		[Test]
		procedure TestIsVirtualProperty;
		[Test]
		procedure TestHasHomePathProperty;
		[Test]
		procedure TestIsInAccountsList;
		[Test]
		procedure TestIsAccountEmpty;
		[Test]
		procedure TestIsInAccount;
		[Test]
		procedure TestToPath;
		[Test]
		procedure TestUpDirItem;
		[Test]
		procedure TestGetRealPathStaticMethod;
		[Test]
		procedure TestFromPathWithEmptySegments;
	end;

implementation

procedure TRealPathTest.TestFromPathEmpty;
var
	RP: TRealPath;
begin
	RP.FromPath('');

	Assert.IsEmpty(RP.account);
	Assert.IsEmpty(RP.path);
	Assert.IsTrue(RP.isAccountEmpty);
	Assert.IsFalse(RP.isVirtual);
end;

procedure TRealPathTest.TestFromPathAccountOnly;
var
	RP: TRealPath;
begin
	RP.FromPath('\MyAccount');

	Assert.AreEqual('MyAccount', RP.account);
	Assert.IsEmpty(RP.path);
	Assert.AreEqual(ID_True, RP.isDir);
	Assert.IsTrue(RP.isInAccountsList);
end;

procedure TRealPathTest.TestFromPathAccountWithSubpath;
var
	RP: TRealPath;
begin
	RP.FromPath('\MyAccount\folder\subfolder');

	Assert.AreEqual('MyAccount', RP.account);
	Assert.AreEqual('folder\subfolder', RP.path);
	Assert.IsFalse(RP.isInAccountsList);
end;

procedure TRealPathTest.TestFromPathDeepNesting;
var
	RP: TRealPath;
begin
	RP.FromPath('\Account\level1\level2\level3\file.txt');

	Assert.AreEqual('Account', RP.account);
	Assert.AreEqual('level1\level2\level3\file.txt', RP.path);
end;

procedure TRealPathTest.TestTrashVirtualDir;
var
	RP: TRealPath;
begin
	RP.FromPath('\MyAccount' + TrashPostfix + '\deleted_file.txt');

	Assert.AreEqual('MyAccount', RP.account);
	Assert.AreEqual('deleted_file.txt', RP.path);
	Assert.IsTrue(RP.trashDir);
	Assert.IsFalse(RP.sharedDir);
	Assert.IsFalse(RP.invitesDir);
	Assert.IsTrue(RP.isVirtual);
end;

procedure TRealPathTest.TestSharedVirtualDir;
var
	RP: TRealPath;
begin
	RP.FromPath('\MyAccount' + SharedPostfix + '\shared_item');

	Assert.AreEqual('MyAccount', RP.account);
	Assert.AreEqual('shared_item', RP.path);
	Assert.IsFalse(RP.trashDir);
	Assert.IsTrue(RP.sharedDir);
	Assert.IsFalse(RP.invitesDir);
	Assert.IsTrue(RP.isVirtual);
end;

procedure TRealPathTest.TestInvitesVirtualDir;
var
	RP: TRealPath;
begin
	RP.FromPath('\MyAccount' + InvitesPostfix + '\invite_item');

	Assert.AreEqual('MyAccount', RP.account);
	Assert.AreEqual('invite_item', RP.path);
	Assert.IsFalse(RP.trashDir);
	Assert.IsFalse(RP.sharedDir);
	Assert.IsTrue(RP.invitesDir);
	Assert.IsTrue(RP.isVirtual);
end;

procedure TRealPathTest.TestIsVirtualProperty;
var
	RP: TRealPath;
begin
	RP.FromPath('\Account\normal\path');
	Assert.IsFalse(RP.isVirtual);

	RP.FromPath('\Account' + TrashPostfix);
	Assert.IsTrue(RP.isVirtual);

	RP.FromPath('\Account' + SharedPostfix);
	Assert.IsTrue(RP.isVirtual);

	RP.FromPath('\Account' + InvitesPostfix);
	Assert.IsTrue(RP.isVirtual);
end;

procedure TRealPathTest.TestHasHomePathProperty;
var
	RP: TRealPath;
begin
	{ Normal paths have home path }
	RP.FromPath('\Account\folder');
	Assert.IsTrue(RP.hasHomePath);

	{ Trash and shared dirs do not have home path }
	RP.FromPath('\Account' + TrashPostfix);
	Assert.IsFalse(RP.hasHomePath);

	RP.FromPath('\Account' + SharedPostfix);
	Assert.IsFalse(RP.hasHomePath);

	{ Invites dir has home path (per implementation) }
	RP.FromPath('\Account' + InvitesPostfix);
	Assert.IsTrue(RP.hasHomePath);
end;

procedure TRealPathTest.TestIsInAccountsList;
var
	RP: TRealPath;
begin
	{ Account root is in accounts list }
	RP.FromPath('\Account');
	Assert.IsTrue(RP.isInAccountsList);

	{ Path inside account is not in accounts list }
	RP.FromPath('\Account\folder');
	Assert.IsFalse(RP.isInAccountsList);

	{ Virtual dir root is in accounts list }
	RP.FromPath('\Account' + TrashPostfix);
	Assert.IsTrue(RP.isInAccountsList);
end;

procedure TRealPathTest.TestIsAccountEmpty;
var
	RP: TRealPath;
begin
	RP.FromPath('');
	Assert.IsTrue(RP.isAccountEmpty);

	RP.FromPath('\Account');
	Assert.IsFalse(RP.isAccountEmpty);
end;

procedure TRealPathTest.TestIsInAccount;
var
	RP: TRealPath;
begin
	{ Empty path is not in account }
	RP.FromPath('');
	Assert.IsFalse(RP.IsInAccount(true));
	Assert.IsFalse(RP.IsInAccount(false));

	{ Normal path is in account }
	RP.FromPath('\Account\folder');
	Assert.IsTrue(RP.IsInAccount(true));
	Assert.IsTrue(RP.IsInAccount(false));

	{ Virtual path: depends on ignoreVirtual flag }
	RP.FromPath('\Account' + TrashPostfix + '\file');
	Assert.IsFalse(RP.IsInAccount(true));
	Assert.IsTrue(RP.IsInAccount(false));
end;

procedure TRealPathTest.TestToPath;
var
	RP: TRealPath;
begin
	RP.FromPath('\MyAccount\folder\file.txt');
	Assert.AreEqual('\MyAccount\folder\file.txt', RP.ToPath);

	RP.FromPath('\Account');
	Assert.AreEqual('\Account', RP.ToPath);
end;

procedure TRealPathTest.TestUpDirItem;
var
	RP: TRealPath;
begin
	RP.FromPath('\Account\folder\..');
	Assert.IsTrue(RP.upDirItem);

	RP.FromPath('\Account\folder');
	Assert.IsFalse(RP.upDirItem);
end;

procedure TRealPathTest.TestGetRealPathStaticMethod;
var
	RP: TRealPath;
begin
	RP := TRealPath.GetRealPath('\Account\folder', ID_True);

	Assert.AreEqual('Account', RP.account);
	Assert.AreEqual('folder', RP.path);
	Assert.AreEqual(ID_True, RP.isDir);
end;

procedure TRealPathTest.TestFromPathWithEmptySegments;
var
	RP: TRealPath;
begin
	{ Consecutive backslashes produce empty segments that are stripped }
	RP.FromPath('\Account\\folder');

	Assert.AreEqual('Account', RP.account);
	Assert.AreEqual('folder', RP.path);
end;

initialization

TDUnitX.RegisterTestFixture(TRealPathTest);

end.
