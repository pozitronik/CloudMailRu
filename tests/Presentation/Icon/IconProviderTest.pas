unit IconProviderTest;

interface

uses
	SysUtils,
	CloudDirItem, CloudIncomingInvite, CloudConstants, RealPath,
	SettingsConstants,
	IconProvider,
	DUnitX.TestFramework;

type
	[TestFixture]
	TIconProviderTest = class
	private
		FProvider: IIconProvider;
		function CreateEmptyContext: TIconContext;
		function CreateContextWithItem(const Item: TCloudDirItem): TIconContext;
		function CreateContextWithInvite(const Invite: TCloudIncomingInvite): TIconContext;
		function CreateTestDirItem: TCloudDirItem;
		function CreateTestFileItem: TCloudDirItem;
		function CreateSharedDirItem: TCloudDirItem;
		function CreatePublishedDirItem: TCloudDirItem;
		function CreateMountedInvite: TCloudIncomingInvite;
		function CreateUnmountedInvite: TCloudIncomingInvite;
		function CreateEmptyInvite: TCloudIncomingInvite;
	public
		[Setup]
		procedure Setup;

		[TearDown]
		procedure TearDown;

		{ Skip conditions }
		[Test]
		procedure TestGetIcon_UpDirItem_ReturnsUseDefault;

		{ Trash directory }
		[Test]
		procedure TestGetIcon_TrashAtRoot_ReturnsSystemTrash;
		[Test]
		procedure TestGetIcon_TrashInside_NotSystemTrash;

		{ Shared directory }
		[Test]
		procedure TestGetIcon_SharedAtRoot_ReturnsInternalOverlay;
		[Test]
		procedure TestGetIcon_SharedInside_DisabledMode_ForcesOverlay;
		[Test]
		procedure TestGetIcon_SharedInside_EnabledMode_UsesConfiguredMode;

		{ Invites directory }
		[Test]
		procedure TestGetIcon_InvitesAtRoot_ReturnsSharedIncoming;
		[Test]
		procedure TestGetIcon_InvitesInside_MountedItem_ReturnsSharedIncoming;
		[Test]
		procedure TestGetIcon_InvitesInside_UnmountedItem_ReturnsShared;
		[Test]
		procedure TestGetIcon_InvitesInside_EmptyItem_ReturnsUseDefault;
		[Test]
		procedure TestGetIcon_InvitesInside_NoInviteItem_ReturnsUseDefault;

		{ Account root }
		[Test]
		procedure TestGetIcon_AccountRoot_PublicAccount_ReturnsCloudPublic;
		[Test]
		procedure TestGetIcon_AccountRoot_PrivateAccount_ReturnsCloud;

		{ Directory items }
		[Test]
		procedure TestGetIcon_Directory_KindShared_ReturnsShared;
		[Test]
		procedure TestGetIcon_Directory_Published_ReturnsSharedPublic;
		[Test]
		procedure TestGetIcon_Directory_NotPublished_ReturnsUseDefault;
		[Test]
		procedure TestGetIcon_File_ReturnsUseDefault;
		[Test]
		procedure TestGetIcon_NoItem_ReturnsUseDefault;

		{ Render modes }
		[Test]
		procedure TestGetIcon_IconsModeDisabled_ReturnsUseDefault;
		[Test]
		procedure TestGetIcon_IconsModeInternal_ReturnsInternal;
		[Test]
		procedure TestGetIcon_IconsModeInternalOverlay_ReturnsInternalOverlay;
		[Test]
		procedure TestGetIcon_IconsModeExternal_ReturnsExternal;
		[Test]
		procedure TestGetIcon_IconsModeExternalOverlay_ReturnsExternalOverlay;
		[Test]
		procedure TestGetIcon_UnknownIconsMode_ReturnsUseDefault;
	end;

implementation

procedure TIconProviderTest.Setup;
begin
	FProvider := TIconProvider.Create;
end;

procedure TIconProviderTest.TearDown;
begin
	FProvider := nil;
end;

function TIconProviderTest.CreateEmptyContext: TIconContext;
begin
	Result.IconsMode := IconsModeInternal;
	Result.IsPublicAccount := False;
	Result.HasItem := False;
	Result.HasInviteItem := False;
end;

function TIconProviderTest.CreateContextWithItem(const Item: TCloudDirItem): TIconContext;
begin
	Result := CreateEmptyContext;
	Result.Item := Item;
	Result.HasItem := True;
end;

function TIconProviderTest.CreateContextWithInvite(const Invite: TCloudIncomingInvite): TIconContext;
begin
	Result := CreateEmptyContext;
	Result.InviteItem := Invite;
	Result.HasInviteItem := True;
end;

function TIconProviderTest.CreateTestDirItem: TCloudDirItem;
begin
	Result := Default(TCloudDirItem);
	Result.name := 'test_folder';
	Result.type_ := TYPE_DIR;
	Result.kind := '';
	Result.weblink := '';
end;

function TIconProviderTest.CreateTestFileItem: TCloudDirItem;
begin
	Result := Default(TCloudDirItem);
	Result.name := 'test_file.txt';
	Result.type_ := TYPE_FILE;
	Result.kind := 'file';
end;

function TIconProviderTest.CreateSharedDirItem: TCloudDirItem;
begin
	Result := CreateTestDirItem;
	Result.kind := KIND_SHARED;
end;

function TIconProviderTest.CreatePublishedDirItem: TCloudDirItem;
begin
	Result := CreateTestDirItem;
	Result.weblink := 'https://cloud.mail.ru/public/abc123';
end;

function TIconProviderTest.CreateMountedInvite: TCloudIncomingInvite;
begin
	Result := Default(TCloudIncomingInvite);
	Result.name := 'shared_folder';
	Result.home := '/mounted/path'; { isMounted returns True when home is not empty }
end;

function TIconProviderTest.CreateUnmountedInvite: TCloudIncomingInvite;
begin
	Result := Default(TCloudIncomingInvite);
	Result.name := 'shared_folder';
	Result.home := ''; { isMounted returns False when home is empty }
end;

function TIconProviderTest.CreateEmptyInvite: TCloudIncomingInvite;
begin
	Result := Default(TCloudIncomingInvite);
	Result.name := '';
end;

{ Skip conditions }

procedure TIconProviderTest.TestGetIcon_UpDirItem_ReturnsUseDefault;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account\..');
	Context := CreateEmptyContext;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itUseDefault, Info.IconType);
end;

{ Trash directory }

procedure TIconProviderTest.TestGetIcon_TrashAtRoot_ReturnsSystemTrash;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account.trash');
	Context := CreateEmptyContext;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itSystemTrash, Info.IconType);
	Assert.AreEqual('cloud_trash', Info.IconName);
end;

procedure TIconProviderTest.TestGetIcon_TrashInside_NotSystemTrash;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account.trash\deleted_file.txt');
	Context := CreateContextWithItem(CreateTestFileItem);

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreNotEqual(itSystemTrash, Info.IconType);
end;

{ Shared directory }

procedure TIconProviderTest.TestGetIcon_SharedAtRoot_ReturnsInternalOverlay;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account.shared');
	Context := CreateEmptyContext;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itInternalOverlay, Info.IconType);
	Assert.AreEqual('shared', Info.IconName);
end;

procedure TIconProviderTest.TestGetIcon_SharedInside_DisabledMode_ForcesOverlay;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account.shared\some_link');
	Context := CreateContextWithItem(CreateSharedDirItem);
	Context.IconsMode := IconsModeDisabled;

	Info := FProvider.GetIcon(Path, Context);

	{ Should force overlay mode even though icons are disabled }
	Assert.AreEqual(itInternalOverlay, Info.IconType);
end;

procedure TIconProviderTest.TestGetIcon_SharedInside_EnabledMode_UsesConfiguredMode;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account.shared\some_link');
	Context := CreateContextWithItem(CreateSharedDirItem);
	Context.IconsMode := IconsModeExternal;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itExternal, Info.IconType);
end;

{ Invites directory }

procedure TIconProviderTest.TestGetIcon_InvitesAtRoot_ReturnsSharedIncoming;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account.invites');
	Context := CreateEmptyContext;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itInternalOverlay, Info.IconType);
	Assert.AreEqual('shared_incoming', Info.IconName);
end;

procedure TIconProviderTest.TestGetIcon_InvitesInside_MountedItem_ReturnsSharedIncoming;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account.invites\shared_folder');
	Context := CreateContextWithInvite(CreateMountedInvite);

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itInternalOverlay, Info.IconType);
	Assert.AreEqual('shared_incoming', Info.IconName);
end;

procedure TIconProviderTest.TestGetIcon_InvitesInside_UnmountedItem_ReturnsShared;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account.invites\shared_folder');
	Context := CreateContextWithInvite(CreateUnmountedInvite);

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itInternalOverlay, Info.IconType);
	Assert.AreEqual('shared', Info.IconName);
end;

procedure TIconProviderTest.TestGetIcon_InvitesInside_EmptyItem_ReturnsUseDefault;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account.invites\unknown');
	Context := CreateContextWithInvite(CreateEmptyInvite);

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itUseDefault, Info.IconType);
end;

procedure TIconProviderTest.TestGetIcon_InvitesInside_NoInviteItem_ReturnsUseDefault;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account.invites\unknown');
	Context := CreateEmptyContext;
	Context.HasInviteItem := False;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itUseDefault, Info.IconType);
end;

{ Account root }

procedure TIconProviderTest.TestGetIcon_AccountRoot_PublicAccount_ReturnsCloudPublic;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\public_account');
	Context := CreateEmptyContext;
	Context.IsPublicAccount := True;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual('cloud_public', Info.IconName);
end;

procedure TIconProviderTest.TestGetIcon_AccountRoot_PrivateAccount_ReturnsCloud;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\private_account');
	Context := CreateEmptyContext;
	Context.IsPublicAccount := False;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual('cloud', Info.IconName);
end;

{ Directory items }

procedure TIconProviderTest.TestGetIcon_Directory_KindShared_ReturnsShared;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account\shared_folder');
	Context := CreateContextWithItem(CreateSharedDirItem);

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual('shared', Info.IconName);
end;

procedure TIconProviderTest.TestGetIcon_Directory_Published_ReturnsSharedPublic;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account\published_folder');
	Context := CreateContextWithItem(CreatePublishedDirItem);

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual('shared_public', Info.IconName);
end;

procedure TIconProviderTest.TestGetIcon_Directory_NotPublished_ReturnsUseDefault;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account\regular_folder');
	Context := CreateContextWithItem(CreateTestDirItem);

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itUseDefault, Info.IconType);
end;

procedure TIconProviderTest.TestGetIcon_File_ReturnsUseDefault;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account\file.txt');
	Context := CreateContextWithItem(CreateTestFileItem);

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itUseDefault, Info.IconType);
end;

procedure TIconProviderTest.TestGetIcon_NoItem_ReturnsUseDefault;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account\unknown');
	Context := CreateEmptyContext;
	Context.HasItem := False;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itUseDefault, Info.IconType);
end;

{ Render modes }

procedure TIconProviderTest.TestGetIcon_IconsModeDisabled_ReturnsUseDefault;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account');
	Context := CreateEmptyContext;
	Context.IconsMode := IconsModeDisabled;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itUseDefault, Info.IconType);
end;

procedure TIconProviderTest.TestGetIcon_IconsModeInternal_ReturnsInternal;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account');
	Context := CreateEmptyContext;
	Context.IconsMode := IconsModeInternal;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itInternal, Info.IconType);
end;

procedure TIconProviderTest.TestGetIcon_IconsModeInternalOverlay_ReturnsInternalOverlay;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account');
	Context := CreateEmptyContext;
	Context.IconsMode := IconsModeInternalOverlay;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itInternalOverlay, Info.IconType);
end;

procedure TIconProviderTest.TestGetIcon_IconsModeExternal_ReturnsExternal;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account');
	Context := CreateEmptyContext;
	Context.IconsMode := IconsModeExternal;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itExternal, Info.IconType);
end;

procedure TIconProviderTest.TestGetIcon_IconsModeExternalOverlay_ReturnsExternalOverlay;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account');
	Context := CreateEmptyContext;
	Context.IconsMode := IconsModeExternalOverlay;

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itExternalOverlay, Info.IconType);
end;

procedure TIconProviderTest.TestGetIcon_UnknownIconsMode_ReturnsUseDefault;
var
	Path: TRealPath;
	Context: TIconContext;
	Info: TIconInfo;
begin
	Path.FromPath('\account');
	Context := CreateEmptyContext;
	Context.IconsMode := 999; { Invalid mode triggers default else branch }

	Info := FProvider.GetIcon(Path, Context);

	Assert.AreEqual(itUseDefault, Info.IconType);
end;

initialization
	TDUnitX.RegisterTestFixture(TIconProviderTest);

end.
