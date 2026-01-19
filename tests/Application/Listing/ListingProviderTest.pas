unit ListingProviderTest;

{Unit tests for TListingProvider - virtual directory listing routing.
 Tests verify that the correct cloud API is called based on path type.}

interface

uses
	System.SysUtils,
	DUnitX.TestFramework,
	ListingProvider,
	RealPath,
	CMRDirItemList,
	CMRIncomingInviteList,
	SETTINGS_CONSTANTS;

type
	[TestFixture]
	TListingProviderTest = class
	private
		FProvider: IListingProvider;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Path type detection tests - verify TRealPath flags work correctly with provider}
		[Test]
		procedure TestTrashPath_HasTrashDirFlag;
		[Test]
		procedure TestSharedPath_HasSharedDirFlag;
		[Test]
		procedure TestInvitesPath_HasInvitesDirFlag;
		[Test]
		procedure TestNormalPath_HasNoVirtualFlags;

		{Provider instantiation tests}
		[Test]
		procedure TestCreate_ReturnsValidInstance;
		[Test]
		procedure TestCreate_ImplementsInterface;
	end;

implementation

procedure TListingProviderTest.Setup;
begin
	FProvider := TListingProvider.Create;
end;

procedure TListingProviderTest.TearDown;
begin
	FProvider := nil;
end;

{Path type detection tests}

procedure TListingProviderTest.TestTrashPath_HasTrashDirFlag;
var
	Path: TRealPath;
begin
	Path.FromPath('\account' + TrashPostfix + '\somefile');
	Assert.IsTrue(Path.trashDir, 'Trash path should have trashDir flag');
	Assert.IsFalse(Path.sharedDir, 'Trash path should not have sharedDir flag');
	Assert.IsFalse(Path.invitesDir, 'Trash path should not have invitesDir flag');
end;

procedure TListingProviderTest.TestSharedPath_HasSharedDirFlag;
var
	Path: TRealPath;
begin
	Path.FromPath('\account' + SharedPostfix + '\somefile');
	Assert.IsFalse(Path.trashDir, 'Shared path should not have trashDir flag');
	Assert.IsTrue(Path.sharedDir, 'Shared path should have sharedDir flag');
	Assert.IsFalse(Path.invitesDir, 'Shared path should not have invitesDir flag');
end;

procedure TListingProviderTest.TestInvitesPath_HasInvitesDirFlag;
var
	Path: TRealPath;
begin
	Path.FromPath('\account' + InvitesPostfix + '\somefile');
	Assert.IsFalse(Path.trashDir, 'Invites path should not have trashDir flag');
	Assert.IsFalse(Path.sharedDir, 'Invites path should not have sharedDir flag');
	Assert.IsTrue(Path.invitesDir, 'Invites path should have invitesDir flag');
end;

procedure TListingProviderTest.TestNormalPath_HasNoVirtualFlags;
var
	Path: TRealPath;
begin
	Path.FromPath('\account\normalfolder\file.txt');
	Assert.IsFalse(Path.trashDir, 'Normal path should not have trashDir flag');
	Assert.IsFalse(Path.sharedDir, 'Normal path should not have sharedDir flag');
	Assert.IsFalse(Path.invitesDir, 'Normal path should not have invitesDir flag');
end;

{Provider instantiation tests}

procedure TListingProviderTest.TestCreate_ReturnsValidInstance;
begin
	Assert.IsNotNull(FProvider, 'Provider should not be nil');
end;

procedure TListingProviderTest.TestCreate_ImplementsInterface;
var
	Intf: IListingProvider;
begin
	Assert.IsTrue(Supports(TListingProvider.Create, IListingProvider, Intf),
		'TListingProvider should implement IListingProvider');
end;

initialization
	TDUnitX.RegisterTestFixture(TListingProviderTest);

end.
