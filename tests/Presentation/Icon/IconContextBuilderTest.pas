unit IconContextBuilderTest;

{Unit tests for TIconContextBuilder.
 Tests icon context building with various path types and account settings.}

interface

uses
	DUnitX.TestFramework,
	Windows,
	RealPath,
	CloudDirItem,
	CloudDirItemList,
	CloudIncomingInvite,
	CloudIncomingInviteList,
	AccountSettings,
	CloudMailRu,
	CloudSettings,
	ConnectionManager,
	IconProvider,
	IconContextBuilder,
	AccountsManager,
	ListingItemFetcher,
	MockConnectionManager,
	WSList;

type
	{Mock accounts manager for testing - implements interface directly}
	TMockAccountsManager = class(TInterfacedObject, IAccountsManager)
	private
		FPublicAccountNames: array of WideString;
	public
		procedure AddPublicAccount(const AccountName: WideString);
		function GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
		function GetAccountSettings(Account: WideString): TAccountSettings;
		procedure SetAccountSettings(Account: WideString; AccountSettings: TAccountSettings); overload;
		procedure SetAccountSettings(AccountSettings: TAccountSettings); overload;
		procedure DeleteAccount(Account: WideString);
		procedure RenameAccount(const OldName, NewName: WideString);
		procedure SwitchPasswordStorage(Account: WideString);
		procedure SetCryptedGUID(Account: WideString; GUID: WideString);
	end;

	{Mock listing item fetcher for testing}
	TMockIconListingItemFetcher = class(TInterfacedObject, IListingItemFetcher)
	private
		FReturnItem: TCloudDirItem;
		FFetchCalled: Boolean;
	public
		property FetchCalled: Boolean read FFetchCalled;
		procedure SetReturnItem(const Item: TCloudDirItem);
		function FetchItem(var Listing: TCloudDirItemList; const Path: TRealPath;
			Cloud: TCloudMailRu; UpdateListing: Boolean): TCloudDirItem;
	end;

	[TestFixture]
	TIconContextBuilderTest = class
	private
		FBuilder: IIconContextBuilder;
		FMockAccountSettings: TMockAccountsManager;
		FMockItemFetcher: TMockIconListingItemFetcher;
		FMockConnectionManager: TMockConnectionManager;

		function CreateInviteItem(const Name: WideString): TCloudIncomingInvite;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Basic context building tests}
		[Test]
		procedure TestBuildContext_SetsIconsMode;
		[Test]
		procedure TestBuildContext_DefaultsHasItemFalse;
		[Test]
		procedure TestBuildContext_DefaultsHasInviteItemFalse;
		[Test]
		procedure TestBuildContext_DefaultsIsPublicAccountFalse;

		{Public account detection tests}
		[Test]
		procedure TestBuildContext_AccountRoot_DetectsPublicAccount;
		[Test]
		procedure TestBuildContext_AccountRoot_DetectsPrivateAccount;
		[Test]
		procedure TestBuildContext_VirtualPath_SkipsPublicAccountCheck;

		{Dir item lookup tests}
		[Test]
		procedure TestBuildContext_RegularPath_FindsDirItem;
		[Test]
		procedure TestBuildContext_RegularPath_SetsHasItemTrue;
		[Test]
		procedure TestBuildContext_AccountRoot_SkipsDirItemLookup;

		{Invite item lookup tests}
		[Test]
		procedure TestBuildContext_InvitesPath_FindsInviteItem;
		[Test]
		procedure TestBuildContext_InvitesPath_SetsHasInviteItemTrue;
		[Test]
		procedure TestBuildContext_InvitesRoot_SkipsInviteItemLookup;
	end;

implementation

uses
	SysUtils;

{TMockAccountsManager}

procedure TMockAccountsManager.AddPublicAccount(const AccountName: WideString);
begin
	SetLength(FPublicAccountNames, Length(FPublicAccountNames) + 1);
	FPublicAccountNames[High(FPublicAccountNames)] := AccountName;
end;

function TMockAccountsManager.GetAccountsList(const AccountTypes: EAccountType; const VirtualTypes: EVirtualType): TWSList;
begin
	Result.Clear;
end;

function TMockAccountsManager.GetAccountSettings(Account: WideString): TAccountSettings;
var
	i: Integer;
begin
	Result := Default(TAccountSettings);
	Result.Account := Account;
	for i := 0 to High(FPublicAccountNames) do
		if FPublicAccountNames[i] = Account then
		begin
			Result.PublicAccount := True;
			Exit;
		end;
end;

procedure TMockAccountsManager.SetAccountSettings(Account: WideString; AccountSettings: TAccountSettings);
begin
	{No-op for mock}
end;

procedure TMockAccountsManager.SetAccountSettings(AccountSettings: TAccountSettings);
begin
	{No-op for mock}
end;

procedure TMockAccountsManager.DeleteAccount(Account: WideString);
begin
	{No-op for mock}
end;

procedure TMockAccountsManager.RenameAccount(const OldName, NewName: WideString);
begin
	{No-op for mock}
end;

procedure TMockAccountsManager.SwitchPasswordStorage(Account: WideString);
begin
	{No-op for mock}
end;

procedure TMockAccountsManager.SetCryptedGUID(Account: WideString; GUID: WideString);
begin
	{No-op for mock}
end;

{TMockIconListingItemFetcher}

procedure TMockIconListingItemFetcher.SetReturnItem(const Item: TCloudDirItem);
begin
	FReturnItem := Item;
end;

function TMockIconListingItemFetcher.FetchItem(var Listing: TCloudDirItemList;
	const Path: TRealPath; Cloud: TCloudMailRu; UpdateListing: Boolean): TCloudDirItem;
begin
	FFetchCalled := True;
	Result := FReturnItem;
end;

{TIconContextBuilderTest}

function TIconContextBuilderTest.CreateInviteItem(const Name: WideString): TCloudIncomingInvite;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.name := Name;
end;

procedure TIconContextBuilderTest.Setup;
begin
	FMockAccountSettings := TMockAccountsManager.Create;
	FMockItemFetcher := TMockIconListingItemFetcher.Create;
	FMockConnectionManager := TMockConnectionManager.Create;

	FBuilder := TIconContextBuilder.Create(
		FMockAccountSettings,
		FMockConnectionManager,
		FMockItemFetcher);
end;

procedure TIconContextBuilderTest.TearDown;
begin
	FBuilder := nil;
	FMockConnectionManager := nil;
	FMockItemFetcher := nil;
	FMockAccountSettings := nil;
end;

{Basic context building tests}

procedure TIconContextBuilderTest.TestBuildContext_SetsIconsMode;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
begin
	Input.Path.FromPath('\account');
	// RemoteName removed := '\account';
	Input.IconsMode := 3;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 0);

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	Assert.AreEqual(3, Context.IconsMode);
end;

procedure TIconContextBuilderTest.TestBuildContext_DefaultsHasItemFalse;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
begin
	Input.Path.FromPath('\account');
	// RemoteName removed := '\account';
	Input.IconsMode := 1;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 0);

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	Assert.IsFalse(Context.HasItem);
end;

procedure TIconContextBuilderTest.TestBuildContext_DefaultsHasInviteItemFalse;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
begin
	Input.Path.FromPath('\account');
	// RemoteName removed := '\account';
	Input.IconsMode := 1;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 0);

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	Assert.IsFalse(Context.HasInviteItem);
end;

procedure TIconContextBuilderTest.TestBuildContext_DefaultsIsPublicAccountFalse;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
begin
	{Use virtual path to avoid triggering FindDirItem which requires ConnectionManager}
	Input.Path.FromPath('\account.trash\subfolder');
	Input.IconsMode := 1;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 0);

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	Assert.IsFalse(Context.IsPublicAccount);
end;

{Public account detection tests}

procedure TIconContextBuilderTest.TestBuildContext_AccountRoot_DetectsPublicAccount;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
begin
	FMockAccountSettings.AddPublicAccount('public_acc');
	Input.Path.FromPath('\public_acc');
	// RemoteName removed := '\public_acc';
	Input.IconsMode := 1;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 0);

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	Assert.IsTrue(Context.IsPublicAccount);
end;

procedure TIconContextBuilderTest.TestBuildContext_AccountRoot_DetectsPrivateAccount;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
begin
	Input.Path.FromPath('\private_acc');
	// RemoteName removed := '\private_acc';
	Input.IconsMode := 1;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 0);

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	Assert.IsFalse(Context.IsPublicAccount);
end;

procedure TIconContextBuilderTest.TestBuildContext_VirtualPath_SkipsPublicAccountCheck;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
begin
	FMockAccountSettings.AddPublicAccount('account');
	Input.Path.FromPath('\account.trash');
	// RemoteName removed := '\account.trash';
	Input.IconsMode := 1;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 0);

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	{Virtual paths should not check public account status}
	Assert.IsFalse(Context.IsPublicAccount);
end;

{Dir item lookup tests}

procedure TIconContextBuilderTest.TestBuildContext_RegularPath_FindsDirItem;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
	TestItem: TCloudDirItem;
begin
	{ConnectionManager returns nil for 'account' -- mock fetcher ignores Cloud parameter}
	TestItem := Default(TCloudDirItem);
	TestItem.name := 'test.txt';
	TestItem.type_ := 'file';
	TestItem.size := 42;
	FMockItemFetcher.SetReturnItem(TestItem);

	Input.Path.FromPath('\account\folder\test.txt');
	Input.IconsMode := 1;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 0);

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	Assert.IsTrue(Context.HasItem, 'Regular path should set HasItem');
	Assert.IsFalse(Context.HasInviteItem, 'Regular path should not set HasInviteItem');
	Assert.IsTrue(FMockItemFetcher.FetchCalled, 'Should call fetcher for regular path');
	Assert.AreEqual('test.txt', string(Context.Item.name));
	Assert.AreEqual(Int64(42), Context.Item.size);
end;

procedure TIconContextBuilderTest.TestBuildContext_RegularPath_SetsHasItemTrue;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
	TestItem: TCloudDirItem;
begin
	{ConnectionManager returns nil for 'account' -- mock fetcher ignores Cloud parameter}
	TestItem := Default(TCloudDirItem);
	TestItem.name := 'file.txt';
	TestItem.type_ := 'file';
	FMockItemFetcher.SetReturnItem(TestItem);

	Input.Path.FromPath('\account\file.txt');
	Input.IconsMode := 1;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 0);

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	Assert.IsTrue(Context.HasItem);
	Assert.IsTrue(FMockItemFetcher.FetchCalled, 'Should call fetcher for regular path');
	Assert.AreEqual('file.txt', string(Context.Item.name));
end;

procedure TIconContextBuilderTest.TestBuildContext_AccountRoot_SkipsDirItemLookup;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
begin
	Input.Path.FromPath('\account');
	// RemoteName removed := '\account';
	Input.IconsMode := 1;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 0);

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	Assert.IsFalse(FMockItemFetcher.FetchCalled, 'Should not call fetcher for account root');
	Assert.IsFalse(Context.HasItem);
end;

{Invite item lookup tests}

procedure TIconContextBuilderTest.TestBuildContext_InvitesPath_FindsInviteItem;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
begin
	Input.Path.FromPath('\account.invites\shared_folder');
	// RemoteName removed := '\account.invites\shared_folder';
	Input.IconsMode := 1;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 1);
	InviteListing[0] := CreateInviteItem('shared_folder');

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	Assert.AreEqual('shared_folder', string(Context.InviteItem.name));
end;

procedure TIconContextBuilderTest.TestBuildContext_InvitesPath_SetsHasInviteItemTrue;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
begin
	Input.Path.FromPath('\account.invites\folder');
	// RemoteName removed := '\account.invites\folder';
	Input.IconsMode := 1;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 1);
	InviteListing[0] := CreateInviteItem('folder');

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	Assert.IsTrue(Context.HasInviteItem);
end;

procedure TIconContextBuilderTest.TestBuildContext_InvitesRoot_SkipsInviteItemLookup;
var
	Input: TIconContextInput;
	DirListing: TCloudDirItemList;
	InviteListing: TCloudIncomingInviteList;
	Context: TIconContext;
begin
	Input.Path.FromPath('\account.invites');
	// RemoteName removed := '\account.invites';
	Input.IconsMode := 1;
	SetLength(DirListing, 0);
	SetLength(InviteListing, 0);

	Context := FBuilder.BuildContext(Input, DirListing, InviteListing);

	{Account root in invites should not look up invite items}
	Assert.IsFalse(Context.HasInviteItem);
end;

initialization
	TDUnitX.RegisterTestFixture(TIconContextBuilderTest);

end.
