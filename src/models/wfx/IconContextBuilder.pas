unit IconContextBuilder;

{Builds TIconContext for icon provider.
 Handles public account detection, item lookup, and invite lookup.}

interface

uses
	Windows,
	IIconContextBuilderInterface,
	IIconProviderInterface,
	IListingItemFetcherInterface,
	IAccountsManagerInterface,
	RealPath,
	CMRDirItem,
	CMRDirItemList,
	CMRIncomingInvite,
	CMRIncomingInviteList,
	ConnectionManager;

type
	TIconContextBuilder = class(TInterfacedObject, IIconContextBuilder)
	private
		FAccountSettings: IAccountsManager;
		FConnectionManager: TConnectionManager;
		FListingItemFetcher: IListingItemFetcher;

		{Checks if account is a public account}
		function IsPublicAccount(const AccountName: WideString): Boolean;

		{Finds invite item in listing, refreshing if not found}
		function FindInviteItem(const Path: TRealPath;
			var InviteListing: TCMRIncomingInviteList): TCMRIncomingInvite;

		{Finds dir item in listing using ListingItemFetcher}
		function FindDirItem(const Path: TRealPath;
			var DirListing: TCMRDirItemList): TCMRDirItem;
	public
		constructor Create(
			AccountSettings: IAccountsManager;
			AConnectionManager: TConnectionManager;
			ListingItemFetcher: IListingItemFetcher);

		function BuildContext(const Input: TIconContextInput;
			var DirListing: TCMRDirItemList;
			var InviteListing: TCMRIncomingInviteList): TIconContext;
	end;

implementation

uses
	SysUtils,
	CloudMailRu;

constructor TIconContextBuilder.Create(
	AccountSettings: IAccountsManager;
	AConnectionManager: TConnectionManager;
	ListingItemFetcher: IListingItemFetcher);
begin
	inherited Create;
	FAccountSettings := AccountSettings;
	FConnectionManager := AConnectionManager;
	FListingItemFetcher := ListingItemFetcher;
end;

function TIconContextBuilder.IsPublicAccount(const AccountName: WideString): Boolean;
begin
	Result := FAccountSettings.GetAccountSettings(AccountName).PublicAccount;
end;

function TIconContextBuilder.FindInviteItem(const Path: TRealPath;
	var InviteListing: TCMRIncomingInviteList): TCMRIncomingInvite;
var
	getResult: Integer;
	Cloud: TCloudMailRu;
begin
	Result := InviteListing.FindByName(Path.Path);

	{Item not found in current listing, refresh and search again}
	if Result.isNone then
	begin
		Cloud := FConnectionManager.Get(Path.account, getResult);
		if Assigned(Cloud) and Cloud.getIncomingLinksListing(InviteListing) then
			Result := InviteListing.FindByName(Path.Path);
	end;
end;

function TIconContextBuilder.FindDirItem(const Path: TRealPath;
	var DirListing: TCMRDirItemList): TCMRDirItem;
var
	getResult: Integer;
	Cloud: TCloudMailRu;
begin
	if not Assigned(FConnectionManager) then
	begin
		Result := Result.None;
		Exit;
	end;

	Cloud := FConnectionManager.Get(Path.account, getResult);
	if not Assigned(Cloud) then
	begin
		Result := Result.None;
		Exit;
	end;

	Result := FListingItemFetcher.FetchItem(DirListing, Path, Cloud, True);
end;

function TIconContextBuilder.BuildContext(const Input: TIconContextInput;
	var DirListing: TCMRDirItemList;
	var InviteListing: TCMRIncomingInviteList): TIconContext;
begin
	{Initialize context with defaults}
	Result.IconsMode := Input.IconsMode;
	Result.HasItem := False;
	Result.HasInviteItem := False;
	Result.IsPublicAccount := False;

	{Check if account root and determine public account status}
	if Input.Path.isInAccountsList and not Input.Path.isVirtual then
		Result.IsPublicAccount := IsPublicAccount(Input.Path.account);

	{Find appropriate item based on path type}
	if Input.Path.invitesDir and not Input.Path.isInAccountsList then
	begin
		Result.InviteItem := FindInviteItem(Input.Path, InviteListing);
		Result.HasInviteItem := True;
	end
	else if not Input.Path.isInAccountsList and not Input.Path.isVirtual then
	begin
		Result.Item := FindDirItem(Input.Path, DirListing);
		Result.HasItem := True;
	end;
end;

end.
