unit IconContextBuilder;

{Builds TIconContext for icon provider.
 Encapsulates the logic for determining public account status,
 and finding the appropriate item (dir item or invite) for icon display.
 Handles public account detection, item lookup, and invite lookup.}

interface

uses
	Windows,
	IconProvider,
	ListingItemFetcher,
	IAccountsManagerInterface,
	IConnectionManagerInterface,
	RealPath,
	CMRDirItem,
	CMRDirItemList,
	CMRIncomingInvite,
	CMRIncomingInviteList;

type
	{Input data for context building - all external data needed}
	TIconContextInput = record
		Path: TRealPath;
		IconsMode: Integer;
	end;

	{Builds TIconContext from path and current listings.
	 Handles public account detection and item lookup.}
	IIconContextBuilder = interface
		['{C4D5E6F7-A8B9-0123-CDEF-456789ABCDEF}']

		{Builds icon context from input and current listings.
		 @param Input Path, remote name, and icon mode settings
		 @param DirListing Current directory listing (may be refreshed if item not found)
		 @param InviteListing Current invite listing (may be refreshed if item not found)
		 @return TIconContext ready for IIconProvider}
		function BuildContext(const Input: TIconContextInput;
			var DirListing: TCMRDirItemList;
			var InviteListing: TCMRIncomingInviteList): TIconContext;
	end;

	TIconContextBuilder = class(TInterfacedObject, IIconContextBuilder)
	private
		FAccountSettings: IAccountsManager;
		FConnectionManager: IConnectionManager;
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
			AConnectionManager: IConnectionManager;
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
	AConnectionManager: IConnectionManager;
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
begin
	Result := InviteListing.FindByName(Path.Path);

	{Item not found in current listing, refresh and search again}
	if Result.isNone then
		if FConnectionManager.Get(Path.account, getResult).getIncomingLinksListing(InviteListing) then
			Result := InviteListing.FindByName(Path.Path);
end;

function TIconContextBuilder.FindDirItem(const Path: TRealPath;
	var DirListing: TCMRDirItemList): TCMRDirItem;
var
	getResult: Integer;
	Cloud: TCloudMailRu;
begin
	Cloud := FConnectionManager.Get(Path.account, getResult);
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
