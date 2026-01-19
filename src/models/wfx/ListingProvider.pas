unit ListingProvider;

{Provides directory listings based on virtual path type.
 Encapsulates the decision logic for which cloud API to call.}

interface

uses
	IListingProviderInterface,
	CMRDirItemList,
	CMRIncomingInviteList,
	CloudMailRu,
	RealPath;

type
	TListingProvider = class(TInterfacedObject, IListingProvider)
	public
		function FetchListing(
			Cloud: TCloudMailRu;
			const Path: TRealPath;
			var DirListing: TCMRDirItemList;
			var InviteListing: TCMRIncomingInviteList
		): Boolean;
	end;

implementation

function TListingProvider.FetchListing(
	Cloud: TCloudMailRu;
	const Path: TRealPath;
	var DirListing: TCMRDirItemList;
	var InviteListing: TCMRIncomingInviteList
): Boolean;
begin
	if Path.trashDir then
		Result := Cloud.getTrashbinListing(DirListing)
	else if Path.sharedDir then
		Result := Cloud.getSharedLinksListing(DirListing) {Results will be interpreted as symlinks later}
	else if Path.invitesDir then
		Result := Cloud.getIncomingLinksListing(DirListing, InviteListing) {Fetch both listings to avoid re-reading invites on every action}
	else
		{Need to verify target is a directory - API returns parent listing for files, see issue #174}
		Result := Cloud.getDirListing(Path.Path, DirListing);
end;

end.
