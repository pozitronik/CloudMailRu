unit IListingProviderInterface;

{Interface for fetching directory listings based on virtual path type.
 Encapsulates the decision logic for which cloud API to call (trashbin, shared links,
 incoming invites, or regular directory listing).}

interface

uses
	CMRDirItemList,
	CMRIncomingInviteList,
	CloudMailRu,
	RealPath;

type
	IListingProvider = interface
		['{A7D3E8F2-5B1C-4A9D-8E6F-2C4B7A9D1E3F}']

		{Fetches the appropriate listing based on path type.
		 Determines which cloud API to call based on virtual directory flags in Path.
		 @param Cloud The cloud connection to use
		 @param Path The real path (trashDir/sharedDir/invitesDir flags determine listing type)
		 @param DirListing Output: directory item listing (always populated on success)
		 @param InviteListing Output: incoming invite listing (only populated for invites dir)
		 @return True if listing was fetched successfully}
		function FetchListing(
			Cloud: TCloudMailRu;
			const Path: TRealPath;
			var DirListing: TCMRDirItemList;
			var InviteListing: TCMRIncomingInviteList
		): Boolean;
	end;

implementation

end.
