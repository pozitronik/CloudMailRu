unit IListingItemFetcherInterface;

{Interface for context-aware listing item fetching.
 Finds items in listing, refreshing from appropriate source if needed.}

interface

uses
	RealPath,
	CMRDirItem,
	CMRDirItemList,
	CloudMailRu;

type
	IListingItemFetcher = interface
		['{D9A6F3B2-1E8C-4D7A-C5B9-6F2E8A1D4C7B}']

		{Finds an item in the listing, optionally refreshing from cloud.
		 Searches by home path (private) or name (public).
		 If not found and UpdateListing is true, refreshes from appropriate source.
		 @param Listing Current directory listing (may be updated)
		 @param Path Path to find
		 @param Cloud Cloud connection for the account
		 @param UpdateListing True to refresh listing if not found
		 @return Found item or None record}
		function FetchItem(var Listing: TCMRDirItemList; const Path: TRealPath;
			Cloud: TCloudMailRu; UpdateListing: Boolean): TCMRDirItem;
	end;

implementation

end.
