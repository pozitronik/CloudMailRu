unit IIconContextBuilderInterface;

{Interface for building icon context from path and listings.
 Encapsulates the logic for determining public account status,
 and finding the appropriate item (dir item or invite) for icon display.}

interface

uses
	RealPath,
	CMRDirItemList,
	CMRIncomingInviteList,
	IIconProviderInterface;

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

implementation

end.
