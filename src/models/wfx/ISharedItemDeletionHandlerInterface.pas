unit ISharedItemDeletionHandlerInterface;

{Interface for handling shared item deletion.
 Unshares item with all collaborators and unpublishes if public.}

interface

uses
	CMRDirItem,
	CloudMailRu;

type
	ISharedItemDeletionHandler = interface
		['{A7B3C9D1-2E4F-5A6B-8C7D-9E0F1A2B3C4D}']

		{Removes all sharing and publication from an item.
		 @param Cloud Cloud connection for the account
		 @param Item The shared item to process
		 @return True if operation completed (always true for shared items)}
		function Execute(Cloud: TCloudMailRu; const Item: TCMRDirItem): Boolean;
	end;

implementation

end.
