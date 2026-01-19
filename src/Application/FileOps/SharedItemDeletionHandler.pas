unit SharedItemDeletionHandler;

{Handles shared item deletion by removing all sharing and publication.
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

	TSharedItemDeletionHandler = class(TInterfacedObject, ISharedItemDeletionHandler)
	public
		function Execute(Cloud: TCloudMailRu; const Item: TCMRDirItem): Boolean;
	end;

implementation

uses
	CMRInviteList,
	CMRInvite,
	CMRConstants;

function TSharedItemDeletionHandler.Execute(Cloud: TCloudMailRu; const Item: TCMRDirItem): Boolean;
var
	InvitesListing: TCMRInviteList;
	Invite: TCMRInvite;
	Weblink: WideString;
begin
	if not Assigned(Cloud) then
		Exit(False);

	{Get list of collaborators and unshare with each}
	Cloud.GetShareInfo(Item.home, InvitesListing);
	for Invite in InvitesListing do
		Cloud.shareFolder(Item.home, Invite.email, CLOUD_SHARE_NO); //no reporting here

	{Unpublish if item has public link}
	if Item.isPublished then
	begin
		Weblink := Item.weblink;
		Cloud.PublishFile(Item.home, Weblink, CLOUD_UNPUBLISH);
	end;

	Result := True;
end;

end.
