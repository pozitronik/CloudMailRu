unit SharedItemDeletionHandler;

{Handles shared item deletion by removing all sharing and publication.
	Unshares item with all collaborators and unpublishes if public.}

interface

uses
	CloudDirItem,
	CloudMailRu;

type
	ISharedItemDeletionHandler = interface
		['{DE60363C-7C11-4F5D-A363-310606242118}']

		{Removes all sharing and publication from an item.
			@param Cloud Cloud connection for the account
			@param Item The shared item to process
			@return True if operation completed (always true for shared items)}
		function Execute(Cloud: TCloudMailRu; const Item: TCloudDirItem): Boolean;
	end;

	TSharedItemDeletionHandler = class(TInterfacedObject, ISharedItemDeletionHandler)
	public
		function Execute(Cloud: TCloudMailRu; const Item: TCloudDirItem): Boolean;
	end;

implementation

uses
	CloudInviteList,
	CloudInvite,
	CloudConstants;

function TSharedItemDeletionHandler.Execute(Cloud: TCloudMailRu; const Item: TCloudDirItem): Boolean;
var
	InvitesListing: TCloudInviteList;
	Invite: TCloudInvite;
	Weblink: WideString;
begin
	{Get list of collaborators and unshare with each}
	Cloud.ShareService.GetShareInfo(Item.home, InvitesListing);
	for Invite in InvitesListing do
		Cloud.shareFolder(Item.home, Invite.email, CLOUD_SHARE_NO); //no reporting here

	{Unpublish if item has public link}
	if Item.isPublished then
	begin
		Weblink := Item.Weblink;
		Cloud.PublishFile(Item.home, Weblink, CLOUD_UNPUBLISH);
	end;

	Result := True;
end;

end.
