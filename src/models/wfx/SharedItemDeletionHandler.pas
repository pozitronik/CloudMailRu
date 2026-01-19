unit SharedItemDeletionHandler;

{Handles shared item deletion by removing all sharing and publication.}

interface

uses
	CMRDirItem,
	CloudMailRu,
	ISharedItemDeletionHandlerInterface;

type
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
