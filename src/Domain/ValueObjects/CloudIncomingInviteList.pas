unit CloudIncomingInviteList;

interface

uses
	CloudIncomingInvite;

type
	TCloudIncomingInviteList = TArray<TCloudIncomingInvite>;

	TCloudIncomingInviteListHelper = record helper for TCloudIncomingInviteList
		function FindByName(ItemName: WideString): TCloudIncomingInvite;
	end;

implementation

function TCloudIncomingInviteListHelper.FindByName(ItemName: WideString): TCloudIncomingInvite;
var
	CurrentItem: TCloudIncomingInvite;
begin
	for CurrentItem in self do
		if CurrentItem.name = ItemName then
			Exit(CurrentItem);
	Exit(CurrentItem.None)
end;

end.
