unit CMRIncomingInviteList;

interface

uses
	CMRIncomingInvite;

type
	TCMRIncomingInviteList = TArray<TCMRIncomingInvite>;

	TCMRIncomingInviteListHelper = record helper for TCMRIncomingInviteList
		function FindByName(ItemName: WideString): TCMRIncomingInvite;
	end;

implementation

function TCMRIncomingInviteListHelper.FindByName(ItemName: WideString): TCMRIncomingInvite;
var
	CurrentItem: TCMRIncomingInvite;
begin
	for CurrentItem in self do
		if CurrentItem.name = ItemName then
			Exit(CurrentItem);
	Exit(CurrentItem.None)
end;

end.
