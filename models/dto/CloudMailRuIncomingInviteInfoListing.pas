unit CloudMailRuIncomingInviteInfoListing;

interface

uses
	RealPath,
	CloudMailRuIncomingInviteInfo;

type
	TCloudMailRuIncomingInviteInfoListing = TArray<TCloudMailRuIncomingInviteInfo>;

function FindByName(InviteListing: TCloudMailRuIncomingInviteInfoListing; ItemName: WideString): TCloudMailRuIncomingInviteInfo;

implementation

function FindByName(InviteListing: TCloudMailRuIncomingInviteInfoListing; ItemName: WideString): TCloudMailRuIncomingInviteInfo;
var
	CurrentItem: TCloudMailRuIncomingInviteInfo;
begin
	for CurrentItem in InviteListing do
		if CurrentItem.name = ItemName then
			exit(CurrentItem);
end;



end.
