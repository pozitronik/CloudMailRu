unit CloudMailRuIncomingInviteInfo;

interface

uses
	CloudMailRuOwnerInfo;

type
	TCloudMailRuIncomingInviteInfo = record
		owner: TCloudMailRuOwnerInfo;
		tree: WideString;
		access: WideString;
		name: WideString;
		size: int64;
		home: WideString; //only on already mounted items
		invite_token: WideString;
	end;

implementation

end.
