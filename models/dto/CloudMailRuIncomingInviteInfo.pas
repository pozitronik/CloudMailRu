unit CloudMailRuIncomingInviteInfo;

interface

uses
	CMROwnerInfo;

type
	TCloudMailRuIncomingInviteInfo = record
		owner: TCMROwnerInfo;
		tree: WideString;
		access: WideString;
		name: WideString;
		size: int64;
		home: WideString; //only on already mounted items
		invite_token: WideString;
	end;

implementation

end.
