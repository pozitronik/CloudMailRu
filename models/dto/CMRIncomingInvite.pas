unit CMRIncomingInvite;

interface

uses
	CMROwner;

type
	TCloudMailRuIncomingInviteInfo = record
		owner: TCMROwner;
		tree: WideString;
		access: WideString;
		name: WideString;
		size: int64;
		home: WideString; //only on already mounted items
		invite_token: WideString;
	end;

implementation

end.
