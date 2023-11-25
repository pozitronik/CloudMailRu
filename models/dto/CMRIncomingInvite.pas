unit CMRIncomingInvite;

interface

uses
	CMROwner,
	SysUtils;

type
	TCMRIncomingInvite = record
		owner: TCMROwner;
		tree: WideString;
		access: WideString;
		name: WideString;
		size: int64;
		home: WideString; //only on already mounted items
		invite_token: WideString;
		function None: TCMRIncomingInvite; // Creates a special record, which indicate that Item is not found/not applicable.
		function IsNone: Boolean; //Check, if it is a special record which can't be used
	end;

implementation

{TCMRIncomingInvite}

function TCMRIncomingInvite.IsNone: Boolean;
begin
	Result := self.name = EmptyWideStr;
end;

function TCMRIncomingInvite.None: TCMRIncomingInvite;
begin
	FillChar(self, sizeof(self), 0);
	Result := self;
end;

end.
