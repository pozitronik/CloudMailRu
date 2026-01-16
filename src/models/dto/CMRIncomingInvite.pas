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
	private
		function GetIsNone: Boolean;
		function GetIsMounted: Boolean; //Check, if it is a special record which can't be used
	public
		property isNone: Boolean read GetIsNone;
		property isMounted: Boolean read GetIsMounted;
		function None: TCMRIncomingInvite; // Creates a special record, which indicate that Item is not found/not applicable.
	end;

implementation

{TCMRIncomingInvite}

function TCMRIncomingInvite.GetIsMounted: Boolean;
begin
	result := self.home <> EmptyWideStr;
end;

function TCMRIncomingInvite.GetIsNone: Boolean;
begin
	result := self.name = EmptyWideStr;
end;

function TCMRIncomingInvite.None: TCMRIncomingInvite;
begin
	FillChar(self, sizeof(self), 0);
	result := self;
end;

end.
