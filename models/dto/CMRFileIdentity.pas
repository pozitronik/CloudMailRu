unit CMRFileIdentity;

interface

type
	TCMRFileIdentity = record
		Hash: WideString;
		Size: Int64;

	public
		function IsEqualTo(Identity: TCMRFileIdentity): Boolean;
	end;

implementation

{TCMRFileIdentity}

function TCMRFileIdentity.IsEqualTo(Identity: TCMRFileIdentity): Boolean;
begin
	Exit((self.Size = Identity.Size) and (self.Hash = Identity.Hash));
end;

end.
