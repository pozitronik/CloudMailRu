unit CloudFileIdentity;

interface

type
	TCloudFileIdentity = record
		Hash: WideString;
		Size: Int64;

	public
		function IsEqualTo(Identity: TCloudFileIdentity): Boolean;
	end;

implementation

{TCloudFileIdentity}

function TCloudFileIdentity.IsEqualTo(Identity: TCloudFileIdentity): Boolean;
begin
	Exit((self.Size = Identity.Size) and (self.Hash = Identity.Hash));
end;

end.
