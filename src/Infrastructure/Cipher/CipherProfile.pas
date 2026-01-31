unit CipherProfile;

{Cipher profile registry -- maps profile IDs to concrete DCPCrypt cipher+hash class pairs.
	Each profile defines a complete encryption configuration (algorithm, key size, KDF hash).
	Only the Software/DCPCrypt backend is populated now; BCrypt/OpenSSL backends can be added later.}

interface

uses
	DCPcrypt2,
	DCPblockciphers;

type
	TDCP_blockcipher128class = class of TDCP_blockcipher128;

	TCipherProfile = record
		Id: WideString;
		DisplayName: WideString;
		BackendName: WideString;
		CipherClass: TDCP_blockcipher128class;
		HashClass: TDCP_hashclass;
		KeySizeBits: Integer;
	end;

	{Class-level registry of available cipher profiles.
		Call Initialize once at startup to populate. Thread-safe after initialization.}
	TCipherProfileRegistry = class
	private class var
		FProfiles: TArray<TCipherProfile>;
		FInitialized: Boolean;
	public
		class procedure Initialize;
		class function GetProfiles: TArray<TCipherProfile>;
		class function FindById(const ProfileId: WideString; out Profile: TCipherProfile): Boolean;
		class function GetDefaultProfile: TCipherProfile;
		class function Count: Integer;
	end;

const
	CIPHER_PROFILE_LEGACY_DEFAULT = 'dcpcrypt-aes256-cfb8-sha1';

implementation

uses
	DCPrijndael,
	DCPsha1,
	DCPsha256,
	DCPtwofish,
	DCPserpent;

class procedure TCipherProfileRegistry.Initialize;
begin
	if FInitialized then
		Exit;

	SetLength(FProfiles, 4);

	FProfiles[0].Id := CIPHER_PROFILE_LEGACY_DEFAULT;
	FProfiles[0].DisplayName := 'AES-256 / SHA-1 KDF (Legacy)';
	FProfiles[0].BackendName := 'Software (DCPCrypt)';
	FProfiles[0].CipherClass := TDCP_rijndael;
	FProfiles[0].HashClass := TDCP_sha1;
	FProfiles[0].KeySizeBits := 256;

	FProfiles[1].Id := 'dcpcrypt-aes256-cfb8-sha256';
	FProfiles[1].DisplayName := 'AES-256 / SHA-256 KDF';
	FProfiles[1].BackendName := 'Software (DCPCrypt)';
	FProfiles[1].CipherClass := TDCP_rijndael;
	FProfiles[1].HashClass := TDCP_sha256;
	FProfiles[1].KeySizeBits := 256;

	FProfiles[2].Id := 'dcpcrypt-twofish256-cfb8-sha256';
	FProfiles[2].DisplayName := 'Twofish-256 / SHA-256 KDF';
	FProfiles[2].BackendName := 'Software (DCPCrypt)';
	FProfiles[2].CipherClass := TDCP_twofish;
	FProfiles[2].HashClass := TDCP_sha256;
	FProfiles[2].KeySizeBits := 256;

	FProfiles[3].Id := 'dcpcrypt-serpent256-cfb8-sha256';
	FProfiles[3].DisplayName := 'Serpent-256 / SHA-256 KDF';
	FProfiles[3].BackendName := 'Software (DCPCrypt)';
	FProfiles[3].CipherClass := TDCP_serpent;
	FProfiles[3].HashClass := TDCP_sha256;
	FProfiles[3].KeySizeBits := 256;

	FInitialized := True;
end;

class function TCipherProfileRegistry.GetProfiles: TArray<TCipherProfile>;
begin
	Result := FProfiles;
end;

class function TCipherProfileRegistry.FindById(const ProfileId: WideString; out Profile: TCipherProfile): Boolean;
var
	I: Integer;
begin
	for I := 0 to High(FProfiles) do
		if FProfiles[I].Id = ProfileId then
		begin
			Profile := FProfiles[I];
			Exit(True);
		end;
	Result := False;
end;

class function TCipherProfileRegistry.GetDefaultProfile: TCipherProfile;
begin
	{First profile is always the legacy default}
	Result := FProfiles[0];
end;

class function TCipherProfileRegistry.Count: Integer;
begin
	Result := Length(FProfiles);
end;

end.
