unit CipherProfile;

{Cipher profile registry -- maps profile IDs to backend-agnostic cipher factories.
	Each profile defines a complete encryption configuration.
	Supports DCPCrypt, OpenSSL, and BCrypt backends via factory closures.}

interface

uses
	FileCipher,
	OpenSSLProvider,
	BCryptProvider;

type
	{Factory type: creates a fully initialized ICipher from parameters}
	TCipherFactory = reference to function(
		const Password: WideString;
		const PasswordControl: WideString;
		DoFilenameCipher: Boolean
	): ICipher;

	TCipherProfile = record
		Id: WideString;
		DisplayName: WideString;
		BackendName: WideString;
		KeySizeBits: Integer;
		CreateCipher: TCipherFactory;
	end;

	{Class-level registry of available cipher profiles.
		Call Initialize once at startup to populate. Thread-safe after initialization.}
	TCipherProfileRegistry = class
	private class var
		FProfiles: TArray<TCipherProfile>;
		FInitialized: Boolean;
	public
		class procedure Initialize(OpenSSLProvider: IOpenSSLProvider = nil; BCryptProvider: IBCryptProvider = nil);
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
	DCPserpent,
	OpenSSLCipher,
	BCryptCipher;

class procedure TCipherProfileRegistry.Initialize(OpenSSLProvider: IOpenSSLProvider = nil; BCryptProvider: IBCryptProvider = nil);
var
	NextIndex: Integer;
	HasOpenSSL, HasBCrypt: Boolean;
	CapturedFunctions: TOpenSSLFunctions;
	CapturedBCryptProvider: IBCryptProvider;
begin
	if FInitialized then
		Exit;

	{Detect available backends}
	HasOpenSSL := (OpenSSLProvider <> nil) and OpenSSLProvider.IsAvailable and OpenSSLProvider.GetFunctions.CipherLoaded;
	HasBCrypt := (BCryptProvider <> nil) and BCryptProvider.IsAvailable;

	{Base: 4 DCPCrypt profiles, plus conditional backends}
	NextIndex := 4;
	if HasOpenSSL then Inc(NextIndex);
	if HasBCrypt then Inc(NextIndex);
	SetLength(FProfiles, NextIndex);

	FProfiles[0].Id := CIPHER_PROFILE_LEGACY_DEFAULT;
	FProfiles[0].DisplayName := 'AES-256 / SHA-1 KDF (Legacy)';
	FProfiles[0].BackendName := 'Software (DCPCrypt)';
	FProfiles[0].KeySizeBits := 256;
	FProfiles[0].CreateCipher := function(const Password, PasswordControl: WideString; DoFilenameCipher: Boolean): ICipher
	begin
		Result := TFileCipher.Create(Password, TDCP_rijndael, TDCP_sha1, PasswordControl, DoFilenameCipher);
	end;

	FProfiles[1].Id := 'dcpcrypt-aes256-cfb8-sha256';
	FProfiles[1].DisplayName := 'AES-256 / SHA-256 KDF';
	FProfiles[1].BackendName := 'Software (DCPCrypt)';
	FProfiles[1].KeySizeBits := 256;
	FProfiles[1].CreateCipher := function(const Password, PasswordControl: WideString; DoFilenameCipher: Boolean): ICipher
	begin
		Result := TFileCipher.Create(Password, TDCP_rijndael, TDCP_sha256, PasswordControl, DoFilenameCipher);
	end;

	FProfiles[2].Id := 'dcpcrypt-twofish256-cfb8-sha256';
	FProfiles[2].DisplayName := 'Twofish-256 / SHA-256 KDF';
	FProfiles[2].BackendName := 'Software (DCPCrypt)';
	FProfiles[2].KeySizeBits := 256;
	FProfiles[2].CreateCipher := function(const Password, PasswordControl: WideString; DoFilenameCipher: Boolean): ICipher
	begin
		Result := TFileCipher.Create(Password, TDCP_twofish, TDCP_sha256, PasswordControl, DoFilenameCipher);
	end;

	FProfiles[3].Id := 'dcpcrypt-serpent256-cfb8-sha256';
	FProfiles[3].DisplayName := 'Serpent-256 / SHA-256 KDF';
	FProfiles[3].BackendName := 'Software (DCPCrypt)';
	FProfiles[3].KeySizeBits := 256;
	FProfiles[3].CreateCipher := function(const Password, PasswordControl: WideString; DoFilenameCipher: Boolean): ICipher
	begin
		Result := TFileCipher.Create(Password, TDCP_serpent, TDCP_sha256, PasswordControl, DoFilenameCipher);
	end;

	{Dynamic index for conditional backends}
	NextIndex := 4;

	{OpenSSL backend -- only registered when cipher functions are available}
	if HasOpenSSL then
	begin
		CapturedFunctions := OpenSSLProvider.GetFunctions;
		FProfiles[NextIndex].Id := 'openssl-aes256-cfb8-pbkdf2';
		FProfiles[NextIndex].DisplayName := 'AES-256 / PBKDF2 (OpenSSL)';
		FProfiles[NextIndex].BackendName := 'OpenSSL';
		FProfiles[NextIndex].KeySizeBits := 256;
		FProfiles[NextIndex].CreateCipher := function(const Password, PasswordControl: WideString; DoFilenameCipher: Boolean): ICipher
		begin
			Result := TOpenSSLCipher.Create(Password, CapturedFunctions, PasswordControl, DoFilenameCipher);
		end;
		Inc(NextIndex);
	end;

	{BCrypt backend -- available on Windows Vista+ via bcrypt.dll}
	if HasBCrypt then
	begin
		CapturedBCryptProvider := BCryptProvider;
		FProfiles[NextIndex].Id := 'bcrypt-aes256-cfb8-pbkdf2';
		FProfiles[NextIndex].DisplayName := 'AES-256 / PBKDF2 (BCrypt)';
		FProfiles[NextIndex].BackendName := 'Windows CNG';
		FProfiles[NextIndex].KeySizeBits := 256;
		FProfiles[NextIndex].CreateCipher := function(const Password, PasswordControl: WideString; DoFilenameCipher: Boolean): ICipher
		begin
			Result := TBCryptCipher.Create(Password, CapturedBCryptProvider, PasswordControl, DoFilenameCipher);
		end;
	end;

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
