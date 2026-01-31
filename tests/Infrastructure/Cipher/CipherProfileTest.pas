unit CipherProfileTest;

interface

uses
	CipherProfile,
	DCPcrypt2,
	DCPblockciphers,
	DCPrijndael,
	DCPsha1,
	DCPsha256,
	DCPtwofish,
	DCPserpent,
	System.SysUtils,
	DUnitX.TestFramework;

type
	[TestFixture]
	TCipherProfileRegistryTest = class
	public
		[SetupFixture]
		procedure SetupFixture;

		{Registry initialization}
		[Test]
		procedure TestRegistryCount_Returns4Profiles;
		[Test]
		procedure TestGetProfiles_ReturnsNonEmptyArray;

		{FindById -- known profiles}
		[Test]
		procedure TestFindById_LegacyAES256SHA1_ReturnsTrue;
		[Test]
		procedure TestFindById_AES256SHA256_ReturnsTrue;
		[Test]
		procedure TestFindById_Twofish256SHA256_ReturnsTrue;
		[Test]
		procedure TestFindById_Serpent256SHA256_ReturnsTrue;

		{FindById -- unknown}
		[Test]
		procedure TestFindById_UnknownId_ReturnsFalse;
		[Test]
		procedure TestFindById_EmptyId_ReturnsFalse;

		{Default profile}
		[Test]
		procedure TestGetDefaultProfile_ReturnsLegacyProfile;
		[Test]
		procedure TestGetDefaultProfile_HasCorrectCipherClass;
		[Test]
		procedure TestGetDefaultProfile_HasCorrectHashClass;

		{Profile ID uniqueness}
		[Test]
		procedure TestAllProfileIds_AreUnique;

		{Class references are valid (can instantiate)}
		[Test]
		procedure TestAllProfiles_HaveValidCipherClasses;
		[Test]
		procedure TestAllProfiles_HaveValidHashClasses;

		{Profile field completeness}
		[Test]
		procedure TestAllProfiles_HaveNonEmptyDisplayName;
		[Test]
		procedure TestAllProfiles_HaveNonEmptyBackendName;
		[Test]
		procedure TestAllProfiles_Have256BitKeySize;
	end;

implementation

procedure TCipherProfileRegistryTest.SetupFixture;
begin
	TCipherProfileRegistry.Initialize;
end;

{Registry initialization}

procedure TCipherProfileRegistryTest.TestRegistryCount_Returns4Profiles;
begin
	Assert.AreEqual(4, TCipherProfileRegistry.Count);
end;

procedure TCipherProfileRegistryTest.TestGetProfiles_ReturnsNonEmptyArray;
begin
	Assert.IsTrue(Length(TCipherProfileRegistry.GetProfiles) > 0);
end;

{FindById -- known profiles}

procedure TCipherProfileRegistryTest.TestFindById_LegacyAES256SHA1_ReturnsTrue;
var
	Profile: TCipherProfile;
begin
	Assert.IsTrue(TCipherProfileRegistry.FindById('dcpcrypt-aes256-cfb8-sha1', Profile));
	Assert.AreEqual('AES-256 / SHA-1 KDF (Legacy)', Profile.DisplayName);
end;

procedure TCipherProfileRegistryTest.TestFindById_AES256SHA256_ReturnsTrue;
var
	Profile: TCipherProfile;
begin
	Assert.IsTrue(TCipherProfileRegistry.FindById('dcpcrypt-aes256-cfb8-sha256', Profile));
	Assert.AreEqual('AES-256 / SHA-256 KDF', Profile.DisplayName);
end;

procedure TCipherProfileRegistryTest.TestFindById_Twofish256SHA256_ReturnsTrue;
var
	Profile: TCipherProfile;
begin
	Assert.IsTrue(TCipherProfileRegistry.FindById('dcpcrypt-twofish256-cfb8-sha256', Profile));
	Assert.AreEqual('Twofish-256 / SHA-256 KDF', Profile.DisplayName);
end;

procedure TCipherProfileRegistryTest.TestFindById_Serpent256SHA256_ReturnsTrue;
var
	Profile: TCipherProfile;
begin
	Assert.IsTrue(TCipherProfileRegistry.FindById('dcpcrypt-serpent256-cfb8-sha256', Profile));
	Assert.AreEqual('Serpent-256 / SHA-256 KDF', Profile.DisplayName);
end;

{FindById -- unknown}

procedure TCipherProfileRegistryTest.TestFindById_UnknownId_ReturnsFalse;
var
	Profile: TCipherProfile;
begin
	Assert.IsFalse(TCipherProfileRegistry.FindById('nonexistent-profile', Profile));
end;

procedure TCipherProfileRegistryTest.TestFindById_EmptyId_ReturnsFalse;
var
	Profile: TCipherProfile;
begin
	Assert.IsFalse(TCipherProfileRegistry.FindById('', Profile));
end;

{Default profile}

procedure TCipherProfileRegistryTest.TestGetDefaultProfile_ReturnsLegacyProfile;
begin
	Assert.AreEqual(CIPHER_PROFILE_LEGACY_DEFAULT, TCipherProfileRegistry.GetDefaultProfile.Id);
end;

procedure TCipherProfileRegistryTest.TestGetDefaultProfile_HasCorrectCipherClass;
begin
	Assert.AreEqual(TClass(TDCP_rijndael), TClass(TCipherProfileRegistry.GetDefaultProfile.CipherClass));
end;

procedure TCipherProfileRegistryTest.TestGetDefaultProfile_HasCorrectHashClass;
begin
	Assert.AreEqual(TClass(TDCP_sha1), TClass(TCipherProfileRegistry.GetDefaultProfile.HashClass));
end;

{Profile ID uniqueness}

procedure TCipherProfileRegistryTest.TestAllProfileIds_AreUnique;
var
	Profiles: TArray<TCipherProfile>;
	I, J: Integer;
begin
	Profiles := TCipherProfileRegistry.GetProfiles;
	for I := 0 to High(Profiles) do
		for J := I + 1 to High(Profiles) do
			Assert.AreNotEqual(Profiles[I].Id, Profiles[J].Id,
				Format('Profiles %d and %d have duplicate ID: %s', [I, J, Profiles[I].Id]));
end;

{Class references are valid}

procedure TCipherProfileRegistryTest.TestAllProfiles_HaveValidCipherClasses;
var
	Profiles: TArray<TCipherProfile>;
	I: Integer;
	Cipher: TDCP_blockcipher128;
begin
	Profiles := TCipherProfileRegistry.GetProfiles;
	for I := 0 to High(Profiles) do
	begin
		Assert.IsNotNull(TObject(Profiles[I].CipherClass),
			Format('Profile %s has nil CipherClass', [Profiles[I].Id]));
		{Verify we can instantiate the cipher class}
		Cipher := Profiles[I].CipherClass.Create(nil);
		try
			Assert.IsTrue(Cipher.MaxKeySize >= Profiles[I].KeySizeBits,
				Format('Profile %s: MaxKeySize %d < declared %d', [Profiles[I].Id, Cipher.MaxKeySize, Profiles[I].KeySizeBits]));
		finally
			Cipher.Free;
		end;
	end;
end;

procedure TCipherProfileRegistryTest.TestAllProfiles_HaveValidHashClasses;
var
	Profiles: TArray<TCipherProfile>;
	I: Integer;
begin
	Profiles := TCipherProfileRegistry.GetProfiles;
	for I := 0 to High(Profiles) do
		Assert.IsNotNull(TObject(Profiles[I].HashClass),
			Format('Profile %s has nil HashClass', [Profiles[I].Id]));
end;

{Profile field completeness}

procedure TCipherProfileRegistryTest.TestAllProfiles_HaveNonEmptyDisplayName;
var
	Profiles: TArray<TCipherProfile>;
	I: Integer;
begin
	Profiles := TCipherProfileRegistry.GetProfiles;
	for I := 0 to High(Profiles) do
		Assert.IsNotEmpty(Profiles[I].DisplayName,
			Format('Profile %s has empty DisplayName', [Profiles[I].Id]));
end;

procedure TCipherProfileRegistryTest.TestAllProfiles_HaveNonEmptyBackendName;
var
	Profiles: TArray<TCipherProfile>;
	I: Integer;
begin
	Profiles := TCipherProfileRegistry.GetProfiles;
	for I := 0 to High(Profiles) do
		Assert.IsNotEmpty(Profiles[I].BackendName,
			Format('Profile %s has empty BackendName', [Profiles[I].Id]));
end;

procedure TCipherProfileRegistryTest.TestAllProfiles_Have256BitKeySize;
var
	Profiles: TArray<TCipherProfile>;
	I: Integer;
begin
	Profiles := TCipherProfileRegistry.GetProfiles;
	for I := 0 to High(Profiles) do
		Assert.AreEqual(256, Profiles[I].KeySizeBits,
			Format('Profile %s has unexpected key size', [Profiles[I].Id]));
end;

initialization

TDUnitX.RegisterTestFixture(TCipherProfileRegistryTest);

end.
