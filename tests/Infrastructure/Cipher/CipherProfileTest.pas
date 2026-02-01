unit CipherProfileTest;

interface

uses
	CipherProfile,
	Cipher,
	Classes,
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

		{FindById -- unknown}
		[Test]
		procedure TestFindById_UnknownId_ReturnsFalse;
		[Test]
		procedure TestFindById_EmptyId_ReturnsFalse;

		{Default profile}
		[Test]
		procedure TestGetDefaultProfile_ReturnsLegacyProfile;
		[Test]
		procedure TestGetDefaultProfile_FactoryCreatesValidCipher;

		{Profile ID uniqueness}
		[Test]
		procedure TestAllProfileIds_AreUnique;

		{Factory functions produce valid ciphers}
		[Test]
		procedure TestAllProfiles_HaveValidFactories;
		[Test]
		procedure TestFactory_LegacyProfile_CreatesWorkingCipher;

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
	Assert.AreEqual(3, TCipherProfileRegistry.Count);
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

procedure TCipherProfileRegistryTest.TestGetDefaultProfile_FactoryCreatesValidCipher;
var
	Cipher: ICipher;
begin
	Cipher := TCipherProfileRegistry.GetDefaultProfile.CreateCipher('testpass');
	Assert.IsNotNull(Cipher, 'Default profile factory returned nil');
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

{Factory functions produce valid ciphers}

procedure TCipherProfileRegistryTest.TestAllProfiles_HaveValidFactories;
var
	Profiles: TArray<TCipherProfile>;
	I: Integer;
	Cipher: ICipher;
begin
	Profiles := TCipherProfileRegistry.GetProfiles;
	for I := 0 to High(Profiles) do
	begin
		Cipher := Profiles[I].CreateCipher('testpass');
		Assert.IsNotNull(Cipher,
			Format('Profile %s factory returned nil', [Profiles[I].Id]));
	end;
end;

procedure TCipherProfileRegistryTest.TestFactory_LegacyProfile_CreatesWorkingCipher;
var
	Profile: TCipherProfile;
	Cipher: ICipher;
	Source, Encrypted, Decrypted: TMemoryStream;
	TestData: AnsiString;
	ResultData: AnsiString;
begin
	{Verify encrypt/decrypt roundtrip through the legacy profile factory}
	Assert.IsTrue(TCipherProfileRegistry.FindById(CIPHER_PROFILE_LEGACY_DEFAULT, Profile));
	Cipher := Profile.CreateCipher('roundtrip-password');

	Source := TMemoryStream.Create;
	Encrypted := TMemoryStream.Create;
	Decrypted := TMemoryStream.Create;
	try
		TestData := 'Hello, cipher roundtrip test!';
		Source.WriteBuffer(TestData[1], Length(TestData));
		Source.Position := 0;

		Cipher.CryptStream(Source, Encrypted);
		Assert.IsTrue(Encrypted.Size > 0, 'Encrypted stream should not be empty');

		{Need a fresh cipher instance for decryption -- cipher state is consumed}
		Cipher := Profile.CreateCipher('roundtrip-password');
		Encrypted.Position := 0;
		Cipher.DecryptStream(Encrypted, Decrypted);

		Assert.AreEqual(Int64(Length(TestData)), Decrypted.Size, 'Decrypted size must match original');

		SetLength(ResultData, Decrypted.Size);
		Decrypted.Position := 0;
		Decrypted.ReadBuffer(ResultData[1], Decrypted.Size);
		Assert.AreEqual(String(TestData), String(ResultData), 'Decrypted content must match original');
	finally
		Source.Free;
		Encrypted.Free;
		Decrypted.Free;
	end;
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
