unit BCryptProviderTest;

{Tests for BCrypt (Windows CNG) provider.
	BCrypt should be available on all modern Windows systems.}

interface

uses
	BCryptProvider,
	System.SysUtils,
	DUnitX.TestFramework;

type
	[TestFixture]
	TBCryptProviderTest = class
	public
		{Availability}
		[Test]
		procedure TestProvider_IsAvailable_ReturnsTrue;

		{Key derivation}
		[Test]
		procedure TestDeriveKey_ProducesCorrectLength;
		[Test]
		procedure TestDeriveKey_Deterministic;
		[Test]
		procedure TestDeriveKey_DifferentPasswords_DifferentKeys;

		{AES ECB single-block}
		[Test]
		procedure TestEncryptBlock_ProducesOutput;
		[Test]
		procedure TestEncryptBlock_SameInput_SameOutput;

		{Key handle lifecycle}
		[Test]
		procedure TestCreateDestroyKeyHandle_NoError;

		{Null provider}
		[Test]
		procedure TestNullProvider_IsAvailable_ReturnsFalse;
		[Test]
		procedure TestNullProvider_DeriveKey_ReturnsEmpty;
		[Test]
		{Verifies CreateAESKeyHandle returns 0 (null handle)}
		procedure TestNullProvider_CreateAESKeyHandle_ReturnsZero;
		[Test]
		{Verifies EncryptBlock completes without error}
		procedure TestNullProvider_EncryptBlock_NoOp;
		[Test]
		{Verifies DestroyKeyHandle completes without error}
		procedure TestNullProvider_DestroyKeyHandle_NoOp;

		{Interface compliance}
		[Test]
		procedure TestProvider_ImplementsInterface;
	end;

implementation

{Availability}

procedure TBCryptProviderTest.TestProvider_IsAvailable_ReturnsTrue;
var
	Provider: IBCryptProvider;
begin
	Provider := TBCryptProvider.Create;
	Assert.IsTrue(Provider.IsAvailable, 'BCrypt should be available on Windows');
end;

{Key derivation}

procedure TBCryptProviderTest.TestDeriveKey_ProducesCorrectLength;
var
	Provider: IBCryptProvider;
	Key: TBytes;
begin
	Provider := TBCryptProvider.Create;
	if not Provider.IsAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	Key := Provider.DeriveKey('testpassword');
	Assert.AreEqual(Integer(BCRYPT_AES_KEY_SIZE), Integer(Length(Key)), 'Derived key should be 32 bytes');
end;

procedure TBCryptProviderTest.TestDeriveKey_Deterministic;
var
	Provider: IBCryptProvider;
	Key1, Key2: TBytes;
begin
	Provider := TBCryptProvider.Create;
	if not Provider.IsAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	Key1 := Provider.DeriveKey('mypassword');
	Key2 := Provider.DeriveKey('mypassword');
	Assert.AreEqual(Key1, Key2, 'Same password must produce same key');
end;

procedure TBCryptProviderTest.TestDeriveKey_DifferentPasswords_DifferentKeys;
var
	Provider: IBCryptProvider;
	Key1, Key2: TBytes;
begin
	Provider := TBCryptProvider.Create;
	if not Provider.IsAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	Key1 := Provider.DeriveKey('password1');
	Key2 := Provider.DeriveKey('password2');
	Assert.AreNotEqual(Key1, Key2, 'Different passwords must produce different keys');
end;

{AES ECB single-block}

procedure TBCryptProviderTest.TestEncryptBlock_ProducesOutput;
var
	Provider: IBCryptProvider;
	Key: TBytes;
	Handle: BCRYPT_KEY_HANDLE;
	InBlock, OutBlock: array[0..15] of Byte;
begin
	Provider := TBCryptProvider.Create;
	if not Provider.IsAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	Key := Provider.DeriveKey('testkey');
	Handle := Provider.CreateAESKeyHandle(Key);
	try
		FillChar(InBlock[0], 16, $42);
		FillChar(OutBlock[0], 16, 0);

		Provider.EncryptBlock(Handle, InBlock[0], OutBlock[0]);

		{Output should differ from input after encryption}
		Assert.IsFalse(CompareMem(@InBlock[0], @OutBlock[0], 16),
			'Encrypted block should differ from input');
	finally
		Provider.DestroyKeyHandle(Handle);
	end;
end;

procedure TBCryptProviderTest.TestEncryptBlock_SameInput_SameOutput;
var
	Provider: IBCryptProvider;
	Key: TBytes;
	Handle: BCRYPT_KEY_HANDLE;
	InBlock, OutBlock1, OutBlock2: array[0..15] of Byte;
begin
	Provider := TBCryptProvider.Create;
	if not Provider.IsAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	Key := Provider.DeriveKey('testkey');
	Handle := Provider.CreateAESKeyHandle(Key);
	try
		FillChar(InBlock[0], 16, $55);

		Provider.EncryptBlock(Handle, InBlock[0], OutBlock1[0]);
		Provider.EncryptBlock(Handle, InBlock[0], OutBlock2[0]);

		{ECB mode: same input always produces same output}
		Assert.IsTrue(CompareMem(@OutBlock1[0], @OutBlock2[0], 16),
			'ECB encryption of same input should produce same output');
	finally
		Provider.DestroyKeyHandle(Handle);
	end;
end;

{Key handle lifecycle}

procedure TBCryptProviderTest.TestCreateDestroyKeyHandle_NoError;
var
	Provider: IBCryptProvider;
	Key: TBytes;
	Handle: BCRYPT_KEY_HANDLE;
begin
	Provider := TBCryptProvider.Create;
	if not Provider.IsAvailable then
	begin
		Assert.Pass('Skipped - BCrypt not available');
		Exit;
	end;

	Key := Provider.DeriveKey('testkey');
	Handle := Provider.CreateAESKeyHandle(Key);
	Provider.DestroyKeyHandle(Handle);
	Assert.Pass('Key handle created and destroyed without error');
end;

{Null provider}

procedure TBCryptProviderTest.TestNullProvider_IsAvailable_ReturnsFalse;
var
	Provider: IBCryptProvider;
begin
	Provider := TNullBCryptProvider.Create;
	Assert.IsFalse(Provider.IsAvailable);
end;

procedure TBCryptProviderTest.TestNullProvider_DeriveKey_ReturnsEmpty;
var
	Provider: IBCryptProvider;
	Key: TBytes;
begin
	Provider := TNullBCryptProvider.Create;
	Key := Provider.DeriveKey('test');
	Assert.AreEqual(Integer(0), Integer(Length(Key)));
end;

procedure TBCryptProviderTest.TestNullProvider_CreateAESKeyHandle_ReturnsZero;
var
	Provider: IBCryptProvider;
	Key: TBytes;
	Handle: BCRYPT_KEY_HANDLE;
begin
	Provider := TNullBCryptProvider.Create;
	SetLength(Key, 32);
	Handle := Provider.CreateAESKeyHandle(Key);
	Assert.AreEqual(THandle(0), Handle);
end;

procedure TBCryptProviderTest.TestNullProvider_EncryptBlock_NoOp;
var
	Provider: IBCryptProvider;
	InBlock, OutBlock: array[0..15] of Byte;
begin
	Provider := TNullBCryptProvider.Create;
	FillChar(InBlock, SizeOf(InBlock), $AA);
	FillChar(OutBlock, SizeOf(OutBlock), 0);
	Provider.EncryptBlock(0, InBlock, OutBlock);
	Assert.Pass('EncryptBlock completed without error');
end;

procedure TBCryptProviderTest.TestNullProvider_DestroyKeyHandle_NoOp;
var
	Provider: IBCryptProvider;
begin
	Provider := TNullBCryptProvider.Create;
	Provider.DestroyKeyHandle(0);
	Assert.Pass('DestroyKeyHandle completed without error');
end;

{Interface compliance}

procedure TBCryptProviderTest.TestProvider_ImplementsInterface;
var
	Provider: IBCryptProvider;
begin
	Provider := TBCryptProvider.Create;
	Assert.IsNotNull(Provider, 'TBCryptProvider should implement IBCryptProvider');
end;

initialization

TDUnitX.RegisterTestFixture(TBCryptProviderTest);

end.
