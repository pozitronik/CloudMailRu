unit BCryptProvider;

{Thin wrapper for Windows CNG (BCrypt) API for AES encryption.
	Provides AES-256 ECB single-block encryption as a building block
	for manual CFB-8 mode implementation. PBKDF2-SHA256 key derivation
	via BCryptDeriveKeyPBKDF2.

	BCrypt is always available on Windows Vista+ (part of bcrypt.dll).}

interface

uses
	Windows,
	System.SysUtils;

const
	BCRYPT_AES_KEY_SIZE = 32; {256 bits}
	BCRYPT_AES_BLOCK_SIZE = 16; {128-bit block size}
	BCRYPT_PBKDF2_ITERATIONS = 10000;
	BCRYPT_PBKDF2_SALT: AnsiString = 'bcrypt-aes256-cfb8-pbkdf2'; {Deterministic salt derived from profile ID}

type
	{Opaque handle types for Windows CNG API}
	BCRYPT_ALG_HANDLE = THandle;
	BCRYPT_KEY_HANDLE = THandle;
	BCRYPT_HASH_HANDLE = THandle;

	{Interface for BCrypt operations -- allows DI and testability}
	IBCryptProvider = interface
		['{F7A1B2C3-D4E5-6789-ABCD-EF0123456789}']
		function IsAvailable: Boolean;
		function DeriveKey(const Password: WideString): TBytes;
		function CreateAESKeyHandle(const Key: TBytes): BCRYPT_KEY_HANDLE;
		procedure EncryptBlock(KeyHandle: BCRYPT_KEY_HANDLE; const InBlock; var OutBlock);
		procedure DestroyKeyHandle(KeyHandle: BCRYPT_KEY_HANDLE);
	end;

	{Production BCrypt provider -- loads bcrypt.dll and resolves functions}
	TBCryptProvider = class(TInterfacedObject, IBCryptProvider)
	private
		FHandle: THandle;
		FAlgHandle: BCRYPT_ALG_HANDLE;
		FHmacAlgHandle: BCRYPT_ALG_HANDLE;
		FLoaded: Boolean;
		FLoadAttempted: Boolean;

		{BCrypt function pointers}
		FBCryptOpenAlgorithmProvider: function(out phAlgorithm: BCRYPT_ALG_HANDLE; pszAlgId: PWideChar; pszImplementation: PWideChar; dwFlags: ULONG): LONG; stdcall;
		FBCryptCloseAlgorithmProvider: function(hAlgorithm: BCRYPT_ALG_HANDLE; dwFlags: ULONG): LONG; stdcall;
		FBCryptSetProperty: function(hObject: THandle; pszProperty: PWideChar; pbInput: PByte; cbInput: ULONG; dwFlags: ULONG): LONG; stdcall;
		FBCryptGenerateSymmetricKey: function(hAlgorithm: BCRYPT_ALG_HANDLE; out phKey: BCRYPT_KEY_HANDLE; pbKeyObject: PByte; cbKeyObject: ULONG; pbSecret: PByte; cbSecret: ULONG; dwFlags: ULONG): LONG; stdcall;
		FBCryptDestroyKey: function(hKey: BCRYPT_KEY_HANDLE): LONG; stdcall;
		FBCryptEncrypt: function(hKey: BCRYPT_KEY_HANDLE; pbInput: PByte; cbInput: ULONG; pPaddingInfo: Pointer; pbIV: PByte; cbIV: ULONG; pbOutput: PByte; cbOutput: ULONG; out pcbResult: ULONG; dwFlags: ULONG): LONG; stdcall;
		FBCryptDeriveKeyPBKDF2: function(hPrf: BCRYPT_ALG_HANDLE; pbPassword: PByte; cbPassword: ULONG; pbSalt: PByte; cbSalt: ULONG; cIterations: UInt64; pbDerivedKey: PByte; cbDerivedKey: ULONG; dwFlags: ULONG): LONG; stdcall;

		procedure TryLoad;
	public
		constructor Create;
		destructor Destroy; override;
		function IsAvailable: Boolean;
		function DeriveKey(const Password: WideString): TBytes;
		function CreateAESKeyHandle(const Key: TBytes): BCRYPT_KEY_HANDLE;
		procedure EncryptBlock(KeyHandle: BCRYPT_KEY_HANDLE; const InBlock; var OutBlock);
		procedure DestroyKeyHandle(KeyHandle: BCRYPT_KEY_HANDLE);
	end;

	{Null implementation for testing}
	TNullBCryptProvider = class(TInterfacedObject, IBCryptProvider)
	public
		function IsAvailable: Boolean;
		function DeriveKey(const Password: WideString): TBytes;
		function CreateAESKeyHandle(const Key: TBytes): BCRYPT_KEY_HANDLE;
		procedure EncryptBlock(KeyHandle: BCRYPT_KEY_HANDLE; const InBlock; var OutBlock);
		procedure DestroyKeyHandle(KeyHandle: BCRYPT_KEY_HANDLE);
	end;

implementation

const
	BCRYPT_DLL = 'bcrypt.dll';
	BCRYPT_AES_ALGORITHM: PWideChar = 'AES';
	BCRYPT_SHA256_HMAC_ALGORITHM: PWideChar = 'SHA256';
	BCRYPT_CHAINING_MODE: PWideChar = 'ChainingMode';
	BCRYPT_CHAIN_MODE_ECB: PWideChar = 'ChainingModeECB';
	BCRYPT_ALG_HANDLE_HMAC_FLAG = $00000008;

{TBCryptProvider}

constructor TBCryptProvider.Create;
begin
	inherited Create;
	FHandle := 0;
	FAlgHandle := 0;
	FHmacAlgHandle := 0;
	FLoaded := False;
	FLoadAttempted := False;
end;

destructor TBCryptProvider.Destroy;
begin
	if (FAlgHandle <> 0) and Assigned(FBCryptCloseAlgorithmProvider) then
		FBCryptCloseAlgorithmProvider(FAlgHandle, 0);
	if (FHmacAlgHandle <> 0) and Assigned(FBCryptCloseAlgorithmProvider) then
		FBCryptCloseAlgorithmProvider(FHmacAlgHandle, 0);
	inherited;
end;

procedure TBCryptProvider.TryLoad;
var
	Status: LONG;
begin
	if FLoadAttempted then
		Exit;
	FLoadAttempted := True;

	FHandle := LoadLibrary(BCRYPT_DLL);
	if FHandle = 0 then
		Exit;

	@FBCryptOpenAlgorithmProvider := GetProcAddress(FHandle, 'BCryptOpenAlgorithmProvider');
	@FBCryptCloseAlgorithmProvider := GetProcAddress(FHandle, 'BCryptCloseAlgorithmProvider');
	@FBCryptSetProperty := GetProcAddress(FHandle, 'BCryptSetProperty');
	@FBCryptGenerateSymmetricKey := GetProcAddress(FHandle, 'BCryptGenerateSymmetricKey');
	@FBCryptDestroyKey := GetProcAddress(FHandle, 'BCryptDestroyKey');
	@FBCryptEncrypt := GetProcAddress(FHandle, 'BCryptEncrypt');
	@FBCryptDeriveKeyPBKDF2 := GetProcAddress(FHandle, 'BCryptDeriveKeyPBKDF2');

	if not (Assigned(FBCryptOpenAlgorithmProvider) and
		Assigned(FBCryptCloseAlgorithmProvider) and
		Assigned(FBCryptSetProperty) and
		Assigned(FBCryptGenerateSymmetricKey) and
		Assigned(FBCryptDestroyKey) and
		Assigned(FBCryptEncrypt) and
		Assigned(FBCryptDeriveKeyPBKDF2)) then
		Exit;

	{Open AES algorithm provider in ECB mode}
	Status := FBCryptOpenAlgorithmProvider(FAlgHandle, BCRYPT_AES_ALGORITHM, nil, 0);
	if Status <> 0 then
		Exit;

	Status := FBCryptSetProperty(FAlgHandle, BCRYPT_CHAINING_MODE,
		PByte(BCRYPT_CHAIN_MODE_ECB), (Length(BCRYPT_CHAIN_MODE_ECB) + 1) * SizeOf(WideChar), 0);
	if Status <> 0 then
	begin
		FBCryptCloseAlgorithmProvider(FAlgHandle, 0);
		FAlgHandle := 0;
		Exit;
	end;

	{Open SHA-256 HMAC algorithm provider for PBKDF2}
	Status := FBCryptOpenAlgorithmProvider(FHmacAlgHandle, BCRYPT_SHA256_HMAC_ALGORITHM, nil, BCRYPT_ALG_HANDLE_HMAC_FLAG);
	if Status <> 0 then
	begin
		FBCryptCloseAlgorithmProvider(FAlgHandle, 0);
		FAlgHandle := 0;
		Exit;
	end;

	FLoaded := True;
end;

function TBCryptProvider.IsAvailable: Boolean;
begin
	TryLoad;
	Result := FLoaded;
end;

function TBCryptProvider.DeriveKey(const Password: WideString): TBytes;
var
	PassBytes: TBytes;
begin
	TryLoad;
	PassBytes := TEncoding.UTF8.GetBytes(Password);
	SetLength(Result, BCRYPT_AES_KEY_SIZE);

	FBCryptDeriveKeyPBKDF2(FHmacAlgHandle,
		PByte(PassBytes), Length(PassBytes),
		PByte(@BCRYPT_PBKDF2_SALT[1]), Length(BCRYPT_PBKDF2_SALT),
		BCRYPT_PBKDF2_ITERATIONS,
		@Result[0], BCRYPT_AES_KEY_SIZE, 0);

	{Wipe password bytes}
	if Length(PassBytes) > 0 then
		FillChar(PassBytes[0], Length(PassBytes), 0);
end;

function TBCryptProvider.CreateAESKeyHandle(const Key: TBytes): BCRYPT_KEY_HANDLE;
begin
	TryLoad;
	Result := 0;
	FBCryptGenerateSymmetricKey(FAlgHandle, Result, nil, 0,
		PByte(@Key[0]), Length(Key), 0);
end;

procedure TBCryptProvider.EncryptBlock(KeyHandle: BCRYPT_KEY_HANDLE; const InBlock; var OutBlock);
var
	BytesWritten: ULONG;
begin
	{ECB single-block encryption: 16 bytes in, 16 bytes out, no padding}
	FBCryptEncrypt(KeyHandle, @InBlock, BCRYPT_AES_BLOCK_SIZE,
		nil, nil, 0, @OutBlock, BCRYPT_AES_BLOCK_SIZE, BytesWritten, 0);
end;

procedure TBCryptProvider.DestroyKeyHandle(KeyHandle: BCRYPT_KEY_HANDLE);
begin
	if KeyHandle <> 0 then
		FBCryptDestroyKey(KeyHandle);
end;

{TNullBCryptProvider}

function TNullBCryptProvider.IsAvailable: Boolean;
begin
	Result := False;
end;

function TNullBCryptProvider.DeriveKey(const Password: WideString): TBytes;
begin
	SetLength(Result, 0);
end;

function TNullBCryptProvider.CreateAESKeyHandle(const Key: TBytes): BCRYPT_KEY_HANDLE;
begin
	Result := 0;
end;

procedure TNullBCryptProvider.EncryptBlock(KeyHandle: BCRYPT_KEY_HANDLE; const InBlock; var OutBlock);
begin
	{No-op}
end;

procedure TNullBCryptProvider.DestroyKeyHandle(KeyHandle: BCRYPT_KEY_HANDLE);
begin
	{No-op}
end;

end.
