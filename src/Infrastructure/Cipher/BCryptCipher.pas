unit BCryptCipher;

{Windows CNG (BCrypt) AES-256 CFB-8 cipher backend.
	Manual CFB-8 implementation on top of AES ECB single-block encryption.
	Uses PBKDF2-SHA256 for key derivation (10000 iterations).
	Zero IV for deterministic encryption (matches DCPCrypt behavior).
	Available on Windows Vista+ without external DLL dependencies.}

interface

uses
	Classes,
	System.SysUtils,
	BlockCipher,
	FileCipher,
	BCryptProvider;

type
	{IBlockCipher implementation using manual CFB-8 on top of BCrypt AES ECB.
		ShiftRegister holds the 16-byte feedback state.
		For each byte: ECB-encrypt ShiftRegister, XOR plaintext with EncryptedBlock[0],
		then shift register left by 1 and append ciphertext byte.}
	TBCryptBlockCipher = class(TInterfacedObject, IBlockCipher)
	private
		FProvider: IBCryptProvider;
		FKeyHandle: BCRYPT_KEY_HANDLE;
		FShiftRegister: array[0..BCRYPT_AES_BLOCK_SIZE - 1] of Byte;
		FInitialIV: TBytes;
		FKey: TBytes;
	public
		constructor Create(const Provider: IBCryptProvider; const Key, IV: TBytes);
		destructor Destroy; override;
		procedure EncryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
		procedure DecryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
		procedure Reset;
		procedure Burn;
	end;

	{ICipher implementation using BCrypt backend.
		Creates TBCryptBlockCipher instances via CreateBlockCipher.
		Password validation delegates to TFileCipher.CheckPasswordGUID (hardcoded AES/SHA-1).}
	TBCryptCipher = class(TBaseCipher)
	private
		FProvider: IBCryptProvider;
		FKey: TBytes;
		FIV: TBytes;
	protected
		function CreateBlockCipher: IBlockCipher; override;
	public
		constructor Create(const Password: WideString; const Provider: IBCryptProvider; const PasswordControl: WideString = '');
		destructor Destroy; override;
	end;

implementation

uses
	System.Math,
	CloudConstants;

{TBCryptBlockCipher}

constructor TBCryptBlockCipher.Create(const Provider: IBCryptProvider; const Key, IV: TBytes);
begin
	inherited Create;
	FProvider := Provider;

	{Keep copies for Reset}
	FKey := Copy(Key);
	FInitialIV := Copy(IV);

	{Initialize shift register from IV}
	Move(FInitialIV[0], FShiftRegister[0], BCRYPT_AES_BLOCK_SIZE);

	FKeyHandle := FProvider.CreateAESKeyHandle(FKey);
end;

destructor TBCryptBlockCipher.Destroy;
begin
	Burn;
	inherited;
end;

procedure TBCryptBlockCipher.EncryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
var
	I: Cardinal;
	EncryptedBlock: array[0..BCRYPT_AES_BLOCK_SIZE - 1] of Byte;
	InPtr: PByte;
	OutPtr: PByte;
	CipherByte: Byte;
begin
	InPtr := @Indata;
	OutPtr := @Outdata;

	for I := 0 to Size - 1 do
	begin
		{1. ECB-encrypt the shift register}
		FProvider.EncryptBlock(FKeyHandle, FShiftRegister[0], EncryptedBlock[0]);

		{2. XOR plaintext byte with first byte of encrypted block}
		CipherByte := InPtr^ xor EncryptedBlock[0];
		OutPtr^ := CipherByte;

		{3. Shift register left by 1 byte and append ciphertext byte}
		Move(FShiftRegister[1], FShiftRegister[0], BCRYPT_AES_BLOCK_SIZE - 1);
		FShiftRegister[BCRYPT_AES_BLOCK_SIZE - 1] := CipherByte;

		Inc(InPtr);
		Inc(OutPtr);
	end;
end;

procedure TBCryptBlockCipher.DecryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
var
	I: Cardinal;
	EncryptedBlock: array[0..BCRYPT_AES_BLOCK_SIZE - 1] of Byte;
	InPtr: PByte;
	OutPtr: PByte;
	CipherByte: Byte;
begin
	InPtr := @Indata;
	OutPtr := @Outdata;

	for I := 0 to Size - 1 do
	begin
		{1. ECB-encrypt the shift register}
		FProvider.EncryptBlock(FKeyHandle, FShiftRegister[0], EncryptedBlock[0]);

		{2. Save ciphertext byte before XOR (needed for shift register update)}
		CipherByte := InPtr^;

		{3. XOR ciphertext byte with first byte of encrypted block to get plaintext}
		OutPtr^ := CipherByte xor EncryptedBlock[0];

		{4. Shift register left by 1 byte and append ciphertext byte}
		Move(FShiftRegister[1], FShiftRegister[0], BCRYPT_AES_BLOCK_SIZE - 1);
		FShiftRegister[BCRYPT_AES_BLOCK_SIZE - 1] := CipherByte;

		Inc(InPtr);
		Inc(OutPtr);
	end;
end;

procedure TBCryptBlockCipher.Reset;
begin
	{Restore shift register to initial IV state}
	Move(FInitialIV[0], FShiftRegister[0], BCRYPT_AES_BLOCK_SIZE);

	{Re-create key handle for clean state}
	FProvider.DestroyKeyHandle(FKeyHandle);
	FKeyHandle := FProvider.CreateAESKeyHandle(FKey);
end;

procedure TBCryptBlockCipher.Burn;
begin
	FProvider.DestroyKeyHandle(FKeyHandle);
	FKeyHandle := 0;
	FillChar(FShiftRegister[0], BCRYPT_AES_BLOCK_SIZE, 0);
	if Length(FKey) > 0 then
		FillChar(FKey[0], Length(FKey), 0);
	if Length(FInitialIV) > 0 then
		FillChar(FInitialIV[0], Length(FInitialIV), 0);
	SetLength(FKey, 0);
	SetLength(FInitialIV, 0);
end;

{TBCryptCipher}

constructor TBCryptCipher.Create(const Password: WideString; const Provider: IBCryptProvider; const PasswordControl: WideString = '');
begin
	inherited Create;
	FProvider := Provider;

	FKey := FProvider.DeriveKey(Password);

	{Zero IV for deterministic encryption}
	SetLength(FIV, BCRYPT_AES_BLOCK_SIZE);
	FillChar(FIV[0], BCRYPT_AES_BLOCK_SIZE, 0);

	{Password validation always uses legacy AES-256/SHA-1 regardless of backend}
	if PasswordControl <> EmptyWideStr then
		FPasswordIsWrong := not TFileCipher.CheckPasswordGUID(Password, PasswordControl);
end;

destructor TBCryptCipher.Destroy;
begin
	if Length(FKey) > 0 then
		FillChar(FKey[0], Length(FKey), 0);
	if Length(FIV) > 0 then
		FillChar(FIV[0], Length(FIV), 0);
	inherited;
end;

function TBCryptCipher.CreateBlockCipher: IBlockCipher;
begin
	Result := TBCryptBlockCipher.Create(FProvider, FKey, FIV);
end;

end.
