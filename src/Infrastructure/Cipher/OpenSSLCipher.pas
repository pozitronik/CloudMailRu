unit OpenSSLCipher;

{OpenSSL AES-256 CFB-8 cipher backend via EVP API.
	Uses PBKDF2-SHA256 for key derivation (10000 iterations).
	Zero IV for deterministic encryption (matches DCPCrypt behavior).
	Requires OpenSSL 1.1.x+ with EVP_aes_256_cfb8 support.}

interface

uses
	Classes,
	System.SysUtils,
	BlockCipher,
	Cipher,
	OpenSSLProvider;

const
	OPENSSL_AES256_KEY_SIZE = 32; {256 bits}
	OPENSSL_AES256_IV_SIZE = 16; {128-bit block size}
	OPENSSL_PBKDF2_ITERATIONS = 10000;
	OPENSSL_PBKDF2_SALT: AnsiString = 'openssl-aes256-cfb8-pbkdf2'; {Deterministic salt derived from profile ID}

type
	{IBlockCipher implementation using OpenSSL EVP AES-256-CFB8.
		Holds two EVP_CIPHER_CTX pointers (encrypt + decrypt).
		Key derived via PBKDF2-SHA256 at construction time.}
	TOpenSSLBlockCipher = class(TInterfacedObject, IBlockCipher)
	private
		FFunctions: TOpenSSLFunctions;
		FEncCtx: Pointer;
		FDecCtx: Pointer;
		FKey: TBytes;
		FIV: TBytes;
		procedure InitContexts;
		procedure FreeContexts;
	public
		constructor Create(const Functions: TOpenSSLFunctions; const Key, IV: TBytes);
		destructor Destroy; override;
		procedure EncryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
		procedure DecryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
		procedure Reset;
		procedure Burn;
	end;

	{ICipher implementation using OpenSSL backend.
		Creates TOpenSSLBlockCipher instances via CreateBlockCipher.}
	TOpenSSLCipher = class(TBaseCipher)
	private
		FFunctions: TOpenSSLFunctions;
		FKey: TBytes;
		FIV: TBytes;
		function DeriveKey(const Password: WideString): TBytes;
	protected
		function CreateBlockCipher: IBlockCipher; override;
	public
		constructor Create(const Password: WideString; const Functions: TOpenSSLFunctions);
		destructor Destroy; override;
	end;

implementation

uses
	System.Math,
	CloudConstants;

{TOpenSSLBlockCipher}

constructor TOpenSSLBlockCipher.Create(const Functions: TOpenSSLFunctions; const Key, IV: TBytes);
begin
	inherited Create;
	FFunctions := Functions;

	{Keep copies for Reset}
	FKey := Copy(Key);
	FIV := Copy(IV);

	FEncCtx := nil;
	FDecCtx := nil;
	InitContexts;
end;

destructor TOpenSSLBlockCipher.Destroy;
begin
	Burn;
	inherited;
end;

procedure TOpenSSLBlockCipher.InitContexts;
begin
	FreeContexts;

	FEncCtx := FFunctions.EVP_CIPHER_CTX_new();
	FDecCtx := FFunctions.EVP_CIPHER_CTX_new();

	FFunctions.EVP_EncryptInit_ex(FEncCtx, FFunctions.EVP_aes_256_cfb8(), nil, @FKey[0], @FIV[0]);
	FFunctions.EVP_DecryptInit_ex(FDecCtx, FFunctions.EVP_aes_256_cfb8(), nil, @FKey[0], @FIV[0]);
end;

procedure TOpenSSLBlockCipher.FreeContexts;
begin
	if FEncCtx <> nil then
	begin
		FFunctions.EVP_CIPHER_CTX_free(FEncCtx);
		FEncCtx := nil;
	end;
	if FDecCtx <> nil then
	begin
		FFunctions.EVP_CIPHER_CTX_free(FDecCtx);
		FDecCtx := nil;
	end;
end;

procedure TOpenSSLBlockCipher.EncryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
var
	OutLen: Integer;
begin
	FFunctions.EVP_EncryptUpdate(FEncCtx, @Outdata, OutLen, @Indata, Size);
end;

procedure TOpenSSLBlockCipher.DecryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
var
	OutLen: Integer;
begin
	FFunctions.EVP_DecryptUpdate(FDecCtx, @Outdata, OutLen, @Indata, Size);
end;

procedure TOpenSSLBlockCipher.Reset;
begin
	{Re-initialize contexts with same key/IV to restore post-init state}
	InitContexts;
end;

procedure TOpenSSLBlockCipher.Burn;
begin
	FreeContexts;
	if Length(FKey) > 0 then
		FillChar(FKey[0], Length(FKey), 0);
	if Length(FIV) > 0 then
		FillChar(FIV[0], Length(FIV), 0);
	SetLength(FKey, 0);
	SetLength(FIV, 0);
end;

{TOpenSSLCipher}

constructor TOpenSSLCipher.Create(const Password: WideString; const Functions: TOpenSSLFunctions);
begin
	inherited Create;
	FFunctions := Functions;

	FKey := DeriveKey(Password);

	{Zero IV for deterministic encryption}
	SetLength(FIV, OPENSSL_AES256_IV_SIZE);
	FillChar(FIV[0], OPENSSL_AES256_IV_SIZE, 0);
end;

destructor TOpenSSLCipher.Destroy;
begin
	if Length(FKey) > 0 then
		FillChar(FKey[0], Length(FKey), 0);
	if Length(FIV) > 0 then
		FillChar(FIV[0], Length(FIV), 0);
	inherited;
end;

function TOpenSSLCipher.DeriveKey(const Password: WideString): TBytes;
var
	PassBytes: TBytes;
begin
	PassBytes := TEncoding.UTF8.GetBytes(Password);
	SetLength(Result, OPENSSL_AES256_KEY_SIZE);

	FFunctions.PKCS5_PBKDF2_HMAC(
		PAnsiChar(@PassBytes[0]), Length(PassBytes),
		PByte(@OPENSSL_PBKDF2_SALT[1]), Length(OPENSSL_PBKDF2_SALT),
		OPENSSL_PBKDF2_ITERATIONS,
		FFunctions.EVP_sha256(),
		OPENSSL_AES256_KEY_SIZE, @Result[0]
	);

	{Wipe password bytes}
	if Length(PassBytes) > 0 then
		FillChar(PassBytes[0], Length(PassBytes), 0);
end;

function TOpenSSLCipher.CreateBlockCipher: IBlockCipher;
begin
	Result := TOpenSSLBlockCipher.Create(FFunctions, FKey, FIV);
end;

end.
