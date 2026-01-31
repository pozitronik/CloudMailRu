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
	FileCipher,
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
		Creates TOpenSSLBlockCipher instances for streaming operations.
		Bulk encrypt/decrypt uses CFB8 chunked loop via IBlockCipher.
		Password validation delegates to TFileCipher.CheckPasswordGUID (hardcoded AES/SHA-1).}
	TOpenSSLCipher = class(TInterfacedObject, ICipher)
	private
		FPassword: WideString;
		FFunctions: TOpenSSLFunctions;
		FKey: TBytes;
		FIV: TBytes;
		FPasswordIsWrong: Boolean;
		function DeriveKey(const Password: WideString): TBytes;
	public
		constructor Create(const Password: WideString; const Functions: TOpenSSLFunctions; const PasswordControl: WideString = '');
		destructor Destroy; override;

		function CryptFile(SourceFileName, DestinationFilename: WideString): Integer;
		function CryptStream(SourceStream, DestinationStream: TStream): Integer;
		function DecryptFile(SourceFileName, DestinationFilename: WideString): Integer;
		function DecryptStream(SourceStream, DestinationStream: TStream): Integer;
		function GetEncryptingStream(Source: TStream): TStream;
		function GetDecryptingStream(Source: TStream): TStream;

		property IsWrongPassword: Boolean read FPasswordIsWrong;
	end;

implementation

uses
	System.Math,
	CloudConstants,
	CipherStreams;

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

constructor TOpenSSLCipher.Create(const Password: WideString; const Functions: TOpenSSLFunctions; const PasswordControl: WideString = '');
begin
	inherited Create;
	FPassword := Password;
	FFunctions := Functions;

	FKey := DeriveKey(Password);

	{Zero IV for deterministic encryption}
	SetLength(FIV, OPENSSL_AES256_IV_SIZE);
	FillChar(FIV[0], OPENSSL_AES256_IV_SIZE, 0);

	{Password validation always uses legacy AES-256/SHA-1 regardless of backend,
		because CryptedGUID was generated with that combination}
	if PasswordControl <> EmptyWideStr then
		FPasswordIsWrong := not TFileCipher.CheckPasswordGUID(Password, PasswordControl);
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

function TOpenSSLCipher.CryptFile(SourceFileName, DestinationFilename: WideString): Integer;
var
	SourceStream, DestinationStream: TBufferedFileStream;
begin
	Result := CIPHER_OK;
	SourceStream := nil;
	DestinationStream := nil;
	try
		try
			SourceStream := TBufferedFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyWrite);
			DestinationStream := TBufferedFileStream.Create(DestinationFilename, fmCreate);
			if SourceStream.Size > 0 then
				self.CryptStream(SourceStream, DestinationStream);
		except
			Result := CIPHER_IO_ERROR;
		end;
	finally
		SourceStream.Free;
		DestinationStream.Free;
	end;
end;

function TOpenSSLCipher.CryptStream(SourceStream, DestinationStream: TStream): Integer;
var
	BlockCipher: IBlockCipher;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	Result := 0;
	if SourceStream.Size <= 0 then
		Exit;

	SourceStream.Position := 0;
	BlockCipher := TOpenSSLBlockCipher.Create(FFunctions, FKey, FIV);
	try
		SetLength(Buffer, CIPHER_BUFFER_SIZE);
		repeat
			BytesRead := SourceStream.Read(Buffer[0], CIPHER_BUFFER_SIZE);
			if BytesRead > 0 then
			begin
				BlockCipher.EncryptCFB8bit(Buffer[0], Buffer[0], BytesRead);
				DestinationStream.WriteBuffer(Buffer[0], BytesRead);
				Inc(Result, BytesRead);
			end;
		until BytesRead = 0;
	finally
		BlockCipher.Burn;
	end;
end;

function TOpenSSLCipher.DecryptFile(SourceFileName, DestinationFilename: WideString): Integer;
var
	SourceStream, DestinationStream: TBufferedFileStream;
begin
	Result := CIPHER_OK;
	SourceStream := nil;
	DestinationStream := nil;
	try
		try
			SourceStream := TBufferedFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyWrite);
			DestinationStream := TBufferedFileStream.Create(DestinationFilename, fmCreate);
			if SourceStream.Size > 0 then
				self.DecryptStream(SourceStream, DestinationStream);
		except
			Result := CIPHER_IO_ERROR;
		end;
	finally
		SourceStream.Free;
		DestinationStream.Free;
	end;
end;

function TOpenSSLCipher.DecryptStream(SourceStream, DestinationStream: TStream): Integer;
var
	BlockCipher: IBlockCipher;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	Result := 0;
	if SourceStream.Size <= 0 then
		Exit;

	SourceStream.Position := 0;
	BlockCipher := TOpenSSLBlockCipher.Create(FFunctions, FKey, FIV);
	try
		SetLength(Buffer, CIPHER_BUFFER_SIZE);
		repeat
			BytesRead := SourceStream.Read(Buffer[0], CIPHER_BUFFER_SIZE);
			if BytesRead > 0 then
			begin
				BlockCipher.DecryptCFB8bit(Buffer[0], Buffer[0], BytesRead);
				DestinationStream.WriteBuffer(Buffer[0], BytesRead);
				Inc(Result, BytesRead);
			end;
		until BytesRead = 0;
	finally
		BlockCipher.Burn;
	end;
end;

function TOpenSSLCipher.GetEncryptingStream(Source: TStream): TStream;
begin
	Result := TEncryptingStream.Create(Source, TOpenSSLBlockCipher.Create(FFunctions, FKey, FIV));
end;

function TOpenSSLCipher.GetDecryptingStream(Source: TStream): TStream;
begin
	Result := TDecryptingStream.Create(Source, TOpenSSLBlockCipher.Create(FFunctions, FKey, FIV));
end;

end.
