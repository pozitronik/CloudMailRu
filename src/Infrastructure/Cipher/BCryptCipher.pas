unit BCryptCipher;

{Windows CNG (BCrypt) AES-256 CFB-8 cipher backend.
	Manual CFB-8 implementation on top of AES ECB single-block encryption.
	Uses PBKDF2-SHA256 for key derivation (10000 iterations).
	Zero IV for deterministic encryption (matches DCPCrypt behavior).
	Available on Windows Vista+ without external DLL dependencies.}

interface

uses
	Classes,
	CloudDirItemList,
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
		Creates TBCryptBlockCipher instances for streaming operations.
		Bulk encrypt/decrypt uses CFB8 chunked loop via IBlockCipher.
		Password validation delegates to TFileCipher.CheckPasswordGUID (hardcoded AES/SHA-1).}
	TBCryptCipher = class(TInterfacedObject, ICipher)
	private
		FPassword: WideString;
		FProvider: IBCryptProvider;
		FKey: TBytes;
		FIV: TBytes;
		FDoFilenameCipher: Boolean;
		FPasswordIsWrong: Boolean;
		function Base64ToSafe(const Base64: WideString): WideString;
		function Base64FromSafe(const Safe: WideString): WideString;
	public
		constructor Create(const Password: WideString; const Provider: IBCryptProvider; const PasswordControl: WideString = ''; DoFilenameCipher: Boolean = false);
		destructor Destroy; override;

		function CryptFile(SourceFileName, DestinationFilename: WideString): Integer;
		function CryptStream(SourceStream, DestinationStream: TStream): Integer;
		function CryptFileName(const FileName: WideString): WideString;
		function DecryptFile(SourceFileName, DestinationFilename: WideString): Integer;
		function DecryptStream(SourceStream, DestinationStream: TStream): Integer;
		function DecryptFileName(const FileName: WideString): WideString;
		procedure DecryptDirListing(var CloudMailRuDirListing: TCloudDirItemList);
		function GetEncryptingStream(Source: TStream): TStream;
		function GetDecryptingStream(Source: TStream): TStream;

		property IsWrongPassword: Boolean read FPasswordIsWrong;
	end;

implementation

uses
	System.Math,
	CloudConstants,
	CipherStreams,
	DCPbase64;

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

constructor TBCryptCipher.Create(const Password: WideString; const Provider: IBCryptProvider; const PasswordControl: WideString = ''; DoFilenameCipher: Boolean = false);
begin
	inherited Create;
	FPassword := Password;
	FProvider := Provider;
	FDoFilenameCipher := DoFilenameCipher;

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

function TBCryptCipher.Base64ToSafe(const Base64: WideString): WideString;
begin
	Result := Base64;
	Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
	Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
end;

function TBCryptCipher.Base64FromSafe(const Safe: WideString): WideString;
begin
	Result := Safe;
	Result := StringReplace(Result, '-', '+', [rfReplaceAll]);
	Result := StringReplace(Result, '_', '/', [rfReplaceAll]);
end;

function TBCryptCipher.CryptFile(SourceFileName, DestinationFilename: WideString): Integer;
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

function TBCryptCipher.CryptStream(SourceStream, DestinationStream: TStream): Integer;
var
	Cipher: IBlockCipher;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	Result := 0;
	if SourceStream.Size <= 0 then
		Exit;

	SourceStream.Position := 0;
	Cipher := TBCryptBlockCipher.Create(FProvider, FKey, FIV);
	try
		SetLength(Buffer, CIPHER_BUFFER_SIZE);
		repeat
			BytesRead := SourceStream.Read(Buffer[0], CIPHER_BUFFER_SIZE);
			if BytesRead > 0 then
			begin
				Cipher.EncryptCFB8bit(Buffer[0], Buffer[0], BytesRead);
				DestinationStream.WriteBuffer(Buffer[0], BytesRead);
				Inc(Result, BytesRead);
			end;
		until BytesRead = 0;
	finally
		Cipher.Burn;
	end;
end;

function TBCryptCipher.CryptFileName(const FileName: WideString): WideString;
var
	Cipher: IBlockCipher;
	NameBytes: TBytes;
	EncryptedStr: AnsiString;
begin
	Result := ExtractFileName(FileName);
	if Result = EmptyWideStr then
		Exit;

	if FDoFilenameCipher then
	begin
		Cipher := TBCryptBlockCipher.Create(FProvider, FKey, FIV);
		try
			NameBytes := TEncoding.UTF8.GetBytes(Result);
			Cipher.EncryptCFB8bit(NameBytes[0], NameBytes[0], Length(NameBytes));
			SetLength(EncryptedStr, Length(NameBytes));
			Move(NameBytes[0], EncryptedStr[1], Length(NameBytes));
			Result := Base64ToSafe(Base64EncodeStr(EncryptedStr));
		finally
			Cipher.Burn;
		end;
	end;
end;

function TBCryptCipher.DecryptFile(SourceFileName, DestinationFilename: WideString): Integer;
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

function TBCryptCipher.DecryptStream(SourceStream, DestinationStream: TStream): Integer;
var
	Cipher: IBlockCipher;
	Buffer: TBytes;
	BytesRead: Integer;
begin
	Result := 0;
	if SourceStream.Size <= 0 then
		Exit;

	SourceStream.Position := 0;
	Cipher := TBCryptBlockCipher.Create(FProvider, FKey, FIV);
	try
		SetLength(Buffer, CIPHER_BUFFER_SIZE);
		repeat
			BytesRead := SourceStream.Read(Buffer[0], CIPHER_BUFFER_SIZE);
			if BytesRead > 0 then
			begin
				Cipher.DecryptCFB8bit(Buffer[0], Buffer[0], BytesRead);
				DestinationStream.WriteBuffer(Buffer[0], BytesRead);
				Inc(Result, BytesRead);
			end;
		until BytesRead = 0;
	finally
		Cipher.Burn;
	end;
end;

function TBCryptCipher.DecryptFileName(const FileName: WideString): WideString;
var
	Cipher: IBlockCipher;
	DecodedStr: AnsiString;
	NameBytes: TBytes;
begin
	Result := ExtractFileName(FileName);
	if Result = EmptyWideStr then
		Exit;

	if FDoFilenameCipher then
	begin
		Cipher := TBCryptBlockCipher.Create(FProvider, FKey, FIV);
		try
			DecodedStr := Base64DecodeStr(AnsiString(Base64FromSafe(FileName)));
			SetLength(NameBytes, Length(DecodedStr));
			Move(DecodedStr[1], NameBytes[0], Length(DecodedStr));
			Cipher.DecryptCFB8bit(NameBytes[0], NameBytes[0], Length(NameBytes));
			Result := TEncoding.UTF8.GetString(NameBytes);
		finally
			Cipher.Burn;
		end;
	end;
end;

procedure TBCryptCipher.DecryptDirListing(var CloudMailRuDirListing: TCloudDirItemList);
var
	i: Integer;
begin
	for i := 0 to Length(CloudMailRuDirListing) - 1 do
		CloudMailRuDirListing[i].visible_name := self.DecryptFileName(CloudMailRuDirListing[i].name);
end;

function TBCryptCipher.GetEncryptingStream(Source: TStream): TStream;
begin
	Result := TEncryptingStream.Create(Source, TBCryptBlockCipher.Create(FProvider, FKey, FIV));
end;

function TBCryptCipher.GetDecryptingStream(Source: TStream): TStream;
begin
	Result := TDecryptingStream.Create(Source, TBCryptBlockCipher.Create(FProvider, FKey, FIV));
end;

end.
