unit FileCipher;

{Combined unit for encryption operations: interface, null implementation, and AES/Rijndael implementation}

interface

uses
	Classes,
	CloudDirItemList,
	System.SysUtils,
	CloudConstants,
	DCPcrypt2,
	DCPblockciphers,
	DCPrijndael,
	DCPSha1,
	DCPbase64;

const
	{Cipher operation result codes}
	CIPHER_OK = 0;
	CIPHER_IO_ERROR = 1;
	CIPHER_WRONG_PASSWORD = 2;

	{Control GUID for password validation}
	CIPHER_CONTROL_GUID = '2b580ce6-e72f-433d-9788-3ecb6b0d9580';

type
	ICipher = interface
		['{54C0EBB7-5186-4D89-A2D6-050D4A6CD58B}']
		function CryptFile(SourceFileName, DestinationFilename: WideString): Integer;
		function CryptStream(SourceStream, DestinationStream: TStream): Integer;
		function CryptFileName(const FileName: WideString): WideString;
		function DecryptFile(SourceFileName, DestinationFilename: WideString): Integer;
		function DecryptStream(SourceStream, DestinationStream: TStream): Integer;
		function DecryptFileName(const FileName: WideString): WideString;
		procedure DecryptDirListing(var CloudMailRuDirListing: TCloudDirItemList);

		{Stream wrapper methods - return streams that transform data on-the-fly.
			Caller must free the returned stream. Wrapper does not own/free source.}
		function GetEncryptingStream(Source: TStream): TStream;
		function GetDecryptingStream(Source: TStream): TStream;
	end;

	{Lightweight read-only stream wrapper that delegates to source without copying.
		Used by TNullCipher for zero-overhead pass-through.}
	TPassThroughStream = class(TStream)
	private
		FSource: TStream;
	protected
		function GetSize: Int64; override;
	public
		constructor Create(Source: TStream);
		function Read(var Buffer; Count: Longint): Longint; override;
		function Write(const Buffer; Count: Longint): Longint; override;
		function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
	end;

	{Null implementation - pass-through without encryption.
		Use when encryption is not needed. Zero overhead for stream operations.}
	TNullCipher = class(TInterfacedObject, ICipher)
	public
		function CryptFile(SourceFileName, DestinationFilename: WideString): Integer;
		function CryptStream(SourceStream, DestinationStream: TStream): Integer;
		function CryptFileName(const FileName: WideString): WideString;
		function DecryptFile(SourceFileName, DestinationFilename: WideString): Integer;
		function DecryptStream(SourceStream, DestinationStream: TStream): Integer;
		function DecryptFileName(const FileName: WideString): WideString;
		procedure DecryptDirListing(var CloudMailRuDirListing: TCloudDirItemList);
		function GetEncryptingStream(Source: TStream): TStream;
		function GetDecryptingStream(Source: TStream): TStream;
	end;

	{Production implementation using AES/Rijndael encryption}
	TFileCipher = class(TInterfacedObject, ICipher)
	private
		Password: WideString;
		FFileCipher: TDCP_rijndael; {The cipher used to encrypt files and streams}
		FilenameCipher: TDCP_rijndael; {The cipher used to encrypt filenames}
		DoFilenameCipher: Boolean; {Do filenames encryption}
		PasswordIsWrong: Boolean; {The wrong password flag}

		procedure CiphersInit();
		procedure CiphersDestroy();

	protected
		function Base64ToSafe(const Base64: WideString): WideString; {Safely converts Base64-encoded string to URL and filename (RFC 4648)}
		function Base64FromSafe(const Safe: WideString): WideString; {Converts a string (assuming to be an url or a filename) to a Base64 format}
	public
		constructor Create(Password: WideString; PasswordControl: WideString = ''; DoFilenameCipher: Boolean = false);
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

		property IsWrongPassword: Boolean read PasswordIsWrong;

		class function GetCryptedGUID(const Password: WideString): WideString; {Get an unique GUID on a password, used to check the passwords validity before login}
		class function CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean; {Check if a password is valid by compare with a saved GUID}

	end;

implementation

uses
	CipherStreams;

{TPassThroughStream - lightweight read-only wrapper}

constructor TPassThroughStream.Create(Source: TStream);
begin
	inherited Create;
	FSource := Source;
end;

function TPassThroughStream.GetSize: Int64;
begin
	Result := FSource.Size;
end;

function TPassThroughStream.Read(var Buffer; Count: Longint): Longint;
begin
	Result := FSource.Read(Buffer, Count);
end;

function TPassThroughStream.Write(const Buffer; Count: Longint): Longint;
begin
	{Pass-through stream is read-only for encryption/upload use case}
	Result := FSource.Write(Buffer, Count);
end;

function TPassThroughStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
	Result := FSource.Seek(Offset, Origin);
end;

{TNullCipher - pass-through implementation}

function TNullCipher.CryptFile(SourceFileName, DestinationFilename: WideString): Integer;
var
	SourceStream, DestStream: TFileStream;
begin
	Result := CIPHER_OK;
	try
		SourceStream := TFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyWrite);
		try
			DestStream := TFileStream.Create(DestinationFilename, fmCreate);
			try
				DestStream.CopyFrom(SourceStream, 0);
			finally
				DestStream.Free;
			end;
		finally
			SourceStream.Free;
		end;
	except
		Result := CIPHER_IO_ERROR;
	end;
end;

function TNullCipher.CryptStream(SourceStream, DestinationStream: TStream): Integer;
begin
	Result := 0;
	if SourceStream.Size > 0 then
	begin
		SourceStream.Position := 0;
		Result := DestinationStream.CopyFrom(SourceStream, SourceStream.Size);
	end;
end;

function TNullCipher.CryptFileName(const FileName: WideString): WideString;
begin
	Result := ExtractFileName(FileName);
end;

function TNullCipher.DecryptFile(SourceFileName, DestinationFilename: WideString): Integer;
begin
	{Decryption is same as encryption for null cipher - just copy}
	Result := CryptFile(SourceFileName, DestinationFilename);
end;

function TNullCipher.DecryptStream(SourceStream, DestinationStream: TStream): Integer;
begin
	{Decryption is same as encryption for null cipher - just copy}
	Result := CryptStream(SourceStream, DestinationStream);
end;

function TNullCipher.DecryptFileName(const FileName: WideString): WideString;
begin
	Result := ExtractFileName(FileName);
end;

procedure TNullCipher.DecryptDirListing(var CloudMailRuDirListing: TCloudDirItemList);
var
	i: Integer;
begin
	{Set visible_name to name for all items - no decryption needed}
	for i := 0 to Length(CloudMailRuDirListing) - 1 do
		CloudMailRuDirListing[i].visible_name := CloudMailRuDirListing[i].name;
end;

function TNullCipher.GetEncryptingStream(Source: TStream): TStream;
begin
	{Return lightweight wrapper - no transformation, minimal overhead}
	Result := TPassThroughStream.Create(Source);
end;

function TNullCipher.GetDecryptingStream(Source: TStream): TStream;
begin
	{Return lightweight wrapper - no transformation, minimal overhead}
	Result := TPassThroughStream.Create(Source);
end;

{TFileCipher - AES/Rijndael implementation}

function TFileCipher.Base64FromSafe(const Safe: WideString): WideString;
begin
	Result := Safe;
	Result := StringReplace(Result, '-', '+', [rfReplaceAll]);
	Result := StringReplace(Result, '_', '/', [rfReplaceAll]);
end;

function TFileCipher.Base64ToSafe(const Base64: WideString): WideString;
begin
	Result := Base64;
	Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
	Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
end;

class function TFileCipher.GetCryptedGUID(const Password: WideString): WideString;
var
	tmpCipher: TDCP_rijndael;
begin
	tmpCipher := TDCP_rijndael.Create(nil);
	tmpCipher.InitStr(Password, TDCP_sha1);
	Result := tmpCipher.EncryptString(CIPHER_CONTROL_GUID);
	tmpCipher.Burn;
	tmpCipher.Destroy;
end;

class function TFileCipher.CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean;
begin
	Result := self.GetCryptedGUID(Password) = ControlGUID;
end;

procedure TFileCipher.CiphersDestroy;
begin
	self.FFileCipher.Burn;
	self.FFileCipher.Destroy;

	if self.DoFilenameCipher then
	begin
		self.FilenameCipher.Burn;
		self.FilenameCipher.Destroy;
	end;

end;

procedure TFileCipher.CiphersInit;
begin
	self.FFileCipher := TDCP_rijndael.Create(nil);
	self.FFileCipher.InitStr(self.Password, TDCP_sha1);
	if self.DoFilenameCipher then
	begin
		self.FilenameCipher := TDCP_rijndael.Create(nil);
		self.FilenameCipher.InitStr(self.Password, TDCP_sha1);
	end;

end;

constructor TFileCipher.Create(Password: WideString; PasswordControl: WideString = ''; DoFilenameCipher: Boolean = false);
begin
	self.Password := Password;
	self.DoFilenameCipher := DoFilenameCipher;

	self.CiphersInit();
	if EmptyWideStr <> PasswordControl then
		PasswordIsWrong := not(self.FFileCipher.EncryptString(CIPHER_CONTROL_GUID) = PasswordControl);
	self.CiphersDestroy;
end;

destructor TFileCipher.Destroy;
begin
	inherited;
end;

function TFileCipher.CryptFile(SourceFileName, DestinationFilename: WideString): Integer;
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

function TFileCipher.CryptFileName(const FileName: WideString): WideString;
begin
	self.CiphersInit();
	try
		Result := ExtractFileName(FileName);
		if EmptyWideStr = Result then
			exit;
		if DoFilenameCipher then
			Result := Base64ToSafe(self.FilenameCipher.EncryptString(Result));
	finally
		self.CiphersDestroy;
	end;
end;

function TFileCipher.CryptStream(SourceStream, DestinationStream: TStream): Integer;
begin
	self.CiphersInit();
	try
		Result := 0;
		if SourceStream.Size > 0 then
		begin
			SourceStream.Position := 0;
			Result := self.FFileCipher.EncryptStream(SourceStream, DestinationStream, SourceStream.Size);
		end;
	finally
		self.CiphersDestroy;
	end;
end;

procedure TFileCipher.DecryptDirListing(var CloudMailRuDirListing: TCloudDirItemList);
var
	i: Integer;
begin
	for i := 0 to Length(CloudMailRuDirListing) - 1 do
	begin
		CloudMailRuDirListing[i].visible_name := self.DecryptFileName(CloudMailRuDirListing[i].name);
	end;
end;

function TFileCipher.DecryptFile(SourceFileName, DestinationFilename: WideString): Integer;
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

function TFileCipher.DecryptFileName(const FileName: WideString): WideString;
begin
	self.CiphersInit();
	try
		Result := ExtractFileName(FileName);
		if EmptyWideStr = Result then
			exit;
		if DoFilenameCipher then
			Result := self.FilenameCipher.DecryptString(Base64FromSafe(FileName));
	finally
		self.CiphersDestroy();
	end;
end;

function TFileCipher.DecryptStream(SourceStream, DestinationStream: TStream): Integer;
begin
	self.CiphersInit();
	try
		Result := 0;
		if SourceStream.Size > 0 then
			Result := self.FFileCipher.DecryptStream(SourceStream, DestinationStream, SourceStream.Size);
	finally
		self.CiphersDestroy();
	end;
end;

function TFileCipher.GetEncryptingStream(Source: TStream): TStream;
var
	Cipher: TDCP_rijndael;
begin
	{Create fresh cipher instance - stream takes ownership}
	Cipher := TDCP_rijndael.Create(nil);
	Cipher.InitStr(self.Password, TDCP_sha1);
	Result := TEncryptingStream.Create(Source, Cipher);
end;

function TFileCipher.GetDecryptingStream(Source: TStream): TStream;
var
	Cipher: TDCP_rijndael;
begin
	{Create fresh cipher instance - stream takes ownership}
	Cipher := TDCP_rijndael.Create(nil);
	Cipher.InitStr(self.Password, TDCP_sha1);
	Result := TDecryptingStream.Create(Source, Cipher);
end;

end.
