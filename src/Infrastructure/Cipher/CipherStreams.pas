unit CipherStreams;

{Stream wrappers for on-the-fly encryption/decryption with constant memory usage}

interface

uses
	Classes,
	System.SysUtils,
	System.Math,
	DCPcrypt2,
	DCPblockciphers,
	DCPrijndael;

const
	{Buffer size for streaming encryption/decryption - 64KB for optimal I/O}
	CIPHER_BUFFER_SIZE = 65536;

type
	{Stream wrapper that encrypts data on-the-fly during Read operations.
		Uses CFB8 mode for byte-granular encryption with constant memory.
		Takes ownership of the Cipher instance and frees it on destroy.}
	TEncryptingStream = class(TStream)
	private
		FSource: TStream;
		FCipher: TDCP_rijndael;
		FBuffer: TBytes;
		FBufferPos: Integer;
		FBufferLen: Integer;
		FSourceSize: Int64;
		procedure RefillBuffer;
	protected
		function GetSize: Int64; override;
	public
		constructor Create(Source: TStream; Cipher: TDCP_rijndael);
		destructor Destroy; override;
		function Read(var Buffer; Count: Longint): Longint; override;
		function Write(const Buffer; Count: Longint): Longint; override;
		function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
	end;

	{Stream wrapper that decrypts data on-the-fly during Read operations.
		Uses CFB8 mode for byte-granular decryption with constant memory.
		Takes ownership of the Cipher instance and frees it on destroy.}
	TDecryptingStream = class(TStream)
	private
		FSource: TStream;
		FCipher: TDCP_rijndael;
		FBuffer: TBytes;
		FBufferPos: Integer;
		FBufferLen: Integer;
		FSourceSize: Int64;
		procedure RefillBuffer;
	protected
		function GetSize: Int64; override;
	public
		constructor Create(Source: TStream; Cipher: TDCP_rijndael);
		destructor Destroy; override;
		function Read(var Buffer; Count: Longint): Longint; override;
		function Write(const Buffer; Count: Longint): Longint; override;
		function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
	end;

implementation

{TEncryptingStream - on-the-fly encryption wrapper}

constructor TEncryptingStream.Create(Source: TStream; Cipher: TDCP_rijndael);
begin
	inherited Create;
	FSource := Source;
	FCipher := Cipher;
	FSourceSize := Source.Size;
	SetLength(FBuffer, CIPHER_BUFFER_SIZE);
	FBufferPos := 0;
	FBufferLen := 0;
end;

destructor TEncryptingStream.Destroy;
begin
	FCipher.Burn;
	FCipher.Free;
	SetLength(FBuffer, 0);
	inherited;
end;

function TEncryptingStream.GetSize: Int64;
begin
	{CFB mode produces same size output as input}
	Result := FSourceSize;
end;

procedure TEncryptingStream.RefillBuffer;
var
	BytesRead: Integer;
begin
	BytesRead := FSource.Read(FBuffer[0], CIPHER_BUFFER_SIZE);
	if BytesRead > 0 then
		FCipher.EncryptCFB8bit(FBuffer[0], FBuffer[0], BytesRead);
	FBufferLen := BytesRead;
	FBufferPos := 0;
end;

function TEncryptingStream.Read(var Buffer; Count: Longint): Longint;
var
	BytesToCopy: Integer;
	Dest: PByte;
begin
	Result := 0;
	Dest := @Buffer;
	while Count > 0 do
	begin
		if FBufferPos >= FBufferLen then
		begin
			RefillBuffer;
			if FBufferLen = 0 then
				Break;
		end;
		BytesToCopy := Min(Count, FBufferLen - FBufferPos);
		Move(FBuffer[FBufferPos], Dest^, BytesToCopy);
		Inc(FBufferPos, BytesToCopy);
		Inc(Dest, BytesToCopy);
		Inc(Result, BytesToCopy);
		Dec(Count, BytesToCopy);
	end;
end;

function TEncryptingStream.Write(const Buffer; Count: Longint): Longint;
begin
	{Encrypting stream is read-only - used for upload where we read encrypted data}
	raise Exception.Create('TEncryptingStream is read-only');
end;

function TEncryptingStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
	if (Offset = 0) and (Origin = soCurrent) then
		{Return current logical position}
		Result := FSource.Position - FBufferLen + FBufferPos
	else if (Offset = 0) and (Origin = soBeginning) then
	begin
		{Reset to beginning - required by Indy HTTP for uploads}
		FSource.Position := 0;
		FCipher.Reset; {Restore cipher to state just after Init}
		FBufferPos := 0;
		FBufferLen := 0;
		Result := 0;
	end
	else
		raise Exception.Create('TEncryptingStream does not support arbitrary seeking');
end;

{TDecryptingStream - on-the-fly decryption wrapper}

constructor TDecryptingStream.Create(Source: TStream; Cipher: TDCP_rijndael);
begin
	inherited Create;
	FSource := Source;
	FCipher := Cipher;
	FSourceSize := Source.Size;
	SetLength(FBuffer, CIPHER_BUFFER_SIZE);
	FBufferPos := 0;
	FBufferLen := 0;
end;

destructor TDecryptingStream.Destroy;
begin
	FCipher.Burn;
	FCipher.Free;
	SetLength(FBuffer, 0);
	inherited;
end;

function TDecryptingStream.GetSize: Int64;
begin
	{CFB mode produces same size output as input}
	Result := FSourceSize;
end;

procedure TDecryptingStream.RefillBuffer;
var
	BytesRead: Integer;
begin
	BytesRead := FSource.Read(FBuffer[0], CIPHER_BUFFER_SIZE);
	if BytesRead > 0 then
		FCipher.DecryptCFB8bit(FBuffer[0], FBuffer[0], BytesRead);
	FBufferLen := BytesRead;
	FBufferPos := 0;
end;

function TDecryptingStream.Read(var Buffer; Count: Longint): Longint;
var
	BytesToCopy: Integer;
	Dest: PByte;
begin
	Result := 0;
	Dest := @Buffer;
	while Count > 0 do
	begin
		if FBufferPos >= FBufferLen then
		begin
			RefillBuffer;
			if FBufferLen = 0 then
				Break;
		end;
		BytesToCopy := Min(Count, FBufferLen - FBufferPos);
		Move(FBuffer[FBufferPos], Dest^, BytesToCopy);
		Inc(FBufferPos, BytesToCopy);
		Inc(Dest, BytesToCopy);
		Inc(Result, BytesToCopy);
		Dec(Count, BytesToCopy);
	end;
end;

function TDecryptingStream.Write(const Buffer; Count: Longint): Longint;
begin
	{Decrypting stream is read-only - used for download where we read decrypted data}
	raise Exception.Create('TDecryptingStream is read-only');
end;

function TDecryptingStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
	if (Offset = 0) and (Origin = soCurrent) then
		{Return current logical position}
		Result := FSource.Position - FBufferLen + FBufferPos
	else if (Offset = 0) and (Origin = soBeginning) then
	begin
		{Reset to beginning - required by Indy HTTP for downloads}
		FSource.Position := 0;
		FCipher.Reset; {Restore cipher to state just after Init}
		FBufferPos := 0;
		FBufferLen := 0;
		Result := 0;
	end
	else
		raise Exception.Create('TDecryptingStream does not support arbitrary seeking');
end;

end.
