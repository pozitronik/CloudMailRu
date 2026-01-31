unit CipherStreams;

{Stream wrappers for on-the-fly encryption/decryption with constant memory usage}

interface

uses
	Classes,
	System.SysUtils,
	System.Math,
	BlockCipher;

const
	{Buffer size for streaming encryption/decryption - 64KB for optimal I/O}
	CIPHER_BUFFER_SIZE = 65536;

type
	{Base class for on-the-fly cipher stream wrappers with constant memory usage.
		Reads source in buffered chunks, applies TransformBuffer (encrypt or decrypt),
		and serves transformed data via Read. Subclasses override TransformBuffer only.
		Takes ownership of the Cipher instance (via interface ref counting).}
	TBaseCipherStream = class(TStream)
	private
		FSource: TStream;
		FCipher: IBlockCipher;
		FBuffer: TBytes;
		FBufferPos: Integer;
		FBufferLen: Integer;
		FSourceSize: Int64;
		procedure RefillBuffer;
	protected
		function GetSize: Int64; override;
		procedure TransformBuffer(var Buffer; Size: Integer); virtual; abstract;
	public
		constructor Create(Source: TStream; Cipher: IBlockCipher);
		destructor Destroy; override;
		function Read(var Buffer; Count: Longint): Longint; override;
		function Write(const Buffer; Count: Longint): Longint; override;
		function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
	end;

	{Encrypts data on-the-fly during Read operations via CFB8 mode.}
	TEncryptingStream = class(TBaseCipherStream)
	protected
		procedure TransformBuffer(var Buffer; Size: Integer); override;
	end;

	{Decrypts data on-the-fly during Read operations via CFB8 mode.}
	TDecryptingStream = class(TBaseCipherStream)
	protected
		procedure TransformBuffer(var Buffer; Size: Integer); override;
	end;

implementation

{TBaseCipherStream}

constructor TBaseCipherStream.Create(Source: TStream; Cipher: IBlockCipher);
begin
	inherited Create;
	FSource := Source;
	FCipher := Cipher;
	FSourceSize := Source.Size;
	SetLength(FBuffer, CIPHER_BUFFER_SIZE);
	FBufferPos := 0;
	FBufferLen := 0;
end;

destructor TBaseCipherStream.Destroy;
begin
	FCipher.Burn;
	FCipher := nil;
	SetLength(FBuffer, 0);
	inherited;
end;

function TBaseCipherStream.GetSize: Int64;
begin
	{CFB mode produces same size output as input}
	Result := FSourceSize;
end;

procedure TBaseCipherStream.RefillBuffer;
var
	BytesRead: Integer;
begin
	BytesRead := FSource.Read(FBuffer[0], CIPHER_BUFFER_SIZE);
	if BytesRead > 0 then
		TransformBuffer(FBuffer[0], BytesRead);
	FBufferLen := BytesRead;
	FBufferPos := 0;
end;

function TBaseCipherStream.Read(var Buffer; Count: Longint): Longint;
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

function TBaseCipherStream.Write(const Buffer; Count: Longint): Longint;
begin
	{Cipher streams are read-only - used for upload/download where we read transformed data}
	raise Exception.Create(ClassName + ' is read-only');
end;

function TBaseCipherStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
	if (Offset = 0) and (Origin = soCurrent) then
		{Return current logical position}
		Result := FSource.Position - FBufferLen + FBufferPos
	else if (Offset = 0) and (Origin = soBeginning) then
	begin
		{Reset to beginning - required by Indy HTTP for uploads/downloads}
		FSource.Position := 0;
		FCipher.Reset; {Restore cipher to state just after Init}
		FBufferPos := 0;
		FBufferLen := 0;
		Result := 0;
	end
	else
		raise Exception.Create(ClassName + ' does not support arbitrary seeking');
end;

{TEncryptingStream}

procedure TEncryptingStream.TransformBuffer(var Buffer; Size: Integer);
begin
	FCipher.EncryptCFB8bit(Buffer, Buffer, Size);
end;

{TDecryptingStream}

procedure TDecryptingStream.TransformBuffer(var Buffer; Size: Integer);
begin
	FCipher.DecryptCFB8bit(Buffer, Buffer, Size);
end;

end.
