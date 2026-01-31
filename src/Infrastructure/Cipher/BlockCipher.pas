unit BlockCipher;

{Backend-agnostic block cipher interface for streaming encryption/decryption.
	Decouples CipherStreams from DCPCrypt, enabling OpenSSL/BCrypt backends.}

interface

uses
	DCPblockciphers;

type
	{Abstract interface for CFB-8 block cipher operations.
		All streaming encryption goes through this, regardless of backend.}
	IBlockCipher = interface
		['{B3A7C8E1-4F2D-4B6A-9E1C-5D3F7A8B2C4E}']
		procedure EncryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
		procedure DecryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
		{Restore cipher to post-init state (for seek-to-beginning)}
		procedure Reset;
		{Securely wipe key material}
		procedure Burn;
	end;

	{Adapter: wraps DCPCrypt TDCP_blockcipher128 behind IBlockCipher.
		Takes ownership of the cipher instance and frees it on destroy.}
	TDCPCryptBlockCipher = class(TInterfacedObject, IBlockCipher)
	private
		FCipher: TDCP_blockcipher128;
	public
		constructor Create(Cipher: TDCP_blockcipher128);
		destructor Destroy; override;
		procedure EncryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
		procedure DecryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
		procedure Reset;
		procedure Burn;
	end;

implementation

{TDCPCryptBlockCipher}

constructor TDCPCryptBlockCipher.Create(Cipher: TDCP_blockcipher128);
begin
	inherited Create;
	FCipher := Cipher;
end;

destructor TDCPCryptBlockCipher.Destroy;
begin
	if FCipher <> nil then
	begin
		FCipher.Burn;
		FCipher.Free;
	end;
	inherited;
end;

procedure TDCPCryptBlockCipher.EncryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
begin
	FCipher.EncryptCFB8bit(Indata, Outdata, Size);
end;

procedure TDCPCryptBlockCipher.DecryptCFB8bit(const Indata; var Outdata; Size: Cardinal);
begin
	FCipher.DecryptCFB8bit(Indata, Outdata, Size);
end;

procedure TDCPCryptBlockCipher.Reset;
begin
	FCipher.Reset;
end;

procedure TDCPCryptBlockCipher.Burn;
begin
	FCipher.Burn;
end;

end.
