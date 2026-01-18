unit CipherInterface;

{Interface for encryption operations, decoupled from specific cipher implementation}

interface

uses
	Classes,
	CMRDirItemList;

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
		procedure DecryptDirListing(var CloudMailRuDirListing: TCMRDirItemList);
	end;

	{Null implementation for testing - pass-through without encryption}
	TNullCipher = class(TInterfacedObject, ICipher)
	public
		function CryptFile(SourceFileName, DestinationFilename: WideString): Integer;
		function CryptStream(SourceStream, DestinationStream: TStream): Integer;
		function CryptFileName(const FileName: WideString): WideString;
		function DecryptFile(SourceFileName, DestinationFilename: WideString): Integer;
		function DecryptStream(SourceStream, DestinationStream: TStream): Integer;
		function DecryptFileName(const FileName: WideString): WideString;
		procedure DecryptDirListing(var CloudMailRuDirListing: TCMRDirItemList);
	end;

implementation

uses
	SysUtils;

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

procedure TNullCipher.DecryptDirListing(var CloudMailRuDirListing: TCMRDirItemList);
var
	i: Integer;
begin
	{Set visible_name to name for all items - no decryption needed}
	for i := 0 to Length(CloudMailRuDirListing) - 1 do
		CloudMailRuDirListing[i].visible_name := CloudMailRuDirListing[i].name;
end;

end.
