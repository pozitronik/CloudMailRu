unit Cipher;

interface

uses System.SysUtils, System.Classes, DCPcrypt2, DCPblockciphers, DCPrijndael, DCPSha1;

const
	CIPHER_OK = 0;
	CIPHER_IO_ERROR = 1;

type
	TCipher = class
	private
		fileCipher, filenameCipher: TDCP_rijndael; //AES ciphers
		DoFilenameCipher: boolean; //רטפנמגאעü טלוםא פאיכמג
	protected
	public
		constructor Create(fileKey: WideString; filenameKey: WideString = '');
		destructor Destroy; override;
		function CryptFile(SourceFileName, DestinationFilename: WideString): integer;
		function CryptStream(SourceStream, DestinationStream: TStream): integer;
		function CryptFileName(const FileName: WideString): WideString;

		function DecryptFile(SourceFileName, DestinationFilename: WideString): integer;
		function DecryptStream(SourceStream, DestinationStream: TStream): integer;
		function DecryptFileName(const FileName: WideString): WideString;
		class function Base64ToSafe(const Base64: WideString): WideString; //converts Base64-encoded string to URL and Filename safe (RFC 4648)
		class function Base64FromSafe(const Safe: WideString): WideString;
	end;

implementation

{TCipher}

class function TCipher.Base64FromSafe(const Safe: WideString): WideString;
begin
	Result := Safe;
	Result := StringReplace(Result, '-', '+', [rfReplaceAll]);
	Result := StringReplace(Result, '_', '/', [rfReplaceAll]);
end;

class function TCipher.Base64ToSafe(const Base64: WideString): WideString;
begin
	Result := Base64;
	Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
	Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
end;

constructor TCipher.Create(fileKey: WideString; filenameKey: WideString = '');
begin
	self.fileCipher := TDCP_rijndael.Create(nil);
	self.fileCipher.InitStr(fileKey, TDCP_sha1);
	DoFilenameCipher := false;
	if EmptyWideStr <> filenameKey then
	begin
		self.filenameCipher := TDCP_rijndael.Create(nil);
		self.filenameCipher.InitStr(filenameKey, TDCP_sha1);
		DoFilenameCipher := true;
	end;
end;

function TCipher.CryptFile(SourceFileName, DestinationFilename: WideString): integer;
var
	SourceStream, DestinationStream: TFileStream;
begin
	Result := CIPHER_OK;
	try

		SourceStream := TFileStream.Create(SourceFileName, fmOpenRead);
		DestinationStream := TFileStream.Create(DestinationFilename, fmCreate);
		if SourceStream.Size > 0 then
			self.CryptStream(SourceStream, DestinationStream);

		SourceStream.Free;
		DestinationStream.Free;
	except
		Result := CIPHER_IO_ERROR;
	end;
end;

function TCipher.CryptFileName(const FileName: WideString): WideString;
begin
	Result := ExtractFileName(FileName);
	if EmptyWideStr = Result then
		exit;
	if DoFilenameCipher then
		Result := Base64ToSafe(self.filenameCipher.EncryptString(Result));
end;

function TCipher.CryptStream(SourceStream, DestinationStream: TStream): integer;
begin
	Result := 0;
	if SourceStream.Size > 0 then

		Result := self.fileCipher.EncryptStream(SourceStream, DestinationStream, SourceStream.Size);
	//self.fileCipher.Burn;
	//self.fileCipher.Destroy;
end;

function TCipher.DecryptFile(SourceFileName, DestinationFilename: WideString): integer;
var
	SourceStream, DestinationStream: TFileStream;
begin
	Result := CIPHER_OK;
	try

		SourceStream := TFileStream.Create(SourceFileName, fmOpenRead);
		DestinationStream := TFileStream.Create(DestinationFilename, fmCreate);
		if SourceStream.Size > 0 then
			self.DecryptStream(SourceStream, DestinationStream);
		SourceStream.Free;
		DestinationStream.Free;
	except
		Result := CIPHER_IO_ERROR;
	end;
end;

function TCipher.DecryptFileName(const FileName: WideString): WideString;
begin
	Result := ExtractFileName(FileName);
	if EmptyWideStr = Result then
		exit;

	if DoFilenameCipher then
		Result := self.filenameCipher.DecryptString(Base64FromSafe(FileName));
end;

function TCipher.DecryptStream(SourceStream, DestinationStream: TStream): integer;
begin
	Result := 0;
	if SourceStream.Size > 0 then
		Result := self.fileCipher.DecryptStream(SourceStream, DestinationStream, SourceStream.Size);
	//self.fileCipher.Burn;
	//self.fileCipher.Destroy;
end;

destructor TCipher.Destroy;
begin
	self.fileCipher.Destroy;
	if Assigned(self.filenameCipher) then
		self.filenameCipher.Destroy;
	inherited;
end;

end.
