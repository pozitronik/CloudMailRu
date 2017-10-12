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
		function CryptFileStream(var SourceFileStream, DestinationFileStream: TFileStream): integer;
		function CryptFileName(const FileName: WideString): WideString;

		function DecryptFile(SourceFileName, DestinationFilename: WideString): integer;
		function DecryptFileStream(var SourceFileStream, DestinationFileStream: TFileStream): integer;
		function DecryptFileName(const FileName: WideString): WideString;
	end;

implementation

{TCipher}

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
	SourceFileStream, DestinationFileStream: TFileStream;
begin
	result := CIPHER_OK;
	try

		SourceFileStream := TFileStream.Create(SourceFileName, fmOpenRead);
		DestinationFileStream := TFileStream.Create(DestinationFilename, fmCreate);
		self.CryptFileStream(SourceFileStream, DestinationFileStream);
		SourceFileStream.Free;
		DestinationFileStream.Free;
	except
		result := CIPHER_IO_ERROR;
	end;
end;

function TCipher.CryptFileName(const FileName: WideString): WideString;
begin
	result := ExtractFileName(FileName);
	if DoFilenameCipher then result := self.filenameCipher.EncryptString(result);
end;

function TCipher.CryptFileStream(var SourceFileStream, DestinationFileStream: TFileStream): integer;
begin
	result := self.fileCipher.EncryptStream(SourceFileStream, DestinationFileStream, SourceFileStream.Size);
	//self.fileCipher.Burn;
	//self.fileCipher.Destroy;
end;

function TCipher.DecryptFile(SourceFileName, DestinationFilename: WideString): integer;
var
	SourceFileStream, DestinationFileStream: TFileStream;
begin
	result := CIPHER_OK;
	try

		SourceFileStream := TFileStream.Create(SourceFileName, fmOpenRead);
		DestinationFileStream := TFileStream.Create(DestinationFilename, fmCreate);
		self.DecryptFileStream(SourceFileStream, DestinationFileStream);
		SourceFileStream.Free;
		DestinationFileStream.Free;
	except
		result := CIPHER_IO_ERROR;
	end;
end;

function TCipher.DecryptFileName(const FileName: WideString): WideString;
begin
	result := ExtractFileName(FileName);
	if DoFilenameCipher then result := self.filenameCipher.DecryptString(FileName);
end;

function TCipher.DecryptFileStream(var SourceFileStream, DestinationFileStream: TFileStream): integer;
begin
	result := self.fileCipher.DecryptStream(SourceFileStream, DestinationFileStream, SourceFileStream.Size);
	//self.fileCipher.Burn;
	//self.fileCipher.Destroy;
end;

destructor TCipher.Destroy;
begin
	self.fileCipher.Destroy;
	if Assigned(self.filenameCipher) then self.filenameCipher.Destroy;
	inherited;
end;

end.
