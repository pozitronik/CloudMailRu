unit Cipher;

interface

uses System.SysUtils, System.Classes, DCPcrypt2, DCPblockciphers, DCPrijndael, DCPSha1;

type
	TCipher = class
	private
		fileCipher, filenameCipher: TDCP_rijndael; //AES ciphers

	protected
	public
		constructor Create(fileKey: WideString; filenameKey: WideString = '');
		destructor Destroy; override;
		function CryptFile(SourceFileName, DestinationFileName: WideString): integer;
		function CryptFileStream(var SourceFileStream, DestinationFileStream: TFileStream): integer;
		function DecryptFile(SourceFileName, DestinationFileName: WideString): integer;
		function DecryptFileStream(var SourceFileStream, DestinationFileStream: TFileStream): integer;
	end;

implementation

{TCipher}

constructor TCipher.Create(fileKey: WideString; filenameKey: WideString = '');
begin
	self.fileCipher := TDCP_rijndael.Create(nil);
	self.fileCipher.InitStr(fileKey, TDCP_sha1);
	if EmptyWideStr <> filenameKey then
	begin
		self.filenameCipher := TDCP_rijndael.Create(nil);
		self.filenameCipher.InitStr(filenameKey, TDCP_sha1);
	end;
end;

function TCipher.CryptFile(SourceFileName, DestinationFileName: WideString): integer;
begin

end;

function TCipher.CryptFileStream(var SourceFileStream, DestinationFileStream: TFileStream): integer;
begin

end;

function TCipher.DecryptFile(SourceFileName, DestinationFileName: WideString): integer;
begin

end;

function TCipher.DecryptFileStream(var SourceFileStream, DestinationFileStream: TFileStream): integer;
begin

end;

destructor TCipher.Destroy;
begin
	self.fileCipher.Destroy;
	if Assigned(self.filenameCipher) then self.filenameCipher.Destroy;
	inherited;
end;

end.
