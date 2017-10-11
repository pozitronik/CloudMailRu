unit Cipher;

interface

uses System.SysUtils, System.Classes, MRC_Helper, DCPcrypt2, DCPblockciphers, DCPrijndael, DCPSha1;

const
	CIPHER_OK = 0;
	CIPHER_IO_ERROR = 1;

type
	TCipher = class
	private
		fileCipher, filenameCipher: TDCP_rijndael; //AES ciphers
		DoFilenameCiper: boolean; //шифровать имена файлов
	protected
	public
		constructor Create(fileKey: WideString; filenameKey: WideString = '');
		destructor Destroy; override;
		function CryptFile(SourceFileName: WideString; var DestinationFileName: WideString): integer; {DestinationFileName устанавливается само в зависимости от того, шифруются ли имена}
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
	DoFilenameCiper := false;
	if EmptyWideStr <> filenameKey then
	begin
		self.filenameCipher := TDCP_rijndael.Create(nil);
		self.filenameCipher.InitStr(filenameKey, TDCP_sha1);
		DoFilenameCiper := true;
	end;
end;

function TCipher.CryptFile(SourceFileName: WideString; var DestinationFileName: WideString): integer;
var
	SourceFileStream, DestinationFileStream: TFileStream;
begin
	result := CIPHER_OK;
	try
		if DoFilenameCiper then
		begin
			DestinationFileName := GetTmpDir() + self.filenameCipher.EncryptString(ExtractFileName(SourceFileName));
		end else begin
			DestinationFileName := GetTmpFileName();
		end;
		SourceFileStream := TFileStream.Create(SourceFileName, fmOpenRead);
		DestinationFileStream := TFileStream.Create(DestinationFileName, fmCreate);
		self.CryptFileStream(SourceFileStream, DestinationFileStream);
		SourceFileStream.Free;
		DestinationFileStream.Free;
	except
		result := CIPHER_IO_ERROR;
	end;
end;

function TCipher.CryptFileStream(var SourceFileStream, DestinationFileStream: TFileStream): integer;
begin
	result := self.fileCipher.EncryptStream(SourceFileStream, DestinationFileStream, SourceFileStream.Size);
	//self.fileCipher.Burn;
	//self.fileCipher.Destroy;
end;

function TCipher.DecryptFile(SourceFileName, DestinationFileName: WideString): integer;
var
	SourceFileStream, DestinationFileStream: TFileStream;
begin
	result := CIPHER_OK;
	try
//		if DoFilenameCiper then
//		begin
//			DestinationFileName := GetTmpDir() + self.filenameCipher.DecryptString(ExtractFileName(SourceFileName));
//		end else begin
//			DestinationFileName := GetTmpFileName();
//		end;

		SourceFileStream := TFileStream.Create(SourceFileName, fmOpenRead);
		DestinationFileStream := TFileStream.Create(DestinationFileName, fmCreate);
		self.DecryptFileStream(SourceFileStream, DestinationFileStream);
		SourceFileStream.Free;
		DestinationFileStream.Free;
	except
		result := CIPHER_IO_ERROR;
	end;
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
