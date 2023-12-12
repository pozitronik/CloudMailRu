unit FileCipher;

interface

{Сначала реализация, потом рефакторинг. Шифрование будет каким-нибудь интерфейсом описано, а это уедет в реализацию}
uses
	CMRDirItemList,
	System.SysUtils,
	System.Classes,
	CMRConstants,
	DCPcrypt2,
	DCPblockciphers,
	DCPrijndael,
	DCPSha1,
	DCPbase64;

const
	CIPHER_OK = 0;
	CIPHER_IO_ERROR = 1;
	CIPHER_WRONG_PASSWORD = 2;
	CIPHER_CONTROL_GUID = '2b580ce6-e72f-433d-9788-3ecb6b0d9580';

type
	TFileCipher = class
	private
		password: WideString;
		FileCipher, filenameCipher: TDCP_rijndael; //AES ciphers
		DoFilenameCipher: boolean; //шифровать имена файлов
		PasswordWrong: boolean;

		procedure CiphersInit();
		procedure CiphersDestroy();

	protected
	public
		property WrongPassword: boolean read PasswordWrong;

		{filePassword: пароль для шифрования\дешифрации файлов
		 filePasswordControl: контрольный код для проверки файлового пароля (только дешифрация), пустой - не проверять
		}
		constructor Create(password: WideString; PasswordControl: WideString = ''; DoFilenameCipher: boolean = false);
		destructor Destroy; override;
		function CryptFile(SourceFileName, DestinationFilename: WideString): integer;
		function CryptStream(SourceStream, DestinationStream: TStream): integer;
		function CryptFileName(const FileName: WideString): WideString;

		function DecryptFile(SourceFileName, DestinationFilename: WideString): integer;
		function DecryptStream(SourceStream, DestinationStream: TStream): integer;
		function DecryptFileName(const FileName: WideString): WideString;
		procedure DecryptDirListing(var CloudMailRuDirListing: TCMRDirItemList);

		class function Base64ToSafe(const Base64: WideString): WideString; //converts Base64-encoded string to URL and Filename safe (RFC 4648)
		class function Base64FromSafe(const Safe: WideString): WideString;

		class function CryptedGUID(const password: WideString): WideString;
		class function CheckPasswordGUID(const password, controlGUID: WideString): boolean;

	end;

implementation

{TFileCipher}

class function TFileCipher.Base64FromSafe(const Safe: WideString): WideString;
begin
	Result := Safe;
	Result := StringReplace(Result, '-', '+', [rfReplaceAll]);
	Result := StringReplace(Result, '_', '/', [rfReplaceAll]);
end;

class function TFileCipher.Base64ToSafe(const Base64: WideString): WideString;
begin
	Result := Base64;
	Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
	Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
end;

class function TFileCipher.CryptedGUID(const password: WideString): WideString;
var
	tmpCipher: TDCP_rijndael;
begin
	tmpCipher := TDCP_rijndael.Create(nil);
	tmpCipher.InitStr(password, TDCP_sha1);
	Result := tmpCipher.EncryptString(CIPHER_CONTROL_GUID);
	tmpCipher.Burn;
	tmpCipher.Destroy;
end;

class function TFileCipher.CheckPasswordGUID(const password, controlGUID: WideString): boolean;
begin
	Result := self.CryptedGUID(password) = controlGUID;
end;

procedure TFileCipher.CiphersDestroy;
begin
	self.FileCipher.Burn;
	self.FileCipher.Destroy;

	if self.DoFilenameCipher then
	begin
		self.filenameCipher.Burn;
		self.filenameCipher.Destroy;
	end;

end;

procedure TFileCipher.CiphersInit;
begin
	self.FileCipher := TDCP_rijndael.Create(nil);
	self.FileCipher.InitStr(self.password, TDCP_sha1);
	if self.DoFilenameCipher then
	begin
		self.filenameCipher := TDCP_rijndael.Create(nil);
		self.filenameCipher.InitStr(self.password, TDCP_sha1);
	end;

end;

constructor TFileCipher.Create(password: WideString; PasswordControl: WideString = ''; DoFilenameCipher: boolean = false);
begin
	self.password := password;
	self.DoFilenameCipher := DoFilenameCipher;

	self.CiphersInit();
	if EmptyWideStr <> PasswordControl then
		PasswordWrong := not(self.FileCipher.EncryptString(CIPHER_CONTROL_GUID) = PasswordControl); //признак неверного пароля
	self.CiphersDestroy;
end;

function TFileCipher.CryptFile(SourceFileName, DestinationFilename: WideString): integer;
var
	SourceStream, DestinationStream: TBufferedFileStream;
begin
	Result := CIPHER_OK;
	self.CiphersInit();
	try
		SourceStream := TBufferedFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyWrite);
		DestinationStream := TBufferedFileStream.Create(DestinationFilename, fmCreate);
		if SourceStream.Size > 0 then
			self.CryptStream(SourceStream, DestinationStream);
		SourceStream.Free;
		DestinationStream.Free;
	except
		self.CiphersDestroy();
		Result := CIPHER_IO_ERROR;
	end;
	self.CiphersDestroy();
end;

function TFileCipher.CryptFileName(const FileName: WideString): WideString;
begin
	self.CiphersInit();
	Result := ExtractFileName(FileName);
	if EmptyWideStr = Result then
		exit;
	if DoFilenameCipher then
		Result := Base64ToSafe(self.filenameCipher.EncryptString(Result));
	self.CiphersDestroy;
end;

function TFileCipher.CryptStream(SourceStream, DestinationStream: TStream): integer;
begin
	self.CiphersInit();
	Result := 0;
	if SourceStream.Size > 0 then
	begin
		SourceStream.Position := 0;
		Result := self.FileCipher.EncryptStream(SourceStream, DestinationStream, SourceStream.Size);
	end;
	self.CiphersDestroy;
end;

procedure TFileCipher.DecryptDirListing(var CloudMailRuDirListing: TCMRDirItemList);
var
	i: integer;
begin
	for i := 0 to Length(CloudMailRuDirListing) - 1 do
	begin
		CloudMailRuDirListing[i].visible_name := self.DecryptFileName(CloudMailRuDirListing[i].name);
	end;
end;

function TFileCipher.DecryptFile(SourceFileName, DestinationFilename: WideString): integer;
var
	SourceStream, DestinationStream: TBufferedFileStream;
begin
	self.CiphersInit();
	Result := CIPHER_OK;
	try
		SourceStream := TBufferedFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyWrite);
		DestinationStream := TBufferedFileStream.Create(DestinationFilename, fmCreate);
		if SourceStream.Size > 0 then
			self.DecryptStream(SourceStream, DestinationStream);
		SourceStream.Free;
		DestinationStream.Free;
	except
		self.CiphersDestroy;
		Result := CIPHER_IO_ERROR;
	end;
	self.CiphersDestroy;
end;

function TFileCipher.DecryptFileName(const FileName: WideString): WideString;
begin
	self.CiphersInit();
	Result := ExtractFileName(FileName);
	if EmptyWideStr = Result then
		exit;

	if DoFilenameCipher then
		Result := self.filenameCipher.DecryptString(Base64FromSafe(FileName));
	self.CiphersDestroy();
end;

function TFileCipher.DecryptStream(SourceStream, DestinationStream: TStream): integer;
begin
	self.CiphersInit();
	Result := 0;
	if SourceStream.Size > 0 then
		Result := self.FileCipher.DecryptStream(SourceStream, DestinationStream, SourceStream.Size);
	self.CiphersDestroy();
end;

destructor TFileCipher.Destroy;
begin
	{self.fileCipher.Destroy;
	 if Assigned(self.filenameCipher) then
	 self.filenameCipher.Destroy;}
	inherited;
end;

end.
