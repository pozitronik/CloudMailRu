unit FileCipher;

interface

uses
	CipherInterface,
	CMRDirItemList,
	System.SysUtils,
	System.Classes,
	CMRConstants,
	DCPcrypt2,
	DCPblockciphers,
	DCPrijndael,
	DCPSha1,
	DCPbase64;

type
	TFileCipher = class(TInterfacedObject, ICipher)
	private
		Password: WideString;
		FileCipher: TDCP_rijndael; {The cipher used to encrypt files and streams}
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

		function CryptFile(SourceFileName, DestinationFilename: WideString): integer;
		function CryptStream(SourceStream, DestinationStream: TStream): integer;
		function CryptFileName(const FileName: WideString): WideString;

		function DecryptFile(SourceFileName, DestinationFilename: WideString): integer;
		function DecryptStream(SourceStream, DestinationStream: TStream): integer;
		function DecryptFileName(const FileName: WideString): WideString;
		procedure DecryptDirListing(var CloudMailRuDirListing: TCMRDirItemList);

		property IsWrongPassword: Boolean read PasswordIsWrong;

		class function GetCryptedGUID(const Password: WideString): WideString; {Get an unique GUID on a password, used to check the passwords validity before login}
		class function CheckPasswordGUID(const Password, ControlGUID: WideString): Boolean; {Check if a password is valid by compare with a saved GUID}

	end;

implementation

{TFileCipher}

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
	self.FileCipher.Burn;
	self.FileCipher.Destroy;

	if self.DoFilenameCipher then
	begin
		self.FilenameCipher.Burn;
		self.FilenameCipher.Destroy;
	end;

end;

procedure TFileCipher.CiphersInit;
begin
	self.FileCipher := TDCP_rijndael.Create(nil);
	self.FileCipher.InitStr(self.Password, TDCP_sha1);
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
		PasswordIsWrong := not(self.FileCipher.EncryptString(CIPHER_CONTROL_GUID) = PasswordControl); //признак неверного пароля
	self.CiphersDestroy;
end;

destructor TFileCipher.Destroy;
begin
	{self.fileCipher.Destroy;
	 if Assigned(self.filenameCipher) then
	 self.filenameCipher.Destroy;}
	inherited;
end;

function TFileCipher.CryptFile(SourceFileName, DestinationFilename: WideString): integer;
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

function TFileCipher.CryptStream(SourceStream, DestinationStream: TStream): integer;
begin
	self.CiphersInit();
	try
		Result := 0;
		if SourceStream.Size > 0 then
		begin
			SourceStream.Position := 0;
			Result := self.FileCipher.EncryptStream(SourceStream, DestinationStream, SourceStream.Size);
		end;
	finally
		self.CiphersDestroy;
	end;
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

function TFileCipher.DecryptStream(SourceStream, DestinationStream: TStream): integer;
begin
	self.CiphersInit();
	try
		Result := 0;
		if SourceStream.Size > 0 then
			Result := self.FileCipher.DecryptStream(SourceStream, DestinationStream, SourceStream.Size);
	finally
		self.CiphersDestroy();
	end;
end;

end.
