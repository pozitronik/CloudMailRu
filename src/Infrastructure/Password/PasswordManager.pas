unit PasswordManager;

{Total Commander password manager wrapper implementing IPasswordManager}

interface

uses
	WFXTypes,
	LanguageStrings,
	Windows,
	SysUtils,
	AskPassword,
	TCHandler,
	CloudConstants,
	Logger;

const
	{Password manager key constants}
	PASSWORD_SUFFIX_FILECRYPT = ' filecrypt'; {Suffix for file encryption password keys}
	PASSWORD_KEY_PROXY = 'proxy'; {Key prefix for proxy passwords}

type
	IPasswordManager = interface
		['{6FCDC8B2-5015-446E-9DED-1F9EBEEBB771}']
		function GetPassword(Key: WideString; var Password: WideString): Integer;
		function SetPassword(Key, Password: WideString): Integer;
	end;

	{Null implementation for testing - no password storage}
	TNullPasswordManager = class(TInterfacedObject, IPasswordManager)
	public
		function GetPassword(Key: WideString; var Password: WideString): Integer;
		function SetPassword(Key, Password: WideString): Integer;
	end;

	TTCPasswordManager = class(TInterfacedObject, IPasswordManager)
	private
		CryptProc: TCryptProcW;
		PluginNum: Integer;
		CryptoNum: Integer;
		Logger: ILogger;
		FTCHandler: ITCHandler;

	public
		ParentWindow: HWND;
		constructor Create(CryptProc: TCryptProcW; PluginNum, CryptoNum: Integer; Logger: ILogger; TCHandler: ITCHandler);
		destructor Destroy(); override;
		function GetPassword(Key: WideString; var Password: WideString): Integer;
		function SetPassword(Key, Password: WideString): Integer;
	end;

implementation

{TNullPasswordManager}

function TNullPasswordManager.GetPassword(Key: WideString; var Password: WideString): Integer;
begin
	{No password stored - return not found}
	Result := FS_FILE_NOTFOUND;
end;

function TNullPasswordManager.SetPassword(Key, Password: WideString): Integer;
begin
	{Pretend to save successfully}
	Result := FS_FILE_OK;
end;

{TTCPasswordManager}

constructor TTCPasswordManager.Create(CryptProc: TCryptProcW; PluginNum, CryptoNum: Integer; Logger: ILogger; TCHandler: ITCHandler);
begin
	self.PluginNum := PluginNum;
	self.CryptoNum := CryptoNum;
	self.CryptProc := CryptProc;
	self.Logger := Logger;
	self.FTCHandler := TCHandler;
	self.ParentWindow := TCHandler.FindTCWindow;
end;

destructor TTCPasswordManager.Destroy;
begin
	inherited;
end;

function TTCPasswordManager.GetPassword(Key: WideString; var Password: WideString): Integer;
var
	buf: PWideChar;
begin
	GetMem(buf, 1024);
	ZeroMemory(buf, 1024);
	Result := self.CryptProc(PluginNum, CryptoNum, FS_CRYPT_LOAD_PASSWORD_NO_UI, PWideChar(Key), buf, 1024);
	if FS_FILE_NOTFOUND = Result then
	begin
		Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, ERR_NO_MASTER_PASSWORD);
		ZeroMemory(buf, 1024);
		Result := self.CryptProc(PluginNum, CryptoNum, FS_CRYPT_LOAD_PASSWORD, PWideChar(Key), buf, 1024);
	end;
	if FS_FILE_OK = Result then
	begin
		Password := buf;
	end;
	if FS_FILE_READERROR = Result then
	begin
		Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_NO_PASSWORDS_STORED);
	end;
	if FS_FILE_NOTSUPPORTED = Result then
	begin
		Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_DECRYPT_FAILED);
	end;
	ZeroMemory(buf, 1024);
	FreeMemory(buf);
end;

function TTCPasswordManager.SetPassword(Key, Password: WideString): Integer;
begin
	Result := self.CryptProc(PluginNum, CryptoNum, FS_CRYPT_SAVE_PASSWORD, PWideChar(Key), PWideChar(Password), (Length(Password) + 1) * SizeOf(WideChar));
	case Result of
		FS_FILE_OK:
			begin
				Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PASSWORD_SAVED, [Key]);
			end;
		FS_FILE_NOTSUPPORTED:
			begin
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_ENCRYPT_FAILED, [Key]);
			end;
		FS_FILE_WRITEERROR:
			begin
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_WRITE_FAILED, [Key]);
			end;
		FS_FILE_NOTFOUND:
			begin
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_WRITE_NO_MASTER_PASSWORD, [Key]);
			end;
	end;
end;

end.
