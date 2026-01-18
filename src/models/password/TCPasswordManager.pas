unit TCPasswordManager;

{Total Commander password manager wrapper implementing IPasswordManager}

interface

uses
	PLUGIN_TYPES,
	LANGUAGE_STRINGS,
	Windows,
	SysUtils,
	AskPassword,
	TCHelper,
	CMRConstants,
	ILoggerInterface,
	IPasswordManagerInterface;

type
	TTCPasswordManager = class(TInterfacedObject, IPasswordManager)
	private
		CryptProc: TCryptProcW;
		PluginNum: Integer;
		CryptoNum: Integer;
		Logger: ILogger;

	public
		ParentWindow: HWND;
		constructor Create(CryptProc: TCryptProcW; PluginNum, CryptoNum: Integer; Logger: ILogger; ParentWindow: HWND = 0);
		destructor Destroy(); override;
		function GetPassword(Key: WideString; var Password: WideString): Integer;
		function SetPassword(Key, Password: WideString): Integer;
	end;

implementation

{TTCPasswordManager}

constructor TTCPasswordManager.Create(CryptProc: TCryptProcW; PluginNum, CryptoNum: Integer; Logger: ILogger; ParentWindow: HWND = 0);
begin
	self.PluginNum := PluginNum;
	self.CryptoNum := CryptoNum;
	self.CryptProc := CryptProc;
	self.Logger := Logger;
	if (0 = ParentWindow) then
		self.ParentWindow := FindTCWindow
	else
		self.ParentWindow := ParentWindow;
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
