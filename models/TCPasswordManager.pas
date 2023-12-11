unit TCPasswordManager;

{Обертка над обращениями к менеджеру паролей Total Commander}
interface

Uses
	PLUGIN_TYPES,
	LANGUAGE_STRINGS,
	Windows,
	SysUtils,
	AskPassword, {AskEncryptionPasswords,}
	TCHelper,
	CMRConstants,
	TCLogger;

type

	TTCPasswordManager = class
	private
		CryptProc: TCryptProcW;
		PluginNum: integer;
		CryptoNum: integer;
		Logger: TTCLogger;

	public
		ParentWindow: HWND;
		constructor Create(CryptProc: TCryptProcW; PluginNum, CryptoNum: integer; Logger: TTCLogger; ParentWindow: HWND = 0);
		destructor Destroy(); override;
		function GetPassword(Key: WideString; var Password: WideString): integer;
		function SetPassword(Key, Password: WideString): integer;
	end;

implementation

{TTCPasswordManager}

constructor TTCPasswordManager.Create(CryptProc: TCryptProcW; PluginNum, CryptoNum: integer; Logger: TTCLogger; ParentWindow: HWND = 0);
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

function TTCPasswordManager.GetPassword(Key: WideString; var Password: WideString): integer;
var
	buf: PWideChar;
begin
	GetMem(buf, 1024);
	ZeroMemory(buf, 1024);
	result := self.CryptProc(PluginNum, CryptoNum, FS_CRYPT_LOAD_PASSWORD_NO_UI, PWideChar(Key), buf, 1024);
	if FS_FILE_NOTFOUND = result then //no master password entered yet
	begin
		Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, ERR_NO_MASTER_PASSWORD);
		ZeroMemory(buf, 1024);
		result := self.CryptProc(PluginNum, CryptoNum, FS_CRYPT_LOAD_PASSWORD, PWideChar(Key), buf, 1024); //ask with master password
	end;
	if FS_FILE_OK = result then //all ok, we got password
	begin
		Password := buf;
	end;
	if FS_FILE_READERROR = result then
	begin
		Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_NO_PASSWORDS_STORED);
	end;
	if FS_FILE_NOTSUPPORTED = result then //master password cancelled
	begin
		Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_DECRYPT_FAILED);
	end;
	FreeMemory(buf);
end;

function TTCPasswordManager.SetPassword(Key, Password: WideString): integer;
begin
	result := self.CryptProc(PluginNum, CryptoNum, FS_CRYPT_SAVE_PASSWORD, PWideChar(Key), PWideChar(Password), SizeOf(Password));
	case result of
		FS_FILE_OK:
			begin //TC скушал пароль, запомним в инишник галочку
				Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PASSWORD_SAVED, [Key]);
			end;
		FS_FILE_NOTSUPPORTED: //Сохранение не получилось
			begin
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_ENCRYPT_FAILED, [Key]);
			end;
		FS_FILE_WRITEERROR: //Сохранение опять не получилось
			begin
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_WRITE_FAILED, [Key]);
			end;
		FS_FILE_NOTFOUND: //Не указан мастер-пароль
			begin
				Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_WRITE_NO_MASTER_PASSWORD, [Key]);
			end;
		//Ошибки здесь не значат, что пароль мы не получили - он может быть введён в диалоге
	end;
end;

end.
