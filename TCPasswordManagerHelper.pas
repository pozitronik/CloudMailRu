unit TCPasswordManagerHelper;

{Обертка над обращениями к менеджеру паролей Total Commander}
interface

Uses Plugin_Types, Settings, Windows, SysUtils, AskPassword, {AskEncryptionPasswords,}MRC_Helper, Controls, Cipher, WideStrUtils, System.Classes;

type

	TTCPasswordManager = class
	private
		CryptProc: TCryptProcW;
		PluginNum: integer;
		CryptoNum: integer;
		LogHandleProc: TLogHandler;
	public
		ParentWindow: HWND;
		constructor Create(CryptProc: TCryptProcW; PluginNum, CryptoNum: integer; LogHandleProc: TLogHandler; ParentWindow: HWND = 0);
		destructor Destroy(); override;
		function GetPassword(Key: WideString; var Password: WideString): integer;
		function SetPassword(Key, Password: WideString): integer;
		{--------------------}
		function GetAccountPassword(var AccountSettings: TAccountSettings): Boolean;
		function GetProxyPassword(var ProxySettings: TProxySettings): Boolean;
		function InitCloudCryptPasswords(var AccountSettings: TAccountSettings): Boolean;
		function StoreFileCryptPassword(AccountName: WideString): WideString;

	end;

implementation

{TTCPasswordManager}

constructor TTCPasswordManager.Create(CryptProc: TCryptProcW; PluginNum, CryptoNum: integer; LogHandleProc: TLogHandler; ParentWindow: HWND = 0);
begin
	self.PluginNum := PluginNum;
	self.CryptoNum := CryptoNum;
	self.CryptProc := CryptProc;
	self.LogHandleProc := LogHandleProc;
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
		LogHandleProc(LogLevelDetail, msgtype_details, PWideChar('No master password entered yet'));
		ZeroMemory(buf, 1024);
		result := self.CryptProc(PluginNum, CryptoNum, FS_CRYPT_LOAD_PASSWORD, PWideChar(Key), buf, 1024); //ask with master password
	end;
	if FS_FILE_OK = result then //all ok, we got password
	begin
		Password := buf;
	end;
	if FS_FILE_READERROR = result then
	begin
		LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar('CryptProc returns an error: No password found in the password store'));
	end;
	if FS_FILE_NOTSUPPORTED = result then //master password cancelled
	begin
		LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar('CryptProc returns an error: Decrypt failed'));
	end;
	FreeMemory(buf);
end;

function TTCPasswordManager.SetPassword(Key, Password: WideString): integer;
begin
	result := self.CryptProc(PluginNum, CryptoNum, FS_CRYPT_SAVE_PASSWORD, PWideChar(Key), PWideChar(Password), SizeOf(Password));
	case result of
		FS_FILE_OK:
			begin //TC скушал пароль, запомним в инишник галочку
				LogHandleProc(LogLevelDebug, msgtype_details, PWideChar(Key + ': the password saved in the TC password manager'));
			end;
		FS_FILE_NOTSUPPORTED: //Сохранение не получилось
			begin
				LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar(Key + ': CryptProc returns an error: Encrypt failed'));
			end;
		FS_FILE_WRITEERROR: //Сохранение опять не получилось
			begin
				LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar(Key + ': password NOT saved: Can not write the password to the password store'));
			end;
		FS_FILE_NOTFOUND: //Не указан мастер-пароль
			begin
				LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar(Key + ': password NOT saved: No master password entered yet'));
			end;
		//Ошибки здесь не значат, что пароль мы не получили - он может быть введён в диалоге
	end;
end;

function TTCPasswordManager.GetAccountPassword(var AccountSettings: TAccountSettings): Boolean;
var
	TmpString: WideString;
begin
	if AccountSettings.public_account then
		exit(true);

	if AccountSettings.use_tc_password_manager and (self.GetPassword(AccountSettings.name, AccountSettings.Password) = FS_FILE_OK) then //пароль должен браться из TC
		exit(true);

	//иначе предполагается, что пароль взят из конфига

	if AccountSettings.Password = EmptyWideStr then //но пароля нет, не в инишнике, не в тотале
	begin
		if mrOK <> TAskPasswordForm.AskPassword(AccountSettings.name + ' password', 'Enter account password:', AccountSettings.Password, AccountSettings.use_tc_password_manager, false, FindTCWindow) then
		begin //не указали пароль в диалоге
			exit(false); //отказались вводить пароль
		end else begin
			result := true;
			if AccountSettings.use_tc_password_manager then
			begin
				if FS_FILE_OK = self.SetPassword(AccountSettings.name, AccountSettings.Password) then
				begin //TC скушал пароль, запомним в инишник галочку
					LogHandleProc(LogLevelDebug, msgtype_details, PWideChar('Password saved in the TC password manager'));
					TmpString := AccountSettings.Password;
					AccountSettings.Password := EmptyWideStr;
					SetAccountSettingsToIniFile(AccountSettings);
					AccountSettings.Password := TmpString;
				end;
			end;
		end;
	end
	else
		result := true;
end;

function TTCPasswordManager.GetProxyPassword(var ProxySettings: TProxySettings): Boolean;
var
	TmpString: WideString;
begin
	result := false;
	if (ProxySettings.ProxyType = ProxyNone) or (ProxySettings.user = EmptyWideStr) then
		exit(true); //no username means no password required

	if ProxySettings.use_tc_password_manager and (self.GetPassword('proxy' + ProxySettings.user, ProxySettings.Password) = FS_FILE_OK) then //пароль должен браться из TC
		exit(true);

	//иначе предполагается, что пароль взят из конфига

	if ProxySettings.Password = EmptyWideStr then //но пароля нет, не в инишнике, не в тотале
	begin
		if mrOK <> TAskPasswordForm.AskPassword('User ' + ProxySettings.user + ' proxy password', 'Enter proxy password:', ProxySettings.Password, ProxySettings.use_tc_password_manager, false, FindTCWindow) then
		begin //не указали пароль в диалоге
			exit(false); //отказались вводить пароль
		end else begin
			result := true;
			if ProxySettings.use_tc_password_manager then
			begin
				if FS_FILE_OK = self.SetPassword('proxy' + ProxySettings.user, ProxySettings.Password) then
				begin //TC скушал пароль, запомним в инишник галочку
					LogHandleProc(LogLevelDebug, msgtype_details, PWideChar('Password saved in TC password manager'));
					TmpString := ProxySettings.Password;
					ProxySettings.Password := EmptyWideStr;
					ProxySettings.use_tc_password_manager := true; //чтобы не прокидывать сюда сохранение настроек прокси, галочка сохраняется в вызывающем коде
					ProxySettings.Password := TmpString;
				end; //Ошибки здесь не значат, что пароль мы не получили - он может быть введён в диалоге
			end;
		end;
	end;
end;

function TTCPasswordManager.InitCloudCryptPasswords(var AccountSettings: TAccountSettings): Boolean; //Вносит в AccountSettings пароли из стораджа/введённые руками
var
	crypt_id: WideString;
	StorePassword: Boolean;
begin
	result := true;
	StorePassword := false;
	crypt_id := AccountSettings.name + ' filecrypt';

	if EncryptModeAlways = AccountSettings.encrypt_files_mode then {password must be taken from tc storage, otherwise ask user and store password}
	begin
		case self.GetPassword(crypt_id, AccountSettings.crypt_files_password) of
			FS_FILE_OK:
				begin
					exit(true);
				end;
			FS_FILE_READERROR: //password not found in store => act like EncryptModeAskOnce
				begin
					AccountSettings.encrypt_files_mode := EncryptModeAskOnce;
				end;
			FS_FILE_NOTSUPPORTED: //user doesn't know master password
				begin
					exit(false);
				end;
		end;
	end;
	if EncryptModeAskOnce = AccountSettings.encrypt_files_mode then
	begin
		if mrOK <> TAskPasswordForm.AskPassword(AccountSettings.name + ' encryption password', 'Enter encryption password for current session:', AccountSettings.crypt_files_password, StorePassword, true, self.ParentWindow) then
			result := false
	end;
end;

function TTCPasswordManager.StoreFileCryptPassword(AccountName: WideString): WideString;
var
	CurrentPassword: WideString;
	crypt_id: WideString;
	Verb: WideString;
	StorePassword: Boolean;
begin
	StorePassword := true;
	result := EmptyWideStr;
	crypt_id := AccountName + ' filecrypt';
	case self.GetPassword(crypt_id, CurrentPassword) of
		FS_FILE_OK: //пользователь знает мастер-пароль, и пароль был сохранен
			begin
				Verb := 'Update';
			end;
		FS_FILE_READERROR: //Пользователь знает мастер-пароль, и пароль вводится впервые
			begin
				Verb := 'Set';
			end;
		else
			begin
				exit;
			end;
	end;
	if mrOK = TAskPasswordForm.AskPassword(Verb + ' encryption password', 'New password:', CurrentPassword, StorePassword, true, self.ParentWindow) then
	begin
		self.SetPassword(crypt_id, CurrentPassword);
		result := TFileCipher.CryptedGUID(CurrentPassword);
	end

end;

end.
