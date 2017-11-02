unit TCPasswordManagerHelper;

{Обертка над обращениями к менеджеру паролей Total Commander}
interface

Uses Plugin_Types, Settings, Windows, CloudMailRu, SysUtils, AskPassword, MRC_Helper, Controls;

type
	TTCPasswordManager = class
	private
		CryptHandleProc: TCryptHandler;
		LogHandleProc: TLogHandler;

	public
		constructor Create(CryptHandleProc: TCryptHandler; LogHandleProc: TLogHandler);
		destructor Destroy(); override;
		function GetPassword(Key: WideString; var Password: WideString): integer;
		function SetPassword(Key, Password: WideString): integer;
		{--------------------}
		function GetProxyPassword(var ProxySettings: TProxySettings): boolean;
		function GetAccountPassword(var AccountSettings: TAccountSettings): boolean;
		function InitCloudCryptPasswords(var Cloud: TCloudMailRu; AccountSettings: TAccountSettings): boolean;

	end;

implementation

{TTCPasswordManager}

constructor TTCPasswordManager.Create(CryptHandleProc: TCryptHandler; LogHandleProc: TLogHandler);
begin
	self.CryptHandleProc := CryptHandleProc;
	self.LogHandleProc := LogHandleProc;
end;

destructor TTCPasswordManager.Destroy;
begin
	inherited;
end;

function TTCPasswordManager.GetAccountPassword(var AccountSettings: TAccountSettings): boolean;
begin

end;

function TTCPasswordManager.GetPassword(Key: WideString; var Password: WideString): integer;
var
	buf: pwidechar;
begin
	GetMem(buf, 1024);
	ZeroMemory(buf, 1024);
	result := CryptHandleProc(FS_CRYPT_LOAD_PASSWORD_NO_UI, pwidechar(Key), buf, 1024);
	if FS_FILE_NOTFOUND = result then //no master password entered yet
	begin
		LogHandleProc(LogLevelDetail, msgtype_details, pwidechar('No master password entered yet'));
		ZeroMemory(buf, 1024);
		result := CryptHandleProc(FS_CRYPT_LOAD_PASSWORD, pwidechar(Key), buf, 1024); //ask with master password
	end;
	if FS_FILE_OK = result then //all ok, we got password
	begin
		Password := buf;
	end;
	if FS_FILE_READERROR = result then
	begin
		LogHandleProc(LogLevelError, msgtype_importanterror, pwidechar('CryptProc returns error: Password not found in password store'));
	end;
	if FS_FILE_NOTSUPPORTED = result then //master password cancelled
	begin
		LogHandleProc(LogLevelError, msgtype_importanterror, pwidechar('CryptProc returns error: Decrypt failed'));
	end;
	FreeMemory(buf);
end;

function TTCPasswordManager.SetPassword(Key, Password: WideString): integer;
begin
	result := CryptHandleProc(FS_CRYPT_SAVE_PASSWORD, pwidechar(Key), pwidechar(Password), SizeOf(Password));

	case result of
		FS_FILE_OK:
			begin //TC скушал пароль, запомним в инишник галочку
				if Assigned(LogHandleProc) then
					LogHandleProc(LogLevelDebug, msgtype_details, pwidechar(Key + ': password saved in TC password manager'));
			end;
		FS_FILE_NOTSUPPORTED: //Сохранение не получилось
			begin
				if Assigned(LogHandleProc) then
					LogHandleProc(LogLevelError, msgtype_importanterror, pwidechar(Key + ': CryptProc returns error: Encrypt failed'));
			end;
		FS_FILE_WRITEERROR: //Сохранение опять не получилось
			begin
				if Assigned(LogHandleProc) then
					LogHandleProc(LogLevelError, msgtype_importanterror, pwidechar(Key + ': password NOT saved: Could not write password to password store'));
			end;
		FS_FILE_NOTFOUND: //Не указан мастер-пароль
			begin
				if Assigned(LogHandleProc) then
					LogHandleProc(LogLevelError, msgtype_importanterror, pwidechar(Key + ': password NOT saved: No master password entered yet'));
			end;
		//Ошибки здесь не значат, что пароль мы не получили - он может быть введён в диалоге
	end;
end;

function TTCPasswordManager.GetProxyPassword(var ProxySettings: TProxySettings): boolean;
var
	CryptResult: integer;
	AskResult: integer;
	TmpString: WideString;
	buf: pwidechar;
begin
	if (ProxySettings.ProxyType = ProxyNone) or (ProxySettings.user = EmptyWideStr) then
		exit(true); //no username means no password required

	if ProxySettings.use_tc_password_manager then
	begin //пароль должен браться из TC
		CryptResult := self.GetPassword('proxy' + ProxySettings.user, ProxySettings.Password);
	end; //предполагается, что пароль взят из конфига

	if ProxySettings.Password = EmptyWideStr then //но пароля нет, не в инишнике, не в тотале
	begin
		AskResult := TAskPasswordForm.AskPassword(FindTCWindow, 'User ' + ProxySettings.user + ' proxy', ProxySettings.Password, ProxySettings.use_tc_password_manager, false);
		if AskResult <> mrOK then
		begin //не указали пароль в диалоге
			exit(false); //отказались вводить пароль
		end else begin
			if ProxySettings.use_tc_password_manager then
			begin
				if FS_FILE_OK = self.SetPassword('proxy' + ProxySettings.user, ProxySettings.Password) then
				begin //TC скушал пароль, запомним в инишник галочку
					LogHandleProc(LogLevelDebug, msgtype_details, pwidechar('Password saved in TC password manager'));
					TmpString := ProxySettings.Password;
					ProxySettings.Password := EmptyWideStr;
					ProxySettings.use_tc_password_manager := true; //Не забыть сохранить!
					ProxySettings.Password := TmpString;
				end; //Ошибки здесь не значат, что пароль мы не получили - он может быть введён в диалоге
			end;
			result := true;
		end;
	end
	else
		result := true; //пароль взят из инишника напрямую
end;

function TTCPasswordManager.InitCloudCryptPasswords(var Cloud: TCloudMailRu; AccountSettings: TAccountSettings): boolean;
begin

end;

end.
