﻿unit TCPasswordManager;

{Обертка над обращениями к менеджеру паролей Total Commander}
interface

Uses
	Plugin_Types,
	Settings,
	Windows,
	SysUtils,
	AskPassword, {AskEncryptionPasswords,}
	TCHelper,
	Controls,
	FileCipher,
	WideStrUtils,
	System.Classes,
	CMRStrings,
	CMRConstants,
	SETTINGS_CONSTANTS,
	ProxySettings, //TODO: Класс не должен знать ничего о этой модели
	TCLogger; //TODO: Класс не должен знать ничего о этой модели

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
		{--------------------}
		{TODO: Need to refactor these methods as violating the single responsibilty principle}
		function GetAccountPassword(const Account: WideString; var UseTCPasswordManager: Boolean; out Password: WideString): Boolean;
		function GetProxyPassword(var ProxySettings: TProxySettings): Boolean;
		function InitCloudCryptPasswords(const Account: WideString; var EncryptFilesMode: integer; out Password: WideString): Boolean;
		function StoreFileCryptPassword(AccountName: WideString): WideString;

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

function TTCPasswordManager.GetAccountPassword(const Account: WideString; var UseTCPasswordManager: Boolean; out Password: WideString): Boolean;
begin
	if UseTCPasswordManager and (self.GetPassword(Account, Password) = FS_FILE_OK) then //пароль должен браться из TC
		exit(true);

	//иначе предполагается, что пароль взят из конфига

	if Password = EmptyWideStr then //но пароля нет, не в инишнике, не в тотале
	begin
		if mrOK <> TAskPasswordForm.AskPassword(Format(ASK_PASSWORD, [Account]), PREFIX_ASK_PASSWORD, Password, UseTCPasswordManager, false, FindTCWindow) then
		begin //не указали пароль в диалоге
			exit(false); //отказались вводить пароль
		end else begin
			result := true;
			if UseTCPasswordManager then
			begin
				if FS_FILE_OK = self.SetPassword(Account, Password) then
				begin //TC скушал пароль, запомним в инишник галочку
					Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PASSWORD_SAVED, [Account]);
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
		if mrOK <> TAskPasswordForm.AskPassword(Format(ASK_PROXY_PASSWORD, [ProxySettings.user]), PREFIX_ASK_PROXY_PASSWORD, ProxySettings.Password, ProxySettings.use_tc_password_manager, false, FindTCWindow) then
		begin //не указали пароль в диалоге
			exit(false); //отказались вводить пароль
		end else begin
			result := true;
			if ProxySettings.use_tc_password_manager then
			begin
				if FS_FILE_OK = self.SetPassword('proxy' + ProxySettings.user, ProxySettings.Password) then
				begin //TC скушал пароль, запомним в инишник галочку
					Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PASSWORD_SAVED, [ProxySettings.user]);
					TmpString := ProxySettings.Password;
					ProxySettings.Password := EmptyWideStr;
					ProxySettings.use_tc_password_manager := true; //чтобы не прокидывать сюда сохранение настроек прокси, галочка сохраняется в вызывающем коде
					ProxySettings.Password := TmpString;
				end; //Ошибки здесь не значат, что пароль мы не получили - он может быть введён в диалоге
			end;
		end;
	end;
end;

{Retrieves file encryption password from storage or user input}
function TTCPasswordManager.InitCloudCryptPasswords(const Account: WideString; var EncryptFilesMode: integer; out Password: WideString): Boolean;
var
	crypt_id: WideString;
	StorePassword: Boolean;
begin
	result := true;
	StorePassword := false;
	crypt_id := Account + ' filecrypt';

	if EncryptModeAlways = EncryptFilesMode then {password must be taken from tc storage, otherwise ask user and store password}
	begin
		case self.GetPassword(crypt_id, Password) of
			FS_FILE_OK:
				begin
					exit(true);
				end;
			FS_FILE_READERROR: //password not found in store => act like EncryptModeAskOnce
				begin
					EncryptFilesMode := EncryptModeAskOnce;
				end;
			FS_FILE_NOTSUPPORTED: //user doesn't know master password
				begin
					exit(false);
				end;
		end;
	end;
	if EncryptModeAskOnce = EncryptFilesMode then
	begin
		if mrOK <> TAskPasswordForm.AskPassword(Format(ASK_ENCRYPTION_PASSWORD, [Account]), PREFIX_ASK_ENCRYPTION_PASSWORD, Password, StorePassword, true, self.ParentWindow) then
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
				Verb := VERB_UPDATE;
			end;
		FS_FILE_READERROR: //Пользователь знает мастер-пароль, и пароль вводится впервые
			begin
				Verb := VERB_SET;
			end;
		else
			begin
				exit;
			end;
	end;
	if mrOK = TAskPasswordForm.AskPassword(Format(ASK_ENCRYPTION_PASSWORD, [Verb]), PREFIX_ASK_NEW_PASSWORD, CurrentPassword, StorePassword, true, self.ParentWindow) then
	begin
		self.SetPassword(crypt_id, CurrentPassword);
		result := TFileCipher.CryptedGUID(CurrentPassword);
	end

end;

end.
