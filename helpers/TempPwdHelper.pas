unit TempPwdHelper;

{Temp model to help refactor password retrieval routines}
interface

uses
	PLUGIN_TYPES,
	SETTINGS_CONSTANTS,
	SysUtils,
	Controls,
	FileCipher,
	CMRConstants,
	CMRStrings,
	TCHelper,
	TCPasswordManager,
	CloudSettings,
	AskPassword,
	ProxySettings;

function GetProxyPassword(PasswordManager: TTCPasswordManager; var ProxySettings: TProxySettings): Boolean;
function StoreFileCryptPassword(PasswordManager: TTCPasswordManager; AccountName: WideString): WideString;

implementation


function GetProxyPassword(PasswordManager: TTCPasswordManager; var ProxySettings: TProxySettings): Boolean;
var
	TmpString: WideString;
begin
	result := false;
	if (ProxySettings.ProxyType = ProxyNone) or (ProxySettings.user = EmptyWideStr) then
		exit(true); //no username means no password required

	if ProxySettings.use_tc_password_manager and (PasswordManager.GetPassword('proxy' + ProxySettings.user, ProxySettings.Password) = FS_FILE_OK) then //пароль должен браться из TC
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
				if FS_FILE_OK = PasswordManager.SetPassword('proxy' + ProxySettings.user, ProxySettings.Password) then
				begin //TC скушал пароль, запомним в инишник галочку
					//					Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PASSWORD_SAVED, [ProxySettings.user]);
					TmpString := ProxySettings.Password;
					ProxySettings.Password := EmptyWideStr;
					ProxySettings.use_tc_password_manager := true; //чтобы не прокидывать сюда сохранение настроек прокси, галочка сохраняется в вызывающем коде
					ProxySettings.Password := TmpString;
				end; //Ошибки здесь не значат, что пароль мы не получили - он может быть введён в диалоге
			end;
		end;
	end;
end;


function StoreFileCryptPassword(PasswordManager: TTCPasswordManager; AccountName: WideString): WideString;
var
	CurrentPassword: WideString;
	crypt_id: WideString;
	Verb: WideString;
	StorePassword: Boolean;
begin
	StorePassword := true;
	result := EmptyWideStr;
	crypt_id := AccountName + ' filecrypt';
	case PasswordManager.GetPassword(crypt_id, CurrentPassword) of
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
	if mrOK = TAskPasswordForm.AskPassword(Format(ASK_ENCRYPTION_PASSWORD, [Verb]), PREFIX_ASK_NEW_PASSWORD, CurrentPassword, StorePassword, true, PasswordManager.ParentWindow) then
	begin
		PasswordManager.SetPassword(crypt_id, CurrentPassword);
		result := TFileCipher.CryptedGUID(CurrentPassword);
	end

end;

end.
