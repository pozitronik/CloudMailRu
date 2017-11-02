unit TCPasswordManagerHelper;

{Обертка над обращениями к менеджеру паролей Total Commander}
interface

Uses Plugin_Types, Settings, Windows;

type
	TTCPasswordManager = class
	private
		CryptHandleProc: TCryptHandler;
		LogHandleProc: TLogHandler;

	public
		constructor Create(CryptHandleProc: TCryptHandler; LogHandleProc: TLogHandler = nil);
		destructor Destroy(); override;
		function GetPassword(Key: WideString; var Password: WideString): integer;
		function SetPassword(Key, Password: WideString): integer;
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

function TTCPasswordManager.GetPassword(Key: WideString; var Password: WideString): integer;
var
	buf: PWideChar;
begin
	GetMem(buf, 1024);
	ZeroMemory(buf, 1024);
	result := CryptHandleProc(FS_CRYPT_LOAD_PASSWORD_NO_UI, PWideChar(Key), buf, 1024);
	case result of //Пытаемся взять пароль по-тихому
		FS_FILE_OK: //all ok, we got password
			begin
				Password := buf;
			end;
		FS_FILE_READERROR: //Password not found in password store, ask user for it
			begin
			end;
		FS_FILE_NOTFOUND: //no master password entered yet
			begin
				ZeroMemory(buf, 1024);
				result := CryptHandleProc(FS_CRYPT_LOAD_PASSWORD, PWideChar(Key), buf, 1024);
				case result of
					FS_FILE_OK: //all ok, we got password
						begin
							Password := buf;
						end;
					FS_FILE_READERROR: //Password not found in password store, ask user for it
						begin
						end
					else
						begin
							//something else happened log&exit
						end;
				end;
			end
		else
			begin
				//something else happened  log&exit
			end;
	end;
	FreeMemory(buf);
end;

function TTCPasswordManager.SetPassword(Key, Password: WideString): integer;
begin
	result := CryptHandleProc(FS_CRYPT_SAVE_PASSWORD, PWideChar(Key), PWideChar(Password), SizeOf(Password));

	case result of
		FS_FILE_OK:
			begin //TC скушал пароль, запомним в инишник галочку
				if Assigned(LogHandleProc) then
					LogHandleProc(LogLevelDebug, msgtype_details, PWideChar(Key + ': password saved in TC password manager'));
			end;
		FS_FILE_NOTSUPPORTED: //Сохранение не получилось
			begin
				if Assigned(LogHandleProc) then
					LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar(Key + ': CryptProc returns error: Encrypt failed'));
			end;
		FS_FILE_WRITEERROR: //Сохранение опять не получилось
			begin
				if Assigned(LogHandleProc) then
					LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar(Key + ': password NOT saved: Could not write password to password store'));
			end;
		FS_FILE_NOTFOUND: //Не указан мастер-пароль
			begin
				if Assigned(LogHandleProc) then
					LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar(Key + ': password NOT saved: No master password entered yet'));
			end;
		//Ошибки здесь не значат, что пароль мы не получили - он может быть введён в диалоге
	end;
end;

end.
