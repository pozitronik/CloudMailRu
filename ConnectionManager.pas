unit ConnectionManager;

{Обеспечиваем управление множественными поключениями без необходимости постоянных переподключений. При первом обращении нужное подключение создаётся,
 при последующих - отдаются уже созданные.}

interface

uses CloudMailRu, MRC_Helper, windows, Vcl.Controls, PLUGIN_Types, Settings, AskPassword;

type

	TNamedConnection = record
		Name: WideString;
		Connection: TCloudMailRu;
	end;

	TConnectionManager = class
	private
		Connections: array of TNamedConnection;
		IniFileName: WideString;
		Proxy: TProxySettings;
		Timeout: Integer;
		CloudMaxFileSize: Integer;

		PluginNum: Integer;

		MyProgressProc: TProgressProcW;
		MyLogProc: TLogProcW;
		MyRequestProc: TRequestProcW;

		function ConnectionExists(connectionName: WideString): Integer; //проверяет существование подключение
		function new(connectionName: WideString): Integer; //Добавляет подключение в пул
		function GetMyPasswordNow(var AccountSettings: TAccountSettings): boolean; //Получает пароль из файла, из тоталовского менеджера или запрашивает прямой ввод

	public
		CryptoNum: Integer;
		MyCryptProc: TCryptProcW;
		constructor Create(IniFileName: WideString; PluginNum: Integer; MyProgressProc: TProgressProcW; MyLogProc: TLogProcW; ProxySettings: TProxySettings; Timeout, CloudMaxFileSize: Integer; MyRequestProc: TRequestProcW);
		destructor Destroy(); override;
		function get(connectionName: WideString; var OperationResult: Integer; doInit: boolean = true): TCloudMailRu; //возвращает готовое подклчение по имени
		function set_(connectionName: WideString; cloud: TCloudMailRu): boolean;
		function init(connectionName: WideString; ProxySettings: TProxySettings; Timeout: Integer): Integer; //инициализирует подключение по его имени, возвращает код состояния
		function free(connectionName: WideString): Integer; //освобождает подключение по его имени, возвращает код состояния
		function freeAll: Integer; //освобождает все подключения
		function initialized(connectionName: WideString): boolean; //Проверяет, инициализировано ли подключение

	end;

implementation

{TConnectionManager}
constructor TConnectionManager.Create(IniFileName: WideString; PluginNum: Integer; MyProgressProc: TProgressProcW; MyLogProc: TLogProcW; ProxySettings: TProxySettings; Timeout, CloudMaxFileSize: Integer; MyRequestProc: TRequestProcW);
begin
	SetLength(Connections, 0);
	self.IniFileName := IniFileName;
	self.PluginNum := PluginNum;
	self.MyProgressProc := MyProgressProc;
	self.MyLogProc := MyLogProc;
	self.MyRequestProc := MyRequestProc;
	self.Proxy := ProxySettings;
	self.Timeout := Timeout;
	self.CloudMaxFileSize := CloudMaxFileSize;
end;

destructor TConnectionManager.Destroy;
begin
	freeAll();
end;

function TConnectionManager.get(connectionName: WideString; var OperationResult: Integer; doInit: boolean = true): TCloudMailRu;
var
	ConnectionIndex: Integer;
begin
	ConnectionIndex := ConnectionExists(connectionName);
	if ConnectionIndex <> -1 then
	begin
		result := Connections[ConnectionIndex].Connection;
	end else begin
		result := Connections[new(connectionName)].Connection;
	end;
	if (doInit) then
	begin
		OperationResult := CLOUD_OPERATION_OK;
		if not initialized(connectionName) then OperationResult := init(connectionName, self.Proxy, self.Timeout);
		if (OperationResult = CLOUD_OPERATION_OK) then result := get(connectionName, OperationResult, false);
	end;
	{если подключиться не удалось, все функции облака будут возвращать негативный результат, но без AV}
end;

function TConnectionManager.set_(connectionName: WideString; cloud: TCloudMailRu): boolean;
var
	ConnectionIndex: Integer;
begin
	ConnectionIndex := ConnectionExists(connectionName);
	if ConnectionIndex = -1 then exit(false);
	Connections[ConnectionIndex].Connection := cloud;
	result := true;
end;

function TConnectionManager.init(connectionName: WideString; ProxySettings: TProxySettings; Timeout: Integer): Integer;
var
	cloud: TCloudMailRu;
	AccountSettings: TAccountSettings;
	LoginMethod: Integer;
begin
	result := CLOUD_OPERATION_OK;
	AccountSettings := GetAccountSettingsFromIniFile(IniFileName, connectionName);

	if not GetMyPasswordNow(AccountSettings) then exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE

	MyLogProc(PluginNum, MSGTYPE_CONNECT, PWideChar('CONNECT \' + connectionName));

	cloud := TCloudMailRu.Create(AccountSettings, self.CloudMaxFileSize, self.Proxy, Timeout, MyProgressProc, PluginNum, MyLogProc, MyRequestProc);
	if not set_(connectionName, cloud) then exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE

	if (AccountSettings.twostep_auth) then LoginMethod := 1
	else LoginMethod := 0;

	if not(get(connectionName, result, false).login(LoginMethod)) then
	begin
		result := CLOUD_OPERATION_FAILED;
		free(connectionName);
	end;

	//cloud.Destroy;
end;

function TConnectionManager.initialized(connectionName: WideString): boolean;
var
	dump: Integer;
begin
	result := Assigned(get(connectionName, dump, false));
end;

function TConnectionManager.new(connectionName: WideString): Integer;
begin
	SetLength(Connections, Length(Connections) + 1);
	Connections[Length(Connections) - 1].Name := connectionName;
	result := Length(Connections) - 1;
end;

function TConnectionManager.ConnectionExists(connectionName: WideString): Integer;
var
	I: Integer;
begin
	result := -1;

	for I := 0 to Length(Connections) - 1 do
	begin
		if Connections[I].Name = connectionName then exit(I);
	end;
end;

function TConnectionManager.free(connectionName: WideString): Integer;
begin
	result := CLOUD_OPERATION_OK;
	get(connectionName, result, false).free;
	set_(connectionName, nil);
end;

function TConnectionManager.freeAll: Integer;
var
	I: Integer;
begin
	result := CLOUD_OPERATION_OK;

	for I := 0 to Length(Connections) - 1 do
	begin
		if initialized(Connections[I].Name) then
		begin
			Connections[I].Connection.free;
			set_(Connections[I].Name, nil);
		end;
	end;
	SetLength(Connections, 0);

end;

function TConnectionManager.GetMyPasswordNow(var AccountSettings: TAccountSettings): boolean;
var
	CryptResult: Integer;
	AskResult: Integer;
	TmpString: WideString;
	buf: PWideChar;
begin
	if AccountSettings.public_account then exit(true);

	if AccountSettings.use_tc_password_manager then
	begin //пароль должен браться из TC
		GetMem(buf, 1024);
		CryptResult := MyCryptProc(PluginNum, CryptoNum, FS_CRYPT_LOAD_PASSWORD_NO_UI, PWideChar(AccountSettings.Name), buf, 1024); //Пытаемся взять пароль по-тихому
		if CryptResult = FS_FILE_NOTFOUND then
		begin
			MyLogProc(PluginNum, msgtype_details, PWideChar('No master password entered yet'));
			CryptResult := MyCryptProc(PluginNum, CryptoNum, FS_CRYPT_LOAD_PASSWORD, PWideChar(AccountSettings.Name), buf, 1024);
		end;
		if CryptResult = FS_FILE_OK then //Успешно получили пароль
		begin
			AccountSettings.password := buf;
			//Result := true;
		end;
		if CryptResult = FS_FILE_NOTSUPPORTED then //пользователь отменил ввод главного пароля
		begin
			MyLogProc(PluginNum, msgtype_importanterror, PWideChar('CryptProc returns error: Decrypt failed'));
		end;
		if CryptResult = FS_FILE_READERROR then
		begin
			MyLogProc(PluginNum, msgtype_importanterror, PWideChar('CryptProc returns error: Password not found in password store'));
		end;
		FreeMemory(buf);
	end; //else // ничего не делаем, пароль уже должен быть в настройках (взят в открытом виде из инишника)

	if AccountSettings.password = '' then //но пароля нет, не в инишнике, не в тотале
	begin
		AskResult := TAskPasswordForm.AskPassword(FindTCWindow, AccountSettings.Name, AccountSettings.password, AccountSettings.use_tc_password_manager);
		if AskResult <> mrOK then
		begin //не указали пароль в диалоге
			exit(false); //отказались вводить пароль
		end else begin
			if AccountSettings.use_tc_password_manager then
			begin
				case MyCryptProc(PluginNum, CryptoNum, FS_CRYPT_SAVE_PASSWORD, PWideChar(AccountSettings.Name), PWideChar(AccountSettings.password), SizeOf(AccountSettings.password)) of
					FS_FILE_OK:
						begin //TC скушал пароль, запомним в инишник галочку
							MyLogProc(PluginNum, msgtype_details, PWideChar('Password saved in TC password manager'));
							TmpString := AccountSettings.password;
							AccountSettings.password := '';
							SetAccountSettingsToIniFile(IniFileName, AccountSettings);
							AccountSettings.password := TmpString;
						end;
					FS_FILE_NOTSUPPORTED: //Сохранение не получилось
						begin
							MyLogProc(PluginNum, msgtype_importanterror, PWideChar('CryptProc returns error: Encrypt failed'));
						end;
					FS_FILE_WRITEERROR: //Сохранение опять не получилось
						begin
							MyLogProc(PluginNum, msgtype_importanterror, PWideChar('Password NOT saved: Could not write password to password store'));
						end;
					FS_FILE_NOTFOUND: //Не указан мастер-пароль
						begin
							MyLogProc(PluginNum, msgtype_importanterror, PWideChar('Password NOT saved: No master password entered yet'));
						end;
					//Ошибки здесь не значат, что пароль мы не получили - он может быть введён в диалоге
				end;
			end;
			result := true;
		end;
	end
	else result := true; //пароль взят из инишника напрямую
end;

end.
