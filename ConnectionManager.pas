unit ConnectionManager;

{Обеспечиваем управление множественными поключениями без необходимости постоянных переподключений. При первом обращении нужное подключение создаётся,
 при последующих - отдаются уже созданные.}

interface

uses CloudMailRu, CMLTypes, MRC_Helper, windows, Vcl.Controls, PLUGIN_Types, Settings, TCPasswordManagerHelper;

type

	TNamedConnection = record
		Name: WideString;
		Connection: TCloudMailRu;
	end;

	TConnectionManager = class
	private
		Connections: array of TNamedConnection;
		IniFileName: WideString;
		PluginSettings: TPluginSettings; //Сохраняем параметры плагина, чтобы проксировать параметры из них при инициализации конкретного облака

		ProgressHandleProc: TProgressHandler;
		LogHandleProc: TLogHandler;
		RequestHandleProc: TRequestHandler;

		PasswordManager: TTCPasswordManager;

		function ConnectionExists(connectionName: WideString): integer; //проверяет существование подключение
		function new(connectionName: WideString): integer; //Добавляет подключение в пул

	public
		constructor Create(IniFileName: WideString; PluginSettings: TPluginSettings; ProgressHandleProc: TProgressHandler; LogHandleProc: TLogHandler; RequestHandleProc: TRequestHandler; PasswordManager: TTCPasswordManager);
		destructor Destroy(); override;
		function get(connectionName: WideString; var OperationResult: integer; doInit: boolean = true): TCloudMailRu; //возвращает готовое подклчение по имени
		function set_(connectionName: WideString; cloud: TCloudMailRu): boolean;
		function init(connectionName: WideString): integer; //инициализирует подключение по его имени, возвращает код состояния
		function free(connectionName: WideString): integer; //освобождает подключение по его имени, возвращает код состояния
		function freeAll: integer; //освобождает все подключения
		function initialized(connectionName: WideString): boolean; //Проверяет, инициализировано ли подключение

	end;

implementation

{TConnectionManager}
constructor TConnectionManager.Create(IniFileName: WideString; PluginSettings: TPluginSettings; ProgressHandleProc: TProgressHandler; LogHandleProc: TLogHandler; RequestHandleProc: TRequestHandler; PasswordManager: TTCPasswordManager);
begin
	SetLength(Connections, 0);
	self.IniFileName := IniFileName;

	self.ProgressHandleProc := ProgressHandleProc;
	self.LogHandleProc := LogHandleProc;
	self.RequestHandleProc := RequestHandleProc;

	self.PasswordManager := PasswordManager;
end;

destructor TConnectionManager.Destroy;
begin
	freeAll();
	inherited;
end;

function TConnectionManager.get(connectionName: WideString; var OperationResult: integer; doInit: boolean = true): TCloudMailRu;
var
	ConnectionIndex: integer;
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
		if not initialized(connectionName) then
			OperationResult := init(connectionName);
		if (OperationResult = CLOUD_OPERATION_OK) then
			result := get(connectionName, OperationResult, false);
	end;
	{если подключиться не удалось, все функции облака будут возвращать негативный результат, но без AV}
end;

function TConnectionManager.set_(connectionName: WideString; cloud: TCloudMailRu): boolean;
var
	ConnectionIndex: integer;
begin
	ConnectionIndex := ConnectionExists(connectionName);
	if ConnectionIndex = -1 then
		exit(false);
	Connections[ConnectionIndex].Connection := cloud;
	result := true;
end;

function TConnectionManager.init(connectionName: WideString): integer;
var
	cloud: TCloudMailRu;
	CloudSettings: TCloudSettings;
	LoginMethod: integer;
begin
	result := CLOUD_OPERATION_OK;
	CloudSettings.AccountSettings := GetAccountSettingsFromIniFile(IniFileName, connectionName);

	if not PasswordManager.GetAccountPassword(CloudSettings.AccountSettings) then
		exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE

	if CloudSettings.AccountSettings.encrypt_files_mode <> EncryptModeNone then
	begin
		if not PasswordManager.InitCloudCryptPasswords(CloudSettings.AccountSettings) then
			exit(CLOUD_OPERATION_FAILED);
	end;

	LogHandleProc(LogLevelConnect, MSGTYPE_CONNECT, PWideChar('CONNECT \' + connectionName));

	{proxify plugin settings to cloud}
	CloudSettings.ConnectionSettings := self.PluginSettings.ConnectionSettings;
	CloudSettings.PrecalculateHash := self.PluginSettings.PrecalculateHash;
	CloudSettings.CheckCRC := self.PluginSettings.CheckCRC;
	CloudSettings.CloudMaxFileSize := self.PluginSettings.CloudMaxFileSize;

	cloud := TCloudMailRu.Create(CloudSettings, ProgressHandleProc, LogHandleProc, RequestHandleProc);
	if not set_(connectionName, cloud) then
		exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE

	if (CloudSettings.AccountSettings.twostep_auth) then
		LoginMethod := CLOUD_AUTH_METHOD_TWO_STEP
	else
		LoginMethod := CLOUD_AUTH_METHOD_WEB;

	if not(get(connectionName, result, false).login(LoginMethod)) then
	begin
		result := CLOUD_OPERATION_FAILED;
		free(connectionName);
	end;
end;

function TConnectionManager.initialized(connectionName: WideString): boolean;
var
	dump: integer;
begin
	result := Assigned(get(connectionName, dump, false));
end;

function TConnectionManager.new(connectionName: WideString): integer;
begin
	SetLength(Connections, Length(Connections) + 1);
	Connections[Length(Connections) - 1].Name := connectionName;
	result := Length(Connections) - 1;
end;

function TConnectionManager.ConnectionExists(connectionName: WideString): integer;
var
	I: integer;
begin
	result := -1;

	for I := 0 to Length(Connections) - 1 do
	begin
		if Connections[I].Name = connectionName then
			exit(I);
	end;
end;

function TConnectionManager.free(connectionName: WideString): integer;
begin
	result := CLOUD_OPERATION_OK;
	get(connectionName, result, false).free;
	set_(connectionName, nil);
end;

function TConnectionManager.freeAll: integer;
var
	I: integer;
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

end.
