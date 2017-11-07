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
		Proxy: TProxySettings;
		Timeout: Integer;
		CloudMaxFileSize: Integer;

		ProgressHandleProc: TProgressHandler;
		LogHandleProc: TLogHandler;
		RequestHandleProc: TRequestHandler;

		PasswordManager: TTCPasswordManager;

		function ConnectionExists(connectionName: WideString): Integer; //проверяет существование подключение
		function new(connectionName: WideString): Integer; //Добавляет подключение в пул

	public
		constructor Create(IniFileName: WideString; ProxySettings: TProxySettings; Timeout, CloudMaxFileSize: Integer; ProgressHandleProc: TProgressHandler; LogHandleProc: TLogHandler; RequestHandleProc: TRequestHandler; PasswordManager: TTCPasswordManager);
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
constructor TConnectionManager.Create(IniFileName: WideString; ProxySettings: TProxySettings; Timeout, CloudMaxFileSize: Integer; ProgressHandleProc: TProgressHandler; LogHandleProc: TLogHandler; RequestHandleProc: TRequestHandler; PasswordManager: TTCPasswordManager);
begin
	SetLength(Connections, 0);
	self.IniFileName := IniFileName;

	self.ProgressHandleProc := ProgressHandleProc;
	self.LogHandleProc := LogHandleProc;
	self.RequestHandleProc := RequestHandleProc;
	self.Proxy := ProxySettings;
	self.Timeout := Timeout;
	self.CloudMaxFileSize := CloudMaxFileSize;

	self.PasswordManager := PasswordManager;
end;

destructor TConnectionManager.Destroy;
begin
	freeAll();
	inherited;
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
		if not initialized(connectionName) then
			OperationResult := init(connectionName, self.Proxy, self.Timeout);
		if (OperationResult = CLOUD_OPERATION_OK) then
			result := get(connectionName, OperationResult, false);
	end;
	{если подключиться не удалось, все функции облака будут возвращать негативный результат, но без AV}
end;

function TConnectionManager.set_(connectionName: WideString; cloud: TCloudMailRu): boolean;
var
	ConnectionIndex: Integer;
begin
	ConnectionIndex := ConnectionExists(connectionName);
	if ConnectionIndex = -1 then
		exit(false);
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

	if not PasswordManager.GetAccountPassword(AccountSettings) then
		exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE

	LogHandleProc(LogLevelConnect, MSGTYPE_CONNECT, PWideChar('CONNECT \' + connectionName));

	cloud := TCloudMailRu.Create(AccountSettings, self.CloudMaxFileSize, self.Proxy, Timeout, ProgressHandleProc, LogHandleProc, RequestHandleProc);
	if not set_(connectionName, cloud) then
		exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE

	if (AccountSettings.twostep_auth) then
		LoginMethod := 1
	else
		LoginMethod := 0;

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
		if Connections[I].Name = connectionName then
			exit(I);
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

end.
