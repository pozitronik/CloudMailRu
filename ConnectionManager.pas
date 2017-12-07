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
		UploadBPS: integer;
		DownloadBPS: integer;
		Timeout: integer;
		CloudMaxFileSize: integer;
		PrecalculateHash: boolean;
		CheckCRC: boolean;

		ProgressHandleProc: TProgressHandler;
		LogHandleProc: TLogHandler;
		RequestHandleProc: TRequestHandler;

		PasswordManager: TTCPasswordManager;

		function ConnectionExists(connectionName: WideString): integer; //проверяет существование подключение
		function new(connectionName: WideString): integer; //Добавляет подключение в пул

	public
		constructor Create(IniFileName: WideString; ProxySettings: TProxySettings; Timeout, CloudMaxFileSize, UploadBPS, DownloadBPS: integer; PrecalculateHash: boolean; CheckCRC: boolean; ProgressHandleProc: TProgressHandler; LogHandleProc: TLogHandler; RequestHandleProc: TRequestHandler; PasswordManager: TTCPasswordManager);
		destructor Destroy(); override;
		function get(connectionName: WideString; var OperationResult: integer; doInit: boolean = true): TCloudMailRu; //возвращает готовое подклчение по имени
		function set_(connectionName: WideString; cloud: TCloudMailRu): boolean;
		function init(connectionName: WideString; ProxySettings: TProxySettings; Timeout: integer): integer; //инициализирует подключение по его имени, возвращает код состояния
		function free(connectionName: WideString): integer; //освобождает подключение по его имени, возвращает код состояния
		function freeAll: integer; //освобождает все подключения
		function initialized(connectionName: WideString): boolean; //Проверяет, инициализировано ли подключение

	end;

implementation

{TConnectionManager}
constructor TConnectionManager.Create(IniFileName: WideString; ProxySettings: TProxySettings; Timeout, CloudMaxFileSize, UploadBPS, DownloadBPS: integer; PrecalculateHash: boolean; CheckCRC: boolean; ProgressHandleProc: TProgressHandler; LogHandleProc: TLogHandler; RequestHandleProc: TRequestHandler; PasswordManager: TTCPasswordManager);
begin
	SetLength(Connections, 0);
	self.IniFileName := IniFileName;

	self.ProgressHandleProc := ProgressHandleProc;
	self.LogHandleProc := LogHandleProc;
	self.RequestHandleProc := RequestHandleProc;
	self.Proxy := ProxySettings;
	self.Timeout := Timeout;
	self.CloudMaxFileSize := CloudMaxFileSize;
	self.UploadBPS := UploadBPS;
	self.DownloadBPS := DownloadBPS;
	self.PrecalculateHash := PrecalculateHash;
	self.CheckCRC := CheckCRC;

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
			OperationResult := init(connectionName, self.Proxy, self.Timeout);
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

function TConnectionManager.init(connectionName: WideString; ProxySettings: TProxySettings; Timeout: integer): integer;
var
	cloud: TCloudMailRu;
	AccountSettings: TAccountSettings;
	LoginMethod: integer;
begin
	result := CLOUD_OPERATION_OK;
	AccountSettings := GetAccountSettingsFromIniFile(IniFileName, connectionName);

	if not PasswordManager.GetAccountPassword(AccountSettings) then
		exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE

	if AccountSettings.encrypt_files_mode <> EncryptModeNone then
	begin
		if not PasswordManager.InitCloudCryptPasswords(AccountSettings) then
			exit(CLOUD_OPERATION_FAILED);
	end;

	LogHandleProc(LogLevelConnect, MSGTYPE_CONNECT, PWideChar('CONNECT \' + connectionName));

	cloud := TCloudMailRu.Create(AccountSettings, self.CloudMaxFileSize, self.Proxy, Timeout, self.UploadBPS, self.DownloadBPS, self.PrecalculateHash, self.CheckCRC, ProgressHandleProc, LogHandleProc, RequestHandleProc);
	if not set_(connectionName, cloud) then
		exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE

	if (AccountSettings.twostep_auth) then
		LoginMethod := CLOUD_AUTH_METHOD_TWO_STEP
	else
		LoginMethod := CLOUD_AUTH_METHOD_WEB;

	if not(get(connectionName, result, false).login(LoginMethod)) then
	begin
		result := CLOUD_OPERATION_FAILED;
		free(connectionName);
	end;

	//cloud.Destroy;
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
