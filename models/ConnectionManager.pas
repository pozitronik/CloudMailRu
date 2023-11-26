﻿unit ConnectionManager;

{Обеспечиваем управление множественными поключениями без необходимости постоянных переподключений. При первом обращении нужное подключение создаётся,
 при последующих - отдаются уже созданные.}

interface

uses
	CloudMailRu,
	CMRConstants,
	TCLogger,
	TCProgress,
	TCRequest,
	Windows,
	Vcl.Controls,
	SETTINGS_CONSTANTS,
	PLUGIN_TYPES,
	Settings,
	PluginSettings,
	CloudSettings,
	TCPasswordManager,
	HTTPManager,
	System.Generics.Collections,
	SysUtils,
	AskPassword,
	FileCipher;

type

	TConnectionManager = class //Можно сделать что-то вроде TDictionaryManager, и от него наследоваться.
	private
		Connections: TDictionary<WideString, TCloudMailRu>;
		HTTPManager: THTTPManager;
		IniFileName: WideString;
		Settings: TPluginSettings; //Сохраняем параметры плагина, чтобы проксировать параметры из них при инициализации конкретного облака

		Logger: TTCLogger;
		Progress: TTCProgress;
		Request: TTCRequest;

		PasswordManager: TTCPasswordManager;

		function init(connectionName: WideString; var Cloud: TCloudMailRu): integer; //инициализирует подключение по его имени, возвращает код состояния
	public
		constructor Create(IniFileName: WideString; Settings: TPluginSettings; HTTPManager: THTTPManager; Progress: TTCProgress; Logger: TTCLogger; Request: TTCRequest; PasswordManager: TTCPasswordManager);
		destructor Destroy(); override;
		function get(connectionName: WideString; var OperationResult: integer): TCloudMailRu; //возвращает готовое подклчение по имени
		procedure free(connectionName: WideString); //освобождает подключение по его имени, если оно существует
	end;

implementation

{TConnectionManager}
constructor TConnectionManager.Create(IniFileName: WideString; Settings: TPluginSettings; HTTPManager: THTTPManager; Progress: TTCProgress; Logger: TTCLogger; Request: TTCRequest; PasswordManager: TTCPasswordManager);
begin
	Connections := TDictionary<WideString, TCloudMailRu>.Create;
	self.IniFileName := IniFileName;
	self.Settings := Settings;
	self.Progress := Progress;
	self.Logger := Logger;
	self.Request := Request;
	self.HTTPManager := HTTPManager;
	self.PasswordManager := PasswordManager;
end;

destructor TConnectionManager.Destroy;
var
	Item: TPair<WideString, TCloudMailRu>;
begin
	for Item in Connections do
		Item.Value.Destroy;

	FreeAndNil(Connections);
	inherited;
end;

procedure TConnectionManager.free(connectionName: WideString);
begin
	if Connections.ContainsKey(connectionName) then
	begin
		Connections.Items[connectionName].free;
		Connections.Remove(connectionName);
	end;
end;

function TConnectionManager.get(connectionName: WideString; var OperationResult: integer): TCloudMailRu;
begin
	OperationResult := CLOUD_OPERATION_OK;
	if not Connections.TryGetValue(connectionName, Result) then
	begin
		OperationResult := init(connectionName, Result);
		if CLOUD_OPERATION_OK = OperationResult then
			Connections.AddOrSetValue(connectionName, Result)
		else
			Result := nil; {если подключиться не удалось, все функции облака будут возвращать негативный результат, но без AV}
	end;
end;

function TConnectionManager.init(connectionName: WideString; var Cloud: TCloudMailRu): integer;
var
	CloudSettings: TCloudSettings;
	LoginMethod: integer;
	ActionsList: TDictionary<Int32, WideString>;
	PasswordActionRetry: Boolean;
begin
	Result := CLOUD_OPERATION_OK;
	CloudSettings.AccountSettings := GetAccountSettingsFromIniFile(IniFileName, connectionName);

	if not PasswordManager.GetAccountPassword(CloudSettings.AccountSettings) then
		exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE

	PasswordActionRetry := false;
	if CloudSettings.AccountSettings.encrypt_files_mode <> EncryptModeNone then
	begin
		repeat //пока не будет разрешающего действия
			if not PasswordManager.InitCloudCryptPasswords(CloudSettings.AccountSettings) then
				exit(CLOUD_OPERATION_FAILED);
			if not TFileCipher.CheckPasswordGUID(CloudSettings.AccountSettings.crypt_files_password, CloudSettings.AccountSettings.CryptedGUID_files) then
			begin
				ActionsList := TDictionary<Int32, WideString>.Create;
				ActionsList.AddOrSetValue(mrYes, 'Update and proceed');
				ActionsList.AddOrSetValue(mrNo, 'Proceed without enctyption');
				ActionsList.AddOrSetValue(mrRetry, 'Retype password');
				case TAskPasswordForm.AskAction('Password doesn''t match!', 'It seems that the entered password does not match the password you previously specified. Password update may make previously encrypted files inaccessible.', ActionsList) of
					mrYes: //store and use updated password
						begin
							CloudSettings.AccountSettings.CryptedGUID_files := TFileCipher.CryptedGUID(CloudSettings.AccountSettings.crypt_files_password);
							SetAccountSettingsValue(IniFileName, connectionName, 'CryptedGUID_files', CloudSettings.AccountSettings.CryptedGUID_files);
						end;
					mrNo:
						begin
							//continue without password
						end;
					mrRetry:
						begin
							PasswordActionRetry := true;
						end;
				end;
				FreeAndNil(ActionsList);
			end;

		until not PasswordActionRetry;
	end;

	Logger.Log(LOG_LEVEL_CONNECT, MSGTYPE_CONNECT, 'CONNECT \%s', [connectionName]);

	{proxify plugin settings to cloud}
	CloudSettings.ConnectionSettings := self.Settings.ConnectionSettings;
	CloudSettings.PrecalculateHash := self.Settings.PrecalculateHash;
	CloudSettings.ForcePrecalculateSize := self.Settings.ForcePrecalculateSize;
	CloudSettings.CheckCRC := self.Settings.CheckCRC;
	CloudSettings.CloudMaxFileSize := self.Settings.CloudMaxFileSize;
	CloudSettings.OperationErrorMode := self.Settings.OperationErrorMode;
	CloudSettings.RetryAttempts := self.Settings.RetryAttempts;
	CloudSettings.AttemptWait := self.Settings.AttemptWait;

	Cloud := TCloudMailRu.Create(CloudSettings, HTTPManager, Progress, Logger, Request);

	if (CloudSettings.AccountSettings.twostep_auth) then
		LoginMethod := CLOUD_AUTH_METHOD_TWO_STEP
	else
		LoginMethod := CLOUD_AUTH_METHOD_WEB;

	if not(Cloud.login(LoginMethod)) then
	begin
		Result := CLOUD_OPERATION_FAILED;
		Cloud.free;
	end;
end;

end.
