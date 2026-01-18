unit ConnectionManager;

{Обеспечиваем управление множественными поключениями без необходимости постоянных переподключений. При первом обращении нужное подключение создаётся,
 при последующих - отдаются уже созданные.}

interface

uses
	CloudMailRu,
	CMRConstants,
	LANGUAGE_STRINGS,
	TCHelper,
	ILoggerInterface,
	TCProgress,
	TCRequest,
	Windows,
	Vcl.Controls,
	SETTINGS_CONSTANTS,
	PLUGIN_TYPES,
	ProxySettings,
	AccountSettings,
	AccountsManager,
	PluginSettingsManager,
	PluginSettings,
	CloudSettings,
	TCPasswordManager,
	HTTPManager,
	System.Generics.Collections,
	SysUtils,
	AskPassword,
	FileCipher;

type

	TConnectionManager = class
	private
		FConnections: TDictionary<WideString, TCloudMailRu>; {It is better to encapsulate the dictionary}
		FHTTPManager: THTTPManager;
		FPluginSettings: TPluginSettings; {Required to proxify plugin parameters to cloud parametes, when initialized}

		FLogger: ILogger;
		FProgress: TTCProgress;
		FRequest: TTCRequest;
		FPasswordManager: TTCPasswordManager;

		function Init(ConnectionName: WideString; out Cloud: TCloudMailRu): Integer; {Create a connection by its name, returns the status code}
		function GetAccountPassword(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
		function GetFilesPassword(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
		function GetProxyPassword(): Boolean;
		function InitCloudCryptPasswords(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
	public
		constructor Create(PluginSettings: TPluginSettings; Progress: TTCProgress; Logger: ILogger; Request: TTCRequest; PasswordManager: TTCPasswordManager);
		destructor Destroy(); override;
		function Get(ConnectionName: WideString; var OperationResult: Integer): TCloudMailRu; {Return the cloud connection by its name}
		procedure Free(ConnectionName: WideString); {Free a connection by its name, if present}
	end;

implementation

{TConnectionManager}
constructor TConnectionManager.Create(PluginSettings: TPluginSettings; Progress: TTCProgress; Logger: ILogger; Request: TTCRequest; PasswordManager: TTCPasswordManager);
begin
	FConnections := TDictionary<WideString, TCloudMailRu>.Create;
	self.FPluginSettings := PluginSettings;
	self.FProgress := Progress;
	self.FLogger := Logger;
	self.FRequest := Request;
	self.FHTTPManager := THTTPManager.Create(PluginSettings.ConnectionSettings, Progress, Logger);
	self.FPasswordManager := PasswordManager;
end;

destructor TConnectionManager.Destroy;
var
	Item: TPair<WideString, TCloudMailRu>;
begin
	for Item in FConnections do
		Item.Value.Destroy;

	FreeAndNil(FConnections);

	self.FHTTPManager.Destroy;
	inherited;
end;

procedure TConnectionManager.Free(ConnectionName: WideString);
begin
	if FConnections.ContainsKey(ConnectionName) then
	begin
		FConnections.Items[ConnectionName].Free;
		FConnections.Remove(ConnectionName);
	end;
end;

function TConnectionManager.Get(ConnectionName: WideString; var OperationResult: Integer): TCloudMailRu;
begin
	OperationResult := CLOUD_OPERATION_OK;
	if not FConnections.TryGetValue(ConnectionName, Result) then
	begin
		OperationResult := Init(ConnectionName, Result);
		if CLOUD_OPERATION_OK = OperationResult then
			FConnections.AddOrSetValue(ConnectionName, Result)
		else
			Result := nil; {если подключиться не удалось, все функции облака будут возвращать негативный результат, но без AV}
	end;
end;

function TConnectionManager.Init(ConnectionName: WideString; out Cloud: TCloudMailRu): Integer;
var
	CloudSettings: TCloudSettings;
	LoginMethod: Integer;
	AccountsManager: TAccountsManager;
begin
	Result := CLOUD_OPERATION_OK;
	AccountsManager := TAccountsManager.Create(self.FPluginSettings.AccountsIniFilePath);
	try
		CloudSettings.AccountSettings := AccountsManager.GetAccountSettings(ConnectionName);
	finally
		AccountsManager.Free;
	end;
	with CloudSettings do
	begin
		{proxify plugin settings to the cloud settings}
		ConnectionSettings := self.FPluginSettings.ConnectionSettings;

		PrecalculateHash := self.FPluginSettings.PrecalculateHash;
		ForcePrecalculateSize := self.FPluginSettings.ForcePrecalculateSize;
		CheckCRC := self.FPluginSettings.CheckCRC;
		CloudMaxFileSize := self.FPluginSettings.CloudMaxFileSize;
		OperationErrorMode := self.FPluginSettings.OperationErrorMode;
		RetryAttempts := self.FPluginSettings.RetryAttempts;
		AttemptWait := self.FPluginSettings.AttemptWait;
	end;

	if not CloudSettings.AccountSettings.PublicAccount and (not GetAccountPassword(ConnectionName, CloudSettings) or not GetFilesPassword(ConnectionName, CloudSettings) or not GetProxyPassword) then
		exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE

	FLogger.Log(LOG_LEVEL_CONNECT, MSGTYPE_CONNECT, 'CONNECT \%s', [ConnectionName]);

	Cloud := TCloudMailRu.Create(CloudSettings, FHTTPManager, FProgress, FLogger, FRequest);

	{OAuth app password is the only supported auth method. Legacy methods are kept for backwards compatibility but are deprecated.}
	LoginMethod := CLOUD_AUTH_METHOD_OAUTH_APP;

	if not(Cloud.login(LoginMethod)) then
	begin
		Result := CLOUD_OPERATION_FAILED;
		Cloud.Free;
	end;
end;

{Depending on the account settings, initializes and retrieves the files encryption password.
 The password retrieves from the TC passwords storage or user input. Returns true if password retrieved, false otherwise.
 If file encryption is not enabled, immediately returns true.}
function TConnectionManager.InitCloudCryptPasswords(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
var
	crypt_id: WideString;
	StorePassword: Boolean;
begin
	Result := True;
	StorePassword := False;
	crypt_id := ConnectionName + PASSWORD_SUFFIX_FILECRYPT;

	if EncryptModeAlways = CloudSettings.AccountSettings.EncryptFilesMode then {password must be taken from tc storage, otherwise ask user and store password}
	begin
		case FPasswordManager.GetPassword(crypt_id, CloudSettings.CryptFilesPassword) of
			FS_FILE_OK:
				begin
					exit(True);
				end;
			FS_FILE_READERROR: //password not found in store => act like EncryptModeAskOnce
				begin
					CloudSettings.AccountSettings.EncryptFilesMode := EncryptModeAskOnce;
				end;
			FS_FILE_NOTSUPPORTED: //user doesn't know master password
				begin
					exit(False);
				end;
		end;
	end;
	if EncryptModeAskOnce = CloudSettings.AccountSettings.EncryptFilesMode then
	begin
		if mrOk <> TAskPasswordForm.AskPassword(Format(ASK_ENCRYPTION_PASSWORD, [ConnectionName]), PREFIX_ASK_ENCRYPTION_PASSWORD, CloudSettings.CryptFilesPassword, StorePassword, True, FPasswordManager.ParentWindow) then
			Result := False
	end;
end;

{Retrieves the password for ConnectionName: from TC passwords storage, then from settings, and the from user input. Returns true if password retrieved, false otherwise.
 Note: the metod saves password to TC storage and removes it from config, if current option set for the account}
function TConnectionManager.GetAccountPassword(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
var
	AccountsManager: TAccountsManager;
begin
	if CloudSettings.AccountSettings.UseTCPasswordManager and (FPasswordManager.GetPassword(ConnectionName, CloudSettings.AccountSettings.password) = FS_FILE_OK) then //пароль должен браться из TC
		exit(True);

	//иначе предполагается, что пароль взят из конфига
	if CloudSettings.AccountSettings.password = EmptyWideStr then //но пароля нет, не в инишнике, не в тотале
	begin
		if mrOk <> TAskPasswordForm.AskPassword(Format(ASK_PASSWORD, [ConnectionName]), PREFIX_ASK_PASSWORD, CloudSettings.AccountSettings.password, CloudSettings.AccountSettings.UseTCPasswordManager, False, FindTCWindow) then
		begin //не указали пароль в диалоге
			exit(False); //отказались вводить пароль
		end else begin
			Result := True;
			if CloudSettings.AccountSettings.UseTCPasswordManager then
			begin
				if FS_FILE_OK = FPasswordManager.SetPassword(ConnectionName, CloudSettings.AccountSettings.password) then
				begin //Now the account password stored in TC, clear password from the ini file
					FLogger.Log(LOG_LEVEL_DEBUG, msgtype_details, PASSWORD_SAVED, [ConnectionName]);
					AccountsManager := TAccountsManager.Create(self.FPluginSettings.AccountsIniFilePath);
					try
						AccountsManager.SwitchPasswordStorage(ConnectionName);
					finally
						AccountsManager.Free;
					end;
				end;
			end;
		end;
	end
	else
		Result := True;
end;

function TConnectionManager.GetFilesPassword(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
var
	PasswordActionRetry: Boolean;
	ActionsList: TDictionary<Int32, WideString>;
	AccountsManager: TAccountsManager;
begin
	Result := True;
	PasswordActionRetry := False;
	if CloudSettings.AccountSettings.EncryptFilesMode <> EncryptModeNone then
	begin
		repeat //пока не будет разрешающего действия
			if not InitCloudCryptPasswords(ConnectionName, CloudSettings) then
				exit(False);
			if not TFileCipher.CheckPasswordGUID(CloudSettings.CryptFilesPassword, CloudSettings.AccountSettings.CryptedGUIDFiles) then
			begin
				ActionsList := TDictionary<Int32, WideString>.Create;
				ActionsList.AddOrSetValue(mrYes, PROCEED_UPDATE);
				ActionsList.AddOrSetValue(mrNo, PROCEED_IGNORE);
				ActionsList.AddOrSetValue(mrRetry, PROCEED_RETYPE);
				case TAskPasswordForm.AskAction(PREFIX_ERR_PASSWORD_MATCH, ERR_PASSWORD_MATCH, ActionsList) of
					mrYes: //store and use updated password
						begin
							CloudSettings.AccountSettings.CryptedGUIDFiles := TFileCipher.GetCryptedGUID(CloudSettings.CryptFilesPassword);
							AccountsManager := TAccountsManager.Create(self.FPluginSettings.IniFilePath);
							try
								AccountsManager.SetCryptedGUID(ConnectionName, CloudSettings.AccountSettings.CryptedGUIDFiles);
							finally
								AccountsManager.Free;
							end;
						end;
					mrNo:
						begin
							//continue without password
						end;
					mrRetry:
						begin
							PasswordActionRetry := True;
						end;
				end;
				FreeAndNil(ActionsList);
			end;

		until not PasswordActionRetry;
	end;
end;

{Retrieves the proxy password, if required, from TC passwords storage, the settings file or user input. Returns true if password retrieved or not required, false otherwise.
 Note: the metod saves password to TC storage and removes it from config, if user chooses to do so}
function TConnectionManager.GetProxyPassword: Boolean;
var
	SettingsManager: TPluginSettingsManager;
	ProxySettings: TProxySettings;
begin
	Result := False;
	ProxySettings := FHTTPManager.ConnectionSettings.ProxySettings;

	if (ProxySettings.ProxyType = ProxyNone) or (ProxySettings.User = EmptyWideStr) then
		exit(True); {No proxy or not password protected}

	if ProxySettings.UseTCPasswordManager and (FPasswordManager.GetPassword(PASSWORD_KEY_PROXY + ProxySettings.User, ProxySettings.password) = FS_FILE_OK) then {retrieve the proxy password from TC passwords storage}
		Result := True{Password is retrieved and should be updated in th HTTPManager}
	else
	begin
		if ProxySettings.password = EmptyWideStr then {password can be retrieved previously or just read from config}
		begin
			if mrOk = TAskPasswordForm.AskPassword(Format(ASK_PROXY_PASSWORD, [ProxySettings.User]), PREFIX_ASK_PROXY_PASSWORD, ProxySettings.password, ProxySettings.UseTCPasswordManager, False, FindTCWindow) then
			begin {get proxy password and parameters from the user input}
				if FS_FILE_OK = FPasswordManager.SetPassword(PASSWORD_KEY_PROXY + ProxySettings.User, ProxySettings.password) then
				begin {Now the proxy password stored in TC, clear password from the ini file}
					FLogger.Log(LOG_LEVEL_DEBUG, msgtype_details, PASSWORD_SAVED, [ProxySettings.User]);
					SettingsManager := TPluginSettingsManager.Create();
					try
						SettingsManager.SwitchProxyPasswordStorage;
					finally
						SettingsManager.Free;
					end;
					Result := True;
				end else begin
					FLogger.Log(LOG_LEVEL_WARNING, msgtype_details, WARN_PROXY_PASSWORD_IGNORED);
					Result := False;
				end;
			end;
		end;
	end;

	if Result = True then {update proxy password in the httpmanager to not ask it again}
		FHTTPManager.ProxyPassword := ProxySettings.password;
end;

end.
