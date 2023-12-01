unit ConnectionManager;

{Обеспечиваем управление множественными поключениями без необходимости постоянных переподключений. При первом обращении нужное подключение создаётся,
 при последующих - отдаются уже созданные.}

interface

uses
	CloudMailRu,
	CMRConstants,
	CMRStrings,
	TCHelper,
	TCLogger,
	TCProgress,
	TCRequest,
	Windows,
	Vcl.Controls,
	SETTINGS_CONSTANTS,
	PLUGIN_TYPES,
	NewAccountSettings,
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
		Settings: TPluginSettings; //required to proxyfy plugin parameters to cloud parametes, when initialized

		Logger: TTCLogger;
		Progress: TTCProgress;
		Request: TTCRequest;
		PasswordManager: TTCPasswordManager;

		function Init(ConnectionName: WideString; out Cloud: TCloudMailRu): integer; //инициализирует подключение по его имени, возвращает код состояния
		function GetAccountPassword(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
		function InitCloudCryptPasswords(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
		//		function GetProxyPassword(var CloudSettings: TCloudSettings): Boolean;
	public
		constructor Create(Settings: TPluginSettings; HTTPManager: THTTPManager; Progress: TTCProgress; Logger: TTCLogger; Request: TTCRequest; PasswordManager: TTCPasswordManager);
		destructor Destroy(); override;
		function Get(ConnectionName: WideString; var OperationResult: integer): TCloudMailRu; //возвращает готовое подключение по имени
		procedure Free(ConnectionName: WideString); //освобождает подключение по его имени, если оно существует
	end;

implementation

{TConnectionManager}
constructor TConnectionManager.Create(Settings: TPluginSettings; HTTPManager: THTTPManager; Progress: TTCProgress; Logger: TTCLogger; Request: TTCRequest; PasswordManager: TTCPasswordManager);
begin
	Connections := TDictionary<WideString, TCloudMailRu>.Create;
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

procedure TConnectionManager.Free(ConnectionName: WideString);
begin
	if Connections.ContainsKey(ConnectionName) then
	begin
		Connections.Items[ConnectionName].Free;
		Connections.Remove(ConnectionName);
	end;
end;

function TConnectionManager.Get(ConnectionName: WideString; var OperationResult: integer): TCloudMailRu;
begin
	OperationResult := CLOUD_OPERATION_OK;
	if not Connections.TryGetValue(ConnectionName, Result) then
	begin
		OperationResult := Init(ConnectionName, Result);
		if CLOUD_OPERATION_OK = OperationResult then
			Connections.AddOrSetValue(ConnectionName, Result)
		else
			Result := nil; {если подключиться не удалось, все функции облака будут возвращать негативный результат, но без AV}
	end;
end;

function TConnectionManager.Init(ConnectionName: WideString; out Cloud: TCloudMailRu): integer;
var
	CloudSettings: TCloudSettings;
	LoginMethod: integer;
	ActionsList: TDictionary<Int32, WideString>;
	PasswordActionRetry: Boolean;
	AccountSettingsHandler: TNewAccountSettings;
	AccountSettings: TAccountSettings;
begin
	Result := CLOUD_OPERATION_OK;
	AccountSettingsHandler := TNewAccountSettings.Create(self.Settings.AccountsIniFileName);
	AccountSettings := AccountSettingsHandler.GetAccountSettings(ConnectionName);
	AccountSettingsHandler.Free;
	with CloudSettings do
	begin
		{proxify plugin settings to the cloud settings}
		ConnectionSettings := self.Settings.ConnectionSettings;
		PrecalculateHash := self.Settings.PrecalculateHash;
		ForcePrecalculateSize := self.Settings.ForcePrecalculateSize;
		CheckCRC := self.Settings.CheckCRC;
		CloudMaxFileSize := self.Settings.CloudMaxFileSize;
		OperationErrorMode := self.Settings.OperationErrorMode;
		RetryAttempts := self.Settings.RetryAttempts;
		AttemptWait := self.Settings.AttemptWait;
		{proxify account settings to the cloud settings}
		Email := AccountSettings.Email;
		Password := AccountSettings.Password;
		UseTCPasswordManager := AccountSettings.UseTCPasswordManager;
		TwostepAuth := AccountSettings.TwostepAuth;
		UnlimitedFilesize := AccountSettings.UnlimitedFilesize;
		SplitLargeFiles := AccountSettings.SplitLargeFiles;
		PublicAccount := AccountSettings.PublicAccount;
		PublicUrl := AccountSettings.PublicUrl;
		Description := AccountSettings.Description;
		EncryptFilesMode := AccountSettings.EncryptFilesMode;
		EncryptFilenames := AccountSettings.EncryptFilenames;
		ShardOverride := AccountSettings.ShardOverride;
		UploadUrlOverride := AccountSettings.UploadUrlOverride;
		CryptedGUIDFiles := AccountSettings.CryptedGUIDFiles;
	end;

	if not GetAccountPassword(ConnectionName, CloudSettings) then
	begin
		exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE
	end;

	PasswordActionRetry := false;
	if CloudSettings.EncryptFilesMode <> EncryptModeNone then
	begin
		repeat //пока не будет разрешающего действия
			if not InitCloudCryptPasswords(ConnectionName, CloudSettings) then
				exit(CLOUD_OPERATION_FAILED);
			if not TFileCipher.CheckPasswordGUID(CloudSettings.CryptFilesPassword, CloudSettings.CryptedGUIDFiles) then
			begin
				ActionsList := TDictionary<Int32, WideString>.Create;
				ActionsList.AddOrSetValue(mrYes, PROCEED_UPDATE);
				ActionsList.AddOrSetValue(mrNo, PROCEED_IGNORE);
				ActionsList.AddOrSetValue(mrRetry, PROCEED_RETYPE);
				case TAskPasswordForm.AskAction(PREFIX_ERR_PASSWORD_MATCH, ERR_PASSWORD_MATCH, ActionsList) of
					mrYes: //store and use updated password
						begin
							CloudSettings.CryptedGUIDFiles := TFileCipher.CryptedGUID(CloudSettings.CryptFilesPassword);
							AccountSettingsHandler := TNewAccountSettings.Create(self.Settings.AccountsIniFileName);
							AccountSettingsHandler.SetCryptedGUID(ConnectionName, CloudSettings.CryptedGUIDFiles);
							AccountSettingsHandler.Free;
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

	Logger.Log(LOG_LEVEL_CONNECT, MSGTYPE_CONNECT, 'CONNECT \%s', [ConnectionName]);

	Cloud := TCloudMailRu.Create(CloudSettings, HTTPManager, Progress, Logger, Request);

	if (CloudSettings.TwostepAuth) then
		LoginMethod := CLOUD_AUTH_METHOD_TWO_STEP
	else
		LoginMethod := CLOUD_AUTH_METHOD_WEB;

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
	Result := true;
	StorePassword := false;
	crypt_id := ConnectionName + ' filecrypt';

	if EncryptModeAlways = CloudSettings.EncryptFilesMode then {password must be taken from tc storage, otherwise ask user and store password}
	begin
		case PasswordManager.GetPassword(crypt_id, CloudSettings.CryptFilesPassword) of
			FS_FILE_OK:
				begin
					exit(true);
				end;
			FS_FILE_READERROR: //password not found in store => act like EncryptModeAskOnce
				begin
					CloudSettings.EncryptFilesMode := EncryptModeAskOnce;
				end;
			FS_FILE_NOTSUPPORTED: //user doesn't know master password
				begin
					exit(false);
				end;
		end;
	end;
	if EncryptModeAskOnce = CloudSettings.EncryptFilesMode then
	begin
		if mrOK <> TAskPasswordForm.AskPassword(Format(ASK_ENCRYPTION_PASSWORD, [ConnectionName]), PREFIX_ASK_ENCRYPTION_PASSWORD, CloudSettings.CryptFilesPassword, StorePassword, true, PasswordManager.ParentWindow) then
			Result := false
	end;
end;

{Retrieves the password for ConnectionName: from TC passwords storage, then from settings, and the from user input. Returns true if password retrieved, false otherwise.
 Note: the metod saves password to TC storage and removes it from config, if current option set for the account}
function TConnectionManager.GetAccountPassword(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
var
	AccountsManager: TNewAccountSettings;
begin
	if CloudSettings.UseTCPasswordManager and (PasswordManager.GetPassword(ConnectionName, CloudSettings.Password) = FS_FILE_OK) then //пароль должен браться из TC
		exit(true);

	//иначе предполагается, что пароль взят из конфига
	if CloudSettings.Password = EmptyWideStr then //но пароля нет, не в инишнике, не в тотале
	begin
		if mrOK <> TAskPasswordForm.AskPassword(Format(ASK_PASSWORD, [ConnectionName]), PREFIX_ASK_PASSWORD, CloudSettings.Password, CloudSettings.UseTCPasswordManager, false, FindTCWindow) then
		begin //не указали пароль в диалоге
			exit(false); //отказались вводить пароль
		end else begin
			Result := true;
			if CloudSettings.UseTCPasswordManager then
			begin
				if FS_FILE_OK = PasswordManager.SetPassword(ConnectionName, CloudSettings.Password) then
				begin //Now the account password stored in TC, clear password from the ini file
					Logger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PASSWORD_SAVED, [ConnectionName]);
					AccountsManager := TNewAccountSettings.Create(self.Settings.AccountsIniFileName);
					AccountsManager.ClearPassword(ConnectionName);
					AccountsManager.Free;
				end;
			end;
		end;
	end
	else
		Result := true;
end;

end.
