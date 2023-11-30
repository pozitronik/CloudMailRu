unit ConnectionManager;

{Обеспечиваем управление множественными поключениями без необходимости постоянных переподключений. При первом обращении нужное подключение создаётся,
 при последующих - отдаются уже созданные.}

interface

uses
	CloudMailRu,
	CMRConstants,
	CMRStrings,
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
	FileCipher,
	TempPwdHelper;

type

	TConnectionManager = class //Можно сделать что-то вроде TDictionaryManager, и от него наследоваться.
	private
		Connections: TDictionary<WideString, TCloudMailRu>;
		HTTPManager: THTTPManager;

		Settings: TPluginSettings; //Сохраняем параметры плагина, чтобы проксировать параметры из них при инициализации конкретного облака

		Logger: TTCLogger;
		Progress: TTCProgress;
		Request: TTCRequest;

		PasswordManager: TTCPasswordManager;

		function Init(ConnectionName: WideString; out Cloud: TCloudMailRu): integer; //инициализирует подключение по его имени, возвращает код состояния
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
	UseTCPasswordManager: Boolean;
	Password: WideString;
	EncryptFilesMode: integer;
	FilePassword: WideString;

	AccountSettings: TNewAccountSettings;
begin
	Result := CLOUD_OPERATION_OK;

	{TODO: This block of code should be refactored to exclude the TAccountSettings entirely}
	if not GetAccountPassword(PasswordManager, ConnectionName, UseTCPasswordManager, Password) then
	begin
		exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE
		if UseTCPasswordManager then
			TNewAccountSettings.ClearPassword(self.Settings.AccountsIniFileName, ConnectionName);
	end;

	AccountSettings := TNewAccountSettings.Create(self.Settings.AccountsIniFileName, ConnectionName);
	AccountSettings.UseTCPasswordManager := UseTCPasswordManager;
	AccountSettings.Password := Password;

	PasswordActionRetry := false;
	if AccountSettings.EncryptFilesMode <> EncryptModeNone then
	begin
		EncryptFilesMode := AccountSettings.EncryptFilesMode;
		repeat //пока не будет разрешающего действия
			if not InitCloudCryptPasswords(PasswordManager, AccountSettings.Account, EncryptFilesMode, FilePassword) then
			begin
				AccountSettings.Free;
				exit(CLOUD_OPERATION_FAILED);
			end;
			AccountSettings.EncryptFilesMode := EncryptFilesMode;
			if not TFileCipher.CheckPasswordGUID(FilePassword, AccountSettings.CryptedGUIDFiles) then
			begin
				ActionsList := TDictionary<Int32, WideString>.Create;
				ActionsList.AddOrSetValue(mrYes, PROCEED_UPDATE);
				ActionsList.AddOrSetValue(mrNo, PROCEED_IGNORE);
				ActionsList.AddOrSetValue(mrRetry, PROCEED_RETYPE);
				case TAskPasswordForm.AskAction(PREFIX_ERR_PASSWORD_MATCH, ERR_PASSWORD_MATCH, ActionsList) of
					mrYes: //store and use updated password
						begin
							AccountSettings.CryptedGUIDFiles := TFileCipher.CryptedGUID(FilePassword);
							AccountSettings.SetSettingValue('CryptedGUID_files', AccountSettings.CryptedGUIDFiles);
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
		CryptFilesPassword := FilePassword;

	end;
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

	AccountSettings.Free;
end;

end.
