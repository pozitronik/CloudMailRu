unit ConnectionManager;

{Connection pool manager for cloud connections by account name.
	Creates connections on first access, returns existing connections on subsequent calls.
	Combines interface definition with implementation.}

interface

uses
	CloudMailRu,
	CMRConstants,
	LanguageStrings,
	TCHandler,
	TCLogger,
	TCProgress,
	TCRequest,
	AccountsManager,
	PluginSettingsManager,
	PasswordUIProvider,
	WindowsFileSystem,
	Windows,
	Vcl.Controls,
	SettingsConstants,
	WFXTypes,
	ProxySettings,
	AccountSettings,
	CloudSettings,
	TCPasswordManager,
	HTTPManager,
	CipherValidator,
	FileCipher,
	AuthStrategy,
	System.Generics.Collections,
	SysUtils;

type
	IConnectionManager = interface
		['{DE5905AB-A410-42C0-A517-C30EC573D7FD}']

		{Returns cloud connection for account, creating if needed.
			@param ConnectionName Account name to get connection for
			@param OperationResult Output status code (CLOUD_OPERATION_OK on success)
			@return TCloudMailRu instance or nil on failure}
		function Get(ConnectionName: WideString; var OperationResult: Integer): TCloudMailRu;

		{Releases connection for account if it exists.
			@param ConnectionName Account name to release connection for}
		procedure Free(ConnectionName: WideString);
	end;

	TConnectionManager = class(TInterfacedObject, IConnectionManager)
	private
		FConnections: TDictionary<WideString, TCloudMailRu>; {It is better to encapsulate the dictionary}
		FHTTPManager: IHTTPManager;
		FPluginSettingsManager: IPluginSettingsManager;
		FAccountsManager: IAccountsManager;
		FPasswordUI: IPasswordUIProvider;
		FCipherValidator: ICipherValidator;
		FFileSystem: IFileSystem;
		FTCHandler: ITCHandler;
		FAuthStrategyFactory: IAuthStrategyFactory;

		FLogger: ILogger;
		FProgress: IProgress;
		FRequest: IRequest;
		FPasswordManager: IPasswordManager;

		function Init(ConnectionName: WideString; out Cloud: TCloudMailRu): Integer; {Create a connection by its name, returns the status code}
		function GetAccountPassword(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
		function GetFilesPassword(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
		function GetProxyPassword(): Boolean;
		function InitCloudCryptPasswords(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
	public
		constructor Create(PluginSettingsManager: IPluginSettingsManager; AccountsManager: IAccountsManager; HTTPManager: IHTTPManager; PasswordUI: IPasswordUIProvider; CipherValidator: ICipherValidator; FileSystem: IFileSystem; Progress: IProgress; Logger: ILogger; Request: IRequest; PasswordManager: IPasswordManager; TCHandler: ITCHandler; AuthStrategyFactory: IAuthStrategyFactory);
		destructor Destroy(); override;
		function Get(ConnectionName: WideString; var OperationResult: Integer): TCloudMailRu; {Return the cloud connection by its name}
		procedure Free(ConnectionName: WideString); {Free a connection by its name, if present}
	end;

implementation

{TConnectionManager}
constructor TConnectionManager.Create(PluginSettingsManager: IPluginSettingsManager; AccountsManager: IAccountsManager; HTTPManager: IHTTPManager; PasswordUI: IPasswordUIProvider; CipherValidator: ICipherValidator; FileSystem: IFileSystem; Progress: IProgress; Logger: ILogger; Request: IRequest; PasswordManager: IPasswordManager; TCHandler: ITCHandler; AuthStrategyFactory: IAuthStrategyFactory);
begin
	FConnections := TDictionary<WideString, TCloudMailRu>.Create;
	FPluginSettingsManager := PluginSettingsManager;
	FAccountsManager := AccountsManager;
	FHTTPManager := HTTPManager;
	FPasswordUI := PasswordUI;
	FCipherValidator := CipherValidator;
	FFileSystem := FileSystem;
	FProgress := Progress;
	FLogger := Logger;
	FRequest := Request;
	FPasswordManager := PasswordManager;
	FTCHandler := TCHandler;
	FAuthStrategyFactory := AuthStrategyFactory;
end;

destructor TConnectionManager.Destroy;
var
	Item: TPair<WideString, TCloudMailRu>;
begin
	for Item in FConnections do
		Item.Value.Destroy;

	FreeAndNil(FConnections);

	{Release interface references}
	FHTTPManager := nil;
	FPluginSettingsManager := nil;
	FAccountsManager := nil;
	FPasswordUI := nil;
	FCipherValidator := nil;
	FAuthStrategyFactory := nil;

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
	AuthStrategy: IAuthStrategy;
	Cipher: ICipher;
	FileCipherInstance: TFileCipher;
begin
	Result := CLOUD_OPERATION_OK;
	Cipher := nil;

	{Create CloudSettings using factory method - combines plugin settings with account settings}
	CloudSettings := TCloudSettings.CreateFromSettings(FPluginSettingsManager.GetSettings, FAccountsManager.GetAccountSettings(ConnectionName));

	if not CloudSettings.AccountSettings.PublicAccount and (not GetAccountPassword(ConnectionName, CloudSettings) or not GetFilesPassword(ConnectionName, CloudSettings) or not GetProxyPassword) then
		exit(CLOUD_OPERATION_ERROR_STATUS_UNKNOWN); //INVALID_HANDLE_VALUE

	FLogger.Log(LOG_LEVEL_CONNECT, MSGTYPE_CONNECT, 'CONNECT \%s', [ConnectionName]);

	{Create cipher when encryption is enabled}
	if CloudSettings.AccountSettings.EncryptFilesMode <> EncryptModeNone then
	begin
		FileCipherInstance := TFileCipher.Create(CloudSettings.CryptFilesPassword, CloudSettings.AccountSettings.CryptedGUIDFiles, CloudSettings.AccountSettings.EncryptFilenames);
		if FileCipherInstance.IsWrongPassword then
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_WRONG_ENCRYPT_PASSWORD);
			FileCipherInstance.Free;
		end
		else
			Cipher := FileCipherInstance;
	end;

	{Create appropriate auth strategy via factory - enables DI and testability}
	AuthStrategy := FAuthStrategyFactory.CreateDefaultStrategy;

	Cloud := TCloudMailRu.Create(CloudSettings, FHTTPManager, AuthStrategy, FFileSystem, FLogger, FProgress, FRequest, FTCHandler, Cipher);

	if not Cloud.Login then
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
		if mrOk <> FPasswordUI.AskPassword(Format(ASK_ENCRYPTION_PASSWORD, [ConnectionName]), PREFIX_ASK_ENCRYPTION_PASSWORD, CloudSettings.CryptFilesPassword, StorePassword, True, FTCHandler.FindTCWindow) then
			Result := False
	end;
end;

{Retrieves the password for ConnectionName: from TC passwords storage, then from settings, and the from user input. Returns true if password retrieved, false otherwise.
	Note: the metod saves password to TC storage and removes it from config, if current option set for the account}
function TConnectionManager.GetAccountPassword(const ConnectionName: WideString; var CloudSettings: TCloudSettings): Boolean;
begin
	if CloudSettings.AccountSettings.UseTCPasswordManager and (FPasswordManager.GetPassword(ConnectionName, CloudSettings.AccountSettings.password) = FS_FILE_OK) then
		exit(True);

	if CloudSettings.AccountSettings.password = EmptyWideStr then
	begin
		if mrOk <> FPasswordUI.AskPassword(Format(ASK_PASSWORD, [ConnectionName]), PREFIX_ASK_PASSWORD, CloudSettings.AccountSettings.password, CloudSettings.AccountSettings.UseTCPasswordManager, False, FTCHandler.FindTCWindow) then
		begin
			exit(False);
		end else begin
			Result := True;
			if CloudSettings.AccountSettings.UseTCPasswordManager then
			begin
				if FS_FILE_OK = FPasswordManager.SetPassword(ConnectionName, CloudSettings.AccountSettings.password) then
				begin {Now the account password stored in TC, clear password from the ini file}
					FLogger.Log(LOG_LEVEL_DEBUG, msgtype_details, PASSWORD_SAVED, [ConnectionName]);
					FAccountsManager.SwitchPasswordStorage(ConnectionName);
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
begin
	Result := True;
	PasswordActionRetry := False;
	if CloudSettings.AccountSettings.EncryptFilesMode <> EncryptModeNone then
	begin
		repeat
			if not InitCloudCryptPasswords(ConnectionName, CloudSettings) then
				exit(False);
			if not FCipherValidator.CheckPasswordGUID(CloudSettings.CryptFilesPassword, CloudSettings.AccountSettings.CryptedGUIDFiles) then
			begin
				ActionsList := TDictionary<Int32, WideString>.Create;
				ActionsList.AddOrSetValue(mrYes, PROCEED_UPDATE);
				ActionsList.AddOrSetValue(mrNo, PROCEED_IGNORE);
				ActionsList.AddOrSetValue(mrRetry, PROCEED_RETYPE);
				case FPasswordUI.AskAction(PREFIX_ERR_PASSWORD_MATCH, ERR_PASSWORD_MATCH, ActionsList, FTCHandler.FindTCWindow) of
					mrYes: {store and use updated password}
						begin
							CloudSettings.AccountSettings.CryptedGUIDFiles := FCipherValidator.GetCryptedGUID(CloudSettings.CryptFilesPassword);
							FAccountsManager.SetCryptedGUID(ConnectionName, CloudSettings.AccountSettings.CryptedGUIDFiles);
						end;
					mrNo:
						begin
							{continue without password}
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
	ProxySettings: TProxySettings;
begin
	Result := False;
	ProxySettings := FHTTPManager.ConnectionSettings.ProxySettings;

	if (ProxySettings.ProxyType = ProxyNone) or (ProxySettings.User = EmptyWideStr) then
		exit(True); {No proxy or not password protected}

	if ProxySettings.UseTCPasswordManager and (FPasswordManager.GetPassword(PASSWORD_KEY_PROXY + ProxySettings.User, ProxySettings.password) = FS_FILE_OK) then
		Result := True {Password is retrieved and should be updated in the HTTPManager}
	else
	begin
		if ProxySettings.password = EmptyWideStr then
		begin
			if mrOk = FPasswordUI.AskPassword(Format(ASK_PROXY_PASSWORD, [ProxySettings.User]), PREFIX_ASK_PROXY_PASSWORD, ProxySettings.password, ProxySettings.UseTCPasswordManager, False, FTCHandler.FindTCWindow) then
			begin {get proxy password and parameters from the user input}
				if FS_FILE_OK = FPasswordManager.SetPassword(PASSWORD_KEY_PROXY + ProxySettings.User, ProxySettings.password) then
				begin {Now the proxy password stored in TC, clear password from the ini file}
					FLogger.Log(LOG_LEVEL_DEBUG, msgtype_details, PASSWORD_SAVED, [ProxySettings.User]);
					FPluginSettingsManager.SwitchProxyPasswordStorage;
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
