unit ProxyPasswordResolver;

{Resolves proxy password from TC store, config file, or user input.
	Extracts proxy password retrieval logic from ConnectionManager
	for better separation of concerns. Follows the same pattern as TAccountCredentialsProvider.}

interface

uses
	HTTPManager,
	PasswordManager,
	PasswordUIProvider,
	PluginSettingsManager,
	TCHandler,
	Logger;

type
	IProxyPasswordResolver = interface
		['{4B9C6D8E-0F2A-5B3C-9D4E-6F7A8B9C0D1E}']

		{Resolves proxy password from TC store, config file, or user input.
			Updates HTTPManager with resolved password. Migrates INI passwords to TC store when saved.
			@return True if password resolved or not required, False if user cancelled}
		function ResolvePassword: Boolean;
	end;

	TProxyPasswordResolver = class(TInterfacedObject, IProxyPasswordResolver)
	private
		FHTTPManager: IHTTPManager;
		FPasswordManager: IPasswordManager;
		FPasswordUI: IPasswordUIProvider;
		FPluginSettingsManager: IPluginSettingsManager;
		FTCHandler: ITCHandler;
		FLogger: ILogger;
	public
		constructor Create(HTTPManager: IHTTPManager; PasswordManager: IPasswordManager; PasswordUI: IPasswordUIProvider; PluginSettingsManager: IPluginSettingsManager; TCHandler: ITCHandler; Logger: ILogger);
		function ResolvePassword: Boolean;
	end;

	{Null implementation for testing - returns True (proxy not required)}
	TNullProxyPasswordResolver = class(TInterfacedObject, IProxyPasswordResolver)
	public
		function ResolvePassword: Boolean;
	end;

implementation

uses
	SysUtils,
	Vcl.Controls,
	WFXTypes,
	ProxySettings,
	CloudConstants,
	LanguageStrings,
	SettingsConstants;

constructor TProxyPasswordResolver.Create(HTTPManager: IHTTPManager; PasswordManager: IPasswordManager; PasswordUI: IPasswordUIProvider; PluginSettingsManager: IPluginSettingsManager; TCHandler: ITCHandler; Logger: ILogger);
begin
	inherited Create;
	FHTTPManager := HTTPManager;
	FPasswordManager := PasswordManager;
	FPasswordUI := PasswordUI;
	FPluginSettingsManager := PluginSettingsManager;
	FTCHandler := TCHandler;
	FLogger := Logger;
end;

{Retrieves the proxy password, if required, from TC passwords storage, the settings file or user input. Returns true if password retrieved or not required, false otherwise.
	Note: the metod saves password to TC storage and removes it from config, if user chooses to do so}
function TProxyPasswordResolver.ResolvePassword: Boolean;
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
		end else
			Result := True; {Password already available from INI settings}
	end;

	if Result = True then {update proxy password in the httpmanager to not ask it again}
		FHTTPManager.ProxyPassword := ProxySettings.password;
end;

{TNullProxyPasswordResolver}

function TNullProxyPasswordResolver.ResolvePassword: Boolean;
begin
	Result := True;
end;

end.
