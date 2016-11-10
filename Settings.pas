unit Settings;

interface

uses Classes, Windows, SysUtils, IniFiles, System.Variants;

const
	ProxyNone = 0;
	ProxySocks5 = 1;
	ProxySocks4 = 2;
	ProxyHTTP = 3;

	SocksProxyTypes = [ProxySocks5, ProxySocks4];

type

	TAccountSettings = record
		name, email, password: WideString;
		use_tc_password_manager: boolean;
		user, domain: WideString; // parsed values from email
		unlimited_filesize: boolean;
		split_large_files: boolean;
	end;

	TProxySettings = record
		ProxyType: Integer;
		Server: WideString;
		Port: Integer;
		user: WideString;
		password: WideString;
		use_tc_password_manager: boolean;
	end;

	TPluginSettings = record
		IniPath: Integer;
		LoadSSLDLLOnlyFromPluginDir: boolean;
		PreserveFileTime: boolean;
		DescriptionEnabled: boolean;
		OperationsViaPublicLinkEnabled: boolean;
		AskOnErrors: boolean;
		SocketTimeout: Integer;
		Proxy: TProxySettings;
	end;


function GetPluginSettings(IniFilePath: WideString): TPluginSettings;
procedure SetPluginSettings(IniFilePath: WideString; PluginSettings: TPluginSettings);
procedure SetPluginSettingsValue(IniFilePath: WideString; OptionName: WideString; OptionValue: Variant);

function GetAccountSettingsFromIniFile(IniFilePath: WideString; AccountName: WideString): TAccountSettings;
function SetAccountSettingsToIniFile(IniFilePath: WideString; AccountSettings: TAccountSettings): boolean;
procedure GetAccountsListFromIniFile(IniFilePath: WideString; var AccountsList: TStringList); // todo move this to accounts mb
procedure DeleteAccountFromIniFile(IniFilePath: WideString; AccountName: WideString);

implementation






function GetPluginSettings(IniFilePath: WideString): TPluginSettings;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	GetPluginSettings.IniPath := IniFile.ReadInteger('Main', 'IniPath', 0);
	GetPluginSettings.LoadSSLDLLOnlyFromPluginDir := IniFile.ReadBool('Main', 'LoadSSLDLLOnlyFromPluginDir', false);
	GetPluginSettings.PreserveFileTime := IniFile.ReadBool('Main', 'PreserveFileTime', false);
	GetPluginSettings.DescriptionEnabled := IniFile.ReadBool('Main', 'DescriptionEnabled', false);
	GetPluginSettings.OperationsViaPublicLinkEnabled := IniFile.ReadBool('Main', 'OperationsViaPublicLinkEnabled', false);
	GetPluginSettings.AskOnErrors := IniFile.ReadBool('Main', 'AskOnErrors', false);
	GetPluginSettings.SocketTimeout := IniFile.ReadInteger('Main', 'SocketTimeout', -1);
	GetPluginSettings.Proxy.ProxyType := IniFile.ReadInteger('Main', 'ProxyType', ProxyNone);
	GetPluginSettings.Proxy.Server := IniFile.ReadString('Main', 'ProxyServer', '');
	GetPluginSettings.Proxy.Port := IniFile.ReadInteger('Main', 'ProxyPort', 0);
	GetPluginSettings.Proxy.user := IniFile.ReadString('Main', 'ProxyUser', '');
	GetPluginSettings.Proxy.password := IniFile.ReadString('Main', 'ProxyPassword', '');
	GetPluginSettings.Proxy.use_tc_password_manager := IniFile.ReadBool('Main', 'TCPwdMngr', false);
	IniFile.Destroy;
end;

procedure SetPluginSettings(IniFilePath: WideString; PluginSettings: TPluginSettings); { Не используется }
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.WriteBool('Main', 'LoadSSLDLLOnlyFromPluginDir', PluginSettings.LoadSSLDLLOnlyFromPluginDir);
	IniFile.WriteBool('Main', 'PreserveFileTime', PluginSettings.PreserveFileTime);
	IniFile.Destroy;
end;

procedure SetPluginSettingsValue(IniFilePath: WideString; OptionName: WideString; OptionValue: Variant);
var
	IniFile: TIniFile;
	basicType: Integer;
begin
	basicType := VarType(OptionValue);
	try
		IniFile := TIniFile.Create(IniFilePath);
		case basicType of
			varInteger: IniFile.WriteInteger('Main', OptionName, OptionValue);
			varString, varUString: IniFile.WriteString('Main', OptionName, OptionValue);
			varBoolean: IniFile.WriteBool('Main', OptionName, OptionValue);
		end;
		IniFile.Destroy;
	except
		On E: EIniFileException do
		begin
			MessageBoxW(0, PWideChar(E.Message), 'INI file error', MB_ICONERROR + MB_OK);
			exit;
		end;
	end;

end;

function GetAccountSettingsFromIniFile(IniFilePath: WideString; AccountName: WideString): TAccountSettings;
var
	IniFile: TIniFile;
	AtPos: Integer;
begin
	IniFile := TIniFile.Create(IniFilePath);
	Result.name := AccountName;
	Result.email := IniFile.ReadString(Result.name, 'email', '');
	Result.password := IniFile.ReadString(Result.name, 'password', '');
	Result.use_tc_password_manager := IniFile.ReadBool(Result.name, 'tc_pwd_mngr', false);
	Result.unlimited_filesize := IniFile.ReadBool(Result.name, 'unlimited_filesize', false);
	Result.split_large_files := IniFile.ReadBool(Result.name, 'split_large_files', false);
	AtPos := AnsiPos('@', Result.email);
	if AtPos <> 0 then
	begin
		Result.user := Copy(Result.email, 0, AtPos - 1);
		Result.domain := Copy(Result.email, AtPos + 1, Length(Result.email) - Length(Result.user) + 1);
	end;
	IniFile.Destroy;
end;

function SetAccountSettingsToIniFile(IniFilePath: WideString; AccountSettings: TAccountSettings): boolean;
var
	IniFile: TIniFile;
begin
	Result := false;
	if AccountSettings.name <> '' then Result := true;
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.WriteString(AccountSettings.name, 'email', AccountSettings.email);
	IniFile.WriteString(AccountSettings.name, 'password', AccountSettings.password);
	IniFile.WriteBool(AccountSettings.name, 'tc_pwd_mngr', AccountSettings.use_tc_password_manager);
	IniFile.WriteBool(AccountSettings.name, 'unlimited_filesize', AccountSettings.unlimited_filesize);
	IniFile.WriteBool(AccountSettings.name, 'split_large_files', AccountSettings.split_large_files);
	IniFile.Destroy;
end;

procedure GetAccountsListFromIniFile(IniFilePath: WideString; var AccountsList: TStringList);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.ReadSections(AccountsList);
	IniFile.Destroy;
end;

procedure DeleteAccountFromIniFile(IniFilePath: WideString; AccountName: WideString);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.EraseSection(AccountName);
	IniFile.Destroy;
end;

end.
