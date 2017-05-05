unit Settings;

interface

uses Classes, Windows, SysUtils, IniFiles, System.Variants, Plugin_Types, AskPassword, MRC_Helper, VCL.Controls;

const
	ProxyNone = 0;
	ProxySocks5 = 1;
	ProxySocks4 = 2;
	ProxyHTTP = 3;

	SocksProxyTypes = [ProxySocks5, ProxySocks4];

	CLOUD_MAX_FILESIZE_DEFAULT = 2147483392; //$80000000-256

	ChunkOverwrite = 0;
	ChunkOverwriteIgnore = 1;
	ChunkOverwriteAbort = 2;

	DeleteFailOnUploadAsk = 0;
	DeleteFailOnUploadIgnore = 1;
	DeleteFailOnUploadAbort = 2;
	DeleteFailOnUploadDeleteIgnore = 3;
	DeleteFailOnUploadDeleteAbort = 4;

	OverwriteLocalModeAsk = 0; //default
	OverwriteLocalModeIgnore = 1;
	OverwriteLocalModeOverwrite = 2;

	OperationErrorModeAsk = 0;
	OperationErrorModeIgnore = 1;
	OperationErrorModeAbort = 2;
	OperationErrorModeRetry = 3;

	IconsModeDisabled = 0;
	IconsModeInternal = 1;
	IconsModeInternalOverlay = 2;
	IconsModeExternal = 3;
	IconsModeExternalOverlay = 4;

type

	TAccountSettings = record
		name, email, password: WideString;
		use_tc_password_manager, twostep_auth: boolean;
		user, domain: WideString; //parsed values from email
		unlimited_filesize: boolean;
		split_large_files: boolean;
		public_account: boolean;
		public_url: WideString;
		description: WideString;
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
		SocketTimeout: Integer;
		Proxy: TProxySettings;
		CloudMaxFileSize: Integer;
		ChunkOverwriteMode: Integer;
		DeleteFailOnUploadMode: Integer;
		OperationErrorMode: Integer;
		RetryAttempts: Integer;
		AttemptWait: Integer;
		OverwriteLocalMode: Integer;
		DisableMultiThreading: boolean;
		LogUserSpace: boolean;
		IconsMode: Integer;
		DownloadLinksEncode: boolean;
		AutoUpdateDownloadListing: boolean;
		ShowTrashFolders: boolean;
		ShowSharedFolders: boolean;
		ShowInvitesFolders: boolean;
	end;

function GetProxyPasswordNow(var ProxySettings: TProxySettings; MyLogProc: TLogProcW; MyCryptProc: TCryptProcW; PluginNum: Integer; CryptoNum: Integer): boolean;
function GetPluginSettings(IniFilePath: WideString): TPluginSettings;
procedure SetPluginSettings(IniFilePath: WideString; PluginSettings: TPluginSettings);
procedure SetPluginSettingsValue(IniFilePath: WideString; OptionName: WideString; OptionValue: Variant);
function GetAccountSettingsFromIniFile(IniFilePath: WideString; AccountName: WideString): TAccountSettings;
function SetAccountSettingsToIniFile(IniFilePath: WideString; AccountSettings: TAccountSettings): boolean;
procedure GetAccountsListFromIniFile(IniFilePath: WideString; var AccountsList: TStringList);
procedure DeleteAccountFromIniFile(IniFilePath: WideString; AccountName: WideString);
procedure AddVirtualAccountsToAccountsList(AccountsIniFilePath: WideString; var AccountsList: TStringList; VirtualAccountsEnabled: TArray<boolean>);

implementation

function GetProxyPasswordNow(var ProxySettings: TProxySettings; MyLogProc: TLogProcW; MyCryptProc: TCryptProcW; PluginNum: Integer; CryptoNum: Integer): boolean;
var
	CryptResult: Integer;
	AskResult: Integer;
	TmpString: WideString;
	buf: PWideChar;
begin
	if (ProxySettings.ProxyType = ProxyNone) or (ProxySettings.user = '') then exit(true); //no username means no password required

	if ProxySettings.use_tc_password_manager then
	begin //пароль должен браться из TC
		GetMem(buf, 1024);
		CryptResult := MyCryptProc(PluginNum, CryptoNum, FS_CRYPT_LOAD_PASSWORD_NO_UI, PWideChar('proxy' + ProxySettings.user), buf, 1024); //Пытаемся взять пароль по-тихому
		if CryptResult = FS_FILE_NOTFOUND then
		begin
			MyLogProc(PluginNum, msgtype_details, PWideChar('No master password entered yet'));
			CryptResult := MyCryptProc(PluginNum, CryptoNum, FS_CRYPT_LOAD_PASSWORD, PWideChar('proxy' + ProxySettings.user), buf, 1024);
		end;
		if CryptResult = FS_FILE_OK then //Успешно получили пароль
		begin
			ProxySettings.password := buf;
			//Result := true;
		end;
		if CryptResult = FS_FILE_NOTSUPPORTED then //пользователь отменил ввод главного пароля
		begin
			MyLogProc(PluginNum, msgtype_importanterror, PWideChar('CryptProc returns error: Decrypt failed'));
		end;
		if CryptResult = FS_FILE_READERROR then
		begin
			MyLogProc(PluginNum, msgtype_importanterror, PWideChar('CryptProc returns error: Password not found in password store'));
		end;
		FreeMemory(buf);
	end; //else // ничего не делаем, пароль уже должен быть в настройках (взят в открытом виде из инишника)

	if ProxySettings.password = '' then //но пароля нет, не в инишнике, не в тотале
	begin
		AskResult := TAskPasswordForm.AskPassword(FindTCWindow, 'User ' + ProxySettings.user + ' proxy', ProxySettings.password, ProxySettings.use_tc_password_manager);
		if AskResult <> mrOK then
		begin //не указали пароль в диалоге
			exit(false); //отказались вводить пароль
		end else begin
			if ProxySettings.use_tc_password_manager then
			begin
				case MyCryptProc(PluginNum, CryptoNum, FS_CRYPT_SAVE_PASSWORD, PWideChar('proxy' + ProxySettings.user), PWideChar(ProxySettings.password), SizeOf(ProxySettings.password)) of
					FS_FILE_OK:
						begin //TC скушал пароль, запомним в инишник галочку
							MyLogProc(PluginNum, msgtype_details, PWideChar('Password saved in TC password manager'));
							TmpString := ProxySettings.password;
							ProxySettings.password := '';
							ProxySettings.use_tc_password_manager := true; //Не забыть сохранить!
							ProxySettings.password := TmpString;
						end;
					FS_FILE_NOTSUPPORTED: //Сохранение не получилось
						begin
							MyLogProc(PluginNum, msgtype_importanterror, PWideChar('CryptProc returns error: Encrypt failed'));
						end;
					FS_FILE_WRITEERROR: //Сохранение опять не получилось
						begin
							MyLogProc(PluginNum, msgtype_importanterror, PWideChar('Password NOT saved: Could not write password to password store'));
						end;
					FS_FILE_NOTFOUND: //Не указан мастер-пароль
						begin
							MyLogProc(PluginNum, msgtype_importanterror, PWideChar('Password NOT saved: No master password entered yet'));
						end;
					//Ошибки здесь не значат, что пароль мы не получили - он может быть введён в диалоге
				end;
			end;
			result := true;
		end;
	end
	else result := true; //пароль взят из инишника напрямую
end;

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
	GetPluginSettings.DisableMultiThreading := IniFile.ReadBool('Main', 'DisableMultiThreading', false);
	GetPluginSettings.LogUserSpace := IniFile.ReadBool('Main', 'LogUserSpace', true);
	GetPluginSettings.IconsMode := IniFile.ReadInteger('Main', 'IconsMode', 0);
	GetPluginSettings.SocketTimeout := IniFile.ReadInteger('Main', 'SocketTimeout', -1);
	GetPluginSettings.CloudMaxFileSize := IniFile.ReadInteger('Main', 'CloudMaxFileSize', CLOUD_MAX_FILESIZE_DEFAULT);
	GetPluginSettings.ChunkOverwriteMode := IniFile.ReadInteger('Main', 'ChunkOverwriteMode', 0);
	GetPluginSettings.DeleteFailOnUploadMode := IniFile.ReadInteger('Main', 'DeleteFailOnUploadMode', 0);
	GetPluginSettings.OverwriteLocalMode := IniFile.ReadInteger('Main', 'OverwriteLocalMode', 0);
	GetPluginSettings.OperationErrorMode := IniFile.ReadInteger('Main', 'OperationErrorMode', 0);
	GetPluginSettings.RetryAttempts := IniFile.ReadInteger('Main', 'RetryAttempts', 1);
	GetPluginSettings.AttemptWait := IniFile.ReadInteger('Main', 'AttemptWait', 1000);
	GetPluginSettings.Proxy.ProxyType := IniFile.ReadInteger('Main', 'ProxyType', ProxyNone);
	GetPluginSettings.Proxy.Server := IniFile.ReadString('Main', 'ProxyServer', '');
	GetPluginSettings.Proxy.Port := IniFile.ReadInteger('Main', 'ProxyPort', 0);
	GetPluginSettings.Proxy.user := IniFile.ReadString('Main', 'ProxyUser', '');
	GetPluginSettings.Proxy.use_tc_password_manager := IniFile.ReadBool('Main', 'ProxyTCPwdMngr', false);
	GetPluginSettings.Proxy.password := IniFile.ReadString('Main', 'ProxyPassword', '');
	GetPluginSettings.DownloadLinksEncode := IniFile.ReadBool('Main', 'DownloadLinksEncode', true);
	GetPluginSettings.AutoUpdateDownloadListing := IniFile.ReadBool('Main', 'AutoUpdateDownloadListing', true);
	GetPluginSettings.ShowTrashFolders := IniFile.ReadBool('Main', 'ShowTrashFolders', true);
	GetPluginSettings.ShowSharedFolders := IniFile.ReadBool('Main', 'ShowSharedFolders', true);
	GetPluginSettings.ShowInvitesFolders := IniFile.ReadBool('Main', 'ShowInvitesFolders', true);
	IniFile.Destroy;
end;

procedure SetPluginSettings(IniFilePath: WideString; PluginSettings: TPluginSettings); {Не используется}
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
	IniFile := TIniFile.Create(IniFilePath);

	basicType := VarType(OptionValue);
	try
		case basicType of
			varNull: IniFile.DeleteKey('Main', OptionName); //remove value in that case
			varInteger: IniFile.WriteInteger('Main', OptionName, OptionValue);
			varString, varUString: IniFile.WriteString('Main', OptionName, OptionValue);
			varBoolean: IniFile.WriteBool('Main', OptionName, OptionValue);
		end;
	except
		On E: EIniFileException do
		begin
			MessageBoxW(0, PWideChar(E.Message), 'INI file error', MB_ICONERROR + MB_OK);
			IniFile.Destroy;
			exit;
		end;
	end;
	IniFile.Destroy;
end;

function GetAccountSettingsFromIniFile(IniFilePath: WideString; AccountName: WideString): TAccountSettings;
var
	IniFile: TIniFile;
	AtPos: Integer;
begin
	IniFile := TIniFile.Create(IniFilePath);
	result.name := AccountName;
	result.email := IniFile.ReadString(result.name, 'email', '');
	result.password := IniFile.ReadString(result.name, 'password', '');
	result.use_tc_password_manager := IniFile.ReadBool(result.name, 'tc_pwd_mngr', false);
	result.unlimited_filesize := IniFile.ReadBool(result.name, 'unlimited_filesize', false);
	result.split_large_files := IniFile.ReadBool(result.name, 'split_large_files', false);
	result.twostep_auth := IniFile.ReadBool(result.name, 'twostep_auth', false);
	result.public_account := IniFile.ReadBool(result.name, 'public_account', false);
	result.public_url := IniFile.ReadString(result.name, 'public_url', '');
	result.description := IniFile.ReadString(result.name, 'description', '');
	AtPos := AnsiPos('@', result.email);
	if AtPos <> 0 then
	begin
		result.user := Copy(result.email, 0, AtPos - 1);
		result.domain := Copy(result.email, AtPos + 1, Length(result.email) - Length(result.user) + 1);
	end;
	IniFile.Destroy;
end;

function SetAccountSettingsToIniFile(IniFilePath: WideString; AccountSettings: TAccountSettings): boolean;
var
	IniFile: TIniFile;
begin
	result := false;
	if AccountSettings.name <> '' then result := true;
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.WriteString(AccountSettings.name, 'email', AccountSettings.email);
	IniFile.WriteString(AccountSettings.name, 'password', AccountSettings.password);
	IniFile.WriteBool(AccountSettings.name, 'tc_pwd_mngr', AccountSettings.use_tc_password_manager);
	IniFile.WriteBool(AccountSettings.name, 'unlimited_filesize', AccountSettings.unlimited_filesize);
	IniFile.WriteBool(AccountSettings.name, 'split_large_files', AccountSettings.split_large_files);
	IniFile.WriteBool(AccountSettings.name, 'twostep_auth', AccountSettings.twostep_auth);
	IniFile.WriteBool(AccountSettings.name, 'public_account', AccountSettings.public_account);
	IniFile.WriteString(AccountSettings.name, 'public_url', AccountSettings.public_url);
	IniFile.WriteString(AccountSettings.name, 'description', AccountSettings.description);
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

procedure AddVirtualAccountsToAccountsList(AccountsIniFilePath: WideString; var AccountsList: TStringList; VirtualAccountsEnabled: TArray<boolean>);
var
	VAccounts: TStringList;
	account: WideString;
begin
	VAccounts := TStringList.Create;
	for account in AccountsList do
	begin
		if GetAccountSettingsFromIniFile(AccountsIniFilePath, account).public_account then Continue; //public accounts ignored
		if VirtualAccountsEnabled[0] then VAccounts.Add(account + TrashPostfix);
		if VirtualAccountsEnabled[1] then VAccounts.Add(account + SharedPostfix);
		if VirtualAccountsEnabled[2] then VAccounts.Add(account + InvitesPostfix);
	end;
	AccountsList.AddStrings(VAccounts);
	VAccounts.Free;
end;

end.
