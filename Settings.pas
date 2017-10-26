unit Settings;

interface

uses Classes, Windows, SysUtils, IniFiles, System.Variants, System.IOUtils, Plugin_Types, AskPassword, MRC_Helper, VCL.Controls;

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

	//Уровни логирования (по степеням двойки)
	LogLevelConnect = 1; //connection
	LogLevelFileOperation = 2; //file operations && free space
	LogLevelDetail = 4; //some detailed info (i.e. retry data or smth)
	LogLevelWarning = 8; //non-critical warnings
	LogLevelError = 16; //error details
	LogLevelDebug = 32; //also same internal debugging info

	EncryptModeNone = 0;
	EncryptModeAlways = 1;
	EncryptModeAskOnce = 2;
	//EncryptModeAskAlways = 3; //не буду поддерживать без необходимости

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
		encrypt_files_mode: integer;
		encrypt_filenames: boolean;
	end;

	TProxySettings = record
		ProxyType: integer;
		Server: WideString;
		Port: integer;
		user: WideString;
		password: WideString;
		use_tc_password_manager: boolean;
	end;

	TPluginSettings = record
		IniPath: integer;
		LoadSSLDLLOnlyFromPluginDir: boolean;
		PreserveFileTime: boolean;
		DescriptionEnabled: boolean;
		DescriptionEditorEnabled: boolean;
		DescriptionCopyToCloud: boolean;
		DescriptionCopyFromCloud: boolean;
		DescriptionTrackCloudFS: boolean;
		DescriptionFileName: WideString;

		OperationsViaPublicLinkEnabled: boolean;
		SocketTimeout: integer;
		Proxy: TProxySettings;
		CloudMaxFileSize: integer;
		ChunkOverwriteMode: integer;
		DeleteFailOnUploadMode: integer;
		OperationErrorMode: integer;
		RetryAttempts: integer;
		AttemptWait: integer;
		OverwriteLocalMode: integer;
		DisableMultiThreading: boolean;
		LogUserSpace: boolean;
		IconsMode: integer;
		DownloadLinksEncode: boolean;
		AutoUpdateDownloadListing: boolean;
		ShowTrashFolders: boolean;
		ShowSharedFolders: boolean;
		ShowInvitesFolders: boolean;
		LogLevel: integer;
	end;

	{TODO -oOwner -cGeneral : Использовать Get/SetCryptPassword во всех процедурах ообращения к менеджеру паролей TC}
function GetProxyPasswordNow(var ProxySettings: TProxySettings; LogHandleProc: TLogHandler; CryptHandleProc: TCryptHandler): boolean;
function GetCryptPassword(crypt_id: WideString; var password: WideString; LogHandleProc: TLogHandler; CryptHandleProc: TCryptHandler): integer;
function SetCryptPassword(crypt_id: WideString; password: WideString; LogHandleProc: TLogHandler; CryptHandleProc: TCryptHandler): integer;

function GetPluginSettings(IniFilePath: WideString): TPluginSettings;
procedure SetPluginSettings(IniFilePath: WideString; PluginSettings: TPluginSettings);
procedure SetPluginSettingsValue(IniFilePath: WideString; OptionName: WideString; OptionValue: Variant);
function GetAccountSettingsFromIniFile(IniFilePath: WideString; AccountName: WideString): TAccountSettings;
function SetAccountSettingsToIniFile(IniFilePath: WideString; AccountSettings: TAccountSettings): boolean;
procedure GetAccountsListFromIniFile(IniFilePath: WideString; var AccountsList: TStringList);
procedure DeleteAccountFromIniFile(IniFilePath: WideString; AccountName: WideString);
procedure AddVirtualAccountsToAccountsList(AccountsIniFilePath: WideString; var AccountsList: TStringList; VirtualAccountsEnabled: TArray<boolean>);
function GetDescriptionFileName(SettingsIniFilePath: WideString): WideString;
function RemoteDescriptionsSupportEnabled(AccountSetting: TAccountSettings): boolean; //в случае включённого шифрования файловых имён поддержка движка файловых комментариев отключается (issue #5)

implementation

function GetProxyPasswordNow(var ProxySettings: TProxySettings; LogHandleProc: TLogHandler; CryptHandleProc: TCryptHandler): boolean;
var
	CryptResult: integer;
	AskResult: integer;
	TmpString: WideString;
	buf: PWideChar;
begin
	if (ProxySettings.ProxyType = ProxyNone) or (ProxySettings.user = '') then
		exit(true); //no username means no password required

	if ProxySettings.use_tc_password_manager then
	begin //пароль должен браться из TC
		GetMem(buf, 1024);
		CryptResult := CryptHandleProc(FS_CRYPT_LOAD_PASSWORD_NO_UI, PWideChar('proxy' + ProxySettings.user), buf, 1024); //Пытаемся взять пароль по-тихому
		if CryptResult = FS_FILE_NOTFOUND then
		begin
			LogHandleProc(LogLevelDetail, msgtype_details, PWideChar('No master password entered yet'));
			CryptResult := CryptHandleProc(FS_CRYPT_LOAD_PASSWORD, PWideChar('proxy' + ProxySettings.user), buf, 1024);
		end;
		if CryptResult = FS_FILE_OK then //Успешно получили пароль
		begin
			ProxySettings.password := buf;
			//Result := true;
		end;
		if CryptResult = FS_FILE_NOTSUPPORTED then //пользователь отменил ввод главного пароля
		begin
			LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar('CryptProc returns error: Decrypt failed'));
		end;
		if CryptResult = FS_FILE_READERROR then
		begin
			LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar('CryptProc returns error: Password not found in password store'));
		end;
		FreeMemory(buf);
	end; //else // ничего не делаем, пароль уже должен быть в настройках (взят в открытом виде из инишника)

	if ProxySettings.password = '' then //но пароля нет, не в инишнике, не в тотале
	begin
		AskResult := TAskPasswordForm.AskPassword(FindTCWindow, 'User ' + ProxySettings.user + ' proxy', ProxySettings.password, ProxySettings.use_tc_password_manager, false);
		if AskResult <> mrOK then
		begin //не указали пароль в диалоге
			exit(false); //отказались вводить пароль
		end else begin
			if ProxySettings.use_tc_password_manager then
			begin
				case CryptHandleProc(FS_CRYPT_SAVE_PASSWORD, PWideChar('proxy' + ProxySettings.user), PWideChar(ProxySettings.password), SizeOf(ProxySettings.password)) of
					FS_FILE_OK:
						begin //TC скушал пароль, запомним в инишник галочку
							LogHandleProc(LogLevelDebug, msgtype_details, PWideChar('Password saved in TC password manager'));
							TmpString := ProxySettings.password;
							ProxySettings.password := '';
							ProxySettings.use_tc_password_manager := true; //Не забыть сохранить!
							ProxySettings.password := TmpString;
						end;
					FS_FILE_NOTSUPPORTED: //Сохранение не получилось
						begin
							LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar('CryptProc returns error: Encrypt failed'));
						end;
					FS_FILE_WRITEERROR: //Сохранение опять не получилось
						begin
							LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar('Password NOT saved: Could not write password to password store'));
						end;
					FS_FILE_NOTFOUND: //Не указан мастер-пароль
						begin
							LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar('Password NOT saved: No master password entered yet'));
						end;
					//Ошибки здесь не значат, что пароль мы не получили - он может быть введён в диалоге
				end;
			end;
			result := true;
		end;
	end
	else
		result := true; //пароль взят из инишника напрямую
end;

{TODO -oOwner -cGeneral : unused LogHandleProc}
function GetCryptPassword(crypt_id: WideString; var password: WideString; LogHandleProc: TLogHandler; CryptHandleProc: TCryptHandler): integer;
var
	buf: PWideChar;
begin
	GetMem(buf, 1024);
	ZeroMemory(buf, 1024);
	result := CryptHandleProc(FS_CRYPT_LOAD_PASSWORD_NO_UI, PWideChar(crypt_id),buf, 1024);
	case result of //Пытаемся взять пароль по-тихому
		FS_FILE_OK: //all ok, we got password
			begin
			 	password := buf;
			end;
		FS_FILE_READERROR: //Password not found in password store, ask user for it
			begin
			end;
		FS_FILE_NOTFOUND: //no master password entered yet
			begin
				ZeroMemory(buf, 1024);
				result := CryptHandleProc(FS_CRYPT_LOAD_PASSWORD, PWideChar(crypt_id), buf, 1024);
				case result of
					FS_FILE_OK: //all ok, we got password
						begin
							password := buf;
						end;
					FS_FILE_READERROR: //Password not found in password store, ask user for it
						begin
						end
					else
						begin
							//something else happened log&exit
						end;
				end;
			end
		else
			begin
				//something else happened  log&exit
			end;
	end;
	FreeMemory(buf);

end;

function SetCryptPassword(crypt_id: WideString; password: WideString; LogHandleProc: TLogHandler; CryptHandleProc: TCryptHandler): integer;
begin
	result := CryptHandleProc(FS_CRYPT_SAVE_PASSWORD, PWideChar(crypt_id), PWideChar(password), SizeOf(password));

	case result of
		FS_FILE_OK:
			begin //TC скушал пароль, запомним в инишник галочку
				if Assigned(LogHandleProc) then
					LogHandleProc(LogLevelDebug, msgtype_details, PWideChar(crypt_id + ': password saved in TC password manager'));
			end;
		FS_FILE_NOTSUPPORTED: //Сохранение не получилось
			begin
				if Assigned(LogHandleProc) then
					LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar(crypt_id + ': CryptProc returns error: Encrypt failed'));
			end;
		FS_FILE_WRITEERROR: //Сохранение опять не получилось
			begin
				if Assigned(LogHandleProc) then
					LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar(crypt_id + ': password NOT saved: Could not write password to password store'));
			end;
		FS_FILE_NOTFOUND: //Не указан мастер-пароль
			begin
				if Assigned(LogHandleProc) then
					LogHandleProc(LogLevelError, msgtype_importanterror, PWideChar(crypt_id + ': password NOT saved: No master password entered yet'));
			end;
		//Ошибки здесь не значат, что пароль мы не получили - он может быть введён в диалоге
	end;

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
	GetPluginSettings.DescriptionEditorEnabled := IniFile.ReadBool('Main', 'DescriptionEditorEnabled', false);
	GetPluginSettings.DescriptionCopyToCloud := IniFile.ReadBool('Main', 'DescriptionCopyToCloud', false);
	GetPluginSettings.DescriptionCopyFromCloud := IniFile.ReadBool('Main', 'DescriptionCopyFromCloud', false);
	GetPluginSettings.DescriptionTrackCloudFS := IniFile.ReadBool('Main', 'DescriptionTrackCloudFS', false);
	GetPluginSettings.DescriptionFileName := IniFile.ReadString('Main', 'DescriptionFileName', 'descript.ion');
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
	GetPluginSettings.LogLevel := IniFile.ReadInteger('Main', 'LogLevel', LogLevelConnect + LogLevelFileOperation + LogLevelDetail + LogLevelWarning + LogLevelError);
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
	basicType: integer;
begin
	IniFile := TIniFile.Create(IniFilePath);

	basicType := VarType(OptionValue);
	try
		case basicType of
			varNull:
				IniFile.DeleteKey('Main', OptionName); //remove value in that case
			varInteger:
				IniFile.WriteInteger('Main', OptionName, OptionValue);
			varString, varUString:
				IniFile.WriteString('Main', OptionName, OptionValue);
			varBoolean:
				IniFile.WriteBool('Main', OptionName, OptionValue);
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
	AtPos: integer;
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
	result.encrypt_files_mode := IniFile.ReadInteger(result.name, 'encrypt_files_mode', EncryptModeNone);
	result.encrypt_filenames := IniFile.ReadBool(result.name, 'encrypt_filenames', false);
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
	if AccountSettings.name <> '' then
		result := true;
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
	IniFile.WriteInteger(AccountSettings.name, 'encrypt_files_mode', AccountSettings.encrypt_files_mode);
	IniFile.WriteBool(AccountSettings.name, 'encrypt_filenames', AccountSettings.encrypt_filenames);
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
		if GetAccountSettingsFromIniFile(AccountsIniFilePath, account).public_account then
			Continue; //public accounts ignored
		if VirtualAccountsEnabled[0] then
			VAccounts.Add(account + TrashPostfix);
		if VirtualAccountsEnabled[1] then
			VAccounts.Add(account + SharedPostfix);
		if VirtualAccountsEnabled[2] then
			VAccounts.Add(account + InvitesPostfix);
	end;
	AccountsList.AddStrings(VAccounts);
	VAccounts.Free;
end;

function GetDescriptionFileName(SettingsIniFilePath: WideString): WideString;
begin
	GetDescriptionFileName := GetPluginSettings(SettingsIniFilePath).DescriptionFileName;
	if TPath.HasValidFileNameChars(GetPluginSettings(SettingsIniFilePath).DescriptionFileName, false) then
		exit;
	exit('descript.ion');
end;

function RemoteDescriptionsSupportEnabled(AccountSetting: TAccountSettings): boolean; //в случае включённого шифрования файловых имён поддержка движка файловых комментариев отключается (issue #5)
begin
	result := not((AccountSetting.encrypt_files_mode <> EncryptModeNone) and AccountSetting.encrypt_filenames)
end;

end.
