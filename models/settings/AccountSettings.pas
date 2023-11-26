unit AccountSettings;

interface

uses
	SysUtils,
	IniFiles,
	SETTINGS_CONSTANTS,
	Variants,
	WindowsHelper,
	Windows,
	CMRStrings;

type
	{Account-related options set}
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
		shard_override: WideString; //hidden option, allows to override working shard for account
		upload_url_override: WideString; //hidden option, alows to override upload server for account
		self_ini_path: WideString; //runtime parameter, contains path to ini file, used for various manipulations
		crypt_files_password: WideString; //runtime parameter
		CryptedGUID_files: WideString; //Шифрованная строка для проверки пароля шифрования
	end;

function GetAccountSettingsFromIniFile(IniFilePath: WideString; AccountName: WideString): TAccountSettings;
function SetAccountSettingsToIniFile(AccountSettings: TAccountSettings; IniFilePath: WideString = ''): boolean;
procedure SetAccountSettingsValue(IniFilePath: WideString; Account, OptionName: WideString; OptionValue: Variant);
function RemoteDescriptionsSupportEnabled(AccountSetting: TAccountSettings): boolean; //в случае включённого шифрования файловых имён поддержка движка файловых комментариев отключается (issue #5)

implementation

function GetAccountSettingsFromIniFile(IniFilePath: WideString; AccountName: WideString): TAccountSettings;
var
	IniFile: TIniFile;
	AtPos: integer;
begin
	IniFile := TIniFile.Create(IniFilePath);
	result.name := AccountName;
	result.email := IniFile.ReadString(result.name, 'email', EmptyWideStr);
	result.password := IniFile.ReadString(result.name, 'password', EmptyWideStr);
	result.use_tc_password_manager := IniFile.ReadBool(result.name, 'tc_pwd_mngr', false);
	result.unlimited_filesize := IniFile.ReadBool(result.name, 'unlimited_filesize', false);
	result.split_large_files := IniFile.ReadBool(result.name, 'split_large_files', false);
	result.twostep_auth := IniFile.ReadBool(result.name, 'twostep_auth', false);
	result.public_account := IniFile.ReadBool(result.name, 'public_account', false);
	result.public_url := IniFile.ReadString(result.name, 'public_url', EmptyWideStr);
	result.description := IniFile.ReadString(result.name, 'description', EmptyWideStr);
	result.encrypt_files_mode := IniFile.ReadInteger(result.name, 'encrypt_files_mode', EncryptModeNone);
	result.encrypt_filenames := IniFile.ReadBool(result.name, 'encrypt_filenames', false);
	result.shard_override := IniFile.ReadString(result.name, 'shard_override', EmptyWideStr);
	result.upload_url_override := IniFile.ReadString(result.name, 'upload_url_override', EmptyWideStr);
	result.CryptedGUID_files := IniFile.ReadString(result.name, 'CryptedGUID_files', EmptyWideStr);
	AtPos := AnsiPos('@', result.email);
	if AtPos <> 0 then
	begin
		result.user := Copy(result.email, 0, AtPos - 1);
		result.domain := Copy(result.email, AtPos + 1, Length(result.email) - Length(result.user) + 1);
	end;
	result.self_ini_path := IniFilePath;
	IniFile.Destroy;
end;

function SetAccountSettingsToIniFile(AccountSettings: TAccountSettings; IniFilePath: WideString = ''): boolean;
var
	IniFile: TIniFile;
begin
	if IniFilePath = EmptyWideStr then
		IniFilePath := AccountSettings.self_ini_path;

	result := false;
	if AccountSettings.name <> EmptyWideStr then
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
	//IniFile.WriteString(AccountSettings.name, 'shard_override', AccountSettings.public_url);
	IniFile.Destroy;
end;

procedure SetAccountSettingsValue(IniFilePath: WideString; Account, OptionName: WideString; OptionValue: Variant);
var
	IniFile: TIniFile;
	basicType: integer;
begin
	IniFile := TIniFile.Create(IniFilePath);

	basicType := VarType(OptionValue);
	try
		case basicType of
			varNull:
				IniFile.DeleteKey(Account, OptionName); //remove value in that case
			varInteger:
				IniFile.WriteInteger(Account, OptionName, OptionValue);
			varString, varUString, varOleStr:
				IniFile.WriteString(Account, OptionName, OptionValue);
			varBoolean:
				IniFile.WriteBool(Account, OptionName, OptionValue);
		end;
	except
		On E: EIniFileException do
		begin
			MsgBox(0, E.Message, ERR_INI_GENERAL, MB_ICONERROR + MB_OK);
			IniFile.Destroy;
			exit;
		end;
	end;
	IniFile.Destroy;
end;

function RemoteDescriptionsSupportEnabled(AccountSetting: TAccountSettings): boolean; //в случае включённого шифрования файловых имён поддержка движка файловых комментариев отключается (issue #5)
begin
	result := not((AccountSetting.encrypt_files_mode <> EncryptModeNone) and AccountSetting.encrypt_filenames)
end;


end.
