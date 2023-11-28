unit NewAccountSettings;

interface

uses
	IniFiles,
	SysUtils,
	Variants,
	WindowsHelper,
	Windows,
	ParsingHelper,
	CMRStrings,
	SETTINGS_CONSTANTS,
	AbstractAccountSettings;

type

	TNewAccountSettings = class(TAbstractAccountSettings)
	private
		FIniFilePath: WideString;
		FAccount: WideString;
		FUser: WideString;
		FDomain: WideString;

		function GetAccount: WideString;
		function GetIsRemoteDescriptionsSupported: Boolean; //Шифрованная строка для проверки пароля шифрования

	public

		crypt_files_password: WideString; //todo: check usage

		constructor Create(IniFilePath: WideString; Account: WideString); overload;
		constructor Create(IniFilePath: WideString); overload;
		procedure Refresh();
		property Account: WideString read GetAccount write FAccount;
		property User: WideString read FUser;
		property Domain: WideString read FDomain;
		property IsRemoteDescriptionsSupported: Boolean read GetIsRemoteDescriptionsSupported;

		procedure SetSettingValue(OptionName: WideString; OptionValue: Variant); override;
		procedure Save(); override;
	end;

implementation

{TNewAccountSettings}

constructor TNewAccountSettings.Create(IniFilePath: WideString);
begin
	self.FIniFilePath := IniFilePath;
	self.FSaveOnChange := False;
	Refresh();
end;

constructor TNewAccountSettings.Create(IniFilePath, Account: WideString);
begin
	self.FIniFilePath := IniFilePath;
	self.FAccount := Account;
	self.FSaveOnChange := False;
	Refresh();
end;

function TNewAccountSettings.GetAccount: WideString;
begin
	if FAccount = EmptyWideStr then
		raise Exception.Create('Account can''t be empty');
	Result := FAccount;
end;

function TNewAccountSettings.GetIsRemoteDescriptionsSupported: Boolean;
begin
	Result := not((FEncryptFilesMode <> EncryptModeNone) and FEncryptFileNames)
end;

procedure TNewAccountSettings.Refresh;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FIniFilePath);

	FEmail := IniFile.ReadString(Account, 'email', EmptyWideStr);
	FPassword := IniFile.ReadString(Account, 'password', EmptyWideStr);
	FUseTCPasswordManager := IniFile.ReadBool(Account, 'tc_pwd_mngr', False);
	FUnlimitedFilesize := IniFile.ReadBool(Account, 'unlimited_filesize', False);
	FSplitLargeFiles := IniFile.ReadBool(Account, 'split_large_files', False);
	FTwostepAuth := IniFile.ReadBool(Account, 'twostep_auth', False);
	FPublicAccount := IniFile.ReadBool(Account, 'public_account', False);
	FPublicUrl := IniFile.ReadString(Account, 'public_url', EmptyWideStr);
	FDescription := IniFile.ReadString(Account, 'description', EmptyWideStr);
	FEncryptFilesMode := IniFile.ReadInteger(Account, 'encrypt_files_mode', EncryptModeNone);
	FEncryptFileNames := IniFile.ReadBool(Account, 'encrypt_filenames', False);
	FShardOverride := IniFile.ReadString(Account, 'shard_override', EmptyWideStr);
	FUploadUrlOverride := IniFile.ReadString(Account, 'upload_url_override', EmptyWideStr);
	FCryptedGUIDFiles := IniFile.ReadString(Account, 'CryptedGUID_files', EmptyWideStr);

	IniFile.Destroy;

	ExtractEmailParts(FEmail, FUser, FDomain);
end;

procedure TNewAccountSettings.Save;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FIniFilePath);
	IniFile.WriteString(Account, 'email', FEmail);
	IniFile.WriteString(Account, 'password', FPassword);
	IniFile.WriteBool(Account, 'tc_pwd_mngr', FUseTCPasswordManager);
	IniFile.WriteBool(Account, 'unlimited_filesize', FUnlimitedFilesize);
	IniFile.WriteBool(Account, 'split_large_files', FSplitLargeFiles);
	IniFile.WriteBool(Account, 'twostep_auth', FTwostepAuth);
	IniFile.WriteBool(Account, 'public_account', FPublicAccount);
	IniFile.WriteString(Account, 'public_url', FPublicUrl);
	IniFile.WriteString(Account, 'description', FDescription);
	IniFile.WriteInteger(Account, 'encrypt_files_mode', FEncryptFilesMode);
	IniFile.WriteBool(Account, 'encrypt_filenames', FEncryptFileNames);
	IniFile.Destroy;
end;

procedure TNewAccountSettings.SetSettingValue(OptionName: WideString; OptionValue: Variant);
var
	IniFile: TIniFile;
	basicType: integer;
begin
	IniFile := TIniFile.Create(FIniFilePath);

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

end.
