unit Settings;

interface

uses Classes, Windows, SysUtils, IniFiles, System.Variants, System.IOUtils, Plugin_Types, MRC_Helper, VCL.Controls, System.RegularExpressions;

const
{$IFDEF WIN64}
	PlatformX = 'x64';
{$ENDIF}
{$IFDEF WIN32}
	PlatformX = 'x32';
{$ENDIF}
	ProxyNone = 0;
	ProxySocks5 = 1;
	ProxySocks4 = 2;
	ProxyHTTP = 3;

	SocksProxyTypes = [ProxySocks5, ProxySocks4];

	CLOUD_MAX_FILESIZE_DEFAULT = 2147483392; //$80000000-256
	CLOUD_PRECALCULATE_LIMIT_DEFAULT = 20; //issue #231

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

	CopyBetweenAccountsModeDisabled = 0;
	CopyBetweenAccountsModeViaHash = 1; //default
	CopyBetweenAccountsModeViaPublicLink = 2; //old mode

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

	EncryptModeNone = 0; //Без шифрования
	EncryptModeAlways = 1; //С прозрачным шифрованием
	EncryptModeAskOnce = 2; //С прозрачным шифрованием, без хранения пароля
	//EncryptModeAskAlways = 3; //не буду поддерживать без необходимости

	StreamingPrefix = 'Streaming:';

	DEFAULT_USERAGENT = 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.162 Safari/537.36/TCWFX(' + PlatformX + ')';

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

	TProxySettings = record
		ProxyType: integer;
		Server: WideString;
		Port: integer;
		user: WideString;
		password: WideString;
		use_tc_password_manager: boolean;
	end;

	{Settings for HTTP transport}
	TConnectionSettings = record
		ProxySettings: TProxySettings;
		SocketTimeout: integer;
		UploadBPS: integer;
		DownloadBPS: integer;
		UserAgent: WideString;
	end;

	{Global plugin options}
	TPluginSettings = record
		ConnectionSettings: TConnectionSettings;
		IniPath: integer;
		LoadSSLDLLOnlyFromPluginDir: boolean;
		PreserveFileTime: boolean;
		DescriptionEnabled: boolean;
		DescriptionEditorEnabled: boolean;
		DescriptionCopyToCloud: boolean;
		DescriptionCopyFromCloud: boolean;
		DescriptionTrackCloudFS: boolean;
		DescriptionFileName: WideString;
		CopyBetweenAccountsMode: integer;
		CloudMaxFileSize: int64;
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
		PrecalculateHash: boolean;
		ForcePrecalculateSize: int64;
		CheckCRC: boolean;
	end;

	{Прототипирую сюда все параметры, которые требуются классом облака}
	TCloudSettings = record
		{Параметры конкретного аккаунта}
		AccountSettings: TAccountSettings;
		{Параметры, наследуемые от глобальных настроек}
		ConnectionSettings: TConnectionSettings;
		PrecalculateHash: boolean;
		ForcePrecalculateSize: int64;
		CheckCRC: boolean;
		CloudMaxFileSize: int64;
		OperationErrorMode: integer;
		RetryAttempts: integer;
		AttemptWait: integer;
	end;

	{Параметры стриминга для расширения}
	TStreamingOptions = record
		Command: WideString; //Вызываемое приложение
		Parameters: WideString; //параметры, передаваемые приложению
		StartPath: WideString; //каталог запуска
		Format: integer;
	end;

	TIniFilesHelper = class helper for TIniFile
		function ReadInt64(const Section, Ident: string; Default: int64): int64;
		procedure WriteInt64(const Section, Ident: string; Value: int64);
		procedure WriteString(const Section, Ident, Value: String); //owerride default Write%Anything% metod
		function ValidateSectionName(const Section: string): boolean;
		function ValidateIdentName(const Ident: string): boolean;
	end;

function GetPluginSettings(IniFilePath: WideString): TPluginSettings;
procedure SetPluginSettingsValue(IniFilePath: WideString; OptionName: WideString; OptionValue: Variant);
function GetAccountSettingsFromIniFile(IniFilePath: WideString; AccountName: WideString): TAccountSettings;
function SetAccountSettingsToIniFile(AccountSettings: TAccountSettings; IniFilePath: WideString = ''): boolean;
procedure SetAccountSettingsValue(IniFilePath: WideString; Account, OptionName: WideString; OptionValue: Variant);
procedure GetAccountsListFromIniFile(IniFilePath: WideString; var AccountsList: TStringList);
procedure DeleteAccountFromIniFile(IniFilePath: WideString; AccountName: WideString);
procedure AddVirtualAccountsToAccountsList(AccountsIniFilePath: WideString; var AccountsList: TStringList; VirtualAccountsEnabled: TArray<boolean>);
function GetDescriptionFileName(SettingsIniFilePath: WideString): WideString;
function RemoteDescriptionsSupportEnabled(AccountSetting: TAccountSettings): boolean; //в случае включённого шифрования файловых имён поддержка движка файловых комментариев отключается (issue #5)

function GetStreamingOptionsFromIniFile(IniFilePath, FileName: WideString; var StreamingOptions: TStreamingOptions): boolean;
function SetStreamingOptionsToIniFile(IniFilePath, FileName: WideString; StreamingOptions: TStreamingOptions): boolean;

procedure GetStreamingExtensionsFromIniFile(IniFilePath: WideString; var StreamingExtensions: TStringList);
procedure DeleteStreamingExtensionsFromIniFile(IniFilePath: WideString; StreamingExtension: WideString);

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
	GetPluginSettings.DescriptionEditorEnabled := IniFile.ReadBool('Main', 'DescriptionEditorEnabled', false);
	GetPluginSettings.DescriptionCopyToCloud := IniFile.ReadBool('Main', 'DescriptionCopyToCloud', false);
	GetPluginSettings.DescriptionCopyFromCloud := IniFile.ReadBool('Main', 'DescriptionCopyFromCloud', false);
	GetPluginSettings.DescriptionTrackCloudFS := IniFile.ReadBool('Main', 'DescriptionTrackCloudFS', false);
	GetPluginSettings.DescriptionFileName := IniFile.ReadString('Main', 'DescriptionFileName', 'descript.ion');
	GetPluginSettings.CopyBetweenAccountsMode := IniFile.ReadInteger('Main', 'CopyBetweenAccountsMode', CopyBetweenAccountsModeDisabled);

	GetPluginSettings.DisableMultiThreading := IniFile.ReadBool('Main', 'DisableMultiThreading', false);
	GetPluginSettings.LogUserSpace := IniFile.ReadBool('Main', 'LogUserSpace', true);
	GetPluginSettings.IconsMode := IniFile.ReadInteger('Main', 'IconsMode', 0);
	GetPluginSettings.ConnectionSettings.SocketTimeout := IniFile.ReadInteger('Main', 'SocketTimeout', -1);
	GetPluginSettings.ConnectionSettings.UploadBPS := IniFile.ReadInteger('Main', 'UploadBPS', -1);
	GetPluginSettings.ConnectionSettings.DownloadBPS := IniFile.ReadInteger('Main', 'DownloadBPS', -1);
	GetPluginSettings.CloudMaxFileSize := IniFile.ReadInt64('Main', 'CloudMaxFileSize', CLOUD_MAX_FILESIZE_DEFAULT);
	GetPluginSettings.ChunkOverwriteMode := IniFile.ReadInteger('Main', 'ChunkOverwriteMode', 0);
	GetPluginSettings.DeleteFailOnUploadMode := IniFile.ReadInteger('Main', 'DeleteFailOnUploadMode', 0);
	GetPluginSettings.OverwriteLocalMode := IniFile.ReadInteger('Main', 'OverwriteLocalMode', 0);
	GetPluginSettings.OperationErrorMode := IniFile.ReadInteger('Main', 'OperationErrorMode', 0);
	GetPluginSettings.RetryAttempts := IniFile.ReadInteger('Main', 'RetryAttempts', 1);
	GetPluginSettings.AttemptWait := IniFile.ReadInteger('Main', 'AttemptWait', 1000);
	GetPluginSettings.ConnectionSettings.ProxySettings.ProxyType := IniFile.ReadInteger('Main', 'ProxyType', ProxyNone);
	GetPluginSettings.ConnectionSettings.ProxySettings.Server := IniFile.ReadString('Main', 'ProxyServer', EmptyWideStr);
	GetPluginSettings.ConnectionSettings.ProxySettings.Port := IniFile.ReadInteger('Main', 'ProxyPort', 0);
	GetPluginSettings.ConnectionSettings.ProxySettings.user := IniFile.ReadString('Main', 'ProxyUser', EmptyWideStr);
	GetPluginSettings.ConnectionSettings.ProxySettings.use_tc_password_manager := IniFile.ReadBool('Main', 'ProxyTCPwdMngr', false);
	GetPluginSettings.ConnectionSettings.ProxySettings.password := IniFile.ReadString('Main', 'ProxyPassword', EmptyWideStr);
	GetPluginSettings.ConnectionSettings.UserAgent := IniFile.ReadString('Main', 'UserAgent', DEFAULT_USERAGENT);
	GetPluginSettings.DownloadLinksEncode := IniFile.ReadBool('Main', 'DownloadLinksEncode', true);
	GetPluginSettings.AutoUpdateDownloadListing := IniFile.ReadBool('Main', 'AutoUpdateDownloadListing', true);
	GetPluginSettings.ShowTrashFolders := IniFile.ReadBool('Main', 'ShowTrashFolders', true);
	GetPluginSettings.ShowSharedFolders := IniFile.ReadBool('Main', 'ShowSharedFolders', true);
	GetPluginSettings.ShowInvitesFolders := IniFile.ReadBool('Main', 'ShowInvitesFolders', true);
	GetPluginSettings.LogLevel := IniFile.ReadInteger('Main', 'LogLevel', LogLevelConnect + LogLevelFileOperation + LogLevelDetail + LogLevelWarning + LogLevelError);
	GetPluginSettings.PrecalculateHash := IniFile.ReadBool('Main', 'PrecalculateHash', true);
	GetPluginSettings.ForcePrecalculateSize := IniFile.ReadInt64('Main', 'ForcePrecalculateSize', CLOUD_PRECALCULATE_LIMIT_DEFAULT);
	GetPluginSettings.CheckCRC := IniFile.ReadBool('Main', 'CheckCRC', true);
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
			varString, varUString, varOleStr:
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
			MessageBoxW(0, PWideChar(E.Message), 'INI file error', MB_ICONERROR + MB_OK);
			IniFile.Destroy;
			exit;
		end;
	end;
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
	Account: WideString;
begin
	VAccounts := TStringList.Create;
	for Account in AccountsList do
	begin
		if GetAccountSettingsFromIniFile(AccountsIniFilePath, Account).public_account then
			Continue; //public accounts ignored
		if VirtualAccountsEnabled[0] then
			VAccounts.Add(Account + TrashPostfix);
		if VirtualAccountsEnabled[1] then
			VAccounts.Add(Account + SharedPostfix);
		if VirtualAccountsEnabled[2] then
			VAccounts.Add(Account + InvitesPostfix);
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

function GetStreamingOptionsFromIniFile(IniFilePath, FileName: WideString; var StreamingOptions: TStreamingOptions): boolean;
var
	IniFile: TIniFile;
	SectionName: WideString;
begin
	result := false;
	StreamingOptions := default (TStreamingOptions);
	IniFile := TIniFile.Create(IniFilePath);
	SectionName := StreamingPrefix + ExtractUniversalFileExt(FileName, true);
	if IniFile.SectionExists(SectionName) then
	begin
		result := true;
		StreamingOptions.Command := IniFile.ReadString(SectionName, 'Command', EmptyWideStr);
		StreamingOptions.Parameters := IniFile.ReadString(SectionName, 'Parameters', EmptyWideStr);
		StreamingOptions.StartPath := IniFile.ReadString(SectionName, 'StartPath', EmptyWideStr);
		StreamingOptions.Format := IniFile.ReadInteger(SectionName, 'Format', 0);
	end;
	IniFile.Destroy;
end;

function SetStreamingOptionsToIniFile(IniFilePath, FileName: WideString; StreamingOptions: TStreamingOptions): boolean;
var
	IniFile: TIniFile;
	SectionName: WideString;
begin
	result := false;
	if ExtractUniversalFileExt(FileName, true) <> EmptyWideStr then
	begin
		result := true;
		SectionName := StreamingPrefix + ExtractUniversalFileExt(FileName, true);
		IniFile := TIniFile.Create(IniFilePath);
		IniFile.WriteString(SectionName, 'Command', StreamingOptions.Command);
		IniFile.WriteString(SectionName, 'Parameters', StreamingOptions.Parameters);
		IniFile.WriteString(SectionName, 'StartPath', StreamingOptions.StartPath);
		IniFile.WriteInteger(SectionName, 'Format', StreamingOptions.Format);
		IniFile.Destroy;
	end;
end;

//loads all streaming extensions list
procedure GetStreamingExtensionsFromIniFile(IniFilePath: WideString; var StreamingExtensions: TStringList);
var
	IniFile: TIniFile;
	TempList: TStringList;
	line: String;
begin
	IniFile := TIniFile.Create(IniFilePath);
	TempList := TStringList.Create;
	IniFile.ReadSections(TempList);
	for line in TempList do
	begin
		if line.StartsWith(StreamingPrefix) then
			StreamingExtensions.Add(line.Substring(Length(StreamingPrefix)));
	end;
	TempList.Destroy;
	IniFile.Destroy;
end;

procedure DeleteStreamingExtensionsFromIniFile(IniFilePath: WideString; StreamingExtension: WideString);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(IniFilePath);
	IniFile.EraseSection(StreamingPrefix + StreamingExtension);
	IniFile.Destroy;
end;

{TIniFilesHelper}

function TIniFilesHelper.ReadInt64(const Section, Ident: string; Default: int64): int64;
var
	IntStr: string;
begin
	IntStr := ReadString(Section, Ident, '');
	if (IntStr.Length > 2) and (IntStr.StartsWith('0x', true)) then
		IntStr := '$' + IntStr.Substring(2);
	result := StrToInt64Def(IntStr, Default);
end;

function TIniFilesHelper.ValidateIdentName(const Ident: string): boolean;
var
	RegEx: TRegEx;
begin
	RegEx := TRegEx.Create('^([a-z]|[A-Z]|\.|\$|\:)([a-z]|[A-Z]|[0-9]|_|~|-|\.|:|\$|\s)+');
	result := RegEx.Match(Ident).Success;
end;

function TIniFilesHelper.ValidateSectionName(const Section: string): boolean;
var
	RegEx: TRegEx;
begin

	RegEx := TRegEx.Create('\[|\]|\n');
	result := not RegEx.Match(Section).Success;
end;

procedure TIniFilesHelper.WriteInt64(const Section, Ident: string; Value: int64);
begin
	WriteString(Section, Ident, IntToStr(Value));
end;

procedure TIniFilesHelper.WriteString(const Section, Ident, Value: String);
begin
	if not(self.ValidateSectionName(Section)) then
		raise EIniFileException.CreateFmt('Invalid section name %s', [Section]);
	if not(self.ValidateIdentName(Ident)) then
		raise EIniFileException.CreateFmt('Invalid identifier name %s', [Ident]);
	inherited WriteString(Section, Ident, Value);
end;

end.
