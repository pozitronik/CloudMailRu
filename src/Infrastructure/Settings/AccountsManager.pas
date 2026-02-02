unit AccountsManager;

{Interface for account settings management, decoupled from INI file implementation}

interface

uses
	SysUtils,
	Variants,
	Classes,
	Windows,
	LanguageStrings,
	SettingsConstants,
	WSList,
	AccountSettings,
	ConfigFile,
	Logger;

type
	IAccountsManager = interface
		['{7B8C9D0E-1F2A-3B4C-5D6E-7F8A9B0C1D2E}']
		function GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
		function GetAccountSettings(Account: WideString): TAccountSettings;
		procedure SetAccountSettings(Account: WideString; AccountSettings: TAccountSettings); overload;
		procedure SetAccountSettings(AccountSettings: TAccountSettings); overload;
		procedure DeleteAccount(Account: WideString);
		procedure RenameAccount(const OldName, NewName: WideString);
		procedure SwitchPasswordStorage(Account: WideString);
		procedure SetCryptedGUID(Account: WideString; GUID: WideString);
	end;

	{Null implementation for testing - returns defaults, no-op for writes}
	TNullAccountsManager = class(TInterfacedObject, IAccountsManager)
	public
		function GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
		function GetAccountSettings(Account: WideString): TAccountSettings;
		procedure SetAccountSettings(Account: WideString; AccountSettings: TAccountSettings); overload;
		procedure SetAccountSettings(AccountSettings: TAccountSettings); overload;
		procedure DeleteAccount(Account: WideString);
		procedure RenameAccount(const OldName, NewName: WideString);
		procedure SwitchPasswordStorage(Account: WideString);
		procedure SetCryptedGUID(Account: WideString; GUID: WideString);
	end;

	TAccountsManager = class(TInterfacedObject, IAccountsManager)
	private
		FConfigFile: IConfigFile;
		FLogger: ILogger;

		function Accounts: TWSList;
	public
		constructor Create(ConfigFile: IConfigFile; Logger: ILogger);
		{Validates account name against WFX path protocol and INI format constraints.
		Rejects names containing path separators (\, /), INI delimiters ([, ]),
		and names ending with reserved virtual directory postfixes (.trash, .shared, .invites).}
		class function IsValidAccountName(const Name: WideString): Boolean;
		function GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
		function GetAccountSettings(Account: WideString): TAccountSettings;
		procedure SetAccountSettings(Account: WideString; AccountSettings: TAccountSettings); overload;
		procedure SetAccountSettings(AccountSettings: TAccountSettings); overload;
		procedure DeleteAccount(Account: WideString);
		procedure RenameAccount(const OldName, NewName: WideString);
		procedure SetCryptedGUID(Account: WideString; GUID: WideString);
		{Clears the account password from INI and sets 'managed by TC' flag}
		procedure SwitchPasswordStorage(Account: WideString);

	end;

implementation

uses
	StrUtils,
	CloudConstants,
	WFXTypes;

{TAccountsManager.IsValidAccountName}

class function TAccountsManager.IsValidAccountName(const Name: WideString): Boolean;
begin
	if Name = '' then
		Exit(False);

	{WFX path separators}
	if (Pos('\', Name) > 0) or (Pos('/', Name) > 0) then
		Exit(False);

	{INI section delimiters}
	if (Pos('[', Name) > 0) or (Pos(']', Name) > 0) then
		Exit(False);

	{Reserved virtual directory postfixes (case-insensitive via StrUtils.EndsText)}
	if EndsText(TrashPostfix, Name) or EndsText(SharedPostfix, Name) or EndsText(InvitesPostfix, Name) then
		Exit(False);

	Result := True;
end;

{TNullAccountsManager}

function TNullAccountsManager.GetAccountsList(const AccountTypes: EAccountType; const VirtualTypes: EVirtualType): TWSList;
begin
	Result.Clear;
end;

function TNullAccountsManager.GetAccountSettings(Account: WideString): TAccountSettings;
begin
	Result := Default (TAccountSettings);
	Result.Account := Account;
end;

procedure TNullAccountsManager.SetAccountSettings(Account: WideString; AccountSettings: TAccountSettings);
begin
	{No-op for null implementation}
end;

procedure TNullAccountsManager.SetAccountSettings(AccountSettings: TAccountSettings);
begin
	{No-op for null implementation}
end;

procedure TNullAccountsManager.DeleteAccount(Account: WideString);
begin
	{No-op for null implementation}
end;

procedure TNullAccountsManager.RenameAccount(const OldName, NewName: WideString);
begin
	{No-op for null implementation}
end;

procedure TNullAccountsManager.SwitchPasswordStorage(Account: WideString);
begin
	{No-op for null implementation}
end;

procedure TNullAccountsManager.SetCryptedGUID(Account: WideString; GUID: WideString);
begin
	{No-op for null implementation}
end;

{TAccountsManager}

function TAccountsManager.Accounts: TWSList;
var
	AccountsList: TStringList;
	I: Integer;
begin
	AccountsList := TStringList.Create();
	try
		FConfigFile.ReadSections(AccountsList);
		SetLength(Result, AccountsList.Count);
		for I := 0 to AccountsList.Count - 1 do
			Result[I] := AccountsList[I];
	finally
		AccountsList.Free;
	end;
end;

constructor TAccountsManager.Create(ConfigFile: IConfigFile; Logger: ILogger);
begin
	FConfigFile := ConfigFile;
	FLogger := Logger;
end;

procedure TAccountsManager.SwitchPasswordStorage(Account: WideString);
begin
	FConfigFile.DeleteKey(Account, 'password');
	FConfigFile.WriteBool(Account, 'tc_pwd_mngr', True);
end;

procedure TAccountsManager.DeleteAccount(Account: WideString);
begin
	FConfigFile.EraseSection(Account);
end;

procedure TAccountsManager.RenameAccount(const OldName, NewName: WideString);
var
	Settings: TAccountSettings;
begin
	if OldName = NewName then
		Exit;

	{Copy all persisted fields to new section, then remove the old one.
	 SetAccountSettings does not write read-only fields (CryptedGUID, shard/upload
	 overrides), so we transfer them explicitly.}
	Settings := GetAccountSettings(OldName);
	SetAccountSettings(NewName, Settings);

	if Settings.CryptedGUIDFiles <> '' then
		FConfigFile.WriteString(NewName, 'CryptedGUID_files', Settings.CryptedGUIDFiles);
	if Settings.ShardOverride <> '' then
		FConfigFile.WriteString(NewName, 'shard_override', Settings.ShardOverride);
	if Settings.UploadUrlOverride <> '' then
		FConfigFile.WriteString(NewName, 'upload_url_override', Settings.UploadUrlOverride);

	FConfigFile.EraseSection(OldName);
end;

function TAccountsManager.GetAccountSettings(Account: WideString): TAccountSettings;
begin
	Result.Account := Account;
	Result.Email := FConfigFile.ReadString(Account, 'email', EmptyWideStr);
	Result.Password := FConfigFile.ReadString(Account, 'password', EmptyWideStr);
	Result.UseTCPasswordManager := FConfigFile.ReadBool(Account, 'tc_pwd_mngr', False);
	Result.UnlimitedFileSize := FConfigFile.ReadBool(Account, 'unlimited_filesize', False);
	Result.SplitLargeFiles := FConfigFile.ReadBool(Account, 'split_large_files', False);
	Result.PublicAccount := FConfigFile.ReadBool(Account, 'public_account', False);
	Result.PublicUrl := FConfigFile.ReadString(Account, 'public_url', EmptyWideStr);
	Result.Description := FConfigFile.ReadString(Account, 'description', EmptyWideStr);
	Result.EncryptFilesMode := FConfigFile.ReadInteger(Account, 'encrypt_files_mode', EncryptModeNone);
	Result.CipherProfileId := FConfigFile.ReadString(Account, 'encrypt_cipher_profile', EmptyWideStr);
	Result.ShardOverride := FConfigFile.ReadString(Account, 'shard_override', EmptyWideStr);
	Result.UploadUrlOverride := FConfigFile.ReadString(Account, 'upload_url_override', EmptyWideStr);
	Result.CryptedGUIDFiles := FConfigFile.ReadString(Account, 'CryptedGUID_files', EmptyWideStr);
	Result.AuthMethod := FConfigFile.ReadInteger(Account, 'auth_method', 0);
	Result.UseAppPassword := FConfigFile.ReadBool(Account, 'use_app_password', False);
end;

procedure TAccountsManager.SetAccountSettings(Account: WideString; AccountSettings: TAccountSettings);
begin
	FConfigFile.WriteStringIfNotDefault(Account, 'email', AccountSettings.Email, EmptyWideStr);
	FConfigFile.WriteStringIfNotDefault(Account, 'password', AccountSettings.Password, EmptyWideStr);
	FConfigFile.WriteBoolIfNotDefault(Account, 'tc_pwd_mngr', AccountSettings.UseTCPasswordManager, False);
	FConfigFile.WriteBoolIfNotDefault(Account, 'unlimited_filesize', AccountSettings.UnlimitedFileSize, False);
	FConfigFile.WriteBoolIfNotDefault(Account, 'split_large_files', AccountSettings.SplitLargeFiles, False);
	FConfigFile.WriteBoolIfNotDefault(Account, 'public_account', AccountSettings.PublicAccount, False);
	FConfigFile.WriteStringIfNotDefault(Account, 'public_url', AccountSettings.PublicUrl, EmptyWideStr);
	FConfigFile.WriteStringIfNotDefault(Account, 'description', AccountSettings.Description, EmptyWideStr);
	FConfigFile.WriteIntegerIfNotDefault(Account, 'encrypt_files_mode', AccountSettings.EncryptFilesMode, EncryptModeNone);
	FConfigFile.WriteStringIfNotDefault(Account, 'encrypt_cipher_profile', AccountSettings.CipherProfileId, EmptyWideStr);
	FConfigFile.WriteIntegerIfNotDefault(Account, 'auth_method', AccountSettings.AuthMethod, 0);
	FConfigFile.WriteBoolIfNotDefault(Account, 'use_app_password', AccountSettings.UseAppPassword, False);
end;

function TAccountsManager.GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
var
	CurrentAccount: WideString;
	TempAccountSettings: TAccountSettings;
begin
	Result.Clear;

	for CurrentAccount in self.Accounts do
	begin
		{Skip accounts with names that would break WFX paths or INI format}
		if not TAccountsManager.IsValidAccountName(CurrentAccount) then
		begin
			FLogger.Log(LOG_LEVEL_WARNING, msgtype_details, WARN_ACCOUNT_NAME_SKIPPED, [CurrentAccount]);
			Continue;
		end;

		TempAccountSettings := self.GetAccountSettings(CurrentAccount);
		if TempAccountSettings.AccountType <= AccountTypes then {current account type is in requested accounts types}
			Result.Add(CurrentAccount);
		if [ATPrivate] = TempAccountSettings.AccountType then {current account is private}
		begin
			if VTTrash in VirtualTypes then
				Result.Add(CurrentAccount + TrashPostfix);
			if VTShared in VirtualTypes then
				Result.Add(CurrentAccount + SharedPostfix);
			if VTInvites in VirtualTypes then
				Result.Add(CurrentAccount + InvitesPostfix);
		end;

	end;

end;

procedure TAccountsManager.SetAccountSettings(AccountSettings: TAccountSettings);
begin
	SetAccountSettings(AccountSettings.Account, AccountSettings);
end;

procedure TAccountsManager.SetCryptedGUID(Account, GUID: WideString);
begin
	FConfigFile.WriteString(Account, 'CryptedGUID_files', GUID);
end;

end.
