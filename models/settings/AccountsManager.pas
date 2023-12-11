unit AccountsManager;

interface

uses
	IniFiles,
	SysUtils,
	Variants,
	Classes,
	WindowsHelper,
	Windows,
	ParsingHelper,
	IniFilesHelper,
	LANGUAGE_STRINGS,
	SETTINGS_CONSTANTS,
	WSList,
	AccountSettings;

type
	TAccountsManager = class
	private
		FIniFilePath: WideString;

		function Accounts: TWSList;
	public
		constructor Create(IniFilePath: WideString);
		function GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
		function GetAccountSettings(Account: WideString): TAccountSettings;
		procedure SetAccountSettings(Account: WideString; AccountSettings: TAccountSettings); overload;
		procedure SetAccountSettings(AccountSettings: TAccountSettings); overload;
		procedure DeleteAccount(Account: WideString);
		procedure SetCryptedGUID(Account: WideString; GUID: WideString);
		procedure SwitchPasswordStorage(Account: WideString); //clears the account password from INI for account and sets 'managed by TC' flag

	end;

implementation

{TNewAccountSettings}

function TAccountsManager.Accounts: TWSList;
var
	AccountsList: TStringList;
	IniFile: TIniFile;
	I: Integer;
begin
	AccountsList := TStringList.Create();
	IniFile := TIniFile.Create(FIniFilePath);
	IniFile.ReadSections(AccountsList);
	IniFile.Destroy;
	SetLength(Result, AccountsList.Count);
	for I := 0 to AccountsList.Count - 1 do
		Result[I] := AccountsList[I];
	AccountsList.Free;
end;

constructor TAccountsManager.Create(IniFilePath: WideString);
begin
	self.FIniFilePath := IniFilePath;
end;

procedure TAccountsManager.SwitchPasswordStorage(Account: WideString);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FIniFilePath);
	IniFile.DeleteKey(Account, 'password');
	IniFile.WriteBool(Account, 'tc_pwd_mngr', True);
	IniFile.Destroy;
end;

procedure TAccountsManager.DeleteAccount(Account: WideString);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FIniFilePath);
	IniFile.EraseSection(Account);
	IniFile.Destroy;
end;

function TAccountsManager.GetAccountSettings(Account: WideString): TAccountSettings;
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FIniFilePath);
	Result.Account := Account;
	Result.Email := IniFile.ReadString(Account, 'email', EmptyWideStr);
	Result.Password := IniFile.ReadString(Account, 'password', EmptyWideStr);
	Result.UseTCPasswordManager := IniFile.ReadBool(Account, 'tc_pwd_mngr', False);
	Result.UnlimitedFileSize := IniFile.ReadBool(Account, 'unlimited_filesize', False);
	Result.SplitLargeFiles := IniFile.ReadBool(Account, 'split_large_files', False);
	Result.TwostepAuth := IniFile.ReadBool(Account, 'twostep_auth', False);
	Result.PublicAccount := IniFile.ReadBool(Account, 'public_account', False);
	Result.PublicUrl := IniFile.ReadString(Account, 'public_url', EmptyWideStr);
	Result.Description := IniFile.ReadString(Account, 'description', EmptyWideStr);
	Result.EncryptFilesMode := IniFile.ReadInteger(Account, 'encrypt_files_mode', EncryptModeNone);
	Result.EncryptFileNames := IniFile.ReadBool(Account, 'encrypt_filenames', False);
	Result.ShardOverride := IniFile.ReadString(Account, 'shard_override', EmptyWideStr);
	Result.UploadUrlOverride := IniFile.ReadString(Account, 'upload_url_override', EmptyWideStr);
	Result.CryptedGUIDFiles := IniFile.ReadString(Account, 'CryptedGUID_files', EmptyWideStr);
	IniFile.Destroy;
end;

procedure TAccountsManager.SetAccountSettings(Account: WideString; AccountSettings: TAccountSettings);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FIniFilePath);
	IniFile.WriteStringIfNotDefault(Account, 'email', AccountSettings.Email, EmptyWideStr);
	IniFile.WriteStringIfNotDefault(Account, 'password', AccountSettings.Password, EmptyWideStr);
	IniFile.WriteBoolIfNotDefault(Account, 'tc_pwd_mngr', AccountSettings.UseTCPasswordManager, False);
	IniFile.WriteBoolIfNotDefault(Account, 'unlimited_filesize', AccountSettings.UnlimitedFileSize, False);
	IniFile.WriteBoolIfNotDefault(Account, 'split_large_files', AccountSettings.SplitLargeFiles, False);
	IniFile.WriteBoolIfNotDefault(Account, 'twostep_auth', AccountSettings.TwostepAuth, False);
	IniFile.WriteBoolIfNotDefault(Account, 'public_account', AccountSettings.PublicAccount, False);
	IniFile.WriteStringIfNotDefault(Account, 'public_url', AccountSettings.PublicUrl, EmptyWideStr);
	IniFile.WriteStringIfNotDefault(Account, 'description', AccountSettings.Description, EmptyWideStr);
	IniFile.WriteIntegerIfNotDefault(Account, 'encrypt_files_mode', AccountSettings.EncryptFilesMode, EncryptModeNone);
	IniFile.WriteBoolIfNotDefault(Account, 'encrypt_filenames', AccountSettings.EncryptFileNames, False);
	IniFile.Destroy;
end;

function TAccountsManager.GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
var
	CurrentAccount: WideString;
	TempAccountSettings: TAccountSettings;
begin
	Result.Clear;

	for CurrentAccount in self.Accounts do
	begin
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
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FIniFilePath);
	IniFile.WriteString(Account, 'CryptedGUID_files', GUID);
	IniFile.Destroy;
end;

end.
