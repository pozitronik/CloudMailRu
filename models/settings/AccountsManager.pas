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
	CMRStrings,
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
		procedure ClearPassword(Account: WideString); //clears the account password from INI for account

	end;

implementation

{TNewAccountSettings}

function TAccountsManager.Accounts: TWSList;
var
	AccountsList: TStringList; {Todo: use a direct method to not to use TStrings}
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
end;

constructor TAccountsManager.Create(IniFilePath: WideString);
begin
	self.FIniFilePath := IniFilePath;
end;

procedure TAccountsManager.ClearPassword(Account: WideString);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FIniFilePath);
	IniFile.DeleteKey(Account, 'password');
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

procedure TAccountsManager.SetAccountSettings(Account: WideString; AccountSettings: TAccountSettings);
var
	IniFile: TIniFile;
begin
	with AccountSettings do
	begin
		IniFile := TIniFile.Create(FIniFilePath);
		IniFile.WriteString(Account, 'email', Email);
		IniFile.WriteString(Account, 'password', Password);
		IniFile.WriteBool(Account, 'tc_pwd_mngr', UseTCPasswordManager);
		IniFile.WriteBool(Account, 'unlimited_filesize', UnlimitedFileSize);
		IniFile.WriteBool(Account, 'split_large_files', SplitLargeFiles);
		IniFile.WriteBool(Account, 'twostep_auth', TwostepAuth);
		IniFile.WriteBool(Account, 'public_account', PublicAccount);
		IniFile.WriteString(Account, 'public_url', PublicUrl);
		IniFile.WriteString(Account, 'description', Description);
		IniFile.WriteInteger(Account, 'encrypt_files_mode', EncryptFilesMode);
		IniFile.WriteBool(Account, 'encrypt_filenames', EncryptFileNames);
		IniFile.Destroy;
	end;
end;

end.
