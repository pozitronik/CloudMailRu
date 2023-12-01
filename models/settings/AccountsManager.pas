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
	TempAccountSettings: TAccountSettings;
begin
	TempAccountSettings := self.GetAccountSettings(Account);
	TempAccountSettings.Password := EmptyWideStr;
	self.SetAccountSettings(Account, TempAccountSettings);
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
	with Result do
	begin
		Email := IniFile.ReadString(Account, 'email', EmptyWideStr);
		Password := IniFile.ReadString(Account, 'password', EmptyWideStr);
		UseTCPasswordManager := IniFile.ReadBool(Account, 'tc_pwd_mngr', False);
		UnlimitedFileSize := IniFile.ReadBool(Account, 'unlimited_filesize', False);
		SplitLargeFiles := IniFile.ReadBool(Account, 'split_large_files', False);
		TwostepAuth := IniFile.ReadBool(Account, 'twostep_auth', False);
		PublicAccount := IniFile.ReadBool(Account, 'public_account', False);
		PublicUrl := IniFile.ReadString(Account, 'public_url', EmptyWideStr);
		Description := IniFile.ReadString(Account, 'description', EmptyWideStr);
		EncryptFilesMode := IniFile.ReadInteger(Account, 'encrypt_files_mode', EncryptModeNone);
		EncryptFileNames := IniFile.ReadBool(Account, 'encrypt_filenames', False);
		ShardOverride := IniFile.ReadString(Account, 'shard_override', EmptyWideStr);
		UploadUrlOverride := IniFile.ReadString(Account, 'upload_url_override', EmptyWideStr);
		CryptedGUIDFiles := IniFile.ReadString(Account, 'CryptedGUID_files', EmptyWideStr);
	end;
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
	SetAccountSettings(AccountSettings.User, AccountSettings);
end;

procedure TAccountsManager.SetCryptedGUID(Account, GUID: WideString);
var
	TempAccountSettings: TAccountSettings;
begin
	TempAccountSettings := self.GetAccountSettings(Account);
	TempAccountSettings.CryptedGUIDFiles := GUID;
	self.SetAccountSettings(Account, TempAccountSettings);
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
