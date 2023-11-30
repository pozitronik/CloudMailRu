unit NewAccountSettings;

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
	AbstractAccountSettings;

type

	EAccountType = set of (ATPrivate, ATPublic);
	EVirtualType = set of (VTTrash, VTShared, VTInvites);

type
	TNewAccountSettings = class(TAbstractAccountSettings)
	private
		FIniFilePath: WideString;
		FAccount: WideString;
		FUser: WideString;
		FDomain: WideString;

		function GetAccount: WideString;
		function GetIsRemoteDescriptionsSupported: Boolean; overload;
		function GetIsInAccount: Boolean;
		procedure SetAccount(const Value: WideString);
		function Accounts: TWSList;
		function GetAccountType: EAccountType;
	public

		constructor Create(IniFilePath: WideString; Account: WideString); overload;
		constructor Create(IniFilePath: WideString); overload;
		constructor Create(AccountSettings: TNewAccountSettings; Account: WideString); overload;
		constructor Create(AccountSettings: TNewAccountSettings); overload;
		procedure Refresh();
		property Account: WideString read GetAccount write SetAccount;
		property IsInAccount: Boolean read GetIsInAccount;
		property User: WideString read FUser;
		property Domain: WideString read FDomain;
		property IsRemoteDescriptionsSupported: Boolean read GetIsRemoteDescriptionsSupported;
		property AccountType: EAccountType read GetAccountType;

		procedure DeleteAccount(Account: WideString);
		function GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
		procedure SetSettingValue(OptionName: WideString; OptionValue: Variant); override;
		procedure Save(); override;

		{TODO: TEMP STUB methods}

		function GetIsRemoteDescriptionsSupported(Account: WideString): Boolean; overload;
		function GetDescription(Account: WideString): WideString;
		function GetIsPublic(Account: WideString): Boolean;
		class procedure ClearPassword(IniFilePath: WideString; Account: WideString); //clears the account password from INI for account
		class procedure SetSettingValueStatic(IniFilePath: WideString; OptionName: WideString; OptionValue: Variant); //todo: it is a temp method
	end;

implementation

{TNewAccountSettings}

function TNewAccountSettings.Accounts: TWSList;
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

constructor TNewAccountSettings.Create(IniFilePath: WideString);
begin
	self.FIniFilePath := IniFilePath;
	self.FSaveOnChange := False;
end;

constructor TNewAccountSettings.Create(AccountSettings: TNewAccountSettings; Account: WideString);
begin
	self.FIniFilePath := AccountSettings.FIniFilePath;
	self.FSaveOnChange := AccountSettings.SaveOnChange;
	self.Account := Account;
	Refresh();
end;

constructor TNewAccountSettings.Create(IniFilePath, Account: WideString);
begin
	self.FIniFilePath := IniFilePath;
	self.FSaveOnChange := False;
	self.Account := Account;
	Refresh();
end;

class procedure TNewAccountSettings.ClearPassword(IniFilePath: WideString; Account: WideString);
var
	TempAccountSettings: TNewAccountSettings;
begin
	TempAccountSettings := TNewAccountSettings.Create(IniFilePath, Account);
	TempAccountSettings.SetSettingValue('password', null);
	TempAccountSettings.Free;
end;

constructor TNewAccountSettings.Create(AccountSettings: TNewAccountSettings);
begin
	self.FIniFilePath := AccountSettings.FIniFilePath;
	self.FSaveOnChange := AccountSettings.SaveOnChange;
	self.Account := Account;
end;

procedure TNewAccountSettings.DeleteAccount(Account: WideString);
var
	IniFile: TIniFile;
begin
	IniFile := TIniFile.Create(FIniFilePath);
	IniFile.EraseSection(Account);
	IniFile.Destroy;
end;

function TNewAccountSettings.GetAccount: WideString;
begin
	Result := FAccount;
end;

function TNewAccountSettings.GetAccountsList(const AccountTypes: EAccountType = [ATPrivate, ATPublic]; const VirtualTypes: EVirtualType = []): TWSList;
var
	CurrentAccount: WideString;
	TempAccountSettings: TNewAccountSettings;
begin
	Result.Clear;
	TempAccountSettings := TNewAccountSettings.Create(self);
	for CurrentAccount in self.Accounts do
	begin
		TempAccountSettings.Account := CurrentAccount;
		if TempAccountSettings.GetAccountType <= AccountTypes then {current account tyep is in requested accounts types}
			Result.Add(CurrentAccount);
		if [ATPrivate] = TempAccountSettings.GetAccountType then {current account is private}
		begin
			if VTTrash in VirtualTypes then
				Result.Add(CurrentAccount + TrashPostfix);
			if VTShared in VirtualTypes then
				Result.Add(CurrentAccount + SharedPostfix);
			if VTInvites in VirtualTypes then
				Result.Add(CurrentAccount + InvitesPostfix);
		end;

	end;
	TempAccountSettings.Free;
end;

function TNewAccountSettings.GetAccountType: EAccountType;
begin
	if self.PublicAccount then
		exit([ATPublic]);
	exit([ATPrivate]);
end;

function TNewAccountSettings.GetDescription(Account: WideString): WideString;
var
	TempAccountSettings: TNewAccountSettings;
begin
	TempAccountSettings := TNewAccountSettings.Create(self);
	Result := TempAccountSettings.Description;
	TempAccountSettings.Free;
end;

function TNewAccountSettings.GetIsInAccount: Boolean;
begin
	Result := FAccount <> EmptyWideStr;
end;

function TNewAccountSettings.GetIsPublic(Account: WideString): Boolean;
var
	TempAccountSettings: TNewAccountSettings;
begin
	TempAccountSettings := TNewAccountSettings.Create(self);
	Result := TempAccountSettings.PublicAccount;
	TempAccountSettings.Free;
end;

function TNewAccountSettings.GetIsRemoteDescriptionsSupported: Boolean;
begin
	Result := not((FEncryptFilesMode <> EncryptModeNone) and FEncryptFileNames)
end;

function TNewAccountSettings.GetIsRemoteDescriptionsSupported(Account: WideString): Boolean;
var
	TempAccountSettings: TNewAccountSettings;
begin
	TempAccountSettings := TNewAccountSettings.Create(self);
	Result := TempAccountSettings.GetIsRemoteDescriptionsSupported;
	TempAccountSettings.Free;
end;

procedure TNewAccountSettings.Refresh;
var
	IniFile: TIniFile;
begin
	if not IsInAccount then
		exit;
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
	if not IsInAccount then
		exit;
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

procedure TNewAccountSettings.SetAccount(const Value: WideString);
begin
	FAccount := Value;
	Refresh();
end;

class procedure TNewAccountSettings.SetSettingValueStatic(IniFilePath, OptionName: WideString; OptionValue: Variant);
var
	TempAccountSettings: TNewAccountSettings;
begin
	TempAccountSettings := TNewAccountSettings.Create(IniFilePath);
	TempAccountSettings.SetSettingValue(OptionName, OptionValue);
	TempAccountSettings.Free;
end;

{TODO: this method violates the model abstraction boundaries and should not be used. It'll be removed after refactoring.}
procedure TNewAccountSettings.SetSettingValue(OptionName: WideString; OptionValue: Variant);
var
	IniFile: TIniFile;
	basicType: Integer;
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
	Refresh;
	IniFile.Destroy;
end;

end.
